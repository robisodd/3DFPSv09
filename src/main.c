/**********************************************************************************
   Pebble 3D FPS Engine v0.7 beta
   Created by Rob Spiess (robisodd@gmail.com) on June 23, 2014
  *********************************************************************************
  v0.1: Initial Release
  
  v0.2: Converted all floating points to int32_t which increased framerate substancially
        Gets overflow errors, so when a number squared is negative, sets it to 2147483647.
        Added door blocks for another example
  
  v0.3: Added distance shading
        Removed door and zebra blocks
        
  v0.4: Optimizations
  
  v0.5: Changed 1000x1000 pixel blocks to 64 x 64
        Changed all /1000 to >>6 for quicker division (*64 = <<6)
        Updated Square Root Function
        Added 64x64bit Textures
        Added Texture Distance Shading overlay
        Modified FOV to give more square view (not squished or stretched)
        Added mirror block and black block
        Select button shoots ray to change blocks
  
  v0.6: Isolated shootray function
        Created new 3D routine - more optimzied and works better?
        All textures 32x32bits -> Faster due to single uint32_t
        Repaired shoot_ray function (no longer need ceiling function)
        
  v0.7: Cleaned up mess
        Added comments to inform those how this thing kinda works
        ray.dist is now unsigned
        raycasting is more optimized
        Got rid of floor/ceil function
        Super optimized raycasting (got rid of square root)
        Added strafing (hold the DOWN button to strafe)
        BEEFED up the drawing routine (direct framebuffer writing)
        Added MazeMap Generation
        Added Floor/Ceiling casting
        
  v0.8: Walls all now made of 32x32px blocks
        32x32bit textures map per block
        Wall heights are variable by increments of 4px
        
  v0.9: Scrapped v0.8, started back with v0.7
        Removed mirror blocks.  Won't work with how sprites will be implemented.
        Added 4 wall faces to blocks
        Changed map to unsigned (uint8 from int8)
        Map squares now point to Square_type array
        Adding sprite support...
        
  To Do:
        Texture looping
        Add switches on walls to change cells
        
        Change how map/walls/textures/levels work:
          Levels specify size/shape, layout and which textures are to be loaded
          Any negative map square means ray passes through
          Map squares -128 to 127 point to array of Square_Type
          Square_Type[256] struct array holds fields:
            Ceiling texture (Texture_Array) (or none -- just sky)
            Floor Texture (Texture_Array)
            Each of the 4 walls have a separate texture (or 0 for clear on this side), uint8_t index of Texture_Array
              Texture_Array[256 (or less)] structure:
              Holds pointer to texture
              Info if texture is 32x32 or 64x64 or 64x32 or 16x32 or whatever
            Possible inner/outer texture depending on direction hit?
            Darkness Amount (and if distance shading is on/off)
            Permissable to walk through
            Permissable for enemies to walk through
            Permissable for bullets/missles/magic?
            Invert texture bit? uint8_t = 4 walls + ceiling + floor + 2 spare bits
            Maybe ray modifying? e.g. Mirror or Light-bending gravity well?  No, nevermind.
            Note: Square_Type[0] is out of bounds render type
            
        Sprite shadow is the shadow of the square it's in.
        Define terms:
          Tile
          Square
          Map
          X & Y (is it pixel specific 64 or map squares)
          
  To Fix:
        Wall textures map to exact coordinate, not distance from left edge
          So textures on corners don't line up
          e.g. Textures on parallel sides of map square have texture going same direction.
          
  *********************************************************************************
  Created with the help of these tutorials:
    http://www.playfuljs.com/a-first-person-engine-in-265-lines/
    http://www.permadi.com/tutorial/raycast/index.html

  CC Copyright (c) 2015 All Right Reserved
  THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
  http://creativecommons.org/licenses/by/3.0/legalcode
  
  *********************************************************************************
   Notes and Ideas
  *********************************************************************************
   Poisoned = Black Boxes and White Blocks -- i.e. Renders without Textures or changes texture
   Night time = Darkness fades off closer.  Can only see 3 away unless torch is lit
   Full brightness = no darkness based on distance
   Mirror seems to reflect magic, too. Magic repulsing shield?
   Rain or Snow overlay
     Snow would eventually change ground to white, maybe raise lower-wall level when deep, slowing walking
     Rain would also increase darkness due to cloudcover.  Black sky?  Clouds in sky?
   Drunk or Poisoned mode: autowalk or autoturn or random strafing when walking or inverted accel.x
   IDCLIP on certain block types -- walk through blocks which look solid (enemies can't penetrate to expose - unless in chase mode?)
   
   
   map[] uint8_t  bit:76543210
     bit7: invisible walls (ray passes through): 0 yes, 1 no
           currently also if can walk through (though this will change)
     
  
  *********************************************************************************/
// 529a7262-efdb-48d4-80d4-da14963099b9
#include "pebble.h"

#define mapsize 10             // Map is 90x90 squares, or whatever number is here
#define MAX_TEXTURES 10        // Most number of textures there's likely to be.  Feel free to increase liberally, but no more than 254.
#define IDCLIP false           // Walk thru walls
#define view_border true       // Draw border around 3D viewing window

//----------------------------------//
// Viewing Window Size and Position //
//----------------------------------//
// beneficial to: (set fov as divisible by view_w) and (have view_w evenly divisible by 2)
// e.g.: view_w=144 is good since 144/2=no remainder. Set fov = 13104fov (since it = 144w x 91 and is close to 20% of 65536)

GRect view;
int32_t    fov = 10650;             // Field of view angle (20% of a circle is good) (TRIG_MAX_RATIO = 0x10000 or 65536) * 20%
//view = GRect(0, 0, 144, 168); fov = 13104;  // Full Screen (You should also comment out drawing the text box)
//view = GRect(20, 30, 100, 100); fov = 13100;  // Smaller square

//Nearly full screen
//#define fov 13064              // Field of view angle (20% of a circle is good) (TRIG_MAX_RATIO = 0x10000 or 65536) * 20%

//----------------------------------//
//#define fov_over_w fov/view_w  // Do math now so less during execution
//#define half_view_w view_w/2   //
//#define half_view_h view_h/2
//----------------------------------//

uint32_t dist[144];                 // Array of distance for each vertical wall segment -- for sprite rendering
GBitmap *texture[MAX_TEXTURES];
//uint8_t texture_count;  // probably remove once delete routine works

struct squaretypestruct {
  uint8_t face[4]; // texture[] number
  uint8_t ceiling; // texture[] number (255 = no ceiling / sky.  Well, 255 or any number >MAX_TEXTURES)
  uint8_t floor;   // texture[] number (255 = no floor texture)
  // other characteristics like walk thru and stuff
} squaretype[10];
// note squaretype[0]=out of bounds ceiling/floor rendering

typedef struct PlayerStruct {
  int32_t x;                  // Player's X Position (64 pixels per square)
  int32_t y;                  // Player's Y Position (64 pixels per square)
  int32_t facing;             // Player Direction Facing (from 0 - TRIG_MAX_ANGLE)
} PlayerStruct;
static PlayerStruct player;

static PlayerStruct sprite;

typedef struct RayStruct {
   int32_t x;                 // x coordinate on map the ray hit
   int32_t y;                 // y coordinate on map the ray hit
  uint32_t dist;              // length of the ray / distance ray traveled
   uint8_t hit;               // block type the ray hit
   int32_t offset;            // horizontal spot on texture the ray hit [0-63]
   uint8_t face;              // face of the block it hit (00=East Wall, 01=North, 10=West, 11=South Wall)
} RayStruct;
static RayStruct ray;

static Window *window;
static GRect window_frame;
static Layer *graphics_layer;

static bool up_button_depressed = false; // Whether Pebble's   Up   button is held
static bool dn_button_depressed = false; // Whether Pebble's  Down  button is held
static bool sl_button_depressed = false; // Whether Pebble's Select button is held
//static bool bk_button_depressed = false; // Whether Pebble's  Back  button is held

static uint8_t map[mapsize * mapsize];  // 0-255, 0-127 are squaretypes[], high bit set means ray hits

int32_t  sqrt_int(int32_t a, int8_t root_depth) {int32_t b=a; for(int8_t i=0; i<root_depth; i++) b=(b+(a/b))/2; return b;} // Square Root
int32_t   abs_int(int32_t a){return (a<0 ? 0 - a : a);} // Absolute Value

#define root_depth 10          // How many iterations square root function performs
int32_t  sqrt32(int32_t a) {int32_t b=a; for(int8_t i=0; i<root_depth; i++) b=(b+(a/b))/2; return b;} // Square Root

int32_t abs32(int32_t x) {return (x^(x>>31)) - (x>>31);}
int16_t abs16(int16_t x) {return (x^(x>>15)) - (x>>15);}
int8_t  abs8 (int8_t  x) {return (x^(x>> 7)) - (x>> 7);}

// sign function returns: -1 or 0 or 1 if input is <0 or =0 or >0
int8_t  sign8 (int8_t  x){return (x > 0) - (x < 0);}
int16_t sign16(int16_t x){return (x > 0) - (x < 0);}
int32_t sign32(int32_t x){return (x > 0) - (x < 0);}

// ------------------------------------------------------------------------ //
//  Map Functions
// ------------------------------------------------------------------------ //
void LoadMapTextures() {
  texture[0] = gbitmap_create_with_resource(RESOURCE_ID_STONE);
  texture[1] = gbitmap_create_with_resource(RESOURCE_ID_WALL_FIFTY);
  texture[2] = gbitmap_create_with_resource(RESOURCE_ID_WALL_CIRCLE);
  texture[3] = gbitmap_create_with_resource(RESOURCE_ID_FLOOR_TILE);
  texture[4] = gbitmap_create_with_resource(RESOURCE_ID_CEILING_LIGHTS);
  texture[5] = gbitmap_create_with_resource(RESOURCE_ID_WALL_BRICK);
  texture[6] = gbitmap_create_with_resource(RESOURCE_ID_GRASS);
}

void UnLoadMapTextures() {
  for(uint8_t i=0; i<MAX_TEXTURES; i++)
    if(texture[i])
      gbitmap_destroy(texture[i]);
}

void GenerateSquareMap() {
  squaretype[0].ceiling = 255; // outside sky
  squaretype[0].floor   = 6;   // outside grass

  squaretype[1].face[0]=squaretype[1].face[1]=squaretype[1].face[2]=squaretype[1].face[3]=5;
  squaretype[1].ceiling = 4;
  squaretype[1].floor   = 3;
  
  for (int16_t i=0; i<mapsize*mapsize; i++)
    map[i] = 1;                            // inside floor/ceiling
  
  for (int16_t i=0; i<mapsize; i++) {
    map[i*mapsize + 0]           = 128+0;  // west wall
    map[i*mapsize + mapsize - 1] = 128+0;  // east wall
    map[i]                       = 128+0;  // north wall
    map[(mapsize-1)*mapsize + i] = 128+0;  // south wall
  }
  map[((mapsize-1) * mapsize)/2] = 128+1;  // middle block
 
  player.x=64; player.y=64*mapsize/2; player.facing=0; // start inside
}

void GenerateRandomMap() {
  //squaretype[0].face[0] = 0; squaretype[0].face[1] = 0; squaretype[0].face[2] = 0; squaretype[0].face[3] = 0;
  squaretype[0].ceiling = 255;
  squaretype[0].floor = 6;

  squaretype[1].face[0] = 0;
  squaretype[1].face[1] = 5;
  squaretype[1].face[2] = 5;
  squaretype[1].face[3] = 0;
  squaretype[1].ceiling = 4;
  squaretype[1].floor = 3;
  
  //squaretype[2].face[0] = 0; squaretype[2].face[1] = 0; squaretype[2].face[2] = 0; squaretype[2].face[3] = 0;
  squaretype[2].ceiling = 2;
  squaretype[2].floor = 4;

  
  for (int16_t i=0; i<mapsize*mapsize; i++) map[i] = rand() % 3 == 0 ? 128+1 : 1;       // Randomly 1/3 of spots are [type 2] blocks, the rest are [type 1]
  for (int16_t i=0; i<mapsize*mapsize; i++) if(map[i]==1 && rand()%10==0) map[i]=128+2; // Change 10% of [type 2] blocks to [type 3] blocks
  //for (int16_t i=0; i<mapsize*mapsize; i++) if(map[i]==2 && rand()%2==0) map[i]=3;  // Changes 50% of [type 2] blocks to [type 3] blocks
}

// Generates maze starting from startx, starty, filling map with (0=empty, 1=wall, -1=special)
void GenerateMazeMap(int32_t startx, int32_t starty) {
  squaretype[0].ceiling = 255;
  squaretype[0].floor = 6;

  squaretype[1].ceiling = 4;
  squaretype[1].floor = 3;
  
  squaretype[2].ceiling = 2;
  squaretype[2].floor = 4;

  int32_t x, y;
  int8_t try;
  int32_t cursorx, cursory, next=1;
  
  cursorx = startx; cursory=starty;  
  for (int16_t i=0; i<mapsize*mapsize; i++) map[i] = 0; // Fill map with 0s
  
  while(true) {
    int32_t current = cursory * mapsize + cursorx;
    if((map[current] & 15) == 15) {  // If all directions have been tried, then go to previous cell unless you're back at the start
      if(cursory==starty && cursorx==startx) {  // If back at the start, then we're done.
        map[current]=1;
        for (int16_t i=0; i<mapsize*mapsize; i++) if(map[i]==0) map[i] = 128+1; // invert map bits (1=empty, 128+1=wall, 2=special)
        return;
      }
      switch(map[current] >> 4) { // Else go back to the previous cell:  NOTE: If the 1st two bits are used, need to "&3" mask this
       case 0: cursorx++; break;
       case 1: cursory++; break;
       case 2: cursorx--; break;
       case 3: cursory--; break;
      }
      map[current]=next; next=1;   // cells which have been double-traversed are not the end of a dead end, cause we backtracked through it, so set it to square-type 1
    } else {                       // not all directions have been tried
      do try = rand()%4; while (map[current] & (1<<try));  // Pick random direction until that direction hasn't been tried
      map[current] |= (1<<try); // turn on bit in this cell saying this path has been tried
      // below is just: x=0, y=0; if(try=0)x=1; if(try=1)y=1; if(try=2)x=-1; if(try=3)y=-1;
      y=(try&1); x=y^1; if(try&2){y=(~y)+1; x=(~x)+1;} //  y = try's 1st bit, x=y with 1st bit xor'd (toggled).  Then "Two's Complement Negation" if try's 2nd bit=1
      
      // Move if spot is blank and every spot around it is blank (except where it came from)
      if((cursory+y)>0 && (cursory+y)<mapsize-1 && (cursorx+x)>0 && (cursorx+x)<mapsize-1) // Make sure not moving to or over boundary
        if(map[(cursory+y) * mapsize + cursorx + x]==0)                                    // Make sure not moving to a dug spot
          if((map[(cursory+y-1) * mapsize + cursorx+x]==0 || try==1))                      // Nothing above (unless came from above)
            if((map[(cursory+y+1) * mapsize + cursorx+x]==0 || try==3))                    // nothing below (unless came from below)
              if((map[(cursory+y) * mapsize + cursorx+x - 1]==0 || try==0))                // nothing to the left (unless came from left)
                if((map[(cursory+y) * mapsize + cursorx + x + 1]==0 || try==2)) {          // nothing to the right (unless came from right)
                  cursorx += x; cursory += y;                                              // All's good!  Let's move
                  next=2;                                                                  // Set dead end spots as square-type 2
                  map[cursory * mapsize + cursorx] |= ((try+2)%4) << 4; //record in new cell where ya came from -- the (try+2)%4 is because when you move west, you came from east
                }
    }
  } //End While True
}

uint8_t getmap(int32_t x, int32_t y) {
  x=x>>6; y=y>>6;
  return (x<0 || x>=mapsize || y<0 || y>=mapsize) ? 0 : map[(y * mapsize) + x];
}

void setmap(int32_t x, int32_t y, uint8_t value) {
  x=x>>6; y=y>>6;
  if((x >= 0) && (x < mapsize) && (y >= 0) && (y < mapsize))
    map[y * mapsize + x] = value;
}


// ------------------------------------------------------------------------ //
//  General game functions
// ------------------------------------------------------------------------ //

void walk(int32_t direction, int32_t distance) {
  int32_t dx = (cos_lookup(direction) * distance) >> 16;
  int32_t dy = (sin_lookup(direction) * distance) >> 16;
  if(getmap(player.x + dx, player.y) < 128 || IDCLIP) player.x += dx;  // currently <128 so blocks rays hit user hits.  will change to walkthru type blocks
  if(getmap(player.x, player.y + dy) < 128 || IDCLIP) player.y += dy;
}

static void main_loop(void *data) {
  AccelData accel=(AccelData){.x=0, .y=0, .z=0};          // all three are int16_t
  accel_service_peek(&accel);                             // read accelerometer
  walk(player.facing, accel.y>>5);                        // walk based on accel.y  Technically: walk(accel.y * 64px / 1000);
  if(dn_button_depressed)                                 // if down button is held
    walk(player.facing + (1<<14), accel.x>>5);            //   strafe (1<<14 = TRIG_MAX_ANGLE / 4)
  else                                                    // else
    player.facing = (player.facing + (accel.x<<3) + TRIG_MAX_ANGLE) % TRIG_MAX_ANGLE;                        //   spin
  layer_mark_dirty(graphics_layer);                       // tell pebble to draw when it's ready
}

//shoot_ray(x, y, angle)
//  x, y = position on map to shoot the ray from
//  angle = direction to shoot the ray (in Pebble angle notation)
//modifies: global RayStruct ray
void shoot_ray(int32_t start_x, int32_t start_y, int32_t angle) {
  int32_t sin, cos, dx, dy, nx, ny;
  
    sin = sin_lookup(angle);
    cos = cos_lookup(angle);
  ray.x = start_x;
  ray.y = start_y;
     ny = sin>0 ? 64 : -1;
     nx = cos>0 ? 64 : -1;
  
  do { do {
    dy = ny - (ray.y & 63);                          // north-south component of distance to next east-west wall
    dx = nx - (ray.x & 63);                          // east-west component of distance to next north-south wall

    if(abs32(dx * sin) < abs32(dy * cos)) {          // if(distance to north-south wall < distance to east-west wall) See Footnote 1
      ray.x += dx;
      ray.y += ((dx * sin) / cos);
      ray.hit = getmap(ray.x, ray.y);
      if(ray.hit > 127) {                            // if ray hits a wall (a block)
        ray.face = cos>0 ? 2 : 0;                      // hit west or east face of block
        ray.offset = cos>0 ? 64-(ray.y&63) : ray.y&63; // Offset is where on wall ray hits: 0 (left edge) to 63 (right edge)
        ray.dist = ((ray.x - start_x) << 16) / cos;    // Distance ray traveled.    <<16 = * TRIG_MAX_RATIO
        return;                                      // Returning a "1" means "ray hit a wall"
      }      // End if hit
    } else { // else distance to Y wall < distance to X wall
      ray.x += (dy * cos) / sin;
      ray.y += dy;
      ray.hit = getmap(ray.x, ray.y);
      if(ray.hit > 127) {                            // if ray hits a wall (a block)
          ray.face = sin>0 ? 3 : 1;                    // hit south or north face of block
        ray.offset = sin>0 ? ray.x&63 : 64-(ray.x&63); // Get offset: offset is where on wall ray hits: 0 (left edge) to 63 (right edge)
          ray.dist = ((ray.y - start_y) << 16) / sin;  // Distance ray traveled.    <<16 = * TRIG_MAX_RATIO
        return;                                      // Returning 1: means "ray hit a wall"
      } // End if hit
    } // End else Xlen<Ylen
  } while(ray.hit>0); } while (!((sin<0&&ray.y<0) || (sin>0&&ray.y>=(mapsize<<6)) || (cos<0&&ray.x<0) || (cos>0&&ray.x>=(mapsize<<6))) );
  // loop while ray is in bounds or not going further out of bounds
  
  // ray will never hit a wall (out of bounds AND going further out of bounds)
   ray.hit = 0;           // Never hit, so set to out-of-bounds block type (0)
  ray.face = 0;
  ray.dist = 0xFFFFFFFF;
  return;
}

// ------------------------------------------------------------------------ //
//  Drawing to screen functions
// ------------------------------------------------------------------------ //
void fill_window(GContext *ctx, uint8_t *data) {
  for(uint16_t y=0, yaddr=0; y<168; y++, yaddr+=20)
    for(uint16_t x=0; x<19; x++)
      ((uint8_t*)(((GBitmap*)ctx)->addr))[yaddr+x] = data[y%8];
}

static void draw_textbox(GContext *ctx, GRect textframe, char *text) {
    graphics_context_set_fill_color(ctx, 0);   graphics_fill_rect(ctx, textframe, 0, GCornerNone);  //Black Solid Rectangle
    graphics_context_set_stroke_color(ctx, 1); graphics_draw_rect(ctx, textframe);                //White Rectangle Border  
    graphics_context_set_text_color(ctx, 1);  // White Text
    graphics_draw_text(ctx, text, fonts_get_system_font(FONT_KEY_GOTHIC_14), textframe, GTextOverflowModeWordWrap, GTextAlignmentLeft, NULL);  //Write Text
}


// 1-pixel-per-square map:
//   for (int16_t x = 0; x < mapsize; x++) for (int16_t y = 0; y < mapsize; y++) {graphics_context_set_stroke_color(ctx, map[y*mapsize+x]>0?1:0); graphics_draw_pixel(ctx, GPoint(x, y));}
static void draw_map(GContext *ctx, GRect box, int32_t zoom) {
  // note: Currently doesn't handle drawing beyond screen boundaries
  // zoom = pixel size of each square
  uint32_t *ctx32 = ((uint32_t*)(((GBitmap*)ctx)->addr));
  uint32_t xbit;
  int32_t x, y, yaddr, xaddr, xonmap, yonmap, yonmapinit;
  
  xonmap = ((player.x*zoom)>>6) - (box.size.w/2);  // Divide by ZOOM to get map X coord, but rounds [-ZOOM to 0] to 0 and plots it, so divide by ZOOM after checking if <0
  yonmapinit = ((player.y*zoom)>>6) - (box.size.h/2);
  for(x=0; x<box.size.w; x++, xonmap++) {
    xaddr = (x+box.origin.x) >> 5;        // X memory address
    xbit = ~(1<<((x+box.origin.x) & 31)); // X bit shift level (normally wouldn't invert it with ~, but ~ is used more often than not)
    if(xonmap>=0 && xonmap<(mapsize*zoom)) {
      yonmap = yonmapinit;
      yaddr = box.origin.y * 5;           // Y memory address
      for(y=0; y<box.size.h; y++, yonmap++, yaddr+=5) {
        if(yonmap>=0 && yonmap<(mapsize*zoom)) {             // If within Y bounds
          if(map[(((yonmap/zoom)*mapsize))+(xonmap/zoom)]>127) //   Map shows a wall >127
            ctx32[xaddr + yaddr] |= ~xbit;                   //     White dot (un-invert xbit by inverting it again)
          else                                               //   Map shows <= 0
            ctx32[xaddr + yaddr] &= xbit;                    //     Black dot
        } else {                                             // Else: Out of Y bounds
          ctx32[xaddr + yaddr] &= xbit;                      //   Black dot
        }
      }
    } else {                                // Out of X bounds: Black vertical stripe
      for(yaddr=box.origin.y*5; yaddr<((box.size.h + box.origin.y)*5); yaddr+=5)
        ctx32[xaddr + yaddr] &= xbit;
    }
  }

  graphics_context_set_fill_color(ctx, (time_ms(NULL, NULL) % 250)>125?0:1);                      // Flashing dot
  graphics_fill_rect(ctx, GRect((box.size.w/2)+box.origin.x - 1, (box.size.h/2)+box.origin.y - 1, 3, 3), 0, GCornerNone); // Square Cursor

  graphics_context_set_stroke_color(ctx, 1); graphics_draw_rect(ctx, GRect(box.origin.x-1, box.origin.y-1, box.size.w+2, box.size.h+2)); // White Border
}
int32_t Q1=0, Q2=0, Q3=0, Q4=0, Q5=0;
// implement more options
//draw_3D_wireframe?  draw_3D_shaded?
static void draw_3D(GContext *ctx, GRect box) { //, int32_t zoom) {
  int32_t colheight, angle; //colh, z;
  uint32_t x, xaddr, xbit, yaddr;
  uint32_t *target;
  
  uint32_t *ctx32 = ((uint32_t*)(((GBitmap*)ctx)->addr));
  
  // Draw Box around view (not needed if fullscreen)
  if(view_border) {graphics_context_set_stroke_color(ctx, 1); graphics_draw_rect(ctx, GRect(box.origin.x-1, box.origin.y-1, box.size.w+2, box.size.h+2));}  //White Rectangle Border

  // Draw background
    // Umm... ok... A nice black background.  Done.  Next?
    // Draw Sky from horizion on up
    //graphics_context_set_fill_color(ctx, 1); graphics_fill_rect(ctx, GRect(box.x, box.origin.y, box.size.w, box.size.h/2), 0, GCornerNone); // White Sky  (Lightning?  Daytime?)
  uint32_t farthest = 0;
  
  for(int16_t col = 0; col < box.size.w; col++) {  // Begin RayTracing Loop
    angle = (fov * (col - (box.size.w/2))) / box.size.w;
    
    x = col+box.origin.x;  // X screen coordinate
    xaddr = x >> 5;        // X memory address (for which 32bit screen memory word)
     xbit = x & 31;        // X bit-shift amount (for which bit within screen memory 32bit word)
    
    shoot_ray(player.x, player.y, player.facing + angle);  //Shoot rays out of player's eyes.  pew pew.
    ray.hit &= 127;                                        // If ray hit a block (>127) or not (<128), set ray.hit to valid block type [0-127]
    dist[col] = ray.dist;                                  // save distance of this column for sprite rendering later
    if(ray.dist > farthest) farthest = ray.dist;           // farthest (furthest?) wall (for sprite rendering. only render sprites closer than farthest wall)
    ray.dist = ray.dist * cos_lookup(angle);               // multiply by cos to stop fisheye lens (should be >>16 to get actual dist, as is done often below)
    
      // Calculate amount of shade
      //z =  ray.dist >> 16; //z=(ray.dist*cos_lookup(angle))/TRIG_MAX_RATIO;  // z = distance
      //z -= 64; if(z<0) z=0;   // Make everything 1 block (64px) closer (solid white without having to be nearly touching)
      //z = sqrt_int(z,10) >> 1; // z was 0-RANGE(max dist visible), now z = 0 to 12: 0=close 10=distant.  Square Root makes it logarithmic
      //z -= 2; if(z<0) z=0;    // Closer still (zWas=zNow: 0-64=0, 65-128=2, 129-192=3, 256=4, 320=6, 384=6, 448=7, 512=8, 576=9, 640=10)

      colheight = (box.size.h << 22) /  ray.dist;  // wall segment height = box.size.h * wallheight * 64(the "zoom factor") / (distance >> 16)
      colheight = (colheight>box.size.h) ? box.size.h/2 : colheight/2;   // Make sure line isn't drawn beyond bounding box (also halve it cause of 2 32bit textures)
      
      // Texture the Ray hit, point to 1st half of texture (half, cause a 64x64px texture menas there's 2 uint32_t per texture row.  Also why * 2 below)
      target = (uint32_t*)texture[squaretype[ray.hit].face[ray.face]]->addr + ray.offset * 2;// maybe use GBitmap's size veriables to store texture size?
    
      yaddr = ((box.origin.y + (box.size.h/2)) * 5);   // Y Address of vertical center = Y screen coordinate * 5
      for(int32_t i=0, yoffset=0; i<colheight; i++, yoffset+=5) {
        int32_t ch = (i * ray.dist / box.size.h) >> 16; // ch = which pixel of the texture is hit (0-31)
        // total_column_height = box.size.h / (dist>>16) (Total height of 32pixel wall half. This isn't clipped to screen, so could be thousands of pixels tall)
        // i IS clipped to screen and goes from 3D view's middle to 3D view's top/bottom edge
        // which pixel in the texture hit is i/total_colum_height = i / (box.size.h / (dist>>16)) = (i * dist / box.size.h) >> 16
        ctx32[xaddr + yaddr - yoffset] |= (((*target >> (31-ch))&1) << xbit);  // Draw Top Half
        ctx32[xaddr + yaddr + yoffset] |= (((*(target+1)  >> ch)&1) << xbit);  // Draw Bottom Half
      }
    
    // Draw Floor/Ceiling
    for(int32_t i=colheight; i<box.size.h/2; i++) {
      int32_t map_x = player.x + (((box.size.h << 5) * cos_lookup(player.facing + angle)) / (i * cos_lookup(angle))); // map_x & map_y = spot on map the screen pixel points to
      int32_t map_y = player.y + (((box.size.h << 5) * sin_lookup(player.facing + angle)) / (i * cos_lookup(angle))); // map_y = player.y + distance_y, distance = 64 * (sin if y, cos if x) / i (/cos to un-fisheye)
      
      ray.hit = getmap(map_x, map_y) & 127;       // ceiling/ground of which cell is hit

      if(squaretype[ray.hit].floor<MAX_TEXTURES) // If ceiling texture exists (else just show sky)
        ctx32[((box.origin.y + (box.size.h/2) + i) * 5) + xaddr] |= (((*( ((uint32_t*)texture[squaretype[ray.hit].floor]->addr + (map_x&63) * 2) + ((map_y&63) >> 5)) >> (map_y&31))&1) << xbit);
      if(squaretype[ray.hit].ceiling<MAX_TEXTURES) // If floor texture exists (else just show abyss)
        ctx32[((box.origin.y + (box.size.h/2) - i) * 5) + xaddr] |= (((*( ((uint32_t*)texture[squaretype[ray.hit].ceiling]->addr + (map_x&63) * 2) + ((map_y&63) >> 5)) >> (map_y&31))&1) << xbit);
    } // End Floor/Ceiling
    
  
  } //End For (End RayTracing Loop)
  
  // Draw Sprites!
  // Sort sprites by distance from player
  // draw sprites in order from farthest to closest
  // sprite:
  // x
  // y
  // angle
  // distance
  // type
  // d
  int32_t diffx, diffy, dist;
  diffx=sprite.x - player.x;
  diffy=sprite.y - player.y;
  //diffx = player.x - sprite.x;
  //diffy = player.y - sprite.y;
  dist=sqrt32(diffx*diffx + diffy*diffy);
  Q1=dist;
  //dist = dist * cos_lookup(angle);           // multiply by cos to stop fisheye lens (should be >>16 to get actual dist, as is done often below)
  int32_t spritesize = ((32*64)) /  dist; // << >> 16
  Q2 = spritesize;
  angle = atan2_lookup(diffy, diffx); // angle between player's x,y and sprite's x,y
  Q3=angle;
  //angle=player.facing - angle;          // angle between center column and sprite
  angle=angle - player.facing;          // angle between center column and sprite
  angle=((angle+TRIG_MAX_ANGLE+32768)%TRIG_MAX_ANGLE)-32768; // convert angle to [-32768 to 32767]
  Q4=angle;
  //angle now is angle from center column. 0=center of view, -fov/2 is left view edge, fov/2 is right view edge
  int32_t col = (box.size.w/2) + ((box.size.w/2) * (angle) / (fov/2));  // convert angle to on-screen column
  Q5=col;
  if((col+(spritesize/2))>=0 && (col-(spritesize/2))<box.size.w) { // if any of the sprite is within view
    int16_t colmin = box.origin.x + ((col<=(spritesize/2)) ? 0 : (col-(spritesize/2)));
    int16_t colmax = box.origin.x + (((col+(spritesize/2))>=box.size.w) ? box.size.w : (col+(spritesize/2)));
    int16_t   ymin = box.origin.y + ((box.size.h<=spritesize) ? 0 : ((box.size.h-spritesize)/2));
    int16_t   ymax = box.origin.y + ((spritesize/2)>=(box.size.h/2) ? box.size.h : ((box.size.h+spritesize)/2));
    graphics_context_set_stroke_color(ctx, 1);
    for(int16_t x = colmin; x < colmax; x++)
      for(int16_t y = ymin; y < ymax; y++)
        graphics_draw_pixel(ctx, GPoint(x, y));
    graphics_context_set_stroke_color(ctx, 0);
    for(int16_t x = colmin; x < colmax; x++) {
      graphics_draw_pixel(ctx, GPoint(x, ymin));
      graphics_draw_pixel(ctx, GPoint(x, ymax-1));
    }
    for(int16_t y = ymin; y < ymax; y++) {
        graphics_draw_pixel(ctx, GPoint(colmin, y));
        graphics_draw_pixel(ctx, GPoint(colmax-1, y));
    }
    
  }
  //graphics_fill_rect(ctx, GRect(box.origin.x + col-(spritesize/2),box.origin.y + (box.size.h - spritesize)/2,spritesize,spritesize), 0, GCornerNone);
  
  /*
int32_t diffx, diffy, dist;
  diffx=sprite.x - player.x;
  diffy=sprite.y - player.y;
  dist=sqrt32(diffx*diffx + diffy*diffy);
  Q1=dist;
  
  dist = dist * cos_lookup(angle);           // multiply by cos to stop fisheye lens (should be >>16 to get actual dist, as is done often below)
  
  int32_t spritesize = ((32*64) << 16) /  dist;  // 32 pixel width / (dist>>16)
  if(spritesize>168) spritesize=168;
  Q2 = spritesize;
  angle = atan2_lookup(diffy, diffx);
  Q3=angle;
  //angle = angle - player.facing;
  int32_t col = (box.size.w/2) + ((box.size.w/2) * angle / (fov/2));
  Q4=col;
  graphics_context_set_stroke_color(ctx, GColorWhite);
  if(col>0 && col<144)
    graphics_draw_line(ctx, GPoint(col,(box.size.h/2)-spritesize), GPoint(col,(box.size.h/2)+spritesize));
  //angle = (fov * (col - (box.size.w>>1))) / box.size.w;
    
   // x = col+box.origin.x;  // X screen coordinate
   // xaddr = x >> 5;        // X memory address (for which 32bit screen memory word)
   // xbit = x & 31;        // X bit-shift amount (for which bit within screen memory 32bit word)
   // dist[col] = ray.dist;  
   */
}

static void graphics_layer_update_proc(Layer *me, GContext *ctx) {
  static char text[40];  //Buffer to hold text
  time_t sec1, sec2; uint16_t ms1, ms2, dt; // time snapshot variables, to calculate render time and FPS
  time_ms(&sec1, &ms1);  //1st Time Snapshot
  
  //draw_3D(ctx,  GRect(view_x, view_y, view_w, view_h));
  //draw_3D(ctx,  view);
  draw_3D(ctx,  GRect(1, 25, 142, 128));
  draw_map(ctx, GRect(4, 110, 40, 40), 4);
  
  time_ms(&sec2, &ms2);  //2nd Time Snapshot
  dt = (uint16_t)(1000*(sec2 - sec1)) + (ms2 - ms1);  //dt=delta time: time between two time snapshots in milliseconds
  
  snprintf(text, sizeof(text), "(%ld,%ld) %ld %dms %dfps %d", player.x>>6, player.y>>6, player.facing, dt, 1000/dt, getmap(player.x,player.y));  // What text to draw
  snprintf(text, sizeof(text), "(%ld,%ld) %ld\n%ld %ld %ld %ld %ld", player.x, player.y, player.facing, Q1,Q2,Q3,Q4,Q5);  // What text to draw
  draw_textbox(ctx, GRect(0, 0, 143, 32), text);
   
  //  Set a timer to restart loop in 50ms
  if(dt<40 && dt>0) // if time to render is less than 40ms, force framerate of 20FPS or worse
     app_timer_register(50-dt, main_loop, NULL); // 10FPS
  else
     app_timer_register(10, main_loop, NULL);     // took longer than 40ms, loop  in 10ms (asap)
}


// ------------------------------------------------------------------------ //
//  Button Pushing
// ------------------------------------------------------------------------ //
void up_push_in_handler(ClickRecognizerRef recognizer, void *context) {up_button_depressed = true;
                                                                      GenerateMazeMap(mapsize/2, 0);
                                                                      }
void up_release_handler(ClickRecognizerRef recognizer, void *context) {up_button_depressed = false;}
void dn_push_in_handler(ClickRecognizerRef recognizer, void *context) {dn_button_depressed = true;}
void dn_release_handler(ClickRecognizerRef recognizer, void *context) {dn_button_depressed = false;}
void sl_push_in_handler(ClickRecognizerRef recognizer, void *context) {sl_button_depressed = true;   // SELECT button was pushed in
  shoot_ray(player.x, player.y, player.facing);             // Shoot Ray from center of screen.  If it hit something:
  //if(ray.hit>127) 
  setmap(ray.x, ray.y, ray.hit&127); // If you shoot a block, remove it.
  //if(ray.hit==128+1) setmap(ray.x, ray.y, 1);   // If Ray hit normal block(1), change it to a Circle Block (3) (Changed from Mirror Block(4), as it was confusing)
  //if(ray.hit==128+3) setmap(ray.x, ray.y, 1);   // If Ray hit Circle Block(3), change it to a Normal Block (1)
}
void sl_release_handler(ClickRecognizerRef recognizer, void *context) {sl_button_depressed = false;}

static void click_config_provider(void *context) {
  window_raw_click_subscribe(BUTTON_ID_UP, up_push_in_handler, up_release_handler, context);
  window_raw_click_subscribe(BUTTON_ID_DOWN, dn_push_in_handler, dn_release_handler, context);
  window_raw_click_subscribe(BUTTON_ID_SELECT, sl_push_in_handler, sl_release_handler, context);
  //window_single_click_subscribe(BUTTON_ID_SELECT, sl_push_in_handler);
}

// ------------------------------------------------------------------------ //
//  Main Program Structure
// ------------------------------------------------------------------------ //
static void window_load(Window *window) {
  Layer *window_layer = window_get_root_layer(window);
  window_frame = layer_get_frame(window_layer);

  graphics_layer = layer_create(window_frame);
  layer_set_update_proc(graphics_layer, graphics_layer_update_proc);
  layer_add_child(window_layer, graphics_layer);
}

static void window_unload(Window *window) {
  layer_destroy(graphics_layer);
}

static void init(void) {
  window = window_create();
  window_set_click_config_provider(window, click_config_provider);
  window_set_window_handlers(window, (WindowHandlers) {
    .load = window_load,
    .unload = window_unload
  });
  window_set_fullscreen(window, true);  // Get rid of the top bar
  window_stack_push(window, false /* False = Not Animated */);
  window_set_background_color(window, GColorBlack);
  accel_data_service_subscribe(0, NULL);  // Start accelerometer
  
  srand(time(NULL));  // Seed randomizer so different map every time
  player = (struct PlayerStruct){.x=(64*(mapsize/2)), .y=(-2 * 64), .facing=10000};    // Seems like a good place to start
  sprite = (struct PlayerStruct){.x=(2 * 64), .y=(64*(mapsize/2)), .facing=10000};    // sprite position
  //GenerateRandomMap();                // generate a randomly dotted map
  //GenerateMazeMap(mapsize/2, 0);    // generate a random maze, enterane on middle of top side
  GenerateSquareMap();
  
  view = GRect(1, 25, 142, 128); // size and placement of 3D window
  // MainLoop() automatically called with dirty layer drawing
  LoadMapTextures(); // Load textures
}

static void deinit(void) {
  accel_data_service_unsubscribe();
  window_destroy(window);
  UnLoadMapTextures();
}

int main(void) {
  init();
  app_event_loop();
  deinit();
}

// ------------------------------------------------------------------------ //
//  Footnotes
// ------------------------------------------------------------------------ //
// 1: if(abs32(dx * sin) < abs32(dy * cos)) {
//   translates to: if("distance to x wall" is smaller than "distance to y wall")
//      Used to be: if((dx*TRIG_MAX_RATIO/cos) < (dy*TRIG_MAX_RATIO/sin)).  Added abs cause of sign changes
//   & before that: if(sqrt(dxx*dxx + dxy*dxy) < sqrt(dyx*dyx + dyy*dyy)), though got rid of "sqrt" on both sides
//   To learn more: www.permadi.com/tutorial/raycast/rayc8.html
