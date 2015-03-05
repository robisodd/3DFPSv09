/**********************************************************************************
   Pebble 3D FPS Engine v0.9 beta
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
        Got rid of FOV!  Now uses arctan to determine angle; MUCH straighter walls and better positioning
        Got rid of view GRect
        player.facing is now int16 -- removed all player.facing%TRIG_MAX_ANGLE and other large angle corrections
        
  To Do:
        Maybe X&Y coordinates can be int16 (max board of 512x512=256kB)
        Texture looping
        Add switches on walls to change cells
        Open doors
        Ceiling height?  Crushing
        
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
              Currently texture is invisible from the inside
            Darkness Amount (and if distance shading is on/off)
            Permissable to walk through
            Permissable for enemies to walk through
            Permissable for bullets/missles/magic?
            Invert texture bit? uint8_t = 4 walls + ceiling + floor + 2 spare bits
            Maybe ray modifying? e.g. Mirror or Light-bending gravity well?  No, nevermind.
            Note: Square_Type[0] is out of bounds render type
            
        Sprite shading is the shading of the square it's in.
                      
Definitions:
  Object: enemy or item or other thing
  Sprite: object as displayed on screen
          Tile: 
          Square: X&Y on map
                  10x10 map has 100 squares
                  each square is 8bits: High bit=ray hits walls, 7bits=squaretype
                  = coordinate / 64 (aka >>6)
          SquareType: [0-127] Type of square for the map
                      Holds information about textures, passible, etc
          Map: Grid (or Array) of squares
               Mabye should rename to Level or PlayField? Map sounds like 2D top-down
          Wall:  What a ray hits, usually impassible by player
          Coordinate: X & Y position on map
                      Pixel specific, 64 per square
                      e.g. 10x10 map has 640x640=409600 coordinate points
          Shading:
          

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

#define mapsize 21             // Map is mapsize x mapsize squares big
#define MAX_TEXTURES 10        // Most number of textures there's likely to be.  Feel free to increase liberally, but no more than 254.
#define IDCLIP false           // Walk thru walls
#define view_border true       // Draw border around 3D viewing window

static uint8_t map[mapsize * mapsize];  // 0-255, 0-127 are squaretypes[], high bit set means ray hits wall

GBitmap *texture[MAX_TEXTURES];
GBitmap *sprite_image[1];
GBitmap *sprite_mask[1];

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
  int16_t facing;             // Player Direction Facing (from 0 - TRIG_MAX_ANGLE)
} PlayerStruct;
static PlayerStruct player;

static PlayerStruct object;

typedef struct ObjectStruct {
  int32_t x;                  // Player's X Position (64 pixels per square)
  int32_t y;                  // Player's Y Position (64 pixels per square)
  int16_t health;             //
  uint8_t type;               // Enemy, Lamp, Coin, etc
  uint8_t sprite;             // sprite_image[] and sprite_mask[] for object
  int32_t data1;              // 
  int32_t data2;              // 
} ObjectStruct;

typedef struct RayStruct {
   int32_t x;                 // x coordinate the ray hit
   int32_t y;                 // y coordinate the ray hit
  uint32_t dist;              // length of the ray, i.e. distance ray traveled
   uint8_t hit;               // square_type the ray hit [0-127]
   int32_t offset;            // horizontal spot on texture the ray hit [0-63] (used in memory pointers so int32_t)
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

// ------------------------------------------------------------------------ //

// square root
#define root_depth 10          // How many iterations square root function performs
int32_t sqrt32(int32_t a) {int32_t b=a; for(int8_t i=0; i<root_depth; i++) b=(b+(a/b))/2; return b;} // Square Root
int32_t sqrt_int(int32_t a, int8_t depth){int32_t b=a; for(int8_t i=0; i<depth; i++) b=(b+(a/b))/2; return b;} // Square Root

// absolute value
int32_t abs32(int32_t x) {return (x^(x>>31)) - (x>>31);}
int16_t abs16(int16_t x) {return (x^(x>>15)) - (x>>15);}
int8_t  abs8 (int8_t  x) {return (x^(x>> 7)) - (x>> 7);}
int32_t abs_int(int32_t a){return (a<0 ? 0 - a : a);} // Absolute Value (might be faster than above)

// sign function returns: -1 or 0 or 1 if input is <0 or =0 or >0
int8_t  sign8 (int8_t  x){return (x > 0) - (x < 0);}
int16_t sign16(int16_t x){return (x > 0) - (x < 0);}
int32_t sign32(int32_t x){return (x > 0) - (x < 0);}

// ------------------------------------------------------------------------ //
//  Map Functions
// ------------------------------------------------------------------------ //
uint8_t getmap(int32_t x, int32_t y) {
  x>>=6; y>>=6;
  return (x<0 || x>=mapsize || y<0 || y>=mapsize) ? 0 : map[(y * mapsize) + x];
}

void setmap(int32_t x, int32_t y, uint8_t value) {
  x>>=6; y>>=6;
  if((x >= 0) && (x < mapsize) && (y >= 0) && (y < mapsize))
    map[y * mapsize + x] = value;
}

void LoadMapTextures() {
  
/*
  const int ANIM_IMAGE_RESOURCE_IDS[] = {
  RESOURCE_ID_FRAME_0,
  RESOURCE_ID_FRAME_1,
  RESOURCE_ID_FRAME_2,
  RESOURCE_ID_FRAME_3,
  RESOURCE_ID_FRAME_4,
  RESOURCE_ID_FRAME_5
};
*/
  
  texture[0] = gbitmap_create_with_resource(RESOURCE_ID_STONE);
  texture[1] = gbitmap_create_with_resource(RESOURCE_ID_WALL_FIFTY);
  texture[2] = gbitmap_create_with_resource(RESOURCE_ID_WALL_CIRCLE);
  texture[3] = gbitmap_create_with_resource(RESOURCE_ID_FLOOR_TILE);
  texture[4] = gbitmap_create_with_resource(RESOURCE_ID_CEILING_LIGHTS);
  texture[5] = gbitmap_create_with_resource(RESOURCE_ID_WALL_BRICK);
  texture[6] = gbitmap_create_with_resource(RESOURCE_ID_GRASS);

  sprite_image[0] = gbitmap_create_with_resource(RESOURCE_ID_SPRITE_SMILEY);
  sprite_mask[0] = gbitmap_create_with_resource(RESOURCE_ID_SPRITE_SMILEY_MASK);
}

void UnLoadMapTextures() {
  for(uint8_t i=0; i<MAX_TEXTURES; i++)
    if(texture[i])
      gbitmap_destroy(texture[i]);
}

void GenerateSquareMap() {
  squaretype[0].ceiling = 255; // outside sky
  squaretype[0].floor   = 6;   // outside grass
  squaretype[0].face[0]=squaretype[0].face[1]=squaretype[0].face[2]=squaretype[0].face[3]=0;

  squaretype[1].face[0] = 1;
  squaretype[1].face[1] = 2;
  squaretype[1].face[2] = 5;
  squaretype[1].face[3] = 6;
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
  map[((mapsize/2) * mapsize) + (mapsize/2)] = 128+1;  // middle block

  
   player.x = 1 * 64; player.y = (64*mapsize)/2; player.facing=0;    // start inside
   object.x = 2 * 64; object.y = (64*mapsize)/2; object.facing=0;    // sprite position
   //player.x = 6 * 32 + 16; player.y = (64*mapsize)/2; player.facing=TRIG_MAX_ANGLE/2;    // start inside
   //object.x = 3 * 32;      object.y = (64*mapsize)/2; object.facing=0;    // sprite position
//  player.x = ((64*mapsize)/2)-64; player.y = (64*mapsize)/2; player.facing=0;    // start inside
//  object.x = (64*mapsize)/2; object.y = (64*mapsize)/2; object.facing=0;    // sprite position
  //setmap(object.x, object.y, 0);
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
  squaretype[1].face[0]=squaretype[1].face[1]=squaretype[1].face[2]=squaretype[1].face[3] = 5;
  
  squaretype[2].ceiling = 2;
  squaretype[2].floor = 4;
  squaretype[2].face[0]=squaretype[2].face[1]=squaretype[2].face[2]=squaretype[2].face[3] = 5;

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
        for (int16_t i=0; i<mapsize*mapsize; i++) if(map[i]==0) map[i] = 128+1; // convert map bits (1=empty, 128+1=wall, 2=special)
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
  walk(player.facing, accel.y>>5);                        // walk based on accel.y
  if(dn_button_depressed)                                 // if down button is held
    walk(player.facing + (1<<14), accel.x>>5);            //   strafe (1<<14 = TRIG_MAX_ANGLE / 4)
  else                                                    // else
    player.facing = (player.facing + (accel.x<<3));       //   spin
  layer_mark_dirty(graphics_layer);                       // tell pebble to draw when it's ready
}

//shoot_ray(x, y, angle)
//  x, y = position on map to shoot the ray from
//  angle = direction to shoot the ray (in Pebble angle notation)
//modifies: global RayStruct ray
//    uses: getmap(), abs32()
void shoot_ray(int32_t start_x, int32_t start_y, int32_t angle) {
  int32_t sin, cos, dx, dy, nx, ny;

    sin = sin_lookup(angle);
    cos = cos_lookup(angle);
  ray.x = start_x;// + (cos>>11); // fixes fisheye, but puts you inside walls if you're too close. was ((32*cos)>>16), 32 being dist from player to edge of view plane
  ray.y = start_y;// + (sin>>11); //
     ny = sin>0 ? 64 : -1;
     nx = cos>0 ? 64 : -1;

  do {
    do {
      dy = ny - (ray.y & 63);                        //   north-south component of distance to next east-west wall
      dx = nx - (ray.x & 63);                        //   east-west component of distance to next north-south wall
  
      if(abs32(dx * sin) < abs32(dy * cos)) {        // if(distance to north-south wall < distance to east-west wall) See Footnote 1
        ray.x += dx;                                   // move ray to north-south wall: x part
        ray.y += ((dx * sin) / cos);                   // move ray to north-south wall: y part
        ray.hit = getmap(ray.x, ray.y);                // see what the ray is at on the map
        if(ray.hit > 127) {                            // if ray hits a wall (a block)
          ray.face = cos>0 ? 2 : 0;                      // hit west or east face of block
          ray.offset = cos>0 ? 63-(ray.y&63) : ray.y&63; // Offset is where on wall ray hits: 0 (left edge) to 63 (right edge)
          ray.dist = ((ray.x - start_x) << 16) / cos;    // Distance ray traveled.    <<16 = * TRIG_MAX_RATIO
          return;                                      // Exit
        }
      } else {                                       // else: distance to Y wall < distance to X wall
        ray.x += (dy * cos) / sin;                     // move ray to east-west wall: x part
        ray.y += dy;                                   // move ray to east-west wall: y part
        ray.hit = getmap(ray.x, ray.y);                // see what the ray is at on the map
        if(ray.hit > 127) {                            // if ray hits a wall (a block)
            ray.face = sin>0 ? 3 : 1;                    // hit south or north face of block
          ray.offset = sin>0 ? ray.x&63 : 63-(ray.x&63); // Get offset: offset is where on wall ray hits: 0 (left edge) to 63 (right edge)
            ray.dist = ((ray.y - start_y) << 16) / sin;  // Distance ray traveled.    <<16 = * TRIG_MAX_RATIO
          return;                                      // Exit
        }
      }                                              // End if/then/else (x dist < y dist)
      
    } while(ray.hit>0);  //loop while ray is not out of bounds
  } while (!((sin<0&&ray.y<0) || (sin>0&&ray.y>=(mapsize<<6)) || (cos<0&&ray.x<0) || (cos>0&&ray.x>=(mapsize<<6))) ); // loop if ray is not going further out of bounds
  
  // ray will never hit a wall (out of bounds AND going further out of bounds)
  ray.hit  = 0;           // Never hit, so set to out-of-bounds block type (0)
  ray.face = 0;           // TODO: set face to face wall hit on block type 0 //ray.face = sin>0 ? (cos>0 ? 2 : 0) : (sin>0 ? 3 : 1);
  ray.dist = 0x7FFFFFFF;  // Never hits makes distance effectively infinity. 7F instead of FF cause of signed/unsigned conversion issues
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
    graphics_draw_text(ctx, text, fonts_get_system_font(FONT_KEY_GOTHIC_14), textframe, GTextOverflowModeWordWrap, GTextAlignmentCenter, NULL);  //Write Text
}


// 1-pixel-per-square map:
//   for (int16_t x = 0; x < mapsize; x++) for (int16_t y = 0; y < mapsize; y++) {graphics_context_set_stroke_color(ctx, map[y*mapsize+x]>0?1:0); graphics_draw_pixel(ctx, GPoint(x, y));}
static void draw_map(GContext *ctx, GRect box, int32_t zoom) {
  // note: Currently doesn't handle drawing beyond screen boundaries
  // zoom = pixel size of each square
  uint32_t *ctx32 = ((uint32_t*)(((GBitmap*)ctx)->addr));  // framebuffer pointer (screen memory)
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

//mid_y = (box.size.h/2) or maybe box.origin.y + (box.size.h/2) (middle in view or pixel on screen)
//mid_x = (box.size.w/2) or maybe box.origin.x + (box.size.w/2)
 int32_t dx, dy;
  int16_t angle;
  int32_t farthest = 0; //colh, z;
  int32_t y, colheight, halfheight;
  uint32_t x, addr, xaddr, yaddr, xbit, xoffset, yoffset;
  uint32_t *target, *mask;
  int32_t dist[144];                 // Array of non-cos adjusted distance for each vertical wall segment -- for sprite rendering
  halfheight = box.size.h/2;
  uint32_t *ctx32 = ((uint32_t*)(((GBitmap*)ctx)->addr));  // framebuffer pointer (screen memory)
  
  // Draw Box around view (not needed if fullscreen)
  //TODO: Draw straight to framebuffer
  if(view_border) {graphics_context_set_stroke_color(ctx, 1); graphics_draw_rect(ctx, GRect(box.origin.x-1, box.origin.y-1, box.size.w+2, box.size.h+2));}  //White Rectangle Border

  // Draw background
    graphics_context_set_fill_color(ctx, 0);  graphics_fill_rect(ctx, box, 0, GCornerNone); // Black background
    // Draw Sky from horizion on up, rotate based upon player angle
    //graphics_context_set_fill_color(ctx, 1); graphics_fill_rect(ctx, GRect(box.origin.x, box.origin.y, box.size.w, box.size.h/2), 0, GCornerNone); // White Sky  (Lightning?  Daytime?)

  for(int16_t col = 0; col < box.size.w; col++) {        // Begin RayTracing Loop
    angle = atan2_lookup((64*col/box.size.w) - 32, 64);    // dx = (64*(col-(box.size.w/2)))/box.size.w; dy = 64; angle = atan2_lookup(dx, dy);
    
    shoot_ray(player.x, player.y, player.facing + angle);  //Shoot rays out of player's eyes.  pew pew.
    ray.hit &= 127;                                        // Whether ray hit a block (>127) or not (<128), set ray.hit to valid block type [0-127]
    if(ray.dist > (uint32_t)farthest) farthest = ray.dist; // farthest (furthest?) wall (for sprite rendering. only render sprites closer than farthest wall)
    dist[col] = (uint32_t)ray.dist;                        // save distance of this column for sprite rendering later
    ray.dist *= cos_lookup(angle);                         // multiply by cos to stop fisheye lens (should be >>16 to get actual dist, but that's all done below)
//  ray.dist <<= 16;    // use this if commenting out "ray.dist*=cos" above, cause it >>16's a lot below
    
      // Calculate amount of shade
      //z =  ray.dist >> 16; //z=(ray.dist*cos_lookup(angle))/TRIG_MAX_RATIO;  // z = distance
      //z -= 64; if(z<0) z=0;   // Make everything 1 block (64px) closer (solid white without having to be nearly touching)
      //z = sqrt_int(z,10) >> 1; // z was 0-RANGE(max dist visible), now z = 0 to 12: 0=close 10=distant.  Square Root makes it logarithmic
      //z -= 2; if(z<0) z=0;    // Closer still (zWas=zNow: 0-64=0, 65-128=2, 129-192=3, 256=4, 320=6, 384=6, 448=7, 512=8, 576=9, 640=10)

      colheight = (box.size.h << 21) /  ray.dist;    // half wall segment height = box.size.h * wallheight * 64(the "zoom factor") / (distance >> 16) // now /2 (<<21 instead of 22)
      if(colheight>halfheight) colheight=halfheight; // Make sure line isn't drawn beyond bounding box (also halve it cause of 2 32bit textures)
      
      // Texture the Ray hit, point to 1st half of texture (half, cause a 64x64px texture menas there's 2 uint32_t per texture row.  Also why * 2 below)
      target = (uint32_t*)texture[squaretype[ray.hit].face[ray.face]]->addr + ray.offset*2;// maybe use GBitmap's size veriables to store texture size?
    
      x = col+box.origin.x;  // X screen coordinate
      addr = (x >> 5) + ((box.origin.y + halfheight) * 5); // 32bit memory word containing pixel vertically centered at X. (Address=xaddr + yaddr = (Pixel.X/32) + 5*Pixel.Y)
       xbit = x & 31;        // X bit-shift amount (for which bit within screen memory's 32bit word the pixel exists)
    
      y=0; yoffset=0;  // y is y +/- from vertical center, yoffset is the screen memory position of y (and is always = y*5)
      for(; y<colheight; y++, yoffset+=5) {
        xoffset = (y * ray.dist / box.size.h) >> 16; // xoffset = which pixel of the texture is hit (0-31).  See Footnote 2
        ctx32[addr - yoffset] |= (((*target >> (31-xoffset))&1) << xbit);  // Draw Top Half
        ctx32[addr + yoffset] |= (((*(target+1)  >> xoffset)&1) << xbit);  // Draw Bottom Half
      }

    // Draw Floor/Ceiling
    int32_t temp_x = (((box.size.h << 5) * cos_lookup(player.facing + angle)) / cos_lookup(angle)); // Calculate now to save time later
    int32_t temp_y = (((box.size.h << 5) * sin_lookup(player.facing + angle)) / cos_lookup(angle)); // Calculate now to save time later
//if(false)  // enable/disable floor and ceiling
    for(; y<halfheight; y++, yoffset+=5) {         // y and yoffset continue from wall top&bottom to view edge (unless wall is taller than view edge)
      int32_t map_x = player.x + (temp_x / y);     // map_x & map_y = spot on map the screen pixel points to
      int32_t map_y = player.y + (temp_y / y);     // map_y = player.y + dist_y, dist = (height/2 * 64 * (sin if y, cos if x) / i) (/cos to un-fisheye)
      ray.hit = getmap(map_x, map_y) & 127;        // ceiling/ground of which cell is hit.  &127 shouldn't be needed since it *should* be hitting a spot without a wall
      if(squaretype[ray.hit].floor<MAX_TEXTURES)   // If ceiling texture exists (else just show sky)
        ctx32[addr + yoffset] |= (((*( ((uint32_t*)texture[squaretype[ray.hit].floor]->addr + (map_x&63) * 2) + ((map_y&63) >> 5)) >> (map_y&31))&1) << xbit);
      if(squaretype[ray.hit].ceiling<MAX_TEXTURES) // If floor texture exists (else just show abyss)
        ctx32[addr - yoffset] |= (((*( ((uint32_t*)texture[squaretype[ray.hit].ceiling]->addr + (map_x&63) * 2) + ((map_y&63) >> 5)) >> (map_y&31))&1) << xbit);
    } // End Floor/Ceiling

  } //End For (End RayTracing Loop)
    
  // Draw Sprites!
  // Sort sprites by distance from player
  // draw sprites in order from farthest to closest
  // start from sprites closer than "farthest wall"
  // sprite:
  // x
  // y
  // angle
  // distance
  // type
  // d


 
  uint8_t numobjects=1;
  int32_t spritecol, objectdist;  //, xoffset, yoffset;
//if(false)  // enable/disable drawing of sprites
  for(uint8_t obj=0; obj<numobjects; obj++) {
    dx = object.x - player.x;
    dy = object.y - player.y;
    angle = atan2_lookup(dy, dx); // angle = angle between player's x,y and sprite's x,y
    objectdist =  (((dx^(dx>>31)) - (dx>>31)) > ((dy^(dy>>31)) - (dy>>31))) ? (dx<<16) / cos_lookup(angle) : (dy<<16) / sin_lookup(angle);
//     objectdist = (abs32(dx)>abs32(dy)) ? (dx<<16) / cos_lookup(angle) : (dy<<16) / sin_lookup(angle);
    angle = angle - player.facing;  // angle is now angle between center view column and object. <0=left of center, 0=center column, >0=right of center
    
    if(cos_lookup(angle)>0) { // if object is in front of player.  note: if angle = 0 then object is straight right or left of the player
      if(farthest>=objectdist) { // if ANY wall is further (or =) than object distance, then display it
        spritecol = (box.size.w/2) + ((sin_lookup(angle)*box.size.w)>>16);  // column on screen of sprite center

        int32_t objectwidth = 32;           // 32 pixels wide  TODO: maybe other sized objects?  Besides scaling?
        int32_t objectheight = 32;//16          // 32 pixels tall
        int32_t objectverticaloffset = 64-objectheight;//+32;//16; // normally center dot is vertically centered, + or - how far to move it.
//         int32_t spritescale = box.size.h ;// * 20 / 10;
        
        //objectdist = (objectdist * cos_lookup(angle)) >> 16;
        
        int32_t spritescale = (box.size.h);// * 20 / 10;
        int32_t spritewidth  = (spritescale * objectwidth) / objectdist;   // should be box.size.w, but wanna keep sprite h/w proportionate
        int32_t spriteheight = (spritescale * objectheight)/ objectdist;  // note: make sure to use not-cosine adjusted distance!
//         int32_t halfspriteheight = spriteheight/2;
        int32_t spriteverticaloffset = (objectverticaloffset * spritescale) / objectdist; // fisheye adjustment
//         int32_t spriteverticaloffset = ((((objectverticaloffset * spritescale) + (32*box.size.h)) << 16) / (objectdist * cos_lookup(angle))); // floor it
        
        
        int16_t sprite_xmin = spritecol - (spritewidth/2);
        int16_t sprite_xmax = sprite_xmin + spritewidth;  // was =spritecol+(spritewidth/2);  Changed to display whole sprite cause /2 loses info
        if(sprite_xmax>=0 && sprite_xmin<box.size.w) {    // if any of the sprite is horizontally within view
          int16_t xmin = sprite_xmin<0 ? 0: sprite_xmin;
          int16_t xmax = sprite_xmax>box.size.w ? box.size.w : sprite_xmax;


// Half through floor
//int32_t objectheight = 16;          // 32 pixels tall
//int32_t objectverticaloffset = 64-objectheight;//+32;//16; // normally center dot is vertically centered, + or - how far to move it.
          
// perfectly puts 32x32 sprite on ceiling
//int32_t objectwidth = 32;
//int32_t objectheight = 64;
//int32_t objectverticaloffset = 0;
//int32_t spritescale = box.size.h;
//int32_t spritewidth  = (spritescale * objectwidth) / objectdist;
//int32_t spriteheight = (spritescale * objectheight) / objectdist;
//int32_t spriteverticaloffset = ((objectverticaloffset * spritescale) << 16) / (objectdist * cos_lookup(angle)); // fisheye adjustment
//int16_t sprite_ymax = spriteverticaloffset + ((box.size.h + spriteheight)/2);// + (((32*box.size.h) << 16) / (objectdist * cos_lookup(angle)));
//int16_t sprite_ymin = sprite_ymax - spriteheight; // note: sprite is not cos adjusted but offset is (to keep it in place)

          
          int16_t sprite_ymax = (box.size.h + spriteheight + spriteverticaloffset)/2;// + (((32*box.size.h) << 16) / (objectdist * cos_lookup(angle)));
          int16_t sprite_ymin = sprite_ymax - spriteheight; // note: sprite is not cos adjusted but offset is (to keep it in place)
          
//           int16_t sprite_ymin = halfheight + spriteverticaloffset - spriteheight; // note: sprite is not cos adjusted but offset is (to keep it in place)
//           int16_t sprite_ymax = halfheight + spriteverticaloffset;
    

          
          if(sprite_ymax>=0 && sprite_ymin<box.size.h) { // if any of the sprite is vertically within view
            int16_t ymin = sprite_ymin<0 ? 0 : sprite_ymin;
            int16_t ymax = sprite_ymax>box.size.h ? box.size.h : sprite_ymax;
///BEGIN DRAWING LOOPS
            for(int16_t x = xmin; x < xmax; x++) {
              if(dist[x]>=objectdist) {  // if not behind wall
                xaddr = (box.origin.x + x) >> 5;
                xbit  = (box.origin.x + x) & 31;
                xoffset = ((x - sprite_xmin) * objectdist) / spritescale; // x point hit on texture -- make sure to use the original object dist, not the cosine adjusted one
                mask   = (uint32_t*)sprite_mask[0]->addr + xoffset;  // mask = mask
                target = (uint32_t*)sprite_image[0]->addr + xoffset; // target = sprite
                yaddr = (box.origin.y + ymin) * 5;
                
                for(int16_t y=ymin; y<ymax; y++, yaddr+=5) {
                  //graphics_draw_pixel(ctx, GPoint(box.origin.x + x, box.origin.y + y));
                  yoffset = ((y - sprite_ymin) * objectdist) / spritescale; // y point hit on texture column (was = (objectheight*(y-sprite_ymin))/spriteheight)
                  if(((*mask >> yoffset) & 1) == 1) {   // If mask point isn't clear, then draw point.  TODO: try removing == 1
                    ctx32[xaddr + yaddr] &= ~(1 << xbit);  // blacken bit
                  //ctx32[xaddr + yaddr] |= 1 << xbit;     // whiten bit
                    ctx32[xaddr + yaddr] |= ((*(target) >> yoffset)&1) << xbit;  // make bit white or keep it black
                  //ctx32[xaddr + yaddr] |= ((*((uint32_t*)sprite_image[0]->addr + xoffset) >> yoffset)&1) << xbit;  // make bit white or keep it black
                  }
                } // next y
              } // end display column if in front of wall
            } // next x
//END DRAWING LOOPS      
          } // end display if within y bounds
        } // end display if within x bounds
      } // end display if within farthest
    } // end display if not behind you
  } // next obj
} // end draw 3D function

static void graphics_layer_update_proc(Layer *me, GContext *ctx) {
  static char text[40];  //Buffer to hold text
  time_t sec1, sec2; uint16_t ms1, ms2, dt; // time snapshot variables, to calculate render time and FPS
  time_ms(&sec1, &ms1);  //1st Time Snapshot
  
  //draw_3D(ctx,  GRect(view_x, view_y, view_w, view_h));
  draw_3D(ctx,  GRect(1, 34, 142, 128));
  //draw_3D(ctx,  GRect(4, 110, 40, 40));
  draw_map(ctx, GRect(4, 110, 40, 40), 4);
  
  time_ms(&sec2, &ms2);  //2nd Time Snapshot
  dt = (uint16_t)(1000*(sec2 - sec1)) + (ms2 - ms1);  //dt=delta time: time between two time snapshots in milliseconds

  snprintf(text, sizeof(text), "(%ld,%ld) %d\n%dms %dfps", player.x, player.y, player.facing, dt, 1000/dt);  // What text to draw
//  snprintf(text, sizeof(text), "(%ld,%ld) %d\n%dms %dfps", player.x>>6, player.y>>6, player.facing, dt, 1000/dt);  // What text to draw
//  snprintf(text, sizeof(text), "%db (%ld,%ld) %d\n%ld %ld %ld %ld %ld", heap_bytes_free(), player.x, player.y, player.facing, Q1,Q2,Q3,Q4,Q5);  // What text to draw
  draw_textbox(ctx, GRect(0, 0, 143, 32), text);
   
  //  Set a timer to restart loop in 50ms
  if(dt<40 && dt>0) // if time to render is less than 40ms, force max framerate of 20FPS or worse
     app_timer_register(50-dt, main_loop, NULL);  // 20FPS
  else
     app_timer_register(10, main_loop, NULL);     // took longer than 40ms, loop asap (in 10ms)
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
  object = (struct PlayerStruct){.x=(2 * 64), .y=(64*(mapsize/2)), .facing=10000};    // sprite position
  //GenerateRandomMap();                // generate a randomly dotted map
  //GenerateMazeMap(mapsize/2, 0);    // generate a random maze, enterane on middle of top side
  GenerateSquareMap();
  
  LoadMapTextures(); // Load textures
  // MainLoop() will be automatically started with dirty layer drawing
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

// 2: xoffset = (y * ray.dist / box.size.h) >> 16;
//    was 
// total_column_height = box.size.h / (dist>>16) (Total height of 32pixel wall half. This isn't clipped to screen, so could be thousands of pixels tall)
// i IS clipped to screen and goes from 3D view's middle to 3D view's top/bottom edge
// which pixel in the texture hit is i/total_colum_height = i / (box.size.h / (dist>>16)) = (i * dist / box.size.h) >> 16
