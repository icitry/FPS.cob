      IDENTIFICATION DIVISION.
      PROGRAM-ID. FPS.
      
      ENVIRONMENT DIVISION.
      INPUT-OUTPUT SECTION.

      FILE-CONTROL.
           SELECT MAP_DATA_FILE ASSIGN TO DYNAMIC MAP_DATA_PATH
               FILE STATUS IS MAP_FILE_STATUS
               ORGANIZATION IS LINE SEQUENTIAL.

      DATA DIVISION.
      FILE SECTION.

      FD   MAP_DATA_FILE.
      01   MAP_DATA_LINE               PIC X(256).

      WORKING-STORAGE SECTION.
      
      *> Rendering
      01   RENDER_SLEEP                PIC S9(9) COMP-5 VALUE 16000.  *> ~16ms (≈60 fps cap)
      01   WINDOW_WIDTH                PIC 9(4) COMP-5 VALUE 120.
      01   WINDOW_HEIGHT               PIC 9(4) COMP-5 VALUE 90.
      01   WINDOW_PIXELS_COUNT         PIC 9(9) COMP-5 VALUE 32400. *> 120*90*3
      01   FRAMEBUF.
           05 FB-BYTE OCCURS 35000 TIMES PIC X.
       
      01   WINDOW_WIDTH_MIN_1          PIC S9(6) COMP-5.
      01   WINDOW_HEIGHT_MIN_1         PIC S9(6) COMP-5.
      01   WINDOW_WIDTH_HALF           PIC S9(6) COMP-5.
      01   WINDOW_HEIGHT_HALF          PIC S9(6) COMP-5.
      01   WINDOW_HEIGHT_CENTER        USAGE COMP-2.
       
      01   FOV                         USAGE COMP-2 VALUE 0.9599310886. *> ~55 deg
      01   FOV_HALF                    USAGE COMP-2 VALUE 0.4799655443.
      01   CAM_PLANE_SCALE             USAGE COMP-2 VALUE 0.5205670506.
      01   MAX_DIST                    USAGE COMP-2 VALUE 20.0.
      01   STEP_DIST                   USAGE COMP-2 VALUE 0.03.
      01   PROJ_DIST                   USAGE COMP-2.
       
      01   INV_DET                     USAGE COMP-2.
      01   CAM_TRANSF_X                USAGE COMP-2.
      01   CAM_TRANSF_Y                USAGE COMP-2.
      01   CAM_PLANE_X                 USAGE COMP-2.
      01   CAM_PLANE_Y                 USAGE COMP-2.
       
      01   DISP_WIDTH                  PIC 9(4).
      01   DISP_HEIGHT                 PIC 9(4).
      01   HDR                         PIC X(17).
      01   HDR_LEN                     PIC 9(4) COMP-5 VALUE 17.
       
      01   CURR_PIXEL_X                PIC 9(4) COMP-5.
      01   CURR_PIXEL_Y                PIC 9(4) COMP-5.
      01   CURR_PIXEL_R                PIC 9(3) COMP-5.
      01   CURR_PIXEL_G                PIC 9(3) COMP-5.
      01   CURR_PIXEL_B                PIC 9(3) COMP-5.

      78   TEX_WIDTH                   VALUE 64.
      78   TEX_HEIGHT                  VALUE 64.
      78   TEX_DATA                    VALUE 12288.  *> 64*64*3
      78   WALL_TEX_NUM                VALUE 4.
      78   SPR_DEPTH_PIXELS            VALUE 10800.

      01   WALL_TEX.
           05 W_TEX OCCURS WALL_TEX_NUM TIMES.
              10 W_TEX_DATA OCCURS TEX_DATA TIMES PIC X.
      
      78   SPR_NUM                     VALUE 16.
      
      78   SPR_ENEMY                   VALUE 1.
      78   SPR_GUN                     VALUE 2.
      78   SPR_MEDKIT                  VALUE 3.

      78   ENEMY_TEX_NUM               VALUE 1.
      
      78   SPR_TRANSPARENT_R           VALUE 256.
      78   SPR_TRANSPARENT_G           VALUE 1.
      78   SPR_TRANSPARENT_B           VALUE 256.

      01   SPR_TEX.
           05 S_TEX OCCURS SPR_NUM TIMES.
              10 SPR_TEX_DATA OCCURS TEX_DATA TIMES PIC X.

      *> IO
      01   STDOUT_FD                   PIC S9(9) COMP-5 VALUE 1.
      01   STDIN_FD                    PIC S9(9) COMP-5 VALUE 0.
      01   WRET                        PIC S9(9) COMP-5.
      01   RRET                        PIC S9(9) COMP-5.
      01   O-RDONLY                    PIC S9(9) COMP-5 VALUE 0.
       
      01   INPUT_KEY                   PIC X VALUE X"00".
      01   QUIT_FLAG                   PIC 9 COMP-5 VALUE 0.
       
      01   STTY_RAW                    PIC X(80) VALUE "sh -c 'test -t 0 && stty -echo -icanon min 0 time 0'".
      01   STTY_SANE                   PIC X(80) VALUE "sh -c 'test -t 0 && stty sane'".
      
      01   EOF_FLAG                    PIC 9 COMP-5 VALUE 0.
      01   MAP_FILE_STATUS             PIC XX VALUE SPACES.

      01   MAP_DATA_PATH               PIC X(255) VALUE "level1.map".
      01   IS_READING_MAP_DATA         PIC 9 COMP-5 VALUE 0.
      01   MAP_DATA_ROWS_READ          PIC 9(4) COMP-5 VALUE 0.
      01   MAP_FD                      PIC S9(9) COMP-5.
      01   MAP_RCOUNT                  PIC S9(9) COMP-5.
      01   MAP_NEED                    PIC S9(9) COMP-5.
      01   MAP_BYTES_USED              PIC S9(9) COMP-5 VALUE 0.
      01   MAP_PARSE_POS               PIC S9(9) COMP-5 VALUE 0.
      01   MAP_LINE_START              PIC S9(9) COMP-5 VALUE 0.
      01   MAP_LINE_LEN                PIC S9(9) COMP-5 VALUE 0.
      01   MAP_OPEN_PATH               PIC X(260).
      01   MAP_FILE_BUF.
           05 MAP_FILE_BYTE OCCURS 65536 TIMES PIC X.

      01   TEX_FD                      PIC S9(9) COMP-5.
      01   TEX_RCOUNT                  PIC S9(9) COMP-5.
      01   TEX_OFFSET                  PIC S9(9) COMP-5.
      01   TEX_NEED                    PIC S9(9) COMP-5.
      01   TEX_PATH                    PIC X(260).
       
      78   TOKENS_MAX                  VALUE 64.

      01   TOKENS_NUM                  PIC 9(4) COMP-5 VALUE 0.
      01   TOKEN_IDX                   PIC 9(4) COMP-5.

      01   TOKENS.
           05 TOK OCCURS TOKENS_MAX TIMES PIC X(32).


      *> Env
      01   PLAYER_X                    USAGE COMP-2 VALUE 3.50.
      01   PLAYER_Y                    USAGE COMP-2 VALUE 3.50.
      01   PLAYER_A                    USAGE COMP-2 VALUE 0.0.
       
      01   PLAYER_NEXT_POS_X           USAGE COMP-2.
      01   PLAYER_NEXT_POS_Y           USAGE COMP-2.
      01   PLAYER_MOVE_DX              USAGE COMP-2.
      01   PLAYER_MOVE_DY              USAGE COMP-2.
       
      01   MAP_WIDTH                   PIC 9(3) COMP-5 VALUE 16.
      01   MAP_HEIGHT                  PIC 9(3) COMP-5 VALUE 16.
      01   MAP_DATA.
           05 MAP-ROW OCCURS 64 TIMES PIC X(64).
      01   GRID_SECTORS.
           05 GRID-SECTOR-ROW OCCURS 64 TIMES.
               10 GRID-SECTOR-CELL OCCURS 64 TIMES PIC S9(4) COMP-5.

      78   MAX_VERTICES                VALUE 4096.
      78   MAX_SECTORS                 VALUE 1024.
      78   MAX_LINEDEFS                VALUE 4096.
      78   MAX_THINGS                  VALUE 256.
      78   MAX_SSECTORS                VALUE 1024.
      78   MAX_BSP_NODES               VALUE 1024.
      78   MAX_WORLD_STEPS             VALUE 32.

      78   THING_NONE                  VALUE 0.
      78   THING_HP                    VALUE 1.

      78   E_STATE_LOOK                VALUE 0.
      78   E_STATE_CHASE               VALUE 1.
      78   E_STATE_ATTACK              VALUE 2.
      78   E_STATE_PAIN                VALUE 3.
      78   E_STATE_DEAD                VALUE 4.

      01   WORLD_MODE                  PIC 9 COMP-5 VALUE 0.
      01   VERTICES_NUM                PIC 9(4) COMP-5 VALUE 0.
      01   SECTORS_NUM                 PIC 9(4) COMP-5 VALUE 0.
      01   LINEDEFS_NUM                PIC 9(4) COMP-5 VALUE 0.
      01   THINGS_NUM                  PIC 9(4) COMP-5 VALUE 0.
      01   SSECTORS_NUM                PIC 9(4) COMP-5 VALUE 0.
      01   BSP_NODES_NUM               PIC 9(4) COMP-5 VALUE 0.

      01   VERTICES.
           05 VERTEX OCCURS MAX_VERTICES TIMES.
               10 V_POS_X              USAGE COMP-2.
               10 V_POS_Y              USAGE COMP-2.

      01   SECTORS.
           05 SECTOR OCCURS MAX_SECTORS TIMES.
               10 S_IS_ACTIVE          PIC 9 COMP-5.
               10 S_FLOOR_Z            USAGE COMP-2.
               10 S_CEIL_Z             USAGE COMP-2.
               10 S_WALL_TEX           PIC 9(2) COMP-5.
               10 S_CEIL_R             PIC 9(3) COMP-5.
               10 S_CEIL_G             PIC 9(3) COMP-5.
               10 S_CEIL_B             PIC 9(3) COMP-5.
               10 S_FLOOR_R            PIC 9(3) COMP-5.
               10 S_FLOOR_G            PIC 9(3) COMP-5.
               10 S_FLOOR_B            PIC 9(3) COMP-5.

      01   LINEDEFS.
           05 LINEDEF OCCURS MAX_LINEDEFS TIMES.
               10 L_IS_ACTIVE          PIC 9 COMP-5.
               10 L_V1                 PIC 9(4) COMP-5.
               10 L_V2                 PIC 9(4) COMP-5.
               10 L_FRONT_SECTOR       PIC S9(4) COMP-5.
               10 L_BACK_SECTOR        PIC S9(4) COMP-5.
               10 L_FLAGS              PIC 9(4) COMP-5.
               10 L_MID_TEX            PIC 9(2) COMP-5.
               10 L_UPPER_TEX          PIC 9(2) COMP-5.
               10 L_LOWER_TEX          PIC 9(2) COMP-5.
               10 L_X1                 USAGE COMP-2.
               10 L_Y1                 USAGE COMP-2.
               10 L_X2                 USAGE COMP-2.
               10 L_Y2                 USAGE COMP-2.

      01   THINGS.
           05 THING OCCURS MAX_THINGS TIMES.
               10 T_IS_ACTIVE          PIC 9 COMP-5.
               10 T_KIND               PIC 9(2) COMP-5.
               10 T_POS_X              USAGE COMP-2.
               10 T_POS_Y              USAGE COMP-2.
               10 T_SECTOR             PIC S9(4) COMP-5.
               10 T_VALUE              PIC S9(4) COMP-5.

      01   SSECTORS.
           05 SSECTOR OCCURS MAX_SSECTORS TIMES.
               10 SS_IS_ACTIVE         PIC 9 COMP-5.
               10 SS_SECTOR_ID         PIC S9(4) COMP-5.

      01   BSP_NODES.
           05 BSP_NODE OCCURS MAX_BSP_NODES TIMES.
               10 B_IS_ACTIVE          PIC 9 COMP-5.
               10 B_AXIS               PIC 9 COMP-5.
               10 B_SPLIT_POS          USAGE COMP-2.
               10 B_FRONT_CHILD        PIC S9(4) COMP-5.
               10 B_BACK_CHILD         PIC S9(4) COMP-5.
       
      01   WALL_HEIGHT                 USAGE COMP-2 VALUE 1.0.
      01   PLAYER_SECTOR               PIC S9(4) COMP-5 VALUE 0.
      01   PLAYER_HP                   PIC S9(4) COMP-5 VALUE 100.
      01   PLAYER_MAX_HP               PIC S9(4) COMP-5 VALUE 100.
      01   PLAYER_EYE_Z                USAGE COMP-2 VALUE 0.60.
      01   PLAYER_HEIGHT               USAGE COMP-2 VALUE 0.60.
      01   PLAYER_STEP_Z               USAGE COMP-2 VALUE 0.35.
      01   PLAYER_RADIUS               USAGE COMP-2 VALUE 0.18.
       
      01   MAP_PX                      PIC 9(4) COMP-5.
      01   MAP_PY                      PIC 9(4) COMP-5.
      01   MAP_PVAL                    PIC X.
       
      01   CEIL_R                      PIC 9(3) COMP-5 VALUE 222.
      01   CEIL_G                      PIC 9(3) COMP-5 VALUE 190.
      01   CEIL_B                      PIC 9(3) COMP-5 VALUE 142.
       
      01   FLOOR_R                     PIC 9(3) COMP-5 VALUE 30.
      01   FLOOR_G                     PIC 9(3) COMP-5 VALUE 20.
      01   FLOOR_B                     PIC 9(3) COMP-5 VALUE 20.
       
      01   WALL_DEFAULT_R              PIC 9(3) COMP-5 VALUE 220.
      01   WALL_DEFAULT_G              PIC 9(3) COMP-5 VALUE 220.
      01   WALL_DEFAULT_B              PIC 9(3) COMP-5 VALUE 220.
      
      *> Shooting
      01   SHOOT_REQUEST               PIC 9 COMP-5 VALUE 0.
      01   SHOOT_COOLDOWN              PIC 9(4) COMP-5 VALUE 0. *> frames until can fire again
      01   FLASH_TIMER                 PIC 9(4) COMP-5 VALUE 0. *> frames to show muzzle flash
       
      01   LAST_HIT_DIST               USAGE COMP-2 VALUE 0.0.
      01   LAST_HIT_TIMER              PIC 9(4) COMP-5 VALUE 0. *> frames to show impact highlight
      
      
      *> Enemies
      78   ENEMIES_NUM                 VALUE 16.
      78   ENEMY_MAX_WP                VALUE 8.
       
      01   CHASE_RANGE_SQ              USAGE COMP-2 VALUE 49.0.  *> 7^2
      01   LOST_RANGE_SQ               USAGE COMP-2 VALUE 81.0.  *> 9^2
      01   ATTACK_RANGE_SQ             USAGE COMP-2 VALUE 16.0.  *> 4^2
       
      01   ENEMIES.
           05 ENEMY OCCURS ENEMIES_NUM TIMES.
               10 E_IS_ACTIVE          PIC 9 COMP-5.
               10 E_HP                 PIC S9(4) COMP-5.
               10 E_MAX_HP             PIC S9(4) COMP-5.
               10 E_POS_X              USAGE COMP-2.
               10 E_POS_Y              USAGE COMP-2.
               10 E_SECTOR             PIC S9(4) COMP-5.
               10 E_RADIUS             USAGE COMP-2.
               10 E_WP_COUNT           PIC 9(2) COMP-5.
               10 E_WP_IDX             PIC 9(2) COMP-5.
               10 E_SPEED              USAGE COMP-2.
               10 E_WP_X               OCCURS ENEMY_MAX_WP TIMES USAGE COMP-2.
               10 E_WP_Y               OCCURS ENEMY_MAX_WP TIMES USAGE COMP-2.
               10 E_STATE              PIC 9 COMP-5.
               10 E_IS_AGGRO           PIC 9(4) COMP-5.
               10 E_ATTACK_COOLDOWN    PIC 9(4) COMP-5.
               10 E_PAIN_TIMER         PIC 9(4) COMP-5.
       
      01   ZBUFFER.
           05 ZD OCCURS 160 TIMES USAGE COMP-2.

      01   SPRITE-ZBUFFER.
           05 SPR_ZD OCCURS SPR_DEPTH_PIXELS TIMES USAGE COMP-2.

      01   SPR_VIS_TARGET_X            USAGE COMP-2.
      01   SPR_VIS_TARGET_Y            USAGE COMP-2.
      01   SPR_VIS_TARGET_SECTOR       PIC S9(4) COMP-5.
      01   SPR_VIS_TOP                 PIC S9(6) COMP-5.
      01   SPR_VIS_BOT                 PIC S9(6) COMP-5.
      01   SPR_VIS_BLOCKED             PIC 9 COMP-5.
       
      01   DAMAGE                      PIC S9(4) COMP-5 VALUE 10.
      01   BEST_ENEMY                  PIC 9(2) COMP-5 VALUE 0.
      01   BEST_ALONG                  USAGE COMP-2 VALUE 1.0E9.
       
      01   E_HP_FRAC                   USAGE COMP-2.
      01   E_SPRITE_BRIGHT             USAGE COMP-2.


      *> HUD
      01   XHAIR_X                     USAGE COMP-2 VALUE 0.0.
      01   XHAIR_Y                     USAGE COMP-2 VALUE 0.0.
       
      01   XHAIR_RET_X                 USAGE COMP-2 VALUE 0.0.
      01   XHAIR_RET_Y                 USAGE COMP-2 VALUE 0.0.
      
      01   GUN_X0                      PIC S9(6) COMP-5.
      01   GUN_X1                      PIC S9(6) COMP-5.
      01   GUN_Y0                      PIC S9(6) COMP-5.
      01   GUN_Y1                      PIC S9(6) COMP-5.
       
      01   FLASH_X0                    PIC S9(6) COMP-5.
      01   FLASH_X1                    PIC S9(6) COMP-5.
      01   FLASH_Y0                    PIC S9(6) COMP-5.
      01   FLASH_Y1                    PIC S9(6) COMP-5.
       
       
      *> Local vars
      01   X                           PIC 9(4) COMP-5.
      01   Y                           PIC 9(4) COMP-5.
      01   T                           USAGE COMP-2.
      01   I                           PIC 9(4) COMP-5.
      01   J                           PIC 9(4) COMP-5.
      01   K                           PIC 9(4) COMP-5.
      01   N                           PIC 9(4) COMP-5.
       
      01   RAY_A                       USAGE COMP-2.
      01   RAY_DX                      USAGE COMP-2.
      01   RAY_DY                      USAGE COMP-2.
       
      01   RAY_X                       USAGE COMP-2.
      01   RAY_Y                       USAGE COMP-2.
      01   HIT                         PIC 9 COMP-5.
       
      01   CELL_X                      PIC S9(5) COMP-5.
      01   CELL_Y                      PIC S9(5) COMP-5.
       
      01   SLICE_HEIGHT                USAGE COMP-2.
      01   WALL_TOP                    PIC S9(6) COMP-5.
      01   WALL_BOT                    PIC S9(6) COMP-5.
       
      01   SHADE                       USAGE COMP-2.
      01   WALL_R                      PIC 9(3) COMP-5.
      01   WALL_G                      PIC 9(3) COMP-5.
      01   WALL_B                      PIC 9(3) COMP-5.
       
      01   PIXEL_IDX                   PIC 9(9) COMP-5.
      01   PIXEL_TEX_OFFSETET                PIC 9(9) COMP-5.
      01   SPR_DEPTH_IDX               PIC 9(5) COMP-5.
       
      01   ALONG                       USAGE COMP-2.
      01   D2                          USAGE COMP-2.
      01   R2                          USAGE COMP-2.
       
      01   DX                          USAGE COMP-2.
      01   DY                          USAGE COMP-2.
      01   DX2                         USAGE COMP-2.
      01   DY2                         USAGE COMP-2.
      01   DY_SLICE                    PIC S9(6) COMP-5.
       
      01   ENEMY_NEXT_POS_X            USAGE COMP-2.
      01   ENEMY_NEXT_POS_Y            USAGE COMP-2.
       
      01   SCR_X                       PIC S9(6) COMP-5.
      01   E_SPRITE_HEIGHT             PIC S9(6) COMP-5.
      01   E_SPRITE_WIDTH              PIC S9(6) COMP-5.
       
      01   E_I                         PIC 9(2) COMP-5.
      01   E_X                         USAGE COMP-2.
      01   E_Y                         USAGE COMP-2.
      01   E_HEALTH                    PIC S9(4) COMP-5.
      01   E_SPD                       USAGE COMP-2.
      01   E_ID                        PIC 9(2) COMP-5.
      01   E_WP_N                      PIC 9(2) COMP-5.
       
      01   E_TARGET_X                  USAGE COMP-2.
      01   E_TARGET_Y                  USAGE COMP-2.
       
      01   E_SEE_DIST                  USAGE COMP-2.
      01   E_SEE_T                     USAGE COMP-2.
      01   E_SEE_RX                    USAGE COMP-2.
      01   E_SEE_RY                    USAGE COMP-2.
      01   E_SEE_HIT                   PIC 9 COMP-5.
      01   E_SEE_DX                    USAGE COMP-2.
      01   E_SEE_DY                    USAGE COMP-2.
      01   E_SEE_A                     USAGE COMP-2.
       
      01   DIST_SQ                     USAGE COMP-2.
      01   DIST                        USAGE COMP-2.
      01   PERP_DIST                   USAGE COMP-2.
      
      01   T_ID                        PIC 9(2) COMP-5.
      01   TEX_ID                      PIC 9(2) COMP-5.
      01   TEX_ID_TXT                  PIC 9(2).
      01   TEX_IDX                     PIC S9(9) COMP-5.
      01   TEX_X                       PIC S9(6) COMP-5.
      01   TEX_Y                       PIC S9(6) COMP-5.
      
      01  SPR_R                        PIC 9(3) COMP-5.
      01  SPR_G                        PIC 9(3) COMP-5.
      01  SPR_B                        PIC 9(3) COMP-5.

      01 STR                           PIC 9(3) COMP-5.
      01 STG                           PIC 9(3) COMP-5.
      01 STB                           PIC 9(3) COMP-5.
       
      01  SPR_X0                     PIC S9(6) COMP-5.
      01  SPR_X1                     PIC S9(6) COMP-5.
      01  SPR_Y0                     PIC S9(6) COMP-5.
      01  SPR_Y1                     PIC S9(6) COMP-5.

      01  CLIP_X0                    PIC S9(6) COMP-5.
      01  CLIP_X1                    PIC S9(6) COMP-5.
      01  CLIP_Y0                      PIC S9(6) COMP-5.
      01  CLIP_Y1                    PIC S9(6) COMP-5.

      01  VIS_WIDTH                  PIC S9(6) COMP-5.
      01  VIS_HEIGHT                 PIC S9(6) COMP-5.

      01   WALL_TEX_T                  USAGE COMP-2.
      01   WALL_TEX_B                  USAGE COMP-2.
      01   WALL_X                      USAGE COMP-2.
      01   WALL_Y                      PIC S9(6) COMP-5.

      01   MAP_X                       PIC S9(6) COMP-5.
      01   MAP_Y                       PIC S9(6) COMP-5.
      01   STEP_X                      PIC S9(6) COMP-5.
      01   STEP_Y                      PIC S9(6) COMP-5.
      01   SIDE                        PIC 9 COMP-5.
      01   SIDE_X                      USAGE COMP-2.
      01   SIDE_Y                      USAGE COMP-2.
      01   EDGE_X                      USAGE COMP-2.
      01   EDGE_Y                      USAGE COMP-2.

      01   FX                          USAGE COMP-2.
      01   FY                          USAGE COMP-2.

      01  GUN_WIDTH                PIC S9(6) COMP-5.
      01  GUN_HEIGHT                PIC S9(6) COMP-5.

      01   TRACE_START_X               USAGE COMP-2.
      01   TRACE_START_Y               USAGE COMP-2.
      01   TRACE_END_X                 USAGE COMP-2.
      01   TRACE_END_Y                 USAGE COMP-2.
      01   TRACE_CURR_SECTOR           PIC S9(4) COMP-5.
      01   TRACE_NEXT_SECTOR           PIC S9(4) COMP-5.
      01   TRACE_PREV_LINEDEF          PIC S9(4) COMP-5.
      01   TRACE_HIT_LINEDEF           PIC S9(4) COMP-5.
      01   TRACE_HIT_BLOCKED           PIC 9 COMP-5.
      01   TRACE_HIT_FOUND             PIC 9 COMP-5.
      01   TRACE_HIT_DIST              USAGE COMP-2.
      01   TRACE_HIT_U                 USAGE COMP-2.
      01   TRACE_CURR_T                USAGE COMP-2.
      01   TRACE_STOP_T                USAGE COMP-2.
      01   TRACE_CLOSEST_T             USAGE COMP-2.
      01   TRACE_DEN                   USAGE COMP-2.
      01   TRACE_DIFF_X                USAGE COMP-2.
      01   TRACE_DIFF_Y                USAGE COMP-2.
      01   TRACE_U                     USAGE COMP-2.
      01   TRACE_T                     USAGE COMP-2.
      01   TRACE_SEG_DX                USAGE COMP-2.
      01   TRACE_SEG_DY                USAGE COMP-2.
      01   TRACE_EPS                   USAGE COMP-2 VALUE 0.0005.
      01   TRACE_PASSABLE              PIC 9 COMP-5.
      01   TRACE_END_SECTOR            PIC S9(4) COMP-5.

      01   WORLD_PICKUP_VALUE          PIC S9(4) COMP-5.
      01   WORLD_PICKUP_KIND           PIC 9(2) COMP-5.
      01   WORLD_VERTEX_ID             PIC 9(4) COMP-5.
      01   WORLD_SECTOR_ID             PIC 9(4) COMP-5.
      01   WORLD_LINEDEF_ID            PIC 9(4) COMP-5.
      01   WORLD_FRONT_SECTOR          PIC S9(4) COMP-5.
      01   WORLD_BACK_SECTOR           PIC S9(4) COMP-5.
      01   WORLD_FLAGS                 PIC 9(4) COMP-5.
      01   WORLD_TEX_MID               PIC 9(2) COMP-5.
      01   WORLD_TEX_UP                PIC 9(2) COMP-5.
      01   WORLD_TEX_LOW               PIC 9(2) COMP-5.
      01   WORLD_V1                    PIC 9(4) COMP-5.
      01   WORLD_V2                    PIC 9(4) COMP-5.
      01   WORLD_X1                    USAGE COMP-2.
      01   WORLD_Y1                    USAGE COMP-2.
      01   WORLD_X2                    USAGE COMP-2.
      01   WORLD_Y2                    USAGE COMP-2.
      01   WORLD_CROSSINGS             PIC 9(4) COMP-5.
      01   WORLD_POINT_INSIDE          PIC 9 COMP-5.
      01   WORLD_STEP_COUNT            PIC 9(4) COMP-5.
      01   WORLD_OPENING_TOP           USAGE COMP-2.
      01   WORLD_OPENING_BOT           USAGE COMP-2.
      01   WORLD_CEIL_Y                PIC S9(6) COMP-5.
      01   WORLD_FLOOR_Y               PIC S9(6) COMP-5.
      01   WORLD_NEXT_CEIL_Y           PIC S9(6) COMP-5.
      01   WORLD_NEXT_FLOOR_Y          PIC S9(6) COMP-5.
      01   WORLD_COL_TOP               PIC S9(6) COMP-5.
      01   WORLD_COL_BOT               PIC S9(6) COMP-5.
      01   WORLD_NEW_TOP               PIC S9(6) COMP-5.
      01   WORLD_NEW_BOT               PIC S9(6) COMP-5.
      01   WORLD_DRAW_TOP              PIC S9(6) COMP-5.
      01   WORLD_DRAW_BOT              PIC S9(6) COMP-5.
      01   WORLD_WALL_TEX              PIC 9(2) COMP-5.
      01   WORLD_PLAYER_Z              USAGE COMP-2.
      01   WORLD_PORTAL_OPEN           PIC 9 COMP-5.
      01   WORLD_HIT_X                 USAGE COMP-2.
      01   WORLD_HIT_Y                 USAGE COMP-2.
      01   WORLD_TEX_U                 USAGE COMP-2.
      01   WORLD_TOP_Z                 USAGE COMP-2.
      01   WORLD_BOT_Z                 USAGE COMP-2.
      01   WORLD_PLAYER_FLOOR_Z        USAGE COMP-2.
      01   WORLD_ENEMY_Z               USAGE COMP-2.
      01   WORLD_PICKUP_Z              USAGE COMP-2.
      01   WORLD_ATTACK_DAMAGE         PIC S9(4) COMP-5 VALUE 6.
      01   WORLD_FLASH_R               PIC 9(3) COMP-5 VALUE 110.
      01   WORLD_FLASH_G               PIC 9(3) COMP-5 VALUE 255.
      01   WORLD_FLASH_B               PIC 9(3) COMP-5 VALUE 120.
      01   WORLD_VALIDATE_ERRORS       PIC 9(4) COMP-5 VALUE 0.
      01   WORLD_SECTOR_LINE_COUNT     PIC 9(4) COMP-5 VALUE 0.
      01   WORLD_ENTITY_X              USAGE COMP-2.
      01   WORLD_ENTITY_Y              USAGE COMP-2.
      01   WORLD_ENTITY_RADIUS         USAGE COMP-2.
      01   WORLD_ENTITY_SECTOR         PIC S9(4) COMP-5.
      01   WORLD_ENTITY_OTHER_SECTOR   PIC S9(4) COMP-5.
      01   WORLD_ENTITY_ITER           PIC 9(2) COMP-5.
      01   WORLD_ENTITY_BLOCKED        PIC 9 COMP-5.
      01   WORLD_ENTITY_PASSABLE       PIC 9 COMP-5.
      01   WORLD_ENTITY_SEG_LEN_SQ     USAGE COMP-2.
      01   WORLD_ENTITY_CLOSE_X        USAGE COMP-2.
      01   WORLD_ENTITY_CLOSE_Y        USAGE COMP-2.
      01   WORLD_ENTITY_DIST_SQ        USAGE COMP-2.
      01   WORLD_ENTITY_DIST           USAGE COMP-2.
      01   WORLD_ENTITY_CLEARANCE      USAGE COMP-2.
      01   WORLD_ENTITY_PUSH           USAGE COMP-2.
      01   WORLD_ENTITY_PUSH_X         USAGE COMP-2.
      01   WORLD_ENTITY_PUSH_Y         USAGE COMP-2.
      01   WORLD_ENTITY_PUSH_LEN       USAGE COMP-2.


      PROCEDURE DIVISION.
      MAIN.
           CALL "signal" USING BY VALUE 13 BY VALUE 1 END-CALL.

           COMPUTE WINDOW_HEIGHT_MIN_1 = WINDOW_HEIGHT - 1
           COMPUTE WINDOW_WIDTH_MIN_1 = WINDOW_WIDTH - 1
           COMPUTE WINDOW_WIDTH_HALF  = WINDOW_WIDTH / 2
           COMPUTE WINDOW_HEIGHT_HALF  = WINDOW_HEIGHT / 2
           MOVE WINDOW_HEIGHT_HALF TO WINDOW_HEIGHT_CENTER
       
           COMPUTE PROJ_DIST = WINDOW_WIDTH_HALF / CAM_PLANE_SCALE
       
           PERFORM LOAD-LEVEL
           PERFORM TERM-RAW

           PERFORM LOAD-WALL-TEXTURES
           PERFORM LOAD-SPRITE-TEXTURES

           PERFORM UNTIL QUIT_FLAG = 1
               PERFORM POLL-INPUT
       
               IF SHOOT_COOLDOWN > 0
                   SUBTRACT 1 FROM SHOOT_COOLDOWN
               END-IF
               IF FLASH_TIMER > 0
                   SUBTRACT 1 FROM FLASH_TIMER
               END-IF
               IF LAST_HIT_TIMER > 0
                   SUBTRACT 1 FROM LAST_HIT_TIMER
               END-IF
       
               IF SHOOT_REQUEST = 1
                   PERFORM DO-FIRE
                   MOVE 0 TO SHOOT_REQUEST
               END-IF
       
               PERFORM UPDATE-ENEMIES
       
               PERFORM BUILD-FRAME
               PERFORM WRITE-FRAME
               CALL "usleep" USING BY VALUE RENDER_SLEEP END-CALL
           END-PERFORM
       
           PERFORM TERM-SANE
           STOP RUN.

      TERM-RAW.
           CALL "system" USING STTY_RAW END-CALL.
      
      TERM-SANE.
           CALL "system" USING STTY_SANE END-CALL.
      
      LOAD-WALL-TEXTURES.
           PERFORM VARYING T_ID FROM 1 BY 1 UNTIL T_ID > WALL_TEX_NUM
               MOVE T_ID TO TEX_ID_TXT
               MOVE SPACES TO TEX_PATH

               STRING
                   "res/wall"
                   FUNCTION TRIM(TEX_ID_TXT)
                   ".rgb"
                   X"00"
                   INTO TEX_PATH
               END-STRING

               PERFORM LOAD-RGB-INTO-WALL
           END-PERFORM.

       LOAD-RGB-INTO-WALL.
           CALL "open" USING
               BY REFERENCE TEX_PATH
               BY VALUE O-RDONLY
               BY VALUE 0
           RETURNING TEX_FD
           END-CALL

           IF TEX_FD < 0
               IF T_ID > 1
                   PERFORM VARYING TEX_IDX FROM 1 BY 1 UNTIL TEX_IDX > TEX_DATA
                       MOVE W_TEX_DATA(1, TEX_IDX) TO W_TEX_DATA(T_ID, TEX_IDX)
                   END-PERFORM
                   EXIT PARAGRAPH
               END-IF
               DISPLAY "Failed to open texture: " TEX_PATH UPON STDERR
               STOP RUN
           END-IF

           MOVE 0 TO TEX_OFFSET
           MOVE TEX_DATA TO TEX_NEED

           PERFORM UNTIL TEX_NEED <= 0
               CALL "read" USING
                   BY VALUE TEX_FD
                   BY REFERENCE W_TEX_DATA(T_ID, TEX_OFFSET + 1)
                   BY VALUE TEX_NEED
               RETURNING TEX_RCOUNT
               END-CALL

               IF TEX_RCOUNT <= 0
                   DISPLAY "Short read on texture: " TEX_PATH UPON STDERR
                   CALL "close" USING BY VALUE TEX_FD END-CALL
                   STOP RUN
               END-IF

               ADD TEX_RCOUNT TO TEX_OFFSET
               SUBTRACT TEX_RCOUNT FROM TEX_NEED
           END-PERFORM

           CALL "close" USING BY VALUE TEX_FD END-CALL.
      
      LOAD-SPRITE-TEXTURES.
           MOVE 1 TO T_ID
           MOVE SPACES TO TEX_PATH
           STRING "res/enemy.rgb" X"00" INTO TEX_PATH END-STRING
           PERFORM LOAD-RGB-INTO-SPR

           MOVE 2 TO T_ID
           MOVE SPACES TO TEX_PATH
           STRING "res/gun.rgb" X"00" INTO TEX_PATH END-STRING
           PERFORM LOAD-RGB-INTO-SPR

           MOVE 3 TO T_ID
           MOVE SPACES TO TEX_PATH
           STRING "res/medkit.rgb" X"00" INTO TEX_PATH END-STRING
           PERFORM LOAD-RGB-INTO-SPR.
      
      LOAD-RGB-INTO-SPR.
           CALL "open" USING
               BY REFERENCE TEX_PATH
               BY VALUE O-RDONLY
               BY VALUE 0
           RETURNING TEX_FD
           END-CALL

           IF TEX_FD < 0
               DISPLAY "Failed to open sprite texture: " FUNCTION TRIM(TEX_PATH) UPON STDERR
               STOP RUN
           END-IF

           MOVE 0 TO TEX_OFFSET
           MOVE TEX_DATA TO TEX_NEED

           PERFORM UNTIL TEX_NEED <= 0
               CALL "read" USING
                   BY VALUE TEX_FD
                   BY REFERENCE SPR_TEX_DATA(T_ID, TEX_OFFSET + 1)
                   BY VALUE TEX_NEED
               RETURNING TEX_RCOUNT
               END-CALL

               IF TEX_RCOUNT <= 0
                   DISPLAY "Short read on sprite texture" UPON STDERR
                   CALL "close" USING BY VALUE TEX_FD END-CALL
                   STOP RUN
               END-IF

               ADD TEX_RCOUNT TO TEX_OFFSET
               SUBTRACT TEX_RCOUNT FROM TEX_NEED
           END-PERFORM

           CALL "close" USING BY VALUE TEX_FD END-CALL.

      TOKENIZE.
           PERFORM VARYING TOKEN_IDX FROM 1 BY 1 UNTIL TOKEN_IDX > TOKENS_MAX
               MOVE SPACES TO TOK(TOKEN_IDX)
           END-PERFORM

           MOVE 0 TO TOKENS_NUM

           UNSTRING MAP_DATA_LINE
               DELIMITED BY ALL SPACE
               INTO TOK(1) TOK(2) TOK(3) TOK(4) TOK(5)
                    TOK(6) TOK(7) TOK(8) TOK(9) TOK(10)
                    TOK(11) TOK(12) TOK(13) TOK(14) TOK(15)
                    TOK(16) TOK(17) TOK(18) TOK(19) TOK(20)
                    TOK(21) TOK(22) TOK(23) TOK(24) TOK(25)
                    TOK(26) TOK(27) TOK(28) TOK(29) TOK(30)
                    TOK(31) TOK(32) TOK(33) TOK(34) TOK(35)
                    TOK(36) TOK(37) TOK(38) TOK(39) TOK(40)
                    TOK(41) TOK(42) TOK(43) TOK(44) TOK(45)
                    TOK(46) TOK(47) TOK(48) TOK(49) TOK(50)
                    TOK(51) TOK(52) TOK(53) TOK(54) TOK(55)
                    TOK(56) TOK(57) TOK(58) TOK(59) TOK(60)
                    TOK(61) TOK(62) TOK(63) TOK(64)
           END-UNSTRING

           PERFORM VARYING TOKEN_IDX FROM 1 BY 1 UNTIL TOKEN_IDX > TOKENS_MAX
               IF TOK(TOKEN_IDX) = SPACES
                   EXIT PERFORM
               END-IF
               ADD 1 TO TOKENS_NUM
           END-PERFORM.
      
      LOAD-LEVEL.
           ACCEPT MAP_DATA_PATH FROM COMMAND-LINE
           IF MAP_DATA_PATH = SPACES
               MOVE "map/level1.map" TO MAP_DATA_PATH
           END-IF

           PERFORM CLEAR-ENTITIES
           PERFORM CLEAR-WORLD

           MOVE 0 TO EOF_FLAG
           MOVE 0 TO IS_READING_MAP_DATA
           MOVE 0 TO MAP_DATA_ROWS_READ
           MOVE SPACES TO MAP_OPEN_PATH
           STRING FUNCTION TRIM(MAP_DATA_PATH) X"00" INTO MAP_OPEN_PATH
           END-STRING

           CALL "open" USING
               BY REFERENCE MAP_OPEN_PATH
               BY VALUE O-RDONLY
               BY VALUE 0
           RETURNING MAP_FD
           END-CALL

           IF MAP_FD < 0
               DISPLAY "Failed to open map: " FUNCTION TRIM(MAP_DATA_PATH)
                   UPON STDERR
               STOP RUN
           END-IF

           MOVE 0 TO MAP_BYTES_USED

           PERFORM UNTIL MAP_BYTES_USED >= 65536
               COMPUTE MAP_NEED = 65536 - MAP_BYTES_USED
               CALL "read" USING
                   BY VALUE MAP_FD
                   BY REFERENCE MAP_FILE_BYTE(MAP_BYTES_USED + 1)
                   BY VALUE MAP_NEED
               RETURNING MAP_RCOUNT
               END-CALL

               IF MAP_RCOUNT <= 0
                   EXIT PERFORM
               END-IF

               ADD MAP_RCOUNT TO MAP_BYTES_USED
           END-PERFORM

           CALL "close" USING BY VALUE MAP_FD END-CALL

           MOVE 1 TO MAP_LINE_START
           PERFORM VARYING MAP_PARSE_POS FROM 1 BY 1 UNTIL MAP_PARSE_POS > MAP_BYTES_USED
               IF MAP_FILE_BYTE(MAP_PARSE_POS) = X"0A"
                   COMPUTE MAP_LINE_LEN = MAP_PARSE_POS - MAP_LINE_START
                   PERFORM HANDLE-LEVEL-BUFFER-LINE
                   COMPUTE MAP_LINE_START = MAP_PARSE_POS + 1
               END-IF
           END-PERFORM

           IF MAP_LINE_START <= MAP_BYTES_USED
               COMPUTE MAP_LINE_LEN = MAP_BYTES_USED - MAP_LINE_START + 1
               PERFORM HANDLE-LEVEL-BUFFER-LINE
           END-IF

           PERFORM FINALIZE-WORLD.

      HANDLE-LEVEL-BUFFER-LINE.
           IF MAP_LINE_LEN < 0
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO MAP_DATA_LINE

           IF MAP_LINE_LEN > LENGTH OF MAP_DATA_LINE
               MOVE LENGTH OF MAP_DATA_LINE TO MAP_LINE_LEN
           END-IF

           IF MAP_LINE_LEN > 0
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > MAP_LINE_LEN
                   MOVE MAP_FILE_BYTE(MAP_LINE_START + J - 1)
                       TO MAP_DATA_LINE(J:1)
               END-PERFORM

               IF MAP_DATA_LINE(MAP_LINE_LEN:1) = X"0D"
                   SUBTRACT 1 FROM MAP_LINE_LEN
                   MOVE SPACE TO MAP_DATA_LINE(MAP_LINE_LEN + 1:1)
               END-IF
           END-IF

           PERFORM HANDLE-LEVEL-LINE.
      
      CLEAR-ENTITIES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
               MOVE 0 TO E_IS_ACTIVE(I)
               MOVE E_STATE_LOOK TO E_STATE(I)
               MOVE 0 TO E_IS_AGGRO(I)
               MOVE 0 TO E_ATTACK_COOLDOWN(I)
               MOVE 0 TO E_PAIN_TIMER(I)
               MOVE 0 TO E_WP_COUNT(I)
               MOVE 1 TO E_WP_IDX(I)
               MOVE 0 TO E_SECTOR(I)
               MOVE 0.25 TO E_RADIUS(I)
               MOVE 0.03 TO E_SPEED(I)
               MOVE 10 TO E_HP(I)
           END-PERFORM.

      CLEAR-WORLD.
           MOVE 0 TO WORLD_MODE
           MOVE 0 TO VERTICES_NUM
           MOVE 0 TO SECTORS_NUM
           MOVE 0 TO LINEDEFS_NUM
           MOVE 0 TO THINGS_NUM
           MOVE 0 TO SSECTORS_NUM
           MOVE 0 TO BSP_NODES_NUM
           MOVE 0 TO PLAYER_SECTOR
           MOVE 100 TO PLAYER_HP
           MOVE 0.0 TO WORLD_PLAYER_FLOOR_Z

           INITIALIZE MAP_DATA, GRID_SECTORS VERTICES, SECTORS, LINEDEFS, THINGS, SSECTORS, BSP_NODES.

      HANDLE-LEVEL-LINE.
           IF MAP_DATA_LINE(1:1) = "#" OR MAP_DATA_LINE = SPACES
               EXIT PARAGRAPH
           END-IF

           IF IS_READING_MAP_DATA = 1
               IF MAP_DATA_LINE(1:1) = "0" OR MAP_DATA_LINE(1:1) = "1"
                   ADD 1 TO MAP_DATA_ROWS_READ
                   IF MAP_DATA_ROWS_READ <= MAP_HEIGHT
                       MOVE MAP_DATA_LINE(1:MAP_WIDTH) TO MAP-ROW(MAP_DATA_ROWS_READ)
                   END-IF
                   IF MAP_DATA_ROWS_READ >= MAP_HEIGHT
                       MOVE 0 TO IS_READING_MAP_DATA
                   END-IF
               END-IF
               EXIT PARAGRAPH
           END-IF

           PERFORM TOKENIZE
           IF TOKENS_NUM = 0
               EXIT PARAGRAPH
           END-IF

           EVALUATE TOK(1)
               WHEN "W"
                   COMPUTE MAP_WIDTH = FUNCTION NUMVAL(TOK(2))
               WHEN "H"
                   COMPUTE MAP_HEIGHT = FUNCTION NUMVAL(TOK(2))
               WHEN "MAP"
                   MOVE 1 TO IS_READING_MAP_DATA
                   MOVE 0 TO MAP_DATA_ROWS_READ
               WHEN "P"
                   COMPUTE PLAYER_X = FUNCTION NUMVAL(TOK(2))
                   COMPUTE PLAYER_Y = FUNCTION NUMVAL(TOK(3))
                   COMPUTE PLAYER_A = FUNCTION NUMVAL(TOK(4))
               WHEN "E"
                   PERFORM PARSE-ENEMY
               WHEN "HP"
                   PERFORM PARSE-PICKUP
               WHEN "VERTEX"
                   PERFORM PARSE-VERTEX
               WHEN "SECTOR"
                   PERFORM PARSE-SECTOR
               WHEN "LINEDEF"
                   PERFORM PARSE-LINEDEF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      PARSE-VERTEX.
           IF TOKENS_NUM < 4
               EXIT PARAGRAPH
           END-IF

           COMPUTE WORLD_VERTEX_ID = FUNCTION NUMVAL(TOK(2))
           IF WORLD_VERTEX_ID < 1 OR WORLD_VERTEX_ID > MAX_VERTICES
               EXIT PARAGRAPH
           END-IF

           IF WORLD_VERTEX_ID > VERTICES_NUM
               MOVE WORLD_VERTEX_ID TO VERTICES_NUM
           END-IF

           COMPUTE V_POS_X(WORLD_VERTEX_ID) = FUNCTION NUMVAL(TOK(3))
           COMPUTE V_POS_Y(WORLD_VERTEX_ID) = FUNCTION NUMVAL(TOK(4)).

      PARSE-SECTOR.
           IF TOKENS_NUM < 11
               EXIT PARAGRAPH
           END-IF

           COMPUTE WORLD_SECTOR_ID = FUNCTION NUMVAL(TOK(2))
           IF WORLD_SECTOR_ID < 1 OR WORLD_SECTOR_ID > MAX_SECTORS
               EXIT PARAGRAPH
           END-IF

           IF WORLD_SECTOR_ID > SECTORS_NUM
               MOVE WORLD_SECTOR_ID TO SECTORS_NUM
           END-IF

           MOVE 1 TO S_IS_ACTIVE(WORLD_SECTOR_ID)
           COMPUTE S_FLOOR_Z(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(3))
           COMPUTE S_CEIL_Z(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(4))
           COMPUTE S_WALL_TEX(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(5))
           COMPUTE S_CEIL_R(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(6))
           COMPUTE S_CEIL_G(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(7))
           COMPUTE S_CEIL_B(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(8))
           COMPUTE S_FLOOR_R(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(9))
           COMPUTE S_FLOOR_G(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(10))
           COMPUTE S_FLOOR_B(WORLD_SECTOR_ID) = FUNCTION NUMVAL(TOK(11))

           IF S_WALL_TEX(WORLD_SECTOR_ID) < 1 OR S_WALL_TEX(WORLD_SECTOR_ID) > WALL_TEX_NUM
               MOVE 1 TO S_WALL_TEX(WORLD_SECTOR_ID)
           END-IF.

      PARSE-LINEDEF.
           IF TOKENS_NUM < 10
               EXIT PARAGRAPH
           END-IF

           COMPUTE WORLD_LINEDEF_ID = FUNCTION NUMVAL(TOK(2))
           IF WORLD_LINEDEF_ID < 1 OR WORLD_LINEDEF_ID > MAX_LINEDEFS
               EXIT PARAGRAPH
           END-IF

           IF WORLD_LINEDEF_ID > LINEDEFS_NUM
               MOVE WORLD_LINEDEF_ID TO LINEDEFS_NUM
           END-IF

           MOVE 1 TO L_IS_ACTIVE(WORLD_LINEDEF_ID)
           COMPUTE L_V1(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(3))
           COMPUTE L_V2(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(4))
           COMPUTE L_FRONT_SECTOR(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(5))
           COMPUTE L_BACK_SECTOR(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(6))
           COMPUTE L_FLAGS(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(7))
           COMPUTE L_MID_TEX(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(8))
           COMPUTE L_UPPER_TEX(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(9))
           COMPUTE L_LOWER_TEX(WORLD_LINEDEF_ID) = FUNCTION NUMVAL(TOK(10)).

      PARSE-PICKUP.
           IF TOKENS_NUM < 4
               EXIT PARAGRAPH
           END-IF

           IF THINGS_NUM >= MAX_THINGS
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO THINGS_NUM
           MOVE 1 TO T_IS_ACTIVE(THINGS_NUM)
           MOVE THING_HP TO T_KIND(THINGS_NUM)
           COMPUTE T_POS_X(THINGS_NUM) = FUNCTION NUMVAL(TOK(2))
           COMPUTE T_POS_Y(THINGS_NUM) = FUNCTION NUMVAL(TOK(3))
           COMPUTE T_VALUE(THINGS_NUM) = FUNCTION NUMVAL(TOK(4)).

      PARSE-ENEMY.
           IF TOKENS_NUM < 8
               EXIT PARAGRAPH
           END-IF

           COMPUTE E_ID = FUNCTION NUMVAL(TOK(2))

           IF E_ID < 1 OR E_ID > ENEMIES_NUM
               EXIT PARAGRAPH
           END-IF

           MOVE E_ID TO E_I
           COMPUTE E_X   = FUNCTION NUMVAL(TOK(3))
           COMPUTE E_Y   = FUNCTION NUMVAL(TOK(4))
           COMPUTE E_HEALTH  = FUNCTION NUMVAL(TOK(5))
           COMPUTE E_SPD = FUNCTION NUMVAL(TOK(6))
           PERFORM SET-ENEMY

           COMPUTE E_RADIUS(E_ID) = FUNCTION NUMVAL(TOK(7))

           COMPUTE E_WP_N = FUNCTION NUMVAL(TOK(8))
           IF E_WP_N < 0
               MOVE 0 TO E_WP_N
           END-IF
           IF E_WP_N > ENEMY_MAX_WP
               MOVE ENEMY_MAX_WP TO E_WP_N
           END-IF

           MOVE E_WP_N TO E_WP_COUNT(E_ID)
           MOVE 1 TO E_WP_IDX(E_ID)

           COMPUTE N = 8 + (2 * E_WP_N)
           IF TOKENS_NUM < N
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > E_WP_N
               COMPUTE TOKEN_IDX = 9 + (K - 1) * 2

               IF TOKEN_IDX < 1 OR TOKEN_IDX + 1 > TOKENS_NUM
                   EXIT PERFORM
               END-IF

               COMPUTE E_WP_X(E_ID, K) = FUNCTION NUMVAL(TOK(TOKEN_IDX))
               COMPUTE E_WP_Y(E_ID, K) = FUNCTION NUMVAL(TOK(TOKEN_IDX + 1))
           END-PERFORM.
      
      SET-ENEMY.
           MOVE 1          TO E_IS_ACTIVE(E_I)
           MOVE E_STATE_LOOK TO E_STATE(E_I)
           MOVE 0          TO E_IS_AGGRO(E_I)
           MOVE 0          TO E_ATTACK_COOLDOWN(E_I)
           MOVE 0          TO E_PAIN_TIMER(E_I)
           MOVE E_HEALTH   TO E_HP(E_I)
           MOVE E_HEALTH   TO E_MAX_HP(E_I)
           MOVE E_X        TO E_POS_X(E_I)
           MOVE E_Y        TO E_POS_Y(E_I)
           MOVE 0          TO E_SECTOR(E_I)
           MOVE 0.25       TO E_RADIUS(E_I)
           MOVE E_SPD      TO E_SPEED(E_I)
           MOVE 1          TO E_WP_IDX(E_I).

      FINALIZE-WORLD.
           IF SECTORS_NUM > 0 AND LINEDEFS_NUM > 0 AND VERTICES_NUM > 0
               PERFORM FINALIZE-LINEDEFS
               PERFORM VALIDATE-WORLD
           END-IF

           IF SECTORS_NUM > 0 AND LINEDEFS_NUM > 0
               MOVE 1 TO WORLD_MODE
               PERFORM BUILD-BSP

               MOVE PLAYER_X TO TRACE_END_X
               MOVE PLAYER_Y TO TRACE_END_Y
               PERFORM FIND-SECTOR-AT
               MOVE TRACE_END_SECTOR TO PLAYER_SECTOR

               IF PLAYER_SECTOR > 0
                   MOVE S_FLOOR_Z(PLAYER_SECTOR) TO WORLD_PLAYER_FLOOR_Z
               ELSE
                   MOVE 0.0 TO WORLD_PLAYER_FLOOR_Z
               END-IF

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
                   IF E_IS_ACTIVE(I) = 1
                       MOVE E_POS_X(I) TO TRACE_END_X
                       MOVE E_POS_Y(I) TO TRACE_END_Y
                       PERFORM FIND-SECTOR-AT
                       MOVE TRACE_END_SECTOR TO E_SECTOR(I)
                   END-IF
               END-PERFORM

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > THINGS_NUM
                   IF T_IS_ACTIVE(I) = 1
                       MOVE T_POS_X(I) TO TRACE_END_X
                       MOVE T_POS_Y(I) TO TRACE_END_Y
                       PERFORM FIND-SECTOR-AT
                       MOVE TRACE_END_SECTOR TO T_SECTOR(I)
                   END-IF
               END-PERFORM
           END-IF.

      FINALIZE-LINEDEFS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LINEDEFS_NUM
               IF L_IS_ACTIVE(I) = 1
                   IF L_V1(I) >= 1 AND L_V1(I) <= VERTICES_NUM
                       MOVE V_POS_X(L_V1(I)) TO L_X1(I)
                       MOVE V_POS_Y(L_V1(I)) TO L_Y1(I)
                   END-IF
                   IF L_V2(I) >= 1 AND L_V2(I) <= VERTICES_NUM
                       MOVE V_POS_X(L_V2(I)) TO L_X2(I)
                       MOVE V_POS_Y(L_V2(I)) TO L_Y2(I)
                   END-IF
                   IF L_MID_TEX(I) < 1 OR L_MID_TEX(I) > WALL_TEX_NUM
                       MOVE 1 TO L_MID_TEX(I)
                   END-IF
                   IF L_UPPER_TEX(I) < 1 OR L_UPPER_TEX(I) > WALL_TEX_NUM
                       MOVE L_MID_TEX(I) TO L_UPPER_TEX(I)
                   END-IF
                   IF L_LOWER_TEX(I) < 1 OR L_LOWER_TEX(I) > WALL_TEX_NUM
                       MOVE L_MID_TEX(I) TO L_LOWER_TEX(I)
                   END-IF
               END-IF
           END-PERFORM.

      VALIDATE-WORLD.
           MOVE 0 TO WORLD_VALIDATE_ERRORS

           PERFORM VARYING WORLD_SECTOR_ID FROM 1 BY 1 UNTIL WORLD_SECTOR_ID > SECTORS_NUM
               IF S_IS_ACTIVE(WORLD_SECTOR_ID) = 1
                   IF S_CEIL_Z(WORLD_SECTOR_ID) <= S_FLOOR_Z(WORLD_SECTOR_ID)
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Invalid sector height: " WORLD_SECTOR_ID UPON STDERR
                   END-IF

                   MOVE 0 TO WORLD_SECTOR_LINE_COUNT
                   PERFORM VARYING WORLD_LINEDEF_ID FROM 1 BY 1 UNTIL WORLD_LINEDEF_ID > LINEDEFS_NUM
                       IF L_IS_ACTIVE(WORLD_LINEDEF_ID) = 1
                           AND (L_FRONT_SECTOR(WORLD_LINEDEF_ID) = WORLD_SECTOR_ID OR L_BACK_SECTOR(WORLD_LINEDEF_ID) = WORLD_SECTOR_ID)
                           ADD 1 TO WORLD_SECTOR_LINE_COUNT
                       END-IF
                   END-PERFORM

                   IF WORLD_SECTOR_LINE_COUNT < 3
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Sector has too few linedefs: " WORLD_SECTOR_ID UPON STDERR
                   END-IF
               END-IF
           END-PERFORM

           PERFORM VARYING WORLD_LINEDEF_ID FROM 1 BY 1 UNTIL WORLD_LINEDEF_ID > LINEDEFS_NUM
               IF L_IS_ACTIVE(WORLD_LINEDEF_ID) = 1
                   IF L_V1(WORLD_LINEDEF_ID) < 1 OR L_V1(WORLD_LINEDEF_ID) > VERTICES_NUM
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Invalid linedef vertex v1: " WORLD_LINEDEF_ID UPON STDERR
                   END-IF

                   IF L_V2(WORLD_LINEDEF_ID) < 1 OR L_V2(WORLD_LINEDEF_ID) > VERTICES_NUM
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Invalid linedef vertex v2: " WORLD_LINEDEF_ID UPON STDERR
                   END-IF

                   IF L_FRONT_SECTOR(WORLD_LINEDEF_ID) < 1 OR L_FRONT_SECTOR(WORLD_LINEDEF_ID) > SECTORS_NUM
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Invalid linedef front sector: " WORLD_LINEDEF_ID UPON STDERR
                   ELSE
                       IF S_IS_ACTIVE(L_FRONT_SECTOR(WORLD_LINEDEF_ID)) = 0
                           ADD 1 TO WORLD_VALIDATE_ERRORS
                           DISPLAY "Inactive linedef front sector: " WORLD_LINEDEF_ID UPON STDERR
                       END-IF
                   END-IF

                   IF L_BACK_SECTOR(WORLD_LINEDEF_ID) < 0 OR L_BACK_SECTOR(WORLD_LINEDEF_ID) > SECTORS_NUM
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Invalid linedef back sector: " WORLD_LINEDEF_ID UPON STDERR
                   ELSE
                       IF L_BACK_SECTOR(WORLD_LINEDEF_ID) > 0 AND S_IS_ACTIVE(L_BACK_SECTOR(WORLD_LINEDEF_ID)) = 0
                           ADD 1 TO WORLD_VALIDATE_ERRORS
                           DISPLAY "Inactive linedef back sector: " WORLD_LINEDEF_ID UPON STDERR
                       END-IF
                   END-IF

                   IF L_FRONT_SECTOR(WORLD_LINEDEF_ID) = L_BACK_SECTOR(WORLD_LINEDEF_ID)
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Linedef reuses same sector on both sides: " WORLD_LINEDEF_ID UPON STDERR
                   END-IF
               END-IF
           END-PERFORM

           MOVE PLAYER_X TO TRACE_END_X
           MOVE PLAYER_Y TO TRACE_END_Y
           PERFORM FIND-SECTOR-AT
           IF TRACE_END_SECTOR = 0
               ADD 1 TO WORLD_VALIDATE_ERRORS
               DISPLAY "Player start is outside explicit sectors" UPON STDERR
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
               IF E_IS_ACTIVE(I) = 1
                   MOVE E_POS_X(I) TO TRACE_END_X
                   MOVE E_POS_Y(I) TO TRACE_END_Y
                   PERFORM FIND-SECTOR-AT
                   IF TRACE_END_SECTOR = 0
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Enemy is outside explicit sectors: " I UPON STDERR
                   END-IF
               END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THINGS_NUM
               IF T_IS_ACTIVE(I) = 1
                   MOVE T_POS_X(I) TO TRACE_END_X
                   MOVE T_POS_Y(I) TO TRACE_END_Y
                   PERFORM FIND-SECTOR-AT
                   IF TRACE_END_SECTOR = 0
                       ADD 1 TO WORLD_VALIDATE_ERRORS
                       DISPLAY "Thing is outside explicit sectors: " I UPON STDERR
                   END-IF
               END-IF
           END-PERFORM

           IF WORLD_VALIDATE_ERRORS > 0
               DISPLAY "Explicit world validation failed with "
                   WORLD_VALIDATE_ERRORS " error(s)" UPON STDERR
               STOP RUN
           END-IF.

      BUILD-LEGACY-SECTORS.
           MOVE 0 TO VERTICES_NUM
           MOVE 0 TO SECTORS_NUM
           MOVE 0 TO LINEDEFS_NUM

           PERFORM VARYING Y FROM 0 BY 1 UNTIL Y > MAP_HEIGHT
               PERFORM VARYING X FROM 0 BY 1 UNTIL X > MAP_WIDTH
                   COMPUTE N = (Y * (MAP_WIDTH + 1)) + X + 1
                   IF N <= MAX_VERTICES
                       IF N > VERTICES_NUM
                           MOVE N TO VERTICES_NUM
                       END-IF
                       COMPUTE V_POS_X(N) = X
                       COMPUTE V_POS_Y(N) = Y
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAP_HEIGHT
               PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAP_WIDTH
                   IF MAP-ROW(Y)(X:1) = "0"
                       ADD 1 TO SECTORS_NUM
                       MOVE SECTORS_NUM TO GRID-SECTOR-CELL(Y, X)
                       MOVE 1 TO S_IS_ACTIVE(SECTORS_NUM)
                       MOVE 0.0 TO S_FLOOR_Z(SECTORS_NUM)
                       MOVE 1.0 TO S_CEIL_Z(SECTORS_NUM)
                       MOVE 1 TO S_WALL_TEX(SECTORS_NUM)
                       MOVE CEIL_R TO S_CEIL_R(SECTORS_NUM)
                       MOVE CEIL_G TO S_CEIL_G(SECTORS_NUM)
                       MOVE CEIL_B TO S_CEIL_B(SECTORS_NUM)
                       MOVE FLOOR_R TO S_FLOOR_R(SECTORS_NUM)
                       MOVE FLOOR_G TO S_FLOOR_G(SECTORS_NUM)
                       MOVE FLOOR_B TO S_FLOOR_B(SECTORS_NUM)
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAP_HEIGHT
               PERFORM VARYING X FROM 1 BY 1 UNTIL X > MAP_WIDTH
                   IF GRID-SECTOR-CELL(Y, X) > 0
                       MOVE GRID-SECTOR-CELL(Y, X) TO WORLD_SECTOR_ID

                       COMPUTE WORLD_V1 = ((Y - 1) * (MAP_WIDTH + 1)) + (X - 1) + 1
                       COMPUTE WORLD_V2 = ((Y - 1) * (MAP_WIDTH + 1)) + X + 1
                       ADD 1 TO LINEDEFS_NUM
                       MOVE 1 TO L_IS_ACTIVE(LINEDEFS_NUM)
                       MOVE WORLD_V1 TO L_V1(LINEDEFS_NUM)
                       MOVE WORLD_V2 TO L_V2(LINEDEFS_NUM)
                       MOVE WORLD_SECTOR_ID TO L_FRONT_SECTOR(LINEDEFS_NUM)
                       IF Y > 1
                           MOVE GRID-SECTOR-CELL(Y - 1, X) TO L_BACK_SECTOR(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_BACK_SECTOR(LINEDEFS_NUM)
                       END-IF
                       IF L_BACK_SECTOR(LINEDEFS_NUM) = 0
                           MOVE 1 TO L_FLAGS(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_FLAGS(LINEDEFS_NUM)
                       END-IF
                       MOVE 1 TO L_MID_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_UPPER_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_LOWER_TEX(LINEDEFS_NUM)

                       COMPUTE WORLD_V1 = ((Y - 1) * (MAP_WIDTH + 1)) + X + 1
                       COMPUTE WORLD_V2 = (Y * (MAP_WIDTH + 1)) + X + 1
                       ADD 1 TO LINEDEFS_NUM
                       MOVE 1 TO L_IS_ACTIVE(LINEDEFS_NUM)
                       MOVE WORLD_V1 TO L_V1(LINEDEFS_NUM)
                       MOVE WORLD_V2 TO L_V2(LINEDEFS_NUM)
                       MOVE WORLD_SECTOR_ID TO L_FRONT_SECTOR(LINEDEFS_NUM)
                       IF X < MAP_WIDTH
                           MOVE GRID-SECTOR-CELL(Y, X + 1) TO L_BACK_SECTOR(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_BACK_SECTOR(LINEDEFS_NUM)
                       END-IF
                       IF L_BACK_SECTOR(LINEDEFS_NUM) = 0
                           MOVE 1 TO L_FLAGS(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_FLAGS(LINEDEFS_NUM)
                       END-IF
                       MOVE 1 TO L_MID_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_UPPER_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_LOWER_TEX(LINEDEFS_NUM)

                       COMPUTE WORLD_V1 = (Y * (MAP_WIDTH + 1)) + X + 1
                       COMPUTE WORLD_V2 = (Y * (MAP_WIDTH + 1)) + (X - 1) + 1
                       ADD 1 TO LINEDEFS_NUM
                       MOVE 1 TO L_IS_ACTIVE(LINEDEFS_NUM)
                       MOVE WORLD_V1 TO L_V1(LINEDEFS_NUM)
                       MOVE WORLD_V2 TO L_V2(LINEDEFS_NUM)
                       MOVE WORLD_SECTOR_ID TO L_FRONT_SECTOR(LINEDEFS_NUM)
                       IF Y < MAP_HEIGHT
                           MOVE GRID-SECTOR-CELL(Y + 1, X) TO L_BACK_SECTOR(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_BACK_SECTOR(LINEDEFS_NUM)
                       END-IF
                       IF L_BACK_SECTOR(LINEDEFS_NUM) = 0
                           MOVE 1 TO L_FLAGS(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_FLAGS(LINEDEFS_NUM)
                       END-IF
                       MOVE 1 TO L_MID_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_UPPER_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_LOWER_TEX(LINEDEFS_NUM)

                       COMPUTE WORLD_V1 = (Y * (MAP_WIDTH + 1)) + (X - 1) + 1
                       COMPUTE WORLD_V2 = ((Y - 1) * (MAP_WIDTH + 1)) + (X - 1) + 1
                       ADD 1 TO LINEDEFS_NUM
                       MOVE 1 TO L_IS_ACTIVE(LINEDEFS_NUM)
                       MOVE WORLD_V1 TO L_V1(LINEDEFS_NUM)
                       MOVE WORLD_V2 TO L_V2(LINEDEFS_NUM)
                       MOVE WORLD_SECTOR_ID TO L_FRONT_SECTOR(LINEDEFS_NUM)
                       IF X > 1
                           MOVE GRID-SECTOR-CELL(Y, X - 1) TO L_BACK_SECTOR(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_BACK_SECTOR(LINEDEFS_NUM)
                       END-IF
                       IF L_BACK_SECTOR(LINEDEFS_NUM) = 0
                           MOVE 1 TO L_FLAGS(LINEDEFS_NUM)
                       ELSE
                           MOVE 0 TO L_FLAGS(LINEDEFS_NUM)
                       END-IF
                       MOVE 1 TO L_MID_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_UPPER_TEX(LINEDEFS_NUM)
                       MOVE 1 TO L_LOWER_TEX(LINEDEFS_NUM)
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM FINALIZE-LINEDEFS.

      BUILD-BSP.
           MOVE 0 TO SSECTORS_NUM
           MOVE 0 TO BSP_NODES_NUM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SECTORS_NUM
               IF S_IS_ACTIVE(I) = 1
                   ADD 1 TO SSECTORS_NUM
                   MOVE 1 TO SS_IS_ACTIVE(SSECTORS_NUM)
                   MOVE I TO SS_SECTOR_ID(SSECTORS_NUM)
               END-IF
           END-PERFORM.

      FIND-SECTOR-AT.
           MOVE 0 TO TRACE_END_SECTOR

           PERFORM VARYING WORLD_SECTOR_ID FROM 1 BY 1 UNTIL WORLD_SECTOR_ID > SECTORS_NUM
               IF S_IS_ACTIVE(WORLD_SECTOR_ID) = 1
                   MOVE 0 TO WORLD_CROSSINGS

                   PERFORM VARYING WORLD_LINEDEF_ID FROM 1 BY 1 UNTIL WORLD_LINEDEF_ID > LINEDEFS_NUM
                       IF L_IS_ACTIVE(WORLD_LINEDEF_ID) = 1
                           AND (L_FRONT_SECTOR(WORLD_LINEDEF_ID) = WORLD_SECTOR_ID OR L_BACK_SECTOR(WORLD_LINEDEF_ID) = WORLD_SECTOR_ID)
                           IF (L_Y1(WORLD_LINEDEF_ID) > TRACE_END_Y AND L_Y2(WORLD_LINEDEF_ID) <= TRACE_END_Y)
                               OR (L_Y2(WORLD_LINEDEF_ID) > TRACE_END_Y AND L_Y1(WORLD_LINEDEF_ID) <= TRACE_END_Y)
                               COMPUTE T = (TRACE_END_Y - L_Y1(WORLD_LINEDEF_ID)) / (L_Y2(WORLD_LINEDEF_ID) - L_Y1(WORLD_LINEDEF_ID))
                               COMPUTE FX = L_X1(WORLD_LINEDEF_ID) + (T * (L_X2(WORLD_LINEDEF_ID) - L_X1(WORLD_LINEDEF_ID)))
                               IF FX > TRACE_END_X
                                   ADD 1 TO WORLD_CROSSINGS
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM

                   IF FUNCTION MOD(WORLD_CROSSINGS, 2) = 1
                       MOVE WORLD_SECTOR_ID TO TRACE_END_SECTOR
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

      FIND-NEAREST-SECTOR-HIT.
           MOVE 0 TO TRACE_HIT_FOUND
           MOVE 0 TO TRACE_HIT_LINEDEF
           MOVE 2.0 TO TRACE_CLOSEST_T

           COMPUTE TRACE_SEG_DX = TRACE_END_X - TRACE_START_X
           COMPUTE TRACE_SEG_DY = TRACE_END_Y - TRACE_START_Y

           PERFORM VARYING WORLD_LINEDEF_ID FROM 1 BY 1 UNTIL WORLD_LINEDEF_ID > LINEDEFS_NUM
               IF L_IS_ACTIVE(WORLD_LINEDEF_ID) = 1
                   AND (L_FRONT_SECTOR(WORLD_LINEDEF_ID) = TRACE_CURR_SECTOR OR L_BACK_SECTOR(WORLD_LINEDEF_ID) = TRACE_CURR_SECTOR)
                   COMPUTE WORLD_X1 = L_X2(WORLD_LINEDEF_ID) - L_X1(WORLD_LINEDEF_ID)
                   COMPUTE WORLD_Y1 = L_Y2(WORLD_LINEDEF_ID) - L_Y1(WORLD_LINEDEF_ID)
                   COMPUTE TRACE_DEN = (TRACE_SEG_DX * WORLD_Y1) - (TRACE_SEG_DY * WORLD_X1)

                   IF FUNCTION ABS(TRACE_DEN) > TRACE_EPS
                       COMPUTE TRACE_DIFF_X = L_X1(WORLD_LINEDEF_ID) - TRACE_START_X
                       COMPUTE TRACE_DIFF_Y = L_Y1(WORLD_LINEDEF_ID) - TRACE_START_Y
                       COMPUTE TRACE_T = ((TRACE_DIFF_X * WORLD_Y1) - (TRACE_DIFF_Y * WORLD_X1)) / TRACE_DEN
                       COMPUTE TRACE_U = ((TRACE_DIFF_X * TRACE_SEG_DY) - (TRACE_DIFF_Y * TRACE_SEG_DX)) / TRACE_DEN

                       IF TRACE_T > TRACE_CURR_T + TRACE_EPS
                           AND TRACE_T <= 1.0
                           AND TRACE_U >= 0.0
                           AND TRACE_U <= 1.0
                           IF TRACE_T < TRACE_CLOSEST_T
                               MOVE 1 TO TRACE_HIT_FOUND
                               MOVE WORLD_LINEDEF_ID TO TRACE_HIT_LINEDEF
                               MOVE TRACE_T TO TRACE_CLOSEST_T
                               MOVE TRACE_U TO TRACE_HIT_U
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF TRACE_HIT_FOUND = 1
               COMPUTE TRACE_HIT_DIST = FUNCTION SQRT(
                   (TRACE_SEG_DX * TRACE_CLOSEST_T * TRACE_SEG_DX * TRACE_CLOSEST_T)
                   + (TRACE_SEG_DY * TRACE_CLOSEST_T * TRACE_SEG_DY * TRACE_CLOSEST_T))
           END-IF.

      GET-LINE-TRANSITION.
           MOVE 0 TO TRACE_NEXT_SECTOR

           IF L_FRONT_SECTOR(TRACE_HIT_LINEDEF) = TRACE_CURR_SECTOR
               MOVE L_BACK_SECTOR(TRACE_HIT_LINEDEF) TO TRACE_NEXT_SECTOR
           ELSE
               IF L_BACK_SECTOR(TRACE_HIT_LINEDEF) = TRACE_CURR_SECTOR
                   MOVE L_FRONT_SECTOR(TRACE_HIT_LINEDEF) TO TRACE_NEXT_SECTOR
               END-IF
           END-IF

           MOVE 0.0 TO WORLD_OPENING_TOP
           MOVE 0.0 TO WORLD_OPENING_BOT

           IF TRACE_NEXT_SECTOR > 0
               IF S_CEIL_Z(TRACE_CURR_SECTOR) < S_CEIL_Z(TRACE_NEXT_SECTOR)
                   MOVE S_CEIL_Z(TRACE_CURR_SECTOR) TO WORLD_OPENING_TOP
               ELSE
                   MOVE S_CEIL_Z(TRACE_NEXT_SECTOR) TO WORLD_OPENING_TOP
               END-IF

               IF S_FLOOR_Z(TRACE_CURR_SECTOR) > S_FLOOR_Z(TRACE_NEXT_SECTOR)
                   MOVE S_FLOOR_Z(TRACE_CURR_SECTOR) TO WORLD_OPENING_BOT
               ELSE
                   MOVE S_FLOOR_Z(TRACE_NEXT_SECTOR) TO WORLD_OPENING_BOT
               END-IF
           END-IF.
      
      UPDATE-ENEMIES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
               IF E_IS_ACTIVE(I) = 1
                   IF E_ATTACK_COOLDOWN(I) > 0
                       SUBTRACT 1 FROM E_ATTACK_COOLDOWN(I)
                   END-IF

                   IF E_PAIN_TIMER(I) > 0
                       SUBTRACT 1 FROM E_PAIN_TIMER(I)
                   END-IF

                   IF WORLD_MODE = 1
                       MOVE E_POS_X(I) TO TRACE_END_X
                       MOVE E_POS_Y(I) TO TRACE_END_Y
                       PERFORM FIND-SECTOR-AT
                       MOVE TRACE_END_SECTOR TO E_SECTOR(I)
                   END-IF

                   IF E_HP(I) <= 0
                       MOVE E_STATE_DEAD TO E_STATE(I)
                   ELSE
                       COMPUTE DX2 = PLAYER_X - E_POS_X(I)
                       COMPUTE DY2 = PLAYER_Y - E_POS_Y(I)
                       COMPUTE DIST_SQ = (DX2 * DX2) + (DY2 * DY2)

                       MOVE E_POS_X(I) TO E_SEE_RX
                       MOVE E_POS_Y(I) TO E_SEE_RY
                       MOVE PLAYER_X TO E_TARGET_X
                       MOVE PLAYER_Y TO E_TARGET_Y
                       PERFORM HAS-LOS

                       IF DIST_SQ <= CHASE_RANGE_SQ AND E_SEE_HIT = 0
                           MOVE 120 TO E_IS_AGGRO(I)
                           IF DIST_SQ <= ATTACK_RANGE_SQ AND E_ATTACK_COOLDOWN(I) = 0
                               MOVE E_STATE_ATTACK TO E_STATE(I)
                               PERFORM ENEMY-ATTACK
                           ELSE
                               MOVE E_STATE_CHASE TO E_STATE(I)
                           END-IF
                       ELSE
                           IF E_IS_AGGRO(I) > 0
                               SUBTRACT 1 FROM E_IS_AGGRO(I)
                               MOVE E_STATE_CHASE TO E_STATE(I)
                           ELSE
                               MOVE E_STATE_LOOK TO E_STATE(I)
                           END-IF
                       END-IF

                       IF E_PAIN_TIMER(I) > 0
                           MOVE E_STATE_PAIN TO E_STATE(I)
                       END-IF

                       EVALUATE E_STATE(I)
                           WHEN E_STATE_CHASE
                               PERFORM ENEMY-CHASE
                           WHEN E_STATE_ATTACK
                               CONTINUE
                           WHEN E_STATE_PAIN
                               CONTINUE
                           WHEN E_STATE_DEAD
                               CONTINUE
                           WHEN OTHER
                               PERFORM ENEMY-PATROL
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM

           PERFORM UPDATE-PICKUPS.
      
      HAS-LOS.
           *> returns E_SEE_HIT = 1 if blocked, 0 if clear
           MOVE 0 TO E_SEE_HIT

           IF WORLD_MODE = 1
               MOVE E_SEE_RX TO TRACE_START_X
               MOVE E_SEE_RY TO TRACE_START_Y
               MOVE E_TARGET_X TO TRACE_END_X
               MOVE E_TARGET_Y TO TRACE_END_Y
               MOVE 0.0 TO TRACE_CURR_T
               MOVE 0 TO TRACE_HIT_BLOCKED

               MOVE TRACE_START_X TO TRACE_END_X
               MOVE TRACE_START_Y TO TRACE_END_Y
               PERFORM FIND-SECTOR-AT
               MOVE TRACE_END_SECTOR TO TRACE_CURR_SECTOR

               IF TRACE_CURR_SECTOR = 0
                   MOVE 1 TO E_SEE_HIT
                   EXIT PARAGRAPH
               END-IF

               MOVE E_TARGET_X TO TRACE_END_X
               MOVE E_TARGET_Y TO TRACE_END_Y

               PERFORM VARYING WORLD_STEP_COUNT FROM 1 BY 1 UNTIL WORLD_STEP_COUNT > MAX_WORLD_STEPS OR TRACE_HIT_BLOCKED = 1 OR TRACE_CURR_T >= 1.0
                   PERFORM FIND-NEAREST-SECTOR-HIT

                   IF TRACE_HIT_FOUND = 0
                       EXIT PERFORM
                   END-IF

                   PERFORM GET-LINE-TRANSITION

                   IF TRACE_NEXT_SECTOR <= 0 OR L_FLAGS(TRACE_HIT_LINEDEF) <> 0
                       MOVE 1 TO TRACE_HIT_BLOCKED
                   ELSE
                       IF WORLD_OPENING_TOP - WORLD_OPENING_BOT <= 0.05
                           MOVE 1 TO TRACE_HIT_BLOCKED
                       ELSE
                           MOVE TRACE_NEXT_SECTOR TO TRACE_CURR_SECTOR
                           MOVE TRACE_CLOSEST_T TO TRACE_CURR_T
                       END-IF
                   END-IF
               END-PERFORM

               IF TRACE_HIT_BLOCKED = 1
                   MOVE 1 TO E_SEE_HIT
               END-IF
           ELSE
               COMPUTE E_SEE_DX = E_TARGET_X - E_SEE_RX
               COMPUTE E_SEE_DY = E_TARGET_Y - E_SEE_RY
               COMPUTE E_SEE_DIST = FUNCTION SQRT(E_SEE_DX*E_SEE_DX + E_SEE_DY*E_SEE_DY)
       
               IF E_SEE_DIST < 0.001
                   EXIT PARAGRAPH
               END-IF
       
               COMPUTE E_SEE_DX = E_SEE_DX / E_SEE_DIST
               COMPUTE E_SEE_DY = E_SEE_DY / E_SEE_DIST
       
               MOVE 0.0 TO E_SEE_T
               PERFORM UNTIL E_SEE_T >= E_SEE_DIST OR E_SEE_HIT = 1
                   COMPUTE E_SEE_RX = E_SEE_RX + E_SEE_DX * STEP_DIST
                   COMPUTE E_SEE_RY = E_SEE_RY + E_SEE_DY * STEP_DIST
                   ADD STEP_DIST TO E_SEE_T

                   COMPUTE CELL_X = FUNCTION INTEGER(E_SEE_RX)
                   COMPUTE CELL_Y = FUNCTION INTEGER(E_SEE_RY)

                   IF CELL_X < 0 OR CELL_X >= MAP_WIDTH OR CELL_Y < 0 OR CELL_Y >= MAP_HEIGHT
                       MOVE 1 TO E_SEE_HIT
                   ELSE
                       COMPUTE MAP_PX = CELL_Y + 1
                       COMPUTE MAP_PY = CELL_X + 1
                       IF MAP-ROW(MAP_PX)(MAP_PY:1) NOT = "0"
                           MOVE 1 TO E_SEE_HIT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

      TRACE-MOVE-WORLD.
           MOVE 0 TO TRACE_HIT_BLOCKED
           MOVE 0 TO TRACE_END_SECTOR
           MOVE 0.0 TO TRACE_CURR_T

           MOVE TRACE_START_X TO TRACE_END_X
           MOVE TRACE_START_Y TO TRACE_END_Y
           PERFORM FIND-SECTOR-AT
           MOVE TRACE_END_SECTOR TO TRACE_CURR_SECTOR

           IF TRACE_CURR_SECTOR = 0
               MOVE 1 TO TRACE_HIT_BLOCKED
               EXIT PARAGRAPH
           END-IF

           MOVE WORLD_X1 TO TRACE_END_X
           MOVE WORLD_Y1 TO TRACE_END_Y

           PERFORM VARYING WORLD_STEP_COUNT FROM 1 BY 1 UNTIL WORLD_STEP_COUNT > MAX_WORLD_STEPS OR TRACE_HIT_BLOCKED = 1 OR TRACE_CURR_T >= 1.0
               PERFORM FIND-NEAREST-SECTOR-HIT

               IF TRACE_HIT_FOUND = 0
                   EXIT PERFORM
               END-IF

               PERFORM GET-LINE-TRANSITION

               IF TRACE_NEXT_SECTOR <= 0 OR L_FLAGS(TRACE_HIT_LINEDEF) <> 0
                   MOVE 1 TO TRACE_HIT_BLOCKED
               ELSE
                   IF WORLD_OPENING_TOP - WORLD_OPENING_BOT < PLAYER_HEIGHT
                       MOVE 1 TO TRACE_HIT_BLOCKED
                   ELSE
                       COMPUTE DX = S_FLOOR_Z(TRACE_NEXT_SECTOR) - S_FLOOR_Z(TRACE_CURR_SECTOR)
                       IF DX > PLAYER_STEP_Z
                           MOVE 1 TO TRACE_HIT_BLOCKED
                       ELSE
                           MOVE TRACE_NEXT_SECTOR TO TRACE_CURR_SECTOR
                           MOVE TRACE_CLOSEST_T TO TRACE_CURR_T
                           MOVE TRACE_CURR_SECTOR TO TRACE_END_SECTOR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF TRACE_HIT_BLOCKED = 0 AND TRACE_END_SECTOR = 0
               MOVE TRACE_CURR_SECTOR TO TRACE_END_SECTOR
           END-IF.

      RESOLVE-WORLD-COLLISION.
           IF WORLD_ENTITY_SECTOR <= 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WORLD_ENTITY_CLEARANCE = WORLD_ENTITY_RADIUS + 0.02

           PERFORM VARYING WORLD_ENTITY_ITER FROM 1 BY 1 UNTIL WORLD_ENTITY_ITER > 3
               MOVE 0 TO WORLD_ENTITY_BLOCKED

               PERFORM VARYING WORLD_LINEDEF_ID FROM 1 BY 1 UNTIL WORLD_LINEDEF_ID > LINEDEFS_NUM
                   IF L_IS_ACTIVE(WORLD_LINEDEF_ID) = 1
                       AND (L_FRONT_SECTOR(WORLD_LINEDEF_ID) = WORLD_ENTITY_SECTOR OR L_BACK_SECTOR(WORLD_LINEDEF_ID) = WORLD_ENTITY_SECTOR)
                       MOVE 0 TO WORLD_ENTITY_PASSABLE
                       MOVE 0 TO WORLD_ENTITY_OTHER_SECTOR

                       IF L_FRONT_SECTOR(WORLD_LINEDEF_ID) = WORLD_ENTITY_SECTOR
                           MOVE L_BACK_SECTOR(WORLD_LINEDEF_ID) TO WORLD_ENTITY_OTHER_SECTOR
                       ELSE
                           MOVE L_FRONT_SECTOR(WORLD_LINEDEF_ID) TO WORLD_ENTITY_OTHER_SECTOR
                       END-IF

                       IF WORLD_ENTITY_OTHER_SECTOR > 0 AND L_FLAGS(WORLD_LINEDEF_ID) = 0
                           IF WORLD_ENTITY_OTHER_SECTOR >= 1 AND WORLD_ENTITY_OTHER_SECTOR <= SECTORS_NUM
                               IF S_IS_ACTIVE(WORLD_ENTITY_OTHER_SECTOR) = 1
                                   MOVE WORLD_ENTITY_SECTOR TO TRACE_CURR_SECTOR
                                   MOVE WORLD_LINEDEF_ID TO TRACE_HIT_LINEDEF
                                   PERFORM GET-LINE-TRANSITION
                                   IF WORLD_OPENING_TOP - WORLD_OPENING_BOT >= PLAYER_HEIGHT
                                       COMPUTE DX = S_FLOOR_Z(WORLD_ENTITY_OTHER_SECTOR) - S_FLOOR_Z(WORLD_ENTITY_SECTOR)
                                       IF DX <= PLAYER_STEP_Z
                                           MOVE 1 TO WORLD_ENTITY_PASSABLE
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF

                       IF WORLD_ENTITY_PASSABLE = 0
                           COMPUTE WORLD_X1 = L_X2(WORLD_LINEDEF_ID) - L_X1(WORLD_LINEDEF_ID)
                           COMPUTE WORLD_Y1 = L_Y2(WORLD_LINEDEF_ID) - L_Y1(WORLD_LINEDEF_ID)
                           COMPUTE WORLD_ENTITY_SEG_LEN_SQ = (WORLD_X1 * WORLD_X1) + (WORLD_Y1 * WORLD_Y1)

                           IF WORLD_ENTITY_SEG_LEN_SQ > TRACE_EPS
                               COMPUTE T = (((WORLD_ENTITY_X - L_X1(WORLD_LINEDEF_ID)) * WORLD_X1) + ((WORLD_ENTITY_Y - L_Y1(WORLD_LINEDEF_ID)) * WORLD_Y1)) / WORLD_ENTITY_SEG_LEN_SQ

                               IF T < 0.0
                                   MOVE 0.0 TO T
                               END-IF
                               IF T > 1.0
                                   MOVE 1.0 TO T
                               END-IF

                               COMPUTE WORLD_ENTITY_CLOSE_X = L_X1(WORLD_LINEDEF_ID) + (T * WORLD_X1)
                               COMPUTE WORLD_ENTITY_CLOSE_Y = L_Y1(WORLD_LINEDEF_ID) + (T * WORLD_Y1)
                               COMPUTE FX = WORLD_ENTITY_X - WORLD_ENTITY_CLOSE_X
                               COMPUTE FY = WORLD_ENTITY_Y - WORLD_ENTITY_CLOSE_Y
                               COMPUTE WORLD_ENTITY_DIST_SQ = (FX * FX) + (FY * FY)

                               IF WORLD_ENTITY_DIST_SQ < (WORLD_ENTITY_CLEARANCE * WORLD_ENTITY_CLEARANCE)
                                   MOVE 1 TO WORLD_ENTITY_BLOCKED

                                   IF WORLD_ENTITY_DIST_SQ > TRACE_EPS
                                       COMPUTE WORLD_ENTITY_DIST = FUNCTION SQRT(WORLD_ENTITY_DIST_SQ)
                                       COMPUTE WORLD_ENTITY_PUSH = WORLD_ENTITY_CLEARANCE - WORLD_ENTITY_DIST
                                       COMPUTE WORLD_ENTITY_X = WORLD_ENTITY_X + ((FX / WORLD_ENTITY_DIST) * WORLD_ENTITY_PUSH)
                                       COMPUTE WORLD_ENTITY_Y = WORLD_ENTITY_Y + ((FY / WORLD_ENTITY_DIST) * WORLD_ENTITY_PUSH)
                                   ELSE
                                       COMPUTE WORLD_ENTITY_PUSH_X = -WORLD_Y1
                                       COMPUTE WORLD_ENTITY_PUSH_Y = WORLD_X1
                                       COMPUTE WORLD_ENTITY_PUSH_LEN = FUNCTION SQRT((WORLD_ENTITY_PUSH_X * WORLD_ENTITY_PUSH_X) + (WORLD_ENTITY_PUSH_Y * WORLD_ENTITY_PUSH_Y))

                                       IF WORLD_ENTITY_PUSH_LEN > TRACE_EPS
                                           COMPUTE WORLD_ENTITY_PUSH_X = WORLD_ENTITY_PUSH_X / WORLD_ENTITY_PUSH_LEN
                                           COMPUTE WORLD_ENTITY_PUSH_Y = WORLD_ENTITY_PUSH_Y / WORLD_ENTITY_PUSH_LEN
                                           COMPUTE WORLD_ENTITY_X = WORLD_ENTITY_X + (WORLD_ENTITY_PUSH_X * WORLD_ENTITY_CLEARANCE)
                                           COMPUTE WORLD_ENTITY_Y = WORLD_ENTITY_Y + (WORLD_ENTITY_PUSH_Y * WORLD_ENTITY_CLEARANCE)
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               MOVE WORLD_ENTITY_X TO TRACE_END_X
               MOVE WORLD_ENTITY_Y TO TRACE_END_Y
               PERFORM FIND-SECTOR-AT
               IF TRACE_END_SECTOR > 0
                   MOVE TRACE_END_SECTOR TO WORLD_ENTITY_SECTOR
               END-IF

               IF WORLD_ENTITY_BLOCKED = 0
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      GET-SPRITE-VISIBILITY.
           MOVE 0 TO SPR_VIS_BLOCKED
           MOVE 0 TO SPR_VIS_TOP
           MOVE WINDOW_HEIGHT_MIN_1 TO SPR_VIS_BOT

           IF PLAYER_SECTOR <= 0 OR SPR_VIS_TARGET_SECTOR <= 0
               MOVE 1 TO SPR_VIS_BLOCKED
               EXIT PARAGRAPH
           END-IF

           MOVE PLAYER_X TO TRACE_START_X
           MOVE PLAYER_Y TO TRACE_START_Y
           MOVE SPR_VIS_TARGET_X TO TRACE_END_X
           MOVE SPR_VIS_TARGET_Y TO TRACE_END_Y
           MOVE PLAYER_SECTOR TO TRACE_CURR_SECTOR
           MOVE 0.0 TO TRACE_CURR_T

           PERFORM VARYING WORLD_STEP_COUNT FROM 1 BY 1 UNTIL WORLD_STEP_COUNT > MAX_WORLD_STEPS OR TRACE_CURR_T >= 1.0 OR SPR_VIS_BLOCKED = 1
               PERFORM FIND-NEAREST-SECTOR-HIT

               IF TRACE_HIT_FOUND = 0
                   EXIT PERFORM
               END-IF

               MOVE TRACE_HIT_DIST TO DIST
               IF DIST < 0.001
                   MOVE 0.001 TO DIST
               END-IF

               PERFORM GET-LINE-TRANSITION

               IF TRACE_NEXT_SECTOR <= 0 OR L_FLAGS(TRACE_HIT_LINEDEF) <> 0
                   MOVE 1 TO SPR_VIS_BLOCKED
               ELSE
                   IF WORLD_OPENING_TOP - WORLD_OPENING_BOT <= 0.05
                       MOVE 1 TO SPR_VIS_BLOCKED
                   ELSE
                       COMPUTE WORLD_CEIL_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_CEIL_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / DIST))
                       COMPUTE WORLD_FLOOR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_FLOOR_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / DIST))
                       COMPUTE WORLD_NEXT_CEIL_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_CEIL_Z(TRACE_NEXT_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / DIST))
                       COMPUTE WORLD_NEXT_FLOOR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_FLOOR_Z(TRACE_NEXT_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / DIST))

                       IF WORLD_NEXT_CEIL_Y > WORLD_CEIL_Y
                           IF WORLD_CEIL_Y > SPR_VIS_TOP
                               MOVE WORLD_CEIL_Y TO SPR_VIS_TOP
                           END-IF
                           IF WORLD_NEXT_CEIL_Y > SPR_VIS_TOP
                               MOVE WORLD_NEXT_CEIL_Y TO SPR_VIS_TOP
                           END-IF
                       END-IF

                       IF WORLD_NEXT_FLOOR_Y < WORLD_FLOOR_Y
                           IF WORLD_FLOOR_Y < SPR_VIS_BOT
                               MOVE WORLD_FLOOR_Y TO SPR_VIS_BOT
                           END-IF
                           IF WORLD_NEXT_FLOOR_Y < SPR_VIS_BOT
                               MOVE WORLD_NEXT_FLOOR_Y TO SPR_VIS_BOT
                           END-IF
                       END-IF

                       MOVE TRACE_NEXT_SECTOR TO TRACE_CURR_SECTOR
                       MOVE TRACE_CLOSEST_T TO TRACE_CURR_T
                   END-IF
               END-IF
           END-PERFORM

           IF SPR_VIS_TOP > SPR_VIS_BOT
               MOVE 1 TO SPR_VIS_BLOCKED
           END-IF.
      
      ENEMY-PATROL.
           IF E_WP_COUNT(I) <= 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE E_TARGET_X = E_WP_X(I, E_WP_IDX(I))
           COMPUTE E_TARGET_Y = E_WP_Y(I, E_WP_IDX(I))
       
           COMPUTE DX2 = E_TARGET_X - E_POS_X(I)
           COMPUTE DY2 = E_TARGET_Y - E_POS_Y(I)
           COMPUTE DIST = FUNCTION SQRT((DX2*DX2) + (DY2*DY2))
       
           IF DIST < 0.10
               ADD 1 TO E_WP_IDX(I)
               IF E_WP_IDX(I) > E_WP_COUNT(I)
                   MOVE 1 TO E_WP_IDX(I)
               END-IF
               EXIT PARAGRAPH
           END-IF
       
           PERFORM ENEMY-STEP-TOWARD.
      
      ENEMY-CHASE.
           IF DIST_SQ >= LOST_RANGE_SQ AND E_IS_AGGRO(I) = 0
               MOVE E_STATE_LOOK TO E_STATE(I)
               MOVE 0 TO E_IS_AGGRO(I)
               EXIT PARAGRAPH
           END-IF
       
           MOVE PLAYER_X TO E_TARGET_X
           MOVE PLAYER_Y TO E_TARGET_Y
           PERFORM ENEMY-STEP-TOWARD.

      ENEMY-ATTACK.
           IF E_ATTACK_COOLDOWN(I) > 0
               EXIT PARAGRAPH
           END-IF

           MOVE 20 TO E_ATTACK_COOLDOWN(I)
           SUBTRACT WORLD_ATTACK_DAMAGE FROM PLAYER_HP
           IF PLAYER_HP < 0
               MOVE 0 TO PLAYER_HP
           END-IF
           MOVE 4 TO LAST_HIT_TIMER.
      
      ENEMY-STEP-TOWARD.
           COMPUTE DX2 = E_TARGET_X - E_POS_X(I)
           COMPUTE DY2 = E_TARGET_Y - E_POS_Y(I)
           COMPUTE DIST = FUNCTION SQRT((DX2*DX2) + (DY2*DY2))
       
           IF DIST < 0.001
               EXIT PARAGRAPH
           END-IF
       
           COMPUTE ENEMY_NEXT_POS_X = E_POS_X(I) + (DX2 / DIST) * E_SPEED(I)
           COMPUTE ENEMY_NEXT_POS_Y = E_POS_Y(I) + (DY2 / DIST) * E_SPEED(I)

           IF WORLD_MODE = 1
               MOVE E_POS_X(I) TO TRACE_START_X
               MOVE E_POS_Y(I) TO TRACE_START_Y
               MOVE ENEMY_NEXT_POS_X TO WORLD_X1
               MOVE ENEMY_NEXT_POS_Y TO WORLD_Y1
               PERFORM TRACE-MOVE-WORLD

               IF TRACE_HIT_BLOCKED = 0
                   MOVE ENEMY_NEXT_POS_X TO E_POS_X(I)
                   MOVE ENEMY_NEXT_POS_Y TO E_POS_Y(I)
                   MOVE TRACE_END_SECTOR TO E_SECTOR(I)
                   MOVE E_POS_X(I) TO WORLD_ENTITY_X
                   MOVE E_POS_Y(I) TO WORLD_ENTITY_Y
                   MOVE E_RADIUS(I) TO WORLD_ENTITY_RADIUS
                   MOVE E_SECTOR(I) TO WORLD_ENTITY_SECTOR
                   PERFORM RESOLVE-WORLD-COLLISION
                   MOVE WORLD_ENTITY_X TO E_POS_X(I)
                   MOVE WORLD_ENTITY_Y TO E_POS_Y(I)
                   MOVE WORLD_ENTITY_SECTOR TO E_SECTOR(I)
               ELSE
                   MOVE E_POS_X(I) TO TRACE_START_X
                   MOVE E_POS_Y(I) TO TRACE_START_Y
                   MOVE ENEMY_NEXT_POS_X TO WORLD_X1
                   MOVE E_POS_Y(I) TO WORLD_Y1
                   PERFORM TRACE-MOVE-WORLD
                   IF TRACE_HIT_BLOCKED = 0
                       MOVE ENEMY_NEXT_POS_X TO E_POS_X(I)
                       MOVE TRACE_END_SECTOR TO E_SECTOR(I)
                       MOVE E_POS_X(I) TO WORLD_ENTITY_X
                       MOVE E_POS_Y(I) TO WORLD_ENTITY_Y
                       MOVE E_RADIUS(I) TO WORLD_ENTITY_RADIUS
                       MOVE E_SECTOR(I) TO WORLD_ENTITY_SECTOR
                       PERFORM RESOLVE-WORLD-COLLISION
                       MOVE WORLD_ENTITY_X TO E_POS_X(I)
                       MOVE WORLD_ENTITY_Y TO E_POS_Y(I)
                       MOVE WORLD_ENTITY_SECTOR TO E_SECTOR(I)
                   ELSE
                       MOVE E_POS_X(I) TO TRACE_START_X
                       MOVE E_POS_Y(I) TO TRACE_START_Y
                       MOVE E_POS_X(I) TO WORLD_X1
                       MOVE ENEMY_NEXT_POS_Y TO WORLD_Y1
                       PERFORM TRACE-MOVE-WORLD
                       IF TRACE_HIT_BLOCKED = 0
                           MOVE ENEMY_NEXT_POS_Y TO E_POS_Y(I)
                           MOVE TRACE_END_SECTOR TO E_SECTOR(I)
                           MOVE E_POS_X(I) TO WORLD_ENTITY_X
                           MOVE E_POS_Y(I) TO WORLD_ENTITY_Y
                           MOVE E_RADIUS(I) TO WORLD_ENTITY_RADIUS
                           MOVE E_SECTOR(I) TO WORLD_ENTITY_SECTOR
                           PERFORM RESOLVE-WORLD-COLLISION
                           MOVE WORLD_ENTITY_X TO E_POS_X(I)
                           MOVE WORLD_ENTITY_Y TO E_POS_Y(I)
                           MOVE WORLD_ENTITY_SECTOR TO E_SECTOR(I)
                       END-IF
                   END-IF
               END-IF
           ELSE
               COMPUTE CELL_X = FUNCTION INTEGER(ENEMY_NEXT_POS_X)
               COMPUTE CELL_Y = FUNCTION INTEGER(ENEMY_NEXT_POS_Y)
       
               IF CELL_X >= 0 AND CELL_X < MAP_WIDTH
                   AND CELL_Y >= 0 AND CELL_Y < MAP_HEIGHT
                   AND MAP-ROW(CELL_Y + 1)(CELL_X + 1:1) = "0"
                   MOVE ENEMY_NEXT_POS_X TO E_POS_X(I)
                   MOVE ENEMY_NEXT_POS_Y TO E_POS_Y(I)
               END-IF
           END-IF.

      UPDATE-PICKUPS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THINGS_NUM
               IF T_IS_ACTIVE(I) = 1
                   COMPUTE DX2 = PLAYER_X - T_POS_X(I)
                   COMPUTE DY2 = PLAYER_Y - T_POS_Y(I)
                   COMPUTE DIST_SQ = (DX2 * DX2) + (DY2 * DY2)

                   IF DIST_SQ <= 0.25
                       IF T_KIND(I) = THING_HP
                           ADD T_VALUE(I) TO PLAYER_HP
                           IF PLAYER_HP > PLAYER_MAX_HP
                               MOVE PLAYER_MAX_HP TO PLAYER_HP
                           END-IF
                       END-IF
                       MOVE 0 TO T_IS_ACTIVE(I)
                   END-IF
               END-IF
           END-PERFORM.
      
      POLL-INPUT.
           MOVE X"00" TO INPUT_KEY
           CALL "read" USING
               BY VALUE STDIN_FD
               BY REFERENCE INPUT_KEY
               BY VALUE 1
           RETURNING RRET
           END-CALL
       
           IF RRET <= 0
               EXIT PARAGRAPH
           END-IF
       
           EVALUATE INPUT_KEY
               WHEN "q" WHEN "Q"
                   MOVE 1 TO QUIT_FLAG
               WHEN "a" WHEN "A"
                   SUBTRACT 0.08 FROM PLAYER_A
               WHEN "d" WHEN "D"
                   ADD 0.08 TO PLAYER_A
               WHEN "w" WHEN "W"
                   PERFORM MOVE-FORWARD
               WHEN "s" WHEN "S"
                   PERFORM MOVE-BACKWARD
               WHEN X"20"
                   MOVE 1 TO SHOOT_REQUEST
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      
      MOVE-FORWARD.
           COMPUTE PLAYER_MOVE_DX = FUNCTION COS(PLAYER_A) * 0.1
           COMPUTE PLAYER_MOVE_DY = FUNCTION SIN(PLAYER_A) * 0.1
           PERFORM TRY-MOVE.
      
      MOVE-BACKWARD.
           COMPUTE PLAYER_MOVE_DX = FUNCTION COS(PLAYER_A) * -0.1
           COMPUTE PLAYER_MOVE_DY = FUNCTION SIN(PLAYER_A) * -0.1
           PERFORM TRY-MOVE.
      
      TRY-MOVE.
           COMPUTE PLAYER_NEXT_POS_X = PLAYER_X + PLAYER_MOVE_DX
           COMPUTE PLAYER_NEXT_POS_Y = PLAYER_Y + PLAYER_MOVE_DY

           IF WORLD_MODE = 1
               MOVE PLAYER_X TO TRACE_START_X
               MOVE PLAYER_Y TO TRACE_START_Y
               MOVE PLAYER_NEXT_POS_X TO WORLD_X1
               MOVE PLAYER_NEXT_POS_Y TO WORLD_Y1
               PERFORM TRACE-MOVE-WORLD

               IF TRACE_HIT_BLOCKED = 0
                   MOVE PLAYER_NEXT_POS_X TO PLAYER_X
                   MOVE PLAYER_NEXT_POS_Y TO PLAYER_Y
                   MOVE TRACE_END_SECTOR TO PLAYER_SECTOR
                   MOVE PLAYER_X TO WORLD_ENTITY_X
                   MOVE PLAYER_Y TO WORLD_ENTITY_Y
                   MOVE PLAYER_RADIUS TO WORLD_ENTITY_RADIUS
                   MOVE PLAYER_SECTOR TO WORLD_ENTITY_SECTOR
                   PERFORM RESOLVE-WORLD-COLLISION
                   MOVE WORLD_ENTITY_X TO PLAYER_X
                   MOVE WORLD_ENTITY_Y TO PLAYER_Y
                   MOVE WORLD_ENTITY_SECTOR TO PLAYER_SECTOR
               ELSE
                   MOVE PLAYER_X TO TRACE_START_X
                   MOVE PLAYER_Y TO TRACE_START_Y
                   MOVE PLAYER_NEXT_POS_X TO WORLD_X1
                   MOVE PLAYER_Y TO WORLD_Y1
                   PERFORM TRACE-MOVE-WORLD
                   IF TRACE_HIT_BLOCKED = 0
                       MOVE PLAYER_NEXT_POS_X TO PLAYER_X
                       MOVE TRACE_END_SECTOR TO PLAYER_SECTOR
                       MOVE PLAYER_X TO WORLD_ENTITY_X
                       MOVE PLAYER_Y TO WORLD_ENTITY_Y
                       MOVE PLAYER_RADIUS TO WORLD_ENTITY_RADIUS
                       MOVE PLAYER_SECTOR TO WORLD_ENTITY_SECTOR
                       PERFORM RESOLVE-WORLD-COLLISION
                       MOVE WORLD_ENTITY_X TO PLAYER_X
                       MOVE WORLD_ENTITY_Y TO PLAYER_Y
                       MOVE WORLD_ENTITY_SECTOR TO PLAYER_SECTOR
                   ELSE
                       MOVE PLAYER_X TO TRACE_START_X
                       MOVE PLAYER_Y TO TRACE_START_Y
                       MOVE PLAYER_X TO WORLD_X1
                       MOVE PLAYER_NEXT_POS_Y TO WORLD_Y1
                       PERFORM TRACE-MOVE-WORLD
                       IF TRACE_HIT_BLOCKED = 0
                           MOVE PLAYER_NEXT_POS_Y TO PLAYER_Y
                           MOVE TRACE_END_SECTOR TO PLAYER_SECTOR
                           MOVE PLAYER_X TO WORLD_ENTITY_X
                           MOVE PLAYER_Y TO WORLD_ENTITY_Y
                           MOVE PLAYER_RADIUS TO WORLD_ENTITY_RADIUS
                           MOVE PLAYER_SECTOR TO WORLD_ENTITY_SECTOR
                           PERFORM RESOLVE-WORLD-COLLISION
                           MOVE WORLD_ENTITY_X TO PLAYER_X
                           MOVE WORLD_ENTITY_Y TO PLAYER_Y
                           MOVE WORLD_ENTITY_SECTOR TO PLAYER_SECTOR
                       END-IF
                   END-IF
               END-IF

               IF PLAYER_SECTOR > 0
                   MOVE S_FLOOR_Z(PLAYER_SECTOR) TO WORLD_PLAYER_FLOOR_Z
               END-IF
           ELSE
               COMPUTE CELL_X = FUNCTION INTEGER(PLAYER_NEXT_POS_X)
               COMPUTE CELL_Y = FUNCTION INTEGER(PLAYER_NEXT_POS_Y)
       
               IF CELL_X < 0 OR CELL_X >= MAP_WIDTH OR CELL_Y < 0 OR CELL_Y >= MAP_HEIGHT
                   EXIT PARAGRAPH
               END-IF
       
               COMPUTE MAP_PX = CELL_Y + 1
               COMPUTE MAP_PY = CELL_X + 1
       
               IF MAP-ROW(MAP_PX)(MAP_PY:1) = "0"
                   MOVE PLAYER_NEXT_POS_X TO PLAYER_X
                   MOVE PLAYER_NEXT_POS_Y TO PLAYER_Y
               END-IF
           END-IF.
      
      DO-FIRE.
           IF SHOOT_COOLDOWN > 0
               EXIT PARAGRAPH
           END-IF
       
           MOVE 3 TO FLASH_TIMER
           MOVE 6 TO SHOOT_COOLDOWN
       
           COMPUTE RAY_A = PLAYER_A
           COMPUTE RAY_DX = FUNCTION COS(RAY_A)
           COMPUTE RAY_DY = FUNCTION SIN(RAY_A)

           IF WORLD_MODE = 1
               MOVE PLAYER_X TO TRACE_START_X
               MOVE PLAYER_Y TO TRACE_START_Y
               COMPUTE TRACE_END_X = PLAYER_X + (RAY_DX * MAX_DIST)
               COMPUTE TRACE_END_Y = PLAYER_Y + (RAY_DY * MAX_DIST)
               MOVE 0.0 TO TRACE_CURR_T
               MOVE 0 TO TRACE_HIT_BLOCKED
               MOVE 0 TO TRACE_HIT_FOUND

               MOVE PLAYER_X TO TRACE_END_X
               MOVE PLAYER_Y TO TRACE_END_Y
               PERFORM FIND-SECTOR-AT
               MOVE TRACE_END_SECTOR TO TRACE_CURR_SECTOR

               IF TRACE_CURR_SECTOR = 0
                   MOVE MAX_DIST TO DIST
               ELSE
                   COMPUTE TRACE_END_X = PLAYER_X + (RAY_DX * MAX_DIST)
                   COMPUTE TRACE_END_Y = PLAYER_Y + (RAY_DY * MAX_DIST)
                   MOVE MAX_DIST TO DIST

                   PERFORM VARYING WORLD_STEP_COUNT FROM 1 BY 1 UNTIL WORLD_STEP_COUNT > MAX_WORLD_STEPS OR TRACE_HIT_BLOCKED = 1 OR TRACE_CURR_T >= 1.0
                       PERFORM FIND-NEAREST-SECTOR-HIT

                       IF TRACE_HIT_FOUND = 0
                           EXIT PERFORM
                       END-IF

                       PERFORM GET-LINE-TRANSITION

                       IF TRACE_NEXT_SECTOR <= 0 OR L_FLAGS(TRACE_HIT_LINEDEF) <> 0
                           MOVE 1 TO TRACE_HIT_BLOCKED
                           MOVE TRACE_HIT_DIST TO DIST
                       ELSE
                           IF WORLD_OPENING_TOP - WORLD_OPENING_BOT <= 0.05
                               MOVE 1 TO TRACE_HIT_BLOCKED
                               MOVE TRACE_HIT_DIST TO DIST
                           ELSE
                               MOVE TRACE_NEXT_SECTOR TO TRACE_CURR_SECTOR
                               MOVE TRACE_CLOSEST_T TO TRACE_CURR_T
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
               MOVE 0 TO HIT
               MOVE STEP_DIST TO T
               MOVE MAX_DIST TO DIST
       
               PERFORM UNTIL HIT = 1 OR T >= MAX_DIST
                   COMPUTE RAY_X = PLAYER_X + (RAY_DX * T)
                   COMPUTE RAY_Y = PLAYER_Y + (RAY_DY * T)
       
                   COMPUTE CELL_X = FUNCTION INTEGER(RAY_X)
                   COMPUTE CELL_Y = FUNCTION INTEGER(RAY_Y)
       
                   IF CELL_X < 0 OR CELL_X >= MAP_WIDTH OR CELL_Y < 0 OR CELL_Y >= MAP_HEIGHT
                       MOVE 1 TO HIT
                       MOVE MAX_DIST TO DIST
                   ELSE
                       COMPUTE MAP_PX = CELL_Y + 1
                       COMPUTE MAP_PY = CELL_X + 1
                       MOVE MAP-ROW(MAP_PX)(MAP_PY:1) TO MAP_PVAL
       
                       IF MAP_PVAL = "1"
                           MOVE 1 TO HIT
                           MOVE T TO DIST
                       ELSE
                           ADD STEP_DIST TO T
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
       
           MOVE DIST TO LAST_HIT_DIST
           MOVE 6 TO LAST_HIT_TIMER
       
           MOVE 0 TO BEST_ENEMY
           MOVE 1.0E9 TO BEST_ALONG
       
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
               IF E_IS_ACTIVE(I) = 1 AND E_STATE(I) NOT = E_STATE_DEAD
                   COMPUTE DX = E_POS_X(I) - PLAYER_X
                   COMPUTE DY = E_POS_Y(I) - PLAYER_Y
       
                   COMPUTE ALONG = (DX * RAY_DX) + (DY * RAY_DY)
                   IF ALONG > 0.0 AND ALONG < DIST
                       COMPUTE D2 = (DX*DX + DY*DY) - (ALONG*ALONG)
                       COMPUTE R2 = E_RADIUS(I) * E_RADIUS(I)
                       IF D2 <= R2
                           MOVE PLAYER_X TO E_SEE_RX
                           MOVE PLAYER_Y TO E_SEE_RY
                           MOVE E_POS_X(I) TO E_TARGET_X
                           MOVE E_POS_Y(I) TO E_TARGET_Y
                           PERFORM HAS-LOS
                           IF E_SEE_HIT = 0
                               IF ALONG < BEST_ALONG
                                   MOVE I TO BEST_ENEMY
                                   MOVE ALONG TO BEST_ALONG
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
       
           IF BEST_ENEMY > 0
               SUBTRACT DAMAGE FROM E_HP(BEST_ENEMY)
               IF E_HP(BEST_ENEMY) <= 0
                   MOVE E_STATE_DEAD TO E_STATE(BEST_ENEMY)
                   MOVE 0 TO E_IS_ACTIVE(BEST_ENEMY)
                   MOVE 0 TO E_IS_AGGRO(BEST_ENEMY)
                   MOVE 0 TO E_ATTACK_COOLDOWN(BEST_ENEMY)
                   MOVE 0 TO E_PAIN_TIMER(BEST_ENEMY)
               ELSE
                   MOVE 6 TO E_PAIN_TIMER(BEST_ENEMY)
                   MOVE E_STATE_PAIN TO E_STATE(BEST_ENEMY)
                   MOVE 60 TO E_IS_AGGRO(BEST_ENEMY)
               END-IF
           END-IF.
      
      BUILD-FRAME.
           PERFORM CLEAR-FRAMEBUF

           IF WORLD_MODE = 1
               PERFORM BUILD-FRAME-WORLD
           ELSE
               PERFORM VARYING X FROM 0 BY 1 UNTIL X >= WINDOW_WIDTH
               COMPUTE RAY_A  = (PLAYER_A - FOV_HALF) + (FOV * X / WINDOW_WIDTH)
               COMPUTE RAY_DX = FUNCTION COS(RAY_A)
               COMPUTE RAY_DY = FUNCTION SIN(RAY_A)

               MOVE 0 TO HIT
               MOVE STEP_DIST TO T
               MOVE MAX_DIST TO DIST
               PERFORM UNTIL HIT = 1 OR T >= MAX_DIST
                   COMPUTE RAY_X = PLAYER_X + (RAY_DX * T)
                   COMPUTE RAY_Y = PLAYER_Y + (RAY_DY * T)

                  *> Use DDA for better sprite rendering
                   COMPUTE MAP_X = FUNCTION INTEGER(PLAYER_X)
                   COMPUTE MAP_Y = FUNCTION INTEGER(PLAYER_Y)
    
                   IF RAY_DX = 0.0
                       MOVE 1.0E9 TO DX
                   ELSE
                       COMPUTE DX = FUNCTION ABS(1.0 / RAY_DX)
                   END-IF
    
                   IF RAY_DY = 0.0
                       MOVE 1.0E9 TO DY
                   ELSE
                       COMPUTE DY = FUNCTION ABS(1.0 / RAY_DY)
                   END-IF

                   IF RAY_DX < 0.0
                       MOVE -1 TO STEP_X
                       COMPUTE SIDE_X = (PLAYER_X - MAP_X) * DX
                   ELSE
                       MOVE 1 TO STEP_X
                       COMPUTE SIDE_X = ((MAP_X + 1.0) - PLAYER_X) * DX
                   END-IF
    
                   IF RAY_DY < 0.0
                       MOVE -1 TO STEP_Y
                       COMPUTE SIDE_Y = (PLAYER_Y - MAP_Y) * DY
                   ELSE
                       MOVE 1 TO STEP_Y
                       COMPUTE SIDE_Y = ((MAP_Y + 1.0) - PLAYER_Y) * DY
                   END-IF
    
                   MOVE 0 TO HIT
    
                   PERFORM UNTIL HIT = 1
                       IF SIDE_X < SIDE_Y
                           COMPUTE SIDE_X = SIDE_X + DX
                           ADD STEP_X TO MAP_X
                           MOVE 0 TO SIDE
                       ELSE
                           COMPUTE SIDE_Y = SIDE_Y + DY
                           ADD STEP_Y TO MAP_Y
                           MOVE 1 TO SIDE
                       END-IF
    
                       IF MAP_X < 0 OR MAP_X >= MAP_WIDTH OR MAP_Y < 0 OR MAP_Y >= MAP_HEIGHT
                           MOVE 1 TO HIT
                           MOVE MAX_DIST TO PERP_DIST
                           MOVE "1" TO MAP_PVAL
                       ELSE
                           MOVE MAP-ROW(MAP_Y + 1)(MAP_X + 1:1) TO MAP_PVAL
                           IF MAP_PVAL NOT = "0"
                               MOVE 1 TO HIT
                           END-IF
                       END-IF
                   END-PERFORM
    
                   *> perpendicular distance (prevents fisheye)
                   IF PERP_DIST = MAX_DIST
                       MOVE MAX_DIST TO DIST
                   ELSE
                       IF SIDE = 0
                           COMPUTE PERP_DIST = (MAP_X - PLAYER_X + (1.0 - STEP_X) / 2.0) / RAY_DX
                       ELSE
                           COMPUTE PERP_DIST = (MAP_Y - PLAYER_Y + (1.0 - STEP_Y) / 2.0) / RAY_DY
                       END-IF
                       MOVE PERP_DIST TO DIST
                   END-IF
    
                   IF DIST < 0.001
                       MOVE 0.001 TO DIST
                   END-IF
    
                   MOVE DIST TO ZD(X + 1)
    
                   *> exact wall hit position (world coordinate)
                   IF SIDE = 0
                       COMPUTE WALL_X = PLAYER_Y + (DIST * RAY_DY)
                   ELSE
                       COMPUTE WALL_X = PLAYER_X + (DIST * RAY_DX)
                   END-IF
                   
                   COMPUTE WALL_X = WALL_X - FUNCTION INTEGER(WALL_X)
                   IF WALL_X < 0.0
                       COMPUTE WALL_X = WALL_X + 1.0
                   END-IF
                   
                   COMPUTE TEX_X = FUNCTION INTEGER(WALL_X * TEX_WIDTH)
                   
                   IF TEX_X < 0
                       MOVE 0 TO TEX_X
                   END-IF
                   IF TEX_X > TEX_WIDTH - 1
                       COMPUTE TEX_X = TEX_WIDTH - 1
                   END-IF
    
                   IF SIDE = 0 AND RAY_DX > 0.0
                       COMPUTE TEX_X = (TEX_WIDTH - 1) - TEX_X
                   END-IF
                   IF SIDE = 1 AND RAY_DY < 0.0
                       COMPUTE TEX_X = (TEX_WIDTH - 1) - TEX_X
                   END-IF
               END-PERFORM

               IF DIST < 0.001
                   MOVE 0.001 TO DIST
               END-IF

               MOVE DIST TO ZD(X + 1)

               COMPUTE SLICE_HEIGHT = PROJ_DIST / DIST
               
               IF SLICE_HEIGHT > WINDOW_HEIGHT * 2
                   COMPUTE SLICE_HEIGHT = WINDOW_HEIGHT * 2
               END-IF

               COMPUTE WALL_TOP = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER  - (SLICE_HEIGHT / 2.0))
               COMPUTE WALL_BOT = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER  + (SLICE_HEIGHT / 2.0))

               IF WALL_TOP < 0
                   MOVE 0 TO WALL_TOP
               END-IF
               IF WALL_BOT > WINDOW_HEIGHT_MIN_1
                   MOVE WINDOW_HEIGHT_MIN_1 TO WALL_BOT
               END-IF

               IF WALL_TOP > 0
                   MOVE 0 TO WALL_TEX_T
                   COMPUTE WALL_TEX_B = WALL_TOP - 1
                   MOVE CEIL_R TO CURR_PIXEL_R
                   MOVE CEIL_G TO CURR_PIXEL_G
                   MOVE CEIL_B TO CURR_PIXEL_B
                   MOVE X TO CURR_PIXEL_X
                   MOVE WALL_TEX_T TO TEX_X
                   MOVE WALL_TEX_B TO TEX_Y
                   PERFORM DRAW-VLINE-SOLID
               END-IF

               COMPUTE TEX_ID = FUNCTION NUMVAL(MAP_PVAL)
               IF TEX_ID < 1 OR TEX_ID > WALL_TEX_NUM
                   MOVE 1 TO TEX_ID
               END-IF

               COMPUTE FX = RAY_X - FUNCTION INTEGER(RAY_X)
               COMPUTE FY = RAY_Y - FUNCTION INTEGER(RAY_Y)

               COMPUTE EDGE_X = FX
               IF EDGE_X > (1.0 - FX)
                   COMPUTE EDGE_X = 1.0 - FX
               END-IF

               COMPUTE EDGE_Y = FY
               IF EDGE_Y > (1.0 - FY)
                   COMPUTE EDGE_Y = 1.0 - FY
               END-IF

               IF EDGE_X < EDGE_Y
                   COMPUTE TEX_X = FUNCTION INTEGER(FY * TEX_WIDTH)
               ELSE
                   COMPUTE TEX_X = FUNCTION INTEGER(FX * TEX_WIDTH)
               END-IF

               IF TEX_X < 0
                   MOVE 0 TO TEX_X
               END-IF
               IF TEX_X > TEX_WIDTH - 1
                   COMPUTE TEX_X = TEX_WIDTH - 1
               END-IF

               COMPUTE WALL_Y = (WALL_BOT - WALL_TOP) + 1
               IF WALL_Y < 1
                   MOVE 1 TO WALL_Y
               END-IF

               PERFORM VARYING Y FROM WALL_TOP BY 1 UNTIL Y > WALL_BOT
                   COMPUTE DY_SLICE = Y - WALL_TOP
                   COMPUTE TEX_Y = FUNCTION INTEGER((DY_SLICE * TEX_HEIGHT) / WALL_Y)

                   IF TEX_Y < 0
                       MOVE 0 TO TEX_Y
                   END-IF
                   IF TEX_Y > TEX_HEIGHT - 1
                       COMPUTE TEX_Y = TEX_HEIGHT - 1
                   END-IF

                   COMPUTE TEX_IDX = ((TEX_Y * TEX_WIDTH) + TEX_X) * 3 + 1
                   
                   MOVE FUNCTION ORD(W_TEX_DATA(TEX_ID, TEX_IDX))       TO CURR_PIXEL_R
                   MOVE FUNCTION ORD(W_TEX_DATA(TEX_ID, TEX_IDX + 1))   TO CURR_PIXEL_G
                   MOVE FUNCTION ORD(W_TEX_DATA(TEX_ID, TEX_IDX + 2))   TO CURR_PIXEL_B

                   COMPUTE SHADE = 1.0 / (1.0 + (DIST * 0.15))
                   COMPUTE CURR_PIXEL_R = FUNCTION INTEGER(CURR_PIXEL_R * SHADE)
                   COMPUTE CURR_PIXEL_G = FUNCTION INTEGER(CURR_PIXEL_G * SHADE)
                   COMPUTE CURR_PIXEL_B = FUNCTION INTEGER(CURR_PIXEL_B * SHADE)

                   MOVE X TO CURR_PIXEL_X
                   MOVE Y TO CURR_PIXEL_Y
                   PERFORM PUT-PIXEL
               END-PERFORM

               IF WALL_BOT < WINDOW_HEIGHT_MIN_1
                   COMPUTE WALL_TEX_T = WALL_BOT + 1
                   MOVE WINDOW_HEIGHT_MIN_1 TO WALL_TEX_B
                   MOVE FLOOR_R TO CURR_PIXEL_R
                   MOVE FLOOR_G TO CURR_PIXEL_G
                   MOVE FLOOR_B TO CURR_PIXEL_B
                   MOVE X TO CURR_PIXEL_X
                   MOVE WALL_TEX_T TO TEX_X
                   MOVE WALL_TEX_B TO TEX_Y
                   PERFORM DRAW-VLINE-SOLID
               END-IF
               END-PERFORM
           END-IF

           PERFORM INIT-SPRITE-ZBUFFER
           PERFORM DRAW-ENEMIES
           PERFORM DRAW-PICKUPS
           PERFORM DRAW-CROSSHAIR
           PERFORM DRAW-GUN.

      CLEAR-FRAMEBUF.
           MOVE LOW-VALUES TO FRAMEBUF.

      INIT-SPRITE-ZBUFFER.
           PERFORM VARYING X FROM 0 BY 1 UNTIL X >= WINDOW_WIDTH
               PERFORM VARYING Y FROM 0 BY 1 UNTIL Y >= WINDOW_HEIGHT
                   COMPUTE SPR_DEPTH_IDX = (Y * WINDOW_WIDTH) + X + 1
                   MOVE ZD(X + 1) TO SPR_ZD(SPR_DEPTH_IDX)
               END-PERFORM
           END-PERFORM.

      BUILD-FRAME-WORLD.
           IF PLAYER_SECTOR <= 0
               MOVE PLAYER_X TO TRACE_END_X
               MOVE PLAYER_Y TO TRACE_END_Y
               PERFORM FIND-SECTOR-AT
               MOVE TRACE_END_SECTOR TO PLAYER_SECTOR
           END-IF

           IF PLAYER_SECTOR > 0
               MOVE S_FLOOR_Z(PLAYER_SECTOR) TO WORLD_PLAYER_FLOOR_Z
           ELSE
               MOVE 0.0 TO WORLD_PLAYER_FLOOR_Z
           END-IF

           COMPUTE WORLD_PLAYER_Z = WORLD_PLAYER_FLOOR_Z + PLAYER_EYE_Z

           PERFORM VARYING X FROM 0 BY 1 UNTIL X >= WINDOW_WIDTH
               MOVE MAX_DIST TO ZD(X + 1)
               COMPUTE RAY_A  = (PLAYER_A - FOV_HALF) + (FOV * X / WINDOW_WIDTH)
               COMPUTE RAY_DX = FUNCTION COS(RAY_A)
               COMPUTE RAY_DY = FUNCTION SIN(RAY_A)
               MOVE 0 TO WORLD_COL_TOP
               MOVE WINDOW_HEIGHT_MIN_1 TO WORLD_COL_BOT
               MOVE PLAYER_SECTOR TO TRACE_CURR_SECTOR
               MOVE 0.0 TO TRACE_CURR_T

               IF TRACE_CURR_SECTOR <= 0
                   MOVE CEIL_R TO CURR_PIXEL_R
                   MOVE CEIL_G TO CURR_PIXEL_G
                   MOVE CEIL_B TO CURR_PIXEL_B
                   MOVE X TO CURR_PIXEL_X
                   MOVE 0 TO TEX_X
                   MOVE WINDOW_HEIGHT_MIN_1 TO TEX_Y
                   PERFORM DRAW-VLINE-SOLID
               ELSE
                   PERFORM VARYING WORLD_STEP_COUNT FROM 1 BY 1 UNTIL WORLD_STEP_COUNT > MAX_WORLD_STEPS OR WORLD_COL_TOP > WORLD_COL_BOT
                       MOVE PLAYER_X TO TRACE_START_X
                       MOVE PLAYER_Y TO TRACE_START_Y
                       COMPUTE TRACE_END_X = PLAYER_X + (RAY_DX * MAX_DIST)
                       COMPUTE TRACE_END_Y = PLAYER_Y + (RAY_DY * MAX_DIST)

                       PERFORM FIND-NEAREST-SECTOR-HIT

                       IF TRACE_HIT_FOUND = 0
                           MOVE MAX_DIST TO DIST
                           COMPUTE PERP_DIST = DIST * FUNCTION COS(RAY_A - PLAYER_A)
                           IF PERP_DIST < 0.001
                               MOVE 0.001 TO PERP_DIST
                           END-IF
                           COMPUTE WORLD_CEIL_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_CEIL_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))
                           COMPUTE WORLD_FLOOR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_FLOOR_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))

                           IF WORLD_COL_TOP <= WORLD_COL_BOT
                               MOVE S_CEIL_R(TRACE_CURR_SECTOR) TO CURR_PIXEL_R
                               MOVE S_CEIL_G(TRACE_CURR_SECTOR) TO CURR_PIXEL_G
                               MOVE S_CEIL_B(TRACE_CURR_SECTOR) TO CURR_PIXEL_B
                               MOVE X TO CURR_PIXEL_X
                               MOVE WORLD_COL_TOP TO TEX_X
                               COMPUTE TEX_Y = WORLD_CEIL_Y - 1
                               IF TEX_Y > WORLD_COL_BOT
                                   MOVE WORLD_COL_BOT TO TEX_Y
                               END-IF
                               IF TEX_Y >= TEX_X
                                   PERFORM DRAW-VLINE-SOLID
                               END-IF

                               MOVE S_FLOOR_R(TRACE_CURR_SECTOR) TO CURR_PIXEL_R
                               MOVE S_FLOOR_G(TRACE_CURR_SECTOR) TO CURR_PIXEL_G
                               MOVE S_FLOOR_B(TRACE_CURR_SECTOR) TO CURR_PIXEL_B
                               MOVE X TO CURR_PIXEL_X
                               COMPUTE TEX_X = WORLD_FLOOR_Y + 1
                               IF TEX_X < WORLD_COL_TOP
                                   MOVE WORLD_COL_TOP TO TEX_X
                               END-IF
                               MOVE WORLD_COL_BOT TO TEX_Y
                               IF TEX_Y >= TEX_X
                                   PERFORM DRAW-VLINE-SOLID
                               END-IF
                           END-IF
                           EXIT PERFORM
                       END-IF

                       MOVE TRACE_HIT_DIST TO DIST
                       COMPUTE PERP_DIST = DIST * FUNCTION COS(RAY_A - PLAYER_A)
                       IF DIST < 0.001
                           MOVE 0.001 TO DIST
                       END-IF
                       IF PERP_DIST < 0.001
                           MOVE 0.001 TO PERP_DIST
                       END-IF

                       COMPUTE WORLD_HIT_X = PLAYER_X + (RAY_DX * DIST)
                       COMPUTE WORLD_HIT_Y = PLAYER_Y + (RAY_DY * DIST)
                       MOVE TRACE_HIT_U TO WORLD_TEX_U

                       COMPUTE WORLD_CEIL_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_CEIL_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))
                       COMPUTE WORLD_FLOOR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_FLOOR_Z(TRACE_CURR_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))

                       MOVE S_CEIL_R(TRACE_CURR_SECTOR) TO CURR_PIXEL_R
                       MOVE S_CEIL_G(TRACE_CURR_SECTOR) TO CURR_PIXEL_G
                       MOVE S_CEIL_B(TRACE_CURR_SECTOR) TO CURR_PIXEL_B
                       MOVE X TO CURR_PIXEL_X
                       MOVE WORLD_COL_TOP TO TEX_X
                       COMPUTE TEX_Y = WORLD_CEIL_Y - 1
                       IF TEX_Y > WORLD_COL_BOT
                           MOVE WORLD_COL_BOT TO TEX_Y
                       END-IF
                       IF TEX_Y >= TEX_X
                           PERFORM DRAW-VLINE-SOLID
                       END-IF

                       MOVE S_FLOOR_R(TRACE_CURR_SECTOR) TO CURR_PIXEL_R
                       MOVE S_FLOOR_G(TRACE_CURR_SECTOR) TO CURR_PIXEL_G
                       MOVE S_FLOOR_B(TRACE_CURR_SECTOR) TO CURR_PIXEL_B
                       MOVE X TO CURR_PIXEL_X
                       COMPUTE TEX_X = WORLD_FLOOR_Y + 1
                       IF TEX_X < WORLD_COL_TOP
                           MOVE WORLD_COL_TOP TO TEX_X
                       END-IF
                       MOVE WORLD_COL_BOT TO TEX_Y
                       IF TEX_Y >= TEX_X
                           PERFORM DRAW-VLINE-SOLID
                       END-IF

                       PERFORM GET-LINE-TRANSITION

                       MOVE 0 TO WORLD_PORTAL_OPEN
                       IF TRACE_NEXT_SECTOR > 0 AND L_FLAGS(TRACE_HIT_LINEDEF) = 0
                           IF WORLD_OPENING_TOP - WORLD_OPENING_BOT > 0.05
                               MOVE 1 TO WORLD_PORTAL_OPEN
                           END-IF
                       END-IF

                       IF WORLD_PORTAL_OPEN = 0
                           MOVE L_MID_TEX(TRACE_HIT_LINEDEF) TO WORLD_WALL_TEX
                           MOVE WORLD_CEIL_Y TO WORLD_DRAW_TOP
                           MOVE WORLD_FLOOR_Y TO WORLD_DRAW_BOT
                           IF WORLD_DRAW_TOP < WORLD_COL_TOP
                               MOVE WORLD_COL_TOP TO WORLD_DRAW_TOP
                           END-IF
                           IF WORLD_DRAW_BOT > WORLD_COL_BOT
                               MOVE WORLD_COL_BOT TO WORLD_DRAW_BOT
                           END-IF
                           IF WORLD_DRAW_BOT >= WORLD_DRAW_TOP
                               PERFORM DRAW-WORLD-WALL-SLICE
                           END-IF
                           IF PERP_DIST < ZD(X + 1)
                               MOVE PERP_DIST TO ZD(X + 1)
                           END-IF
                           EXIT PERFORM
                       ELSE
                           COMPUTE WORLD_NEXT_CEIL_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_CEIL_Z(TRACE_NEXT_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))
                           COMPUTE WORLD_NEXT_FLOOR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((S_FLOOR_Z(TRACE_NEXT_SECTOR) - WORLD_PLAYER_Z) * PROJ_DIST / PERP_DIST))

                           IF WORLD_NEXT_CEIL_Y > WORLD_CEIL_Y
                               MOVE L_UPPER_TEX(TRACE_HIT_LINEDEF) TO WORLD_WALL_TEX
                               MOVE WORLD_CEIL_Y TO WORLD_DRAW_TOP
                               COMPUTE WORLD_DRAW_BOT = WORLD_NEXT_CEIL_Y - 1
                               IF WORLD_DRAW_TOP < WORLD_COL_TOP
                                   MOVE WORLD_COL_TOP TO WORLD_DRAW_TOP
                               END-IF
                               IF WORLD_DRAW_BOT > WORLD_COL_BOT
                                   MOVE WORLD_COL_BOT TO WORLD_DRAW_BOT
                               END-IF
                               IF WORLD_DRAW_BOT >= WORLD_DRAW_TOP
                                   PERFORM DRAW-WORLD-WALL-SLICE
                               END-IF
                           END-IF

                           IF WORLD_NEXT_FLOOR_Y < WORLD_FLOOR_Y
                               MOVE L_LOWER_TEX(TRACE_HIT_LINEDEF) TO WORLD_WALL_TEX
                               COMPUTE WORLD_DRAW_TOP = WORLD_NEXT_FLOOR_Y + 1
                               MOVE WORLD_FLOOR_Y TO WORLD_DRAW_BOT
                               IF WORLD_DRAW_TOP < WORLD_COL_TOP
                                   MOVE WORLD_COL_TOP TO WORLD_DRAW_TOP
                               END-IF
                               IF WORLD_DRAW_BOT > WORLD_COL_BOT
                                   MOVE WORLD_COL_BOT TO WORLD_DRAW_BOT
                               END-IF
                               IF WORLD_DRAW_BOT >= WORLD_DRAW_TOP
                                   PERFORM DRAW-WORLD-WALL-SLICE
                               END-IF
                           END-IF

                           MOVE WORLD_COL_TOP TO WORLD_NEW_TOP
                           IF WORLD_CEIL_Y > WORLD_NEW_TOP
                               MOVE WORLD_CEIL_Y TO WORLD_NEW_TOP
                           END-IF
                           IF WORLD_NEXT_CEIL_Y > WORLD_NEW_TOP
                               MOVE WORLD_NEXT_CEIL_Y TO WORLD_NEW_TOP
                           END-IF

                           MOVE WORLD_COL_BOT TO WORLD_NEW_BOT
                           IF WORLD_FLOOR_Y < WORLD_NEW_BOT
                               MOVE WORLD_FLOOR_Y TO WORLD_NEW_BOT
                           END-IF
                           IF WORLD_NEXT_FLOOR_Y < WORLD_NEW_BOT
                               MOVE WORLD_NEXT_FLOOR_Y TO WORLD_NEW_BOT
                           END-IF

                           MOVE WORLD_NEW_TOP TO WORLD_COL_TOP
                           MOVE WORLD_NEW_BOT TO WORLD_COL_BOT
                           MOVE TRACE_NEXT_SECTOR TO TRACE_CURR_SECTOR
                           MOVE TRACE_CLOSEST_T TO TRACE_CURR_T
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

      DRAW-WORLD-WALL-SLICE.
           COMPUTE TEX_X = FUNCTION INTEGER(WORLD_TEX_U * TEX_WIDTH)
           IF TEX_X < 0
               MOVE 0 TO TEX_X
           END-IF
           IF TEX_X > TEX_WIDTH - 1
               COMPUTE TEX_X = TEX_WIDTH - 1
           END-IF

           COMPUTE WALL_Y = (WORLD_DRAW_BOT - WORLD_DRAW_TOP) + 1
           IF WALL_Y < 1
               MOVE 1 TO WALL_Y
           END-IF

           PERFORM VARYING Y FROM WORLD_DRAW_TOP BY 1 UNTIL Y > WORLD_DRAW_BOT
               COMPUTE DY_SLICE = Y - WORLD_DRAW_TOP
               COMPUTE TEX_Y = FUNCTION INTEGER((DY_SLICE * TEX_HEIGHT) / WALL_Y)

               IF TEX_Y < 0
                   MOVE 0 TO TEX_Y
               END-IF
               IF TEX_Y > TEX_HEIGHT - 1
                   COMPUTE TEX_Y = TEX_HEIGHT - 1
               END-IF

               COMPUTE TEX_IDX = ((TEX_Y * TEX_WIDTH) + TEX_X) * 3 + 1
               MOVE FUNCTION ORD(W_TEX_DATA(WORLD_WALL_TEX, TEX_IDX)) TO CURR_PIXEL_R
               MOVE FUNCTION ORD(W_TEX_DATA(WORLD_WALL_TEX, TEX_IDX + 1)) TO CURR_PIXEL_G
               MOVE FUNCTION ORD(W_TEX_DATA(WORLD_WALL_TEX, TEX_IDX + 2)) TO CURR_PIXEL_B

               COMPUTE SHADE = 1.0 / (1.0 + (PERP_DIST * 0.15))
               COMPUTE CURR_PIXEL_R = FUNCTION INTEGER(CURR_PIXEL_R * SHADE)
               COMPUTE CURR_PIXEL_G = FUNCTION INTEGER(CURR_PIXEL_G * SHADE)
               COMPUTE CURR_PIXEL_B = FUNCTION INTEGER(CURR_PIXEL_B * SHADE)

               MOVE X TO CURR_PIXEL_X
               MOVE Y TO CURR_PIXEL_Y
               PERFORM PUT-PIXEL
           END-PERFORM.
      
      DRAW-VLINE-SOLID.
           PERFORM VARYING Y FROM TEX_X BY 1 UNTIL Y > TEX_Y
               MOVE Y TO CURR_PIXEL_Y
               PERFORM PUT-PIXEL
           END-PERFORM.

      DRAW-ENEMIES.
           COMPUTE RAY_DX = FUNCTION COS(PLAYER_A)
           COMPUTE RAY_DY = FUNCTION SIN(PLAYER_A)
           COMPUTE WORLD_PLAYER_Z = WORLD_PLAYER_FLOOR_Z + PLAYER_EYE_Z
       
           COMPUTE CAM_PLANE_X = -RAY_DY * CAM_PLANE_SCALE
           COMPUTE CAM_PLANE_Y =  RAY_DX * CAM_PLANE_SCALE
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ENEMIES_NUM
               IF E_IS_ACTIVE(I) = 0
                   CONTINUE
               ELSE
                   COMPUTE DX2 = E_POS_X(I) - PLAYER_X
                   COMPUTE DY2 = E_POS_Y(I) - PLAYER_Y
       
                   COMPUTE INV_DET = 1.0 / (CAM_PLANE_X * RAY_DY - RAY_DX * CAM_PLANE_Y)
       
                   COMPUTE CAM_TRANSF_X = INV_DET * ( RAY_DY * DX2 - RAY_DX * DY2 )
                   COMPUTE CAM_TRANSF_Y = INV_DET * (-CAM_PLANE_Y * DX2 + CAM_PLANE_X * DY2 )
       
                   *> Behind camera or too close = skip
                   IF CAM_TRANSF_Y <= 0.05
                       CONTINUE
                   ELSE
                       COMPUTE SCR_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF * (1.0 + (CAM_TRANSF_X / CAM_TRANSF_Y)))
       
                       *> Sprite size based on depth
                       COMPUTE E_SPRITE_HEIGHT = FUNCTION INTEGER((WINDOW_HEIGHT * 0.8) / CAM_TRANSF_Y)
                       IF E_SPRITE_HEIGHT < 2
                           CONTINUE
                       END-IF
                       MOVE E_SPRITE_HEIGHT TO E_SPRITE_WIDTH
       
                       COMPUTE SPR_X0 = SCR_X - (E_SPRITE_WIDTH / 2)
                       COMPUTE SPR_X1 = SCR_X + (E_SPRITE_WIDTH / 2)

                       IF WORLD_MODE = 1 AND E_SECTOR(I) > 0
                           MOVE S_FLOOR_Z(E_SECTOR(I)) TO WORLD_ENEMY_Z
                           COMPUTE SPR_Y1 = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((WORLD_ENEMY_Z - WORLD_PLAYER_Z) * PROJ_DIST / CAM_TRANSF_Y))
                           COMPUTE SPR_Y0 = SPR_Y1 - E_SPRITE_HEIGHT
                       ELSE
                           COMPUTE SPR_Y0 = WINDOW_HEIGHT_HALF - (E_SPRITE_HEIGHT / 2)
                           COMPUTE SPR_Y1 = WINDOW_HEIGHT_HALF + (E_SPRITE_HEIGHT / 2)
                       END-IF
                
                       *> Clamped bounds for drawing
                       MOVE SPR_X0 TO CLIP_X0
                       MOVE SPR_X1 TO CLIP_X1
                       MOVE SPR_Y0 TO CLIP_Y0
                       MOVE SPR_Y1 TO CLIP_Y1
                
                       IF CLIP_X0 < 0
                           MOVE 0 TO CLIP_X0
                       END-IF
                       IF CLIP_Y0 < 0
                           MOVE 0 TO CLIP_Y0
                       END-IF
                       IF CLIP_X1 > WINDOW_WIDTH - 1
                           COMPUTE CLIP_X1 = WINDOW_WIDTH - 1
                       END-IF
                       IF CLIP_Y1 > WINDOW_HEIGHT - 1
                           COMPUTE CLIP_Y1 = WINDOW_HEIGHT - 1
                       END-IF

                       IF WORLD_MODE = 1 AND E_SECTOR(I) > 0
                           MOVE E_POS_X(I) TO SPR_VIS_TARGET_X
                           MOVE E_POS_Y(I) TO SPR_VIS_TARGET_Y
                           MOVE E_SECTOR(I) TO SPR_VIS_TARGET_SECTOR
                           PERFORM GET-SPRITE-VISIBILITY
                           IF SPR_VIS_BLOCKED = 1
                               MOVE 1 TO CLIP_Y0
                               MOVE 0 TO CLIP_Y1
                           ELSE
                               IF CLIP_Y0 < SPR_VIS_TOP
                                   MOVE SPR_VIS_TOP TO CLIP_Y0
                               END-IF
                               IF CLIP_Y1 > SPR_VIS_BOT
                                   MOVE SPR_VIS_BOT TO CLIP_Y1
                               END-IF
                           END-IF
                       END-IF

                       IF CLIP_X0 > CLIP_X1 OR CLIP_Y0 > CLIP_Y1
                           MOVE 0 TO CLIP_X0
                           MOVE -1 TO CLIP_X1
                       END-IF
                
                       COMPUTE VIS_WIDTH = (SPR_X1 - SPR_X0) + 1
                       COMPUTE VIS_HEIGHT = (SPR_Y1 - SPR_Y0) + 1
                       IF VIS_WIDTH < 1 MOVE 1 TO VIS_WIDTH END-IF
                       IF VIS_HEIGHT < 1 MOVE 1 TO VIS_HEIGHT END-IF

                       COMPUTE E_HP_FRAC = E_HP(I) / E_MAX_HP(I)
       
                       IF E_HP_FRAC < 0.0
                           MOVE 0.0 TO E_HP_FRAC
                       END-IF
                       IF E_HP_FRAC > 1.0
                           MOVE 1.0 TO E_HP_FRAC
                       END-IF
       
                       IF E_STATE(I) = E_STATE_DEAD
                           MOVE 0.25 TO E_SPRITE_BRIGHT
                       ELSE
                           COMPUTE E_SPRITE_BRIGHT = 0.1 + (0.9 * E_HP_FRAC)
                       END-IF
       
                       PERFORM VARYING X FROM CLIP_X0 BY 1 UNTIL X > CLIP_X1
                           IF CAM_TRANSF_Y < ZD(X + 1)
                              COMPUTE TEX_X = FUNCTION INTEGER(((X - SPR_X0) * TEX_WIDTH) / VIS_WIDTH)
           
                              IF TEX_X < 0
                                  MOVE 0 TO TEX_X
                              END-IF
                              IF TEX_X > TEX_WIDTH - 1
                                  COMPUTE TEX_X = TEX_WIDTH - 1
                              END-IF
           
                              PERFORM VARYING Y FROM CLIP_Y0 BY 1 UNTIL Y > CLIP_Y1
                                  COMPUTE TEX_Y = FUNCTION INTEGER(((Y - SPR_Y0) * TEX_HEIGHT) / VIS_HEIGHT)
           
                                  IF TEX_Y < 0
                                      MOVE 0 TO TEX_Y
                                  END-IF
                                  IF TEX_Y > TEX_HEIGHT - 1
                                      COMPUTE TEX_Y = TEX_HEIGHT - 1
                                  END-IF
           
                                  COMPUTE TEX_IDX = ((TEX_Y * TEX_WIDTH) + TEX_X) * 3 + 1
           
                                  IF TEX_IDX < 1 OR TEX_IDX + 2 > TEX_DATA
                                      EXIT PERFORM
                                  END-IF
           
                                  MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_ENEMY, TEX_IDX))       TO SPR_R
                                  MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_ENEMY, TEX_IDX + 1))   TO SPR_G
                                  MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_ENEMY, TEX_IDX + 2))   TO SPR_B
                                   
                                   COMPUTE SPR_R = FUNCTION INTEGER(SPR_R * E_SPRITE_BRIGHT)
                                   COMPUTE SPR_G = FUNCTION INTEGER(SPR_G * E_SPRITE_BRIGHT)
                                   COMPUTE SPR_B = FUNCTION INTEGER(SPR_B * E_SPRITE_BRIGHT)
                                   
                                   COMPUTE STR = FUNCTION INTEGER(SPR_TRANSPARENT_R * E_SPRITE_BRIGHT)
                                  COMPUTE STG = FUNCTION INTEGER(SPR_TRANSPARENT_G * E_SPRITE_BRIGHT)
                                  COMPUTE STB = FUNCTION INTEGER(SPR_TRANSPARENT_B * E_SPRITE_BRIGHT)

                                  IF NOT (SPR_R = STR AND SPR_G = STG AND SPR_B = STB)
                                      COMPUTE SPR_DEPTH_IDX = (Y * WINDOW_WIDTH) + X + 1
                                      IF CAM_TRANSF_Y < SPR_ZD(SPR_DEPTH_IDX)
                                          MOVE X TO CURR_PIXEL_X
                                          MOVE Y TO CURR_PIXEL_Y
                                          MOVE SPR_R TO CURR_PIXEL_R
                                          MOVE SPR_G TO CURR_PIXEL_G
                                          MOVE SPR_B TO CURR_PIXEL_B
                                          PERFORM PUT-PIXEL
                                          MOVE CAM_TRANSF_Y TO SPR_ZD(SPR_DEPTH_IDX)
                                      END-IF
                                  END-IF
                              END-PERFORM
                          END-IF
                       END-PERFORM
                       END-IF
               END-IF
           END-PERFORM.

      DRAW-PICKUPS.
           COMPUTE RAY_DX = FUNCTION COS(PLAYER_A)
           COMPUTE RAY_DY = FUNCTION SIN(PLAYER_A)
           COMPUTE WORLD_PLAYER_Z = WORLD_PLAYER_FLOOR_Z + PLAYER_EYE_Z
           COMPUTE CAM_PLANE_X = -RAY_DY * CAM_PLANE_SCALE
           COMPUTE CAM_PLANE_Y =  RAY_DX * CAM_PLANE_SCALE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THINGS_NUM
               IF T_IS_ACTIVE(I) = 1
                   COMPUTE DX2 = T_POS_X(I) - PLAYER_X
                   COMPUTE DY2 = T_POS_Y(I) - PLAYER_Y

                   COMPUTE INV_DET = 1.0 / (CAM_PLANE_X * RAY_DY - RAY_DX * CAM_PLANE_Y)
                   COMPUTE CAM_TRANSF_X = INV_DET * ( RAY_DY * DX2 - RAY_DX * DY2 )
                   COMPUTE CAM_TRANSF_Y = INV_DET * (-CAM_PLANE_Y * DX2 + CAM_PLANE_X * DY2 )

                   IF CAM_TRANSF_Y > 0.05
                       COMPUTE SCR_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF * (1.0 + (CAM_TRANSF_X / CAM_TRANSF_Y)))
                       COMPUTE E_SPRITE_HEIGHT = FUNCTION INTEGER((WINDOW_HEIGHT * 0.30) / CAM_TRANSF_Y)
                       IF E_SPRITE_HEIGHT > 1
                           MOVE E_SPRITE_HEIGHT TO E_SPRITE_WIDTH
                           COMPUTE SPR_X0 = SCR_X - (E_SPRITE_WIDTH / 2)
                           COMPUTE SPR_X1 = SCR_X + (E_SPRITE_WIDTH / 2)

                           IF WORLD_MODE = 1 AND T_SECTOR(I) > 0
                               MOVE S_FLOOR_Z(T_SECTOR(I)) TO WORLD_PICKUP_Z
                               COMPUTE SPR_Y1 = FUNCTION INTEGER(WINDOW_HEIGHT_CENTER - ((WORLD_PICKUP_Z - WORLD_PLAYER_Z) * PROJ_DIST / CAM_TRANSF_Y))
                               COMPUTE SPR_Y0 = SPR_Y1 - E_SPRITE_HEIGHT
                           ELSE
                               COMPUTE SPR_Y0 = WINDOW_HEIGHT_HALF - (E_SPRITE_HEIGHT / 2)
                               COMPUTE SPR_Y1 = WINDOW_HEIGHT_HALF + (E_SPRITE_HEIGHT / 2)
                           END-IF

                           MOVE SPR_X0 TO CLIP_X0
                           MOVE SPR_X1 TO CLIP_X1
                           MOVE SPR_Y0 TO CLIP_Y0
                           MOVE SPR_Y1 TO CLIP_Y1

                           IF CLIP_X0 < 0 MOVE 0 TO CLIP_X0 END-IF
                           IF CLIP_Y0 < 0 MOVE 0 TO CLIP_Y0 END-IF
                           IF CLIP_X1 > WINDOW_WIDTH - 1 COMPUTE CLIP_X1 = WINDOW_WIDTH - 1 END-IF
                           IF CLIP_Y1 > WINDOW_HEIGHT - 1 COMPUTE CLIP_Y1 = WINDOW_HEIGHT - 1 END-IF

                           IF WORLD_MODE = 1 AND T_SECTOR(I) > 0
                               MOVE T_POS_X(I) TO SPR_VIS_TARGET_X
                               MOVE T_POS_Y(I) TO SPR_VIS_TARGET_Y
                               MOVE T_SECTOR(I) TO SPR_VIS_TARGET_SECTOR
                               PERFORM GET-SPRITE-VISIBILITY
                               IF SPR_VIS_BLOCKED = 1
                                   MOVE 1 TO CLIP_Y0
                                   MOVE 0 TO CLIP_Y1
                               ELSE
                                   IF CLIP_Y0 < SPR_VIS_TOP
                                       MOVE SPR_VIS_TOP TO CLIP_Y0
                                   END-IF
                                   IF CLIP_Y1 > SPR_VIS_BOT
                                       MOVE SPR_VIS_BOT TO CLIP_Y1
                                   END-IF
                               END-IF
                           END-IF

                           IF CLIP_X0 > CLIP_X1 OR CLIP_Y0 > CLIP_Y1
                               MOVE 0 TO CLIP_X0
                               MOVE -1 TO CLIP_X1
                           END-IF

                           COMPUTE VIS_WIDTH = (SPR_X1 - SPR_X0) + 1
                           COMPUTE VIS_HEIGHT = (SPR_Y1 - SPR_Y0) + 1
                           IF VIS_WIDTH < 1 MOVE 1 TO VIS_WIDTH END-IF
                           IF VIS_HEIGHT < 1 MOVE 1 TO VIS_HEIGHT END-IF

                           COMPUTE E_SPRITE_BRIGHT = 1.0 / (1.0 + (CAM_TRANSF_Y * 0.18))
                           IF E_SPRITE_BRIGHT < 0.35
                               MOVE 0.35 TO E_SPRITE_BRIGHT
                           END-IF

                           PERFORM VARYING X FROM CLIP_X0 BY 1 UNTIL X > CLIP_X1
                               IF CAM_TRANSF_Y < ZD(X + 1)
                                   COMPUTE TEX_X = FUNCTION INTEGER(((X - SPR_X0) * TEX_WIDTH) / VIS_WIDTH)

                                   IF TEX_X < 0
                                       MOVE 0 TO TEX_X
                                   END-IF
                                   IF TEX_X > TEX_WIDTH - 1
                                       COMPUTE TEX_X = TEX_WIDTH - 1
                                   END-IF

                                   PERFORM VARYING Y FROM CLIP_Y0 BY 1 UNTIL Y > CLIP_Y1
                                       COMPUTE TEX_Y = FUNCTION INTEGER(((Y - SPR_Y0) * TEX_HEIGHT) / VIS_HEIGHT)

                                       IF TEX_Y < 0
                                           MOVE 0 TO TEX_Y
                                       END-IF
                                       IF TEX_Y > TEX_HEIGHT - 1
                                           COMPUTE TEX_Y = TEX_HEIGHT - 1
                                       END-IF

                                       COMPUTE TEX_IDX = ((TEX_Y * TEX_WIDTH) + TEX_X) * 3 + 1

                                       IF TEX_IDX < 1 OR TEX_IDX + 2 > TEX_DATA
                                           EXIT PERFORM
                                       END-IF

                                       MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_MEDKIT, TEX_IDX)) TO SPR_R
                                       MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_MEDKIT, TEX_IDX + 1)) TO SPR_G
                                       MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_MEDKIT, TEX_IDX + 2)) TO SPR_B

                                       IF NOT (SPR_R = SPR_TRANSPARENT_R AND SPR_G = SPR_TRANSPARENT_G AND SPR_B = SPR_TRANSPARENT_B)
                                           COMPUTE SPR_DEPTH_IDX = (Y * WINDOW_WIDTH) + X + 1
                                           IF CAM_TRANSF_Y < SPR_ZD(SPR_DEPTH_IDX)
                                               COMPUTE CURR_PIXEL_R = FUNCTION INTEGER(SPR_R * E_SPRITE_BRIGHT)
                                               COMPUTE CURR_PIXEL_G = FUNCTION INTEGER(SPR_G * E_SPRITE_BRIGHT)
                                               COMPUTE CURR_PIXEL_B = FUNCTION INTEGER(SPR_B * E_SPRITE_BRIGHT)
                                               MOVE X TO CURR_PIXEL_X
                                               MOVE Y TO CURR_PIXEL_Y
                                               PERFORM PUT-PIXEL
                                               MOVE CAM_TRANSF_Y TO SPR_ZD(SPR_DEPTH_IDX)
                                           END-IF
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      
      DRAW-CROSSHAIR.
           COMPUTE XHAIR_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF)
           MOVE XHAIR_X TO CURR_PIXEL_X
       
           COMPUTE XHAIR_Y = FUNCTION INTEGER(WINDOW_HEIGHT_HALF)
           MOVE XHAIR_Y TO CURR_PIXEL_Y
           MOVE 255 TO CURR_PIXEL_R
           MOVE 255 TO CURR_PIXEL_G
           MOVE 255 TO CURR_PIXEL_B
           PERFORM PUT-PIXEL
       
           COMPUTE XHAIR_RET_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF - 2)
           MOVE XHAIR_RET_X TO CURR_PIXEL_X
           
           COMPUTE XHAIR_RET_Y = FUNCTION INTEGER(WINDOW_HEIGHT_HALF)
           MOVE XHAIR_RET_Y TO CURR_PIXEL_Y
           PERFORM PUT-PIXEL
       
           COMPUTE XHAIR_RET_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF + 2)
           MOVE XHAIR_RET_X TO CURR_PIXEL_X
       
           COMPUTE XHAIR_RET_Y = FUNCTION INTEGER(WINDOW_HEIGHT_HALF)
           MOVE XHAIR_RET_Y TO CURR_PIXEL_Y
           PERFORM PUT-PIXEL
       
           COMPUTE XHAIR_RET_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF)
           MOVE XHAIR_RET_X     TO CURR_PIXEL_X
       
           COMPUTE XHAIR_RET_Y = FUNCTION INTEGER(WINDOW_HEIGHT_HALF - 2)
           MOVE XHAIR_RET_Y TO CURR_PIXEL_Y
           PERFORM PUT-PIXEL
       
           COMPUTE XHAIR_RET_X = FUNCTION INTEGER(WINDOW_WIDTH_HALF)
           MOVE XHAIR_RET_X     TO CURR_PIXEL_X
       
           COMPUTE XHAIR_RET_Y = FUNCTION INTEGER(WINDOW_HEIGHT_HALF + 2)
           MOVE XHAIR_RET_Y TO CURR_PIXEL_Y
           PERFORM PUT-PIXEL.
      
      DRAW-GUN.
           IF FLASH_TIMER > 0
               COMPUTE FLASH_X0 = WINDOW_WIDTH_HALF - 1
               COMPUTE FLASH_X1 = WINDOW_WIDTH_HALF + 3
               COMPUTE FLASH_Y0 = WINDOW_HEIGHT - 32
               COMPUTE FLASH_Y1 = WINDOW_HEIGHT - 28
       
               PERFORM VARYING Y FROM FLASH_Y0 BY 1 UNTIL Y > FLASH_Y1
                   PERFORM VARYING X FROM FLASH_X0 BY 1 UNTIL X > FLASH_X1
                       IF NOT ((X = FLASH_X0 AND Y = FLASH_Y0) OR 
                               (X = FLASH_X1 AND Y = FLASH_Y0) OR 
                               (X = FLASH_X0 AND Y = FLASH_Y1) OR 
                               (X = FLASH_X1 AND Y = FLASH_Y1))
                           MOVE X TO CURR_PIXEL_X
                           MOVE Y TO CURR_PIXEL_Y
                           MOVE 255 TO CURR_PIXEL_R
                           MOVE 220 TO CURR_PIXEL_G
                           MOVE 100 TO CURR_PIXEL_B
                           PERFORM PUT-PIXEL
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      
           COMPUTE GUN_WIDTH = FUNCTION INTEGER(WINDOW_WIDTH * 0.3)
           COMPUTE GUN_HEIGHT = FUNCTION INTEGER(GUN_WIDTH * 1.0)

           IF GUN_WIDTH < 1 OR GUN_HEIGHT < 1
               EXIT PARAGRAPH
           END-IF

           COMPUTE GUN_X0 = (WINDOW_WIDTH - GUN_WIDTH) / 2
           COMPUTE GUN_Y0 = (WINDOW_HEIGHT - GUN_HEIGHT)
           IF GUN_Y0 < 0
               MOVE 0 TO GUN_Y0
           END-IF

           PERFORM VARYING Y FROM 0 BY 1 UNTIL Y >= GUN_HEIGHT
               COMPUTE TEX_Y = FUNCTION INTEGER((Y * TEX_HEIGHT) / GUN_HEIGHT)
               IF TEX_Y < 0 MOVE 0 TO TEX_Y END-IF
               IF TEX_Y > TEX_HEIGHT - 1 COMPUTE TEX_Y = TEX_HEIGHT - 1 END-IF

               PERFORM VARYING X FROM 0 BY 1 UNTIL X >= GUN_WIDTH
                   COMPUTE TEX_X = FUNCTION INTEGER((X * TEX_WIDTH) / GUN_WIDTH)
                   IF TEX_X < 0 MOVE 0 TO TEX_X END-IF
                   IF TEX_X > TEX_WIDTH - 1 COMPUTE TEX_X = TEX_WIDTH - 1 END-IF

                   COMPUTE TEX_IDX = ((TEX_Y * TEX_WIDTH) + TEX_X) * 3 + 1

                   MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_GUN, TEX_IDX))     TO SPR_R
                   MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_GUN, TEX_IDX + 1)) TO SPR_G
                   MOVE FUNCTION ORD(SPR_TEX_DATA(SPR_GUN, TEX_IDX + 2)) TO SPR_B

                   IF NOT (SPR_R = SPR_TRANSPARENT_R AND SPR_G = SPR_TRANSPARENT_G AND SPR_B = SPR_TRANSPARENT_B)
                       COMPUTE CURR_PIXEL_X = GUN_X0 + X
                       COMPUTE CURR_PIXEL_Y = GUN_Y0 + Y

                       IF CURR_PIXEL_X >= 0 AND CURR_PIXEL_X < WINDOW_WIDTH AND CURR_PIXEL_Y >= 0 AND CURR_PIXEL_Y < WINDOW_HEIGHT
                           MOVE SPR_R TO CURR_PIXEL_R
                           MOVE SPR_G TO CURR_PIXEL_G
                           MOVE SPR_B TO CURR_PIXEL_B
                           PERFORM PUT-PIXEL
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.
      
      WRITE-FRAME.
           MOVE WINDOW_WIDTH TO DISP_WIDTH
           MOVE WINDOW_HEIGHT TO DISP_HEIGHT
           MOVE SPACES TO HDR
           STRING
               "P6" X"0A"
               DISP_WIDTH
               " "
               DISP_HEIGHT
               X"0A"
               "255" X"0A"
           INTO HDR
           END-STRING
        
           CALL "write" USING
               BY VALUE STDOUT_FD
               BY REFERENCE HDR
               BY VALUE HDR_LEN
           RETURNING WRET
           END-CALL
       
           IF WRET <= 0
               PERFORM TERM-SANE
               STOP RUN
           END-IF
       
           CALL "write" USING
               BY VALUE STDOUT_FD
               BY REFERENCE FRAMEBUF
               BY VALUE WINDOW_PIXELS_COUNT
           RETURNING WRET
           END-CALL
       
           IF WRET <= 0
               PERFORM TERM-SANE
               STOP RUN
           END-IF.
      
      PUT-PIXEL.
           IF CURR_PIXEL_X >= WINDOW_WIDTH OR CURR_PIXEL_Y >= WINDOW_HEIGHT
               EXIT PARAGRAPH
           END-IF

           COMPUTE PIXEL_IDX = (CURR_PIXEL_Y * WINDOW_WIDTH) + CURR_PIXEL_X
           COMPUTE PIXEL_TEX_OFFSETET = (PIXEL_IDX * 3) + 1

           IF PIXEL_TEX_OFFSETET < 1 OR PIXEL_TEX_OFFSETET + 2 > WINDOW_PIXELS_COUNT
               EXIT PARAGRAPH
           END-IF
       
           MOVE FUNCTION CHAR(CURR_PIXEL_R) TO FB-BYTE(PIXEL_TEX_OFFSETET)
           MOVE FUNCTION CHAR(CURR_PIXEL_G) TO FB-BYTE(PIXEL_TEX_OFFSETET + 1)
           MOVE FUNCTION CHAR(CURR_PIXEL_B) TO FB-BYTE(PIXEL_TEX_OFFSETET + 2).
      
