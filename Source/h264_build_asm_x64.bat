yasm -f win64 -o pixel_x64.obj  pixel_x64.asm
yasm -f win64 -o motion_comp_x64.obj  motion_comp_x64.asm
yasm -f win64 -o frame_x64.obj  frame_x64.asm
yasm -f win64 -o intra_pred_x64.obj  intra_pred_x64.asm

pause
