{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveAnyClass #-}

module Emulator.CrtShader (getCrtShaderProgram) where


import           Control.Monad
import           Control.Exception
import           Data.ByteString hiding (putStrLn)
import           Data.StateVar
import           Graphics.Rendering.OpenGL.GL.Shaders
import           System.Console.ANSI
import           System.Mem
import           Text.RawString.QQ


crtVertexShader = [r|

varying vec2 v_texCoord;

void main()
{
    gl_Position = gl_Vertex;
    v_texCoord = vec2(gl_MultiTexCoord0);
}

|]


crtFragmentShader = [r|

//
// PUBLIC DOMAIN CRT STYLED SCAN-LINE SHADER
//
//   by Timothy Lottes
//
// This is more along the style of a really good CGA arcade monitor.
// With RGB inputs instead of NTSC.
// The shadow mask example has the mask rotated 90 degrees for less chromatic aberration.
//
// Left it unoptimized to show the theory behind the algorithm.
//
// It is an example what I personally would want as a display option for pixel art games.
// Please take and use, change, or whatever.
//

varying vec2 v_texCoord;

uniform sampler2D texImg;

// Hardness of scanline.
//  -8.0 = soft
// -16.0 = medium
float hardScan=-8.0;

// Hardness of pixels in scanline.
// -2.0 = soft
// -4.0 = hard
float hardPix=-2.0;

// Display warp.
// 0.0 = none
// 1.0/8.0 = extreme
vec2 warp=vec2(1.0/32.0,1.0/24.0); 

// Amount of shadow mask.
float maskDark=1.0;
float maskLight=1.5;

vec2 res = vec2(800.0,600.0); // /3.0

//------------------------------------------------------------------------

// sRGB to Linear.
// Assuing using sRGB typed textures this should not be needed.
float ToLinear1(float c){return(c<=0.04045)?c/12.92:pow((c+0.055)/1.055,2.4);}
vec3 ToLinear(vec3 c){return vec3(ToLinear1(c.r),ToLinear1(c.g),ToLinear1(c.b));}

// Linear to sRGB.
// Assuing using sRGB typed textures this should not be needed.
float ToSrgb1(float c){return(c<0.0031308?c*12.92:1.055*pow(c,0.41666)-0.055);}
vec3 ToSrgb(vec3 c){return vec3(ToSrgb1(c.r),ToSrgb1(c.g),ToSrgb1(c.b));}

// Nearest emulated sample given floating point position and texel offset.
// Also zero's off screen.
vec3 Fetch(vec2 pos,vec2 off){
  pos=floor(pos*res+off)/res;
  if(max(abs(pos.x-0.5),abs(pos.y-0.5))>0.5)return vec3(0.0,0.0,0.0);
  return ToLinear(texture2D(texImg,pos.xy,-16.0).rgb);}

// Distance in emulated pixels to nearest texel.
vec2 Dist(vec2 pos){pos=pos*res;return -((pos-floor(pos))-vec2(0.5));}
    
// 1D Gaussian.
float Gaus(float pos,float scale){return exp2(scale*pos*pos);}

// 3-tap Gaussian filter along horz line.
vec3 Horz3(vec2 pos,float off){
  vec3 b=Fetch(pos,vec2(-1.0,off));
  vec3 c=Fetch(pos,vec2( 0.0,off));
  vec3 d=Fetch(pos,vec2( 1.0,off));
  float dst=Dist(pos).x;
  // Convert distance to weight.
  float scale=hardPix;
  float wb=Gaus(dst-1.0,scale);
  float wc=Gaus(dst+0.0,scale);
  float wd=Gaus(dst+1.0,scale);
  // Return filtered sample.
  return (b*wb+c*wc+d*wd)/(wb+wc+wd);}

// 5-tap Gaussian filter along horz line.
vec3 Horz5(vec2 pos,float off){
  vec3 a=Fetch(pos,vec2(-2.0,off));
  vec3 b=Fetch(pos,vec2(-1.0,off));
  vec3 c=Fetch(pos,vec2( 0.0,off));
  vec3 d=Fetch(pos,vec2( 1.0,off));
  vec3 e=Fetch(pos,vec2( 2.0,off));
  float dst=Dist(pos).x;
  // Convert distance to weight.
  float scale=hardPix;
  float wa=Gaus(dst-2.0,scale);
  float wb=Gaus(dst-1.0,scale);
  float wc=Gaus(dst+0.0,scale);
  float wd=Gaus(dst+1.0,scale);
  float we=Gaus(dst+2.0,scale);
  // Return filtered sample.
  return (a*wa+b*wb+c*wc+d*wd+e*we)/(wa+wb+wc+wd+we);}

// Return scanline weight.
float Scan(vec2 pos,float off){
  float dst=Dist(pos).y;
  return Gaus(dst+off,hardScan);}

// Allow nearest three lines to effect pixel.
vec3 Tri(vec2 pos){
  vec3 a=Horz3(pos,-1.0);
  vec3 b=Horz5(pos, 0.0);
  vec3 c=Horz3(pos, 1.0);
  float wa=Scan(pos,-1.0);
  float wb=Scan(pos, 0.0);
  float wc=Scan(pos, 1.0);
  return a*wa+b*wb+c*wc;}

// Distortion of scanlines, and end of screen alpha.
vec2 Warp(vec2 pos){
  pos=pos*2.0-1.0;    
  pos*=vec2(1.0+(pos.y*pos.y)*warp.x,1.0+(pos.x*pos.x)*warp.y);
  return pos*0.5+0.5;}

// Shadow mask.
vec3 Mask(vec2 pos){
  pos.x+=pos.y*3.0;
  vec3 mask=vec3(maskDark,maskDark,maskDark);
  pos.x=fract(pos.x/6.0);
  if(pos.x<0.333)mask.r=maskLight;
  else if(pos.x<0.666)mask.g=maskLight;
  else mask.b=maskLight;
  return mask;}    

// Draw dividing bars.
float Bar(float pos,float bar){pos-=bar;return pos*pos<4.0?0.0:1.0;}

// Entry.
void main(){
  // Unmodified.
  vec2 pos=Warp(v_texCoord);
  vec4 fragColor;
  fragColor.rgb=Tri(pos)*Mask(gl_FragCoord.xy);
  fragColor.rgb=ToSrgb(fragColor.rgb);
  gl_FragColor= vec4(fragColor.rgb, 1.0);
}

|]

data ShaderException 
  = ShaderValidationError
  | ShaderLinkingError
  | ShaderCompileError
  deriving Exception

instance Show ShaderException where
  show = let common = "\n Please make sure the video driver is up to date and OpenGL 4.3 is supported." in 
    \case
      ShaderCompileError    -> "Failed to compile the GLSL vertex or fragment shader for the CRT effect." ++ common
      ShaderLinkingError    -> "Failed to link the OpenGL shader program." ++ common
      ShaderValidationError -> "Failed to validate the OpenGL shader program." ++ common

newShader :: ShaderType -> ByteString -> IO Shader
newShader shaderType source = do
    shader <- createShader shaderType
    shaderSourceBS shader $= source
    compileShader shader
    success <- compileStatus shader
    when (not success) $ do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Failed to compile " ++ show shaderType ++ ":"
      setSGR [SetColor Foreground Vivid Yellow]
      shaderInfoLog shader >>= putStrLn
      setSGR [Reset]
      throw ShaderCompileError
    return shader


createProgramFrom :: [Shader] -> IO Program
createProgramFrom shaders = do
    program <- createProgram
    attachedShaders program $= shaders 
    linkProgram program
    linkSuccess <- linkStatus program
    when (not linkSuccess) $ throw ShaderLinkingError
    validateProgram program
    validateSuccess <- validateStatus program
    when (not validateSuccess) $ throw ShaderLinkingError
    performMajorGC
    return program


getCrtShaderProgram :: IO Program
getCrtShaderProgram = 
  createProgramFrom 
  =<< 
  sequence 
  [
    newShader VertexShader crtVertexShader, 
    newShader FragmentShader crtFragmentShader
  ]
