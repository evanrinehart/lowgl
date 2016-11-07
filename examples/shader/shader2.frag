#version 150
uniform float time;
in vec2 Location;
out vec4 outColor;
void main()
{
  float x = gl_FragCoord.x / 640;
  float y = gl_FragCoord.y / 480;
  outColor = vec4(
    fract(x*25) < 0.5 ? 1.0 : 0.0,
    fract(y*25) < 0.5 ? 1.0 : 0.0,
    0.0, 1.0
  );
}
