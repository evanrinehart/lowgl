#version 150
uniform float time;
in vec2 Texcoord;
out vec4 outColor;
void main()
{
  float t = time;
  outColor = vec4(
    fract(Texcoord.x*5) < 0.5 ? sin(t*0.145) : cos(t*0.567),
    fract(Texcoord.y*5) < 0.5 ? cos(t*0.534) : sin(t*0.321),
    0.0, 1.0
  );
}