#version 150
uniform float time;
in vec2 Location;
out vec4 outColor;
void main()
{
  float t = time;
  outColor = vec4(
    fract(Location.x*5) < 0.5 ? sin(t*3.145) : cos(t*4.567),
    fract(Location.y*5) < 0.5 ? cos(t*6.534) : sin(t*4.321),
    0.0, 1.0
  );
}

