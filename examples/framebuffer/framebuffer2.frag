#version 150
uniform float time;
uniform sampler2D tex;
in vec2 Texcoord;
out vec4 outColor;

void main()
{
  float d = pow(10,(abs(cos(time))+1.5));
  outColor = texture(tex, floor(Texcoord*d)/d);
}
