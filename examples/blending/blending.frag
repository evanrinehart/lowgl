#version 150

in vec3 Color;
in float Alpha;
out vec4 outColor;

void main()
{
    outColor = vec4(Color, Alpha);
}