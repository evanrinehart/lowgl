#version 150

uniform vec3 color;
uniform float alpha;
uniform mat4 move;

in vec2 position;
out vec3 Color;
out float Alpha;

void main()
{
    gl_Position = move * vec4(position, 0.0, 1.0);
    Color = color;
    Alpha = alpha;
}

