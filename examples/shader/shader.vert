#version 150
uniform mat4 move;
in vec2 position;
in vec2 location;
out vec2 Location;
void main()
{
    gl_Position = move * vec4(position, 0.0, 1.0);
    Location = location;
}
