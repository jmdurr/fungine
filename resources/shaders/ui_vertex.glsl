#version 310 es
layout (location = 0) in mediump vec3 aPos;
layout (location = 1) in mediump vec4 aColor;
layout (location = 2) in mediump vec2 aUvCoord;
layout (location = 3) in mediump vec3 aNorm;
uniform mediump mat4 transform;
out mediump vec4 vColor;
out mediump vec2 TexCoord;

void main()
{
    gl_Position = transform * vec4(aPos,1.0); //transform * vec4(aPos, 1.0);
    vColor = vec4(1.0,0.5,0,0.75);
    TexCoord = aUvCoord;
}