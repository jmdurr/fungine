#version 310 es
in mediump vec4 vColor;
in mediump vec2 TexCoord;
out mediump vec4 FragColor;

uniform sampler2D ourTexture;

void main()
{
    FragColor = texture(ourTexture, TexCoord);
}