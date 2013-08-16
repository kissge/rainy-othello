#include <stdio.h>

int main ()
{
    char c;
    int i, beginflag = 1;
    while ((c = getchar()) != EOF)
    {
        if (beginflag)
        {
            beginflag = 0;
            printf("\"");
        }
        if ('A' <= c && c <= 'H')
            i = (int) (c - 'A');
        else if ('a' <= c && c <= 'h')
            i = (int) (c - 'a');
        else if ('1' <= c && c <= '8')
        {
            i = i * 8 + (int) (c - '1');
            if (i == 10 || i == 11 || i == 12 || i == 13 || i == 18)
                printf("\\%03d", i);
            else if (i == '"') printf("\\\"");
            else if (i == '\\') printf("\\\\");
            else printf("%c", i);
        }
        else if (c == 10 || c == 13)
        {
            printf("\";");
            beginflag = 1;
        }
    }
    return 0;
}
