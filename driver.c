#include <stdio.h>

#define fixnum_mask      1
#define fixnum_shift     1
#define fixnum_tag       1

#define boolean_mask     0xf
#define boolean_shift    4
#define boolean_tag      0x6

#define char_mask        0xff
#define char_shift       8
#define char_tag         10

#define emptylist_tag    0xe

extern int scheme_entry (void);

int main (int argc, char **argv)
{
  int val = scheme_entry ();
  if ((val & fixnum_mask) == fixnum_tag) {
    printf ("%d\n", (val >> fixnum_shift));
  } else if ((val & boolean_mask) == boolean_tag) {
    printf ("%s\n", (val >> boolean_shift) ? "#t" : "#f");
  } else if ((val & char_mask) == char_tag) {
    printf ("#\\%c\n", (char) (val >> char_shift));
  } else if (val == emptylist_tag) {
    printf ("()\n");
  } else {
    printf ("uknown type\n");
  }
  return 0;
}
