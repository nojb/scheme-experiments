#include <stdint.h>
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

#define block_type_mask  0x0f
#define cons_tag         0x03

#define block_mask       3
#define block_tag        0
#define block_shift      0

extern long scheme_entry (void);

void print (unsigned long val)
{
  if ((val & fixnum_mask) == fixnum_tag) {
    printf ("%ld", (val >> fixnum_shift));
  } else if ((val & boolean_mask) == boolean_tag) {
    printf ("%s", (val >> boolean_shift) ? "#t" : "#f");
  } else if ((val & char_mask) == char_tag) {
    printf ("#\\%c", (char) (val >> char_shift));
  } else if (val == emptylist_tag) {
    printf ("()");
  } else if ((val & block_mask) == block_tag) {
    unsigned long *p = (unsigned long *)val;
    switch (p[-1] & block_type_mask) {
      case cons_tag:
        printf ("(");
        print (p[0]);
        printf (" . ");
        print (p[1]);
        printf (")");
        break;
      default:
        printf ("unknown block tag");
        break;
    }
  } else {
    printf ("uknown type");
  }
}

int main (int argc, char **argv)
{
  unsigned long val = scheme_entry ();

  print (val);
  printf ("\n");

  return 0;
}
