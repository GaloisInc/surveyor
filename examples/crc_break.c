#include <crucible.h>

#include <stdio.h>                  // Needed for printf()
#include <stdlib.h>                 // Needed for rand()

//----- Type defines ----------------------------------------------------------
typedef unsigned char      byte;    // Byte is a char
typedef unsigned short int word16;  // 16-bit word is a short int
typedef unsigned int       word32;  // 32-bit word is an int

//----- Defines ---------------------------------------------------------------
#define POLYNOMIAL 0x04c11db7L      // Standard CRC-32 ppolynomial
#define BUFFER_LEN       4096L      // Length of buffer

//----- Gloabl variables ------------------------------------------------------
static word32 crc_table[256];       // Table of 8-bit remainders

//----- Prototypes ------------------------------------------------------------
void gen_crc_table(void);
word32 update_crc(word32 crc_accum, byte *data_blk_ptr, word32 data_blk_size);

//===== Main program ==========================================================
int main(void)
{
  byte        buff[BUFFER_LEN]; // Buffer of packet bytes
  word32      crc32 = crucible_uint32_t("crc32_init");            // 32-bit CRC value
  word32      crc32_0 = crc32;
  word16      i = crucible_uint16_t("i0");                // Loop counter (16 bit)
  word32      j = crucible_uint32_t("j0");                // Loop counter (32 bit)

  crucible_breakpoint("Setup", i, j);

  // Initialize the CRC table
  gen_crc_table();

  crucible_breakpoint("Post-setup");

  // Load buffer with BUFFER_LEN random bytes
  for (i=0; i<BUFFER_LEN; i++)
    buff[i] = (byte) crucible_uint8_t("byte");

  crucible_breakpoint("Initialized buffer");

  // Compute and output CRC
  crc32 = update_crc(-1, buff, BUFFER_LEN);

  crucible_breakpoint("Updated CRC");

  crucible_assert(crc32 != crc32_0, __FILE__, __LINE__);
  printf("CRC = %08X \n", crc32);
  return 0;
}

//=============================================================================
//=  CRC32 table initialization                                               =
//=============================================================================
void gen_crc_table(void)
{
  register word16 i, j;
  register word32 crc_accum;

  for (i=0;  i<256;  i++)
  {
    crc_accum = ( (word32) i << 24 );
    for ( j = 0;  j < 8;  j++ )
    {
      if ( crc_accum & 0x80000000L )
        crc_accum = (crc_accum << 1) ^ POLYNOMIAL;
      else
        crc_accum = (crc_accum << 1);
    }
    crc_table[i] = crc_accum;
  }
}

//=============================================================================
//=  CRC32 generation                                                         =
//=============================================================================
word32 update_crc(word32 crc_accum, byte *data_blk_ptr, word32 data_blk_size)
{
   register word32 i, j;

   for (j=0; j<data_blk_size; j++)
   {
     i = ((int) (crc_accum >> 24) ^ *data_blk_ptr++) & 0xFF;
     crc_accum = (crc_accum << 8) ^ crc_table[i];
   }
   crc_accum = ~crc_accum;

   return crc_accum;
}
