/*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */
#include <stdio.h>
#include <macaroons.h>

#define MACAROON_MACRO(shortname) fprintf(file,"let m_%s = %d\n",#shortname, MACAROON_ ## shortname)

#define MACAROON_MAP_returncode(FN,num_suffix)\
  FN(num_suffix,SUCCESS, Success, "success"); \
  FN(num_suffix,OUT_OF_MEMORY, Out_of_memory, "out of memory"); \
  FN(num_suffix,HASH_FAILED, Hash_failed, "hash failed");       \
  FN(num_suffix,INVALID, Invalid, "invalid");                   \
  FN(num_suffix,TOO_MANY_CAVEATS, Too_many_caveats, "too many caveats");\
  FN(num_suffix,CYCLE, Cycle, "cycle");                                 \
  FN(num_suffix,BUF_TOO_SMALL, Buf_too_small, "buffer too small");      \
  FN(num_suffix,NOT_AUTHORIZED, Not_authorized, "not authorized");      \
  FN(num_suffix,NO_JSON_SUPPORT, No_json_support, "no JSON support");

#define MACAROON_ENUM_DEF(_a, _b, consname, _c) fprintf(file,"| %s\n",#consname)
#define MACAROON_CODE_MAP(suffix, enumname, consname, _) fprintf(file,"| %d%s -> %s\n", MACAROON_ ## enumname, suffix, #consname)
#define MACAROON_CODE_UNMAP(suffix, enumname, consname, _) fprintf(file,"| %s -> %d%s\n", #consname, MACAROON_ ## enumname, suffix)
#define MACAROON_STRING_UNMAP(_a, _b, consname, descr) fprintf(file, "| %s -> \"%s\"\n", #consname, descr)

int main(void) {
  int sz_enum_macaroon_returncode = sizeof(enum macaroon_returncode);
  FILE *file = fopen("macaroons_probe.ml", "w");
  char *returncode_suffix;
  char *returncode_type;
  char *returncode_to_string;

  switch (sz_enum_macaroon_returncode) {
  case 4:
    returncode_suffix = "_l";
    returncode_type = "int32";
    returncode_to_string = "Int32.to_string";
    break;
  case 8:
    returncode_suffix = "_L";
    returncode_type = "int64";
    returncode_to_string = "Int64.to_string";
    break;
  default:
    fprintf(stderr,
            "Don't know numeric suffix for enum of size %d. Terminating.\n",
            sz_enum_macaroon_returncode);
    exit(1);
  }

  MACAROON_MACRO(MAX_STRLEN);
  MACAROON_MACRO(MAX_CAVEATS);
  MACAROON_MACRO(SUGGESTED_SECRET_LENGTH);

  fprintf(file, "\ntype returncode =\n");

  MACAROON_MAP_returncode(MACAROON_ENUM_DEF,returncode_suffix);
  fprintf(file, "| Unknown of %s\n", returncode_type);

  fprintf(file, "\nlet returncode_of_code = function\n");

  MACAROON_MAP_returncode(MACAROON_CODE_MAP,returncode_suffix);
  fprintf(file, "| k    -> Unknown k\n");

  fprintf(file, "\nlet code_of_returncode = function\n");

  MACAROON_MAP_returncode(MACAROON_CODE_UNMAP,returncode_suffix);
  fprintf(file, "| Unknown k -> k\n");

  switch (sz_enum_macaroon_returncode) {
  case 4:
    fprintf(file, "\nlet ty_returncode = Ctypes.int32_t\n");
    break;
  case 8:
    fprintf(file, "\nlet ty_returncode = Ctypes.int64_t\n");
    break;
  default:
    fprintf(stderr,
            "Don't know how to allocate enum of size %d. Terminating.\n",
            sz_enum_macaroon_returncode);
    exit(1);
  }

  fprintf(file, "\nlet z_returncode = %d%s\n",
          MACAROON_SUCCESS,returncode_suffix);
  fprintf(file, "\nlet string_of_returncode = function\n");

  MACAROON_MAP_returncode(MACAROON_STRING_UNMAP,returncode_suffix);
  fprintf(file, "| Unknown k -> \"unknown returncode \"^(%s k)\n",
          returncode_to_string);

  fclose(file);
  return 0;
}
