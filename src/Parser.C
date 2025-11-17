/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         javalette_parse
#define yylex           javalette_lex
#define yyerror         javalette_error
#define yydebug         javalette_debug
#define yynerrs         javalette_nerrs

/* First part of user prologue.  */
#line 20 "Javalette.y"

/* Begin C preamble code */

#include <algorithm> /* for std::reverse */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Absyn.H"

#define YYMAXDEPTH 10000000

/* The type yyscan_t is defined by flex, but we need it in the parser already. */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE javalette__scan_string(const char *str, yyscan_t scanner);
extern void javalette__delete_buffer(YY_BUFFER_STATE buf, yyscan_t scanner);

extern void javalette_lex_destroy(yyscan_t scanner);
extern char* javalette_get_text(yyscan_t scanner);

extern yyscan_t javalette__initialize_lexer(FILE * inp);

/* End C preamble code */

#line 105 "Parser.C"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "Bison.H"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL__ERROR_ = 3,                    /* _ERROR_  */
  YYSYMBOL__BANG = 4,                      /* _BANG  */
  YYSYMBOL__BANGEQ = 5,                    /* _BANGEQ  */
  YYSYMBOL__PERCENT = 6,                   /* _PERCENT  */
  YYSYMBOL__DAMP = 7,                      /* _DAMP  */
  YYSYMBOL__LPAREN = 8,                    /* _LPAREN  */
  YYSYMBOL__RPAREN = 9,                    /* _RPAREN  */
  YYSYMBOL__STAR = 10,                     /* _STAR  */
  YYSYMBOL__PLUS = 11,                     /* _PLUS  */
  YYSYMBOL__DPLUS = 12,                    /* _DPLUS  */
  YYSYMBOL__COMMA = 13,                    /* _COMMA  */
  YYSYMBOL__MINUS = 14,                    /* _MINUS  */
  YYSYMBOL__DMINUS = 15,                   /* _DMINUS  */
  YYSYMBOL__SLASH = 16,                    /* _SLASH  */
  YYSYMBOL__SEMI = 17,                     /* _SEMI  */
  YYSYMBOL__LT = 18,                       /* _LT  */
  YYSYMBOL__LDARROW = 19,                  /* _LDARROW  */
  YYSYMBOL__EQ = 20,                       /* _EQ  */
  YYSYMBOL__DEQ = 21,                      /* _DEQ  */
  YYSYMBOL__GT = 22,                       /* _GT  */
  YYSYMBOL__GTEQ = 23,                     /* _GTEQ  */
  YYSYMBOL__AT = 24,                       /* _AT  */
  YYSYMBOL__KW_boolean = 25,               /* _KW_boolean  */
  YYSYMBOL__KW_double = 26,                /* _KW_double  */
  YYSYMBOL__KW_else = 27,                  /* _KW_else  */
  YYSYMBOL__KW_false = 28,                 /* _KW_false  */
  YYSYMBOL__KW_if = 29,                    /* _KW_if  */
  YYSYMBOL__KW_int = 30,                   /* _KW_int  */
  YYSYMBOL__KW_return = 31,                /* _KW_return  */
  YYSYMBOL__KW_true = 32,                  /* _KW_true  */
  YYSYMBOL__KW_void = 33,                  /* _KW_void  */
  YYSYMBOL__KW_while = 34,                 /* _KW_while  */
  YYSYMBOL__LBRACE = 35,                   /* _LBRACE  */
  YYSYMBOL__DBAR = 36,                     /* _DBAR  */
  YYSYMBOL__RBRACE = 37,                   /* _RBRACE  */
  YYSYMBOL__STRING_ = 38,                  /* _STRING_  */
  YYSYMBOL__INTEGER_ = 39,                 /* _INTEGER_  */
  YYSYMBOL__DOUBLE_ = 40,                  /* _DOUBLE_  */
  YYSYMBOL__IDENT_ = 41,                   /* _IDENT_  */
  YYSYMBOL_YYACCEPT = 42,                  /* $accept  */
  YYSYMBOL_Prog = 43,                      /* Prog  */
  YYSYMBOL_TopDef = 44,                    /* TopDef  */
  YYSYMBOL_ListTopDef = 45,                /* ListTopDef  */
  YYSYMBOL_Arg = 46,                       /* Arg  */
  YYSYMBOL_ListArg = 47,                   /* ListArg  */
  YYSYMBOL_Blk = 48,                       /* Blk  */
  YYSYMBOL_ListStmt = 49,                  /* ListStmt  */
  YYSYMBOL_Stmt = 50,                      /* Stmt  */
  YYSYMBOL_Item = 51,                      /* Item  */
  YYSYMBOL_ListItem = 52,                  /* ListItem  */
  YYSYMBOL_Type = 53,                      /* Type  */
  YYSYMBOL_Expr6 = 54,                     /* Expr6  */
  YYSYMBOL_Expr5 = 55,                     /* Expr5  */
  YYSYMBOL_Expr4 = 56,                     /* Expr4  */
  YYSYMBOL_Expr3 = 57,                     /* Expr3  */
  YYSYMBOL_Expr2 = 58,                     /* Expr2  */
  YYSYMBOL_Expr1 = 59,                     /* Expr1  */
  YYSYMBOL_Expr = 60,                      /* Expr  */
  YYSYMBOL_ListExpr = 61,                  /* ListExpr  */
  YYSYMBOL_AddOp = 62,                     /* AddOp  */
  YYSYMBOL_MulOp = 63,                     /* MulOp  */
  YYSYMBOL_RelOp = 64                      /* RelOp  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 86 "Javalette.y"

#include "ParserError.H"   // optional, only if you want to use parse_error
#include <sstream>         // optional

void yyerror(YYLTYPE *loc, yyscan_t scanner, YYSTYPE *result, const char *msg)
{
  (void)loc;
  (void)scanner;
  (void)result;
  (void)msg;
  // Do NOTHING here – just let yyparse() fail and return non-zero.
}

int yyparse(yyscan_t scanner, YYSTYPE *result);

extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t scanner);

#line 221 "Parser.C"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   179

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  42
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  23
/* YYNRULES -- Number of rules.  */
#define YYNRULES  68
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  111

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   296


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   172,   172,   174,   176,   177,   179,   181,   182,   183,
     185,   187,   188,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   203,   204,   206,   207,   209,
     210,   211,   212,   218,   219,   220,   221,   222,   223,   224,
     225,   227,   228,   229,   231,   232,   234,   235,   237,   238,
     240,   241,   243,   244,   245,   247,   248,   249,   251,   252,
     254,   255,   256,   258,   259,   260,   261,   262,   263
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "_ERROR_", "_BANG",
  "_BANGEQ", "_PERCENT", "_DAMP", "_LPAREN", "_RPAREN", "_STAR", "_PLUS",
  "_DPLUS", "_COMMA", "_MINUS", "_DMINUS", "_SLASH", "_SEMI", "_LT",
  "_LDARROW", "_EQ", "_DEQ", "_GT", "_GTEQ", "_AT", "_KW_boolean",
  "_KW_double", "_KW_else", "_KW_false", "_KW_if", "_KW_int", "_KW_return",
  "_KW_true", "_KW_void", "_KW_while", "_LBRACE", "_DBAR", "_RBRACE",
  "_STRING_", "_INTEGER_", "_DOUBLE_", "_IDENT_", "$accept", "Prog",
  "TopDef", "ListTopDef", "Arg", "ListArg", "Blk", "ListStmt", "Stmt",
  "Item", "ListItem", "Type", "Expr6", "Expr5", "Expr4", "Expr3", "Expr2",
  "Expr1", "Expr", "ListExpr", "AddOp", "MulOp", "RelOp", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-74)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      33,   -74,   -74,   -74,   -74,     9,    33,   -74,   -31,   -74,
     -74,     3,    33,     2,    19,   -10,    33,     4,   -74,   -74,
     -74,   -74,    15,    75,   117,    75,   -74,   -74,    27,    94,
     -74,    34,   -74,   -74,   -74,   -74,     5,   -74,   -74,   -23,
     -74,   -74,    -2,    10,   156,    25,    45,    43,   -74,    41,
      58,   -74,   117,   -74,    51,   117,   117,    54,    55,   117,
     117,    57,    61,    59,   -74,   -74,   -74,   132,   -74,   -74,
     132,   -74,   132,   -74,   -74,   -74,   -74,   -74,   132,   117,
     -74,   -74,    66,   -74,    69,    67,    83,   -74,   -74,    76,
     -74,   117,    60,   -74,   -74,    -2,   -74,    10,   -74,    56,
      56,   117,   -74,   -74,   -74,   -74,    77,   -74,   -74,    56,
     -74
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    31,    30,    29,    32,     0,     4,     2,     0,     1,
       5,     0,     7,     8,     0,     0,     7,     0,     6,     9,
      11,     3,     0,     0,     0,     0,    13,    37,     0,     0,
      36,     0,    10,    39,    34,    35,    33,    14,    12,     0,
      43,    45,    47,    49,    51,    53,     0,    33,    42,     0,
       0,    41,     0,    20,     0,     0,    55,     0,     0,     0,
       0,    25,    27,     0,    62,    60,    61,     0,    58,    59,
       0,    68,     0,    63,    64,    67,    65,    66,     0,     0,
      24,    40,     0,    19,     0,    56,     0,    17,    18,     0,
      54,     0,     0,    15,    44,    46,    50,    48,    52,     0,
       0,    55,    38,    16,    26,    28,    21,    23,    57,     0,
      22
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -74,   -74,   -74,    99,   -74,    90,    93,   -74,   -73,   -74,
      20,     0,   -20,    50,    48,    52,   -74,    65,   -22,    22,
     -74,   -74,   -74
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     5,     6,     7,    13,    14,    37,    22,    38,    62,
      63,    49,    40,    41,    42,    43,    44,    45,    46,    86,
      70,    67,    78
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
       8,    60,    50,    48,    64,    51,     8,    54,    65,     9,
      11,    12,    15,    56,    66,    16,    15,    57,    61,    23,
      58,    68,    39,    24,    69,    59,   106,   107,    17,    25,
      82,    18,    26,    84,    85,    52,   110,    89,    90,    20,
       1,     2,    55,    27,    28,     3,    29,    30,     4,    31,
      20,    56,    32,    33,    34,    35,    36,    98,     1,     2,
      23,    79,    80,     3,    24,    60,     4,    81,    83,   104,
      25,    87,    88,    26,    92,    99,    93,    91,   100,    85,
     101,     1,     2,    24,    27,    28,     3,    29,    30,     4,
      31,    20,   102,   103,    33,    34,    35,    36,    23,    39,
      39,    61,    24,    27,   109,    10,    19,    30,    25,    39,
      21,    53,   105,    33,    34,    35,    47,    94,    95,     1,
       2,    23,    27,   108,     3,    24,    30,     4,     0,     0,
      97,    25,    33,    34,    35,    47,    23,    96,     0,     0,
      24,     0,     1,     2,     0,    27,    25,     3,     0,    30,
       4,     0,     0,     0,     0,    33,    34,    35,    47,     0,
      27,    71,     0,    72,    30,     0,     0,     0,     0,     0,
      33,    34,    35,    47,    73,    74,     0,    75,    76,    77
};

static const yytype_int8 yycheck[] =
{
       0,    24,    24,    23,     6,    25,     6,    29,    10,     0,
      41,     8,    12,     8,    16,    13,    16,    12,    41,     4,
      15,    11,    22,     8,    14,    20,    99,   100,     9,    14,
      52,    41,    17,    55,    56,     8,   109,    59,    60,    35,
      25,    26,     8,    28,    29,    30,    31,    32,    33,    34,
      35,     8,    37,    38,    39,    40,    41,    79,    25,    26,
       4,    36,    17,    30,     8,    24,    33,     9,    17,    91,
      14,    17,    17,    17,    13,     9,    17,    20,     9,   101,
      13,    25,    26,     8,    28,    29,    30,    31,    32,    33,
      34,    35,     9,    17,    38,    39,    40,    41,     4,    99,
     100,    41,     8,    28,    27,     6,    16,    32,    14,   109,
      17,    17,    92,    38,    39,    40,    41,    67,    70,    25,
      26,     4,    28,   101,    30,     8,    32,    33,    -1,    -1,
      78,    14,    38,    39,    40,    41,     4,    72,    -1,    -1,
       8,    -1,    25,    26,    -1,    28,    14,    30,    -1,    32,
      33,    -1,    -1,    -1,    -1,    38,    39,    40,    41,    -1,
      28,     5,    -1,     7,    32,    -1,    -1,    -1,    -1,    -1,
      38,    39,    40,    41,    18,    19,    -1,    21,    22,    23
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    25,    26,    30,    33,    43,    44,    45,    53,     0,
      45,    41,     8,    46,    47,    53,    13,     9,    41,    47,
      35,    48,    49,     4,     8,    14,    17,    28,    29,    31,
      32,    34,    37,    38,    39,    40,    41,    48,    50,    53,
      54,    55,    56,    57,    58,    59,    60,    41,    54,    53,
      60,    54,     8,    17,    60,     8,     8,    12,    15,    20,
      24,    41,    51,    52,     6,    10,    16,    63,    11,    14,
      62,     5,     7,    18,    19,    21,    22,    23,    64,    36,
      17,     9,    60,    17,    60,    60,    61,    17,    17,    60,
      60,    20,    13,    17,    55,    56,    59,    57,    60,     9,
       9,    13,     9,    17,    60,    52,    50,    50,    61,    27,
      50
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    42,    43,    44,    45,    45,    46,    47,    47,    47,
      48,    49,    49,    50,    50,    50,    50,    50,    50,    50,
      50,    50,    50,    50,    50,    51,    51,    52,    52,    53,
      53,    53,    53,    54,    54,    54,    54,    54,    54,    54,
      54,    55,    55,    55,    56,    56,    57,    57,    58,    58,
      59,    59,    60,    60,    60,    61,    61,    61,    62,    62,
      63,    63,    63,    64,    64,    64,    64,    64,    64
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     6,     1,     2,     2,     0,     1,     3,
       3,     0,     2,     1,     1,     3,     4,     3,     3,     3,
       2,     5,     7,     5,     2,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     1,
       3,     2,     2,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     0,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, scanner, result, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, scanner, result); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, scanner, result);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, yyscan_t scanner, YYSTYPE *result)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), scanner, result);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, scanner, result); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t scanner, YYSTYPE *result)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Prog: ListTopDef  */
#line 172 "Javalette.y"
                  { std::reverse((yyvsp[0].listtopdef_)->begin(),(yyvsp[0].listtopdef_)->end()) ;(yyval.prog_) = new Program((yyvsp[0].listtopdef_)); result->prog_ = (yyval.prog_); }
#line 1391 "Parser.C"
    break;

  case 3: /* TopDef: Type _IDENT_ _LPAREN ListArg _RPAREN Blk  */
#line 174 "Javalette.y"
                                                  { std::reverse((yyvsp[-2].listarg_)->begin(),(yyvsp[-2].listarg_)->end()) ;(yyval.topdef_) = new FnDef((yyvsp[-5].type_), (yyvsp[-4]._string), (yyvsp[-2].listarg_), (yyvsp[0].blk_)); }
#line 1397 "Parser.C"
    break;

  case 4: /* ListTopDef: TopDef  */
#line 176 "Javalette.y"
                    { (yyval.listtopdef_) = new ListTopDef(); (yyval.listtopdef_)->push_back((yyvsp[0].topdef_)); }
#line 1403 "Parser.C"
    break;

  case 5: /* ListTopDef: TopDef ListTopDef  */
#line 177 "Javalette.y"
                      { (yyvsp[0].listtopdef_)->push_back((yyvsp[-1].topdef_)); (yyval.listtopdef_) = (yyvsp[0].listtopdef_); }
#line 1409 "Parser.C"
    break;

  case 6: /* Arg: Type _IDENT_  */
#line 179 "Javalette.y"
                   { (yyval.arg_) = new Argument((yyvsp[-1].type_), (yyvsp[0]._string)); }
#line 1415 "Parser.C"
    break;

  case 7: /* ListArg: %empty  */
#line 181 "Javalette.y"
                      { (yyval.listarg_) = new ListArg(); }
#line 1421 "Parser.C"
    break;

  case 8: /* ListArg: Arg  */
#line 182 "Javalette.y"
        { (yyval.listarg_) = new ListArg(); (yyval.listarg_)->push_back((yyvsp[0].arg_)); }
#line 1427 "Parser.C"
    break;

  case 9: /* ListArg: Arg _COMMA ListArg  */
#line 183 "Javalette.y"
                       { (yyvsp[0].listarg_)->push_back((yyvsp[-2].arg_)); (yyval.listarg_) = (yyvsp[0].listarg_); }
#line 1433 "Parser.C"
    break;

  case 10: /* Blk: _LBRACE ListStmt _RBRACE  */
#line 185 "Javalette.y"
                               { (yyval.blk_) = new Block((yyvsp[-1].liststmt_)); }
#line 1439 "Parser.C"
    break;

  case 11: /* ListStmt: %empty  */
#line 187 "Javalette.y"
                       { (yyval.liststmt_) = new ListStmt(); }
#line 1445 "Parser.C"
    break;

  case 12: /* ListStmt: ListStmt Stmt  */
#line 188 "Javalette.y"
                  { (yyvsp[-1].liststmt_)->push_back((yyvsp[0].stmt_)); (yyval.liststmt_) = (yyvsp[-1].liststmt_); }
#line 1451 "Parser.C"
    break;

  case 13: /* Stmt: _SEMI  */
#line 190 "Javalette.y"
             { (yyval.stmt_) = new Empty(); }
#line 1457 "Parser.C"
    break;

  case 14: /* Stmt: Blk  */
#line 191 "Javalette.y"
        { (yyval.stmt_) = new BStmt((yyvsp[0].blk_)); }
#line 1463 "Parser.C"
    break;

  case 15: /* Stmt: Type ListItem _SEMI  */
#line 192 "Javalette.y"
                        { std::reverse((yyvsp[-1].listitem_)->begin(),(yyvsp[-1].listitem_)->end()) ;(yyval.stmt_) = new Decl((yyvsp[-2].type_), (yyvsp[-1].listitem_)); }
#line 1469 "Parser.C"
    break;

  case 16: /* Stmt: _IDENT_ _EQ Expr _SEMI  */
#line 193 "Javalette.y"
                           { (yyval.stmt_) = new Ass((yyvsp[-3]._string), (yyvsp[-1].expr_)); }
#line 1475 "Parser.C"
    break;

  case 17: /* Stmt: _IDENT_ _DPLUS _SEMI  */
#line 194 "Javalette.y"
                         { (yyval.stmt_) = new Incr((yyvsp[-2]._string)); }
#line 1481 "Parser.C"
    break;

  case 18: /* Stmt: _IDENT_ _DMINUS _SEMI  */
#line 195 "Javalette.y"
                          { (yyval.stmt_) = new Decr((yyvsp[-2]._string)); }
#line 1487 "Parser.C"
    break;

  case 19: /* Stmt: _KW_return Expr _SEMI  */
#line 196 "Javalette.y"
                          { (yyval.stmt_) = new Ret((yyvsp[-1].expr_)); }
#line 1493 "Parser.C"
    break;

  case 20: /* Stmt: _KW_return _SEMI  */
#line 197 "Javalette.y"
                     { (yyval.stmt_) = new VRet(); }
#line 1499 "Parser.C"
    break;

  case 21: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt  */
#line 198 "Javalette.y"
                                     { (yyval.stmt_) = new Cond((yyvsp[-2].expr_), (yyvsp[0].stmt_)); }
#line 1505 "Parser.C"
    break;

  case 22: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt _KW_else Stmt  */
#line 199 "Javalette.y"
                                                   { (yyval.stmt_) = new CondElse((yyvsp[-4].expr_), (yyvsp[-2].stmt_), (yyvsp[0].stmt_)); }
#line 1511 "Parser.C"
    break;

  case 23: /* Stmt: _KW_while _LPAREN Expr _RPAREN Stmt  */
#line 200 "Javalette.y"
                                        { (yyval.stmt_) = new While((yyvsp[-2].expr_), (yyvsp[0].stmt_)); }
#line 1517 "Parser.C"
    break;

  case 24: /* Stmt: Expr _SEMI  */
#line 201 "Javalette.y"
               { (yyval.stmt_) = new SExp((yyvsp[-1].expr_)); }
#line 1523 "Parser.C"
    break;

  case 25: /* Item: _IDENT_  */
#line 203 "Javalette.y"
               { (yyval.item_) = new NoInit((yyvsp[0]._string)); }
#line 1529 "Parser.C"
    break;

  case 26: /* Item: _IDENT_ _EQ Expr  */
#line 204 "Javalette.y"
                     { (yyval.item_) = new Init((yyvsp[-2]._string), (yyvsp[0].expr_)); }
#line 1535 "Parser.C"
    break;

  case 27: /* ListItem: Item  */
#line 206 "Javalette.y"
                { (yyval.listitem_) = new ListItem(); (yyval.listitem_)->push_back((yyvsp[0].item_)); }
#line 1541 "Parser.C"
    break;

  case 28: /* ListItem: Item _COMMA ListItem  */
#line 207 "Javalette.y"
                         { (yyvsp[0].listitem_)->push_back((yyvsp[-2].item_)); (yyval.listitem_) = (yyvsp[0].listitem_); }
#line 1547 "Parser.C"
    break;

  case 29: /* Type: _KW_int  */
#line 209 "Javalette.y"
               { (yyval.type_) = new Int(); }
#line 1553 "Parser.C"
    break;

  case 30: /* Type: _KW_double  */
#line 210 "Javalette.y"
               { (yyval.type_) = new Doub(); }
#line 1559 "Parser.C"
    break;

  case 31: /* Type: _KW_boolean  */
#line 211 "Javalette.y"
                { (yyval.type_) = new Bool(); }
#line 1565 "Parser.C"
    break;

  case 32: /* Type: _KW_void  */
#line 212 "Javalette.y"
             { (yyval.type_) = new Void(); }
#line 1571 "Parser.C"
    break;

  case 33: /* Expr6: _IDENT_  */
#line 218 "Javalette.y"
                { (yyval.expr_) = new EVar((yyvsp[0]._string)); }
#line 1577 "Parser.C"
    break;

  case 34: /* Expr6: _INTEGER_  */
#line 219 "Javalette.y"
              { (yyval.expr_) = new ELitInt((yyvsp[0]._int)); }
#line 1583 "Parser.C"
    break;

  case 35: /* Expr6: _DOUBLE_  */
#line 220 "Javalette.y"
             { (yyval.expr_) = new ELitDoub((yyvsp[0]._double)); }
#line 1589 "Parser.C"
    break;

  case 36: /* Expr6: _KW_true  */
#line 221 "Javalette.y"
             { (yyval.expr_) = new ELitTrue(); }
#line 1595 "Parser.C"
    break;

  case 37: /* Expr6: _KW_false  */
#line 222 "Javalette.y"
              { (yyval.expr_) = new ELitFalse(); }
#line 1601 "Parser.C"
    break;

  case 38: /* Expr6: _IDENT_ _LPAREN ListExpr _RPAREN  */
#line 223 "Javalette.y"
                                     { std::reverse((yyvsp[-1].listexpr_)->begin(),(yyvsp[-1].listexpr_)->end()) ;(yyval.expr_) = new EApp((yyvsp[-3]._string), (yyvsp[-1].listexpr_)); }
#line 1607 "Parser.C"
    break;

  case 39: /* Expr6: _STRING_  */
#line 224 "Javalette.y"
             { (yyval.expr_) = new EString((yyvsp[0]._string)); }
#line 1613 "Parser.C"
    break;

  case 40: /* Expr6: _LPAREN Expr _RPAREN  */
#line 225 "Javalette.y"
                         { (yyval.expr_) = (yyvsp[-1].expr_); }
#line 1619 "Parser.C"
    break;

  case 41: /* Expr5: _MINUS Expr6  */
#line 227 "Javalette.y"
                     { (yyval.expr_) = new Neg((yyvsp[0].expr_)); }
#line 1625 "Parser.C"
    break;

  case 42: /* Expr5: _BANG Expr6  */
#line 228 "Javalette.y"
                { (yyval.expr_) = new Not((yyvsp[0].expr_)); }
#line 1631 "Parser.C"
    break;

  case 43: /* Expr5: Expr6  */
#line 229 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1637 "Parser.C"
    break;

  case 44: /* Expr4: Expr4 MulOp Expr5  */
#line 231 "Javalette.y"
                          { (yyval.expr_) = new EMul((yyvsp[-2].expr_), (yyvsp[-1].mulop_), (yyvsp[0].expr_)); }
#line 1643 "Parser.C"
    break;

  case 45: /* Expr4: Expr5  */
#line 232 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1649 "Parser.C"
    break;

  case 46: /* Expr3: Expr3 AddOp Expr4  */
#line 234 "Javalette.y"
                          { (yyval.expr_) = new EAdd((yyvsp[-2].expr_), (yyvsp[-1].addop_), (yyvsp[0].expr_)); }
#line 1655 "Parser.C"
    break;

  case 47: /* Expr3: Expr4  */
#line 235 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1661 "Parser.C"
    break;

  case 48: /* Expr2: Expr2 RelOp Expr3  */
#line 237 "Javalette.y"
                          { (yyval.expr_) = new ERel((yyvsp[-2].expr_), (yyvsp[-1].relop_), (yyvsp[0].expr_)); }
#line 1667 "Parser.C"
    break;

  case 49: /* Expr2: Expr3  */
#line 238 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1673 "Parser.C"
    break;

  case 50: /* Expr1: Expr2 _DAMP Expr1  */
#line 240 "Javalette.y"
                          { (yyval.expr_) = new EAnd((yyvsp[-2].expr_), (yyvsp[0].expr_)); }
#line 1679 "Parser.C"
    break;

  case 51: /* Expr1: Expr2  */
#line 241 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1685 "Parser.C"
    break;

  case 52: /* Expr: Expr1 _DBAR Expr  */
#line 243 "Javalette.y"
                        { (yyval.expr_) = new EOr((yyvsp[-2].expr_), (yyvsp[0].expr_)); }
#line 1691 "Parser.C"
    break;

  case 53: /* Expr: Expr1  */
#line 244 "Javalette.y"
          { (yyval.expr_) = (yyvsp[0].expr_); }
#line 1697 "Parser.C"
    break;

  case 54: /* Expr: Type _AT Expr  */
#line 245 "Javalette.y"
                  { (yyval.expr_) = new EAnnotExp((yyvsp[-2].type_), (yyvsp[0].expr_)); }
#line 1703 "Parser.C"
    break;

  case 55: /* ListExpr: %empty  */
#line 247 "Javalette.y"
                       { (yyval.listexpr_) = new ListExpr(); }
#line 1709 "Parser.C"
    break;

  case 56: /* ListExpr: Expr  */
#line 248 "Javalette.y"
         { (yyval.listexpr_) = new ListExpr(); (yyval.listexpr_)->push_back((yyvsp[0].expr_)); }
#line 1715 "Parser.C"
    break;

  case 57: /* ListExpr: Expr _COMMA ListExpr  */
#line 249 "Javalette.y"
                         { (yyvsp[0].listexpr_)->push_back((yyvsp[-2].expr_)); (yyval.listexpr_) = (yyvsp[0].listexpr_); }
#line 1721 "Parser.C"
    break;

  case 58: /* AddOp: _PLUS  */
#line 251 "Javalette.y"
              { (yyval.addop_) = new Plus(); }
#line 1727 "Parser.C"
    break;

  case 59: /* AddOp: _MINUS  */
#line 252 "Javalette.y"
           { (yyval.addop_) = new Minus(); }
#line 1733 "Parser.C"
    break;

  case 60: /* MulOp: _STAR  */
#line 254 "Javalette.y"
              { (yyval.mulop_) = new Times(); }
#line 1739 "Parser.C"
    break;

  case 61: /* MulOp: _SLASH  */
#line 255 "Javalette.y"
           { (yyval.mulop_) = new Div(); }
#line 1745 "Parser.C"
    break;

  case 62: /* MulOp: _PERCENT  */
#line 256 "Javalette.y"
             { (yyval.mulop_) = new Mod(); }
#line 1751 "Parser.C"
    break;

  case 63: /* RelOp: _LT  */
#line 258 "Javalette.y"
            { (yyval.relop_) = new LTH(); }
#line 1757 "Parser.C"
    break;

  case 64: /* RelOp: _LDARROW  */
#line 259 "Javalette.y"
             { (yyval.relop_) = new LE(); }
#line 1763 "Parser.C"
    break;

  case 65: /* RelOp: _GT  */
#line 260 "Javalette.y"
        { (yyval.relop_) = new GTH(); }
#line 1769 "Parser.C"
    break;

  case 66: /* RelOp: _GTEQ  */
#line 261 "Javalette.y"
          { (yyval.relop_) = new GE(); }
#line 1775 "Parser.C"
    break;

  case 67: /* RelOp: _DEQ  */
#line 262 "Javalette.y"
         { (yyval.relop_) = new EQU(); }
#line 1781 "Parser.C"
    break;

  case 68: /* RelOp: _BANGEQ  */
#line 263 "Javalette.y"
            { (yyval.relop_) = new NE(); }
#line 1787 "Parser.C"
    break;


#line 1791 "Parser.C"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (&yylloc, scanner, result, YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, scanner, result);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, result, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, scanner, result);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 266 "Javalette.y"



/* Entrypoint: parse Prog* from file. */
Prog* pProg(FILE *inp)
{
  YYSTYPE result;
  yyscan_t scanner = javalette__initialize_lexer(inp);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  int error = yyparse(scanner, &result);
  javalette_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.prog_;
  }
}

/* Entrypoint: parse Prog* from string. */
Prog* psProg(const char *str)
{
  YYSTYPE result;
  yyscan_t scanner = javalette__initialize_lexer(0);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  YY_BUFFER_STATE buf = javalette__scan_string(str, scanner);
  int error = yyparse(scanner, &result);
  javalette__delete_buffer(buf, scanner);
  javalette_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.prog_;
  }
}



