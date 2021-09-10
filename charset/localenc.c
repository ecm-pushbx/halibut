/*
 * local.c - translate our internal character set codes to and from
 * our own set of plausibly legible character-set names. Also
 * provides a canonical name for each encoding (useful for software
 * announcing what character set it will be using), and a set of
 * enumeration functions which return a list of supported
 * encodings one by one.
 * 
 * Also in this table are other ways people might plausibly refer
 * to a charset (for example, Win1252 as well as CP1252). Where
 * more than one string is given for a particular character set,
 * the first one is the canonical one returned by
 * charset_to_localenc.
 * 
 * charset_from_localenc will attempt all other text translations
 * as well as this table, to maximise the number of different ways
 * you can select a supported charset.
 */

#include <ctype.h>
#include "charset.h"
#include "internal.h"

static const struct {
    const char *name;
    int charset;
    bool return_in_enum;   /* enumeration misses some charsets */
} localencs[] = {
    { "<UNKNOWN>", CS_NONE, false },
    { "ASCII", CS_ASCII, true },
    { "BS 4730", CS_BS4730, true },
    { "BS-4730", CS_BS4730, false },
    { "BS4730", CS_BS4730, false },
    { "ISO-8859-1", CS_ISO8859_1, true },
    { "ISO-8859-1 with X11 line drawing", CS_ISO8859_1_X11, false },
    { "ISO-8859-1-X11", CS_ISO8859_1_X11, false },
    { "ISO8859-1-X11", CS_ISO8859_1_X11, false },
    { "ISO-8859-2", CS_ISO8859_2, true },
    { "ISO-8859-3", CS_ISO8859_3, true },
    { "ISO-8859-4", CS_ISO8859_4, true },
    { "ISO-8859-5", CS_ISO8859_5, true },
    { "ISO-8859-6", CS_ISO8859_6, true },
    { "ISO-8859-7", CS_ISO8859_7, true },
    { "ISO-8859-8", CS_ISO8859_8, true },
    { "ISO-8859-9", CS_ISO8859_9, true },
    { "ISO-8859-10", CS_ISO8859_10, true },
    { "ISO-8859-11", CS_ISO8859_11, true },
    { "ISO-8859-13", CS_ISO8859_13, true },
    { "ISO-8859-14", CS_ISO8859_14, true },
    { "ISO-8859-15", CS_ISO8859_15, true },
    { "ISO-8859-16", CS_ISO8859_16, true },
    { "CP437", CS_CP437, true },
    { "CP850", CS_CP850, true },
    { "CP852", CS_CP852, true },
    { "CP866", CS_CP866, true },
    { "CP874", CS_CP874, true },
    { "Win874", CS_CP874, false },
    { "Win-874", CS_CP874, false },
    { "CP1250", CS_CP1250, true },
    { "Win1250", CS_CP1250, false },
    { "CP1251", CS_CP1251, true },
    { "Win1251", CS_CP1251, false },
    { "CP1252", CS_CP1252, true },
    { "Win1252", CS_CP1252, false },
    { "CP1253", CS_CP1253, true },
    { "Win1253", CS_CP1253, false },
    { "CP1254", CS_CP1254, true },
    { "Win1254", CS_CP1254, false },
    { "CP1255", CS_CP1255, true },
    { "Win1255", CS_CP1255, false },
    { "CP1256", CS_CP1256, true },
    { "Win1256", CS_CP1256, false },
    { "CP1257", CS_CP1257, true },
    { "Win1257", CS_CP1257, false },
    { "CP1258", CS_CP1258, true },
    { "Win1258", CS_CP1258, false },
    { "KOI8-R", CS_KOI8_R, true },
    { "KOI8R", CS_KOI8_R, false },
    { "KOI8-U", CS_KOI8_U, true },
    { "KOI8U", CS_KOI8_U, false },
    { "KOI8-RU", CS_KOI8_RU, true },
    { "KOI8RU", CS_KOI8_RU, false },
    { "JIS X 0201", CS_JISX0201, true },
    { "JIS-X-0201", CS_JISX0201, false },
    { "JIS_X_0201", CS_JISX0201, false },
    { "JISX0201", CS_JISX0201, false },
    { "Mac Roman", CS_MAC_ROMAN, true },
    { "Mac-Roman", CS_MAC_ROMAN, false },
    { "MacRoman", CS_MAC_ROMAN, false },
    { "Mac Turkish", CS_MAC_TURKISH, true },
    { "Mac-Turkish", CS_MAC_TURKISH, false },
    { "MacTurkish", CS_MAC_TURKISH, false },
    { "Mac Croatian", CS_MAC_CROATIAN, true },
    { "Mac-Croatian", CS_MAC_CROATIAN, false },
    { "MacCroatian", CS_MAC_CROATIAN, false },
    { "Mac Iceland", CS_MAC_ICELAND, true },
    { "Mac-Iceland", CS_MAC_ICELAND, false },
    { "MacIceland", CS_MAC_ICELAND, false },
    { "Mac Romanian", CS_MAC_ROMANIAN, true },
    { "Mac-Romanian", CS_MAC_ROMANIAN, false },
    { "MacRomanian", CS_MAC_ROMANIAN, false },
    { "Mac Greek", CS_MAC_GREEK, true },
    { "Mac-Greek", CS_MAC_GREEK, false },
    { "MacGreek", CS_MAC_GREEK, false },
    { "Mac Cyrillic", CS_MAC_CYRILLIC, true },
    { "Mac-Cyrillic", CS_MAC_CYRILLIC, false },
    { "MacCyrillic", CS_MAC_CYRILLIC, false },
    { "Mac Thai", CS_MAC_THAI, true },
    { "Mac-Thai", CS_MAC_THAI, false },
    { "MacThai", CS_MAC_THAI, false },
    { "Mac Centeuro", CS_MAC_CENTEURO, true },
    { "Mac-Centeuro", CS_MAC_CENTEURO, false },
    { "MacCenteuro", CS_MAC_CENTEURO, false },
    { "Mac Symbol", CS_MAC_SYMBOL, true },
    { "Mac-Symbol", CS_MAC_SYMBOL, false },
    { "MacSymbol", CS_MAC_SYMBOL, false },
    { "Mac Dingbats", CS_MAC_DINGBATS, true },
    { "Mac-Dingbats", CS_MAC_DINGBATS, false },
    { "MacDingbats", CS_MAC_DINGBATS, false },
    { "Mac Roman (old)", CS_MAC_ROMAN_OLD, false },
    { "Mac-Roman-old", CS_MAC_ROMAN_OLD, false },
    { "MacRoman-old", CS_MAC_ROMAN_OLD, false },
    { "Mac Croatian (old)", CS_MAC_CROATIAN_OLD, false },
    { "Mac-Croatian-old", CS_MAC_CROATIAN_OLD, false },
    { "MacCroatian-old", CS_MAC_CROATIAN_OLD, false },
    { "Mac Iceland (old)", CS_MAC_ICELAND_OLD, false },
    { "Mac-Iceland-old", CS_MAC_ICELAND_OLD, false },
    { "MacIceland-old", CS_MAC_ICELAND_OLD, false },
    { "Mac Romanian (old)", CS_MAC_ROMANIAN_OLD, false },
    { "Mac-Romanian-old", CS_MAC_ROMANIAN_OLD, false },
    { "MacRomanian-old", CS_MAC_ROMANIAN_OLD, false },
    { "Mac Greek (old)", CS_MAC_GREEK_OLD, false },
    { "Mac-Greek-old", CS_MAC_GREEK_OLD, false },
    { "MacGreek-old", CS_MAC_GREEK_OLD, false },
    { "Mac Cyrillic (old)", CS_MAC_CYRILLIC_OLD, false },
    { "Mac-Cyrillic-old", CS_MAC_CYRILLIC_OLD, false },
    { "MacCyrillic-old", CS_MAC_CYRILLIC_OLD, false },
    { "Mac Ukraine", CS_MAC_UKRAINE, true },
    { "Mac-Ukraine", CS_MAC_UKRAINE, false },
    { "MacUkraine", CS_MAC_UKRAINE, false },
    { "Mac VT100", CS_MAC_VT100, true },
    { "Mac-VT100", CS_MAC_VT100, false },
    { "MacVT100", CS_MAC_VT100, false },
    { "Mac VT100 (old)", CS_MAC_VT100_OLD, false },
    { "Mac-VT100-old", CS_MAC_VT100_OLD, false },
    { "MacVT100-old", CS_MAC_VT100_OLD, false },
    { "Mac Roman (Pirard encoding)", CS_MAC_PIRARD, false },
    { "Mac Pirard", CS_MAC_PIRARD, false },
    { "Mac-Pirard", CS_MAC_PIRARD, false },
    { "MacPirard", CS_MAC_PIRARD, false },
    { "VISCII", CS_VISCII, true },
    { "HP ROMAN8", CS_HP_ROMAN8, true },
    { "HP-ROMAN8", CS_HP_ROMAN8, false },
    { "DEC MCS", CS_DEC_MCS, true },
    { "DEC-MCS", CS_DEC_MCS, true },
    { "DEC graphics", CS_DEC_GRAPHICS, true },
    { "DEC-graphics", CS_DEC_GRAPHICS, false },
    { "DECgraphics", CS_DEC_GRAPHICS, false },
    { "UTF-8", CS_UTF8, true },
    { "UTF8", CS_UTF8, false },
    { "UTF-7", CS_UTF7, true },
    { "UTF7", CS_UTF7, false },
    { "UTF-7-conservative", CS_UTF7_CONSERVATIVE, false },
    { "EUC-CN", CS_EUC_CN, true },
    { "EUC-KR", CS_EUC_KR, true },
    { "EUC-JP", CS_EUC_JP, true },
    { "EUC-TW", CS_EUC_TW, true },
    { "ISO-2022-JP", CS_ISO2022_JP, true },
    { "ISO-2022-KR", CS_ISO2022_KR, true },
    { "Big5", CS_BIG5, true },
    { "Shift-JIS", CS_SHIFT_JIS, true },
    { "HZ", CS_HZ, true },
    { "UTF-16BE", CS_UTF16BE, true },
    { "UTF16BE", CS_UTF16BE, false },
    { "UTF-16LE", CS_UTF16LE, true },
    { "UTF16LE", CS_UTF16LE, false },
    { "UTF-16BE-NO-BOM", CS_UTF16BE_NO_BOM, true },
    { "UTF-16BE-NOBOM", CS_UTF16BE_NO_BOM, false },
    { "UTF16BENOBOM", CS_UTF16BE_NO_BOM, false },
    { "UTF-16LE-NO-BOM", CS_UTF16LE_NO_BOM, true },
    { "UTF-16LE-NOBOM", CS_UTF16LE_NO_BOM, false },
    { "UTF16LENOBOM", CS_UTF16LE_NO_BOM, false },
    { "UTF-16", CS_UTF16, true },
    { "UTF16", CS_UTF16, false },
    { "CP949", CS_CP949, true },
    { "PDFDocEncoding", CS_PDF, true },
    { "StandardEncoding", CS_PSSTD, true },
    { "COMPOUND_TEXT", CS_CTEXT, true },
    { "COMPOUND-TEXT", CS_CTEXT, false },
    { "COMPOUND TEXT", CS_CTEXT, false },
    { "COMPOUNDTEXT", CS_CTEXT, false },
    { "CTEXT", CS_CTEXT, false },
    { "ISO-2022", CS_ISO2022, true },
    { "ISO2022", CS_ISO2022, false },
    { "ISO-6937", CS_ISO6937, true },
    { "ISO6937", CS_ISO6937, false },
    { "ISO-6937 with euro sign", CS_ISO6937_EURO, true },
    { "ISO-6937-euro", CS_ISO6937_EURO, false },
    { "ISO6937-euro", CS_ISO6937_EURO, false },
    { "ITS", CS_ITS, true },
    { "SAIL", CS_SAIL, true },
    { "WAITS", CS_SAIL, false },
};

const char *charset_to_localenc(int charset)
{
    int i;

    for (i = 0; i < (int)lenof(localencs); i++)
	if (charset == localencs[i].charset)
	    return localencs[i].name;

    return NULL;		       /* not found */
}

int charset_from_localenc(const char *name)
{
    int i;

    if ( (i = charset_from_mimeenc(name)) != CS_NONE)
	return i;
    if ( (i = charset_from_xenc(name)) != CS_NONE)
	return i;
    if ( (i = charset_from_emacsenc(name)) != CS_NONE)
	return i;

    for (i = 0; i < (int)lenof(localencs); i++) {
	const char *p, *q;
	p = name;
	q = localencs[i].name;
	while (*p || *q) {
		if (tolower((unsigned char)*p) != tolower((unsigned char)*q))
		break;
	    p++; q++;
	}
	if (!*p && !*q)
	    return localencs[i].charset;
    }

    return CS_NONE;		       /* not found */
}

int charset_localenc_nth(int n)
{
    int i;

    for (i = 0; i < (int)lenof(localencs); i++)
	if (localencs[i].return_in_enum && !n--)
	    return localencs[i].charset;

    return CS_NONE;		       /* end of list */
}
