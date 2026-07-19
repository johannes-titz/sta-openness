# Figure font

The analysis exports the manuscript's PDF and EPS figures with Charis SIL
6.200. A recent *Journal of Research in Personality* production article uses
Charis SIL for body text, making it a journal-matched—but not journal-required—
choice for the figure typography.

`CharisSIL-Regular.ttf` was downloaded from SIL's official previous-versions
page:

<https://software.sil.org/charis/download/previous-versions/>

The font is distributed under the SIL Open Font License; see `OFL.txt`.
The SHA-256 checksum of the original `CharisSIL-6.200.zip` archive is:

`4b09aa75760b8aa697b762c34afb995dde0754c8f09256cb912dbfc478c97ade`

`fonts.conf` lets R/Cairo resolve the vendored font without a system-wide
installation. `README.qmd` checks that resolution before rendering.
