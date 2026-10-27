# WUENIC Checks

Run during the draft session for individual countries, at the end of the draft session on all countries, and at the end of the final session for all countries.

| Check | Description |
|-------|-------------|
| 1 | Compare country-vaccine estimates, current year vs previous year |
| 2 | Compare coverage between current and previous revision |
| 3 | Cross-check DTP3, Hib3, HepB3 |
| 4 | Cross-check PCVC, Pol3, RotaC against DTP3 |
| 5 | Cross-check DTP3, MCV1 |
| 6 | Cross-check MCV1, YFV |
| 7b | Cross-check DTP3, IPV1 (OPV-using countries) |
| 7c | Cross-check DTP1, IPV1 (DTP-IPV combination using countries) |
| 7d | Cross-check DTP3, IPVC (DTP-IPV combination using countries) |
| 8 | Cross-check MCV1, MENGA |
| 9 | Compare admin denominators, current year vs previous year |
| 10 | WUENIC estimate is the same, but admin coverage differs for vaccines recommended at the same time (DTP3, PCVC, ROTAC; BCG, HepBB; MCV1, MENGA, YFV) |
| 11 | DTP3 increased and PCVC decreased |
| 12 | Flag where DTP3 > DTP1 |
| 13 | Flag where IPVC > IPV1 |
| 14 | Flag where MCV2 > MCV1 [rev_yr] |
| 15 | RCV1 != MCV1 |
| 16 | MENGA > MCV1 but admin MENGA <= MCV1 |
| 17 | YFV > MCV1 but admin YFV <= MCV1 |
| 18 | Admin coverage is the same, but WUENIC differs for vaccines recommended at the same time (DTP3, PCVC, ROTAC; BCG, HepBB; MCV1, MENGA, YFV) |
| 19 | Flag cases where survey data show discrepancies between co-administered antigens while admin data show similar coverage (within 2pp) |
| 50 | Compare draft and final WUENIC estimates (final only, flag any differences) |
