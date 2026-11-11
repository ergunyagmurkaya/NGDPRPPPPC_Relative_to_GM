
# Assigning one color to each country according the colors and the weights of the colors in their flags.
# Saves it as a rds file.

# Colors are derived from an AI-Tool. Not Checked!!)

## Weighted Palette Script Featuring the Actual Color Ratios of 222 Countries

library(dplyr)
library(grDevices)  # Basic R library for color conversions

# 1. Database of 222 Countries in Alphabetical Order by ISO-3 Codes
flag_raw_data <- data.frame(
  country = c(
    "Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "Andorra", "United Arab Emirates", "Argentina", "Armenia", "American Samoa",
    "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria",
    "Bahrain", "Bahamas", "Bosnia and Herzegovina", "Belarus", "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei",
    "Bhutan", "Botswana", "Central African Republic", "Canada", "Switzerland", "Chile", "China", "Cote d'Ivoire", "Cameroon", "Democratic Republic of the Congo",
    "Congo (Congo-Brazzaville)", "Cook Islands", "Colombia", "Comoros", "Cabo Verde", "Costa Rica", "Cuba", "Cayman Islands", "Cyprus", "Czechia",
    "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", "Algeria", "Ecuador", "Egypt", "Eritrea", "Spain",
    "Estonia", "Ethiopia", "Finland", "Fiji", "France", "Micronesia", "Gabon", "United Kingdom", "Georgia", "Ghana",
    "Gibraltar", "Guinea", "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Greece", "Grenada", "Greenland", "Guatemala", "Guam",
    "Guyana", "Hong Kong", "Honduras", "Croatia", "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran",
    "Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", "Kyrgyzstan",
    "Cambodia", "Kiribati", "Saint Kitts and Nevis", "South Korea", "Kuwait", "Laos", "Lebanon", "Liberia", "Libya", "Saint Lucia",
    "Liechtenstein", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", "Macao", "Morocco", "Monaco", "Moldova",
    "Madagascar", "Maldives", "Mexico", "Marshall Islands", "North Macedonia", "Mali", "Malta", "Myanmar", "Montenegro", "Mongolia",
    "Northern Mariana Islands", "Mozambique", "Mauritania", "Montserrat", "Mauritius", "Malawi", "Malaysia", "Namibia", "New Caledonia", "Niger",
    "Nigeria", "Nicaragua", "Niue", "Netherlands", "Norway", "Nepal", "Nauru", "New Zealand", "Oman", "Pakistan",
    "Panama", "Pitcairn Islands", "Peru", "Philippines", "Palau", "Papua New Guinea", "Poland", "Puerto Rico", "North Korea", "Portugal",
    "Paraguay", "Palestine", "French Polynesia", "Qatar", "Romania", "Russia", "Rwanda", "Saudi Arabia", "Sudan", "Senegal",
    "Singapore", "South Georgia", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", "Somalia", "Saint Pierre and Miquelon", "Serbia", "South Sudan",
    "Sao Tome and Principe", "Suriname", "Slovakia", "Slovenia", "Sweden", "Eswatini", "Sint Maarten", "Seychelles", "Syria", "Turks and Caicos Islands",
    "Chad", "Togo", "Thailand", "Tajikistan", "Tokelau", "Turkmenistan", "Timor-Leste", "Tonga", "Trinidad and Tobago", "Tunisia",
    "Turkiye", "Tuvalu", "Taiwan", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", "Uzbekistan", "Holy See",
    "Saint Vincent and the Grenadines", "Venezuela", "British Virgin Islands", "US Virgin Islands", "Vietnam", "Vanuatu", "Wallis and Futuna", "Samoa", "Yemen", "South Africa",
    "Zambia", "Zimbabwe"
  ),
  iso_alpha2 = c(
    "AW", "AF", "AO", "AI", "AL", "AD", "AE", "AR", "AM", "AS", "AG", "AU", "AT", "AZ", "BI", "BE", "BJ", "BF", "BD", "BG",
    "BH", "BS", "BA", "BY", "BI", "BM", "BO", "BR", "BB", "BN", "BT", "BW", "CF", "CA", "CH", "CL", "CN", "CI", "CM", "CD",
    "CG", "CK", "CO", "KM", "CV", "CR", "CU", "KY", "CY", "CZE", "DE", "DJ", "DM", "DK", "DO", "DZ", "EC", "EG", "ER", "ES",
    "EE", "ET", "FIN", "FJ", "FR", "FM", "GA", "GB", "GE", "GH", "GI", "GN", "GM", "GW", "GQ", "GR", "GD", "GL", "GT", "GU",
    "GY", "HK", "HN", "HR", "HT", "HU", "ID", "IN", "IE", "IR", "IQ", "IS", "IL", "IT", "JM", "JOR", "JP", "KZ", "KE", "KG",
    "KH", "KI", "KN", "KR", "KW", "LA", "LB", "LR", "LY", "LC", "LI", "LK", "LS", "LT", "LU", "LV", "MO", "MA", "MC", "MD",
    "MG", "MV", "MX", "MH", "MK", "ML", "MT", "MM", "ME", "MN", "MP", "MZ", "MR", "MS", "MU", "MW", "MY", "NA", "NC", "NE",
    "NG", "NI", "NU", "NL", "NO", "NP", "NR", "NZ", "OM", "PK", "PA", "PN", "PER", "PH", "PW", "PNG", "PL", "PR", "KP", "PT",
    "PY", "PSE", "PF", "QA", "RO", "RU", "RW", "SA", "SD", "SN", "SG", "GS", "SB", "SL", "SV", "SMR", "SOM", "PM", "RS", "SSD",
    "ST", "SR", "SVK", "SI", "SWE", "SZ", "SX", "SC", "SY", "TC", "TD", "TG", "TH", "TJ", "TK", "TKM", "TLS", "TON", "TTO", "TUN",
    "TR", "TUV", "TW", "TZA", "UG", "UA", "URY", "US", "UZ", "VA", "VC", "VEN", "VG", "VI", "VNM", "VUT", "WF", "WS", "YE", "ZA",
    "ZMB", "ZW"
  ),
  iso_alpha3 = c(
    "ABW", "AFG", "AGO", "AIA", "ALB", "AND", "ARE", "ARG", "ARM", "ASM", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR",
    "BHR", "BHS", "BIH", "BLR", "BLZ", "BMU", "BOL", "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COD",
    "COG", "COK", "COL", "COM", "CPV", "CRI", "CUB", "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP",
    "EST", "ETH", "FIN", "FJI", "FRA", "FSM", "GAB", "GBR", "GEO", "GHA", "GIB", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRL", "GTM", "GUM",
    "GUY", "HKG", "HND", "HRV", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ",
    "KHM", "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA", "MAC", "MAR", "MCO", "MDA",
    "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP", "MOZ", "MRT", "MSR", "MUS", "MWI", "MYS", "NAM", "NCL", "NER",
    "NGA", "NIC", "NIU", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN", "PCN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", "PRK", "PRT",
    "PRY", "PSE", "PYF", "QAT", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SGS", "SLB", "SLE", "SLV", "SMR", "SOM", "SPM", "SRB", "SSD",
    "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SXM", "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKL", "TKM", "TLS", "TON", "TTO", "TUN",
    "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VAT", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "WLF", "WSM", "YEM", "ZAF",
    "ZMB", "ZWE"
  )
) %>% arrange(iso_alpha3) # Ensure alphabetical ordering

# 2. List of True Color Ratios Named According to ISO 3 Codes (Derived from AI - NOT CHECKED!!)
colors_list <- list(
  ABW = c("#0021A5"=0.70, "#FFD100"=0.25, "#E30A17"=0.03, "#FFFFFF"=0.02),
  AFG = c("#000000"=0.33, "#D32011"=0.33, "#006233"=0.34),
  AGO = c("#CE1126"=0.48, "#000000"=0.48, "#F7CE17"=0.04),
  AIA = c("#00247D"=0.55, "#FFFFFF"=0.35, "#FF0000"=0.08, "#00A6EF"=0.02),
  ALB = c("#E41E26"=0.96, "#000000"=0.04),
  AND = c("#0018A8"=0.33, "#FFD700"=0.34, "#CE1126"=0.33),
  ARE = c("#FF0000"=0.25, "#007A3D"=0.25, "#FFFFFF"=0.25, "#000000"=0.25),
  ARG = c("#74ACDF"=0.64, "#FFFFFF"=0.34, "#F6B426"=0.02),
  ARM = c("#D9241E"=0.33, "#0033A0"=0.33, "#F2A801"=0.34),
  ASM = c("#002060"=0.50, "#FFFFFF"=0.30, "#D21034"=0.15, "#FFC72C"=0.05),
  ATG = c("#FF0000"=0.45, "#000000"=0.25, "#FFFFFF"=0.15, "#0072C6"=0.10, "#FCD116"=0.05),
  AUS = c("#000033"=0.80, "#FFFFFF"=0.12, "#CC0033"=0.08),
  AUT = c("#ED2939"=0.66, "#FFFFFF"=0.34),
  AZE = c("#00BFFF"=0.32, "#CC0033"=0.32, "#009A44"=0.32, "#FFFFFF"=0.04),
  BDI = c("#CE1126"=0.45, "#FFFFFF"=0.30, "#009B77"=0.25),
  BEL = c("#000000"=0.33, "#FFD100"=0.33, "#CE1126"=0.34),
  BEN = c("#008751"=0.40, "#FCD116"=0.30, "#E30A17"=0.30),
  BFA = c("#FFD100"=0.48, "#CE1126"=0.48, "#009E49"=0.04),
  BGD = c("#006A4E"=0.87, "#F42A41"=0.13),
  BGR = c("#FFFFFF"=0.33, "#00966E"=0.33, "#D00C27"=0.34),
  BHR = c("#CE1126"=0.75, "#FFFFFF"=0.25),
  BHS = c("#00778B"=0.66, "#FCD116"=0.34),
  BIH = c("#002395"=0.75, "#FCD116"=0.18, "#FFFFFF"=0.07),
  BLR = c("#C8102E"=0.60, "#009A44"=0.30, "#FFFFFF"=0.10),
  BLZ = c("#003F87"=0.85, "#D21034"=0.10, "#FFFFFF"=0.05),
  BMU = c("#CE1126"=0.60, "#FFFFFF"=0.25, "#002060"=0.12, "#009A44"=0.03),
  BOL = c("#CE1126"=0.33, "#FCD116"=0.33, "#007A33"=0.34),
  BRA = c("#009C3B"=0.70, "#FCD116"=0.22, "#002171"=0.06, "#FFFFFF"=0.02),
  BRB = c("#002060"=0.66, "#FFC72C"=0.34),
  BRN = c("#F7E017"=0.62, "#FFFFFF"=0.18, "#000000"=0.15, "#CD1626"=0.05),
  BTN = c("#FF6600"=0.48, "#FFCC00"=0.48, "#FFFFFF"=0.04),
  BWA = c("#00A8B5"=0.72, "#FFFFFF"=0.16, "#000000"=0.12),
  CAF = c("#002B7F"=0.24, "#FFFFFF"=0.24, "#009739"=0.24, "#FFD100"=0.24, "#D21034"=0.04),
  CAN = c("#FF0000"=0.55, "#FFFFFF"=0.45),
  CHE = c("#FF0000"=0.80, "#FFFFFF"=0.20),
  CHL = c("#FFFFFF"=0.38, "#0039A6"=0.12, "#CE1126"=0.50),
  CHN = c("#EE1C25"=0.97, "#FFFF00"=0.03),
  CIV = c("#F77F00"=0.33, "#FFFFFF"=0.33, "#009E49"=0.34),
  CMR = c("#007A5E"=0.32, "#E30A17"=0.32, "#FFD100"=0.32, "#FFFF00"=0.04),
  COD = c("#007FFF"=0.70, "#FFD100"=0.20, "#CE1126"=0.10),
  COG = c("#009543"=0.40, "#FFFF00"=0.20, "#CE1126"=0.40),
  COK = c("#00247D"=0.60, "#FFFFFF"=0.25, "#CC0033"=0.15),
  COL = c("#FCD116"=0.50, "#003893"=0.25, "#CE1126"=0.25),
  COM = c("#FFFF00"=0.23, "#FFFFFF"=0.23, "#FF0000"=0.23, "#00A6EF"=0.23, "#009543"=0.08),
  CPV = c("#003893"=0.72, "#FFFFFF"=0.18, "#FCD116"=0.05, "#E30A17"=0.05),
  CRI = c("#002B7F"=0.28, "#FFFFFF"=0.28, "#CE1126"=0.44),
  CUB = c("#D12421"=0.45, "#FFFFFF"=0.30, "#002B7F"=0.25),
  CYM = c("#002060"=0.60, "#FFFFFF"=0.25, "#D21034"=0.15),
  CYP = c("#FFFFFF"=0.82, "#D47312"=0.12, "#436039"=0.06),
  CZE = c("#FFFFFF"=0.50, "#D7141A"=0.50, "#11457E"=0.25),
  DEU = c("#000000"=0.33, "#FF0000"=0.33, "#FFCE00"=0.34),
  DJI = c("#00A6EF"=0.44, "#009A44"=0.44, "#FFFFFF"=0.10, "#E30A17"=0.02),
  DMA = c("#009A44"=0.70, "#FFFF00"=0.08, "#000000"=0.08, "#FFFFFF"=0.08, "#E30A17"=0.06),
  DNK = c("#C60C30"=0.86, "#FFFFFF"=0.14),
  DOM = c("#002B7F"=0.46, "#CE1126"=0.46, "#FFFFFF"=0.08),
  DZA = c("#006233"=0.47, "#FFFFFF"=0.47, "#D32011"=0.06),
  ECU = c("#FFDD00"=0.50, "#003893"=0.25, "#ED2939"=0.25),
  EGY = c("#CE1126"=0.33, "#FFFFFF"=0.33, "#000000"=0.33, "#C5A059"=0.01),
  ERI = c("#E30A17"=0.38, "#00A35B"=0.32, "#0072CE"=0.24, "#FFFF00"=0.06),
  ESP = c("#C60B1E"=0.50, "#FFC400"=0.50),
  EST = c("#242528"=0.33, "#0072CE"=0.33, "#FFFFFF"=0.34),
  ETH = c("#009A44"=0.32, "#FFFF00"=0.32, "#D21034"=0.32, "#0038A8"=0.04),
  FIN = c("#FFFFFF"=0.75, "#003580"=0.25),
  FJI = c("#69B3E7"=0.80, "#FFFFFF"=0.09, "#CC0033"=0.09, "#00247D"=0.02),
  FRA = c("#0055A4"=0.33, "#FFFFFF"=0.33, "#EF4135"=0.34),
  FSM = c("#00AEC7"=0.96, "#FFFFFF"=0.04),
  GAB = c("#009E49"=0.33, "#FFFF00"=0.33, "#0038A8"=0.34),
  GBR = c("#00247D"=0.64, "#FFFFFF"=0.24, "#CC0033"=0.12),
  GEO = c("#FFFFFF"=0.80, "#FF0000"=0.20),
  GHA = c("#FFD100"=0.32, "#006B3F"=0.32, "#CE1126"=0.32, "#000000"=0.04),
  GIB = c("#FFFFFF"=0.65, "#CE1126"=0.30, "#FFD100"=0.05),
  GIN = c("#CE1126"=0.33, "#FFFF00"=0.33, "#009B77"=0.34),
  GMB = c("#FF0000"=0.38, "#000000"=0.24, "#009E49"=0.24, "#FFFFFF"=0.14),
  GNB = c("#CE1126"=0.35, "#FFFF00"=0.32, "#009E49"=0.30, "#000000"=0.03),
  GNQ = c("#00A651"=0.38, "#FFFFFF"=0.24, "#0073CF"=0.24, "#E30A17"=0.14),
  GRC = c("#003E7E"=0.60, "#FFFFFF"=0.40),
  GRD = c("#007A3D"=0.42, "#CE1126"=0.42, "#FFFF00"=0.16),
  GRL = c("#FFFFFF"=0.50, "#D00C27"=0.50),
  GTM = c("#4997D0"=0.66, "#FFFFFF"=0.34),
  GUM = c("#002060"=0.85, "#CE1126"=0.10, "#FFFFFF"=0.05),
  GUY = c("#009E49"=0.55, "#FFFF00"=0.25, "#000000"=0.12, "#D21034"=0.08),
  HKG = c("#EE1C25"=0.94, "#FFFFFF"=0.06),
  HND = c("#0073CF"=0.66, "#FFFFFF"=0.34),
  HRV = c("#FF0000"=0.31, "#FFFFFF"=0.31, "#002B7F"=0.31, "#D21034"=0.07),
  HTI = c("#002060"=0.49, "#D21034"=0.49, "#FFFFFF"=0.02),
  HUN = c("#CE1126"=0.33, "#FFFFFF"=0.33, "#436F4D"=0.34),
  IDN = c("#FF0000"=0.50, "#FFFFFF"=0.50),
  IND = c("#FF671F"=0.33, "#FFFFFF"=0.33, "#008000"=0.33, "#000080"=0.01),
  IRL = c("#009A44"=0.33, "#FFFFFF"=0.33, "#FF7900"=0.34),
  IRN = c("#008543"=0.33, "#FFFFFF"=0.33, "#DA0000"=0.34),
  IRQ = c("#FF0000"=0.33, "#FFFFFF"=0.33, "#000000"=0.33, "#007A3D"=0.01),
  ISL = c("#003897"=0.70, "#FFFFFF"=0.18, "#D21034"=0.12),
  ISR = c("#FFFFFF"=0.70, "#0038A8"=0.30),
  ITA = c("#009246"=0.33, "#FFFFFF"=0.33, "#CE2B37"=0.34),
  JAM = c("#000000"=0.48, "#008751"=0.40, "#FFD100"=0.12),
  JOR = c("#000000"=0.28, "#FFFFFF"=0.28, "#007A3D"=0.28, "#CE1126"=0.16),
  JPN = c("#FFFFFF"=0.85, "#BC002D"=0.15),
  KAZ = c("#00AEC7"=0.92, "#FFFF00"=0.08),
  KEN = c("#000000"=0.30, "#009A44"=0.30, "#BB0000"=0.30, "#FFFFFF"=0.10),
  KGZ = c("#E41E26"=0.94, "#FFFF00"=0.06),
  KHM = c("#002B7F"=0.50, "#CE1126"=0.40, "#ECEFF1"=0.10),
  KIR = c("#CE1126"=0.65, "#002B7F"=0.25, "#FFCC00"=0.10),
  KNA = c("#007A3D"=0.38, "#CE1126"=0.38, "#000000"=0.12, "#FCD116"=0.12),
  KOR = c("#FFFFFF"=0.74, "#003472"=0.14, "#C60C30"=0.12),
  KWT = c("#007A3D"=0.28, "#FFFFFF"=0.28, "#CE1126"=0.28, "#000000"=0.16),
  LAO = c("#002B7F"=0.50, "#CE1126"=0.34, "#FFFFFF"=0.16),
  LBN = c("#CE1126"=0.44, "#FFFFFF"=0.44, "#007A3D"=0.12),
  LBR = c("#B22234"=0.50, "#FFFFFF"=0.36, "#002060"=0.14),
  LBY = c("#000000"=0.46, "#FF0000"=0.24, "#007A3D"=0.24, "#FFFFFF"=0.06),
  LCA = c("#002B7F"=0.76, "#FCD116"=0.14, "#000000"=0.06, "#FFFFFF"=0.04),
  LIE = c("#002B7F"=0.49, "#CE1126"=0.49, "#FDBB30"=0.02),
  LKA = c("#FF9933"=0.55, "#800000"=0.25, "#007A3D"=0.15, "#FFFF00"=0.05),
  LSO = c("#002060"=0.38, "#FFFFFF"=0.38, "#009A44"=0.24),
  LTU = c("#FDBB30"=0.33, "#00B140"=0.33, "#C1272D"=0.34),
  LUX = c("#00A3E0"=0.33, "#FFFFFF"=0.33, "#EA1622"=0.34),
  LVA = c("#9E3039"=0.80, "#FFFFFF"=0.20),
  MAC = c("#00785E"=0.90, "#FFFFFF"=0.08, "#FCD116"=0.02),
  MAR = c("#C1272D"=0.96, "#006233"=0.04),
  MCO = c("#E21C21"=0.99, "#FFFFFF"=0.01),
  MDA = c("#002B7F"=0.33, "#FCD116"=0.33, "#CC0033"=0.34),
  MDG = c("#FFFFFF"=0.33, "#E41D2D"=0.33, "#007E3A"=0.34),
  MDV = c("#D21034"=0.64, "#007A3D"=0.26, "#FFFFFF"=0.10),
  MEX = c("#006847"=0.33, "#FFFFFF"=0.33, "#CE1126"=0.33, "#B38F4B"=0.01),
  MHL = c("#0038A8"=0.85, "#FFFFFF"=0.08, "#FCD116"=0.07),
  MKD = c("#D00C27"=0.70, "#FCD116"=0.30),
  MLI = c("#006B3F"=0.33, "#FFFF00"=0.33, "#CE1126"=0.34),
  MLT = c("#FFFFFF"=0.49, "#CC0033"=0.49, "#002B7F"=0.02),
  MMR = c("#FFFF00"=0.33, "#008543"=0.33, "#FF0000"=0.33, "#FFFFFF"=0.01),
  MNE = c("#C1272D"=0.88, "#FCD116"=0.12),
  MNG = c("#E21C21"=0.66, "#002B7F"=0.31, "#FCD116"=0.03),
  MNP = c("#002A7F"=0.85, "#FFFFFF"=0.10, "#009E49"=0.05),
  MOZ = c("#00933B"=0.28, "#000000"=0.26, "#E41E26"=0.24, "#FFFFFF"=0.14, "#FCD116"=0.08),
  MRT = c("#006233"=0.70, "#FFD100"=0.26, "#FF0000"=0.04),
  MSR = c("#00247D"=0.60, "#FFFFFF"=0.25, "#CC0033"=0.15),
  MUS = c("#FFD100"=0.25, "#002B7F"=0.25, "#006847"=0.25, "#CE1126"=0.25),
  MWI = c("#000000"=0.33, "#CE1126"=0.33, "#007A3D"=0.34),
  MYS = c("#B22234"=0.46, "#FFFFFF"=0.38, "#000066"=0.14, "#FFCC00"=0.02),
  NAM = c("#003580"=0.38, "#009A44"=0.38, "#E41E26"=0.12, "#FFFFFF"=0.10, "#FFCC00"=0.02),
  NCL = c("#0038A8"=0.30, "#FFD100"=0.30, "#CD1626"=0.30, "#009E49"=0.10),
  NER = c("#E25822"=0.34, "#FFFFFF"=0.33, "#008751"=0.33),
  NGA = c("#008751"=0.66, "#FFFFFF"=0.34),
  NIC = c("#0038A8"=0.64, "#FFFFFF"=0.34, "#E25822"=0.02),
  NIU = c("#00247D"=0.50, "#FFD100"=0.35, "#CC0033"=0.15),
  NLD = c("#003188"=0.33, "#FFFFFF"=0.33, "#C8102E"=0.34),
  NOR = c("#EF2B2D"=0.70, "#002868"=0.20, "#FFFFFF"=0.10),
  NPL = c("#DC143C"=0.60, "#003188"=0.32, "#FFFFFF"=0.08),
  NRU = c("#002B7F"=0.88, "#FFFF00"=0.08, "#FFFFFF"=0.04),
  NZL = c("#00247D"=0.82, "#FFFFFF"=0.10, "#CC0033"=0.08),
  OMN = c("#FF0000"=0.65, "#00401A"=0.20, "#FFFFFF"=0.15),
  PAK = c("#00401A"=0.73, "#FFFFFF"=0.27),
  PAN = c("#FFFFFF"=0.44, "#0052B4"=0.28, "#D51E2D"=0.28),
  PCN = c("#00247D"=0.60, "#FFD100"=0.25, "#009A44"=0.15),
  PER = c("#E41E26"=0.66, "#FFFFFF"=0.34),
  PHL = c("#0038A8"=0.40, "#CE1126"=0.40, "#FFFFFF"=0.15, "#FCD116"=0.05),
  PLW = c("#4A90E2"=0.86, "#FCD116"=0.14),
  PNG = c("#000000"=0.46, "#CE1126"=0.46, "#FCD116"=0.08),
  POL = c("#FFFFFF"=0.50, "#DC143C"=0.50),
  PRI = c("#0038A8"=0.45, "#D21034"=0.35, "#FFFFFF"=0.20),
  PRK = c("#002395"=0.68, "#D00C27"=0.20, "#FFFFFF"=0.12),
  PRT = c("#FF0000"=0.60, "#006622"=0.40),
  PRY = c("#0038A8"=0.33, "#FFFFFF"=0.33, "#D21034"=0.34),
  PSE = c("#000000"=0.28, "#FFFFFF"=0.28, "#007A3D"=0.28, "#CE1126"=0.16),
  PYF = c("#FFFFFF"=0.50, "#D21034"=0.45, "#FCD116"=0.05),
  QAT = c("#990033"=0.75, "#FFFFFF"=0.25),
  ROU = c("#002B7F"=0.33, "#FCD116"=0.33, "#CE1126"=0.34),
  RUS = c("#FFFFFF"=0.33, "#0039A6"=0.33, "#D52B1E"=0.34),
  RWA = c("#00A1DE"=0.50, "#FCD116"=0.25, "#007A3D"=0.25),
  SAU = c("#006C35"=0.90, "#FFFFFF"=0.10),
  SDN = c("#FF0000"=0.28, "#FFFFFF"=0.28, "#000000"=0.28, "#007A3D"=0.16),
  SEN = c("#008543"=0.33, "#FCD116"=0.33, "#E41E26"=0.34),
  SGP = c("#E41E26"=0.50, "#FFFFFF"=0.50),
  SGS = c("#00247D"=0.60, "#FFFFFF"=0.25, "#CC0033"=0.15),
  SLB = c("#002B7F"=0.90, "#FFFF00"=0.08, "#FFFFFF"=0.02),
  SLE = c("#007A3D"=0.42, "#FFFFFF"=0.42, "#000000"=0.16),
  SLV = c("#0033A0"=0.65, "#FFFFFF"=0.35),
  SMR = c("#FFFFFF"=0.65, "#002B7F"=0.20, "#CE1126"=0.15),
  SOM = c("#4169E1"=0.96, "#FFFFFF"=0.04),
  SPM = c("#002B7F"=0.50, "#FFFFFF"=0.30, "#CE1126"=0.20),
  SRB = c("#C60C30"=0.31, "#003897"=0.31, "#FFFFFF"=0.31, "#FCD116"=0.07),
  SSD = c("#000000"=0.28, "#D21034"=0.28, "#007A3D"=0.28, "#002B7F"=0.16),
  STP = c("#009E49"=0.48, "#FCD116"=0.28, "#E30A17"=0.24),
  SUR = c("#FFFFFF"=0.44, "#007A3D"=0.24, "#D21034"=0.24, "#FFD100"=0.08),
  SVK = c("#FFFFFF"=0.31, "#0033A0"=0.31, "#EE1C25"=0.31, "#FCD116"=0.07),
  SVN = c("#FFFFFF"=0.33, "#002B7F"=0.33, "#EE1C25"=0.34),
  SWE = c("#006AA7"=0.68, "#FECC00"=0.32),
  SWZ = c("#0038A8"=0.42, "#FDBB30"=0.42, "#FFFFFF"=0.10, "#CE1126"=0.06),
  SXM = c("#FFFFFF"=0.50, "#0033A0"=0.30, "#D21034"=0.20),
  SYC = c("#003897"=0.20, "#FCD116"=0.20, "#E30A17"=0.20, "#000000"=0.20, "#009E49"=0.20),
  SYR = c("#FF0000"=0.33, "#FFFFFF"=0.33, "#000000"=0.33, "#007A3D"=0.01),
  TCA = c("#00247D"=0.60, "#FFFFFF"=0.25, "#CC0033"=0.15),
  TCD = c("#00266F"=0.33, "#FFD100"=0.33, "#D21034"=0.34),
  TGO = c("#006A4E"=0.38, "#FFD100"=0.38, "#E2001A"=0.24),
  THA = c("#002480"=0.33, "#FFFFFF"=0.33, "#A51931"=0.34),
  TJK = c("#CE1126"=0.44, "#FFFFFF"=0.34, "#007A3D"=0.22),
  TKL = c("#002B7F"=0.80, "#FFD100"=0.15, "#FFFFFF"=0.05),
  TKM = c("#009B77"=0.82, "#D21034"=0.10, "#FFCC00"=0.08),
  TLS = c("#000000"=0.70, "#DC143C"=0.22, "#FFC400"=0.08),
  TON = c("#ED1C24"=0.78, "#FFFFFF"=0.22),
  TTO = c("#E2001A"=0.75, "#000000"=0.17, "#FFFFFF"=0.08),
  TUN = c("#E2001A"=0.82, "#FFFFFF"=0.18),
  TUR = c("#E30A17"=0.93, "#FFFFFF"=0.07),
  TUV = c("#002B7F"=0.94, "#FFFF00"=0.06),
  TWN = c("#FF0000"=0.75, "#000099"=0.20, "#FFFFFF"=0.05),
  TZA = c("#007749"=0.52, "#000000"=0.20, "#FFFF00"=0.16, "#4A90E2"=0.12),
  UGA = c("#000000"=0.32, "#FFFF00"=0.32, "#DD0000"=0.32, "#FFFFFF"=0.04),
  UKR = c("#005BBB"=0.50, "#FFD700"=0.50),
  URY = c("#FFFFFF"=0.52, "#74ACDF"=0.44, "#FCD116"=0.04),
  USA = c("#0A3161"=0.42, "#B31942"=0.38, "#FFFFFF"=0.20),
  UZB = c("#0038A8"=0.40, "#FFFFFF"=0.30, "#009B77"=0.26, "#CE1126"=0.04),
  VAT = c("#FFFF00"=0.50, "#FFFFFF"=0.50),
  VCT = c("#007A3D"=0.50, "#FCD116"=0.25, "#002395"=0.25),
  VEN = c("#FCD116"=0.33, "#003893"=0.33, "#CE1126"=0.33, "#FFFFFF"=0.01),
  VGB = c("#00247D"=0.60, "#FFFFFF"=0.25, "#CC0033"=0.15),
  VIR = c("#FFFFFF"=0.75, "#FFD100"=0.15, "#002B7F"=0.10),
  VNM = c("#DA251D"=0.95, "#FFFF00"=0.05),
  VUT = c("#007D3A"=0.40, "#CE1126"=0.35, "#FFD700"=0.25),
  WLF = c("#CE1126"=0.60, "#FFFFFF"=0.25, "#002060"=0.15),
  WSM = c("#002B7F"=0.76, "#CE1126"=0.14, "#FFFFFF"=0.10),
  YEM = c("#CE1126"=0.33, "#FFFFFF"=0.33, "#000000"=0.33),
  ZAF = c("#007C59"=0.52, "#D21034"=0.22, "#FFFF00"=0.14, "#000000"=0.12),
  ZMB = c("#007A3D"=0.74, "#FF7900"=0.10, "#000000"=0.08, "#E51636"=0.08),
  ZWE = c("#007A3D"=0.44, "#FFFF00"=0.22, "#CE1126"=0.22, "#000000"=0.10, "#FFFFFF"=0.02)
)

# 3 Matching the List to an Alphabetical Data Frame Based on ISO-3 Keys

structured_palettes <- flag_raw_data %>%
  mutate(colors = colors_list[iso_alpha3]) %>%
  tidyr::unnest_longer(colors, indices_to = "hex", values_to = "weight") %>%
  select(country, iso_alpha2, iso_alpha3, hex, weight)


# 4. Color Mixing Function
# This function mixes hex codes in RGB space based on their weights

mix_flag_colors <- function(hex_vector, weight_vector) {
  # Normalize the values to ensure that the sum of the percentages is 1 (or 100)
  weights <- weight_vector / sum(weight_vector)
  
  # Convert HEX codes to an RGB matrix (values ranging from 0 to 255)
  rgb_matrix <- col2rgb(hex_vector)
  
  # Calculate the weighted average (separately for Red, Green, and Blue)
  mixed_r <- sum(rgb_matrix["red", ] * weights)
  mixed_g <- sum(rgb_matrix["green", ] * weights)
  mixed_b <- sum(rgb_matrix["blue", ] * weights)
  
  # Convert the mixed RGB values back to a single HEX code
  mixed_hex <- rgb(round(mixed_r), round(mixed_g), round(mixed_b), maxColorValue = 255)
  return(mixed_hex)
}

# 5. Calculating the Mix for All Countries
flag_weighted_palette <- structured_palettes %>%
  group_by(country, iso_alpha2, iso_alpha3) %>%
  summarize(
    unified_hex = mix_flag_colors(hex, weight),
    .groups = "drop"
  )

# 6. Saving the New Color Palette as an RDS
saveRDS(flag_weighted_palette, "flag_weighted_palette.rds")

# Test output
cat("OK.\n")
print(head(flag_weighted_palette, 10))
# View(flag_weighted_palette)

# Example Usage:
# How to import the file
# flag_pal <- readRDS("flag_weighted_palette.rds")

# Example 1: Filter for Türkiye and Germany
# tur_deu <- flag_weighted_palette %>% filter(iso_alpha3 %in% c("TUR", "DEU"))

# left_join(weo_data, flag_weighted_palette, by = "iso_alpha3")