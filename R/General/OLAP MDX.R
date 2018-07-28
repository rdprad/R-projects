library(RSQLite)

con <- COMCreate("ADODB.Connection")
con[["ConnectionString"]] <- paste(
    "Provider=MSOLAP.4",
    "Password=XXXX",
    "Persist Security Info=True",
    "User ID=sispro.local\\XXXX",
    "Data Source=cubos.sispro.gov.co",
    "Initial Catalog=CU_BDUA",
  sep = ";")
con$Open()

Provider=MSOLAP.4;
Integrated Security=SSPI;
Persist Security Info=True;
Initial Catalog=MMRS;
Data Source=MNOYTMIC1;
MDX Compatibility=1;
Safety Options=2;
MDX Missing Member Mode=Error