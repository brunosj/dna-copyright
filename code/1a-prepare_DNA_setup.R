#--------------------------------------------------------------------
# Us and Them: Mapping Discourse Coalitions in the EU Copyright Debate
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

# 1. initialize JVM 
.jinit()
# 2. retrieve the Java-version
.jcall("java/lang/System", "S", "getProperty", "java.version")
# 3. retrieve JAVA_HOME location
.jcall("java/lang/System", "S", "getProperty", "java.home")
# 4. retrieve Java architecture
.jcall("java/lang/System", "S", "getProperty", "sun.arch.data.model")
# 5. retreive architecture of OS (This should have 64 in it if step 4 displays # "64")
.jcall("java/lang/System", "S", "getProperty", "os.arch")
# 6. retrieve architecture of R as well (This should again have 64 in it if # step 4 and 5 display 64)
R.Version()$arch
# 7. load library
library("rDNA")
# 8. initialise DNA
dna_init()
# open database from rDNA
set.seed(12345)
copyright <- dna_copyrightection("copyrightreform.dna", verbose = FALSE)
# retrieve network matrix
nw_mat <- dna_network(copyright)
# retrieve actors attributes
atA <- dna_getAttributes(copyright,
                         statementType = "DNA Statement",
                         variable = "organization")
# retrieve frames attributes
atF <- dna_getAttributes(copyright,
                         statementType = "DNA Statement",
                         variable = "concept")
