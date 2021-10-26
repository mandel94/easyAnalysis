#
# shiny::testServer(addFilesServer, args = list(add_file_counter = reactiveVal()), {
#   session$setInputs(csv_file = "0201_01_02_attitudini_etichette.csv",
#                     xml_file = "0201_01_02_attitudini.xml",
#                     add_file = 1)
#   expect_condition(is.data.frame(addFilesOutput$DF))
#   expect_condition(ncol(addFilesOutput$DF) == length(addFilesOutput$vars))
# })
#
#
