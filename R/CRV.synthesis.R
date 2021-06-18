#8.Afișarea unei “fișe de sinteză” care să conțină informații de bază despre respectiva
#repartiție(cu precizarea sursei informației!). Relevant aici ar fi să precizați pentru ce e
#folosită ȋn mod uzual acea repartiție, semnificația parametrilor, media, dispersia etc.
require(magick)
library(magick)

CRV.synthesis <- function() {
    cat("\nRegasim urmatoarele repartitii\n1.Uniforma\n2.Exponentiala\n3.Normala\n4.Hi Patrat\n5.Student/T\n6.Surse\n0.Iesire\nAlege una dintre Repartitii: ")
    text <- readline()
    result = switch(
      text,
      "1" = "uniforma.png",
      "2" = 'exponentiala.png',
      "3" = 'normala.png',
      "4" = 'hi-patrat.png',
      "5" = 'student.png',
      "6" = cat("Am folosit urmatoarele surse pentru aceste informatii:\n
              http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf\n
              http://software.ucv.ro/~cstoica/MSSC/lab%204%20distributii.pdf\n
              http://sorana.academicdirect.ro/pages/doc/MV2012/MVRom04.pdf\n
              https://www.afahc.ro/ro/facultate/cursuri/luculescu/4.%20Functii%20de%20repartitie.pdf\n
              http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n
              http://images.wikia.com/nccmn/ro/images/3/37/Capitolul_10_REPARTITII_CLASICE.pdf\n
              https://analizamatematicampt.files.wordpress.com/2010/09/cap7pdf.pdf\n
              https://www.info.umfcluj.ro/index.php?option=com_k2&view=item&task=download&id=4076_836d9dea9d3c276b0af1baf89b2db214&Itemid=1005&lang=ro\n")

    )
    if(text == "1" | text == "2" | text == "3" | text == "4" | text == "5")
    {
      img1 <- magick::image_read(paste(result, sep = ""))
      plot(img1)
    }
}
