#' Provide image Lab color space histgram
#'
#' Analysis an image from .jpg or .png file. Draw L(luminance), a(green-magenta) and b(blue-yellow) color space histgram of pixel in ggplot2. Lab color space computed from RGB value.
#' Calculate mean, sd, skewness, and kurtsis for each histgram.
#'
#' @param input Set file, folder or url for image analysis corresponding to mode parameter.
#' @param mode Select a mode in all four modes. Modes are "file"(default), "url", "folder", and "scraping".
#' @param hist Whether histgram draw or not. Dafult is draw. However, you should set FALSE when you want fast computation for images of digital camera and smartphone. Rendering of ggplot2 is so long for these large pixels image.
#' Also this parameter is used for output name when you use url, folder or scraping mode (Default output name is "histgram").
#' @param resize This argument is important to process many image histgram fastly. If you set resize=1/4, the speed of drawing histgram is dramatically up although output values are approximation.
#' Resize is recommended when you use folder mode and want to get many histgram.
#' @param endoff If you want to get rid of image borders extreme value (white or black frame), you set this parameter TRUE.
#'
#' @return image histgram and thier descriptive stastics (Lab color space). Folder and scraping mode provide a pdf file.
#' L range is 0-100. A and b range are -110-110. Transform Lab from RGB are based on ITU-R BT 709, using the D65 white point reference.
#'
#' @export
#'
#' @examples
#' # Simple use is only set input an image file name. Japanese file name is accepted.
#' originaidir <- getwd()  # get current dir
#' setwd(system.file("img", package="imhistR"))  # set analysis dir
#' labhist("Newlogo.png")
#' setwd(originaidir)  # set original dir
#'
#' # labhist("yourfile.jpg")  # you can simply use like this.
#'
#'
#' # Url mode needs to input image URL.
#' # Only URL tail ".jpg" or ".png" can analyze.
#' url <- "http://www.r-project.org/Rlogo.png"
#' labhist(input=url, mode="url", hist="Rlogo", endoff=TRUE)
#'
#'
#' # If you have an image folder in your PC, easily analyze all images by using folder mode.
#' # Althogh the type of these images is limited ".jpg" or ".png", both type files in a folder can analyze one command.
#' labhist(input=setwd(system.file("img", package="imhistR")), mode="folder", hist="Rlogo")
#'
#' # Resize give fast drawing of histgram. Compressed method is kernel.
#' labhist(input="folder name of iphone picture", mode="folder", hist="iphoneImg", resize=1/4)
#'
#'
#' # Web scraping from google image search is conducted by scraping mode. Twenty images were automatically downloaded and analyzed.
#' # So many scraping should avoid in order to conform web manner. If you already scraping image by other function of this package, you should use folder mode.
#' url <- "url from google image search of xxx"  # This package does not provide the way to scraping other web pages
#' labhist(input=url, mode="scraping", hist="xxx")
#'
#'

labhist <- function(input, mode="file", hist="histgram", resize=FALSE, endoff=FALSE) {
  ##### set print
  if((hist!=FALSE && mode=="folder") || (hist!=FALSE && mode=="scraping")) {
    Cairo::CairoPDF(paste0(hist,".pdf"), paper="a4r", width=11.69, height=8.27)
  }
  tryCatch(
    { ##### set input type
      if(grepl(".jpg",  tolower(input), fixed=TRUE))   filetype <- ".jpg"
      if(grepl(".jpeg", tolower(input), fixed=TRUE))   filetype <- ".jpeg"
      if(grepl(".png",  tolower(input), fixed=TRUE))   filetype <- ".png"
      ##### set mode: file, url, dir, scraping
      current <- getwd()
      if(mode=="file") {
        datfil <- input; filNum <- 1
      } else if(mode=="url") {
        downloader::download(input, paste0(current, "/", hist, filetype), mode="wb")
        datfil <- paste0(hist, filetype); filNum <- 1
      } else if(mode=="folder") {
        datfil <- c(dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".jpg"),
                    dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".jpeg"),
                    dir(path=input, full.names=TRUE, ignore.case=TRUE, pattern=".png") )
        filNum <- length(datfil)
      } else if(mode=="scraping") {
        type <- ".jpg"
        if (!file.exists(paste0(current, "/", mode)))  dir.create(mode)     # folder check
        cat(paste0(mode, "folder in ", current, "\n"))  # return message
        webinf <- rvest::html(input)
        imgnod <- rvest::html_nodes(webinf, "img")
        nodtext <- rvest::html_attrs(imgnod)
        for(i in 1:length(nodtext)) {
          downloader::download(as.character(nodtext[[i]][2]),
                               sprintf("%s/%s/%s_%02d%s", current, mode, hist, i, type),
                               mode="wb")
          Sys.sleep(3)
        }
        datfil <- list.files(path=paste0(current, "/", mode), pattern=type)
        filNum <- length(nodtext)
      }

      ##### main loop
      savedat <- matrix(0, nrow = filNum, ncol = 12)
      if(mode=="folder")  title <- gsub(paste0(input,"/"), "", datfil)
      else                title <- datfil
      for(f in 1:filNum) {
        if(mode=="folder" || mode=="scraping") {
          cat(paste0("Processing ", datfil[f], "(", f, "/", filNum, ")", "...\n"))
        }
        ##### read dat
        if(mode=="scraping")  setwd(paste0(current, "/scraping"))
        if(mode=="folder") {
          if(grepl(".jpg",  tolower(datfil[f]), fixed=TRUE))  filetype <- ".jpg"
          if(grepl(".jpeg", tolower(datfil[f]), fixed=TRUE))  filetype <- ".jpeg"
          if(grepl(".png",  tolower(datfil[f]), fixed=TRUE))  filetype <- ".png"
        }
        if(filetype==".jpg" || filetype==".jpeg" || filetype==".JPG" || filetype==".JPEG") {
          img <- jpeg::readJPEG(datfil[f])
        }
        if(filetype==".png" || filetype==".PNG" ) {
          img <- png::readPNG(datfil[f])
        }
        if(mode=="url")  file.remove(datfil[f])
        ##### thumbnail rescale (under 90000 pixel)
        dim1 <- dim(img)[1]; dim2 <- dim(img)[2]
        if(dim1>300 && dim2>300) {
          while(dim1 > 200)  dim1 <- dim1 / 2
          while(dim2 > 200)  dim2 <- dim2 / 2
          resizep <- 1 / min(dim(img)[1] / dim1, dim(img)[2] / dim2)
          redp   <- mmand::rescale(img[,,1], resizep, mmand::mnKernel())
          greenp <- mmand::rescale(img[,,2], resizep, mmand::mnKernel())
          bluep  <- mmand::rescale(img[,,3], resizep, mmand::mnKernel())
          imgp <- array(0, dim = c(dim(redp)[1], dim(redp)[2], 3))
          imgp[,,1] <- redp; imgp[,,2] <- greenp; imgp[,,3] <- bluep;
          imgp <- (imgp - min(imgp)) / (max(imgp) - min(imgp))
        } else {
          imgp <- img
        }

        ##### LAB calculate
        if(resize) {
          img2 <- array(0, dim = c(dim(img)[1]*resize, dim(img)[2]*resize, 3))
          img2[,,1] <- mmand::rescale(img[,,1], resize, mmand::mnKernel())
          img2[,,2] <- mmand::rescale(img[,,2], resize, mmand::mnKernel())
          img2[,,3] <- mmand::rescale(img[,,3], resize, mmand::mnKernel())
          img <- img2
        }
        dat <- array(0, c(nrow(img), ncol(img), 3))
        if(nrow(img) < ncol(img)) {
          for(i in 1:ncol(img))   dat[,i,] <- patchPlot::RGB2Lab(img[,i,1:3])
        } else {
          for(i in 1:nrow(img))   dat[i,,] <- patchPlot::RGB2Lab(img[i,,1:3])
        }
        ##### color settings
        val <- c("L* dimension", "A* dimension", "B* dimension")
        hcol <- c("black", "white", "green", "magenta", "blue", "yellow")
        xrange <- c(100, 220, 220)
        leg <- data.frame(c(0, 50, 100), c(-110, 0, 110), c(-110, 0, 110))

        ##### print settings
        if(mode=="scraping")  setwd(current)
        ##### plot loop
        p <- list(); imgall <- NULL; maxval <- NULL;
        for(i in 1:3) {
          ###### stastics
          imgdat <- tidyr::gather(data.frame(dat[,,i]), pixel, value)
          if(endoff) {
            if(i==1)   imgval <- imgdat$value[(1 < imgdat$value) & (imgdat$value < 99)]
            else       imgval <- c(imgdat$value[imgdat$value < -1], imgdat$value[imgdat$value > 1])
          } else {
            imgval <- imgdat$value
          }
          imgsta <- c(mean(imgval), sd(imgval), e1071::skewness(imgval, type=2), e1071::kurtosis(imgval, type=2))
          imgall <- c(imgall, imgsta); imgval <- data.frame(imgval)
          ##### histgram
          if(hist!=FALSE) {
            g <- ggplot2::ggplot(imgval, ggplot2::aes(x=imgval, fill=..x..)) +
              ggplot2::stat_bin(binwidth = xrange[i]/255) +
              ggplot2::ylab("Number of pixels") + ggplot2::xlab(val[i]) +
              ggplot2::theme_bw(base_size=16) +
              ggplot2::theme(legend.position="bottom", legend.title=ggplot2::element_blank(),
                             plot.title=ggplot2::element_text(size=14)) +
              ggplot2::scale_fill_gradient(low=hcol[2*i-1], high=hcol[2*i],
                                           breaks=leg[,i], labels=leg[,i], space="Lab") +
              ggplot2::ggtitle(paste0("Mean = ",round(imgsta[1],2), ", SD = ",round(imgsta[2],2), "  ",
                                      "\nSkew = ",round(imgsta[3],2), ", Kurt = ",round(imgsta[4],2)))
            if((dim(dat)[1]*dim(dat)[2]) > 1e+06) {
              g <- g + ggplot2::scale_y_continuous(labels=scales::scientific_format())
            } else {
              g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
            }
            p <- c(p, list(g))
          }
        }
        savedat[f,] <- imgall
        ##### multiple plot
        if(hist!=FALSE) {
          grid::grid.newpage()
          grid::pushViewport(
            grid::viewport(layout = grid::grid.layout(2, 3, heights = grid::unit(c(1, 9), "null"))))
          print(p[[1]] + ggplot2::xlim(0, 100),    vp=grid::viewport(layout.pos.row=2, layout.pos.col=1))
          print(p[[2]] + ggplot2::xlim(-110, 110), vp=grid::viewport(layout.pos.row=2, layout.pos.col=2))
          print(p[[3]] + ggplot2::xlim(-110, 110), vp=grid::viewport(layout.pos.row=2, layout.pos.col=3))
          windowsFonts(MEI = windowsFont("Meiryo"))
          grid::grid.text(title[f], vp=grid::viewport(layout.pos.row=1, layout.pos.col=2:3),
                          gp=grid::gpar(fontsize=35), fontfamily="MEI")
          grid::pushViewport(grid::viewport(layout.pos.row=1, layout.pos.col=1, just=c('centre','top')))
          grid::grid.draw(grid::rasterGrob(imgp, interpolate=TRUE))
        }
      }
      ### print end
      colnames(savedat) <- c("Mean_L*dim.", "SD_L*dim.", "Skew_L*dim.", "Kurt_L*dim.",
                             "Mean_A*dim.", "SD_A*dim.", "Skew_A*dim.", "Kurt_A*dim.",
                             "Mean_B*dim.", "SD_B*dim.", "Skew_B*dim.", "Kurt_B*dim.")
      row.names(savedat) <- title
      if((hist!=FALSE && mode=="folder") || (hist!=FALSE && mode=="scraping")) {
        dev.off()
        cat(paste0(hist, ".pdf in ", current, "\n"))  # return message and dat
      }
      return(savedat)
    },
    ##### error processing
    error = function(e) {
      message("!! Error has occurred. Please change some parameters")
      message(e)
      if(hist!=FALSE && mode=="folder" || mode=="scraping")  dev.off()
    },
    silent=TRUE)
}
