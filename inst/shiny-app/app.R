library(shiny)
library(shinydashboard)
library(readxl)
library(openxlsx)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(officer)

# ============================================================
# PAUDanalytics v0.2.1 - Longitudinal Observation Analytics
# Unit data: one row = one child x one observation period.
# Main framework: six STPPA-aligned aspects.
# Additional reading: three Foundation Phase CP elements.
# IMPORTANT: operational indicators below are editable examples,
# not a verbatim official indicator list.
# ============================================================

indicator_meta <- tibble::tribble(
  ~kode, ~aspek, ~indikator, ~cp_elemen,
  "NAM1", "Nilai Agama dan Moral", "Mengenal keyakinan/ajaran agama yang dianut", "Nilai Agama dan Budi Pekerti",
  "NAM2", "Nilai Agama dan Moral", "Mempraktikkan kebiasaan ibadah sesuai konteks dan bimbingan", "Nilai Agama dan Budi Pekerti",
  "NAM3", "Nilai Agama dan Moral", "Menunjukkan kasih sayang, kejujuran, tanggung jawab, dan perilaku baik", "Nilai Agama dan Budi Pekerti",
  "NAM4", "Nilai Agama dan Moral", "Menghargai sesama dan lingkungan", "Nilai Agama dan Budi Pekerti",
  "PAN1", "Nilai Pancasila", "Mengenali identitas diri, keluarga, satuan pendidikan, dan Indonesia", "Jati Diri",
  "PAN2", "Nilai Pancasila", "Mengikuti aturan sederhana dalam kehidupan sehari-hari", "Jati Diri",
  "PAN3", "Nilai Pancasila", "Menghargai perbedaan, berbagi, dan bekerja sama", "Jati Diri",
  "PAN4", "Nilai Pancasila", "Menunjukkan kepedulian, kemandirian, dan tanggung jawab", "Jati Diri",
  "FM1", "Fisik Motorik", "Menggunakan gerak motorik kasar secara terkoordinasi", "Jati Diri",
  "FM2", "Fisik Motorik", "Menggunakan gerak motorik halus untuk memanipulasi benda dan alat", "Jati Diri",
  "FM3", "Fisik Motorik", "Menunjukkan kemandirian dalam merawat diri", "Jati Diri",
  "FM4", "Fisik Motorik", "Menerapkan kebiasaan sehat, aman, dan menjaga keselamatan diri", "Jati Diri",
  "KOG1", "Kognitif", "Memecahkan masalah sederhana melalui eksplorasi", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG2", "Kognitif", "Mengenali pola, hubungan, sebab-akibat, persamaan, dan perbedaan", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG3", "Kognitif", "Menggunakan konsep bilangan, bentuk, ukuran, ruang, atau simbol secara bermakna", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG4", "Kognitif", "Mengamati, membandingkan, mengelompokkan, dan membuat simpulan sederhana", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS1", "Bahasa", "Menyimak dan memahami pesan, cerita, atau instruksi sederhana", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS2", "Bahasa", "Menggunakan kosakata dan bahasa lisan untuk berkomunikasi", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS3", "Bahasa", "Mengekspresikan gagasan, pengalaman, kebutuhan, atau pertanyaan", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS4", "Bahasa", "Menunjukkan kesadaran awal terhadap bunyi bahasa, teks, simbol, dan pramembaca", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "SE1", "Sosial-Emosional", "Mengenali diri, kebutuhan, minat, dan perasaan diri", "Jati Diri",
  "SE2", "Sosial-Emosional", "Mengelola emosi dan perilaku dengan dukungan yang sesuai", "Jati Diri",
  "SE3", "Sosial-Emosional", "Berinteraksi, berbagi, dan bekerja sama dengan teman maupun orang dewasa", "Jati Diri",
  "SE4", "Sosial-Emosional", "Menunjukkan empati dan menyelesaikan konflik sederhana dengan dukungan", "Jati Diri"
)

aspect_order <- c("Nilai Agama dan Moral", "Nilai Pancasila", "Fisik Motorik", "Kognitif", "Bahasa", "Sosial-Emosional")
period_order <- c("Awal Semester", "Tengah Semester", "Akhir Semester")

category_code <- function(x, bb=1.49, mb=2.49, bsh=3.49) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= bb ~ "BB",
    x <= mb ~ "MB",
    x <= bsh ~ "BSH",
    TRUE ~ "BSB"
  )
}
category_long <- function(x) dplyr::recode(x,
  "BB"="Belum Berkembang", "MB"="Mulai Berkembang",
  "BSH"="Berkembang Sesuai Harapan", "BSB"="Berkembang Sangat Baik",
  .default=x)

focus_text <- function(aspect) switch(aspect,
  "Nilai Agama dan Moral"="teladan, rutinitas bermakna, cerita reflektif, kepedulian terhadap sesama dan lingkungan",
  "Nilai Pancasila"="berbagi peran, aturan kelas, gotong royong, identitas diri, kepedulian, dan penghargaan terhadap keberagaman",
  "Fisik Motorik"="permainan gerak kasar, motorik halus, kemandirian merawat diri, kebiasaan sehat, dan keselamatan",
  "Kognitif"="pemecahan masalah, pola, bilangan, klasifikasi, eksperimen sederhana, dan eksplorasi sebab-akibat",
  "Bahasa"="percakapan dua arah, membaca nyaring, bercerita, bermain peran, menyimak, dan kegiatan pramembaca",
  "Sosial-Emosional"="pengenalan emosi, strategi regulasi diri, kerja sama, berbagi, negosiasi, dan penyelesaian konflik sederhana",
  "aktivitas bermain yang sesuai tahap perkembangan")

recommendation_text <- function(aspect, status, final_cat) {
  f <- focus_text(aspect)
  if (is.na(status)) return("Observasi belum cukup untuk menghasilkan rekomendasi longitudinal.")
  if (status == "Menurun") return(paste0("Terlihat penurunan pada aspek ini. Lakukan observasi ulang pada beberapa konteks sebelum menyimpulkan perubahan perkembangan. Prioritaskan stimulasi melalui ", f, ", dengan dukungan individual dan dokumentasi anekdot."))
  if (status == "Stagnan") return(paste0("Perkembangan relatif stagnan. Variasikan konteks, tingkat bantuan, dan tantangan melalui ", f, ". Amati apakah kemampuan muncul secara konsisten tanpa bantuan."))
  if (status == "Meningkat") return(paste0("Terjadi kemajuan. Pertahankan strategi yang efektif melalui ", f, ", lalu tingkatkan tantangan secara bertahap agar kemampuan semakin mandiri dan konsisten."))
  if (final_cat == "BSB") return(paste0("Capaian sudah sangat baik. Berikan pengayaan dan pilihan kegiatan yang lebih kompleks melalui ", f, "."))
  paste0("Pertahankan stimulasi perkembangan melalui ", f, ".")
}

make_narrative <- function(child, aspect_change) {
  if (nrow(aspect_change) == 0) return("Data observasi longitudinal belum memadai untuk membuat narasi.")
  nm <- unique(child$nama_anak)[1]
  final_mean <- child %>% filter(periode == "Akhir Semester") %>% summarise(m=mean(skor_aspek, na.rm=TRUE)) %>% pull(m)
  final_cat <- category_code(final_mean)
  inc <- aspect_change %>% filter(status == "Meningkat") %>% arrange(desc(delta)) %>% pull(aspek)
  stag <- aspect_change %>% filter(status == "Stagnan") %>% pull(aspek)
  dec <- aspect_change %>% filter(status == "Menurun") %>% pull(aspek)
  strengths <- if(length(inc)>0) paste(head(inc,2), collapse=" dan ") else "beberapa aspek perkembangan"
  sent1 <- paste0(nm, " menunjukkan perkembangan yang secara umum berada pada kategori ", category_long(final_cat), " pada akhir semester.")
  sent2 <- if(length(inc)>0) paste0(" Kemajuan paling terlihat pada ", strengths, ".") else " Pola perkembangan cenderung stabil sepanjang periode observasi."
  sent3 <- if(length(stag)>0) paste0(" Aspek yang masih relatif stagnan adalah ", paste(stag, collapse=", "), "; guru disarankan memberikan variasi pengalaman bermain, bantuan bertahap, dan observasi ulang dalam konteks yang berbeda.") else " Tidak terdapat aspek yang terdeteksi stagnan berdasarkan batas perubahan yang digunakan."
  sent4 <- if(length(dec)>0) paste0(" Terdapat penurunan pada ", paste(dec, collapse=", "), ". Temuan ini perlu diverifikasi melalui observasi berulang dan tidak sebaiknya ditafsirkan sebagai diagnosis atau kemunduran permanen.") else " Tidak terdapat aspek yang terdeteksi menurun."
  paste0(sent1, sent2, sent3, sent4)
}

# ----- Demo longitudinal data -----
set.seed(20260911)
children <- data.frame(
  id_anak=sprintf("A%03d",1:8),
  nama_anak=c("Aisyah","Bima","Citra","Damar","Elina","Faris","Gita","Hana"),
  kelompok=rep("B", 8), usia_bulan=c(66,64,68,63,65,67,62,69), stringsAsFactors=FALSE
)
template_df <- tidyr::crossing(children, periode=factor(period_order, levels=period_order)) %>% arrange(id_anak, periode)
for (k in indicator_meta$kode) {
  base <- sample(1:3, nrow(children), replace = TRUE)
  vals <- unlist(lapply(base, function(b) {
    mid <- b + sample(c(0, 0, 1), 1)
    end <- b + sample(c(0, 1, 1), 1)
    pmin(4, pmax(1, c(b, mid, end)))
  }))
  template_df[[k]] <- vals
}
template_df$periode <- as.character(template_df$periode)

ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="PAUDanalytics v2"),
  dashboardSidebar(sidebarMenu(
    menuItem("Beranda", tabName="home", icon=icon("home")),
    menuItem("Data Longitudinal", tabName="data", icon=icon("file-excel")),
    menuItem("Perkembangan Kelas", tabName="class", icon=icon("chart-line")),
    menuItem("Profil Anak", tabName="child", icon=icon("child")),
    menuItem("Perubahan Indikator", tabName="indicator", icon=icon("magnifying-glass-chart")),
    menuItem("Narasi & Rekomendasi", tabName="narrative", icon=icon("file-lines")),
    menuItem("Ekspor Laporan", tabName="export", icon=icon("download")),
    menuItem("Pengaturan", tabName="settings", icon=icon("gear"))
  )),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper{background:#f5f7fb}.box{border-radius:10px}.small-box h3{font-size:27px}.note{padding:12px;background:#fff8e8;border-left:4px solid #f39c12;border-radius:6px}.rec{padding:12px;margin-bottom:10px;background:white;border:1px solid #e5e7eb;border-radius:8px}"))),
    tabItems(
      tabItem(tabName="home",
        fluidRow(valueBoxOutput("vb_child"), valueBoxOutput("vb_obs"), valueBoxOutput("vb_progress"), valueBoxOutput("vb_stagnant")),
        fluidRow(
          box(width=8,title="Tren Rata-rata Kelas",status="primary",solidHeader=TRUE,plotOutput("home_trend",height=360)),
          box(width=4,title="Cara Membaca",status="info",solidHeader=TRUE,
              tags$p("Satu anak idealnya memiliki tiga observasi: Awal, Tengah, dan Akhir Semester."),
              tags$p("Aplikasi membaca perubahan skor per aspek dan per indikator."),
              tags$div(class="note",tags$strong("Penting: "),"Status stagnan/menurun adalah sinyal analitik untuk tindak lanjut observasi, bukan diagnosis perkembangan."))
        )),
      tabItem(tabName="data",
        fluidRow(
          box(width=4,title="Input",status="primary",solidHeader=TRUE,
              fileInput("file","Upload Excel / CSV",accept=c(".xlsx",".xls",".csv")),
              downloadButton("download_template","Unduh Template Longitudinal"),
              checkboxInput("use_demo","Gunakan data contoh",TRUE),
              helpText("Kolom wajib: id_anak, nama_anak, kelompok, usia_bulan, periode, dan indikator.")),
          box(width=8,title="Preview Data",status="primary",solidHeader=TRUE,DTOutput("data_table"))),
        fluidRow(box(width=12,title="Kamus Indikator Operasional",status="info",solidHeader=TRUE,DTOutput("indicator_meta")))),
      tabItem(tabName="class",
        fluidRow(
          box(width=5,title="Filter",status="primary",solidHeader=TRUE,selectInput("class_aspect","Aspek",choices=aspect_order,selected="Bahasa")),
          box(width=7,title="Perubahan Rata-rata Kelas",status="primary",solidHeader=TRUE,plotOutput("class_trend",height=320))),
        fluidRow(
          box(width=7,title="Distribusi Kategori Antarperiode",status="info",solidHeader=TRUE,plotOutput("class_categories",height=390)),
          box(width=5,title="Ringkasan Perubahan",status="warning",solidHeader=TRUE,DTOutput("class_change_table")))),
      tabItem(tabName="child",
        fluidRow(
          box(width=3,title="Pilih Anak",status="primary",solidHeader=TRUE,selectInput("child_select","Anak",choices=NULL),uiOutput("child_info")),
          box(width=9,title="Grafik Longitudinal 6 Aspek",status="primary",solidHeader=TRUE,plotOutput("child_trend",height=400))),
        fluidRow(box(width=12,title="Perubahan Awal → Akhir",status="info",solidHeader=TRUE,DTOutput("child_change_table")))),
      tabItem(tabName="indicator",
        fluidRow(
          box(width=4,title="Pilih Anak & Aspek",status="primary",solidHeader=TRUE,
              selectInput("indicator_child","Anak",choices=NULL),
              selectInput("indicator_aspect","Aspek",choices=aspect_order,selected="Bahasa")),
          box(width=8,title="Tren per Indikator",status="primary",solidHeader=TRUE,plotOutput("indicator_trend",height=380))),
        fluidRow(box(width=12,title="Deteksi Indikator Stagnan / Menurun",status="warning",solidHeader=TRUE,DTOutput("indicator_status_table")))),
      tabItem(tabName="narrative",
        fluidRow(
          box(width=4,title="Pilih Anak",status="primary",solidHeader=TRUE,selectInput("narr_child","Anak",choices=NULL)),
          box(width=8,title="Narasi Perkembangan Otomatis",status="info",solidHeader=TRUE,uiOutput("narrative_text"))),
        fluidRow(box(width=12,title="Rekomendasi per Aspek",status="warning",solidHeader=TRUE,uiOutput("recommendations")))),
      tabItem(tabName="export",
        fluidRow(
          box(width=6,title="Laporan Kelas",status="primary",solidHeader=TRUE,p("Excel berisi data, skor aspek, perubahan, dan indikator yang perlu perhatian."),downloadButton("download_excel","Unduh Excel Analisis")),
          box(width=6,title="Laporan Individual",status="info",solidHeader=TRUE,selectInput("export_child","Pilih Anak",choices=NULL),downloadButton("download_docx","Unduh Laporan Word"))
        )),
      tabItem(tabName="settings",
        fluidRow(
          box(width=6,title="Batas Kategori",status="primary",solidHeader=TRUE,
              numericInput("cut_bb","Batas atas BB",1.49,min=1,max=4,step=.01),
              numericInput("cut_mb","Batas atas MB",2.49,min=1,max=4,step=.01),
              numericInput("cut_bsh","Batas atas BSH",3.49,min=1,max=4,step=.01)),
          box(width=6,title="Deteksi Perubahan",status="warning",solidHeader=TRUE,
              numericInput("stagnation_delta","Batas stagnan (|Δ| ≤)",0.15,min=0,max=1,step=.05),
              numericInput("decline_delta","Batas penurunan (Δ <)",-0.15,min=-2,max=0,step=.05),
              tags$div(class="note","Nilai ini dapat disesuaikan dengan karakteristik instrumen. Untuk skor ordinal 1–4, interpretasikan perubahan secara hati-hati.")))
        ))
    )
  )

server <- function(input, output, session) {
  raw_data <- reactive({
    if (isTRUE(input$use_demo) || is.null(input$file)) return(template_df)
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext %in% c("xlsx","xls")) readxl::read_excel(input$file$datapath) else read.csv(input$file$datapath, stringsAsFactors=FALSE)
  })

  validated <- reactive({
    d <- as.data.frame(raw_data())
    required <- c("id_anak","nama_anak","kelompok","usia_bulan","periode")
    miss <- setdiff(required,names(d)); if(length(miss)>0) stop(paste("Kolom wajib belum ada:",paste(miss,collapse=", ")))
    valid_ind <- intersect(indicator_meta$kode,names(d)); if(length(valid_ind)==0) stop("Tidak ada kolom indikator yang dikenali.")
    d$periode <- factor(as.character(d$periode),levels=period_order,ordered=TRUE)
    for (k in valid_ind) {
      d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
    }
    d
  })

  long_indicator <- reactive({
    d <- validated(); ind <- intersect(indicator_meta$kode,names(d))
    d %>% pivot_longer(all_of(ind),names_to="kode",values_to="skor") %>% left_join(indicator_meta,by="kode")
  })

  aspect_scores <- reactive({
    long_indicator() %>% group_by(id_anak,nama_anak,kelompok,usia_bulan,periode,aspek) %>% summarise(skor_aspek=mean(skor,na.rm=TRUE),.groups="drop") %>%
      mutate(kategori=category_code(skor_aspek,input$cut_bb,input$cut_mb,input$cut_bsh), aspek=factor(aspek,levels=aspect_order))
  })

  status_from_delta <- function(delta) {
    case_when(is.na(delta)~NA_character_, delta < input$decline_delta~"Menurun", abs(delta)<=input$stagnation_delta~"Stagnan", TRUE~"Meningkat")
  }

  aspect_change <- reactive({
    a <- aspect_scores() %>% select(id_anak,nama_anak,aspek,periode,skor_aspek,kategori) %>%
      pivot_wider(names_from=periode,values_from=c(skor_aspek,kategori),names_sep="__")
    ini <- "skor_aspek__Awal Semester"; fin <- "skor_aspek__Akhir Semester"
    if(!(ini %in% names(a)) || !(fin %in% names(a))) return(a %>% mutate(delta=NA_real_,status=NA_character_))
    a %>% mutate(delta=.data[[fin]]-.data[[ini]],status=status_from_delta(delta))
  })

  indicator_change <- reactive({
    x <- long_indicator() %>% group_by(id_anak,nama_anak,aspek,kode,indikator,periode) %>% summarise(skor=mean(skor,na.rm=TRUE),.groups="drop") %>%
      pivot_wider(names_from=periode,values_from=skor)
    if(!all(c("Awal Semester","Akhir Semester") %in% names(x))) return(x %>% mutate(delta=NA_real_,status=NA_character_))
    x %>% mutate(delta=`Akhir Semester`-`Awal Semester`,status=status_from_delta(delta))
  })

  observe({
    ch <- validated() %>% distinct(id_anak,nama_anak) %>% arrange(nama_anak)
    choices <- setNames(ch$id_anak,ch$nama_anak)
    input_ids <- c("child_select", "indicator_child", "narr_child", "export_child")
    for (id in input_ids) {
      updateSelectInput(session, id, choices = choices)
    }
  })

  output$vb_child <- renderValueBox({ valueBox(n_distinct(validated()$id_anak),"Anak",icon=icon("children"),color="aqua") })
  output$vb_obs <- renderValueBox({ valueBox(nrow(validated()),"Observasi",icon=icon("clipboard-check"),color="blue") })
  output$vb_progress <- renderValueBox({ p<-mean(aspect_change()$status=="Meningkat",na.rm=TRUE); valueBox(percent(p,accuracy=1),"Aspek meningkat",icon=icon("arrow-trend-up"),color="green") })
  output$vb_stagnant <- renderValueBox({ p<-mean(aspect_change()$status=="Stagnan",na.rm=TRUE); valueBox(percent(p,accuracy=1),"Aspek stagnan",icon=icon("pause"),color="yellow") })

  output$home_trend <- renderPlot({
    aspect_scores() %>% group_by(periode,aspek) %>% summarise(mean=mean(skor_aspek,na.rm=TRUE),.groups="drop") %>%
      ggplot(aes(periode,mean,group=aspek,linetype=aspek))+geom_line(linewidth=1)+geom_point(size=2)+scale_y_continuous(limits=c(1,4),breaks=1:4)+labs(x=NULL,y="Rata-rata skor",linetype="Aspek")+theme_minimal(base_size=12)
  })
  output$data_table <- renderDT(datatable(validated(),options=list(scrollX=TRUE,pageLength=10)))
  output$indicator_meta <- renderDT(datatable(indicator_meta,options=list(pageLength=12,scrollX=TRUE)))

  output$class_trend <- renderPlot({
    aspect_scores() %>% filter(as.character(aspek)==input$class_aspect) %>% group_by(periode) %>% summarise(mean=mean(skor_aspek,na.rm=TRUE),se=sd(skor_aspek,na.rm=TRUE)/sqrt(n()),.groups="drop") %>%
      ggplot(aes(periode,mean,group=1))+geom_line(linewidth=1.2)+geom_point(size=3)+geom_errorbar(aes(ymin=pmax(1,mean-se),ymax=pmin(4,mean+se)),width=.12)+scale_y_continuous(limits=c(1,4),breaks=1:4)+labs(x=NULL,y="Rata-rata aspek",title=input$class_aspect)+theme_minimal(base_size=13)
  })
  output$class_categories <- renderPlot({
    aspect_scores() %>% filter(as.character(aspek)==input$class_aspect) %>% count(periode,kategori) %>% group_by(periode) %>% mutate(p=n/sum(n)) %>% ungroup() %>%
      ggplot(aes(periode,p,fill=kategori))+geom_col()+scale_y_continuous(labels=percent_format())+labs(x=NULL,y="Proporsi anak",fill="Kategori")+theme_minimal(base_size=12)
  })
  output$class_change_table <- renderDT({
    aspect_change() %>% filter(as.character(aspek)==input$class_aspect) %>% count(status,name="Jumlah") %>% mutate(Persen=percent(Jumlah/sum(Jumlah),accuracy=.1)) %>% datatable(options=list(dom='t'),rownames=FALSE)
  })

  output$child_info <- renderUI({
    req(input$child_select); d<-validated()%>%filter(id_anak==input$child_select)%>%slice(1)
    tagList(h4(d$nama_anak),p(paste("Kelompok:",d$kelompok)),p(paste("Usia:",d$usia_bulan,"bulan")))
  })
  output$child_trend <- renderPlot({
    req(input$child_select)
    aspect_scores()%>%filter(id_anak==input$child_select)%>%ggplot(aes(periode,skor_aspek,group=aspek,linetype=aspek))+geom_line(linewidth=1.1)+geom_point(size=2.5)+scale_y_continuous(limits=c(1,4),breaks=1:4)+labs(x=NULL,y="Skor aspek",linetype="Aspek")+theme_minimal(base_size=12)
  })
  output$child_change_table <- renderDT({ req(input$child_select); datatable(aspect_change()%>%filter(id_anak==input$child_select)%>%select(aspek,contains("Awal Semester"),contains("Akhir Semester"),delta,status),options=list(scrollX=TRUE),rownames=FALSE) })

  output$indicator_trend <- renderPlot({
    req(input$indicator_child,input$indicator_aspect)
    long_indicator()%>%filter(id_anak==input$indicator_child,aspek==input$indicator_aspect)%>%ggplot(aes(periode,skor,group=kode,linetype=kode))+geom_line(linewidth=1)+geom_point(size=2.3)+scale_y_continuous(limits=c(1,4),breaks=1:4)+labs(x=NULL,y="Skor indikator",linetype="Indikator")+theme_minimal(base_size=12)
  })
  output$indicator_status_table <- renderDT({
    req(input$indicator_child,input$indicator_aspect)
    datatable(indicator_change()%>%filter(id_anak==input$indicator_child,aspek==input$indicator_aspect)%>%arrange(factor(status,levels=c("Menurun","Stagnan","Meningkat")))%>%select(kode,indikator,`Awal Semester`,`Tengah Semester`,`Akhir Semester`,delta,status),options=list(scrollX=TRUE,pageLength=10),rownames=FALSE)
  })

  output$narrative_text <- renderUI({
    req(input$narr_child)
    child_as <- aspect_scores()%>%filter(id_anak==input$narr_child)
    chg <- aspect_change()%>%filter(id_anak==input$narr_child)
    tags$div(class="rec",tags$p(style="font-size:16px;line-height:1.7;",make_narrative(child_as,chg)),tags$p(class="text-muted","Narasi dihasilkan otomatis dari pola skor dan aturan analitik. Guru tetap perlu menambahkan bukti autentik seperti catatan anekdot, hasil karya, foto kegiatan, atau konteks perilaku anak."))
  })
  output$recommendations <- renderUI({
    req(input$narr_child)
    chg <- aspect_change()%>%filter(id_anak==input$narr_child)
    tagList(lapply(seq_len(nrow(chg)),function(i){
      r<-chg[i,]; finalcat<-if("kategori__Akhir Semester"%in%names(r)) r[["kategori__Akhir Semester"]] else NA_character_
      tags$div(class="rec",tags$h4(as.character(r$aspek)),tags$strong(paste("Status:",r$status,"| Δ =",round(r$delta,2))),tags$p(recommendation_text(as.character(r$aspek),r$status,finalcat)))
    }))
  })

  output$download_template <- downloadHandler(
    filename = function() {
      "template_observasi_longitudinal_PAUD.xlsx"
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data Observasi")
      writeData(wb, "Data Observasi", template_df)
      addWorksheet(wb, "Kamus Indikator")
      writeData(wb, "Kamus Indikator", indicator_meta)
      addWorksheet(wb, "Petunjuk")
      petunjuk <- data.frame(
        Petunjuk = c(
          "Satu baris = satu anak pada satu periode.",
          "Gunakan periode persis: Awal Semester, Tengah Semester, Akhir Semester.",
          "Skor default indikator 1-4.",
          "Jangan mengubah kode indikator tanpa menyesuaikan aplikasi."
        )
      )
      writeData(wb, "Petunjuk", petunjuk)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("Analisis_PAUD_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, "Data", validated())
      addWorksheet(wb, "Skor Aspek")
      writeData(wb, "Skor Aspek", aspect_scores())
      addWorksheet(wb, "Perubahan Aspek")
      writeData(wb, "Perubahan Aspek", aspect_change())
      addWorksheet(wb, "Perubahan Indikator")
      writeData(wb, "Perubahan Indikator", indicator_change())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_docx <- downloadHandler(
    filename = function() {
      req(input$export_child)
      nm <- validated() %>%
        filter(id_anak == input$export_child) %>%
        slice(1) %>%
        pull(nama_anak)
      paste0("Laporan_", gsub(" ", "_", nm), ".docx")
    },
    content = function(file) {
      req(input$export_child)

      child_as <- aspect_scores() %>%
        filter(id_anak == input$export_child)
      chg <- aspect_change() %>%
        filter(id_anak == input$export_child)
      nm <- unique(child_as$nama_anak)[1]

      doc <- read_docx()
      doc <- body_add_par(doc, "Laporan Perkembangan Anak", style = "heading 1")
      doc <- body_add_par(doc, paste("Nama:", nm))
      doc <- body_add_par(doc, make_narrative(child_as, chg))
      doc <- body_add_par(doc, "Ringkasan Per Aspek", style = "heading 2")

      tbl <- chg %>% select(aspek, delta, status)
      doc <- body_add_table(doc, tbl, style = "Table Grid")
      doc <- body_add_par(doc, "Rekomendasi", style = "heading 2")

      for (i in seq_len(nrow(chg))) {
        r <- chg[i, ]
        if ("kategori__Akhir Semester" %in% names(r)) {
          finalcat <- r[["kategori__Akhir Semester"]]
        } else {
          finalcat <- NA_character_
        }
        doc <- body_add_par(doc, as.character(r$aspek), style = "heading 3")
        doc <- body_add_par(
          doc,
          recommendation_text(as.character(r$aspek), r$status, finalcat)
        )
      }

      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
