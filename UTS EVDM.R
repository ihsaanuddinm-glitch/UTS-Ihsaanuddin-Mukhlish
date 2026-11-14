# =====================================================================
# Dashboard Eksplorasi & Visualisasi Data Multivariat
# Topik   : Faktor-faktor yang berkaitan dengan nilai akhir siswa SMA
# Data    : Student Performance (UCI), file student-por (Bahasa Portugis)
# Jalankan: shiny::runApp()  (student-por.xlsx harus satu folder dengan app.R)
# =====================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(DT)

# ---------------------------------------------------------------------
# 1. MEMBACA DATA
# ---------------------------------------------------------------------
if (file.exists("student-por.xlsx")) {
  data_raw <- as.data.frame(read_excel("student-por.xlsx"))
} else if (file.exists("student-por.csv")) {
  data_raw <- read.csv("student-por.csv", sep = ";")   # file asli UCI dipisah titik koma
} else {
  stop("File student-por.xlsx / student-por.csv tidak ditemukan di folder aplikasi.")
}

kat_cols <- c("school", "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob",
              "reason", "guardian", "schoolsup", "famsup", "paid", "activities",
              "nursery", "higher", "internet", "romantic")
data_raw[kat_cols] <- lapply(data_raw[kat_cols], factor)

# ---------------------------------------------------------------------
# 2. PALET WARNA DAN TEMA GRAFIK
# ---------------------------------------------------------------------
col_primary <- "#4F46E5"   # indigo
col_green   <- "#10B981"
col_amber   <- "#F59E0B"
col_red     <- "#EF4444"
col_ink     <- "#1E293B"
col_muted   <- "#64748B"
pal         <- c(col_primary, col_green, col_amber, col_red)

theme_dash <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15, color = col_ink),
      plot.subtitle    = element_text(color = col_muted),
      plot.caption     = element_text(color = col_muted, size = 9),
      axis.title       = element_text(color = col_muted),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0"),
      legend.position  = "top",
      plot.margin      = margin(10, 14, 10, 10)
    )
}

# ---------------------------------------------------------------------
# 3. FUNGSI BANTU
# ---------------------------------------------------------------------
skew <- function(x) {
  m <- mean(x)
  mean((x - m)^3) / (mean((x - m)^2))^1.5
}

describe_num <- function(x, nama) {
  sk  <- skew(x)
  bentuk <- if (sk > 0.5) "miring ke kanan (ekor panjang di nilai besar)"
            else if (sk < -0.5) "miring ke kiri (ekor panjang di nilai kecil)"
            else "cukup simetris"
  n_out <- length(boxplot.stats(x)$out)
  sprintf(paste0("%s: rata-rata %.2f, median %.2f, simpangan baku %.2f, rentang %s sampai %s. ",
                 "Skewness = %.2f, jadi distribusinya %s. ",
                 "Terdapat %d pencilan menurut aturan 1,5 x IQR."),
          nama, mean(x), median(x), sd(x), min(x), max(x), sk, bentuk, n_out)
}

group_text <- function(d, var) {
  m <- tapply(d$G3, d[[var]], mean)
  paste(sprintf("%s = %.2f", names(m), m), collapse = "; ")
}

interpret_coef <- function(nm, b, p, d) {
  sig <- if (p < 0.05) "signifikan" else "tidak signifikan"
  for (v in c("school", "sex", "higher")) {
    if (startsWith(nm, v)) {
      lv  <- sub(paste0("^", v), "", nm)
      ref <- levels(d[[v]])[1]
      return(sprintf(
        "%s = %s: rata-rata G3 %s %.2f poin dibanding %s = %s, dengan variabel lain dianggap tetap (%s pada alpha 5%%, p = %s).",
        v, lv, if (b > 0) "lebih tinggi" else "lebih rendah", abs(b), v, ref,
        sig, format.pval(p, digits = 3)))
    }
  }
  sprintf(
    "%s: setiap kenaikan 1 satuan, G3 diperkirakan %s %.2f poin, dengan variabel lain dianggap tetap (%s pada alpha 5%%, p = %s).",
    nm, if (b > 0) "naik" else "turun", abs(b), sig, format.pval(p, digits = 3))
}

# Pembungkus kartu putih untuk tiap bagian
kartu <- function(judul, ...) {
  div(class = "dcard", h4(class = "dcard-title", judul), ...)
}

var_def <- data.frame(
  Variabel = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu",
               "Mjob", "Fjob", "reason", "guardian", "traveltime", "studytime", "failures",
               "schoolsup", "famsup", "paid", "activities", "nursery", "higher", "internet",
               "romantic", "famrel", "freetime", "goout", "Dalc", "Walc", "health",
               "absences", "G1", "G2", "G3"),
  Tipe = c("Kategorik", "Kategorik", "Numerik", "Kategorik", "Kategorik", "Kategorik",
           "Ordinal", "Ordinal", "Kategorik", "Kategorik", "Kategorik", "Kategorik",
           "Ordinal", "Ordinal", "Numerik (cacah)", "Kategorik", "Kategorik", "Kategorik",
           "Kategorik", "Kategorik", "Kategorik", "Kategorik", "Kategorik", "Ordinal",
           "Ordinal", "Ordinal", "Ordinal", "Ordinal", "Ordinal", "Numerik (cacah)",
           "Numerik", "Numerik", "Numerik"),
  Definisi = c(
    "Sekolah siswa (GP = Gabriel Pereira, MS = Mousinho da Silveira)",
    "Jenis kelamin (F = perempuan, M = laki-laki)",
    "Umur siswa (15 sampai 22 tahun)",
    "Tipe tempat tinggal (U = perkotaan, R = pedesaan)",
    "Ukuran keluarga (LE3 = 3 orang atau kurang, GT3 = lebih dari 3)",
    "Status tinggal orang tua (T = bersama, A = terpisah)",
    "Pendidikan ibu (0 = tidak ada, 1 = SD, 2 = SMP, 3 = SMA, 4 = perguruan tinggi)",
    "Pendidikan ayah (skala sama dengan Medu)",
    "Pekerjaan ibu (teacher, health, services, at_home, other)",
    "Pekerjaan ayah (teacher, health, services, at_home, other)",
    "Alasan memilih sekolah (home, reputation, course, other)",
    "Wali siswa (mother, father, other)",
    "Waktu tempuh rumah ke sekolah (1 = <15 menit, 2 = 15-30 menit, 3 = 30 menit-1 jam, 4 = >1 jam)",
    "Waktu belajar per minggu (1 = <2 jam, 2 = 2-5 jam, 3 = 5-10 jam, 4 = >10 jam)",
    "Jumlah kegagalan kelas sebelumnya",
    "Dukungan belajar tambahan dari sekolah (yes/no)",
    "Dukungan belajar dari keluarga (yes/no)",
    "Kelas tambahan berbayar pada mata pelajaran (yes/no)",
    "Kegiatan ekstrakurikuler (yes/no)",
    "Pernah mengikuti pendidikan prasekolah (yes/no)",
    "Ingin melanjutkan ke pendidikan tinggi (yes/no)",
    "Akses internet di rumah (yes/no)",
    "Sedang menjalin hubungan romantis (yes/no)",
    "Kualitas hubungan keluarga (1 = sangat buruk sampai 5 = sangat baik)",
    "Waktu luang setelah sekolah (1 = sangat sedikit sampai 5 = sangat banyak)",
    "Frekuensi keluar bersama teman (1 = sangat jarang sampai 5 = sangat sering)",
    "Konsumsi alkohol hari kerja (1 = sangat rendah sampai 5 = sangat tinggi)",
    "Konsumsi alkohol akhir pekan (1 = sangat rendah sampai 5 = sangat tinggi)",
    "Status kesehatan saat ini (1 = sangat buruk sampai 5 = sangat baik)",
    "Jumlah ketidakhadiran di sekolah",
    "Nilai periode pertama (skala 0-20)",
    "Nilai periode kedua (skala 0-20)",
    "Nilai akhir (skala 0-20). VARIABEL RESPONS"),
  Peran_di_model = c("Prediktor", "Prediktor", "-", "-", "-", "-", "Prediktor", "-",
                     "-", "-", "-", "-", "-", "Prediktor", "Prediktor",
                     "-", "-", "-", "-", "-", "Prediktor", "-",
                     "-", "-", "-", "-", "-", "-", "-",
                     "Prediktor", "-", "-", "Respons"),
  stringsAsFactors = FALSE
)
names(var_def)[4] <- "Peran di model"

cat_choices <- c("school", "sex", "higher", "address", "famsize", "Pstatus", "schoolsup",
                 "famsup", "paid", "activities", "internet", "romantic", "failures_grp")
num_cols <- c("age", "Medu", "Fedu", "traveltime", "studytime", "failures", "famrel", "freetime",
              "goout", "Dalc", "Walc", "health", "absences", "G1", "G2", "G3")
num_choices <- c("G3", setdiff(num_cols, "G3"))

# ---------------------------------------------------------------------
# 4. TAMPILAN (UI)
# ---------------------------------------------------------------------
css <- "
body {background:#F1F5F9; font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; color:#1E293B;}
.hero {background:linear-gradient(135deg,#4F46E5 0%,#7C3AED 60%,#06B6D4 100%); color:white;
       padding:28px 32px; border-radius:18px; margin:18px 0 18px 0; box-shadow:0 8px 24px rgba(79,70,229,.25);}
.hero h1 {font-size:1.7rem; font-weight:700; margin:0 0 6px 0;}
.hero p {margin:0; opacity:.92;}
.badge-soft {display:inline-block; background:rgba(255,255,255,.18); border-radius:999px;
             padding:3px 12px; font-size:.78rem; margin:10px 6px 0 0;}
.kpi-row {display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:18px;}
.kpi {background:white; border-radius:14px; padding:14px 16px; box-shadow:0 2px 10px rgba(15,23,42,.06);}
.kpi-val {font-size:1.7rem; font-weight:700; line-height:1.1;}
.kpi-lab {font-size:.8rem; color:#64748B; text-transform:uppercase; letter-spacing:.04em;}
.dcard {background:white; border-radius:14px; padding:18px 22px; margin-bottom:18px;
        box-shadow:0 2px 10px rgba(15,23,42,.06);}
.dcard-title {font-weight:700; margin:0 0 12px 0; font-size:1.05rem;}
.interp {background:#EEF2FF; border-left:4px solid #4F46E5; border-radius:8px;
         padding:10px 14px; font-size:.92rem; margin-top:8px;}
.interp:before {content:'Interpretasi: '; font-weight:700; color:#4F46E5;}
.side-card {background:white; border-radius:14px; padding:18px; box-shadow:0 2px 10px rgba(15,23,42,.06);}
.nav-pills .nav-link {border-radius:999px; font-weight:600; color:#475569; padding:8px 18px; margin-right:6px;}
.nav-pills .nav-link.active {background:#4F46E5; color:white;}
.nav-pills {margin-bottom:16px; flex-wrap:wrap; gap:4px;}
table.dataTable thead th {background:#EEF2FF;}
@media (max-width: 900px) {.kpi-row {grid-template-columns:repeat(2,1fr);}}
"

ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#4F46E5", secondary = "#10B981"),
  tags$head(tags$style(HTML(css))),

  div(class = "hero",
      h1("Eksplorasi Nilai Akhir Siswa SMA"),
      p("Dashboard interaktif analisis deskriptif, visualisasi multivariat, dan regresi linear berganda"),
      span(class = "badge-soft", "Student Performance (UCI)"),
      span(class = "badge-soft", "649 siswa, 33 variabel"),
      span(class = "badge-soft", "EVDM - Statistika")),

  uiOutput("kpi"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "side-card",
          h4("Filter Data"),
          checkboxGroupInput("school", "Sekolah", choices = levels(data_raw$school),
                             selected = levels(data_raw$school)),
          checkboxGroupInput("sex", "Jenis kelamin", choices = levels(data_raw$sex),
                             selected = levels(data_raw$sex)),
          checkboxInput("excl0", "Buang siswa dengan G3 = 0", value = FALSE),
          hr(),
          helpText("Filter berlaku di semua tab, termasuk model regresi."))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "pills",

        # ---------------- TAB 1 ----------------
        tabPanel("Ringkasan", icon = icon("table"),
          kartu("Latar belakang dan tujuan",
            p("Prestasi akademik siswa dipengaruhi banyak faktor, seperti kebiasaan belajar, riwayat kegagalan,
               latar belakang keluarga, dan motivasi melanjutkan pendidikan. Dashboard ini mengeksplorasi data
               siswa SMA di dua sekolah di Portugal untuk memahami faktor-faktor yang berkaitan dengan nilai akhir (G3)."),
            tags$ul(
              tags$li("Mendeskripsikan karakteristik tiap variabel (univariat)."),
              tags$li("Melihat hubungan nilai akhir dengan variabel lain (bivariat dan multivariat)."),
              tags$li("Membangun model regresi linear berganda untuk menjelaskan nilai akhir (G3).")
            )),
          kartu("Sumber data",
            p("Cortez, P. (2008). Student Performance [Dataset]. UCI Machine Learning Repository.
               https://doi.org/10.24432/C5TG7T (lisensi CC BY 4.0). Data yang dipakai: file mata pelajaran
               Bahasa Portugis (student-por)."),
            textOutput("info_data")),
          kartu("Pengolahan data (data hilang, pencilan, transformasi)", uiOutput("info_proses")),
          kartu("Definisi variabel", DTOutput("tbl_def")),
          kartu("Data (mengikuti filter)", DTOutput("tbl_data"))
        ),

        # ---------------- TAB 2 ----------------
        tabPanel("Univariat", icon = icon("chart-bar"),
          kartu("1. Distribusi nilai akhir (G3)",
                plotOutput("u1", height = 320), div(class = "interp", textOutput("i_u1"))),
          kartu("2. Distribusi variabel kategorik",
                selectInput("cat_u", "Pilih variabel kategorik", choices = cat_choices, selected = "higher"),
                plotOutput("u2", height = 320), div(class = "interp", textOutput("i_u2"))),
          kartu("3. Distribusi variabel numerik",
                selectInput("num_u", "Pilih variabel numerik", choices = num_choices, selected = "absences"),
                plotOutput("u3", height = 320), div(class = "interp", textOutput("i_u3"))),
          kartu("Statistik deskriptif semua variabel numerik/ordinal", tableOutput("tbl_desc")),
          kartu("Frekuensi semua variabel kategorik", tableOutput("tbl_kat"))
        ),

        # ---------------- TAB 3 ----------------
        tabPanel("Bivariat", icon = icon("chart-line"),
          kartu("1. G3 menurut keinginan melanjutkan kuliah (numerik vs kategorik)",
                plotOutput("b1", height = 330), div(class = "interp", textOutput("i_b1"))),
          kartu("2. G3 menurut waktu belajar (numerik vs ordinal)",
                plotOutput("b2", height = 330), div(class = "interp", textOutput("i_b2"))),
          kartu("3. G3 menurut variabel kategorik pilihan (numerik vs kategorik)",
                selectInput("cat_b", "Pilih variabel kategorik", choices = cat_choices, selected = "school"),
                plotOutput("b3", height = 330), div(class = "interp", textOutput("i_b3"))),
          kartu("4. Keinginan kuliah menurut sekolah (kategorik vs kategorik)",
                plotOutput("b4", height = 300), div(class = "interp", textOutput("i_b4")))
        ),

        # ---------------- TAB 4 ----------------
        tabPanel("Multivariat", icon = icon("layer-group"),
          kartu("1. G3 vs ketidakhadiran, dibedakan menurut kelompok",
                selectInput("col_m", "Warnai menurut", choices = c("school", "sex", "higher", "failures_grp"),
                            selected = "school"),
                plotOutput("m1", height = 380), div(class = "interp", textOutput("i_m1"))),
          kartu("2. Heatmap korelasi variabel numerik/ordinal",
                checkboxInput("pakai_g12", "Sertakan G1 dan G2", value = FALSE),
                plotOutput("m2", height = 520), div(class = "interp", textOutput("i_m2")))
        ),

        # ---------------- TAB 5 ----------------
        tabPanel("Regresi", icon = icon("calculator"),
          kartu("Pemilihan variabel dan alasannya",
            tags$ul(
              tags$li(tags$b("Respons (Y) = G3. "), "Nilai akhir adalah ukuran utama prestasi siswa, berskala numerik 0-20, sehingga cocok untuk regresi linear."),
              tags$li(tags$b("Prediktor numerik: "), "studytime (usaha belajar), failures (riwayat akademik), absences (kehadiran), Medu (latar belakang pendidikan keluarga)."),
              tags$li(tags$b("Prediktor kategorik: "), "school (perbedaan lingkungan sekolah), sex (perbedaan gender), higher (motivasi melanjutkan kuliah); dikodekan sebagai dummy."),
              tags$li("Prediktor dipilih karena relevan secara konsep dan menunjukkan pola pada eksplorasi bivariat/multivariat, serta saling tidak berkorelasi tinggi (lihat tabel VIF)."),
              tags$li("G1 dan G2 sengaja tidak dimasukkan: keduanya nilai periode sebelumnya yang sangat berkorelasi dengan G3, sehingga akan mendominasi model dan menyembunyikan peran faktor lain.")
            )),
          kartu("Persamaan model", verbatimTextOutput("eq")),
          kartu("Ukuran kecocokan model", tableOutput("tbl_fit")),
          kartu("Hasil estimasi koefisien", tableOutput("tbl_coef")),
          kartu("Visualisasi 1: estimasi koefisien (interval kepercayaan 95%)",
                plotOutput("r1", height = 340), div(class = "interp", textOutput("i_r1"))),
          kartu("Visualisasi 2: nilai aktual vs prediksi",
                plotOutput("r2", height = 340), div(class = "interp", textOutput("i_r2"))),
          kartu("Visualisasi 3: plot residual (pemeriksaan asumsi)",
                plotOutput("r3", height = 340), div(class = "interp", textOutput("i_r3"))),
          kartu("Uji asumsi klasik (normalitas, multikolinearitas, homoskedastisitas, autokorelasi)",
                tableOutput("tbl_asumsi"), uiOutput("i_asumsi")),
          kartu("Visualisasi 4: Q-Q plot residual (pemeriksaan normalitas)",
                plotOutput("r4", height = 340), div(class = "interp", textOutput("i_r4"))),
          kartu("Pemeriksaan multikolinearitas (VIF)", tableOutput("tbl_vif")),
          kartu("Interpretasi koefisien", uiOutput("i_reg")),
          kartu("Keterbatasan model",
            tags$ul(
              tags$li("Data bersifat observasional, sehingga hasil menunjukkan keterkaitan, bukan sebab-akibat."),
              tags$li("Nilai 0 pada G3 (siswa yang kemungkinan tidak mengikuti ujian) membuat residual tidak normal dan miring. Coba centang 'Buang siswa dengan G3 = 0' untuk membandingkan."),
              tags$li("Beberapa prediktor (studytime, Medu) berskala ordinal tetapi diperlakukan sebagai numerik."),
              tags$li("failures sangat timpang (sebagian besar siswa bernilai 0) dan absences miring ke kanan."),
              tags$li("Model hanya menjelaskan sebagian variasi G3 (lihat R-squared); faktor lain di luar model berperan."),
              tags$li("Data berasal dari dua sekolah di Portugal, sehingga generalisasi ke populasi lain terbatas.")
            ))
        ),

        # ---------------- TAB 6 ----------------
        tabPanel("Kesimpulan", icon = icon("lightbulb"),
          kartu("Kesimpulan utama", uiOutput("i_kes")),
          kartu("Rekomendasi",
            tags$ul(
              tags$li("Bagi siswa dan orang tua: dorong kebiasaan belajar yang konsisten dan jaga kehadiran, karena waktu belajar dan kehadiran berkaitan dengan nilai akhir."),
              tags$li("Bagi sekolah: pantau dini siswa yang pernah gagal kelas (failures >= 1) dan berikan pendampingan, karena kelompok ini cenderung bernilai lebih rendah."),
              tags$li("Bagi sekolah: bangun motivasi dan informasi tentang pendidikan lanjutan, karena siswa yang ingin kuliah cenderung bernilai lebih tinggi."),
              tags$li("Bagi sekolah: telaah perbedaan antarsekolah (misalnya sumber daya, metode belajar) yang mungkin menjelaskan selisih nilai."),
              tags$li("Bagi penelitian lanjutan: tambahkan variabel lain, uji interaksi, atau gunakan model yang lebih tahan terhadap pencilan (misalnya regresi robust), serta pertimbangkan model terpisah bagi siswa dengan G3 = 0.")
            )),
          kartu("Catatan kehati-hatian",
            p("Hasil menunjukkan keterkaitan, bukan sebab-akibat. Model hanya menjelaskan sebagian variasi nilai akhir,
               sehingga rekomendasi di atas bersifat arah awal dan perlu dikaji lebih lanjut."))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------
# 5. LOGIKA (SERVER)
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  data_f <- reactive({
    d <- data_raw[data_raw$school %in% input$school & data_raw$sex %in% input$sex, ]
    if (isTRUE(input$excl0)) d <- d[d$G3 > 0, ]
    validate(need(nrow(d) > 30, "Data terlalu sedikit. Longgarkan filter di sidebar."))
    d$failures_grp <- factor(ifelse(d$failures == 0, "0 kali", ">= 1 kali"),
                             levels = c("0 kali", ">= 1 kali"))
    droplevels(d)
  })

  # ---------- KARTU KPI ----------
  output$kpi <- renderUI({
    d <- data_f()
    kp <- function(lab, val, col)
      div(class = "kpi", style = paste0("border-top:4px solid ", col, ";"),
          div(class = "kpi-val", val), div(class = "kpi-lab", lab))
    div(class = "kpi-row",
        kp("Observasi terpilih", nrow(d), col_primary),
        kp("Rata-rata G3", sprintf("%.2f", mean(d$G3)), col_green),
        kp("Median G3", sprintf("%.1f", median(d$G3)), col_amber),
        kp("Ingin kuliah", sprintf("%.1f%%", 100 * mean(d$higher == "yes")), col_red))
  })

  # ---------- TAB 1 ----------
  output$info_data <- renderText(
    sprintf("Dataset asli: %d observasi, %d variabel (tanpa data hilang). Setelah filter: %d observasi.",
            nrow(data_raw), ncol(data_raw), nrow(data_f())))
  output$tbl_def  <- renderDT(datatable(var_def, rownames = FALSE, options = list(pageLength = 8)))
  output$tbl_data <- renderDT(datatable(data_f()[, names(data_raw)], rownames = FALSE,
                                        options = list(pageLength = 8, scrollX = TRUE)))

  # ---------- TAB 2: UNIVARIAT ----------
  output$u1 <- renderPlot({
    d <- data_f()
    ggplot(d, aes(x = G3)) +
      geom_histogram(binwidth = 1, fill = col_primary, color = "white") +
      geom_vline(xintercept = mean(d$G3), color = col_red, linetype = "dashed", linewidth = 0.9) +
      labs(title = "Distribusi Nilai Akhir (G3)", x = "Nilai akhir G3 (skala 0-20)",
           y = "Jumlah siswa", caption = "Garis merah putus-putus = rata-rata") +
      theme_dash()
  })
  output$i_u1 <- renderText({
    d <- data_f()
    paste(describe_num(d$G3, "G3"),
          sprintf("Jumlah siswa dengan G3 = 0: %d (kemungkinan siswa yang tidak mengikuti ujian).", sum(d$G3 == 0)))
  })

  output$u2 <- renderPlot({
    d <- data_f(); d$.v <- d[[input$cat_u]]
    ggplot(d, aes(x = .v, fill = .v)) +
      geom_bar(width = 0.6) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, color = col_ink) +
      scale_fill_manual(values = pal, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste("Distribusi", input$cat_u), x = input$cat_u, y = "Jumlah siswa") +
      theme_dash()
  })
  output$i_u2 <- renderText({
    d <- data_f(); tb <- table(d[[input$cat_u]]); pr <- round(100 * prop.table(tb), 1)
    paste0("Proporsi tiap kategori pada ", input$cat_u, ": ",
           paste(sprintf("%s = %s%%", names(pr), pr), collapse = "; "),
           ". Kategori terbanyak: ", names(tb)[which.max(tb)], ".")
  })

  output$u3 <- renderPlot({
    d <- data_f(); d$.v <- d[[input$num_u]]
    ggplot(d, aes(x = .v)) +
      geom_histogram(bins = 20, fill = col_green, color = "white") +
      labs(title = paste("Distribusi", input$num_u), x = input$num_u, y = "Jumlah siswa") +
      theme_dash()
  })
  output$i_u3 <- renderText(describe_num(data_f()[[input$num_u]], input$num_u))

  output$tbl_desc <- renderTable({
    d <- data_f()
    do.call(rbind, lapply(num_cols, function(v) {
      x <- d[[v]]
      data.frame(Variabel = v, N = length(x), Mean = mean(x), Median = median(x), SD = sd(x),
                 Min = min(x), Q1 = unname(quantile(x, .25)), Q3 = unname(quantile(x, .75)),
                 Max = max(x), Skewness = skew(x), Pencilan = length(boxplot.stats(x)$out))
    }))
  }, digits = 2, striped = TRUE, hover = TRUE, width = "100%")

  # ---------- TAB 3: BIVARIAT ----------
  output$b1 <- renderPlot({
    d <- data_f()
    ggplot(d, aes(x = higher, y = G3, fill = higher)) +
      geom_boxplot(alpha = 0.8, outlier.color = col_red, width = 0.55) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
      scale_fill_manual(values = pal, guide = "none") +
      labs(title = "Nilai Akhir menurut Keinginan Melanjutkan Kuliah",
           x = "Ingin kuliah (higher)", y = "Nilai akhir G3",
           caption = "Berlian putih = rata-rata kelompok; titik merah = pencilan") +
      theme_dash()
  })
  output$i_b1 <- renderText({
    d <- data_f()
    paste0("Rata-rata G3: ", group_text(d, "higher"),
           ". Siswa yang ingin melanjutkan kuliah cenderung memiliki nilai lebih tinggi; ",
           "perhatikan bahwa kelompok 'no' jauh lebih kecil sehingga perbandingannya kurang stabil.")
  })

  output$b2 <- renderPlot({
    d <- data_f()
    ggplot(d, aes(x = factor(studytime), y = G3)) +
      geom_boxplot(fill = "#C7D2FE", outlier.color = col_red, width = 0.6) +
      stat_summary(fun = mean, geom = "line", aes(group = 1), color = col_primary, linewidth = 1) +
      stat_summary(fun = mean, geom = "point", color = col_primary, size = 3.5) +
      labs(title = "Nilai Akhir menurut Waktu Belajar Mingguan",
           x = "studytime (1 = <2 jam, 2 = 2-5 jam, 3 = 5-10 jam, 4 = >10 jam)", y = "Nilai akhir G3",
           caption = "Garis indigo = rata-rata tiap level") +
      theme_dash()
  })
  output$i_b2 <- renderText({
    d <- data_f(); r <- cor(d$studytime, d$G3, method = "spearman")
    paste0("Rata-rata G3 per level studytime: ", group_text(d, "studytime"),
           sprintf(". Korelasi Spearman = %.2f, ", r),
           if (r > 0.1) "menunjukkan hubungan positif lemah-sedang: makin lama belajar, nilai cenderung makin tinggi."
           else "menunjukkan hubungan yang sangat lemah.")
  })

  output$b3 <- renderPlot({
    d <- data_f(); d$.v <- d[[input$cat_b]]
    ggplot(d, aes(x = .v, y = G3, fill = .v)) +
      geom_boxplot(alpha = 0.8, outlier.color = col_red, width = 0.55) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
      scale_fill_manual(values = pal, guide = "none") +
      labs(title = paste("Nilai Akhir menurut", input$cat_b), x = input$cat_b, y = "Nilai akhir G3",
           caption = "Berlian putih = rata-rata kelompok") +
      theme_dash()
  })
  output$i_b3 <- renderText(paste0("Rata-rata G3 menurut ", input$cat_b, ": ",
                                   group_text(data_f(), input$cat_b),
                                   ". Selisih antarkelompok menunjukkan seberapa besar variabel ini membedakan nilai siswa."))

  output$b4 <- renderPlot({
    d <- data_f()
    ggplot(d, aes(x = school, fill = higher)) +
      geom_bar(position = "fill", width = 0.6) +
      scale_fill_manual(values = c(col_red, col_green)) +
      scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
      labs(title = "Proporsi Keinginan Kuliah di Tiap Sekolah", x = "Sekolah", y = "Proporsi siswa",
           fill = "higher") +
      theme_dash()
  })
  output$i_b4 <- renderText({
    d <- data_f(); tb <- prop.table(table(d$school, d$higher), 1)
    if (!"yes" %in% colnames(tb)) return("Tidak ada data untuk dibandingkan.")
    paste0("Persentase siswa yang ingin kuliah per sekolah: ",
           paste(sprintf("%s = %.1f%%", rownames(tb), 100 * tb[, "yes"]), collapse = "; "), ".")
  })

  # ---------- TAB 4: MULTIVARIAT ----------
  output$m1 <- renderPlot({
    d <- data_f(); d$.col <- d[[input$col_m]]
    ggplot(d, aes(x = absences, y = G3, color = .col)) +
      geom_jitter(alpha = 0.5, width = 0.3, height = 0.3, size = 2) +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linewidth = 1.2) +
      scale_color_manual(values = pal) +
      labs(title = "Nilai Akhir vs Ketidakhadiran", x = "Jumlah ketidakhadiran (absences)",
           y = "Nilai akhir G3", color = input$col_m) +
      theme_dash()
  })
  output$i_m1 <- renderText({
    d <- data_f(); d$.col <- d[[input$col_m]]
    sl <- sapply(split(d, d$.col), function(g) {
      if (nrow(g) > 5 && sd(g$absences) > 0) unname(coef(lm(G3 ~ absences, data = g))[2]) else NA
    })
    paste0("Kemiringan garis G3 terhadap absences per kelompok ", input$col_m, ": ",
           paste(sprintf("%s = %.3f", names(sl), sl), collapse = "; "),
           ". Jika kemiringan berbeda antarkelompok, hubungan absences dengan G3 bergantung pada kelompok tersebut.")
  })

  cm_data <- reactive({
    d <- data_f()
    v <- c("age", "Medu", "Fedu", "traveltime", "studytime", "failures", "famrel", "freetime",
           "goout", "Dalc", "Walc", "health", "absences")
    if (isTRUE(input$pakai_g12)) v <- c(v, "G1", "G2")
    cor(d[, c(v, "G3")], method = "pearson")
  })
  output$m2 <- renderPlot({
    cm <- cm_data(); cl <- as.data.frame(as.table(cm)); names(cl) <- c("V1", "V2", "r")
    ggplot(cl, aes(x = V1, y = V2, fill = r)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.2f", r)), size = 3, color = col_ink) +
      scale_fill_gradient2(low = col_red, mid = "white", high = col_primary, limits = c(-1, 1)) +
      labs(title = "Heatmap Korelasi Pearson", x = "Variabel", y = "Variabel", fill = "r") +
      theme_dash() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  })
  output$i_m2 <- renderText({
    cm <- cm_data(); g <- cm[rownames(cm) != "G3", "G3"]
    top <- head(g[order(-abs(g))], 3)
    paste0("Tiga variabel dengan korelasi terkuat terhadap G3: ",
           paste(sprintf("%s (r = %.2f)", names(top), top), collapse = "; "),
           ". Warna indigo = korelasi positif, merah = negatif. ",
           "Korelasi antarprediktor yang rendah menandakan risiko multikolinearitas kecil.")
  })

  # ---------- TAB 5: REGRESI ----------
  model <- reactive({
    d <- data_f()
    fx <- c("studytime", "failures", "absences", "Medu")
    for (v in c("school", "sex", "higher")) if (nlevels(d[[v]]) > 1) fx <- c(fx, v)
    lm(as.formula(paste("G3 ~", paste(fx, collapse = " + "))), data = d)
  })

  output$eq <- renderText({
    b <- coef(model())
    suku <- paste0(ifelse(b[-1] >= 0, " + ", " - "), sprintf("%.3f", abs(b[-1])), "*", names(b)[-1])
    paste0("G3 = ", sprintf("%.3f", b[1]), paste(suku, collapse = ""),
           "\n(schoolMS, sexM, higheryes = variabel dummy; pembanding: GP, F, no)")
  })

  output$tbl_fit <- renderTable({
    m <- model(); s <- summary(m); f <- s$fstatistic
    data.frame(Ukuran = c("Jumlah observasi (n)", "R-squared", "Adjusted R-squared",
                          "Statistik F", "p-value uji F (simultan)", "Residual standard error"),
               Nilai = c(nobs(m), sprintf("%.3f", s$r.squared), sprintf("%.3f", s$adj.r.squared),
                         sprintf("%.2f", f[1]),
                         format.pval(pf(f[1], f[2], f[3], lower.tail = FALSE), digits = 3),
                         sprintf("%.3f", s$sigma)))
  }, striped = TRUE, hover = TRUE, width = "100%")

  coef_df <- reactive({
    m <- model(); s <- summary(m)$coefficients; ci <- confint(m)
    data.frame(Variabel = rownames(s), Estimasi = s[, 1], SE = s[, 2], t = s[, 3], p = s[, 4],
               CI_bawah = ci[, 1], CI_atas = ci[, 2], row.names = NULL)
  })
  output$tbl_coef <- renderTable({
    cd <- coef_df(); cd$p <- format.pval(cd$p, digits = 3); cd
  }, digits = 3, striped = TRUE, hover = TRUE, width = "100%")

  output$r1 <- renderPlot({
    cd <- coef_df(); cd <- cd[cd$Variabel != "(Intercept)", ]
    cd$sig <- ifelse(cd$CI_bawah < 0 & cd$CI_atas > 0, "Tidak signifikan", "Signifikan")
    ggplot(cd, aes(y = reorder(Variabel, Estimasi), color = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = col_muted) +
      geom_segment(aes(x = CI_bawah, xend = CI_atas, yend = reorder(Variabel, Estimasi)), linewidth = 1.3) +
      geom_point(aes(x = Estimasi), size = 4) +
      scale_color_manual(values = c("Signifikan" = col_primary, "Tidak signifikan" = col_muted)) +
      labs(title = "Estimasi Koefisien Regresi (CI 95%)", x = "Perubahan G3 (poin)", y = "Variabel",
           color = "Pada alpha 5%") +
      theme_dash()
  })
  output$i_r1 <- renderText({
    cd <- coef_df(); cd <- cd[cd$Variabel != "(Intercept)", ]
    ns <- cd$Variabel[cd$CI_bawah < 0 & cd$CI_atas > 0]
    paste0("Titik = estimasi, garis = interval kepercayaan 95%. Variabel yang intervalnya memotong garis nol ",
           "(tidak signifikan pada alpha 5%): ",
           if (length(ns) == 0) "tidak ada." else paste(ns, collapse = ", "), ".")
  })

  output$r2 <- renderPlot({
    m <- model(); df <- data.frame(Aktual = m$model$G3, Prediksi = fitted(m))
    ggplot(df, aes(x = Prediksi, y = Aktual)) +
      geom_point(alpha = 0.5, color = col_primary, size = 2) +
      geom_abline(slope = 1, intercept = 0, color = col_red, linetype = "dashed", linewidth = 1) +
      labs(title = "Nilai Aktual vs Prediksi", x = "Nilai prediksi G3", y = "Nilai aktual G3",
           caption = "Garis merah = prediksi sempurna") +
      theme_dash()
  })
  output$i_r2 <- renderText({
    s <- summary(model())
    sprintf("Titik yang dekat garis merah berarti prediksi akurat. Sebaran yang lebar menunjukkan model hanya menjelaskan sebagian variasi (R-squared = %.3f, RMSE sekitar %.2f poin).",
            s$r.squared, s$sigma)
  })

  output$r3 <- renderPlot({
    m <- model(); df <- data.frame(Fitted = fitted(m), Residual = resid(m))
    ggplot(df, aes(x = Fitted, y = Residual)) +
      geom_point(alpha = 0.5, color = col_primary, size = 2) +
      geom_hline(yintercept = 0, color = col_red, linetype = "dashed", linewidth = 1) +
      geom_smooth(se = FALSE, color = col_green, method = "loess", formula = y ~ x, linewidth = 1.2) +
      labs(title = "Residual vs Nilai Prediksi", x = "Nilai prediksi G3", y = "Residual",
           caption = "Garis hijau = tren residual (idealnya datar di sekitar nol)") +
      theme_dash()
  })
  output$i_r3 <- renderText({
    r <- resid(model())
    sprintf("Residual idealnya tersebar acak di sekitar nol tanpa pola. Skewness residual = %.2f; nilai negatif besar menandakan sejumlah siswa (terutama G3 = 0) jauh di bawah prediksi, sehingga asumsi normalitas tidak sepenuhnya terpenuhi.",
            skew(r))
  })

  output$i_reg <- renderUI({
    m <- model(); d <- data_f(); s <- summary(m); f <- s$fstatistic
    p_f <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    cf <- s$coefficients
    items <- lapply(seq_len(nrow(cf))[-1], function(i)
      tags$li(interpret_coef(rownames(cf)[i], cf[i, 1], cf[i, 4], d)))
    tagList(
      tags$p(sprintf("Secara simultan, model %s (uji F, p = %s). R-squared = %.1f%% artinya model menjelaskan sekitar %.1f%% variasi nilai akhir.",
                     if (p_f < 0.05) "signifikan" else "tidak signifikan",
                     format.pval(p_f, digits = 3), 100 * s$r.squared, 100 * s$r.squared)),
      tags$p(sprintf("Intercept = %.2f: perkiraan G3 saat semua prediktor numerik bernilai 0 dan pada kategori pembanding.", cf[1, 1])),
      tags$ul(items)
    )
  })
  # ---------- TAMBAHAN: PENGOLAHAN DATA, FREKUENSI, Q-Q, VIF, KESIMPULAN ----------
  output$info_proses <- renderUI({
    n_na <- sum(is.na(data_raw)); n0 <- sum(data_raw$G3 == 0)
    tags$ul(
      tags$li(sprintf("Data hilang: %d sel kosong pada seluruh dataset, sehingga tidak diperlukan imputasi atau penghapusan baris.", n_na)),
      tags$li(sprintf("Pencilan: %d siswa (%.1f%%) memiliki G3 = 0, kemungkinan tidak mengikuti ujian. Data dipertahankan pada model utama; gunakan filter 'Buang siswa dengan G3 = 0' untuk membandingkan hasilnya.", n0, 100 * n0 / nrow(data_raw))),
      tags$li("absences miring ke kanan dengan beberapa nilai ekstrem; nilai tersebut dipertahankan karena masih mungkin terjadi secara nyata."),
      tags$li("Transformasi: variabel respons G3 tidak ditransformasi. Variabel kategorik diubah menjadi factor; failures dikelompokkan menjadi '0 kali' dan '>= 1 kali' hanya untuk visualisasi; kategorik pada regresi dikodekan sebagai dummy."),
      tags$li("Variabel ordinal (studytime, Medu) diperlakukan sebagai numerik pada regresi.")
    )
  })

  output$tbl_kat <- renderTable({
    d <- data_f()
    do.call(rbind, lapply(kat_cols, function(v) {
      tb <- table(d[[v]]); pr <- round(100 * prop.table(tb), 1)
      data.frame(Variabel = v,
                 Sebaran = paste(sprintf("%s: %d (%s%%)", names(tb), as.integer(tb), pr), collapse = " | "))
    }))
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$r4 <- renderPlot({
    r <- data.frame(res = resid(model()))
    ggplot(r, aes(sample = res)) +
      stat_qq(color = col_primary, alpha = 0.6) +
      stat_qq_line(color = col_red, linewidth = 1) +
      labs(title = "Q-Q Plot Residual", x = "Kuantil teoritis (normal)", y = "Kuantil residual",
           caption = "Titik mengikuti garis merah = residual mendekati normal") +
      theme_dash()
  })
  output$i_r4 <- renderText({
    p <- shapiro.test(resid(model()))$p.value
    sprintf("Uji Shapiro-Wilk p = %s, %s. Jika ada penyimpangan, penyebab utamanya biasanya siswa dengan G3 = 0 yang membentuk ekor bawah.",
            format.pval(p, digits = 3),
            if (p < 0.05) "residual tidak berdistribusi normal sempurna" else "tidak ada bukti residual menyimpang dari normal")
  })

  output$tbl_vif <- renderTable({
    m <- model(); X <- model.matrix(m)[, -1, drop = FALSE]
    v <- sapply(seq_len(ncol(X)), function(i)
      1 / (1 - summary(lm(X[, i] ~ X[, -i, drop = FALSE]))$r.squared))
    data.frame(Variabel = colnames(X), VIF = v,
               Keterangan = ifelse(v < 5, "Aman (VIF < 5)", "Perlu dicek"))
  }, digits = 2, striped = TRUE, hover = TRUE, width = "100%")

  # ---------- UJI ASUMSI KLASIK ----------
  asumsi <- reactive({
    m <- model(); e <- resid(m); n <- length(e)
    X <- model.matrix(m)[, -1, drop = FALSE]
    sw <- shapiro.test(e)                                    # normalitas
    v  <- sapply(seq_len(ncol(X)), function(i)              # multikolinearitas (VIF)
      1 / (1 - summary(lm(X[, i] ~ X[, -i, drop = FALSE]))$r.squared))
    bp <- n * summary(lm(e^2 ~ X))$r.squared                 # Breusch-Pagan (Koenker)
    p_bp <- pchisq(bp, ncol(X), lower.tail = FALSE)
    dw <- sum(diff(e)^2) / sum(e^2)                          # Durbin-Watson
    p_dw <- 2 * pnorm(-abs(dw - 2) / (2 / sqrt(n)))          # pendekatan normal
    out <- data.frame(
      Asumsi = c("Normalitas residual", "Multikolinearitas", "Homoskedastisitas", "Autokorelasi"),
      Uji = c("Shapiro-Wilk", "VIF maksimum", "Breusch-Pagan", "Durbin-Watson"),
      Statistik = c(sprintf("W = %.3f", sw$statistic), sprintf("%.2f", max(v)),
                    sprintf("LM = %.2f (df = %d)", bp, ncol(X)), sprintf("d = %.3f", dw)),
      pv = c(format.pval(sw$p.value, digits = 3), "-",
             format.pval(p_bp, digits = 3), format.pval(p_dw, digits = 3)),
      Kesimpulan = c(if (sw$p.value >= 0.05) "Terpenuhi" else "Tidak terpenuhi",
                     if (max(v) < 5) "Terpenuhi" else "Perlu dicek",
                     if (p_bp >= 0.05) "Terpenuhi" else "Tidak terpenuhi",
                     if (p_dw >= 0.05) "Terpenuhi" else "Tidak terpenuhi"),
      stringsAsFactors = FALSE)
    names(out)[4] <- "p-value"
    out
  })
  output$tbl_asumsi <- renderTable(asumsi(), striped = TRUE, hover = TRUE, width = "100%")
  output$i_asumsi <- renderUI({
    a <- asumsi()
    tags$ul(
      tags$li(paste0("Normalitas: ", a$Kesimpulan[1], " (", a$Statistik[1], ", p = ", a$`p-value`[1],
                     "). Penyimpangan biasanya berasal dari siswa dengan G3 = 0; dengan n besar, uji koefisien relatif tahan terhadap pelanggaran ini.")),
      tags$li(paste0("Multikolinearitas: ", a$Kesimpulan[2], " (VIF maksimum ", a$Statistik[2],
                     "). VIF di bawah 5 berarti prediktor tidak saling tumpang tindih secara berlebihan.")),
      tags$li(paste0("Homoskedastisitas: ", a$Kesimpulan[3], " (", a$Statistik[3], ", p = ", a$`p-value`[3],
                     "). Jika tidak terpenuhi, galat baku koefisien kurang andal dan dapat dipertimbangkan galat baku robust.")),
      tags$li(paste0("Autokorelasi: ", a$Kesimpulan[4], " (", a$Statistik[4],
                     "). Nilai d yang dekat 2 menunjukkan tidak ada korelasi antarresidual berurutan. ",
                     "Catatan: data ini cross-section (bukan runtun waktu), sehingga uji ini dihitung menurut urutan baris data dan tidak bermakna waktu."))
    )
  })

  output$i_kes <- renderUI({
    d <- data_f(); m <- model(); s <- summary(m); f <- s$fstatistic
    p_f <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    cf  <- s$coefficients[-1, , drop = FALSE]
    pos <- rownames(cf)[cf[, 4] < 0.05 & cf[, 1] > 0]
    neg <- rownames(cf)[cf[, 4] < 0.05 & cf[, 1] < 0]
    nsg <- rownames(cf)[cf[, 4] >= 0.05]
    fmt <- function(x) if (length(x) == 0) "tidak ada" else paste(x, collapse = ", ")
    a <- asumsi()
    tags$ul(
      tags$li(sprintf("Nilai akhir (G3) rata-rata %.2f dari 20 (median %.1f); %d siswa bernilai 0.",
                      mean(d$G3), median(d$G3), sum(d$G3 == 0))),
      tags$li(paste0("Rata-rata G3 menurut keinginan kuliah: ", group_text(d, "higher"),
                     "; menurut riwayat kegagalan: ", group_text(d, "failures_grp"), ".")),
      tags$li(sprintf("Model regresi %s secara simultan (uji F, p = %s) dengan R-squared %.1f%% dan Adjusted R-squared %.1f%%.",
                      if (p_f < 0.05) "signifikan" else "tidak signifikan", format.pval(p_f, digits = 3),
                      100 * s$r.squared, 100 * s$adj.r.squared)),
      tags$li(paste0("Berasosiasi positif dan signifikan dengan G3: ", fmt(pos), ".")),
      tags$li(paste0("Berasosiasi negatif dan signifikan dengan G3: ", fmt(neg), ".")),
      tags$li(paste0("Tidak signifikan pada alpha 5%: ", fmt(nsg), ".")),
      tags$li(paste0("Asumsi klasik: ", paste(sprintf("%s %s", tolower(a$Asumsi), tolower(a$Kesimpulan)), collapse = "; "), "."))
    )
  })
}

shinyApp(ui, server)
