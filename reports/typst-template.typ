// Modelo Typst do relatorio do Check-in 2, na identidade "Manifesto de carga"
// (docs/referencias/local/identidade-visual.md). Substitui o parcial
// typst-template.typ do Quarto e mantem a assinatura de article().

#let cor = (
  papel: rgb("#F4F1EA"),
  cartao: rgb("#FBF9F4"),
  kraft: rgb("#E7DCC6"),
  marinho: rgb("#0C2340"),
  laranja: rgb("#E8641E"),
  laranja-texto: rgb("#B7440E"),
  laranja-marinho: rgb("#F07F3C"),
  secundario: rgb("#3E5266"),
  secundario-marinho: rgb("#A9B8C6"),
  rotulo: rgb("#5E6B78"),
  linha: rgb("#D8D0C0"),
)

#let fonte-texto = "IBM Plex Sans"
#let fonte-mono = "IBM Plex Mono"
#let fonte-titulo = "Big Shoulders Stencil"

// O rastreamento tem as 8 secoes do Check-in 2.
#let total-secoes = 8

// "Secao 3 — Descritiva" -> (3, "Descritiva"); sem o prefixo -> (none, titulo).
// O arquivo passa pelo template do Pandoc, por isso o cifrao do regex vem dobrado.
#let partes-secao(titulo) = {
  let m = titulo.match(regex("^Seção\s+(\d+)\s*[—–-]\s*(.+)$$"))
  if m == none { (none, titulo) } else { (int(m.captures.at(0)), m.captures.at(1)) }
}

#let rotulo-mono(corpo, cor-texto: cor.rotulo, tamanho: 7.5pt) = text(
  font: fonte-mono, weight: 500, size: tamanho, tracking: 0.08em, fill: cor-texto, upper(corpo),
)

// Bloco de numero grande, usado na pagina "Em resumo".
#let numero(valor, rotulo, destaque: false) = block(
  width: 100%, inset: (x: 10pt, y: 9pt), fill: cor.cartao, stroke: (left: 2.5pt + if destaque { cor.laranja } else { cor.marinho }),
  breakable: false,
)[
  #text(font: fonte-titulo, weight: 800, size: 26pt, fill: cor.marinho, valor)
  #v(-6pt)
  #text(size: 8.5pt, fill: cor.secundario, rotulo)
]

// Etiqueta de status por texto e cor (identidade: DECISAO em contorno, ATENCAO em laranja).
#let etiqueta(texto, tipo: "decisao") = box(
  inset: (x: 4pt, y: 2pt), outset: (y: 1pt), radius: 1.5pt,
  fill: if tipo == "atencao" { cor.laranja } else if tipo == "corrigido" { cor.marinho } else { none },
  stroke: if tipo == "decisao" { 0.6pt + cor.marinho } else { none },
  text(font: fonte-mono, weight: 600, size: 6.5pt, tracking: 0.08em,
       fill: if tipo == "corrigido" { cor.papel } else { cor.marinho }, upper(texto)),
)

// Linha com as paradas das secoes: passadas em marinho, atual em laranja, futuras em contorno.
#let rastreamento(atual) = {
  let raio = 2.6pt
  let parada(n) = {
    let (preenche, borda) = if atual == none { (none, cor.linha) }
      else if n < atual { (cor.marinho, cor.marinho) }
      else if n == atual { (cor.laranja, cor.laranja) }
      else { (cor.papel, cor.rotulo) }
    box(circle(radius: raio, fill: preenche, stroke: 0.7pt + borda))
  }
  let trilho = box(width: 1fr, height: raio * 2, align(horizon, line(length: 100%, stroke: 0.6pt + cor.linha)))
  box(width: 100%, {
    for n in range(1, total-secoes + 1) {
      parada(n)
      if n < total-secoes { trilho }
    }
  })
}

#let rodape(titulo-doc) = context {
  let anteriores = query(heading.where(level: 1)).filter(h => h.location().page() <= here().page())
  let atual = if anteriores.len() == 0 { none } else {
    partes-secao(content-to-string(anteriores.last().body)).at(0)
  }
  set text(font: fonte-mono, size: 7pt, fill: cor.rotulo)
  grid(
    columns: (auto, 1fr, auto), column-gutter: 10pt, align: horizon,
    rotulo-mono(if atual == none { titulo-doc } else { "Seção " + str(atual) + " de " + str(total-secoes) }),
    rastreamento(atual),
    rotulo-mono(str(here().page()), cor-texto: cor.marinho),
  )
}

// Arco de rota (origem -> destino) da capa.
#let arco-rota(largura) = {
  let altura = largura * 0.32
  box(width: largura, height: altura, {
    place(curve(
      stroke: (paint: cor.laranja, thickness: 1.6pt, dash: (6pt, 4pt)),
      curve.move((0pt, altura)),
      curve.quad((largura / 2, -altura * 0.9), (largura, altura)),
    ))
    place(dx: -4pt, dy: altura - 4pt, circle(radius: 4pt, fill: cor.laranja))
    place(dx: largura - 5pt, dy: altura - 5pt, circle(radius: 5pt, fill: none, stroke: 1.6pt + cor.laranja))
  })
}

#let capa(title, subtitle, authors, date) = page(
  fill: cor.marinho, margin: (x: 22mm, y: 24mm), footer: none, header: none,
)[
  #set text(fill: cor.papel)
  #rotulo-mono("Manifesto de carga · Grupo 6 · ICD", cor-texto: cor.secundario-marinho, tamanho: 8pt)
  #v(1fr)
  #arco-rota(100%)
  #v(18pt)
  #text(font: fonte-titulo, weight: 800, size: 46pt, fill: cor.papel, upper(title))
  #if subtitle != none {
    v(4pt)
    text(size: 13pt, fill: cor.secundario-marinho, subtitle)
  }
  #v(1fr)
  #line(length: 100%, stroke: 0.6pt + cor.secundario)
  #v(6pt)
  #grid(
    columns: (1fr, auto), align: (left + top, right + top),
    {
      if authors != none and authors != () {
        for a in authors [#text(weight: 600, a.name) \ ]
      }
      text(size: 9pt, fill: cor.secundario-marinho)[Commodity Flow Survey 2022 · PUMS (Census/BTS)]
    },
    if date != none { rotulo-mono(date, cor-texto: cor.laranja-marinho, tamanho: 9pt) },
  )
]

#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "pt",
  region: "BR",
  font: none,
  fontsize: 9.5pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  let titulo-doc = if title == none { "" } else { content-to-string(title) }
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()

  set text(lang: lang, region: region, font: fonte-texto, size: fontsize, fill: cor.marinho)
  set par(justify: false, leading: 0.62em, spacing: 0.95em)
  show raw: set text(font: fonte-mono, size: 0.92em)
  show raw.where(block: false): it => box(
    fill: cor.kraft, inset: (x: 2.5pt), outset: (y: 2.5pt), radius: 1.5pt, text(weight: 500, it),
  )
  show link: set text(fill: cor.laranja-texto)
  show strong: set text(weight: 600)
  set list(marker: text(fill: cor.laranja, "▪"), indent: 2pt, body-indent: 6pt)
  set enum(indent: 2pt, body-indent: 6pt)

  // Nivel 1: cada secao abre pagina, com o rotulo "SECAO 0N" e o titulo em estencil.
  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    let (n, nome) = partes-secao(content-to-string(it.body))
    block(above: 0pt, below: 16pt, width: 100%, {
      if n != none {
        rotulo-mono("Seção " + if n < 10 { "0" } + str(n), cor-texto: cor.laranja-texto, tamanho: 8.5pt)
        v(-2pt)
      }
      text(font: fonte-titulo, weight: 800, size: 30pt, fill: cor.marinho, upper(nome))
      v(-4pt)
      line(length: 100%, stroke: 0.6pt + cor.linha)
    })
  }
  show heading.where(level: 2): set text(weight: 600, size: 12.5pt, fill: cor.marinho)
  show heading.where(level: 2): set block(above: 18pt, below: 8pt)
  show heading.where(level: 3): set text(weight: 600, size: 10pt, fill: cor.secundario)
  show heading.where(level: 3): set block(above: 12pt, below: 6pt)

  // Tabelas: cabecalho kraft, linhas em cartao e reguas finas.
  set table(
    inset: (x: 6pt, y: 4.5pt),
    fill: (_, y) => if y == 0 { cor.kraft } else { cor.cartao },
    stroke: (_, y) => (bottom: 0.5pt + cor.linha),
  )
  show table: set text(size: 8pt)
  show table.cell.where(y: 0): set text(weight: 600)
  show figure.where(kind: table): set figure.caption(position: top)
  show figure.caption: it => align(left, text(size: 8.5pt, weight: 600, fill: cor.secundario, it.body))
  show figure: set block(above: 10pt, below: 12pt)
  show outline.entry.where(level: 1): set text(weight: 600)
  set outline.entry(fill: repeat(text(fill: cor.linha, "·"), gap: 2pt))

  set page(
    paper: "a4",
    fill: cor.papel,
    margin: (x: 20mm, top: 18mm, bottom: 22mm),
    numbering: none,
    footer-descent: 35%,
    footer: rodape(titulo-doc),
  )

  if title != none { capa(title, subtitle, authors, date) }

  if toc {
    block(below: 14pt, text(font: fonte-titulo, weight: 800, size: 30pt, upper(if toc_title == none { "Conteúdo" } else { toc_title })))
    outline(title: none, depth: toc_depth, indent: toc_indent)
  }

  doc
}
