# =====================================================================
# 교육용 R 코드: 여성 선교사 조별 응답 의미론적 워드클라우드
# 파일명: women_missionary_wordcloud_teaching.R
#
# 수업 목표
# 1. Excel 원자료를 R로 불러온다.
# 2. Wide → Long 변환을 이해한다.
# 3. 텍스트 정제와 불용어 제거를 수행한다.
# 4. 유사 표현을 연구 주제에 맞는 의미 개념으로 정규화한다.
# 5. 빈도와 의미 가중치를 구분한다.
# 6. 첫 번째 시각화의 문제를 확인한다.
# 7. layout()을 이용해 제목 겹침과 단어 잘림을 개선한다.
# 8. 최종 2×2 워드클라우드를 비교·해석한다.
#
# 주의:
# semantic_weight와 display_weight는 원자료의 '순수 빈도'가 아닙니다.
# 연구 질문에 따른 의미 강조와 시각적 보정을 위한 값입니다.
# =====================================================================


# =====================================================================
# STEP 0. 필요한 패키지
# =====================================================================

# 처음 사용하는 컴퓨터에서는 아래 줄의 #을 제거하고 한 번만 실행합니다.
# install.packages(c(
#   "readxl", "dplyr", "tidyr", "stringr",
#   "tidytext", "wordcloud", "RColorBrewer"
# ))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)


# =====================================================================
# STEP 1. Excel 원자료 불러오기
# =====================================================================

# 현재 Working Directory에 아래 Excel 파일이 있어야 합니다.
# 필요하면 파일명만 실제 파일명에 맞게 수정하십시오.
excel_file <- "조별응답_261007.xlsx"

getwd()

df <- read_excel(excel_file)

# [수업 확인]
print(df)
names(df)

# 예상 열 이름:
# "1조" "2조" "3조" "4조"


# =====================================================================
# STEP 2. Wide format → Long format
# =====================================================================

# 원자료:
# 1조 | 2조 | 3조 | 4조
#
# 변환 후:
# group | text

responses <- df %>%
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "text"
  ) %>%
  filter(
    !is.na(text),
    text != ""
  )

# [수업 확인]
print(responses)


# =====================================================================
# STEP 3. 텍스트 기본 정제
# =====================================================================

responses_clean <- responses %>%
  mutate(
    text = as.character(text),

    # 특수문자 제거
    text = str_replace_all(text, "[[:punct:]]", " "),

    # 숫자 제거
    text = str_replace_all(text, "[0-9]", " "),

    # 중복 공백 제거
    text = str_squish(text)
  )

# [수업 확인]
print(responses_clean)


# =====================================================================
# STEP 4. 단어 단위로 분리(Tokenization)
# =====================================================================

words <- responses_clean %>%
  unnest_tokens(
    word,
    text,
    token = "words"
  ) %>%

  # 한 글자 단어 제거
  filter(str_length(word) >= 2)

# [수업 확인]
print(head(words, 30))


# =====================================================================
# STEP 5. 공통 불용어 제거
# =====================================================================

stop_words_kor <- c(
  "있었다", "있었던", "있으며", "있고", "있다", "있는데", "있다고",
  "되었다", "되면서", "되었다고",
  "하면서", "통해서", "통하여", "속에서", "때문에",
  "생각", "생각된다", "생각보다",
  "사람", "사람들",
  "다양한", "여러", "새로운", "많은", "많았다",
  "초기", "초기에",
  "결과", "결과적으로",
  "모습", "방식",
  "자신", "자신의",
  "이러한", "그러한",
  "그리고", "그러나", "하지만",
  "단순히", "단순한",
  "안에서", "밖에서",
  "이를", "보다",
  "통해", "대한", "위한",
  "것을", "것이", "것은",
  "하는", "하게", "하며",
  "수도", "정도"
)

words_clean <- words %>%
  filter(!word %in% stop_words_kor)

# [수업 질문]
# Q. 불용어를 제거하기 전과 후에 핵심 단어가 더 잘 보이는가?


# =====================================================================
# STEP 6. 의미 정규화(Semantic Normalization)
# =====================================================================

# 같은 의미를 가진 여러 표현을 하나의 분석 개념으로 통합합니다.
#
# 예:
# 변화 / 변형          → 변화
# 주체적 / 주체로      → 주체성
# 교육 / 가르침        → 교육
# 상호 / 교류 / 만남   → 상호작용

words_semantic <- words_clean %>%
  mutate(
    word = case_when(
      str_detect(word, "변화|변형") ~ "변화",
      str_detect(word, "주체") ~ "주체성",
      str_detect(word, "교육|가르") ~ "교육",
      str_detect(word, "상호|교류|만남|마주") ~ "상호작용",
      str_detect(word, "리더") ~ "리더십",
      str_detect(word, "역할") ~ "역할확대",
      str_detect(word, "독립") ~ "독립성",
      str_detect(word, "가정|가사") ~ "가정적영역",
      str_detect(word, "사적") ~ "사적영역",
      str_detect(word, "공적") ~ "공적영역",
      str_detect(word, "사회") ~ "사회적영역",
      str_detect(word, "정치") ~ "정치적환경",
      str_detect(word, "역사") ~ "역사적맥락",
      str_detect(word, "환경") ~ "환경",
      str_detect(word, "현실") ~ "현실",
      str_detect(word, "책임") ~ "책임감",
      str_detect(word, "열정") ~ "열정",
      str_detect(word, "선교") ~ "선교활동",
      str_detect(word, "복음") ~ "복음화",
      str_detect(word, "여학생") ~ "여학생",
      str_detect(word, "성장") ~ "성장",
      str_detect(word, "관계") ~ "관계형성",
      TRUE ~ word
    )
  )


# =====================================================================
# STEP 7. 의미가 약한 표현 추가 제거
# =====================================================================

semantic_stopwords <- c(
  "나라에서는", "나라는", "넘어서야겠을", "그렇기에",
  "한다고", "했음", "하여금", "밖에", "육아의",
  "자기가", "대상이", "맥락이", "가지고", "접할",
  "종교의", "이를", "넘어", "높는", "갖추게",
  "개인의", "그래서", "따라", "머물기에"
)

words_semantic <- words_semantic %>%
  filter(!word %in% semantic_stopwords)


# =====================================================================
# STEP 8. 조별 단어 빈도 계산
# =====================================================================

word_count_raw <- words_semantic %>%
  count(
    group,
    word,
    sort = TRUE
  )

# [수업 확인]
# 이것이 연구자의 가중치를 넣기 전 '빈도'입니다.
print(word_count_raw)


# =====================================================================
# STEP 9. 의미 가중치 부여
# =====================================================================

# 중요:
# semantic_weight는 실제 출현 빈도가 아닙니다.
# 연구 질문에서 중요한 의미구조를 시각적으로 강조하기 위한 값입니다.
#
# 따라서 논문이나 발표에서는
# "raw frequency"와 "research-informed semantic weighting"을
# 반드시 구분해서 설명해야 합니다.

word_count <- word_count_raw %>%
  mutate(
    semantic_weight = case_when(

      # ---------------------------------------------------------------
      # 1조: 상호작용 → 교육 → 주체성 → 변화
      # ---------------------------------------------------------------
      group == "1조" & word == "상호작용" ~ 2.0,
      group == "1조" & word == "주체성" ~ 2.0,
      group == "1조" & word == "변화" ~ 1.8,
      group == "1조" & word == "교육" ~ 1.7,
      group == "1조" & word == "역할확대" ~ 1.5,
      group == "1조" & word == "리더십" ~ 1.5,

      # ---------------------------------------------------------------
      # 2조: 역사·사회적 현실 → 환경 → 리더십 → 변화
      # ---------------------------------------------------------------
      group == "2조" & word == "환경" ~ 1.8,
      group == "2조" & word == "현실" ~ 1.8,
      group == "2조" & word == "역사적맥락" ~ 1.8,
      group == "2조" & word == "사회적영역" ~ 1.7,
      group == "2조" & word == "정치적환경" ~ 1.7,
      group == "2조" & word == "변화" ~ 2.0,
      group == "2조" & word == "리더십" ~ 1.8,

      # ---------------------------------------------------------------
      # 3조: 가정·사적 영역 → 사회·공적 영역 → 역할 확대
      # ---------------------------------------------------------------
      group == "3조" & word == "가정적영역" ~ 1.7,
      group == "3조" & word == "사적영역" ~ 1.8,
      group == "3조" & word == "사회적영역" ~ 1.8,
      group == "3조" & word == "공적영역" ~ 2.0,
      group == "3조" & word == "역할확대" ~ 1.8,
      group == "3조" & word == "독립성" ~ 2.0,
      group == "3조" & word == "주체성" ~ 1.8,
      group == "3조" & word == "리더십" ~ 1.8,

      # ---------------------------------------------------------------
      # 4조: 관계·교육 → 책임·열정 → 주체적 변화
      # ---------------------------------------------------------------
      group == "4조" & word == "관계형성" ~ 1.8,
      group == "4조" & word == "상호작용" ~ 2.0,
      group == "4조" & word == "교육" ~ 1.7,
      group == "4조" & word == "책임감" ~ 1.8,
      group == "4조" & word == "열정" ~ 1.7,
      group == "4조" & word == "변화" ~ 2.0,
      group == "4조" & word == "주체성" ~ 1.8,
      group == "4조" & word == "리더십" ~ 1.8,

      # 그 외 단어
      TRUE ~ 1
    ),

    weighted_n = n * semantic_weight
  )


# =====================================================================
# STEP 10. 각 조의 핵심 단어 선택
# =====================================================================

group1_final <- word_count %>%
  filter(group == "1조") %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 24)

group2_final <- word_count %>%
  filter(group == "2조") %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 24)

group3_final <- word_count %>%
  filter(group == "3조") %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 24)

group4_final <- word_count %>%
  filter(group == "4조") %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 22)


# =====================================================================
# STEP 11. 긴 단어에 대한 '시각적' 크기 보정
# =====================================================================

# 분석값 weighted_n은 변경하지 않습니다.
# display_weight는 그림을 읽기 쉽게 만들기 위한 별도 변수입니다.

group1_final <- group1_final %>%
  mutate(display_weight = weighted_n)

group2_final <- group2_final %>%
  mutate(display_weight = weighted_n)

group3_final <- group3_final %>%
  mutate(
    display_weight = case_when(
      word == "가정적영역" ~ weighted_n * 0.78,
      word == "사회적영역" ~ weighted_n * 0.78,
      word == "공적영역" ~ weighted_n * 0.82,
      word == "사적영역" ~ weighted_n * 0.82,
      word == "역할확대" ~ weighted_n * 0.82,
      TRUE ~ weighted_n
    )
  )

group4_final <- group4_final %>%
  mutate(
    display_weight = case_when(
      word == "상호작용" ~ weighted_n * 0.90,
      word == "관계형성" ~ weighted_n * 0.90,
      word == "선교활동" ~ weighted_n * 0.90,
      word == "역할확대" ~ weighted_n * 0.90,
      TRUE ~ weighted_n
    )
  )

# [수업 확인]
print(group1_final)
print(group2_final)
print(group3_final)
print(group4_final)


# =====================================================================
# STEP 12. VERSION 1 — 첫 번째 2×2 워드클라우드
# =====================================================================

# 수업에서는 이 부분을 먼저 실행해 보십시오.
#
# 관찰할 점:
# - 제목과 워드클라우드가 겹칠 수 있음
# - 긴 단어가 패널 경계에 가까워질 수 있음
# - 조별 cloud 크기가 서로 다르게 느껴질 수 있음
#
# 실행 후 다음 질문:
# "데이터의 문제인가, 시각화 레이아웃의 문제인가?"

graphics.off()

par(
  mfrow = c(2, 2),
  mar = c(1, 1, 4.5, 1)
)

set.seed(1234)
wordcloud(
  words = group1_final$word,
  freq = group1_final$display_weight,
  min.freq = 1,
  max.words = 24,
  random.order = FALSE,
  rot.per = 0,
  scale = c(3.4, 0.8),
  colors = brewer.pal(8, "Reds"),
  random.color = TRUE,
  fixed.asp = FALSE
)
title(
  main = "1조: 상호작용을 통한\n여성 선교사의 주체적 변화",
  cex.main = 1.15,
  line = 2
)

set.seed(1234)
wordcloud(
  words = group2_final$word,
  freq = group2_final$display_weight,
  min.freq = 1,
  max.words = 24,
  random.order = FALSE,
  rot.per = 0,
  scale = c(3.4, 0.8),
  colors = brewer.pal(8, "Blues"),
  random.color = TRUE,
  fixed.asp = FALSE
)
title(
  main = "2조: 역사·사회적 현실과의 만남을 통한\n리더십의 변화",
  cex.main = 1.15,
  line = 2
)

set.seed(1234)
wordcloud(
  words = group3_final$word,
  freq = group3_final$display_weight,
  min.freq = 1,
  max.words = 24,
  random.order = FALSE,
  rot.per = 0,
  scale = c(3.0, 0.75),
  colors = brewer.pal(8, "Greens"),
  random.color = TRUE,
  fixed.asp = FALSE
)
title(
  main = "3조: 가정적 여성상에서\n사회적·공적 주체로의 확장",
  cex.main = 1.15,
  line = 2
)

set.seed(1234)
wordcloud(
  words = group4_final$word,
  freq = group4_final$display_weight,
  min.freq = 1,
  max.words = 22,
  random.order = FALSE,
  rot.per = 0,
  scale = c(3.8, 0.9),
  colors = brewer.pal(8, "Purples"),
  random.color = TRUE,
  fixed.asp = FALSE
)
title(
  main = "4조: 관계와 교육적 실천을 통한\n여성 선교사의 변화",
  cex.main = 1.15,
  line = 2
)

par(mfrow = c(1, 1))


# =====================================================================
# STEP 13. VERSION 2 — 파라미터 조정
# =====================================================================

# VERSION 1의 문제를 완화하기 위해:
# - mar: 좌우 여백 확대
# - max.words: 단어 수 축소
# - scale: 긴 단어가 많은 조의 글자 크기 축소
#
# 이 단계는 '파라미터 튜닝'의 예입니다.

graphics.off()

par(
  mfrow = c(2, 2),
  mar = c(1.5, 2.0, 4.5, 2.0),
  oma = c(1, 1, 1, 1)
)

set.seed(1234)
wordcloud(
  group1_final$word, group1_final$display_weight,
  min.freq = 1, max.words = 22,
  random.order = FALSE, rot.per = 0,
  scale = c(3.2, 0.75),
  colors = brewer.pal(8, "Reds"),
  random.color = TRUE, fixed.asp = FALSE
)
title(
  "1조: 상호작용을 통한\n여성 선교사의 주체적 변화",
  cex.main = 1.05, line = 1.5
)

set.seed(1234)
wordcloud(
  group2_final$word, group2_final$display_weight,
  min.freq = 1, max.words = 20,
  random.order = FALSE, rot.per = 0,
  scale = c(2.7, 0.65),
  colors = brewer.pal(8, "Blues"),
  random.color = TRUE, fixed.asp = FALSE
)
title(
  "2조: 역사·사회적 현실과의 만남을 통한\n리더십의 변화",
  cex.main = 1.05, line = 1.5
)

set.seed(1234)
wordcloud(
  group3_final$word, group3_final$display_weight,
  min.freq = 1, max.words = 20,
  random.order = FALSE, rot.per = 0,
  scale = c(2.5, 0.60),
  colors = brewer.pal(8, "Greens"),
  random.color = TRUE, fixed.asp = FALSE
)
title(
  "3조: 가정적 여성상에서\n사회적·공적 주체로의 확장",
  cex.main = 1.05, line = 1.5
)

set.seed(1234)
wordcloud(
  group4_final$word, group4_final$display_weight,
  min.freq = 1, max.words = 20,
  random.order = FALSE, rot.per = 0,
  scale = c(2.8, 0.65),
  colors = brewer.pal(8, "Purples"),
  random.color = TRUE, fixed.asp = FALSE
)
title(
  "4조: 관계와 교육적 실천을 통한\n여성 선교사의 변화",
  cex.main = 1.05, line = 1.5
)

par(mfrow = c(1, 1))


# =====================================================================
# STEP 14. VERSION 3 / FINAL — 구조 자체를 수정
# =====================================================================

# 핵심 수정:
#
# mfrow 안에서 title()과 wordcloud()가 같은 공간을 사용하는 문제를
# 단순히 scale만 조정하지 않고 layout()으로 구조적으로 해결합니다.
#
# 화면 구조:
#
# ┌─────────────────────┬─────────────────────┐
# │      1조 제목       │      2조 제목       │
# ├─────────────────────┼─────────────────────┤
# │  1조 워드클라우드   │  2조 워드클라우드   │
# ├─────────────────────┼─────────────────────┤
# │      3조 제목       │      4조 제목       │
# ├─────────────────────┼─────────────────────┤
# │  3조 워드클라우드   │  4조 워드클라우드   │
# └─────────────────────┴─────────────────────┘
#
# 또한 최종 관찰에서 상대적으로 작았던
# 1조와 4조의 scale을 확대합니다.

graphics.off()

layout_matrix <- matrix(
  c(
    1, 3,
    2, 4,
    5, 7,
    6, 8
  ),
  nrow = 4,
  byrow = TRUE
)

layout(
  layout_matrix,
  heights = c(1.0, 4.0, 1.0, 4.0),
  widths = c(1, 1)
)


# ---------------------------------------------------------------------
# 1조 제목
# ---------------------------------------------------------------------

par(mar = c(0, 1, 0, 1))
plot.new()

text(
  0.5, 0.45,
  labels = "1조: 상호작용을 통한\n여성 선교사의 주체적 변화",
  cex = 1.25,
  font = 2,
  family = "Malgun Gothic"
)


# ---------------------------------------------------------------------
# 1조 워드클라우드
# ---------------------------------------------------------------------

par(mar = c(0.3, 1.0, 0.3, 1.0))
set.seed(1234)

wordcloud(
  words = group1_final$word,
  freq = group1_final$display_weight,
  min.freq = 1,
  max.words = 22,
  random.order = FALSE,
  rot.per = 0,

  # 최종 수정: 1조 확대
  scale = c(3.8, 0.90),

  colors = brewer.pal(8, "Reds"),
  random.color = TRUE,
  fixed.asp = FALSE
)


# ---------------------------------------------------------------------
# 2조 제목
# ---------------------------------------------------------------------

par(mar = c(0, 1, 0, 1))
plot.new()

text(
  0.5, 0.45,
  labels = "2조: 역사·사회적 현실과의 만남을 통한\n리더십의 변화",
  cex = 1.25,
  font = 2,
  family = "Malgun Gothic"
)


# ---------------------------------------------------------------------
# 2조 워드클라우드
# ---------------------------------------------------------------------

par(mar = c(0.3, 1.0, 0.3, 1.0))
set.seed(1234)

wordcloud(
  words = group2_final$word,
  freq = group2_final$display_weight,
  min.freq = 1,
  max.words = 22,
  random.order = FALSE,
  rot.per = 0,
  scale = c(2.8, 0.68),
  colors = brewer.pal(8, "Blues"),
  random.color = TRUE,
  fixed.asp = FALSE
)


# ---------------------------------------------------------------------
# 3조 제목
# ---------------------------------------------------------------------

par(mar = c(0, 1, 0, 1))
plot.new()

text(
  0.5, 0.45,
  labels = "3조: 가정적 여성상에서\n사회적·공적 주체로의 확장",
  cex = 1.25,
  font = 2,
  family = "Malgun Gothic"
)


# ---------------------------------------------------------------------
# 3조 워드클라우드
# ---------------------------------------------------------------------

par(mar = c(0.3, 1.0, 0.3, 1.0))
set.seed(1234)

wordcloud(
  words = group3_final$word,
  freq = group3_final$display_weight,
  min.freq = 1,
  max.words = 22,
  random.order = FALSE,
  rot.per = 0,

  # 긴 합성어가 많으므로 조금 작게
  scale = c(2.65, 0.65),

  colors = brewer.pal(8, "Greens"),
  random.color = TRUE,
  fixed.asp = FALSE
)


# ---------------------------------------------------------------------
# 4조 제목
# ---------------------------------------------------------------------

par(mar = c(0, 1, 0, 1))
plot.new()

text(
  0.5, 0.45,
  labels = "4조: 관계와 교육적 실천을 통한\n여성 선교사의 변화",
  cex = 1.25,
  font = 2,
  family = "Malgun Gothic"
)


# ---------------------------------------------------------------------
# 4조 워드클라우드
# ---------------------------------------------------------------------

par(mar = c(0.3, 1.0, 0.3, 1.0))
set.seed(1234)

wordcloud(
  words = group4_final$word,
  freq = group4_final$display_weight,
  min.freq = 1,
  max.words = 22,
  random.order = FALSE,
  rot.per = 0,

  # 최종 수정: 4조 확대
  scale = c(4.0, 0.95),

  colors = brewer.pal(8, "Purples"),
  random.color = TRUE,
  fixed.asp = FALSE
)

layout(1)


# =====================================================================
# STEP 15. 수업 마무리
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("여성 선교사 조별 응답 의미론적 워드클라우드 분석 완료\n")
cat("============================================================\n")
cat("\n")
cat("수업 핵심:\n")
cat("1. Raw text → Cleaning → Tokenization\n")
cat("2. Stopword removal → Semantic normalization\n")
cat("3. Raw frequency와 연구자 의미 가중치의 구분\n")
cat("4. 첫 시각화 → 문제 발견 → parameter tuning\n")
cat("5. mfrow의 한계 → layout()을 이용한 구조적 수정\n")
cat("6. 최종 2×2 비교와 조별 의미구조 해석\n")
cat("\n")
cat("다음 단계:\n")
cat("Word Cloud의 단어 수준 분석을 Transformer embedding의\n")
cat("문장·의미 수준 분석과 비교할 수 있습니다.\n")
cat("============================================================\n")

