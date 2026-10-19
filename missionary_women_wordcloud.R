
# ============================================================
# R을 활용한 한글 워드클라우드 실습
# 주제: 여성 선교사와 조선 여성교육의 가치관 변화
#
# 학습 과정
# 1. 패키지 설치 및 환경 설정
# 2. 텍스트 데이터 입력
# 3. 텍스트 전처리
# 4. 단어 분리 및 불용어 제거
# 5. 단어 빈도 분석
# 6. 빈도 막대그래프 생성
# 7. 워드클라우드 생성
# 8. 분석 결과 저장
#
# 실행 환경: Windows / RStudio
# ============================================================


# ============================================================
# STEP 1. 패키지 설치 및 환경 설정
# ============================================================

packages <- c(
  "dplyr",
  "stringr",
  "tidytext",
  "ggplot2",
  "ggwordcloud",
  "showtext",
  "sysfonts",
  "tibble"
)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggwordcloud)
library(showtext)
library(sysfonts)
library(tibble)

# 한글 폰트 설정
font_add(
  family = "korean",
  regular = "C:/Windows/Fonts/malgun.ttf"
)

showtext_auto()

# 결과 저장 폴더 확인
getwd()


# ============================================================
# STEP 2. 텍스트 데이터 입력
# ============================================================

text <- paste(
  "선교사들이 빅토리아 가치관으로 한국 여성을 가르치려 했는데,",
  "한국 여성의 지위와 상황이 상대적으로 더 열약하다보니,",
  "그것을 일깨워주기 위해 교육을 했고",
  "(우리는 이 정도는 아니다. 서양에서는 이정도이다)",
  "그것이 도화선이 되었다.",
  "언더우드 여사의 시대적 가치관이 한국 여성에게 영향을 주었다.",
  "학당이나 교회라는 교육 장소가 생기고,",
  "그로 인해 가치관의 혁명이 발생"
)

cat("\n===== 원문 =====\n")
cat(text, "\n")

cat("\n전체 글자 수:", nchar(text), "\n")


# ============================================================
# STEP 3. 텍스트 전처리
# ============================================================

# 동일한 단어의 조사와 표현을 통일
# 원문의 의미를 임의로 바꾸지 않도록 주의

clean_text <- text %>%
  str_replace_all("선교사들이", "선교사") %>%
  str_replace_all(
    "여성을|여성의|여성에게", "여성"
  ) %>%
  str_replace_all(
    "가치관으로|가치관이|가치관의", "가치관"
  ) %>%
  str_replace_all("교육을", "교육") %>%
  str_replace_all("언더우드 여사의", "언더우드") %>%
  str_replace_all("학당이나", "학당") %>%
  str_replace_all("교회라는", "교회") %>%
  str_replace_all("상황이", "상황") %>%
  str_replace_all("지위와", "지위") %>%
  str_replace_all("혁명이", "혁명") %>%
  str_replace_all("장소가", "장소") %>%
  str_replace_all("영향을", "영향")

cat("\n===== 전처리 결과 =====\n")
cat(clean_text, "\n")


# ============================================================
# STEP 4. 단어 분리 및 불용어 제거
# ============================================================

# 한글이 아닌 문자를 기준으로 단어 분리
# 주의: 완전한 형태소 분석은 아님

words <- tibble(text = clean_text) %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "regex",
    pattern = "[^가-힣]+",
    to_lower = FALSE
  )

cat("\n===== 단어 분리 결과 =====\n")
print(words, n = Inf)


# 불용어 설정
stopwords <- c(
  "그것을", "그것이", "그로",
  "인해", "위해", "했고",
  "했는데", "되었다", "주었다",
  "생기고", "더", "이", "그",
  "정도는", "아니다", "우리는",
  "상대적으로", "이정도이다",
  "보니"
)

words_clean <- words %>%
  filter(!word %in% stopwords) %>%
  filter(nchar(word) >= 2)

cat("\n===== 불용어 제거 결과 =====\n")
print(words_clean, n = Inf)


# ============================================================
# STEP 5. 단어 빈도 분석
# ============================================================

word_freq <- words_clean %>%
  count(word, sort = TRUE)

cat("\n===== 전체 단어 빈도 =====\n")
print(word_freq, n = Inf)

# 상위 10개 단어 추출
top10 <- word_freq %>%
  slice_max(
    order_by = n,
    n = 10,
    with_ties = FALSE
  )

cat("\n===== 상위 10개 단어 =====\n")
print(top10)


# ============================================================
# STEP 6. 단어 빈도 막대그래프
# ============================================================

p_bar <- ggplot(
  top10,
  aes(
    x = reorder(word, n),
    y = n
  )
) +
  geom_col(
    fill = "#4477AA",
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "여성 선교사와 조선 여성교육",
    subtitle = "주요 단어 출현 빈도",
    x = "단어",
    y = "출현 횟수"
  ) +
  theme_minimal(base_family = "korean") +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

print(p_bar)


# ============================================================
# STEP 7. 워드클라우드 생성
# ============================================================

# 동일한 결과를 재현하기 위한 난수 설정
set.seed(123)

p_cloud <- ggplot(
  word_freq,
  aes(
    label = word,
    size = n,
    color = n
  )
) +
  geom_text_wordcloud(
    family = "korean",
    eccentricity = 0.65,
    rm_outside = FALSE
  ) +
  scale_size_area(
    max_size = 24
  ) +
  scale_color_gradient(
    low = "#4386B4",
    high = "#C34A4A"
  ) +
  coord_equal() +
  theme_void() +
  labs(
    title = "여성 선교사와 조선 여성교육의 가치관 변화"
  ) +
  theme(
    plot.title = element_text(
      family = "korean",
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(25, 25, 25, 25)
  )

print(p_cloud)


# ============================================================
# STEP 8. 분석 결과 저장
# ============================================================

# 단어 빈도표 CSV 저장
write.csv(
  word_freq,
  file = "missionary_word_frequency.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 막대그래프 저장
ggsave(
  filename = "missionary_word_frequency.png",
  plot = p_bar,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# 워드클라우드 저장
ggsave(
  filename = "missionary_wordcloud.png",
  plot = p_cloud,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat("\n====================================\n")
cat("모든 분석이 완료되었습니다.\n")
cat("CSV 및 PNG 파일이 저장되었습니다.\n")
cat("저장 위치:", getwd(), "\n")
cat("====================================\n")


# ============================================================
# STEP 9. 학생 토론 및 해석 과제
# ============================================================

# 질문 1.
# 가장 자주 등장한 단어는 무엇인가?

# 질문 2.
# 단어 빈도와 의미적 중요성은 동일한가?

# 질문 3.
# '여성', '교육', '가치관'은
# 어떤 종교교육학적 관계를 형성하는가?

# 질문 4.
# 불용어를 변경하면 분석 결과는 어떻게 달라지는가?

# 질문 5.
# 워드클라우드만으로
# '가치관의 혁명'을 입증할 수 있는가?

# ============================================================
# END
# ============================================================


# ============================================================
# STEP 7. 타원형 워드클라우드 생성 (개선 버전)
# ============================================================

library(ggplot2)
library(ggwordcloud)
library(showtext)

showtext_auto()

# 1. 단어 빈도 데이터 확인
print(word_freq)

# 2. 재현 가능한 결과 설정
set.seed(123)

# 3. 타원형 워드클라우드
p_cloud <- ggplot(
  word_freq,
  aes(
    label = word,
    size = n,
    color = n
  )
) +
  
  geom_text_wordcloud(
    family = "korean",
    
    # 타원형 배치
    eccentricity = 0.75,
    
    # 단어가 그림 밖으로 나가지 않도록 설정
    rm_outside = TRUE,
    
    # 단어 배치의 반복 횟수
    max_steps = 10000,
    
    # 단어 사이 여백
    grid_size = 2
  ) +
  
  # 글자 크기 조정
  scale_size_area(
    max_size = 15
  ) +
  
  # 색상 설정
  scale_color_gradient(
    low = "#4386B4",
    high = "#C34A4A"
  ) +
  
  # 비율 유지
  coord_equal(
    expand = TRUE
  ) +
  
  # 배경 제거
  theme_void() +
  
  labs(
    title = "여성 선교사와 조선 여성교육의 가치관 변화"
  ) +
  
  theme(
    plot.title = element_text(
      family = "korean",
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 25)
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(
      30, 40, 30, 40
    ),
    
    legend.position = "none"
  )

# 4. 결과 출력
print(p_cloud)


# ============================================================
# STEP 8. 고해상도 이미지 저장
# ============================================================

ggsave(
  filename = "missionary_wordcloud_ellipse.png",
  plot = p_cloud,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat("타원형 워드클라우드 저장 완료!\n")


# ============================================================
# STEP 7. 타원형 워드클라우드 크기 및 제목 간격 조정
# ============================================================

library(ggplot2)
library(ggwordcloud)
library(showtext)

showtext_auto()

set.seed(123)

p_cloud <- ggplot(
  word_freq,
  aes(
    label = word,
    size = n,
    color = n
  )
) +
  
  geom_text_wordcloud(
    family = "korean",
    
    # 타원형 유지
    eccentricity = 0.75,
    
    # 글자 배치 정밀도
    grid_size = 1,
    
    # 단어 배치 탐색
    max_steps = 20000,
    
    # 배치 영역 밖의 단어 제거
    rm_outside = TRUE
  ) +
  
  # 글자 크기 확대: 기존 15 -> 22
  scale_size_area(
    max_size = 22
  ) +
  
  # 색상 설정
  scale_color_gradient(
    low = "#4386B4",
    high = "#C34A4A"
  ) +
  
  # 그래프 영역의 불필요한 여백 축소
  coord_equal(
    expand = FALSE
  ) +
  
  theme_void() +
  
  labs(
    title = "여성 선교사와 조선 여성교육의 가치관 변화"
  ) +
  
  theme(
    plot.title = element_text(
      family = "korean",
      size = 19,
      face = "bold",
      hjust = 0.5,
      
      # 제목 아래 여백 축소
      margin = margin(b = 5)
    ),
    
    # 전체 그림 여백 축소
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 10
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.position = "none"
  )

# 결과 출력
print(p_cloud)


# ============================================================
# STEP 8. 저장 이미지의 가로·세로 비율 조정
# ============================================================

ggsave(
  filename = "missionary_wordcloud_final.png",
  plot = p_cloud,
  
  # 기존 12 x 7에서 세로 높이 축소
  width = 12,
  height = 5,
  
  dpi = 300,
  bg = "white"
)

cat("워드클라우드 저장 완료!\n")


