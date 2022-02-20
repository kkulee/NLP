# 0. Import library
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(tidyr) # separate()제공
library(igraph)
library(ggraph)
library(tidygraph)
library(topicmodels) # 토픽모형 방법1
library(textmineR) # 토픽모형 방법2
library(stringr) # 문자열처리


# 1. 감성사전 불러오기
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")


# 2. 감성사전 만들기
# 2-1. 특정 단어 삭제
bing <- bing %>% filter(!word %in% c("refund", "refunded"))

# 2-2. 특정 단어 추가(예: 부정어 추가)
nword <- data.frame(word=c("refund", "refunded"), sentiment="negative")
bing <- rbind(bing, nword)


# 3. 데이터 불러오기 및 정제
df <- read.csv("roblox.csv")
df$content <- str_to_lower(df$content) # 대문자 소문자 변환
df$content <- gsub("\\+d", "", df$content) # 숫자 제거
df$content <- gsub("[[:punct:]]", "", df$content) # 특수문자 제거
df$content <- gsub("roblox|game|games|dont|im|wont|ive|釋|뤲땐", "", df$content) # 특정 단어 제거

df1 <- data.frame(rid=c(1:nrow(df)), df) #리뷰 id 생성


# 4. 시각화
# 4-1. 빈도테이블
df2 <- df1 %>% unnest_tokens(word, content, token = "ngrams", n = 1) # 정돈 데이터(tidy text)
data(stop_words) # 불용어(stopword) 사전 가져오기
df2 <- df2 %>% anti_join(stop_words) # 불용어 사전에 있는 단어 제거
df3 <- df2 %>% count(word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  filter(n > 150)

g <- ggplot(df3, aes(word,n)) +
  geom_col(fill='aquamarine3') +
  xlab(NULL) +
  theme_bw() +
  coord_flip()
g

# 4-2. 워드크라우드 1
wc1 <- df3 %>% wordcloud2()
wc1

# 4-3. 워드크라우드 2
wc2 <- df3 %>% wordcloud2(size = 1.2, color = "random-dark", fontFamily = 'Tahoma', minRotation = -pi/6, maxRotation = -pi/6)
wc2

# 4-4. 특정 감정 표현 단어 추출(nrc)
nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

# 4-5. 정돈데이터와 감정 용어집 결합
word_anger <- df2 %>%
  inner_join(nrc_anger) %>%
  count(word, sort=TRUE) %>%
  filter(n > 5)

# 4-6. 워드크라우드 3
wc3 <- word_anger %>% wordcloud2(size = 1.2, color = "random-dark", fontFamily = 'Tahoma', minRotation = -pi/2, maxRotation = -pi/2)
wc3


# 5. 감성분석
# 5-1. 문서와 감성사전을 단어를 기준으로 매칭하여 결합
text_afinn <- df2 %>%
  inner_join(afinn, by = "word")

# 5-2. 단어 요약표
word_summary <- text_afinn %>%
  group_by(word) %>%
  summarise(count_word = n(), afinn_score=max(value)) %>%
  arrange(desc(count_word))

# 5-3. 단어 감성 기여도
contributions <- df2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(빈도 = n(), 기여도 = sum(value))

contributions %>%
  top_n(25, abs(기여도)) %>%
  mutate(word = reorder(word, 기여도)) %>%
  ggplot(aes(word, 기여도, fill = 기여도 > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +theme_bw()

# 5-4. 리뷰 단위로 단어 감성지수 평균 계산
sentiment_messages <- df2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(rid) %>%
  summarize(sentiment = mean(value), words = n()) %>%
  ungroup() %>%
  filter(words >= 5)

# 5-5. 긍정리뷰 순 정렬
prewviews <- sentiment_messages %>%
  arrange(desc(sentiment)) # 내림차순 정렬

# 5-6. 부정리뷰 순 정렬
nrewviews <- sentiment_messages %>%
  arrange(sentiment) # 오름차순 정렬

# 5-7. 원본 리뷰 확인
original_review <- df1 %>% filter(rid == 11)
original_review$content


# 6. W3 바이그램 빈도/워드네트워크
# 6-1. 바이그램 적용 후 정제
df1$content <- gsub("daters|dating", "date", df1$content)
df1$content <- gsub("playing", "play", df1$content)
df1$content <- gsub("ive", "", df1$content)
df4 <- df1 %>% unnest_tokens(bigram, content, token = "ngrams", n = 2) # 바이그램 정돈 데이터(tidy text)

# 6-2. 바이그램 분리 후 stop word 제거
df4_1 <- df4 %>% separate(bigram, c("word1", "word2"), sep = " ")
df4_2 <- df4_1 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# 6-3. 바이그램 결합
df4_3 <- df4_2 %>% unite(bigram, word1, word2, sep = " ")

# 6-4. 바이그램 빈도 테이블
df5 <- df4_3 %>% count(bigram, sort=TRUE) %>%
  mutate(bigram=reorder(bigram, n)) %>%
  filter(n>50)

g <- ggplot(df5, aes(bigram, n)) +
  geom_col(fill='aquamarine3') +
  xlab(NULL) +
  theme_bw() +
  coord_flip()
g


# 7. 네트워크 연결망 시각화
# 7-1. 단어 리스트와 결합 감성별 색상 설정
sword <- data.frame(name=(c(df4_2$word1, df4_2$word2)))
sword  <- sword %>% left_join(bing, by=c("name"="word")) %>% unique()
sword$color <- ifelse((grepl("positive", sword$sentiment))==TRUE, "red",
                      ifelse((grepl("negative", sword$sentiment))==TRUE, "blue", "green"))

# 7-2. 바이그램 분리
df5_1 <- df5 %>% separate(bigram, c("word1", "word2"), sep = " ")

# 7-3. 단어 감성 반영 네트워크 시각화
ng <- df5_1  %>%
  as_tbl_graph(directed=FALSE) %>%
  left_join(sword, by = "name") %>%
  mutate(eigen = centrality_eigen()) %>%
  ggraph(layout='nicely') +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "slategray4") +
  geom_node_point(aes(color=factor(color), size=(eigen))) +
  geom_node_text(aes(label=name), size=3, repel=TRUE) +
  theme_graph() +
  theme(legend.position='none')
ng


# 8. 토픽모형 방법 1
# 8-1. 문서-단어 빈도 테이블
df6 <- df2 %>% count(rid, word, sort=TRUE)

# 8-2. 문서-단어 매트릭스 생성
dtm <- df6 %>% cast_dtm(rid, word, n)

# 8-3. 토픽모형 생성(토픽의 수 k로 조정)
lda <- LDA(dtm, k = 5, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta")

# 8-4. 토픽별 빈출 단어
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_bw() +
  coord_flip() +
  scale_x_reordered()


# 9. 토픽모형 방법 2
# 9-1. 문서-단어 매트릭스 생성
dtm <- CreateDtm(doc_vec = df1$content, # 텍스트 문서 벡터
                 doc_names = df1$rid, # 문서 이름
                 ngram_window = c(1, 2), # 최소, 최대 n-gram 길이
                 stopword_vec = c(stopwords::stopwords("en"), # 불용어 사전
                                  stopwords::stopwords(source = "smart")),
                 lower = TRUE, # 소문자변환
                 remove_punctuation = TRUE, # 구두점 삭제
                 remove_numbers = TRUE, # 숫자 삭제
                 verbose = FALSE,
                 cpus = 2)
dtm <- dtm[,colSums(dtm) > 2]

# 9-2. 토픽모형
model <- FitLdaModel(dtm = dtm,
                     k = 5,
                     iterations = 500,
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2)

# 9-3. 바이그램 기반 토픽명
model$labels <- LabelTopics(assignments = model$theta > 0.05,
                            dtm = dtm, M = 2)

# 9-4. 토픽별 빈출 단어
model$top_terms <- GetTopTerms(phi = model$phi, M = 10)

# 9-5. 요약 테이블
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }), stringsAsFactors = FALSE)
df7 <- data.frame(model$summary)

# 10. 내보내기
write.csv(df7, "topic_model.csv", row.names=F)