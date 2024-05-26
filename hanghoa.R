#THĂM DÒ DỮ LIỆU
library(readxl)
hanghoa=read_excel("D:/ĐỒ ÁN 1/hanghoa.xlsx")  
hanghoa

# Tóm tắt tổng quan số lượng & giá trị xuất - nhập - tồn
summary(hanghoa$soluongnhap)
summary(hanghoa$giatrinhap)
summary(hanghoa$soluongxuat)
summary(hanghoa$giatrixuat)
summary(hanghoa$soluongton)
summary(hanghoa$giatriton)


# Trung bình số lượng xuất - nhập - tồn
mean(hanghoa$soluongnhap)
mean(hanghoa$soluongxuat)
mean(hanghoa$soluongton)
# Trung bình giá trị xuất - nhập - tồn
mean(hanghoa$giatrinhap)
mean(hanghoa$giatrixuat)
mean(hanghoa$giatriton)


# Độ lệch chuẩn số lượng xuất - nhập - tồn
sd(hanghoa$soluongnhap)
sd(hanghoa$soluongxuat)
sd(hanghoa$soluongton)
# Độ lệch chuẩn giá trị xuất - nhập - tồn
sd(hanghoa$giatrinhap)
sd(hanghoa$giatrixuat)
sd(hanghoa$giatriton)

# Phương sai số lượng xuất - nhập - tồn
var(hanghoa$soluongnhap)
var(hanghoa$soluongxuat)
var(hanghoa$soluongton)
# Phương sai giá trị xuất - nhập - tồn
var(hanghoa$giatrinhap)
var(hanghoa$giatrixuat)
var(hanghoa$giatriton)



# Số lượng xuất - nhập - tồn lớn nhất và nhỏ nhất
max_slxuat <- max(hanghoa$soluongxuat)
max_slnhap <- max(hanghoa$soluongnhap)
max_slton <- max(hanghoa$soluongton)

min_slxuat <- min(hanghoa$soluongxuat)
min_slnhap <- min(hanghoa$soluongnhap)
min_slton <- min(hanghoa$soluongton)

print(max_slxuat)
print(max_slnhap)
print(max_slton)
print(min_slxuat)
print(min_slnhap)
print(min_slton)


# Trung bình giá sản phẩm
mean_price <- mean(hanghoa$dongia)
# Tìm sản phẩm có giá cao nhất và thấp nhất
max_price <- max(hanghoa$dongia)
min_price <- min(hanghoa$dongia)
print(mean_price)
print(max_price)
print(min_price)




# Tạo cột 'Quarter' để xác định quý của mỗi ngày hạch toán
library(dplyr)
hanghoa$Quarter <- quarters(hanghoa$ngayhachtoan)

# Tách dữ liệu thành 4 quý
quy1 <- hanghoa %>% filter(Quarter == "Q1")  
# `%>%` được sử dụng để chuyển đối tượng `hanghoa` (dataframe) từ hàm filter sang hàm khác mà không cần phải tham chiếu đến `hanghoa` nhiều lần.
quy2 <- hanghoa %>% filter(Quarter == "Q2")
quy3 <- hanghoa %>% filter(Quarter == "Q3")
quy4 <- hanghoa %>% filter(Quarter == "Q4")

print("Dữ liệu của Quý 1:")
print(quy1)
print("Dữ liệu của Quý 2:")
print(quy2)
print("Dữ liệu của Quý 3:")
print(quy3)
print("Dữ liệu của Quý 4:")
print(quy4)



# Tính tổng số lượng nhập cho mỗi quý
sum_quy1 <- sum(quy1$soluongnhap)
sum_quy2 <- sum(quy2$soluongnhap)
sum_quy3 <- sum(quy3$soluongnhap)
sum_quy4 <- sum(quy4$soluongnhap)
# Hiển thị tổng số lượng nhập cho mỗi quý
cat("Tổng số lượng nhập cho Quý 1:", sum_quy1, "\n")
cat("Tổng số lượng nhập cho Quý 2:", sum_quy2, "\n")
cat("Tổng số lượng nhập cho Quý 3:", sum_quy3, "\n")
cat("Tổng số lượng nhập cho Quý 4:", sum_quy4, "\n")




# Tính tổng số lượng nhập, số lượng xuất và số lượng tồn cho mỗi quý
library(ggplot2)
summary_quy <- hanghoa %>%
  group_by(Quarter) %>%
  summarise(soluongnhap = sum(soluongnhap),
            soluongxuat = sum(soluongxuat),
            soluongton = sum(soluongton))
# Biểu đồ mô tả số lượng nhập theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = soluongnhap, fill = "So luong nhap"), position = "dodge") +
  labs(title = "Bieu do mo ta so luong nhap theo quy",
       x = "Quy",
       y = "So luong") +
  scale_fill_manual(values = c("So luong nhap" = "blue")) +
  theme_minimal()


# Biểu đồ mô tả số lượng xuất theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = soluongxuat, fill = "So luong xuat"), position = "dodge") +
  
  labs(title = "Bieu do mo ta so luong xuat theo quy",
       x = "Quy",
       y = "So luong") +
  scale_fill_manual(values = c("So luong xuat" = "red")) +
  theme_minimal()


# Biểu đồ mô tả số lượng tồn theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = soluongton, fill = "So luong ton"), position = "dodge") +
  
  labs(title = "Bieu do mo ta so luong ton theo quy",
       x = "Quy",
       y = "So luong") +
  scale_fill_manual(values = c("So luong ton" = "green")) +
  theme_minimal()



# Tính tổng giá trị nhập, giá trị xuất và giá trị tồn cho mỗi quý
summary_quy <- hanghoa %>%
  group_by(Quarter) %>%
  summarise(giatrinhap = sum(giatrinhap),
            giatrixuat = sum(giatrixuat),
            giatriton = sum(giatriton))

# Biểu đồ mô tả giá trị nhập theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = giatrinhap, fill = "Gia tri nhap"), position = "dodge") +
  labs(title = "Bieu do mo ta gia tri nhap theo quy",
       x = "Quy",
       y = "Gia tri") +
  scale_fill_manual(values = c("Gia tri nhap" = "orange")) +
  theme_minimal()

# Biểu đồ mô tả giá trị xuất theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = giatrixuat, fill = "Gia tri xuat"), position = "dodge") +
  labs(title = "Bieu do mo ta gia tri xuat theo quy",
       x = "Quy",
       y = "Gia tri") +
  scale_fill_manual(values = c("Gia tri xuat" = "yellow")) +
  theme_minimal()

# Biểu đồ mô tả giá trị tồn theo quý
ggplot(summary_quy, aes(x = Quarter)) +
  geom_col(aes(y = giatriton, fill = "Gia tri ton"), position = "dodge") +
  labs(title = "Bieu do mo ta gia tri ton theo quy",
       x = "Quy",
       y = "Gia tri") +
  scale_fill_manual(values = c("Gia tri ton" = "violet")) +
  theme_minimal()


# Tính tổng biến đơn giá theo từng quý
sum_dongia_quy <- hanghoa %>%
  group_by(Quarter) %>%
  summarise(total_dongia = sum(dongia))
# Biểu đồ
ggplot(sum_dongia_quy, aes(x = Quarter, y = total_dongia)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
  labs(title = "Tổng giá trị đơn giá theo quý",
       x = "Quý",
       y = "Tổng giá trị đơn giá") +
  theme_minimal()



# Sử dụng biểu đồ tròn (pie chart) để so sánh các mặt hàng 
# Mặt hàng bán chạy nhất và doanh thu cao nhất
summary_hanghoa <- hanghoa %>%
  group_by(mahang, tenhang) %>%
  summarise(tong_soluong_ban = sum(soluongxuat),
            tong_doanhthu = sum(dongiaban * soluongxuat))

# Chọn ra 5 mặt hàng bán chạy nhất
top5_banchay <- summary_hanghoa %>% 
  arrange(desc(tong_soluong_ban)) %>% 
  head(5)

# Biểu đồ tròn cho mặt hàng bán chạy nhất
ggplot(top5_banchay, aes(x = "", y = tong_soluong_ban, fill = tenhang)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 5 mặt hàng bán chạy nhất",
       fill = "Mặt hàng",
       y = "Tổng số lượng bán") +
  theme_void() +
  theme(legend.position = "right")



# Chọn ra 5 mặt hàng doanh thu cao nhất
top5_doanhthu <- summary_hanghoa %>% 
  arrange(desc(tong_doanhthu)) %>% 
  head(5)

# Biểu đồ tròn cho mặt hàng doanh thu cao nhất
ggplot(top5_doanhthu, aes(x = "", y = tong_doanhthu, fill = tenhang)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 5 mặt hàng doanh thu cao nhất",
       fill = "Mặt hàng",
       y = "Tổng doanh thu") +
  theme_void() +
  theme(legend.position = "right")




# Sử dụng biểu đồ tăng trưởng(Growth Chart) để phân tích giá cả hàng hóa
hanghoa$month_year <- format(hanghoa$ngayhachtoan, "%Y-%m")
# Tính giá trung bình hàng hóa theo tháng
mean_price <- hanghoa %>%
  group_by(month_year) %>%
  summarise(mean_dongia = mean(dongia))

# Tạo biểu đồ tăng trưởng
ggplot(mean_price, aes(x = month_year, y = mean_dongia)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Biểu đồ tăng trưởng giá cả hàng hóa theo thời gian",
       x = "Thời gian",
       y = "Giá trung bình") +
  theme_minimal()




# Biểu đồ Scatter Plot mối quan hệ giữa số lượng nhập và giá tiền nhập
ggplot(hanghoa, aes(x = soluongnhap, y = giatrinhap)) +
  geom_point(color = "blue") +
  labs(title = "Mối quan hệ giữa số lượng nhập và giá tiền nhập",
       x = "Số lượng nhập",
       y = "Giá tiền nhập") +
  theme_minimal()

# Biểu đồ Scatter Plot mối quan hệ giữa số lượng xuất và giá tiền xuất
ggplot(hanghoa, aes(x = soluongxuat, y = giatrixuat)) +
  geom_point(color = "yellow") +
  labs(title = "Mối quan hệ giữa số lượng xuất và giá tiền xuất",
       x = "Số lượng xuất",
       y = "Giá tiền xuất") +
  theme_minimal()

# Biểu đồ Scatter Plot mối quan hệ giữa số lượng tồn và giá tiền tồn
ggplot(hanghoa, aes(x = soluongton, y = giatriton)) +
  geom_point(color = "red") +
  labs(title = "Mối quan hệ giữa số lượng tồn và giá tiền tồn",
       x = "Số lượng tồn",
       y = "Giá tiền tồn") +
  theme_minimal()




# Sử dụng ước lượng tỷ lệ để so sánh được sản phẩm bán chạy nhất
library(tidyr)
library(broom)
library(glmnet)

# Tính tổng số lượng bán cho mỗi mặt hàng
summary_hanghoa <- hanghoa %>%
  group_by(tenhang) %>%
  summarise(total_soluong = sum(soluongxuat))

# Sắp xếp theo thứ tự giảm dần của số lượng bán
summary_hanghoa <- summary_hanghoa %>% arrange(desc(total_soluong))
# Chọn ra sản phẩm bán chạy nhất
top_selling_product <- summary_hanghoa$tenhang[1]
# Tạo biến nhị phân cho sản phẩm bán chạy nhất
hanghoa <- hanghoa %>%
  mutate(is_top_selling = as.numeric(tenhang == top_selling_product))

# Tính toán tỷ lệ bán chạy nhất
model <- glm(is_top_selling ~ soluongxuat + giatrixuat + soluongton + giatriton, data = hanghoa, family = "binomial")
# Biểu đồ cột tỷ lệ ước lượng
coefficients_df <- as.data.frame(summary(model)$coefficients)
coefficients_df$variable <- rownames(coefficients_df)

# Biểu đồ cột
ggplot(coefficients_df[-1, ], aes(x = variable, y = Estimate)) +
  geom_col(fill = "lightblue", color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Ước lượng tỷ lệ",
       x = "Biến",
       y = "Ước lượng tỷ lệ",
       caption = paste("Sản phẩm bán chạy nhất:", top_selling_product),
       fill = "Biến") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Kiểm định t để so sánh giá bán trung bình của hai loại sản phẩm khác nhau
library(stats)
# Phân tách dữ liệu thành hai nhóm sản phẩm
product_group1 <- hanghoa %>% filter(mahang == "BM 20.10.10")
product_group2 <- hanghoa %>% filter(mahang == "BM 16-16-8")
# Thực hiện kiểm định t-test
t_test_result <- t.test(product_group1$dongiaban, product_group2$dongiaban)
print(t_test_result)




# Kiểm định ANOVA: So sánh giá bán trung bình của nhiều loại sản phẩm khác nhau.
#Xác định xem có sự khác biệt đáng kể về giá bán giữa các loại sản phẩm hay không.
library(stats)
# Thực hiện kiểm định ANOVA
anova_result <- aov(dongiaban ~ mahang, data = hanghoa)
print(summary(anova_result))




#PHÂN TÍCH TƯƠNG QUAN
# Chọn các biến liên quan đến xuất, nhập và tồn từ dữ liệu hàng hóa
variables_to_analyze <- c("soluongxuat", "giatrixuat", "soluongnhap", "giatrinhap", "soluongton", "giatriton")

# Tính toán ma trận tương quan giữa các biến
correlation_matrix <- cor(hanghoa[, variables_to_analyze])

# In ra ma trận tương quan
print("Ma trận tương quan:\n")
print(correlation_matrix)

# Nhận xét
print("\nNhận xét:\n")
if(any(correlation_matrix > 0.8)) {
  print("Có ít nhất một cặp biến có mức độ tương quan cao (> 0.8). Điều này có thể chỉ ra sự phụ thuộc mạnh mẽ giữa các biến này, và cần được xem xét khi phân tích dữ liệu và đưa ra dự đoán.")
} else {
  print("Không có cặp biến nào có mức độ tương quan cao (> 0.8). Điều này có thể chỉ ra rằng không có sự phụ thuộc mạnh mẽ giữa các biến trong dữ liệu.")
}



#.....

library(ggplot2)

# Chọn các biến liên quan đến xuất, nhập và tồn từ dữ liệu hàng hóa
variables_to_analyze <- c("soluongxuat", "giatrixuat", "soluongnhap", "giatrinhap", "soluongton", "giatriton")

# Tính toán ma trận tương quan giữa các biến
correlation_matrix <- cor(hanghoa[, variables_to_analyze])

# Chuyển đổi ma trận tương quan thành dataframe để sử dụng với ggplot2
correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("Variable1", "Variable2", "Correlation")

# Vẽ biểu đồ heatmap
ggplot(correlation_df, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# Nhận xét
cat("\nNhận xét:\n")
if(any(correlation_matrix > 0.8)) {
  cat("Biểu đồ heatmap cho thấy có ít nhất một cặp biến có mức độ tương quan cao (> 0.8). Các cặp biến có mức độ tương quan cao bao gồm:\n")
  high_correlation_pairs <- which(correlation_matrix > 0.8, arr.ind = TRUE)
  for (i in 1:nrow(high_correlation_pairs)) {
    cat("- ", rownames(correlation_matrix)[high_correlation_pairs[i, 1]], "và", 
        rownames(correlation_matrix)[high_correlation_pairs[i, 2]], "\n")
  }
  cat("Điều này có thể chỉ ra sự phụ thuộc mạnh mẽ giữa các biến này, và cần được xem xét khi phân tích dữ liệu và đưa ra dự đoán.")
} else {
  cat("Biểu đồ heatmap không chỉ ra sự tương quan cao (> 0.8) giữa bất kỳ cặp biến nào. Điều này có thể chỉ ra rằng không có sự phụ thuộc mạnh mẽ giữa các biến trong dữ liệu.")
}

# Nhận xét thêm về biểu đồ heatmap
cat("\nBiểu đồ heatmap có thể giúp chúng ta dễ dàng nhận biết mức độ tương quan giữa các biến thông qua sự biến đổi màu sắc trên lưới. Màu sắc càng sáng thể hiện mức độ tương quan càng cao, trong khi màu sắc càng tối thể hiện mức độ tương quan càng thấp.")
cat("Các ô có màu đậm hơn cho thấy mối quan hệ tương quan lớn hơn giữa cặp biến tương ứng, trong khi các ô có màu nhạt hơn chỉ ra mối quan hệ tương quan yếu hơn.")





#Phân tích hồi quy
# Tạo mô hình hồi quy tuyến tính
lm_model <- lm(soluongton ~ soluongnhap, data = hanghoa)

# Hiển thị kết quả của mô hình
summary(lm_model)
# Biểu đồ scatter plot cho biến nhập và tồn
ggplot(hanghoa, aes(x = soluongnhap, y = soluongton)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Số lượng nhập", y = "Số lượng tồn") +
  ggtitle("Hồi quy tuyến tính: Số lượng tồn dựa trên số lượng nhập")

# Nhận xét 
cat("Kết quả của mô hình hồi quy tuyến tính:\n")
print(summary(lm_model))
cat("\nNhận xét:\n")
cat("Mô hình hồi quy tuyến tính đã được tạo để dự đoán số lượng tồn (số lượngton) dựa trên số lượng nhập (số lượngnhap).")
if (summary(lm_model)$r.squared > 0.5) {
  cat("Mô hình có hiệu suất tốt với R-squared lớn hơn 0.5. Đường trendline có thể phản ánh mối quan hệ tốt giữa số lượng nhập và số lượng tồn.")
} else {
  cat("Mô hình có hiệu suất không tốt với R-squared nhỏ hơn hoặc bằng 0.5. Đường trendline có thể không phản ánh mối quan hệ mạnh mẽ giữa số lượng nhập và số lượng tồn.")
}





# Tạo mô hình hồi quy tuyến tính cho hàm xuất
lm_model_xuat <- lm(soluongxuat ~ soluongton + giatriton, data = hanghoa)

# Hiển thị kết quả của mô hình
summary(lm_model_xuat)

# Biểu đồ scatter plot cho biến xuất và các biến đầu vào liên quan
ggplot(hanghoa, aes(x = soluongton, y = soluongxuat)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Số lượng tồn", y = "Số lượng xuất") +
  ggtitle("Hồi quy tuyến tính: Số lượng xuất dựa trên số lượng tồn") +
  theme_minimal()
# Nhận xét
cat("Mô hình hồi quy tuyến tính đã được tạo để dự đoán số lượng xuất (soluongxuat) dựa trên số lượng tồn (soluongton) và giá trị tồn (giatriton). Kết quả của mô hình có thể được tổng kết như sau:\n")
print(summary(lm_model_xuat))
cat("\nNhận xét: Mô hình đã được điều chỉnh với biến giá trị tồn (giatriton) và có vẻ có hiệu suất khá tốt với R-squared đáng chú ý. Tuy nhiên, cần kiểm tra các giả định của mô hình để đảm bảo tính đáng tin cậy của các ước lượng hồi quy.")






# Thêm đường dự đoán từ mô hình hồi quy lên biểu đồ scatter plot
ggplot(hanghoa, aes(x = soluongton, y = soluongxuat)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", aes(y = predict(lm_model_xuat))) +
  labs(x = "Số lượng tồn", y = "Số lượng xuất") +
  ggtitle("Hồi quy tuyến tính: Số lượng xuất dựa trên số lượng tồn") +
  theme_minimal()
# Nhận xét
cat("Biểu đồ trên đã thêm đường dự đoán từ mô hình hồi quy mới (màu xanh). Đường này biểu diễn dự đoán số lượng xuất dựa trên số lượng tồn theo mô hình mới. Cần kiểm tra sự phù hợp của đường dự đoán này với dữ liệu thực tế.")





# Tính toán ma trận tương quan giữa các biến
correlation_matrix <- cor(hanghoa[, variables_to_analyze])

# In ra ma trận tương quan
print(correlation_matrix)

# Lọc các cặp biến có hệ số tương quan gần 1 hoặc bằng -1
strong_correlations <- which(abs(correlation_matrix) >= 0.8 & correlation_matrix != 1, arr.ind = TRUE)

# Hiển thị các cặp biến có mối quan hệ tương quan mạnh
cat("Các cặp biến có mối quan hệ tương quan mạnh:\n")
for (i in 1:nrow(strong_correlations)) {
  row <- strong_correlations[i, 1]
  col <- strong_correlations[i, 2]
  cat(sprintf("%s và %s: %f\n", rownames(correlation_matrix)[row], colnames(correlation_matrix)[col], correlation_matrix[row, col]))
}





# Phân tích độ đồng biến của mỗi cặp biến
cat("Phân tích độ đồng biến của mỗi cặp biến:\n")
for (i in 1:(length(variables_to_analyze) - 1)) {
  for (j in (i + 1):length(variables_to_analyze)) {
    variable1 <- variables_to_analyze[i]
    variable2 <- variables_to_analyze[j]
    correlation <- correlation_matrix[i, j]
    
    if (correlation > 0) {
      direction <- "dương (cùng hướng)"
    } else if (correlation < 0) {
      direction <- "âm (ngược hướng)"
    } else {
      direction <- "không có mối quan hệ tuyến tính"
    }
    
    cat(sprintf("%s và %s có độ đồng biến %s với hệ số tương quan là %f\n", variable1, variable2, direction, correlation))
  }
}




#TRỰC QUAN HÓA DỮ LIỆU
library(ggplot2)
library(dplyr)
library(lubridate)

# Giả sử dữ liệu của bạn đã được lưu vào một dataframe có tên là data_df

# Chuyển đổi cột ngày thành định dạng ngày
data_df$ngayhachtoan <- dmy(data_df$ngayhachtoan)

# Tách dữ liệu theo tháng và hiển thị các mã hàng bán trong mỗi tháng
data_df %>%
  mutate(thang = month(ngayhachtoan),
         nam = year(ngayhachtoan)) %>%
  group_by(thang, nam, mahang) %>%
  summarize(so_luong_ban = sum(soluongxuat)) %>%
  filter(so_luong_ban > 0) %>%
  arrange(nam, thang)

#Dữ liệu mà hàng và số lượng của quý 1
dongiaa1 <- quy1 %>%
  group_by(mahang) %>%
  summarise(dongia)
dongiaa1
ggplot(dongiaa1, aes(x = dongia,y = mahang)) +
  geom_col(stat="identiy",fill='lightblue', position = "dodge") +
  labs(title = "y",
       x = "don gia dữ liệu quý 1",
       y = "ma hang dữ liệu quý 1")


#Dữ liệu mà hàng và số lượng của quý 2
dongiaa2 <- quy2 %>%
  group_by(mahang) %>%
  summarise(dongia)
dongiaa2
ggplot(dongiaa2, aes(x = dongia,y = mahang)) +
  geom_col(stat="identiy",fill='pink', position = "dodge") +
  labs(title = "y",
       x = "don gia dữ liệu quý 2",
       y = "ma hang dữ liệu quý 2")


#Dữ liệu mã hàng và số lượng quý 3
dongiaa3 <- quy3 %>%
  group_by(mahang) %>%
  summarise(dongia)
ggplot(dongiaa3, aes(x = dongia, y = mahang)) +
  geom_col(stat = "identity", fill = 'blue', position = "dodge") +
  labs(title = "Dữ liệu Quý 3",
       x = "Đơn giá dữ liệu quý 3",
       y = "Mã hàng dữ liệu quý 3")


#Dữ liệu mã hàng và số lượng của quý 4
dongiaa4 <- quy4 %>%
  group_by(mahang) %>%
  summarise(dongia)
dongiaa4
ggplot(dongiaa4, aes(x = dongia, y = mahang)) +
  geom_col(stat = "identity", fill = 'red', position = "dodge") +
  labs(title = "Dữ liệu Quý 4",
       x = "Đơn giá dữ liệu quý 4",
       y = "Mã hàng dữ liệu quý 4")


library(ggpie)
ggpie(data =quy1, group_key = "mahang", count_type = "full",
      label_info = "all", label_type = "horizon", label_split = NULL,
      label_size = 4, label_pos = "in", label_threshold = 15)+
  labs(title ="dữ liệu quý 1", fill =",ma hang")
#dữ liệu 3D pie
ggpie3D( data = quy1, group_key = "mahang", count_type = "full", tilt_degrees = -8)+
  labs(title ="biểu đồ3D", fill ="ma hang")


#dữ liệu 3D pie
ggpie3D( data = dongiaa1, group_key = "mahang", count_type = "full", tilt_degrees = -2)+
  labs(title =" biểu đồ 3D", fill ="ma hang")




# Load gói dữ liệu ggplot2
library(ggplot2)

# Cài đặt gói dữ liệu 'plot3D' nếu chưa có
if (!require("plot3D")) {
  install.packages("plot3D")
}
library(plot3D)
# Dữ liệu tài sản dài hạn
Tp1 <- hanghoa$AB
tp1 <- sum(Tp1)
Tp2 <- hanghoa$AC
tp2 <- sum(Tp2)
Tp3 <- hanghoa$AK
tp3 <- sum(Tp3)

# Tạo vector dữ liệu và tên
data4 <- c(tp1, tp2, tp3)
name2 <- c("Tài sản cố định", "Tài sản cố định hữu hình", "Tài sản dở dang dài hạn")

# Tạo nhãn dữ liệu theo tỉ lệ phần trăm
data_label <- paste0(round(100 * data4 / sum(data4), digits = 2), "%")

# Vẽ biểu đồ pie 3D
ggpie3D(data4, labels = data_label,
        col = hcl.colors(length(data4), "Spectral"),
        explode = 0.2,
        main = "Biểu đồ thể hiện tỉ lệ các thành phần của tài sản dài hạn")

# Xoay biểu đồ
par(mar = c(5, 5, 5, 5)) # Điều chỉnh margin để không bị cắt bớt biểu đồ
view3d(theta = 0, phi = 30, scale = 0.75)

# Thêm chú thích
legend("topright", inset = c(0.3, 0), title = "Ghi chú",
       legend = name2, fill = hcl.colors(length(data4), "Spectral"))





library(ggplot2)
library(dplyr)

hanghoa$Month = format(hanghoa$ngayhachtoan,'%m')
#tính tổng số lượng nhập theo mã hàng và theo tháng
SLN <- hanghoa %>%
  group_by(Month,mahang) %>%
  summarise(soluongnhap=sum(soluongnhap))
SLN
#tính tổng số lượng xuất theo mã hàng và theo tháng
SLX = hanghoa %>% group_by(Month,mahang) %>%
  summarise(soluongxuat=sum(soluongxuat))
SLX
#tính tổng số lượng tồn theo mã hàng và theo tháng
SLT = hanghoa %>% group_by(Month,mahang) %>%
  summarise(soluongton=sum(soluongton))
SLT




library(ggplot2)
ggplot(SLN, aes(x = Month, y = soluongnhap,group=mahang,color=mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo mã hàng và tháng",
       x = "Mã hàng", y = "Tháng") +
  theme_minimal()

ggplot(SLX, aes(x = Month, y = soluongxuat,group=mahang,color=mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo mã hàng và tháng",
       x = "Mã hàng", y = "Tháng") +
  theme_minimal()

ggplot(SLT, aes(x = Month, y = soluongton,group=mahang,color=mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng tồn theo mã hàng và tháng",
       x = "Mã hàng", y = "Tháng") +
  theme_minimal()



# Keep only 3 names
dabc = quy4 %>%
  filter(mahang %in% mahang)

# Plot
dabc %>%
  ggplot( aes(x=Month, y=soluongnhap, group=mahang, color=mahang)) +
  geom_line()




#Biểu đồ kết hợp
# Tạo scatter plot với soluongnhap và soluongxuat
scatter_plot <- ggplot(quy1, aes(x = soluongnhap, y = soluongxuat)) +
  geom_point()

# Thêm marginal plots (phân bố) cho x và y
scatter_plot_with_margin <- ggMarginal(scatter_plot, type = "histogram",color="red")

# Hiển thị biểu đồ
print(scatter_plot_with_margin)




#heatmap
#quý 1
ggplot(summary_data1, aes(x = ngayhachtoan, y = nhom_mahang, fill = total_soluongnhap)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "Ngày hạch toán", y = "Nhóm mã hàng", title = "Heatmap Tổng Số Lượng Nhập Theo Nhóm Mã Hàng và Ngày Hạch Toán", size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#quý 2
ggplot(summary_data2, aes(x = ngayhachtoan, y = nhom_mahang, fill = total_soluongnhap)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "Ngày hạch toán", y = "Nhóm mã hàng", title = "Heatmap Tổng Số Lượng Nhập Theo Nhóm Mã Hàng và Ngày Hạch Toán", size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#quý 3
ggplot(summary_data3, aes(x = ngayhachtoan, y = nhom_mahang, fill = total_soluongnhap)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "Ngày hạch toán", y = "Nhóm mã hàng", title = "Heatmap Tổng Số Lượng Nhập Theo Nhóm Mã Hàng và Ngày Hạch Toán", size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#quý 4
ggplot(summary_data4, aes(x = ngayhachtoan, y = nhom_mahang, fill = total_soluongnhap)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "Ngày hạch toán", y = "Nhóm mã hàng", title = "Heatmap Tổng Số Lượng Nhập Theo Nhóm Mã Hàng và Ngày Hạch Toán", size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#line chart chia theo từng quý và gộp các mã hàng lại với nhau
# Gom các mã hàng vào các nhóm tương ứng quý 1
nhom_mahang <- c("BB", "BG", "BIA", "CV")
quy1 <- quy1 %>%
  mutate(nhom_mahang = case_when(
    grepl("BB", mahang) ~ "BB",
    grepl("BG", mahang) ~ "BG",
    grepl("BIA", mahang) ~ "BIA",
    grepl("CV", mahang) ~ "CV",
    TRUE ~ mahang  # Giữ nguyên nếu không thuộc bất kỳ nhóm nào
  ))

# Tóm tắt dữ liệu theo ngày hạch toán và nhóm mã hàng
summary_data1 <- quy1 %>%
  group_by(ngayhachtoan, nhom_mahang) %>%
  summarise(total_soluongnhap = sum(soluongnhap))

# Vẽ biểu đồ line chart
ggplot(summary_data1, aes(x = ngayhachtoan, y = total_soluongnhap, color = nhom_mahang, group = nhom_mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo ngày (Quý 1)",
       x = "Ngày hạch toán", y = "Tổng số lượng nhập") +
  theme_minimal()




# Gom các mã hàng vào các nhóm tương ứng quý 2
nhom_mahang <- c("BB", "BG", "CV")
quy2 <- quy2 %>%
  mutate(nhom_mahang = case_when(
    grepl("BB", mahang) ~ "BB",
    grepl("BG", mahang) ~ "BG",
    grepl("CV", mahang) ~ "CV",
    TRUE ~ mahang  # Giữ nguyên nếu không thuộc bất kỳ nhóm nào
  ))

# Tóm tắt dữ liệu theo ngày hạch toán và nhóm mã hàng
summary_data2 <- quy2 %>%
  group_by(ngayhachtoan, nhom_mahang) %>%
  summarise(total_soluongnhap = sum(soluongnhap))

# Vẽ biểu đồ line chart
ggplot(summary_data2, aes(x = ngayhachtoan, y = total_soluongnhap, color = nhom_mahang, group = nhom_mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo ngày (Quý 2)",
       x = "Ngày hạch toán", y = "Tổng số lượng nhập") +
  theme_minimal()




# Gom các mã hàng vào các nhóm tương ứng của quý 3
nhom_mahang <- c("BM", "CL", "CV")
quy3 <- quy3 %>%
  mutate(nhom_mahang = case_when(
    grepl("BM", mahang) ~ "BM",
    grepl("CL", mahang) ~ "CL",
    grepl("CV", mahang) ~ "CV",
    TRUE ~ mahang  # Giữ nguyên nếu không thuộc bất kỳ nhóm nào
  ))

# Tóm tắt dữ liệu theo ngày hạch toán và nhóm mã hàng
summary_data3 <- quy3 %>%
  group_by(ngayhachtoan, nhom_mahang) %>%
  summarise(total_soluongnhap = sum(soluongnhap))

# Vẽ biểu đồ line chart
ggplot(summary_data3, aes(x = ngayhachtoan, y = total_soluongnhap, color = nhom_mahang, group = nhom_mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo ngày (Quý 3)",
       x = "Ngày hạch toán", y = "Tổng số lượng nhập") +
  theme_minimal()





# Gom các mã hàng vào các nhóm tương ứng của quý 4
nhom_mahang <- c("BM", "CL", "CV")
quy4 <- quy4 %>%
  mutate(nhom_mahang = case_when(
    grepl("BM", mahang) ~ "BM",
    grepl("CL", mahang) ~ "CL",
    grepl("CV", mahang) ~ "CV",
    TRUE ~ mahang  # Giữ nguyên nếu không thuộc bất kỳ nhóm nào
  ))

# Tóm tắt dữ liệu theo ngày hạch toán và nhóm mã hàng
summary_data4 <- quy4 %>%
  group_by(ngayhachtoan, nhom_mahang) %>%
  summarise(total_soluongnhap = sum(soluongnhap))

# Vẽ biểu đồ lline chart
ggplot(summary_data4, aes(x = ngayhachtoan, y = total_soluongnhap, color = nhom_mahang, group = nhom_mahang)) +
  geom_line() +
  labs(title = "Biểu đồ số lượng nhập theo ngày (Quý 4)",
       x = "Ngày hạch toán", y = "Tổng số lượng nhập") +
  theme_minimal()




#pie chart 2D và pie chart 3D sau khi đã chỉnh sửa theo từng tháng
#dữ liệu chia theo tháng theo từng mã hàng
# Tạo danh sách chứa biểu đồ pie2D cho từng tháng
pie_list <- lapply(1:12, function(month) {
  thang_data <- extract_month_data(hanghoa, sprintf("%02d", month))
  ggpie(data = thang_data, group_key = "mahang", count_type = "full",
        label_info="all",label_type="horizon",label_split = NULL,
        label_size=4,label_pos="in",label_threshold = 15) +
    labs(title = paste("Biểu đồ 2D tháng", month), fill = "Mã hàng")
})
# Hiển thị danh sách biểu đồ pie
pie_list
#dữ liệu 3D pie
pie3D_list <- lapply(1:12, function(month) {
  thang_data <- extract_month_data(hanghoa, sprintf("%02d", month))
  ggpie3D(data = thang_data, group_key = "mahang", count_type = "full", tilt_degrees = -2) +
    labs(title = paste("Biểu đồ 3D tháng", month), fill = "Mã hàng")
})
# Hiển thị danh sách biểu đồ pie
pie3D_list



# Vẽ biểu đồ thể hiện tỉ lệ các thành phần của tài sản dài hạn
Tp1<-hanghoa$AB
tp1<-sum(Tp1[1],Tp1[2],Tp1[3],Tp1[4])
Tp2<-hanghoa$AC
tp2<-sum(Tp2[1],Tp2[2],Tp2[3],Tp2[4])
Tp3<-hanghoa$AK
tp3<-sum(Tp3[1],Tp3[2],Tp3[3],Tp3[4])
data4<-c(tp1, tp2, tp3)
name2 <- c("Tài sản cố định", "Tài sản cố định hữu hình", "Tài sản dở dang dài hạn")
data_label <- paste0(round(100*data4/sum(data4), digits = 2), "%")
# Thiết lập các tham số để xoay biểu đồ
pie3D(data4, labels=data_label,
      col = hcl.colors(length(data4), "Spectral"),
      explode = 0.2,
      main="Biểu đồ thể hiện tỉ lệ các thành phần của tài sản dài hạn")

# Xoay biểu đồ
par(mar=c(5,5,5,5)) # Điều chỉnh margin để không bị cắt bớt biểu đồ
view3d(theta = 0, phi = 30, scale = 0.75)
legend("topright", inset=c(0.3,0), title="Ghi chú",
       legend=name2, fill=hcl.colors(length(data4), "Spectral"))





#PHÂN TÍCH THÀNH PHẦN CHÍNH PCA
# Áp dụng PCA
pca_result <- prcomp(hanghoa[, c("dongia", "soluongnhap", "giatrinhap", "soluongxuat",
                                 "giatrixuat", "soluongton", "giatriton", "dongiaban")], scale = TRUE) 


# Biểu đồ hóa dữ liệu sau khi giảm chiều
pca_data <- as.data.frame(pca_result$x[, 1:2]) # Lấy 2 thành phần chính đầu tiên từ kết quả của PCA
colnames(pca_data) <- c("PC1", "PC2") # Đặt tên cho các cột của dataframe với tên là "PC1" và "PC2".

# Sử dụng ggplot2 để tạo biểu đồ scatter plot của hai thành phần chính.
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point() +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("PCA on Product Data")



# In ra các thành phần chính và tỷ lệ phương sai giải thích
print("Principal Components:")
print(pca_result$rotation[, 1:2])

# In ra tỷ lệ phương sai giải thích của mỗi thành phần chính.
print("Proportion of Variance Explained:")
print(pca_result$sdev^2 / sum(pca_result$sdev^2))






# Tính toán phần trăm phương sai giải thích bởi thành phần chính 1 và 2
variance_explained_PC1 <- (pca_result$sdev[1]^2 / sum(pca_result$sdev^2)) * 100
variance_explained_PC2 <- (pca_result$sdev[2]^2 / sum(pca_result$sdev^2)) * 100

# In ra phần trăm phương sai giải thích
print(paste("Phần trăm phương sai giải thích bởi thành phần chính 1:", variance_explained_PC1, "%"))
print(paste("Phần trăm phương sai giải thích bởi thành phần chính 2:", variance_explained_PC2, "%"))




# Biểu đồ biểu diễn phần trăm phương sai giải thích
prop_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
barplot(prop_var, names.arg = 1:length(prop_var), 
        main = "Tỷ lệ Phương sai Giải thích bởi Mỗi Thành phần Chính",
        xlab = "Thành phần Chính", ylab = "Tỷ lệ Phương sai Giải thích")




# Biểu đồ biểu diễn tỷ lệ phần trăm phương sai tích lũy
cum_var <- cumsum(prop_var)
plot(cum_var, type = "b", 
     main = "Tỷ lệ Phương sai Tích lũy",
     xlab = "Số lượng Thành phần Chính", ylab = "Tỷ lệ Phương sai Tích lũy")




# Biểu đồ biểu diễn hệ số của các biến đối với các thành phần chính
barplot(abs(pca_result$rotation[, 1:2]), beside = TRUE,
        main = "Hệ số Tuyệt đối của Biến trên Thành phần Chính",
        xlab = "Biến", ylab = "Hệ số Tuyệt đối",
        legend.text = c("PC1", "PC2"))




# Lấy ma trận các thành phần chính từ kết quả PCA
pca_components <- pca_result$rotation[, 1:2] # Chỉ lấy hai thành phần chính đầu tiên vì chúng thường giải thích phần lớn phương sai của dữ liệu.

# Xác định các biến quan trọng gần với 1 hoặc -1
important_variables_PC1 <- names(which(abs(pca_components[, 1]) >= 0.5)) # Thay 0.5 là ngưỡng phù hợp.
important_variables_PC2 <- names(which(abs(pca_components[, 2]) >= 0.5)) # Thay 0.5 là ngưỡng phù hợp.

# In ra các biến quan trọng cho từng thành phần chính
cat("Biến quan trọng cho thành phần chính 1:\n")
print(important_variables_PC1)

cat("Biến quan trọng cho thành phần chính 2:\n")
print(important_variables_PC2)





# Tính toán ma trận tương quan giữa các biến quan trọng và các thành phần chính
correlation_matrix <- cor(hanghoa[, c(important_variables_PC1, important_variables_PC2)])
print("Ma trận tương quan giữa các biến quan trọng và các thành phần chính")
print(correlation_matrix)

# Biểu đồ phân bố của các biến quan trọng
par(mfrow = c(1,2))
for (var in important_variables_PC1) {
  hist(hanghoa[[var]], main = paste("Bieu do phan bo cua", var), xlab = var)}





#Phân tích hồi quy giữa các thành phần chính PC1
# Tạo mô hình hồi quy tuyến tính
lm_model <- lm(soluongnhap ~ giatrinhap, data = hanghoa)

# Hiển thị kết quả của mô hình
summary(lm_model)
# Biểu đồ scatter plot cho biến nhập và tồn
ggplot(hanghoa, aes(x = soluongnhap, y = giatrinhap)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Số lượng nhập", y = "Giá trị nhập") +
  ggtitle("Hồi quy tuyến tính giữa số lượng nhập và giá trị nhập")




#Phân tích hồi quy giữa các thành phần chính PC2
# Tạo mô hình hồi quy tuyến tính
lm_model <- lm(soluongxuat ~ soluongton, data = hanghoa)

# Hiển thị kết quả của mô hình
summary(lm_model)
# Biểu đồ scatter plot cho biến nhập và tồn
ggplot(hanghoa, aes(x = soluongxuat, y = soluongton)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Số lượng xuất", y = "Số lượng tồn") +
  ggtitle("Hồi quy tuyến tính giữa số lượng xuất và số lượng tồn")

