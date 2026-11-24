sales <- data.frame(
  product = c("Notebook", "Pen", "Notebook", "Pen", "Folder"),
  sales_amount = c(12, 5, 18, 7, 10)
)

sales_by_product <- aggregate(
  sales_amount ~ product,
  data = sales,
  FUN = sum
)

print(sales_by_product)