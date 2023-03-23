#install.packages("sendmailR")
require(sendmailR)

#发件人
from <- "1623801078@qq.com"

#收件人
to <- "1623801078@qq.com"
#主题
subject <- "send the first email with R"
#正文
msg_body <- "王春虎的第一份R邮件"
#body <- list("It works!", mime_part(iris))
control <- list(smtpServerSMTP = "smtp.qq.com", smtpPortSMTP = "587")
sendmail(from = from,to = to,subject = subject,msg = msg_body,control = control)
sendmail_options()
