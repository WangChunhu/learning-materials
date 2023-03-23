ui <- dashboardPage(title = "数据小工具(SpringTiger)", #浏览器标题
                    #skin = "blue",
                    header = dashboardHeader(title = shinyDashboardLogo(theme = "blue_gradient",
                                                                        boldText = "数据小工具",
                                                                        mainText = "SpringTiger",
                                                                        badgeText = "V1.0"), 
                                             titleWidth = 280,
                                             dropdownMenu(type = "notifications",
                                                          badgeStatus = "success",
                                                          icon = icon("question-circle"),
                                                          headerText = "网页直达",
                                                          notificationItem(text = "shinyApp官网",icon = icon("globe"),status = "danger",href = "https://www.shinyapps.io/"),
                                                          notificationItem(text = "图标查看网址",icon = icon("globe"),status = "danger",href = "https://icons.getbootstrap.com/?"),
                                                          notificationItem(text = "百度翻译",icon = icon("globe"),status = "danger",href = "https://fanyi.baidu.com/?aldtype=16047#auto/zh")
                                                          )
                                             ),
                    sidebar = dashboardSidebar(width = 310,
                                               sidebarUserPanel(name = "王春虎",
                                                                subtitle = a(icon(name = "circle",class = "text-success")),
                                                                image =  "https://gimg2.baidu.com/image_search/src=http%3A%2F%2Fimage.biaobaiju.com%2Fuploads%2F20190318%2F14%2F1552892126-YDfqghBbpu.jpg&refer=http%3A%2F%2Fimage.biaobaiju.com&app=2002&size=f9999,10000&q=a80&n=0&g=0n&fmt=jpeg?sec=1637393764&t=ba9c0adecb2311617e400eaa6365c1e1"),
                                               sidebarMenu(menuItem(text = "1. 转录组差异分析",icon = icon("facebook"),
                                                                    menuSubItem(text = "1.1 差异基因提取",tabName = "trans_dff_extract",icon = icon("search")),
                                                                    menuSubItem(text = "1.2 韦恩图",tabName = "trans_venn",icon = icon("align-center"))
                                                                    )
                                                           )
                                               ),
                    body = dashboardBody(shinyDashboardThemes(theme = "onenote"),
                                         tabItems(tabItem(tabName = "trans_dff_extract",
                                                          fluidPage(titlePanel(title = "1.1 差异基因提取"),
                                                                    sidebarPanel(),
                                                                    mainPanel(tabsetPanel(type = "tabs",
                                                                                          tabPanel(title = "1.1.1 差异基因交集",icon = icon("table"),column(width = 12,style = style,dataTableOutput("trans_dff_output"))),
                                                                                          tabPanel(title = "1.1.2 上调基因交集",icon = icon("table"),column(width = 12,style = style,dataTableOutput("trans_dff_up_output"))),
                                                                                          tabPanel(title = "1.1.3 下调基因交集",icon = icon("table"),column(width = 12,style = style,dataTableOutput("trans_dff_down_output")))
                                                                                          )
                                                                              )
                                                                    )
                                                          ),
                                                  tabItem(tabName = "trans_venn",
                                                          fluidPage(titlePanel(title = "1.2 韦恩图"),
                                                                    sidebarPanel(),
                                                                    mainPanel()
                                                                    )
                                                          )
                                                  )
                                         )
                      )
