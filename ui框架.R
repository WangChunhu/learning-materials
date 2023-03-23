ui <- dashboardPage(title = "(SpringTiger)",skin = "blue",
                    header = dashboardHeader(title = shinyDashboardLogo(theme = "blue_gradient", #badgeText的主题
                                                                        boldText = "生信小工具",
                                                                        mainText = "SpringTiger",
                                                                        badgeText = "v1.1"),
                                             titleWidth = 280),
                    sidebar = dashboardSidebar(width = 310,
                                               sidebarUserPanel(name = "***",
                                                                subtitle = a(icon(name = "circle",class = "text-success"))),
                                               sidebarMenu(id = "侧边栏",
                                                           menuItem(text = ,
                                                                    icon = icon("facebook"),
                                                                    tabName = ,
                                                                    menuSubItem(text = ,
                                                                                tabName = ,
                                                                                icon = icon("table")))
                                                           )
                                               ),
                    body = dashboardBody(shinyDashboardThemes(theme = "onenote"),
                                         tabItems(tabItem(tabName = ,
                                                          fluidPage(titlePanel(title = ""),
                                                                    sidebarPanel(),
                                                                    mainPanel(tabsetPanel(type = "tabs")))),
                                                  
                                         )
                    ))
