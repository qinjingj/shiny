library(shiny)
library(shinydashboard)
library(plotrix)
library(DT)

ui <- dashboardPage(
  #标题
  dashboardHeader(title=strong("贝特朗悖论模拟实验"),titleWidth =NULL,
                  dropdownMenu(type = "messages", badgeStatus = "warning",headerText ="实验研制版权说明",icon = shiny::icon("copyright"),
                               messageItem("实验制作","秦婧婧",icon = shiny::icon("user")),
                               messageItem("指导教师","安建业，程青青",icon = shiny::icon("user")),
                               messageItem("大创项目","基于R软件的概率统计模拟演示实验",icon = shiny::icon("users")) )
  ),
  #侧边栏
  dashboardSidebar(
    width=250,
    column(12,align="center",offset=0,
    br(),
    actionButton("syjj",strong("— 隐藏实验内容"),icon = shiny::icon("laptop"),width = 200,class = "btn-lg",click = "syjj_click"),
    selectInput("pro",
                h4("弦的随机选取方式"),
                c("方式一(样本空间为直径)"="垂直于某直径的弦",
                  "方式二(样本空间为圆周)"="圆周某点生成的弦" ,
                  "方式三(样本空间为圆面)"="圆内任意画出的弦")),
    sliderInput("xx", 
                label=h4("取弦次数"),
                min = 20, max = 500, value =20,ticks = TRUE,step=40,animate=TRUE),
    br(),
    actionButton("start",strong(" — 重复画弦实验"),icon = shiny::icon("laptop-code"),width = 200,class = "btn-lg"),
    br(),
    actionButton("sj",strong(" — 查看模拟数据"),icon = shiny::icon("file-alt"),width = 200,class = "btn-lg"),
    br(),
    actionButton("close",strong(" — 退出演示实验"),icon = shiny::icon("laptop-code"),width = 200,class = "btn-lg"),
    br(),
    downloadButton("downloadExcelSheet",strong(" - 下载模拟数据"),icon = shiny::icon("download"),width = 200,class = "btn-lg"),
    br(),
    br(),
    downloadButton("downloadExcelSheet2",strong(" - 下载画弦数据"),icon = shiny::icon("download"),width = 200,class = "btn-lg")
  )),
  #主体部分
  dashboardBody(
    fluidRow(
      box(title=strong("实验简介窗口"),width=NULL,status="info",solidHeader = TRUE,collapsible = TRUE,style="text-indent: 2em",
          br(),
          strong(textOutput("introduce")),
          textOutput("introduce1"),
          textOutput("introduce2"),
          textOutput("introduce3"),
          textOutput("introduce4"),
          textOutput("introduce5"),
          br()),
      
      box(title=strong("状态参数演示窗口"),width=NULL,status="info",solidHeader = TRUE,collapsible = TRUE,
          br(),
          valueBox(htmlOutput("selected_pro"), h5("选择模拟方式"), width = 4,icon = icon("square-full"), color = "blue"),
          valueBox(htmlOutput("cs"), h5("取弦次数"), width = 4,icon = icon("align-justify"), color = "blue"),
          valueBox(htmlOutput("npl"), h5("当前频率"), width = 4,icon = icon("pen"), color = "blue")),
      box(title=strong("模拟实验演示窗口"),width=NULL,status="info",solidHeader = TRUE,collapsible = TRUE,
          box(width = 5,background ="blue",plotOutput("sy",width = "100%", height = "400px")),
          box(width = 7,background ="blue",plotOutput("pl",click = "plot_click"))
      ),
      box(title=strong("实验次数及频率数据表格"),width=6,status="info",solidHeader = TRUE,collapsible = TRUE,
          DT::dataTableOutput('table')),
      box(title=strong("画弦坐标及长度数据表格"),width=6,status="info",solidHeader = TRUE,collapsible = TRUE,
          DT::dataTableOutput('table2'))
    ))
)

server <- function(input, output,session) {
  #实验原理分析
  output$introduce<-renderText("实验原理分析:")
  output$introduce1<-renderText("问题：在一给定圆内所有的弦中任选一条弦，求该弦的长度长于圆的内接正三角形边长的概率。"
  )
  output$introduce2<-renderText(
    "（1）L > √3 ,如果 ；│r│<1/2, 其发生的概率为1/2。由于对称性，可预先指定弦的方向。作垂直于此方向的直径，只有交直径于1/4 点与 3/4 点间的弦，其长才大于内接正三角形边长。所有交点是等可能的,则所求概率为1/2 。此时假定弦的中心在直径上均匀分布。"
  )
  output$introduce3<-renderText(
    "（2）L > √3 ,如果：2π/3<α<4π/3, 其发生的概率为1/3。由于对称性，可预先固定弦的一端。仅当弦与过此端点的切线的交角在60°～ 120° 之间，其长才合乎要求。所有方向是等可能的，则所求概率为1/3 。此时假定端点在圆周上均匀分布。"
  )
  output$introduce4<-renderText(
    "（3）L > √3 ,如果（x,y）在半径为1/2的圆内，其发生的概率为1/4.。弦被其中点位置唯一确定。只有当弦的中点落在半径缩小了一半的同心圆内，其长才合乎要求。中点位置都是等可能的,则所求概率为1/4。此时假定弦长被其中心唯一确定。"
  )
  output$introduce5<-renderText(
    "这一实验反映了选择不同的坐标会导致不同的概率分配这一事实。就以上悖论而言，造成这种现象的主要是在于条件的限制。若题目中出现“均匀分布”,“随机”,“等可能”这些字眼，则对应着此悖论中1,2.3条的结果。"
  )
  #实验简介显示控制
  observeEvent(input$syjj,if(input$syjj%%2==0){
    output$introduce <- renderText("实验原理分析:")
    output$introduce1 <- renderText("问题：在一给定圆内所有的弦中任选一条弦，求该弦的长度长于圆的内接正三角形边长的概率。")
    output$introduce2 <- renderText("（1）L > √3 ,如果：2π/3<α<4π/3, 其发生的概率为1/3。由于对称性，可预先固定弦的一端。仅当弦与过此端点的切线的交角在60°～ 120° 之间，其长才合乎要求。所有方向是等可能的，则所求概率为1/3 。此时假定端点在圆周上均匀分布。")
    output$introduce3 <- renderText("（2）L > √3 ,如果 ；│r│<1/2, 其发生的概率为1/2。由于对称性，可预先指定弦的方向。作垂直于此方向的直径，只有交直径于1/4 点与 3/4 点间的弦，其长才大于内接正三角形边长。所有交点是等可能的,则所求概率为1/2 。此时假定弦的中心在直径上均匀分布。")
    output$introduce4 <- renderText("（3）L > √3 ,如果（x,y）在半径为1/2的圆内，其发生的概率为1/4.。弦被其中点位置唯一确定。只有当弦的中点落在半径缩小了一半的同心圆内，其长才合乎要求。中点位置都是等可能的,则所求概率为1/4。此时假定弦长被其中心唯一确定。")
    output$introduce5 <- renderText("这一实验反映了选择不同的坐标会导致不同的概率分配这一事实。就以上悖论而言，造成这种现象的主要是在于条件的限制。若题目中出现“随机”,“均匀分布”,“等可能”这些字眼，则对应着此悖论中1,2.3条的结果。")
    updateActionButton(session,"syjj",label=" — 隐藏实验内容")
  }
  else{
    strong(updateActionButton(session,"syjj",label=" — 显示实验内容"))
    output$introduce  <- renderText("")
    output$introduce1 <- renderText("")
    output$introduce2 <- renderText("")
    output$introduce3 <- renderText("")
    output$introduce4 <- renderText("")
    output$introduce5 <- renderText("")
  }
  )
  
  #选择实验显示
  output$selected_pro <- renderText({
    paste(h4(input$pro))
  })
  
  
  #实验次数显示
  output$cs <- renderText({
    paste(h4(input$xx))
  })
  
  
  #圆内画弦
  output$sy <- renderPlot({
    input$pro
    plot(0, 1,xlim=c(-1,1),ylim=c(-1,1),asp=1,xlab="",ylab="",main="试验过程模拟演示")
    draw.circle(0,0,1,border="black",lwd=4)#画圆，坐标（0，0），半径1
    h1<-{}#横坐标1
    h2<-{}#横坐标2
    z1<-{}#纵坐标1
    z2<-{}#纵坐标2
    xc<-{}#弦长
    for(i in 1:input$xx){
      if(isolate(input$pro)=="垂直于某直径的弦"){
        y=runif(1,-1,1)#随机取一个纵坐标
        x1=(1-y^2)^(1/2)
        x2=-(1-y^2)^(1/2)#计算两个横坐标
        if(2*x1>3^0.5){
          lines(c(x1,x2),c(y,y),col="red")
        }else{lines(c(x1,x2),c(y,y),col="blue")} #画弦，坐标（x1，y），（x2，y）
        h1[i]=x1
        z1[i]=y
        h2[i]=x2
        z2[i]=y
        xc[i]=abs(x1-x2)
      }else if(isolate(input$pro)=="圆周某点生成的弦"){
        y=runif(1,-1,1)
        x1=(1-y^2)^(1/2)
        x2=-(1-y^2)^(1/2)
        x3<-sample(c(x1,x2),1)
        if((x1^2+(y+1)^2)^0.5>3^0.5){
          lines(c(0,x3),c(-1,y),col="red")
        }else{lines(c(0,x3),c(-1,y),col="blue")}#画弦，坐标（0，-1），（sample（x1，x2），y）
        h1[i]=0
        z1[i]=-1
        h2[i]=x3
        z2[i]=y
        xc[i]=((x3)^2+(y+1)^2)^0.5
      }else if(isolate(input$pro)=="圆内任意画出的弦"){
        draw.circle(0,0,0.5,border="black",lwd=2)
        y1=runif(1,-1,1)
        x1=(1-y1^2)^(1/2)
        x2=-(1-y1^2)^(1/2)
        y2=runif(1,-1,1)
        x3=(1-y2^2)^(1/2)
        x4=-(1-y2^2)^(1/2)
        x12=sample(c(x1,x2),1)
        x34=sample(c(x3,x4),1)
        if(((x34-x12)^2+(y2-y1)^2)^0.5>3^0.5){
          lines(c(x12,x34),c(y1,y2),col="red")
        }else{lines(c(x12,x34),c(y1,y2),col="blue")}#随意取弦，画弦
        h1[i]=x12
        z1[i]=y1
        h2[i]=x34
        z2[i]=y2
        xc[i]=((x12-x34)^2+(y1-y2)^2)^0.5
      }}
    
    #start键控制重复画弦
    input$start
    plot(0, 1,xlim=c(-1,1),ylim=c(-1,1),asp=1,xlab="",ylab="",main="试验过程模拟演示")
    draw.circle(0,0,1,border="black",lwd=4)
    
    for(i in 1:input$xx){
      if(isolate(input$pro)=="垂直于某直径的弦"){
        y=runif(1,-1,1)#随机取一个纵坐标
        x1=(1-y^2)^(1/2)
        x2=-(1-y^2)^(1/2)#计算两个横坐标
        if(2*x1>3^0.5){
          lines(c(x1,x2),c(y,y),col="red")
        }else{lines(c(x1,x2),c(y,y),col="blue")} #画弦，坐标（x1，y），（x2，y）
        h1[i]=x1
        z1[i]=y
        h2[i]=x2
        z2[i]=y
        xc[i]=abs(x1-x2)
      }else if(isolate(input$pro)=="圆周某点生成的弦"){
        y=runif(1,-1,1)
        x1=(1-y^2)^(1/2)
        x2=-(1-y^2)^(1/2)
        x3<-sample(c(x1,x2),1)
        if((x1^2+(y+1)^2)^0.5>3^0.5){
          lines(c(0,x3),c(-1,y),col="red")
        }else{lines(c(0,x3),c(-1,y),col="blue")}#画弦，坐标（0，-1），（sample（x1，x2），y）
        h1[i]=0
        z1[i]=-1
        h2[i]=x3
        z2[i]=y
        xc[i]=((x3)^2+(y+1)^2)^0.5
      }else if(isolate(input$pro)=="圆内任意画出的弦"){
        draw.circle(0,0,0.5,border="black",lwd=2)
        y1=runif(1,-1,1)
        x1=(1-y1^2)^(1/2)
        x2=-(1-y1^2)^(1/2)
        y2=runif(1,-1,1)
        x3=(1-y2^2)^(1/2)
        x4=-(1-y2^2)^(1/2)
        x12=sample(c(x1,x2),1)
        x34=sample(c(x3,x4),1)
        if(((x34-x12)^2+(y2-y1)^2)^0.5>3^0.5){
          lines(c(x12,x34),c(y1,y2),col="red")
        }else{lines(c(x12,x34),c(y1,y2),col="blue")}#随意取弦，画弦
        h1[i]=x12
        z1[i]=y1
        h2[i]=x34
        z2[i]=y2
        xc[i]=((x12-x34)^2+(y1-y2)^2)^0.5
      }}
    
    #输出弦坐标表格
    data2<-data.frame(横坐标1=format(h1,digits=3),纵坐标1=format(z1,digits=3),
                         横坐标2=format(h2,digits=3),纵坐标2=format(z2,digits=3),
                         弦长=format(xc,digits=3))
    observeEvent(input$sj,if(input$sj%%2==0){
      output$table2<-DT::renderDataTable(data.frame(NULL,NULL))
      updateActionButton(session,"sj",label=" — 查看模拟数据")
    }
    else{
      updateActionButton(session,"sj",label=" — 隐藏模拟数据")
      output$table2<-DT::renderDataTable(data2,
                                         extensions = 'Buttons', 
                                         options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
                                                        searchHighlight = TRUE,dom = 'lBfrtip',
                                                        scrollX = TRUE,fixedColumns = list(leftColumns =2, rightColumns = 1),
                                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                        lengthMenu = c(10, 20, 50, -1)))
    }
    )
    
    #下载弦坐标表格
    output$downloadExcelSheet2 <- downloadHandler(
      filename = function() {
        paste("data-huaxian-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data2,file)
      })
  })
  
  
  
  #画概率图
  observeEvent(input$pro,{
    p<-{}
    hh=0
    if(input$pro=="垂直于某直径的弦"){
      post<-function(n)
      {m<-0
      s<-runif(n,0,2)
      for(i in 1:n)
      {if(abs(s[i]-0)>1/2 && abs(s[i]-0)<3/2)
        m<-m+1}
      prob<-m/n
      prob
      }
      for(i in 1:500){
        p[i]<-post(i)
      }
      hh=1/2
    }else if(input$pro=="圆周某点生成的弦"){
      post<-function(n)
      {m<-0
      s<-runif(n,0,2*pi)
      t<-runif(n,0,2*pi)
      for(i in 1:n)
      {if(abs(s[i]-t[i])>(2/3)*pi && abs(s[i]-t[i])<(4/3)*pi)
        m<-m+1}
      prob<-m/n
      prob
      } 
      for(i in 1:500){
        p[i]<-post(i)
      }
      hh=1/3
    }else if(input$pro=="圆内任意画出的弦"){
      post<-function(n)
      {m<-0
      s<-runif(n,0,pi)
      for(i in 1:n)
      {if(abs(s[i]-0)>0 && abs(s[i]-0)<pi/4)
        m<-m+1}
      prob<-m/n
      prob
      }
      for(i in 1:500){
        p[i]<-post(i)
      }
      hh=1/4
    }else{print("运行错误！")}
    #输出数据表格
    data1<-data.frame(实验次数=1:500,频率=format(p[1:500],digits=3))
    observeEvent(input$sj,if(input$sj%%2==0){
      output$table<-DT::renderDataTable(data.frame(NULL,NULL))
      updateActionButton(session,"sj",label=" — 查看模拟数据")
    }
    else{
      updateActionButton(session,"sj",label=" — 隐藏模拟数据")
      output$table <- DT::renderDataTable(data1,
                                          extensions = 'Buttons', 
                                          options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
                                                         searchHighlight = TRUE,dom = 'lBfrtip',
                                                         scrollX = TRUE,fixedColumns = list(leftColumns =2, rightColumns = 1),
                                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         lengthMenu = c(10, 20, 50, -1)))
      
    }
    )
    
    #画图
    output$pl<-renderPlot({
      plot(1:input$xx,p[1:input$xx],type="p",xlim=c(20,500),ylim=c(0,1),xlab="每次实验取弦次数",ylab="频率",col="blue")
      abline(h=hh,col='red')
    })
    #当前频率显示
    output$npl <- renderText({
      paste(h4(p[input$xx]))
    })
    
  #下载模拟数据
  output$downloadExcelSheet <- downloadHandler(
    filename = function() {
      paste("data-moni-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data1,file)
    })
  })
  
  #结束按钮
  observeEvent(input$close,{stopApp()
  })
}


shinyApp(ui = ui, server = server)
