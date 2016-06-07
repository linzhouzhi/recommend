/**
* Apriori挖掘关联规则的频繁项集算法
* 1.依据支持度找出所有频繁项集（频度）
* 2.依据置信度产生关联规则（强度）
* 支持度3%：意味着3%顾客同时购买牛奶和面包
* 置信度40%：意味着购买牛奶的顾客40%也购买面包
*/

/**
* 建立数学模型
*/
FileDataModel <- function(file){
    #读取文件数据，列属性有头部不要显示header=FALSE
    data <- read.csv( file, header=FALSE )
    tag_data <- cbind( data[,1], data[,2] )
    #取user行，tag列
    user <- unique( tag_data[,1] )
    tag <- unique( sort( tag_data[,2] ) )
    #根据user行，tag列构建空矩阵
    M <- matrix( 0, length( user ), length( tag ) )
    rownames(M) <- user
    colnames(M) <- tag
    #将data匹配成对应的坐标
    row_uid <- match( tag_data[,1], user )
    col_tid <- match( tag_data[,2], tag )
    #组合成新的数据集
    new_data <- cbind( row_uid, col_tid )
    for( i in 1:nrow( new_data ) ){
        #行 new_data[i,][1], 列new_data[i,][2]
        M[ new_data[i,][1],new_data[i,][2] ] <- 1
    }
    M
}

/**
* 建立标签出现的次数模型
*/
TagDataModel <- function( M ){
    row <- ncol( M )
    ids=colnames( M )
    #把每个标签出现的次数写入到T矩阵中
    tag_c <- NULL
    tag_l <- NULL
    for( i in 1:row ){
        tag_c <- append( tag_c, sum( M[ ,i ] ) )
        tag_l <- append( tag_l, as.integer(ids[i]) )
    }
    T <- data.frame( tag_id=tag_l, count = tag_c)
    T
}

/**
* 计算L1项集
*/
Apriori_L1 <- function( T, M, rate ){
    #把支持度转换为次数
    C <- nrow( M ) * rate
    tag_c <- NULL #用于保存标签出现的次数
    tag_l <- NULL #用于保存标签id
    tag_r <- NULL #用于保存标签出现的频率
    for( i in 1:nrow(T) ){
        count <- T$count[i]
        if( count >= C ){
            #保存标签名称，次数，还有频率
            tag_l <- append( tag_l, T$tag_id[i] )
            tag_c <- append( tag_c, count )
            tag_r <- append( tag_r, count/nrow(M) )
        }
    }
    R <- data.frame( tag_id=tag_l, count=tag_c, rate= tag_r )
    R
}

/**
* 计算L2项集
*/
Apriori_L2 <- function( L1, M, rate ){
     #把支持度转换为次数
     C <- nrow( M ) * rate
     #获取标签名称
     tags <- colnames(M)
     #存放标签组合
     tag_c1 <- NULL
     tag_c2 <- NULL
     tag_c <- NULL
     tag_r <- NULL
     #循环查找交集次数
     for( i in 1:(nrow(L1) -1) ){
         #获取该标签及所定义的列
         set_1 <- L1$tag_id[i]
         col_1 <- which( tags== set_1 )
         for( ii in (i+1):nrow(L1) ){
             set_2 <- L1$tag_id[ii]
             col_2 <- which( tags == set_2 )
             #求两列之间有交集的部分
             inter_num = intersect(which( M[,col_1] !=0 ),which( M[,col_2] != 0))
             count <- length(inter_num)
             if( count >= C ){
                 tag_c1 <- append( tag_c1, set_1 )
                 tag_c2 <- append( tag_c2, set_2 )
                 tag_c <- append( tag_c, count )
                 tag_r <- append( tag_r, count/nrow(M) )
             }
         }
     }
     L2 <- data.frame( col1=tag_c1, col2=tag_c2, count= tag_c, rate=tag_r )
     L2
 }

/**
* 计算L3项集
*/
Apriori_L3 <- function( L1, L2, M, rate ){
    #把支持度转换为次数
    C <- nrow( M ) * rate
    #获取标签名称
    tags <- colnames(M)
    #获取L1对应的标签
    L1_tags <- L1$tag_id
    tag_c <- NULL
    tag_c1 <- NULL
    tag_c2 <- NULL
    tag_c3 <- NULL
    tag_r <- NULL
    #循环查找L2和L1的交集次数
    for( i in 1:nrow(L2) ){
        set_c1 <- which( tags == L2$col1[i] )
        set_c2 <- which( tags == L2$col2[i] )
        set <- c( set_c1, set_c2 )
        #取出不包含set中的标签
        diff_tag <- setdiff( L1_tags, c(L2$col1[i],L2$col2[i]) )
        #计算set 中1,2号元素相交的坐标
        inter_tag <- intersect( which(M[,set[1] ]!=0) ,which(M[,set[2]] != 0) )
        for( ii in 1:length(diff_tag) ){
            r_num <- 0
            for( z in 1:length(inter_tag) ){
                #计算标签对应的列
                diff_col = which( tags == diff_tag[ ii ] )
                #获取列中的数据
                r_num <- r_num + as.integer( M[,diff_col][ inter_tag[z] ] )
            }
            if( length(r_num) != 0 && r_num >= C  ){
                tag_c1 <- append( tag_c1, tags[ set[1] ] )
                tag_c2 <- append( tag_c2, tags[ set[2] ]  )
                tag_c3 <- append( tag_c3, diff_tag[ ii ] )
                tag_c <- append( tag_c, r_num )
                tag_r <- append( tag_r, r_num/nrow(M) )
            }
        }
    }
    L3 <- data.frame( col1=tag_c1, col2=tag_c2, col3=tag_c3, count=tag_c, rate=tag_r )
    L3
}


/**
* 运行
*/
rate <- 0.4 #支持度
M <- FileDataModel( "user_tag.csv" )
T <- TagDataModel( M )
L1 <- Apriori_L1( T, M, rate )
L2 <- Apriori_L2( L1, M, rate )
L3 <- Apriori_L3( L1, L2, M, rate )