/**基于标签的推荐 算法步骤
* 建立数据模型（user-tag，item-tag）
* user-item 相似矩阵
* 推荐
*/

/**
* 标签模型
*/
TagModel <- function(file){
   #读取数据
   data <- read.csv( file, header=FALSE )
   #命名头部
   names( data ) <- c( 'id', 'tag' )
   data
}

/**
* 建立用户标签数学模型
* file 是数据保存的文件
* tags 是标签模型
*/
UserDataModel <- function( file, tags ){
    #读取数据
    data <- read.csv( file, header=FALSE )
    #重新命名头部
    names( data ) <- c( "uid", "iid", "count" )
    #建立行列
    m <- unique( sort( data$uid ) )
    n <- unique( tags$id )
    #建立m行n列的空矩阵
    M <- matrix( 0, length( m ), length( n ) )
    #将数据集映射成坐标
    uid_map <- match( data$uid, m )
    iid_map <- match( data$iid, n )
    #根据坐标组合成新的数集
    new_data <- cbind( uid_map, iid_map, data$count )
    #讲new_data中的数据写入到矩阵M中
    for( i in 1:nrow( new_data ) ){
        #行 new_data[i,][1], 列new_data[i,][2]
        M[ new_data[i,][1],new_data[i,][2] ] <- new_data[i,][3]
    }
    #重命名
    dimnames( M ) <- list( m, tags$tag )
    M
}

/**
* 建立标签广告数学模型
* file 是数据保存的文件
* tags 是标签模型
*/
ItemDataModel <- function( file, tags ){
    #读取数据
    data <- read.csv( file, header=FALSE )
    #重新命名头部
    names( data ) <- c( "tagid", "iid", "count" )
    #建立m行n列
    m <- unique( tags$id )
    n <- unique( sort( data$iid ) )
    #建立m行n列的空矩阵
    M <- matrix( 0, length( m ), length( n ) )
    #将数据集映射成坐标
    tagid_map <- match( data$tagid, m )
    iid_map <- match( data$iid, n )
    #根据坐标组合成新的数集
    new_data <- cbind( tagid_map, iid_map, data$count )
    #讲new_data中的数据写入到矩阵M中
    for( i in 1:nrow( new_data ) ){
        #行 new_data[i,][1], 列new_data[i,][2]
        M[ new_data[i,][1],new_data[i,][2] ] <- new_data[i,][3]
    }
    #重命名
    dimnames( M ) <- list( tags$tag, n )
    M
}

/**
* 计算item 和 user之间的相似度
* user是用户标签数据模型
* item是标签item数据模型
*/
Similarit <- function( user, item ){
    #建立一个user为行，item为列的空矩阵
    nuser <- nrow( user )
    nitem <- ncol( item )
    S <- matrix( 0, nuser, nitem )
    dimnames(S)[[1]] <- rownames(user)
    dimnames(S)[[2]] <- colnames(item)
    #循环user
    for( i in 1:nuser ){
        sub_user <- user[i,]
        #每个用户都要跟每个有交集的item进行比较
        for( ii in 1:nitem ){
            sub_item <- item[ ,ii]
            #计算单个user和item直接的交集
            inter_num <- intersect( which( sub_user !=0 ), which( sub_item != 0 ))
            if( length( inter_num ) ){
                #循环计算每组相交的元素
                sum <- 0
                for( z in 1:length( inter_num ) ){
                    #取sub_user 和 sub_item 之间最小的数为交集
                    min_inter <- min( sub_user[ inter_num[ z ] ], sub_item[ inter_num[ z ] ] )
                    #取sub_user 和 sub_item 的并集，用交集除以并集计算相似度
                    sum_union <- sum( sub_user ) + sum( sub_item )
                    sum <- sum + min_inter/sum_union
                }
                S[ i, ii ] <- sum
            }
        }
    }
    S
}

/**
* item 如果是广告，那么广告投放算法如下
* iid是item对应的id
* n是投放的用户数
* S是user-item的相似矩阵
*/
Recommender<-function( iid, n, S ){
    #取iid对应的列
    col <- match( iid, colnames( S ) )
    col_data <- S[,col]
    #从大到小排序后取前面n个(也可以用order)
    sort_data <- sort( col_data, TRUE )
    R <- head( sort_data, n)
    R
}


/**
* 运行
*/
tags <- TagModel( "tag.csv" )
user_data <- UserDataModel( "user_tag.csv",tags )
item_data <- ItemDataModel( "item_tag.csv",tags )
S <- Similarit( user_data, item_data )
#广告102投放到3个用户
R <- Recommender( 102, 3, S );
