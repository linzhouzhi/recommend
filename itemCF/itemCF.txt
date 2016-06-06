基于物品的矩阵协同过滤算法
/** 算法步骤
* 建立数学模型
* 用户邻居矩阵
* 最紧邻矩阵
* 推荐
*/


/**
* 建立数据模型
*/
FileDataModel <- function(file){
    #读取文件数据，列属性有头部不要显示header=FALSE
    data <- read.csv( file, header=FALSE )
    #重新定义头部信息
    names( data ) <- c( 'uid', 'iid', 'pref' )
    #取user行，item列
    user <- unique( data$uid )
    item <- unique( sort(data$iid) )
    #根据user行，item列构建空矩阵
    M <- matrix( 0, length( user ), length( item ) )
    rownames(M) <- user
    colnames(M) <- item
    #将data匹配成对应的坐标
    col_uid <- match( data$uid, user )
    col_iid <- match( data$iid, item )
    #组合成新的数据集
    new_data <- cbind( col_uid, col_iid, pref=data$pref )
    for( i in 1:nrow( new_data ) ){
        #行 new_data[i,][1], 列new_data[i,][2]
        M[ new_data[i,][1],new_data[i,][2] ] <- new_data[i,][3]
    }
    M
}

/**
* 欧氏距离相似度算法
*/
EuclideanDistanceSimilarit <- function(M){
    #将矩阵进行转置，其实是用了userCF模块的代码
    M <- t( M )
    n <- nrow( M )
    #创建一个n行n列的用户矩阵
    s <- matrix( 0, n, n )
    dimnames(s) <- list( rownames(M), rownames(M) )
    #每个用户i都要和其它的n-i个用户进行比较
    for( i in 1:(n-1) ){
        for( ii in (i+1):n ){
            inter_num <- intersect( which( M[i,] != 0 ),which( M[ii,] != 0 ))
            if( length( inter_num ) ){
                sum <- 0
                for( z in inter_num ){
                    sum <- sum+( M[ i, ][ z ] - M[ ii, ][ z ] )^2
                }
                s[ ii, i ] <- length( inter_num )/( length( inter_num ) + sqrt( sum ) )
            }
        }
    }
    #矩阵转置，并相加
    st <- t( s )
    s <- st + s
}

/**
* 最紧邻算法
*/
NearestNUserNeighborhood<-function(S,n){
    row <- nrow(S)
    neighbor <- matrix( 0, row, n)
    for( i in 1:row ){
        nuser = head( order( -1*S[,i] ), n)
        neighbor[ i, ] = nuser
    }
    neighbor
}

/**
* 基于物品的推荐算法
*/
ItemBasedRecommender<-function(uid,n,M,S,N){
    r_sum <- matrix( 0, 1, ncol(M) )
    #获取uid购买过的item
    bug_item <- which( M[ uid, ] != 0 )
    #找出每个item对应的相似高的item
    for( i in 1:length( bug_item ) ){
        nea_item <- N[ bug_item[ i ], ]
        #该用户购买物品i对应的评分
        pref <- M[ uid, bug_item[ i ] ]
        #临时矩阵用于保存相似度和商品评分相乘的相当于加权
        tmp <- matrix( 0, 1, ncol(M) )
        #吧这些item根据相似度和物品评分进行相乘后相加
        for( ii in 1:length( nea_item ) ){
            s <- S[ bug_item[ ii ], nea_item[ ii ] ]
            tmp[ 1, nea_item[ii] ] <- tmp[ 1, nea_item[ii] ] + s * pref
        }
        r_sum <- r_sum + tmp
    }
    #获取没有购买过的item
    sub_set <- intersect( which( r_sum[1,] != 0 ), which( M[uid,] == 0 ) )
    sub_set
    cols <- head( sort( sub_set ), n )
    #吧列转换为item的id
    cnames <- colnames( M )
    R <- matrix( 0, 1, length( cols ) )
    for( z2 in 1:length( cols ) ){
        R[1,][ z2 ] <- cnames[ cols[ z2 ] ]
    }
    R
}

/**
* 运行
*/
M <- FileDataModel( "testCF.csv" )
S <- EuclideanDistanceSimilarit( M )
N <- NearestNUserNeighborhood( S, 3 )
R <- ItemBasedRecommender(1,3,M,S,N); R