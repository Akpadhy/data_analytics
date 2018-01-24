devtools::install_github('apache/spark@v2.2.0', subdir='R/pkg')
devtools::install_github("SKKU-SKT/ggplot2.SparkR")
library(SparkR)  
sc=sparkR.session(master='local')  
install.packages("aws.s3")  
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAI3IMDONERQTNXH2Q", "AWS_SECRET_ACCESS_KEY" = "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",            "AWS_DEFAULT_REGION" = "us-west-2")

#Read the jason files for the 3 categories which are placed on the s3 buckets
movies_df <- SparkR::read.df("s3a://samamazonreview/jsondata/Movies_and_TV_5.json",header = T,"json")
kindle_df <- SparkR::read.df("s3a://samamazonreview/jsondata/Kindle_Store_5.json",header = T,"json")
cds_df <- SparkR::read.df("s3a://samamazonreview/jsondata/CDs_and_Vinyl_5.json",header = T,"json")

#A good start is to consider those reviews where at least 10 people have voted 
#(i.e., the 2nd number in the tuple is greater than 10)
#Therefore filtering out those which have "helpful[1]" or total votes more than 10.

#Read all the all the reviews related to cd vinyl into the dataframe
createOrReplaceTempView(cds_df, "cd_vinyl_view")
cd_master<-sql("
  select drv.* from
  (
    select asin, helpful[0] as useful_vote, helpful[1] as total_vote, overall,
        reviewText, reviewTime, reviewerID, reviewerName, summary, unixReviewTime
    from cd_vinyl_view
  )drv 
  where total_vote >=10 
")

#Obtain the rating count for the cd_vinyl
createOrReplaceTempView(cd_master, "cd_master_tbl")
freq_overall_cd <- sql ("select overall,count(*) from cd_master_tbl group by overall")
collect(freq_overall_cd)
#    overall count(1)
#       1    25921
#       4    26116
#       3    18099
#       2    16137
#       5    78362


#Read all the all the reviews related to movies into the dataframe
createOrReplaceTempView(movies_df, "movies_view")
movies_master<-sql("select drv.* from 
  (
    select asin, helpful[0] as useful_vote, helpful[1] as total_vote, overall,
      reviewText,reviewTime,reviewerID,reviewerName,summary,unixReviewTime
    from movies_view
  )drv 
  where total_vote >=10 
")

#Obtain the rating count for the movies
createOrReplaceTempView(movies_master, "movies_master_tbl")
freq_overall_movies <- sql ("
  select overall,count(*) 
  from 
    movies_master_tbl 
  group by overall
")
collect(freq_overall_movies)
#overall count(1)
#      1    46604
#      4    36013
#      3    25535
#      2    24013
#      5    81441

#Read all the all the reviews related to kindle into the dataframe
createOrReplaceTempView(kindle_df, "kindle_view")	
kindle_master<-sql("
  select drv.* from(
    select asin,helpful[0] as useful_vote,helpful[1] as total_vote,overall,
      reviewText,reviewTime,reviewerID,reviewerName,summary,unixReviewTime
    from kindle_view
  )
  drv where total_vote >=10 
")


# Obtain the rating count for the kindle
createOrReplaceTempView(kindle_master, "kindle_master_tbl")
freq_overall_kindle <- sql ("select overall,count(*) from kindle_master_tbl group by overall")
collect(freq_overall_kindle)
# Overall count(1)
#      1     2960
#      4     3045
#      3     2141
#      2     2257
#      5    11013

####################################################################################################################
#Perform basic analysis of data by summarizing the data
####################################################################################################################
# Summarize the cd categories

cds_flat_helpful_df <- SparkR::sql("select useful_vote,total_vote,overall from cd_master_tbl")
summ<-summary(cds_flat_helpful_df)
collect(summ)
#  summary        useful_vote         total_vote            overall
# count             164635             164635             164635
#  mean 16.727311932456647  23.44581650317369 3.6976706046709387
#stddev  23.77776396648639 26.068564844417274 1.5177594571197106
#   min                  0                 10                1.0
#   max               1955               2013                5.0
# As seen here the average 
 
 kindle_flat_helpful_df <- SparkR::sql("select useful_vote,total_vote,overall from from kindle_master_tbl")
 summ_kindle<-summary(kindle_flat_helpful_df)
 collect(summ_kindle)
 
#  summary       useful_vote        total_vote               overall
#   count             21416             21416              21416
#    mean  17.0861505416511 20.26900448262981  3.788849458348898
#  stddev 32.73175138229615 35.01836165911548 1.4950642938072929
#     min                 0                10                1.0
#     max              2350              2537                5.0


movies_flat_helpful_df <- SparkR::sql("select useful_vote,total_vote,overall from movies_master_tbl")
summ_movies<-summary(movies_flat_helpful_df)
collect(summ_movies)
#   summary        useful_vote         total_vote           overall
#   count             213606             213606            213606
#    mean   20.3553598681685 29.262459855996553 3.382358173459547
#  stddev 45.055332750805064  50.62456752850401 1.591416311925548
#     min                  0                 10               1.0
#     max               6084               6510               5.0

#Finding among the 3 categories are the following.

# Question 2: Which product category is likely to be purchased heavily
# Methodology: Using the 'number of reviews' as a proxy for the number of products sold
# Most reviews are for movies (213k) followed by cd (164k) and then kindle (21k) suggesting cd category are purchased heavily.

# Question 3: Which product category is likely to make the customers happy after the purchase
# Methodology: Using the Average user rating as a proxy for the number of products sold
# Kindle (3.78) has the highest average rating followed by cds (3.697) and then movies (3.38)
#Therefore among the 3 categories, kindle, which is having the highest average rating, 
#is likely to make the customers happy after purchase.
####################################################################################################################

#To check whther any mising productid are present in cd_vinyl dataset or not
cds_null<-SparkR::sql("select count(*) from cd_master_tbl where asin is  null")
collect(cds_null)   
#0 no records with productid as null
kindle_null<-SparkR::sql("select count(*) from kindle_master_tbl where asin is  null")
collect(kindle_null)
#0 no records with productid as null
movies_null<-SparkR::sql("select count(*) from movies_master_tbl where asin is  null")
collect(movies_null)
#0 no records with productid as null

cds_null_1<-SparkR::sql("select count(*) from cd_master_tbl where reviewerID is  null")
collect(cds_null_1) 
#0 no records with reviewerid is null;
kindle_null_1<-SparkR::sql("select count(*) from kindle_master_tbl where reviewerID is  null")
collect(kindle_null_1)
#0 no records with reviewerid is null; 
movies_null_1<-SparkR::sql("select count(*) from movies_master_tbl where reviewerID is  null")
collect(movies_null_1)
#0 no records with reviewerid is null; 

#All records across all three categories have a reviwerID and asin ("ID of the Product")
####################################################################################################################

#To check whether any product is purchased without any reviews given
cds_rvw<-SparkR::sql("select count(*) from cd_master_tbl where length(reviewText) = '0'")
collect(cds_rvw)  
#--5 products have been purchased without any reviews given.

#---to check whther any product is purchased without any reviews given
kindle_rvw<-SparkR::sql("select count(*) from kindle_master_tbl where length(reviewText) = '0'")
collect(kindle_rvw)
#--1 products have been purchased without any reviews given.


#---to check whther any product is purchased without any reviews given
movies_rvw<-SparkR::sql("select count(*) from movies_master_tbl where length(reviewText) = '0'")
collect(movies_rvw)
#--17 products have been purchased without any reviews given.

####################################################################################################################

# Question 1: Which product category has a larger market size
# Methodology: Using the number of reviewers as a proxy
# Movies has the largest market size.

# Finding out the number of unique reviewers who have purchased products in this category.
cd_mkt_size<-SparkR::sql("select count(distinct reviewerID) from cd_master_tbl")
collect(cd_mkt_size)
# 36527 unique reviewers for CD Category

kindle_mkt_size<-SparkR::sql("select count(distinct reviewerID) from kindle_master_tbl")
collect(kindle_mkt_size)
# 12135 unique reviewers for Kindle Category

movies_mkt_size<-SparkR::sql("select count(distinct reviewerID) from movies_master_tbl")
collect(movies_mkt_size)
# 46691 unique reviewers for Movies Category

#Highest number of unique reviewers are for the movies(46,691) category followed by CD (36,527) and then kindle (12,135)
####################################################################################################################

# Question 2 : Which product category is likely to be purchased heavily
# Methodology: Using the 'number of reviews' as a proxy for the number of products sold
# Movies are likely to be purchased heavily.

# inorder to find out which product is going to get purchased heavily, we need to find out total number of reviews for that product category
cd_prdct_prchse<-SparkR::sql("select count(reviewText) from cd_master_tbl")
collect (cd_prdct_prchse)
# 164,635  number of reviews received by the products.

# inorder to find out which product is going to get purchased heavily, we need to find out total number of reviews for that product category
kindle_prdct_prchse<-SparkR::sql("select count(reviewText) from kindle_master_tbl")
collect (kindle_prdct_prchse)
# 21,416

# inorder to find out which product is going to get purchased heavily, we need to find out total number of reviews for that product category
movies_prdct_prchse<-SparkR::sql("select count(reviewText) from movies_master_tbl")
collect (movies_prdct_prchse)
# 213,606

# Reconfirming the output of summary function which clearly showed movies had the highest review
# Most reviews are for movies (213k) followed by cd (164k) and then kindle (21k) suggesting cd category are purchased heavily.
####################################################################################################################

# Studying how helpfulness of review varies with rating.

#To find out the average helpfulness score across different ratings in cd dataset
cd_data_helpful<-SparkR::sql("
  select overall,avg(useful_vote/total_vote) as avg_help 
  from cd_master_tbl group by overall order by avg_help desc
")
collect(cd_data_helpful)
# overall  avg_help
#       5 0.8704626
#       4 0.8201023
#       3 0.5526238
#       2 0.3994748
#       1 0.3302536
# Ratings with 5 have the maximum avg helpfulness score in cd product category
# Also avg helpfulness score drastically increases when the rating is above 3

# To find out the average helpfulness score across different ratings in kindle dataset
kindle_data_helpful<-SparkR::sql("select overall,avg(useful_vote/total_vote) as avg_help from kindle_master_tbl group by overall order by avg_help desc")
collect(kindle_data_helpful)
#	overall  avg_help
#       5 0.9059213
#       4 0.9020633
#       3 0.8156612
#       2 0.7031916
#       1 0.5979802
# Rating with 5 and 4 have almost same helpfulness score
# Rating with 1 have a decent helpfulness score as compared to cd category
# On an average Kindle category seems to have the highest average helpful reviews (across all three categories)
# Across all 5 rating values Kindle has the highest useful rating indicating reviews are not useful/detailed/thoughtful.

#To find out the average helpfulness score across different ratings in movies dataset
movies_data_helpful<-SparkR::sql("select overall,avg(useful_vote/total_vote) as avg_help from movies_master_tbl group by overall order by avg_help desc")
collect(movies_data_helpful)
#      5 0.8174358
#      4 0.8056605
#      3 0.5954319
#      2 0.4332318
#      1 0.3775949
# Rating with 5 & 4 have close helpfulness score
# Helpfulness score is less compared to cds nd kindle for rating 4 & 5
# Drastic increase in the helpfulness score for rating >3

# Based on the data above it could be concluded that the reviews with rating 4 and 5 are found to be useful
# And the usefulness decreases with decrease in rating.
####################################################################################################################

# Studying how review length varies with rating.
# fix 7 ranges for length of reviews 

cd_rvw_cnt<-SparkR::sql("select useful_vote,total_vote,rating,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from cd_master_tbl ")
													 

createOrReplaceTempView(cd_rvw_cnt,"bin_master")
bin_1_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='very very short'")
bin_2_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='very short'")
bin_3_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='medium'")
bin_4_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='fairly medium'")
bin_5_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='large'")
bin_6_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='very large'")
bin_7_cd<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from bin_master where review_rating='very descriptive'")

collect(bin_1_cd)
#---0.5655317
collect(bin_2_cd)
#--0.7753505
collect(bin_3_cd)
#--0.8207163
collect(bin_4_cd)
#--0.8301845		
collect(bin_5_cd)
#--0.8288091
collect(bin_6_cd)
#--0.8303717
collect(bin_7_cd)
#---0.826527
#Hence we can conclude that we can see a increasing trend 
#i.e with the increase in the length of the review text the avg helpfulness score also increases

createOrReplaceTempView(cd_rvw_rtng,"cd_bin_master1")
cd_agg_1<-SparkR::sql("select review_rating,count(*) as cnt from cd_bin_master1   where overall=5 group by 1 order by 2 desc")
collect(cd_agg_1)
#         review_rating   cnt
#       very short 28061
#  very very short 27390
#           medium 12179
#    fairly medium  5528
#            large  2560
#       very large  1441
# very descriptive  1203



kindle_rvw_cnt<-SparkR::sql("select useful_vote,total_vote,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from kindle_master_tbl ")
													 

createOrReplaceTempView(kindle_rvw_cnt,"kindle_bin_master")
kindle_bin_1<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='very very short'")
kindle_bin_2<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='very short'")
kindle_bin_3<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='medium'")
kindle_bin_4<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='fairly medium'")
kindle_bin_5<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='large'")
kindle_bin_6<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='very large'")
kindle_bin_7<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from kindle_bin_master where review_rating='very descriptive'")

collect(kindle_bin_1)
#--0.8048557
collect(kindle_bin_2)
#--0.8771522
collect(kindle_bin_3)
#--0.8733698
collect(kindle_bin_4)
#--0.870729
collect(kindle_bin_5)
#--0.8642662
collect(kindle_bin_6)
#--0.8708497
collect(kindle_bin_7)
#-- 0.832351

#from the above statistics we infer that unlike in cd product category in kindle product category length of review text vs avg helpfulness score kind of constant
#it doesn't show any upward /downward trend as such

createOrReplaceTempView(kindle_rvw_rtng,"kindle_bin_master1")
kindle_agg_1<-SparkR::sql("select review_rating,count(*) as cnt from kindle_bin_master1   where overall=5 group by 1 order by 2 desc")
collect(kindle_agg_1)
# very very short 6912
#      very short 2714
#          medium  826
#   fairly medium  329
#           large  120
#      very large   62
#very descriptive   50


movies_rvw_cnt<-SparkR::sql("select useful_vote,total_vote,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from movies_master_tbl ")
													 

createOrReplaceTempView(movies_rvw_cnt,"movies_bin_master")
movies_bin_1<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='very very short'")


movies_bin_2<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='very short'")
movies_bin_3<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='medium'")
movies_bin_4<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='fairly medium'")
movies_bin_5<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='large'")
movies_bin_6<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='very large'")
movies_bin_7<-SparkR::sql("select avg(useful_vote/total_vote) as avg_help from movies_bin_master where review_rating='very descriptive'")

collect(movies_bin_1)
#--0.533024
collect(movies_bin_2)
#--0.7093621
collect(movies_bin_3)
#--0.7580187
collect(movies_bin_4)
#--0.770468
collect(movies_bin_5)
#--0.7712892
collect(movies_bin_6)
#--0.7726311
collect(movies_bin_7)
#--0.7656397

#from the above we see that unlike in other 2 categories,movies takes steep rise from bin 1 to bin 2 and then becomes constant
#so here after a certain point of lenght reviewText the avg helpfulness score remains fairly constant.


cd_rvw_rtng<-SparkR::sql("select useful_vote,total_vote,overall,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from cd_master_tbl ")
													 

kindle_rvw_rtng<-SparkR::sql("select useful_vote,total_vote,overall,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from kindle_master_tbl ")
													 
													 


movies_rvw_rtng<-SparkR::sql("select useful_vote,total_vote,overall,case when length(reviewText) >= 0 and length(reviewText)< 1000 then 'very very short' 
										when length(reviewText) >=1000 and length(reviewText)< 2000 then 'very short'
										   when length (reviewText) >=2000 and length(reviewText)< 3000 then 'medium'
										      when length(reviewText) >=3000 and length(reviewText)< 4000 then 'fairly medium'
											    when length(reviewText) >=4000 and length(reviewText)< 5000 then 'large'
												  when length(reviewText) >=5000 and length(reviewText)< 6000 then 'very large'
												    else 'very descriptive' end as review_rating
													 from movies_master_tbl ")


createOrReplaceTempView(movies_rvw_rtng,"movies_bin_master1")
movies_agg_1<-SparkR::sql("select review_rating,count(*) as cnt from movies_bin_master1   where overall=5 group by 1 order by 2 desc")
collect(movies_agg_1)
#review_rating   cnt
# very very short 29806
#      very short 24918
#          medium 12854
#   fairly medium  6518
#           large  3214
#very descriptive  2101
#      very large  2030
	

#Finally with movies having the largest market size and likely to be purchased heavily the data suggests that
# "Movies" should be the best choice for new product line to boost the revenue.

