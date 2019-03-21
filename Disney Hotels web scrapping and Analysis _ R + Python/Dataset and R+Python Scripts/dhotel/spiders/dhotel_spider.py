
from dhotel.items import DhotelItem
import scrapy
from scrapy import Spider, Request
import time


class DhotelSpiter(scrapy.Spider):
	name = 'dhotel_spider'
	allowed_urls = ['https://www.tripadvisor.com/']
	start_urls = ['https://www.tripadvisor.com/Hotel_Review-g34515-d84907-Reviews-Walt_Disney_World_Dolphin_Resort-Orlando_Florida.html']
	# start_urls = ['https://www.tripadvisor.com/Hotels-g1954828-Walt_Disney_World_Florida-Hotels.html']
	def parse(self, response):
		# results_url_list = ['https://www.tripadvisor.com/Hotel_Review-g34515-d84907-Reviews-Walt_Disney_World_Dolphin_Resort-Orlando_Florida.html']

		hotel_url_list = response.xpath('')

		for url in hotel_url_list:
			yield scrapy.Request(url, callback = self.parse_hotel)

	def parse_hotel(self, response):
		results_url_list = []
		current_url = response.url

		first, second = current_url.split('Reviews')
		for i in range(5, 3001, 5):
			results_url_list.append(first + 'Reviews'+ str(i) + second)
			# results_url_list.append("https://www.tripadvisor.com/Hotel_Review-g34515-d84907-Reviews-or"+str(i)+"-Walt_Disney_World_Dolphin_Resort-Orlando_Florida.html")

		for url in results_url_list:
			yield scrapy.Request(url, callback = self.parse_details)


	def parse_details(self, response):
		time.sleep(2)
		Hotel = response.xpath('//h1[@class="heading_title "]/text()').extract_first().strip()
		Add = response.xpath('//*[@id="taplc_location_detail_header_hotels_0"]/div[2]/div/div/span[2]/text()').extract()
		extentadd = response.xpath('//*[@id="taplc_location_detail_header_hotels_0"]/div[2]/div/div/span[3]/text()').extract()
		Zip = response.xpath('//*[@id="taplc_location_detail_header_hotels_0"]/div[2]/div/div/span[4]/text()').extract()
		stars = response.xpath('//div[@class="starRating detailListItem"]/div/text()').extract()
		
		reviews = response.xpath('//div[@class="review-container"]')
		for review in reviews:
			bubbles = review.xpath('.//div[@class="rating reviewItemInline"]/span/@class').extract()
			reviewdate = review.xpath('.//div[@class="rating reviewItemInline"]/span[@class="ratingDate relativeDate"]/@title').extract()
			reviewtitle = review.xpath('.//span[@class="noQuotes"]/text()').extract()
			commend = review.xpath('.//p[@class="partial_entry"]/text()').extract()
			badgetext = review.xpath('.//div[@class="memberBadgingNoText"]/span[4]/text()').extract()

			item = DhotelItem()
			item['Hotel'] = Hotel
			item['Add'] = Add
			item['extendadd'] = extentadd
			item['Zip'] = Zip
			item['stars'] = stars
			item['ratingbubble'] = bubbles
			item['ratingDate'] = reviewdate
			item['review_title'] = reviewtitle
			item['partial_entry'] = commend
			item['badgetext'] = badgetext
			yield item



	