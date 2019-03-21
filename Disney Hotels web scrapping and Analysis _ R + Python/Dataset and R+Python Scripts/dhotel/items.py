# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class DhotelItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    Hotel = scrapy.Field()
    Add = scrapy.Field()
    extendadd = scrapy.Field()
    Zip = scrapy.Field()
    stars = scrapy.Field()
    ratingbubble = scrapy.Field()
    ratingDate = scrapy.Field()
    review_title = scrapy.Field()
    partial_entry = scrapy.Field()
    badgetext = scrapy.Field()

    pass
