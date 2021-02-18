# Measurement Protocol Hits

    Code
      mp3
    Output
        fieldPath
      1    events
                                                                                             description
      1 Event at index: [0] has invalid name [_an_event]. Names must start with an alphabetic character.
        validationCode
      1   NAME_INVALID

---

    Code
      it1
    Output
      ==GA4 MP Event Item
      {
        "item_name": "jeggings",
        "item_variant": "Black",
        "price": 8.88
      } 

---

    Code
      items
    Output
      [[1]]
      ==GA4 MP Event Item
      {
        "item_id": "SKU_12345",
        "item_brand": "Gucci",
        "price": 9.99
      } 
      
      [[2]]
      ==GA4 MP Event Item
      {
        "item_name": "jeggings",
        "item_variant": "Black",
        "price": 8.88
      } 
      

---

    Code
      event1
    Output
      
      ==GA4 MP Event
      {
        "name": "add_payment_info",
        "params": {
          "coupon": "SUMMER_FUN",
          "payment_type": "Credit Card",
          "value": 7.77,
          "currency": "USD",
          "items": [
            {
              "item_id": "SKU_12345",
              "item_brand": "Gucci",
              "price": 9.99
            },
            {
              "item_name": "jeggings",
              "item_variant": "Black",
              "price": 8.88
            }
          ]
        }
      } 

