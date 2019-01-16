# cl-json-util
cl-json-util is a Common Lisp utility for JSON, to pretty print JSON and so on.


## License
Copyright(c) 2017 Muyinliu Xing Released under the ISC License.

## Dependencies
* [jsown](https://github.com/madnificent/jsown)

## Install and load with QuickLisp
In shell:
```shell
git clone https://github.com/muyinliu/cl-json-util.git
cp -r cl-json-util ~/quicklisp/local-projects/cl-json-util
```

Then in Common Lisp:
```lisp
(ql:quickload 'cl-json-util)
```

## Usage
### pretty print JSON
```lisp
(json-util:pprint-json "{\"HeWeather5\":[{\"basic\":{\"city\":\"北京\",\"cnty\":\"中国\",\"id\":\"CN101010100\",\"lat\":\"39.904000\",\"lon\":\"116.391000\",\"update\":{\"loc\":\"2017-03-21 08:51\",\"utc\":\"2017-03-21 00:51\"}},\"now\":{\"cond\":{\"code\":\"101\",\"txt\":\"多云\"},\"fl\":\"0\",\"hum\":\"73\",\"pcpn\":\"0\",\"pres\":\"1027\",\"tmp\":\"6\",\"vis\":\"7\",\"wind\":{\"deg\":\"350\",\"dir\":\"东北风\",\"sc\":\"4-5\",\"spd\":\"18\"}},\"status\":\"ok\"}]}")
```
```=>
{
    "HeWeather5": [
        {
            "basic": {
                "city": "北京",
                "cnty": "中国",
                "id": "CN101010100",
                "lat": "39.904000",
                "lon": "116.391000",
                "update": {
                    "loc": "2017-03-21 08:51",
                    "utc": "2017-03-21 00:51"
                }
            },
            "now": {
                "cond": {
                    "code": "101",
                    "txt": "多云"
                },
                "fl": "0",
                "hum": "73",
                "pcpn": "0",
                "pres": "1027",
                "tmp": "6",
                "vis": "7",
                "wind": {
                    "deg": "350",
                    "dir": "东北风",
                    "sc": "4-5",
                    "spd": "18"
                }
            },
            "status": "ok"
        }
    ]
}
; No value
```

### pretty JSON
Make JSON much more readable through add some newline or indent.
```lisp
(json-util:pretty-json "{\"HeWeather5\":[{\"basic\":{\"city\":\"北京\",\"cnty\":\"中国\",\"id\":\"CN101010100\",\"lat\":\"39.904000\",\"lon\":\"116.391000\",\"update\":{\"loc\":\"2017-03-21 08:51\",\"utc\":\"2017-03-21 00:51\"}},\"now\":{\"cond\":{\"code\":\"101\",\"txt\":\"多云\"},\"fl\":\"0\",\"hum\":\"73\",\"pcpn\":\"0\",\"pres\":\"1027\",\"tmp\":\"6\",\"vis\":\"7\",\"wind\":{\"deg\":\"350\",\"dir\":\"东北风\",\"sc\":\"4-5\",\"spd\":\"18\"}},\"status\":\"ok\"}]}")
```
```=>
"{
    \"HeWeather5\": [
        {
            \"basic\": {
                \"city\": \"北京\",
                \"cnty\": \"中国\",
                \"id\": \"CN101010100\",
                \"lat\": \"39.904000\",
                \"lon\": \"116.391000\",
                \"update\": {
                    \"loc\": \"2017-03-21 08:51\",
                    \"utc\": \"2017-03-21 00:51\"
                }
            },
            \"now\": {
                \"cond\": {
                    \"code\": \"101\",
                    \"txt\": \"多云\"
                },
                \"fl\": \"0\",
                \"hum\": \"73\",
                \"pcpn\": \"0\",
                \"pres\": \"1027\",
                \"tmp\": \"6\",
                \"vis\": \"7\",
                \"wind\": {
                    \"deg\": \"350\",
                    \"dir\": \"东北风\",
                    \"sc\": \"4-5\",
                    \"spd\": \"18\"
                }
            },
            \"status\": \"ok\"
        }
    ]
}"
```

## More
Welcome to reply.
