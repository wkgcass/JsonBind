package net.cassite.jsonbind

import net.cassite.jsonbind.parsers.{IfParser, MapAssemblingParser, ForeachParser, ValueParser}
import net.cassite.jsonbind.plugins.{LogicOperatorPlugin, DateFormatPlugin, VariablePlugin}
import net.cassite.jsonbind.views.JsValueView
import net.cassite.jsonbind.views.InputStreamView
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json._

/**
 * unit test
 */
class TestJsonBind extends FlatSpec with Matchers {
  "A scope" should "get results in corresponding types" in {
    val $scope = new Scope(null)
    $scope("int1") = 1
    $scope("double2.0") = 2.0
    $scope("booleanTrue") = true
    $scope("stringABC") = "ABC"
    $scope("list") = List(1, 2, 3)
    $scope("map") = Map("a" -> 1, "b" -> 2, "c" -> 3)
    $scope("supplier") = () => true
    $scope("consumer") = (x: String) => {}
    $scope("func") = (x: String) => x

    $scope.integer("int1") should be(1)
    $scope.number("double2.0") should be(2.0)
    $scope.bool("booleanTrue") should be(right = true)
    $scope.string("stringABC") should be("ABC")
    $scope.list("list") should be(List(1, 2, 3))
    $scope.map("map") should be(Map("a" -> 1, "b" -> 2, "c" -> 3))
    $scope.supplier[Boolean]("supplier")() should be(right = true)
    $scope.consumer[String]("consumer")("x")
    $scope.func[String,String]("func")("x") should be("x")
  }

  "an Nil parser list in App, calling app.view(...)" should "provide exactly as the input" in {
    val app = new App
    app.bind(JsValueView("test", Json.parse( """{"name":"cass","lang":"scala"}"""))) { $scope => }
    app.view("test") should be(new JsObject(Map("name" -> new JsString("cass"), "lang" -> new JsString("scala"))))
  }

  "ValueParser and VariablePlugin in App, calling app.view(...)" should "fill the {{variable}} with right value" in {
    val app = new App(List(ValueParser), List(VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """
        |{
        |"name":"{{name}}",
        |"lang":"{{lang}}"
        |}""".stripMargin))) {
      $scope =>
        $scope("name") = "cass"
        $scope("lang") = "scala"
    }.bind(JsValueView("test2", Json.parse(
      """
        |{
        |"name":"{{name}}{{lang}}",
        |"lang":"{{lang}}"
        |}""".stripMargin))) {
      $scope =>
        $scope("name") = "cass"
        $scope("lang") = "scala"
    }.bind(JsValueView("test3", Json.parse(
      """
        |{
        |"name":"-{{name}}-{{lang}}-",
        |"lang":"-{{lang}}-"
        |}""".stripMargin))) {
      $scope =>
        $scope("name") = "cass"
        $scope("lang") = "scala"
    }.bind(JsValueView("test4", Json.parse(
      """
        |{
        |"name":"{{user.name}}",
        |"lang":"{{lang.name}}"
        |}""".stripMargin))) {
      $scope =>
        $scope("user") = new NameClass("cass")
        $scope("lang") = new NameClass("scala")
    }.bind(JsValueView("test5", Json.parse(
      """
        |{
        |"name":"{{user.name}}",
        |"another":"{{lang.anotherObj.value}}",
        |"lang":"{{lang.name}}"
        |}""".stripMargin))) {
      $scope =>
        $scope("user") = new NameClass("cass")
        val lang = new NameClass("scala")
        lang.anotherObj = new AnotherClass("another")
        $scope("lang") = lang
    }.bind(JsValueView("test6", Json.parse(
      """{
        |"boolean":"{{true}}",
        |"number":"{{1.0}}",
        |"string":"{{'value'}}"
        |}
      """.stripMargin))) {
      $scope =>
    }

    app.view("test") should be(new JsObject(Map("name" -> new JsString("cass"), "lang" -> new JsString("scala"))))
    app.view("test2") should be(new JsObject(Map("name" -> new JsString("cassscala"), "lang" -> new JsString("scala"))))
    app.view("test3") should be(new JsObject(Map("name" -> new JsString("-cass-scala-"), "lang" -> new JsString("-scala-"))))
    app.view("test4") should be(new JsObject(Map("name" -> new JsString("cass"), "lang" -> new JsString("scala"))))
    app.view("test5") should be(new JsObject(Map("name" -> new JsString("cass"), "another" -> new JsString("another"), "lang" -> new JsString("scala"))))
    app.view("test6") should be(new JsObject(Map("boolean" -> new JsBoolean(true), "number" -> new JsNumber(1.0), "string" -> JsString("value"))))
  }

  "ForeachParser,ValueParser and VariablePlugin in App, calling app.view(...)" should "generate repeated values" in {
    val app = new App(List(ForeachParser, ValueParser), List(VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |"something":{
        |"$foreach":{
        |"item in list":{
        |"name":"{{item}}"
        |}
        |}
        |}
        |}""".stripMargin))) {
      $scope =>
        $scope("list") = List("a", "b", "c")
    }

    app.view("test") should be(new JsObject(Map("something" -> new JsArray(List(new JsObject(Map("name" -> new JsString("a"))), new JsObject(Map("name" -> new JsString("b"))), new JsObject(Map("name" -> new JsString("c"))))))))
  }

  "MapAssemblingParser,ValueParser and VariablePlugin in App, calling app.view(...)" should "fill the {{key}} with right value" in {
    val app = new App(List(MapAssemblingParser, ValueParser), List(VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """
        |{
        |"{{user.name}}":"name",
        |"{{lang.name}}":"lang"
        |}
      """.stripMargin))) {
      $scope =>
        $scope("user") = new NameClass("cass")
        $scope("lang") = new NameClass("scala")
    }

    app.view("test") should be(JsObject(Map("cass" -> JsString("name"), "scala" -> JsString("lang"))))
  }

  "ForeachParser,MapAssemblingParser,ValueParser and VariablePlugin in App, calling app.view(...)" should "fill the {{key}} with right value" in {
    val app = new App(List(ForeachParser, MapAssemblingParser, ValueParser), List(VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |"something":{
        |"$foreach":{
        |"item in list":{
        |"{{item}}":"name"
        |}
        |}
        |}
        |}""".stripMargin))) {
      $scope =>
        $scope("list") = List("a", "b", "c")
    }.bind(JsValueView("test2", Json.parse(
      """{
        |"something":{
        |"$foreach":{
        |"(k,v) in map":{
        |"{{k}}":"{{v}}"
        |}
        |}
        |}
        |}""".stripMargin))) {
      $scope =>
        $scope("map") = Map("a" -> 1, "b" -> 2, "c" -> 3)
    }

    app.view("test") should be(new JsObject(Map("something" -> new JsArray(List(new JsObject(Map("a" -> new JsString("name"))), new JsObject(Map("b" -> new JsString("name"))), new JsObject(Map("c" -> new JsString("name"))))))))
    app.view("test2") should be(new JsObject(Map("something" -> new JsArray(List(new JsObject(Map("a" -> new JsNumber(1))), new JsObject(Map("b" -> new JsNumber(2))), new JsObject(Map("c" -> new JsNumber(3))))))))
  }

  "IfParser and VariablePlugin in App, calling app.view(...)" should "show only if the value is 'true'" in {
    val app = new App(List(IfParser), List(VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |   "root":{
        |      "if1":{
        |           "$if":{
        |              "{{bool}}":{
        |                   "key1":"value1"
        |              },
        |              "$else":{
        |                   "key1else":"value1else"
        |              }
        |           }
        |      },
        |      "if2":{
        |          "$if":{
        |             "{{bool2}}":{
        |                  "key2":"value2"
        |             },
        |             "$else":{
        |                  "key2else":"value2else"
        |             }
        |          }
        |      },
        |      "if3":{
        |          "$if":{
        |             "{{bool3}}":{
        |                 "key3":"value3"
        |             }
        |          }
        |      },
        |      "if4":{
        |         "$if":{
        |             "{{true}}":"value4"
        |         }
        |      }
        |   }
        | }
      """.stripMargin))) {
      $scope =>
        $scope("bool") = true
        $scope("bool2") = false
        $scope("bool3") = false
    }

    app.view("test") should be(JsObject(Map("root" -> JsObject(
      Map(
        "if1" -> JsObject(Map("key1" -> JsString("value1"))),
        "if2" -> JsObject(Map("key2else" -> JsString("value2else"))),
        "if4" -> JsString("value4")
      )
    ))))
  }

  "ValueParser and VariablePlugin,DateFormatPlugin" should "fill the date with corresponding time" in {
    val app = new App(List(ValueParser), List(VariablePlugin, DateFormatPlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |"time":"{{timestamp | dateformat('yyyy/m/d HH:i:s')}}"
        |}
      """.stripMargin))) {
      $scope =>
        $scope("timestamp") = 1450760814000L
    }

    app.view("test") should be(JsObject(Map("time" -> JsString("2015/12/22 13:6:54"))))
  }

  "IfParser,ValueParser and LogicOperatorPlugin,VariablePlugin" should "give correct boolean value" in {
    val app = new App(List(IfParser, ValueParser), List(LogicOperatorPlugin, VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |"==":"{{1==2}}",
        |"!=":"{{1!=2}}",
        |"<>":"{{1<>2}}",
        |"!":"{{!false}}",
        |"==2":"{{test1==test2}}",
        |"if1":{
        |   "$if":{
        |      "{{!false}}":{
        |          "key1":"value1"
        |      },
        |      "$else":{
        |           "key2":"value2"
        |       }
        |   }
        |}
        |}
      """.stripMargin
    ))) {
      $scope =>
        $scope("test1") = 1
        $scope("test2") = 1
    }

    app.view("test") should be(JsObject(
      Map(
        "==" -> JsBoolean(false),
        "!=" -> JsBoolean(true),
        "<>" -> JsBoolean(true),
        "!" -> JsBoolean(true),
        "==2" -> JsBoolean(true),
        "if1" -> JsObject(Map("key1" -> JsString("value1")))
      )))
  }

  "IfParser,ForeachParser,MapAssemblingParser,ValueParser and LogicOperatorPlugin,VariablePlugin,DateFormatPlugin" should "function properly" in {
    val app = new App(List(IfParser, ForeachParser, MapAssemblingParser, ValueParser), List(LogicOperatorPlugin, VariablePlugin, DateFormatPlugin))
    app.bind(InputStreamView("test", classOf[TestJsonBind].getResourceAsStream("/test.json"))) {
      $scope =>
        $scope("user") = new User(1, "cass", 1450760814000L, true, 3, 2333)
        $scope("zoneList") = List(
          new Zone(1, "java", 13,
            Map(
              new Post(1, "hello world") -> new Author(2, "SGSLX"),
              new Post(2, "water") -> new Author(3, "SM"),
              new Post(3, "ji shu tie") -> new Author(1, "cass")
            )
          ),
          new Zone(2, "macbook", 12,
            Map(
              new Post(4, "osx") -> new Author(4, "timo"),
              new Post(5, "win vs osx") -> new Author(5, "john")
            )
          )
        )
    }
    app.view("test") should be(
      JsObject(
        Map(
          "userId" -> JsNumber(1),
          "userName" -> JsString("cass"),
          "lastVisit" -> JsString("2015-12-22"),
          "vip" -> JsObject(
            Map(
              "vipLevel" -> JsNumber(3),
              "vipExp" -> JsNumber(2333)
            )
          ),
          "zones" -> JsArray(List(
            JsObject(
              Map(
                "id" -> JsNumber(1),
                "name" -> JsString("java"),
                "level" -> JsNumber(13),
                "posts" -> JsObject(
                  Map(
                    "1" -> JsObject(
                      Map(
                        "title" -> JsString("hello world"),
                        "author" -> JsObject(
                          Map(
                            "id" -> JsNumber(2),
                            "name" -> JsString("SGSLX")
                          )
                        )
                      )
                    ),
                    "2" -> JsObject(
                      Map(
                        "title" -> JsString("water"),
                        "author" -> JsObject(
                          Map(
                            "id" -> JsNumber(3),
                            "name" -> JsString("SM")
                          )
                        )
                      )
                    )
                  ))
              )),
            JsObject(
              Map(
                "id" -> JsNumber(2),
                "name" -> JsString("macbook"),
                "level" -> JsNumber(12),
                "posts" -> JsObject(
                  Map(
                    "4" -> JsObject(
                      Map(
                        "title" -> JsString("osx"),
                        "author" -> JsObject(
                          Map(
                            "id" -> JsNumber(4),
                            "name" -> JsString("timo")
                          )
                        )
                      )
                    ),
                    "5" -> JsObject(
                      Map(
                        "title" -> JsString("win vs osx"),
                        "author" -> JsObject(
                          Map(
                            "id" -> JsNumber(5),
                            "name" -> JsString("john")
                          )
                        )
                      )
                    )
                  ))
              )
            )
          ))
        )
      )
    )
  }
}

class NameClass(val name: String) {
  var anotherObj: AnotherClass = null

  def getName = name
}

class AnotherClass(val value: String)

class User(val id: Int, val name: String, val lastVisit: Long, val isVip: Boolean, val vipLevel: Int, val vipExp: Int)

class Zone(val id: Int, val name: String, val level: Int, val posts: Map[Post, Author])

class Post(val id: Int, val title: String)

class Author(val id: Int, val name: String)