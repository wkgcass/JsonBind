package net.cassite.jsonbind

import net.cassite.jsonbind.parsers.{IfParser, MapAssemblingParser, ForeachParser, ValueParser}
import net.cassite.jsonbind.plugins.{DateFormatPlugin, VariablePlugin}
import net.cassite.jsonbind.views.JsValueView
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
    val app = new App(List(new ValueParser), List(new VariablePlugin))
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
    val app = new App(List(new ForeachParser, new ValueParser), List(new VariablePlugin))
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
    val app = new App(List(new MapAssemblingParser, new ValueParser), List(new VariablePlugin))
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
    val app = new App(List(new ForeachParser, new MapAssemblingParser, new ValueParser), List(new VariablePlugin))
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
    val app = new App(List(new IfParser), List(new VariablePlugin))
    app.bind(JsValueView("test", Json.parse(
      """{
        |   "root":{
        |      "if1":{
        |           "$if":{
        |              "{{bool}}":{
        |                   "key1":"value1"
        |              }
        |           }
        |      },
        |      "if2":{
        |          "$if":{
        |             "{{bool2}}":{
        |                  "key2":"value2"
        |             }
        |          }
        |      }
        |   }
        | }
      """.stripMargin))) {
      $scope =>
        $scope("bool") = true
        $scope("bool2") = false
    }

    app.view("test") should be(JsObject(Map("root" -> JsObject(Map("if1" -> JsObject(Map("key1" -> JsString("value1"))))))))
  }

  "ValueParser and VariablePlugin,DateFormatPlugin" should "fill the date with corresponding time" in {
    val app = new App(List(new ValueParser), List(new VariablePlugin, new DateFormatPlugin))
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
}

class NameClass(val name: String) {
  var anotherObj: AnotherClass = null

  def getName = name
}

class AnotherClass(val value: String)