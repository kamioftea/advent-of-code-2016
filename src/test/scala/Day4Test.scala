import Day4._
import org.scalatest.{FunSuite, Matchers}

class Day4Test extends FunSuite with Matchers {

  test("can test a phrase is secure") {
    isSecure("aa bb cc dd ee") shouldBe true
    isSecure("aa bb cc dd aa") shouldBe false
    isSecure("aa bb cc dd aaa") shouldBe true
    isSecure("ab") shouldBe true
    isSecure("a b") shouldBe true
    isSecure("a a") shouldBe false
    isSecure("a a a") shouldBe false

    isSecure("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    isSecure("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    isSecure("nti rxr bogebb zdwrin") shouldBe true
    isSecure("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    isSecure("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe true
    isSecure("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true

  }

  test("can count secure phrases") {
    countSecure(Seq("aa bb")) shouldBe 1
    countSecure(Seq("aa bb", "aa bb")) shouldBe 2
    countSecure(Seq("aa aa", "aa aa")) shouldBe 0
    countSecure(Seq("aa aa", "aa bb")) shouldBe 1
    countSecure(Seq("aa bb")) shouldBe 1

    countSecure(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    countSecure(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 5

  }

  test("can test a phrase is *really* secure") {

    isReallySecure("aa bb cc dd ee") shouldBe true
    isReallySecure("aa bb cc dd aa") shouldBe false
    isReallySecure("aa bb cc dd aaa") shouldBe true
    isReallySecure("ab") shouldBe true
    isReallySecure("ab ba") shouldBe false
    isReallySecure("a b") shouldBe true
    isReallySecure("a a") shouldBe false
    isReallySecure("a a a") shouldBe false


    isReallySecure("abcde fghij") shouldBe true
    isReallySecure("abcde xyz ecdab") shouldBe false
    isReallySecure("a ab abc abd abf abj") shouldBe true
    isReallySecure("iiii oiii ooii oooi oooo") shouldBe true
    isReallySecure("oiii ioii iioi iiio") shouldBe false


    isReallySecure("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    isReallySecure("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    isReallySecure("nti rxr bogebb zdwrin") shouldBe true
    isReallySecure("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    isReallySecure("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe false
    isReallySecure("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true
  }

  test("can count *really* secure phrases") {

    countReallySecure(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "ab ba",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    countReallySecure(Seq(
      "abcde fghij",
      "abcde xyz ecdab",
      "a ab abc abd abf abj",
      "iiii oiii ooii oooi oooo",
      "oiii ioii iioi iiio"
    )) shouldBe 3

    countReallySecure(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 4
  }

}
