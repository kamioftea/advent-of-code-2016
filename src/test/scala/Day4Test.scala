import org.scalatest.{FunSuite, Matchers}

class Day4Test extends FunSuite with Matchers {

  test("testIsValid") {
    Day4.isValid("aa bb cc dd ee") shouldBe true
    Day4.isValid("aa bb cc dd aa") shouldBe false
    Day4.isValid("aa bb cc dd aaa") shouldBe true
    Day4.isValid("ab") shouldBe true
    Day4.isValid("a b") shouldBe true
    Day4.isValid("a a") shouldBe false
    Day4.isValid("a a a") shouldBe false

    Day4.isValid("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    Day4.isValid("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    Day4.isValid("nti rxr bogebb zdwrin") shouldBe true
    Day4.isValid("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    Day4.isValid("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe true
    Day4.isValid("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true

  }

  test("testCountValid") {
    Day4.countValid(Seq("aa bb")) shouldBe 1
    Day4.countValid(Seq("aa bb", "aa bb")) shouldBe 2
    Day4.countValid(Seq("aa aa", "aa aa")) shouldBe 0
    Day4.countValid(Seq("aa aa", "aa bb")) shouldBe 1
    Day4.countValid(Seq("aa bb")) shouldBe 1

    Day4.countValid(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    Day4.countValid(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 5

  }

  test("testIsReallyValid") {

    Day4.isReallyValid("aa bb cc dd ee") shouldBe true
    Day4.isReallyValid("aa bb cc dd aa") shouldBe false
    Day4.isReallyValid("aa bb cc dd aaa") shouldBe true
    Day4.isReallyValid("ab") shouldBe true
    Day4.isReallyValid("ab ba") shouldBe false
    Day4.isReallyValid("a b") shouldBe true
    Day4.isReallyValid("a a") shouldBe false
    Day4.isReallyValid("a a a") shouldBe false


    Day4.isReallyValid("abcde fghij") shouldBe true
    Day4.isReallyValid("abcde xyz ecdab") shouldBe false
    Day4.isReallyValid("a ab abc abd abf abj") shouldBe true
    Day4.isReallyValid("iiii oiii ooii oooi oooo") shouldBe true
    Day4.isReallyValid("oiii ioii iioi iiio") shouldBe false


    Day4.isReallyValid("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    Day4.isReallyValid("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    Day4.isReallyValid("nti rxr bogebb zdwrin") shouldBe true
    Day4.isReallyValid("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    Day4.isReallyValid("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe false
    Day4.isReallyValid("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true
  }

  test("testCountReallyValid") {

    Day4.countReallyValid(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "ab ba",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    Day4.countReallyValid(Seq(
      "abcde fghij",
      "abcde xyz ecdab",
      "a ab abc abd abf abj",
      "iiii oiii ooii oooi oooo",
      "oiii ioii iioi iiio"
    )) shouldBe 3

    Day4.countReallyValid(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 4
  }

}
