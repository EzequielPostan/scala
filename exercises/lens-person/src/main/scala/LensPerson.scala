import java.time.LocalDate
import com.softwaremill.quicklens._

/*
  Just to play with another lenses implementation, I used Quicklens
  https://github.com/adamw/quicklens
*/

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)
  val personNameLens = modify(_:Person)(_._name)
  val personBornLens = modify(_:Person)(_._born)
  val personAddressLens = modify(_:Person)(_._address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)
  val nameForeNameLens = modify(_:Name)(_._foreNames)
  val nameSurNameLens = modify(_:Name)(_._surName)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)
  val bornBornAtLens = modify(_:Born)(_._bornAt)
  val bornBornOnLens = modify(_:Born)(_._bornOn)

  case class Address(_street: String, _houseNumber: Int,
    _place: String /*Village / city*/ , _country: String)
  val addressStreetLens = modify(_:Address)(_._street)
  val addressHouseNumberLens = modify(_:Address)(_._houseNumber)
  val addressPlaceLens = modify(_:Address)(_._place)
  val addressCountryLens = modify(_:Address)(_._country)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)
  def epochDayToGregorian(ed: EpochDay): Gregorian = {
    val ld = LocalDate.ofEpochDay(ed)
    Gregorian(ld.getYear, ld.getMonth.getValue, ld.getDayOfMonth)
  }
  val gregorianMonthLens = modify(_:Gregorian)(_._month)

  // Implement these.
  val bornStreet: Born => String = _._bornAt._street

  val setCurrentStreet: String => Person => Person =
    s => p => (personAddressLens andThenModify addressStreetLens)(p).using(_ => s)

  def newDate(p: Person, month: Int): Long = {
    require(1 <= month && month <= 12)
    val gregorianDate = gregorianMonthLens(epochDayToGregorian(p._born._bornOn)).using(_ => month)
    LocalDate.of(gregorianDate._year, gregorianDate._month, gregorianDate._dayOfMonth).toEpochDay
  }
  val setBirthMonth: Int => Person => Person =
    month => p => (personBornLens andThenModify bornBornOnLens)(p).using(_ => newDate(p,month))

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person =
    f => p => p.modifyAll(_._born._bornAt._street, _._address._street).using(f)
}
