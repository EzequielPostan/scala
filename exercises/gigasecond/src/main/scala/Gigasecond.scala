import java.time.LocalDate
import java.time.LocalDateTime

/* Examples on differences and conversions for LocalDate and LocalDateTime
 * https://beginnersbook.com/2017/10/java-convert-localdate-to-localdatetime/
 *
 */
object Gigasecond {
  private val OneGigasecond: Long = 1000000000

  def add(startDate: LocalDate): LocalDateTime =
    add(startDate.atStartOfDay())

  def add(startDateTime: LocalDateTime): LocalDateTime =
    startDateTime.plusSeconds(OneGigasecond)
}
