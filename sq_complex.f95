PROGRAM SQ_Decoder_Complex
  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = KIND(1.0D0)
  INTEGER :: i, num_samples, sample_rate, num_channels, bits_per_sample
  COMPLEX(dp), ALLOCATABLE :: left(:), right(:), lb(:), rb(:)
  REAL(dp), ALLOCATABLE :: wav_data(:,:)
  CHARACTER(LEN=100) :: input_filename, output_filename

  PRINT *, "Podaj nazwę pliku wejściowego (WAV):"
  READ(*, '(A)') input_filename
  PRINT *, "Podaj nazwę pliku wyjściowego (WAV):"
  READ(*, '(A)') output_filename

  CALL ReadWav(input_filename, wav_data, num_samples, num_channels, sample_rate, bits_per_sample)

  IF (num_channels /= 2) THEN
     PRINT *, "Błąd: Plik ma ", num_channels, " kanałów, a oczekiwano 2 (stereo)."
     STOP
  END IF

  IF (bits_per_sample /= 16) THEN
     PRINT *, "Błąd: Plik ma ", bits_per_sample, " bitów na próbkę, a obsługiwane są tylko pliki 16-bit PCM."
     STOP
  END IF

  ALLOCATE(left(num_samples), right(num_samples), lb(num_samples), rb(num_samples))

  DO i = 1, num_samples
     left(i) = CMPLX(wav_data(1, i), 0.0_dp, dp)
     right(i) = CMPLX(wav_data(2, i), 0.0_dp, dp)
  END DO

  DO i = 1, num_samples
     lb(i) = (left(i) + right(i)) * EXP(-CMPLX(0.0_dp, 0.25_dp * 3.141592653589793_dp, dp))
     rb(i) = (left(i) - right(i)) * EXP(CMPLX(0.0_dp, 0.25_dp * 3.141592653589793_dp, dp))
  END DO

  ALLOCATE(wav_data(4, num_samples))
  DO i = 1, num_samples
     wav_data(1, i) = REAL(left(i), dp)
     wav_data(2, i) = REAL(right(i), dp)
     wav_data(3, i) = REAL(lb(i), dp)
     wav_data(4, i) = REAL(rb(i), dp)
  END DO

  CALL WriteWav(output_filename, wav_data, num_samples, 4, sample_rate)

  PRINT *, "Dekodowanie QS zakończone. Wynik zapisano w pliku: ", output_filename

CONTAINS

  SUBROUTINE ReadWav(filename, data, num_samples, num_channels, sample_rate, bits_per_sample)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(dp), ALLOCATABLE, INTENT(OUT) :: data(:,:)
    INTEGER, INTENT(OUT) :: num_samples, num_channels, sample_rate, bits_per_sample
    INTEGER :: i
    INTEGER(4) :: subchunk2_size
    CHARACTER(4) :: chunk_id, format, subchunk1_id, subchunk2_id
    INTEGER(2), ALLOCATABLE :: raw_data(:,:)

    OPEN(UNIT=10, FILE=filename, FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD', ACTION='READ')

    READ(10) chunk_id, format
    IF (chunk_id /= 'RIFF' .OR. format /= 'WAVE') THEN
       PRINT *, "Błąd: Niepoprawny format pliku WAV (ChunkID:", chunk_id, ", Format:", format, ")"
       CLOSE(10)
       STOP
    END IF

    READ(10) subchunk1_id, subchunk2_size
    READ(10) i, num_channels, sample_rate, bits_per_sample
    PRINT *, "Parametry pliku: Kanały:", num_channels, " Częstotliwość próbkowania:", sample_rate, " bps:", bits_per_sample
    
    IF (i /= 1) THEN
       PRINT *, "Błąd: Obsługiwane są tylko pliki PCM."
       CLOSE(10)
       STOP
    END IF

    num_samples = subchunk2_size / (num_channels * 2)
    ALLOCATE(raw_data(num_channels, num_samples))
    ALLOCATE(data(num_channels, num_samples))

    READ(10) raw_data
    data = REAL(raw_data, dp) / 32768.0_dp

    CLOSE(10)
  END SUBROUTINE ReadWav

  SUBROUTINE WriteWav(filename, data, num_samples, num_channels, sample_rate)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(dp), ALLOCATABLE, INTENT(IN) :: data(:,:)
    INTEGER, INTENT(IN) :: num_samples, num_channels, sample_rate
    INTEGER(2), ALLOCATABLE :: raw_data(:,:)
    INTEGER(4) :: subchunk2_size, chunk_size
    INTEGER :: i

    OPEN(UNIT=20, FILE=filename, FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE', ACTION='WRITE')

    subchunk2_size = num_samples * num_channels * 2
    chunk_size = 36 + subchunk2_size

    WRITE(20) 'RIFF', chunk_size, 'WAVE'
    WRITE(20) 'fmt ', 16, 1, num_channels, sample_rate
    WRITE(20) sample_rate * num_channels * 2, num_channels * 2, 16
    WRITE(20) 'data', subchunk2_size

    ALLOCATE(raw_data(num_channels, num_samples))
    DO i = 1, num_samples
       raw_data(:, i) = MAX(-32768, MIN(32767, NINT(data(:, i) * 32767.0_dp)))
    END DO

    WRITE(20) raw_data

    CLOSE(20)
  END SUBROUTINE WriteWav

END PROGRAM SQ_Decoder_Complex

