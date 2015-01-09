// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StStat.pas 4.04                             *}
{*********************************************************}
{* SysTools: Statistical math functions modeled on       *}
{*           those in Excel                              *}
{*********************************************************}

{$I StDefine.inc}

unit StStat;

{ The statistical distribution functions return results in singles }
{ since the fractional accuracy of these is typically about 3e-7.  }

interface

uses
  Windows,
  Math,
  SysUtils, StConst, StBase;

{AVEDEV}
function AveDev(const Data: array of Double) : Double;
function AveDev16(const Data; NData : Integer) : Double;
  {-Returns the average of the absolute deviations of data points from their
    mean. AveDev is a measure of the variability in a data set.
  }

{CONFIDENCE}
function Confidence(Alpha, StandardDev : Double; Size : Integer) : Double;
  {-Returns the confidence interval for a population mean.
    The confidence interval is a range on either side of a sample mean.
      Alpha is the significance level used to compute the confidence level.
        The confidence level equals 100*(1 - Alpha)%, or in other words,
        an Alpha of 0.05 indicates a 95 percent confidence level.
      StandardDev is the population standard deviation for the data range
        and is assumed to be known.
      Size is the sample Size.
  }

{CORREL}
function Correlation(const Data1, Data2 : array of Double) : Double;
function Correlation16(const Data1, Data2; NData : Integer) : Double;
  {-Returns the correlation coefficient of the Data1 and Data2 arrays.
    Use the correlation coefficient to determine the relationship between
    two data sets.
    This function also returns the same value as the PEARSON function
    in Excel.
  }

{COVAR}
function Covariance(const Data1, Data2 : array of Double) : Double;
function Covariance16(const Data1, Data2; NData : Integer) : Double;
  {-Returns covariance, the average of products of deviations, for the Data1
    and Data2 arrays. Use covariance to determine the relationship between
    two data sets.
  }

{DEVSQ}
function DevSq(const Data : array of Double) : Double;
function DevSq16(const Data; NData : Integer) : Double;
  {-Returns the sum of squares of deviations of data points from their
    sample mean.
  }

{FREQUENCY}
procedure Frequency(const Data : array of Double; const Bins : array of Double;
  var Counts : array of Integer);
procedure Frequency16(const Data; NData : Integer; const Bins; NBins : Integer;
  var Counts);
  {-Calculates how often values occur within an array of data,
    and then returns an array of counts.
      Data is an array of values for which you want to count frequencies.
      Bins is an array of intervals into which you want to group the values in
        Data.
      Counts is an array into which the frequency counts are returned. Counts
        must have one more element than does Bins. The first element of Counts
        has all items less than the first number in Bins. The next element of
        Counts is the items that fall between Bins[0] and Bins[1]. The last
        element of Counts has all items greater than the last number in Bins.
  }

{GEOMEAN}
function GeometricMean(const Data : array of Double) : Double;
function GeometricMean16(const Data; NData : Integer) : Double;
  {-Returns the geometric mean of an array of positive data. The geometric
    mean is the n'th root of the product of n positive numbers.}

{HARMEAN}
function HarmonicMean(const Data : array of Double) : Double;
function HarmonicMean16(const Data; NData : Integer) : Double;
  {-Returns the harmonic mean of an array of data. The harmonic mean is the
    reciprocal of the arithmetic mean of reciprocals.}

{LARGE}
function Largest(const Data : array of Double; K : Integer) : Double;
function Largest16(const Data; NData : Integer; K : Integer) : Double;
  {-Returns the K'th largest value in an array of data. You can use this
    function to select a value based on its relative standing.}

{MEDIAN}
function Median(const Data : array of Double) : Double;
function Median16(const Data; NData : Integer) : Double;
  {-Returns the median of the given numbers. The median is the number in the
    middle of a set of numbers; that is, half the numbers have values that
    are greater than the median, and half have values that are less.
    If there is an even number of numbers in the set, MEDIAN calculates
    the average of the two numbers in the middle.
  }

{MODE}
function Mode(const Data: array of Double) : Double;
function Mode16(const Data; NData : Integer) : Double;
  {-Returns the most frequently occurring, or repetitive, value in an array
    of data. In case of duplicate frequencies it returns the smallest
    such value.}

{PERCENTILE}
function Percentile(const Data : array of Double; K : Double) : Double;
function Percentile16(const Data; NData : Integer; K : Double) : Double;
  {-Returns the value of the K'th percentile of an array of data.
      K is the percentile value, a number between 0 and 1. If K is not a
        multiple of 1/(n-1) where n is the size of Data, Percentile
        interpolates between the closest bins.
  }

{PERCENTRANK}
function PercentRank(const Data : array of Double; X : Double) : Double;
function PercentRank16(const Data; NData : Integer; X : Double) : Double;
  {-Returns the percentile position of a value within an array of data.
      X is a data value. If X is not found within the array, PercentRank
        interpolates between the closest data points.
  }

{PERMUT}
function Permutations(Number, NumberChosen : Integer) : Extended;
  {-Returns the number of permutations for a given Number of objects that
    can be selected from Number objects. A permutation is any set or subset
    of objects or events where internal order is significant.
    Permutations are different from combinations, for which the internal order
    is not significant.
      Number is an Integer that describes the number of objects.
      NumberChosen is an Integer that describes the number of objects in
        each permutation.
  }

{COMBIN}
function Combinations(Number, NumberChosen : Integer) : Extended;
  {-Returns the number of combinations for a given Number of objects that
    can be selected from Number objects. A combination is any set or subset
    of objects or events where internal order is not significant.
      Number is an Integer that describes the number of objects.
      NumberChosen is an Integer that describes the number of objects in
        each permutation.
  }

{FACT}
function Factorial(N : Integer) : Extended;
  {-Returns N! as a floating point number.
    Extended is used for range, not accuracy.
  }

{RANK}
function Rank(Number : Double; const Data : array of Double;
  Ascending : Boolean) : Integer;
function Rank16(Number : Double; const Data; NData : Integer;
  Ascending : Boolean) : Integer;
  {-Returns the rank of a number in a list of numbers. If you were to sort
    a list that contained no duplicates, the rank of the Number would be its
    position within the sorted list.
      Number is the number whose rank you want.
      Data is the list of numbers.
      If Ascending is True the rank is measured from the beginning of the
        array; otherwise from the end of the array.
      If the Number is not found in the array, 0 is returned. Numbers that
        appear multiple times all have the same rank, but they affect the
        rank of numbers appearing after them. For example, in a list of
        Integers, if the number 10 appears twice and has a rank of 5,
        then 11 would have a rank of 7 (no number would have a rank of 6).
      Be sure to sort Data before calling this routine if you want an
        unambiguous ranking.
  }

{SMALL}
function Smallest(const Data : array of Double; K : Integer) : Double;
function Smallest16(const Data; NData : Integer; K : Integer) : Double;
  {-Returns the K'th smallest value in an array of data. You can use this
    function to select a value based on its relative standing.}

{TRIMMEAN}
function TrimMean(const Data : array of Double; Percent : Double) : Double;
function TrimMean16(const Data; NData : Integer; Percent : Double) : Double;
  {-Returns the mean of Data after trimming Percent points from the data set.
    If Percent is 0.2 and there are 20 points in Data, the 2 largest and 2
    smallest points would be dropped before computing the mean. Percent must
    be a number between 0 and 1.
  }

{--------------------------------------------------------------------------}

type
  {full statistics for a linear regression}
  TStLinEst = record
    B0, B1     : Double;   {model coefficients}
    seB0, seB1 : Double;   {standard error of model coefficients}
    R2         : Double;   {coefficient of determination}
    sigma      : Double;   {standard error of regression}
    SSr, SSe   : Double;   {elements for ANOVA table}
    F0         : Double;   {F-statistic to test B1=0}
    df         : Integer;  {denominator degrees of freedom for F-statistic}
  end;

{LINEST}
procedure LinEst(const KnownY : array of Double;
  const KnownX : array of Double; var LF : TStLinEst; ErrorStats : Boolean);
procedure LinEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);
  {-Performs linear fit to data and returns coefficients and error
    statistics.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
      NData must be greater than 2.
      If ErrorStats is FALSE, only B0 and B1 are computed; the other fields
        of TStLinEst are set to 0.0.
      See declaration of TStLinEst for returned data.

  }

{LOGEST}
procedure LogEst(const KnownY : array of Double;
  const KnownX : array of Double; var LF : TStLinEst; ErrorStats : Boolean);
procedure LogEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);
  {-Performs log-linear fit to data and returns coefficients and error
    statistics.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
      NData must be greater than 2.
      KnownY is transformed using ln(KnownY) before fitting:
        y = B0*B1^x implies that ln(y) = ln(B0)+X*ln(B1)
      In the returned LF, B0 and B1 are returned as shown. Other values in LF
      are in terms of the log transformation.
      If ErrorStats is FALSE, only B0 and B1 are computed; the other fields
        of TStLinEst are set to 0.0.
      See declaration of TStLinEst for returned data.
  }

{FORECAST}
function Forecast(X : Double; const KnownY: array of Double;
  const KnownX : array of Double) : Double;
function Forecast16(X : Double; const KnownY; const KnownX;
  NData : Integer) : Double;
  {-Calculates a future value by using existing values. The predicted value
    is a y-value for a given X-value. The known values are existing X-values
    and y-values, and the new value is predicted by using linear regression.
      X is the data point for which you want to predict a value.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
  }

{similar to GROWTH but more consistent with FORECAST}
function ForecastExponential(X : Double; const KnownY : array of Double;
  const KnownX : array of Double) : Double;
function ForecastExponential16(X : Double; const KnownY; const KnownX;
  NData : Integer) : Double;
  {-Calculates a future value by using existing values. The predicted value
    is a y-value for a given X-value. The known values are existing X-values
    and y-values, and the new value is predicted by using linear regression
    to an exponential growth model, y = B0*B1^X.
      X is the data point for which you want to predict a value.
      KnownY is the dependent array of known data points.
      KnownX is the independent array of known data points.
  }

{INTERCEPT}
function Intercept(const KnownY : array of Double;
  const KnownX : array of Double) : Double;
function Intercept16(const KnownY; const KnownX; NData : Integer) : Double;
  {-Calculates the point at which a line will intersect the y-axis by using
    existing X-values and y-values. The intercept point is based on a best-fit
    regression line plotted through the known X-values and known y-values.
    Use the intercept when you want to determine the value of the dependent
    variable when the independent variable is 0 (zero).
  }

{RSQ}
function RSquared(const KnownY : array of Double;
  const KnownX : array of Double) : Double;
function RSquared16(const KnownY; const KnownX; NData : Integer) : Double;
  {-Returns the square of the Pearson product moment correlation coefficient
    through data points in KnownY's and KnownX's. The r-squared value can
    be interpreted as the proportion of the variance in y attributable to
    the variance in X.
  }

{SLOPE}
function Slope(const KnownY : array of Double;
  const KnownX : array of Double) : Double;
function Slope16(const KnownY; const KnownX; NData : Integer) : Double;
  {-Returns the slope of the linear regression line through data points in
    KnownY's and KnownX's. The slope is the vertical distance divided by the
    horizontal distance between any two points on the line, which is the rate
    of change along the regression line.
  }

{STEYX}
function StandardErrorY(const KnownY : array of Double;
  const KnownX : array of Double) : Double;
function StandardErrorY16(const KnownY; const KnownX;
  NData : Integer) : Double;
  {-Returns the standard error of the predicted y-value for each X in a linear
    regression. The standard error is a measure of the amount of error in the
    prediction of y for an individual X.
  }

{--------------------------------------------------------------------------}

{BETADIST}
function BetaDist(X, Alpha, Beta, A, B : Single) : Single;
  {-Returns the cumulative beta probability density function.
    The cumulative beta probability density function is commonly used to
    study variation in the percentage of something across samples.
      X is the value at which to evaluate the function, A <= X <= B.
      Alpha is a parameter to the distribution.
      Beta is a parameter to the distribution.
      A is the lower bound to the interval of X.
      B is the upper bound to the interval of X.
      The standard beta distribution has A=0 and B=1.
    Fractional error less than 3.0e-7.
  }

{BETAINV}
function BetaInv(Probability, Alpha, Beta, A, B : Single) : Single;
  {-Returns the inverse of the cumulative beta probability density function.
    That is, if Probability = BetaDist(X,...), then
    BetaInv(Probability,...) = X.
      Probability is a probability (0 <= p <= 1) associated with the
        beta distribution.
      Alpha is a parameter to the distribution.
      Beta is a parameter to the distribution.
      A is the lower bound to the interval of the distribution.
      B is the upper bound to the interval of the distribution.
    Fractional error less than 3.0e-7.
  }

{BINOMDIST}
function BinomDist(NumberS, Trials : Integer; ProbabilityS : Single;
  Cumulative : Boolean) : Single;
  {-Returns the individual term binomial distribution probability.
    Use BinomDist in problems with a fixed number of tests or Trials,
    when the outcomes of any trial are only success or failure,
    when Trials are independent, and when the probability of success is
    constant throughout the experiment.
      NumberS is the number of successes in Trials.
      Trials is the number of independent trials.
      ProbabilityS is the probability of success on each trial.
      Cumulative is a logical value that determines the form of the function.
        If Cumulative is TRUE, then BinomDist returns the cumulative
        distribution function, which is the probability that there are at most
        NumberS successes; if FALSE, it returns the probability mass function,
        which is the probability that there are NumberS successes.
  }

{CRITBINOM}
function CritBinom(Trials : Integer; ProbabilityS, Alpha : Single) : Integer;
  {-Returns the smallest value for which the cumulative binomial distribution
    is greater than or equal to a criterion value.
      Trials is the number of Bernoulli trials.
      ProbabilityS is the probability of a success on each trial.
      Alpha is the criterion value.
  }

{CHIDIST}
function ChiDist(X : Single; DegreesFreedom : Integer) : Single;
  {-Returns the one-tailed probability of the chi-squared distribution.
    The chi-squared distribution is associated with a chi-squared test.
    Use the chi-squared test to compare observed and expected values.
      X is the value at which you want to evaluate the distribution.
      DegreesFreedom is the number of degrees of freedom.
  }

{CHIINV}
function ChiInv(Probability : Single; DegreesFreedom : Integer) : Single;
  {-Returns the inverse of the one-tailed probability of the chi-squared
    distribution. If Probability = ChiDist(X,...), then
    ChiInv(Probability,...) = X.
      Probability is a probability associated with the chi-squared
        distribution.
      DegreesFreedom is the number of degrees of freedom.
  }

{EXPONDIST}
function ExponDist(X, Lambda : Single; Cumulative : Boolean) : Single;
  {-Returns the exponential distribution. Use ExponDist to model the time
    between events.
      X is the value of the function.
      Lambda is the parameter value.
      Cumulative is a logical value that indicates which form of the
        exponential function to provide. If Cumulative is TRUE, ExponDist
        returns the cumulative distribution function; if FALSE, it returns
        the probability density function.
  }

{FDIST}
function FDist(X : Single; DegreesFreedom1, DegreesFreedom2 : Integer) : Single;
  {-Returns the F probability distribution. You can use this function to
    determine whether two data sets have different degrees of diversity.
      X is the value at which to evaluate the function.
      DegreesFreedom1 is the numerator degrees of freedom.
      DegreesFreedom2 is the denominator degrees of freedom.
    Fractional error less than 3.0e-7.
  }

{FINV}
function FInv(Probability : Single;
  DegreesFreedom1, DegreesFreedom2 : Integer) : Single;
  {-Returns the inverse of the F probability distribution. If
    p = FDist(X,...), then FInv(p,...) = X.
      Probability is a probability associated with the F cumulative
        distribution.
      DegreesFreedom1 is the numerator degrees of freedom.
      DegreesFreedom2 is the denominator degrees of freedom.
    Fractional error less than 3.0e-7.
  }

{LOGNORMDIST}
function LogNormDist(X, Mean, StandardDev : Single) : Single;
  {-Returns the cumulative lognormal distribution of X, where ln(X) is
    normally distributed with parameters Mean and StandardDev.
    Use this function to analyze data that has been logarithmically
    transformed.
      X is the value at which to evaluate the function.
      Mean is the mean of ln(X).
      StandardDev is the standard deviation of ln(X).
  }

{LOGINV}
function LogInv(Probability, Mean, StandardDev : Single) : Single;
  {-Returns the inverse of the lognormal cumulative distribution function of
    X, where ln(X) is normally distributed with parameters Mean and
    StandardDev. If p = LogNormDist(X,...) then LogInv(p,...) = X.
      Probability is a probability associated with the lognormal distribution.
      Mean is the mean of ln(X).
      StandardDev is the standard deviation of ln(X).
  }

{NORMDIST}
function NormDist(X, Mean, StandardDev : Single; Cumulative : Boolean) : Single;
  {-Returns the normal cumulative distribution for the specified Mean and
    standard deviation.
      X is the value for which you want the distribution.
      Mean is the arithmetic mean of the distribution.
      StandardDev is the standard deviation of the distribution.
      Cumulative is a logical value that determines the form of the function.
        If Cumulative is TRUE, NormDist returns the cumulative distribution
        function; if FALSE, it returns the probability density function.
  }

{NORMINV}
function NormInv(Probability, Mean, StandardDev : Single) : Single;
  {-Returns the inverse of the normal cumulative distribution for the
    specified mean and standard deviation.
      Probability is a probability corresponding to the normal distribution.
      Mean is the arithmetic mean of the distribution.
      StandardDev is the standard deviation of the distribution.
  }

{NORMSDIST}
function NormSDist(Z : Single) : Single;
  {-Returns the standard normal cumulative distribution function.
    The distribution has a mean of 0 (zero) and a standard deviation of one.
      Z is the value for which you want the distribution.
  }

{NORMSINV}
function NormSInv(Probability : Single) : Single;
  {-Returns the inverse of the standard normal cumulative distribution.
    The distribution has a mean of zero and a standard deviation of one.
      Probability is a probability corresponding to the normal distribution.
  }

{POISSON}
function Poisson(X : Integer; Mean : Single; Cumulative : Boolean) : Single;
  {-Returns the Poisson distribution.
      X is the number of events.
      Mean is the expected numeric value.
      Cumulative is a logical value that determines the form of the
        probability distribution returned. If Cumulative is TRUE, Poisson
        returns the cumulative Poisson probability that the number of random
        events occurring will be between zero and X inclusive; if FALSE,
        it returns the Poisson probability mass function that the number of
        events occurring will be exactly X.
  }

{TDIST}
function TDist(X : Single; DegreesFreedom : Integer; TwoTails : Boolean) : Single;
  {-Returns the Student's t-distribution. The t-distribution is used in the
    hypothesis testing of small sample data sets. Use this function in place
    of a table of critical values for the t-distribution.
      X is the numeric value at which to evaluate the distribution.
      DegreesFreedom is an Integer indicating the number of degrees of freedom.
      TwoTails if a logical value that indicates the number of distribution
        tails to return. If FALSE, TDist returns the one-tailed distribution;
        otherwise it returns the two-tailed distribution.
  }

{TINV}
function TInv(Probability : Single; DegreesFreedom : Integer) : Single;
  {-Returns the inverse of the Student's t-distribution for the specified
    degrees of freedom.
      Probability is the probability associated with the two-tailed Student's
        t-distribution.
      DegreesFreedom is the number of degrees of freedom to characterize
        the distribution.
  }

{--------------------------------------------------------------------------}
{undocumented functions you can call if you need}

function Erfc(X : Single) : Single;
  {-Returns the complementary error function with fractional error
    everywhere less than 1.2e-7. X is any finite value.
  }

function GammaLn(X : Single) : Single;
  {-Returns ln(Gamma(X)) where X > 0.0.}

{--------------------------------------------------------------------------}

{.$DEFINE Debug}
{$IFDEF Debug}
{like Largest and Smallest but using slower simpler algorithm}
function LargestSort(const Data: array of Double; K : Integer) : Double;
function SmallestSort(const Data: array of double; K : Integer) : Double;
{$ENDIF}

implementation

procedure RaiseStatError(Code : Integer);
  {-Generate a statistics exception}
var
  E : EStStatError;
begin
  E := EStStatError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure DoubleArraySort(var Data; NData : Integer);
  {-Heapsort an array of Doubles into Ascending order}
type
  TDoubleArray1 = array[1..StMaxBlockSize div SizeOf(Double)] of Double;
var
  i : Integer;
  T : Double;
  DA : TDoubleArray1 absolute Data;

  procedure Adjust(i, N : Integer);
  var
    j : Integer;
    S : Double;
  begin
    {j is left child of i}
    j := 2*i;
    {save i'th element temporarily}
    S := DA[i];
    while (j <= N) do begin
      {compare left and right child}
      if (j < N) and (DA[j] < DA[j+1]) then
        {j indexes larger child}
        inc(j);
      if (S >= DA[j]) then
        {a position for item is found}
        break;
      {move the larger child up a level}
      DA[j shr 1] := DA[j];
      {look at left child of j}
      j := j shl 1;
    end;
    {store saved item}
    DA[j shr 1] := S;
  end;

begin
  {transform the elements into a heap}
  for i := (NData shr 1) downto 1 do
    Adjust(i, NData);

  {repeatedly exchange the maximum at top of heap with the last element}
  for i := NData downto 2 do begin
    T := DA[1];
    DA[1] := DA[i];
    DA[i] := T;
    {update the heap for the remaining elements}
    Adjust(1, i-1);
  end;
end;

function CopyAndSort(const Data; NData : Integer;
  var SD : PDoubleArray) : Cardinal;
  {-Allocates heap space for an array copy, moves data, sorts, and returns size}
var
  Size : Integer;
begin
  Size := Integer(NData)*sizeof(Double);
  {if (Size > MaxBlockSize) then}
  {  RaiseStatError(stscStatBadCount);}
  Result := Size;
  getmem(SD, Size); {raises exception if insufficient memory}
  try
    move(Data, SD^, Size);
    DoubleArraySort(SD^, NData);
  except
    freemem(SD, Size);
    raise;
  end;
end;

function AveDev(const Data: array of Double) : Double;
begin
  Result := AveDev16(Data, High(Data)+1);
end;

function Mean(const Data; NData : Integer) : Extended;
  {-Computes the mean of an array of Doubles}
var
  i : Integer;
  s : Extended;
begin
  s := 0.0;
  for I := 0 to NData-1 do
    s := s+TDoubleArray(Data)[I];
  Result := s/NData;
end;

function AveDev16(const Data; NData : Integer) : Double;
var
  i : Integer;
  m, s : Extended;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  {compute sum of absolute deviations}
  m := Mean(Data, NData);
  s := 0.0;
  for i := 0 to NData-1 do
    s := s+abs(TDoubleArray(Data)[i]-m);

  Result := s/NData;
end;

function Confidence(Alpha, StandardDev : Double; Size : Integer) : Double;
begin
  if (StandardDev <= 0) or (Size < 1) then
    RaiseStatError(stscStatBadParam);
  Result := NormSInv(1.0-Alpha/2.0)*StandardDev/sqrt(Size);
end;

function Correlation(const Data1, Data2 : array of Double) : Double;
begin
  if (High(Data1) <> High(Data2)) then
    RaiseStatError(stscStatBadCount);
  Result := Correlation16(Data1, Data2, High(Data1)+1);
end;

function Correlation16(const Data1, Data2; NData : Integer) : Double;
var
  sx, sy, xmean, ymean, sxx, sxy, syy, x, y : Extended;
  i : Integer;
begin
  if (NData <= 1) then
    RaiseStatError(stscStatBadCount);

  {compute basic sums}
  sx := 0.0;
  sy := 0.0;
  sxx := 0.0;
  sxy := 0.0;
  syy := 0.0;
  for i := 0 to NData-1 do begin
    x := TDoubleArray(Data1)[i];
    y := TDoubleArray(Data2)[i];
    sx := sx+x;
    sy := sy+y;
    sxx := sxx+x*x;
    syy := syy+y*y;
    sxy := sxy+x*y;
  end;
  xmean := sx/NData;
  ymean := sy/NData;
  sxx := sxx-NData*xmean*xmean;
  syy := syy-NData*ymean*ymean;
  sxy := sxy-NData*xmean*ymean;

  Result := sxy/sqrt(sxx*syy);
end;

function Covariance(const Data1, Data2 : array of Double) : Double;
begin
  if (High(Data1) <> High(Data2)) then
    RaiseStatError(stscStatBadCount);
  Result := Covariance16(Data1, Data2, High(Data1)+1);
end;

function Covariance16(const Data1, Data2; NData : Integer) : Double;
var
  sx, sy, xmean, ymean, sxy, x, y : Extended;
  i : Integer;
begin
  if (NData <= 1) then
    RaiseStatError(stscStatBadCount);

  {compute basic sums}
  sx := 0.0;
  sy := 0.0;
  sxy := 0.0;
  for i := 0 to NData-1 do begin
    x := TDoubleArray(Data1)[i];
    y := TDoubleArray(Data2)[i];
    sx := sx+x;
    sy := sy+y;
    sxy := sxy+x*y;
  end;
  xmean := sx/NData;
  ymean := sy/NData;
  sxy := sxy-NData*xmean*ymean;

  Result := sxy/NData;
end;

function DevSq(const Data: array of Double) : Double;
begin
  Result := DevSq16(Data, High(Data)+1);
end;

function DevSq16(const Data; NData : Integer) : Double;
var
  i : Integer;
  sx, sxx, x : Extended;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  sx := 0.0;
  sxx := 0.0;
  for i := 0 to NData-1 do begin
    x := TDoubleArray(Data)[i];
    sx := sx+x;
    sxx := sxx+x*x;
  end;
  Result := sxx-sqr(sx)/NData;
end;

procedure Frequency(const Data: array of Double; const Bins: array of Double;
  var Counts: array of Integer);
begin
  if (High(Counts) <= High(Bins)) then
    RaiseStatError(stscStatBadCount);
  Frequency16(Data, High(Data)+1, Bins, High(Bins)+1, Counts);
end;

procedure Frequency16(const Data; NData : Integer; const Bins; NBins : Integer;
  var Counts);
var
  b, i : Integer;
  Size : Cardinal;
  SD : PDoubleArray;
begin
  if (NData <= 0) or (NBins <= 0) then
    RaiseStatError(stscStatBadCount);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {initialize all counts to zero}
    fillchar(Counts, (NBins+1)*sizeof(Integer), 0);

    {scan all data elements, putting into correct bin}
    b := 0;
    i := 0;
    while (i < NData) do begin
      if (SD^[i] <= TDoubleArray(Bins)[b]) then begin
        {current data element falls into this bin}
        inc(TIntArray(Counts)[b]);
        inc(i);
      end else begin
        {move to next bin that collects data}
        repeat
          inc(b);
        until (b = NBins) or (TDoubleArray(Bins)[b] > SD^[i]);
        if (b = NBins) then begin
          {add remaining elements to the catchall bin}
          inc(TIntArray(Counts)[b], NData-i);
          i := NData;
        end;
      end;
    end;

  finally
    freemem(SD, Size);
  end;
end;

function GeometricMean(const Data: array of Double) : Double;
begin
  Result := GeometricMean16(Data, High(Data)+1);
end;

function GeometricMean16(const Data; NData : Integer) : Double;
var
  i : Integer;
  s, t : Extended;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  s := 1.0;
  for i := 0 to NData-1 do begin
    t := TDoubleArray(Data)[i];
    if (t <= 0.0) then
      RaiseStatError(stscStatBadData);
    s := s*t;
  end;

  Result := Power(s, 1.0/NData);
end;

function HarmonicMean(const Data: array of Double) : Double;
begin
  Result := HarmonicMean16(Data, High(Data)+1);
end;

function HarmonicMean16(const Data; NData : Integer) : Double;
var
  i : Integer;
  s, t : Extended;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  s := 0.0;
  for i := 0 to NData-1 do begin
    t := TDoubleArray(Data)[i];
    if (t = 0.0) then
      RaiseStatError(stscStatBadData);
    s := s+(1.0/t);
  end;
  Result := NData/s;
end;

function Largest(const Data: array of Double; K : Integer) : Double;
begin
  Result := Largest16(Data, High(Data)+1, K);
end;

function Largest16(const Data; NData : Integer; K : Integer) : Double;
var
  b, t, i, j : integer;
  Size : Integer;
  temp, pval : Double;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (K <= 0) or (K > NData) then
    RaiseStatError(stscStatBadParam);

  Size := Integer(NData)*sizeof(Double);
  {if (Size > MaxBlockSize) then}
  {  RaiseStatError(stscStatBadCount);}
  getmem(SD, Size); {raises exception if insufficient memory}
  try
    move(Data, SD^, Size);

    {make K 0-based}
    dec(K);

    {use quicksort-like selection}
    b := 0;
    t := NData-1;
    while (t > b) do begin
      {use random pivot in case of already-sorted data}
      pval := SD^[b+random(t-b+1)];
      i := b;
      j := t;
      repeat
        while (SD^[i] > pval) do
          inc(i);
        while (pval > SD^[j]) do
          dec(j);
        if (i <= j) then begin
          temp := SD^[i];
          SD^[i] := SD^[j];
          SD^[j] := temp;
          inc(i);
          dec(j);
        end;
      until (i > j);
      if (j < K) then
        b := i;
      if (K < i) then
        t := j;
    end;
    Result := SD^[K];

  finally
    freemem(SD, Size);
  end;
end;

{debug version of Largest: slower but simpler}
{$IFDEF Debug}
function LargestSort(const Data: array of Double; K : Integer) : Double;
var
  Size : Cardinal;
  NData : Integer;
  SD : PDoubleArray;
begin
  NData := High(Data)+1;
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (K <= 0) or (K > NData) then
    RaiseStatError(stscStatBadParam);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {K=1 returns largest value, K=NData returns smallest}
    Result := SD^[NData-K];
  finally
    freemem(SD, Size);
  end;
end;
{$ENDIF}

function Median(const Data: array of Double) : Double;
begin
  Result := Median16(Data, High(Data)+1);
end;

function Median16(const Data; NData : Integer) : Double;
var
  m : Integer;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  m := NData shr 1;
  if odd(NData) then
    Result := Largest16(Data, NData, m+1)
  else
    Result := (Largest16(Data, NData, m+1)+Largest16(Data, NData, m))/2.0;
end;

function Mode(const Data: array of Double) : Double;
begin
  Result := Mode16(Data, High(Data)+1);
end;

function Mode16(const Data; NData : Integer) : Double;
var
  maxf, i, f : Integer;
  Size : Cardinal;
  last, max : Double;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {find the value with highest frequency}
    last := SD^[0];
    max := last;
    f := 1;
    maxf := f;

    for i := 1 to NData-1 do begin
      if SD^[i] = last then
        {keep count of identical values}
        inc(f)
      else begin
        {start a new series}
        if f > maxf then begin
          max := last;
          maxf := f;
        end;
        last := SD^[i];
        f := 1;
      end;
    end;

    {test last group}
    if f > maxf then
      max := last;

    Result := max;
  finally
    freemem(SD, Size);
  end;
end;

function Percentile(const Data: array of Double; K : Double) : Double;
begin
  Result := Percentile16(Data, High(Data)+1, K);
end;

function Percentile16(const Data; NData : Integer; K : Double) : Double;
const
  eps = 1.0e-10;
var
  ibin : Integer;
  Size : Cardinal;
  rbin, l, h : Double;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (K < 0.0) or (K > 1.0) then
    RaiseStatError(stscStatBadParam);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {find nearest bins}
    rbin := K*(NData-1);
    ibin := Trunc(rbin);
    if Frac(rbin) < eps then
      {very close to array index below}
      Result := SD^[ibin]
    else if (Int(rbin)+1.0-rbin) < eps then
      {very close to array index above}
      Result := SD^[ibin+1]
    else begin
      {need to interpolate between two bins}
      l := SD^[ibin];
      h := SD^[ibin+1];
      Result := l+(h-l)*(K*(NData-1)-ibin);
    end;
  finally
    freemem(SD, Size);
  end;
end;

function PercentRank(const Data: array of Double; X : Double) : Double;
begin
  Result := PercentRank16(Data, High(Data)+1, X);
end;

function PercentRank16(const Data; NData : Integer; X : Double) : Double;
var
  b, t, m : Integer;
  Size : Cardinal;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {test end conditions}
    if (X < SD^[0]) or (X > SD^[NData-1]) then
      RaiseStatError(stscStatBadParam);

    {find nearby bins using binary search}
    b := 0;
    t := NData-1;
    while (t-b) > 1 do begin
      m := (b+t) shr 1;
      if (X >= SD^[m]) then
        {search upper half}
        b := m
      else
        {search lower half}
        t := m;
    end;

    {now X is known to be between b (inclusive) and b+1}
    {handle duplicate elements below b}
    while (b > 0) and (SD^[b-1] = X) do
      dec(b);

    if (SD^[b] = X) then
      {an exact match}
      Result := b/(NData-1)
    else
      {interpolate}
      Result := (b+(X-SD^[b])/(SD^[b+1]-SD^[b]))/(NData-1);

  finally
    freemem(SD, Size);
  end;
end;

const
  sqrt2pi = 2.5066282746310005; {sqrt(2*pi)}

function GammaLn(X : Single) : Single;
  {-Returns ln(Gamma(X)) where X > 0}
const
  cof : array[0..5] of Double = (
    76.18009172947146, -86.50532032941677, 24.01409824083091,
    -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5);
var
  y, tmp, ser : Double;
  j : Integer;
begin
  if (X <= 0) then
    RaiseStatError(stscStatBadParam);

  y := X;
  tmp := X+5.5;
  tmp := tmp-(X+0.5)*ln(tmp);
  ser := 1.000000000190015;
  for j := low(cof) to high(cof) do begin
    y := y+1.0;
    ser := ser+cof[j]/y;
  end;
  Result := -tmp+ln(sqrt2pi*ser/X);
end;

const
  MFactLnA = 65;
var
  FactLnA : array[2..MFactLna] of Single; {lookup table of FactLn values}

function FactLn(N : Integer) : Single;
  {-Returns ln(N!) for N >= 0}
begin
  if (N <= 1) then
    Result := 0.0
  else if (N <= MFactLnA) then
    {use lookup table}
    Result := FactLnA[N]
  else
    {compute each time}
    Result := GammaLn(N+1.0);
end;

const
  MFactA = 33;
var
  FactA : array[2..MFactA] of Double; {lookup table of factorial values}

function Factorial(N : Integer) : Extended;
begin
  if (N < 0) then
    RaiseStatError(stscStatBadParam);

  if (N <= 1) then
    Result := 1.0
  else if (N <= MFactA) then
    {use lookup table}
    Result := FactA[N]
  else
    {bigger than lookup table allows. may overflow!}
    Result := exp(GammaLn(N+1.0))
end;

function Permutations(Number, NumberChosen : Integer) : Extended;
begin
  if (Number < 0) or (NumberChosen < 0) or (Number < NumberChosen) then
    RaiseStatError(stscStatBadParam);
  {the 0.5 and Int function clean up roundoff error for smaller N and K}
  Result := Int(0.5+exp(FactLn(Number)-FactLn(Number-NumberChosen)));
end;

function Combinations(Number, NumberChosen : Integer) : Extended;
begin
  if (Number < 0) or (NumberChosen < 0) or (Number < NumberChosen) then
    RaiseStatError(stscStatBadParam);
  {the 0.5 and Int function clean up roundoff error for smaller N and K}
  Result := Int(0.5+exp(FactLn(Number)-FactLn(NumberChosen)
    -FactLn(Number-NumberChosen)));
end;

function Rank(Number: Double; const Data: array of Double;
  Ascending: Boolean) : Integer;
begin
  Result := Rank16(Number, Data, High(Data)+1, Ascending);
end;

function Rank16(Number: Double; const Data; NData : Integer;
  Ascending: Boolean) : Integer;
var
  r : Integer;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);

  {Data not known to be sorted, so must search linearly}
  if (Ascending) then begin
    for r := 0 to NData-1 do
      if TDoubleArray(Data)[r] = Number then begin
        Result := r+1;
        exit;
      end;
  end else begin
    for r := NData-1 downto 0 do
      if TDoubleArray(Data)[r] = Number then begin
        Result := NData-r;
        exit;
      end;
  end;
  Result := 0;
end;

function Smallest(const Data: array of Double; K : Integer) : Double;
begin
  Result := Smallest16(Data, High(Data)+1, K);
end;

function Smallest16(const Data; NData : Integer; K : Integer) : Double;
var
  b, t, i, j : integer;
  Size : Integer;
  temp, pval : Double;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (K <= 0) or (K > NData) then
    RaiseStatError(stscStatBadParam);

  Size := Integer(NData)*sizeof(Double);
  {if (Size > MaxBlockSize) then}
  {  RaiseStatError(stscStatBadCount);}
  getmem(SD, Size); {raises exception if insufficient memory}
  try
    move(Data, SD^, Size);

    {make K 0-based}
    dec(K);
    {use quicksort-like selection}
    b := 0;
    t := NData-1;
    while (t > b) do begin
      {use random pivot in case of already-sorted data}
      pval := SD^[b+random(t-b+1)];
      i := b;
      j := t;
      repeat
        while (SD^[i] < pval) do
          inc(i);
        while (pval < SD^[j]) do
          dec(j);
        if (i <= j) then begin
          temp := SD^[i];
          SD^[i] := SD^[j];
          SD^[j] := temp;
          inc(i);
          dec(j);
        end;
      until (i > j);

      if (j < K) then
        b := i;
      if (K < i) then
        t := j;
    end;
    Result := SD^[K];
  finally
    freemem(SD, Size);
  end;
end;

{debug version of Smallest: slower but simpler}
{$IFDEF Debug}
function SmallestSort(const Data: array of double; K : Integer) : Double;
var
  Size : Cardinal;
  NData : Integer;
  SD : PDoubleArray;
begin
  NData := High(Data)+1;
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (K <= 0) or (K > NData) then
    RaiseStatError(stscStatBadParam);

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {K=1 returns smallest value, K=NData returns largest}
    Result := SD^[K-1];
  finally
    freemem(SD, Size);
  end;
end;
{$ENDIF}

function TrimMean(const Data: array of Double; Percent : Double) : Double;
begin
  Result := TrimMean16(Data, High(Data)+1, Percent);
end;

function TrimMean16(const Data; NData : Integer; Percent : Double) : Double;
var
  ntrim : Integer;
  Size : Cardinal;
  SD : PDoubleArray;
begin
  if (NData <= 0) then
    RaiseStatError(stscStatBadCount);
  if (Percent < 0.0) or (Percent > 1.0) then
    RaiseStatError(stscStatBadParam);

  {compute total number of trimmed points, rounding down to an even number}
  ntrim := trunc(Percent*NData);
  if odd(ntrim) then
    dec(ntrim);

  {take the easy way out when possible}
  if (ntrim = 0) then begin
    Result := Mean(Data, NData);
    exit;
  end;

  {copy and sort array}
  Size := CopyAndSort(Data, NData, SD);
  try
    {return Mean of remaining data points}
    Result := Mean(SD^[ntrim shr 1], NData-ntrim);
  finally
    freemem(SD, Size);
  end;
end;

{------------------------------------------------------------------------}

procedure LinEst(const KnownY: array of Double;
  const KnownX: array of Double; var LF : TStLinEst; ErrorStats : Boolean);
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  LinEst16(KnownY, KnownX, High(KnownY)+1, LF, ErrorStats);
end;

procedure LinEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);
var
  i : Integer;
  sx, sy, xmean, ymean, sxx, sxy, syy, x, y : Extended;
begin
  if (NData <= 2) then
    RaiseStatError(stscStatBadCount);

  {compute basic sums}
  sx := 0.0;
  sy := 0.0;
  sxx := 0.0;
  sxy := 0.0;
  syy := 0.0;
  for i := 0 to NData-1 do begin
    x := TDoubleArray(KnownX)[i];
    y := TDoubleArray(KnownY)[i];
    sx := sx+x;
    sy := sy+y;
    sxx := sxx+x*x;
    syy := syy+y*y;
    sxy := sxy+x*y;
  end;
  xmean := sx/NData;
  ymean := sy/NData;
  sxx := sxx-NData*xmean*xmean;
  syy := syy-NData*ymean*ymean;
  sxy := sxy-NData*xmean*ymean;

  {check for zero variance}
  if (sxx <= 0.0) or (syy <= 0.0) then
    RaiseStatError(stscStatBadData);

  {initialize returned parameters}
  fillchar(LF, sizeof(LF), 0);

  {regression coefficients}
  LF.B1 := sxy/sxx;
  LF.B0 := ymean-LF.B1*xmean;

  {error statistics}
  if (ErrorStats) then begin
    LF.ssr := LF.B1*sxy;
    LF.sse := syy-LF.ssr;
    LF.R2 := LF.ssr/syy;
    LF.df := NData-2;
    LF.sigma := sqrt(LF.sse/LF.df);
    if LF.sse = 0.0 then
      {pick an arbitrarily large number for perfect fit}
      LF.F0 := 1.7e+308
    else
      LF.F0 := (LF.ssr*LF.df)/LF.sse;
    LF.seB1 := LF.sigma/sqrt(sxx);
    LF.seB0 := LF.sigma*sqrt((1.0/NData)+(xmean*xmean/sxx));
  end;
end;

procedure LogEst(const KnownY: array of Double;
  const KnownX: array of Double; var LF : TStLinEst; ErrorStats : Boolean);
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  LogEst16(KnownY, KnownX, High(KnownY)+1, LF, ErrorStats);
end;

procedure LogEst16(const KnownY; const KnownX; NData : Integer;
  var LF : TStLinEst; ErrorStats : Boolean);
var
  i : Integer;
  Size : Integer;
  lny : PDoubleArray;
begin
  if (NData <= 2) then
    RaiseStatError(stscStatBadCount);

  {allocate array for the log-transformed data}
  Size := Integer(NData)*sizeof(Double);
  {f (Size > MaxBlockSize) then}
  { RaiseStatError(stscStatBadCount);}
  getmem(lny, Size);
  try
    {initialize transformed data}
    for i := 0 to NData-1 do
      lny^[i] := ln(TDoubleArray(KnownY)[i]);

    {fit transformed data}
    LinEst16(lny^, KnownX, NData, LF, ErrorStats);

    {return values for B0 and B1 in exponential model y=B0*B1^x}
    LF.B0 := exp(LF.B0);
    LF.B1 := exp(LF.B1);
    {leave other values in LF in log form}
  finally
    freemem(lny, Size);
  end;
end;

function Forecast(X : Double; const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := Forecast16(X, KnownY, KnownX, High(KnownY)+1);
end;

function Forecast16(X : Double; const KnownY; const KnownX;
  NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LinEst16(KnownY, KnownX, NData, LF, false);
  Result := LF.B0+LF.B1*X;
end;

function ForecastExponential(X : Double; const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := ForecastExponential16(X, KnownY, KnownX, High(KnownY)+1);
end;

function ForecastExponential16(X : Double; const KnownY; const KnownX;
  NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LogEst16(KnownY, KnownX, NData, LF, false);
  Result := LF.B0*Power(LF.B1, X);
end;

function Intercept(const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := Intercept16(KnownY, KnownX, High(KnownY)+1);
end;

function Intercept16(const KnownY; const KnownX; NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LinEst16(KnownY, KnownX, NData, LF, false);
  Result := LF.B0;
end;

function RSquared(const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := RSquared16(KnownY, KnownX, High(KnownY)+1);
end;

function RSquared16(const KnownY; const KnownX; NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LinEst16(KnownY, KnownX, NData, LF, true);
  Result := LF.R2;
end;

function Slope(const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := Slope16(KnownY, KnownX, High(KnownY)+1);
end;

function Slope16(const KnownY; const KnownX; NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LinEst16(KnownY, KnownX, NData, LF, false);
  Result := LF.B1;
end;

function StandardErrorY(const KnownY: array of Double;
  const KnownX: array of Double) : Double;
begin
  if (High(KnownY) <> High(KnownX)) then
    RaiseStatError(stscStatBadCount);
  Result := StandardErrorY16(KnownY, KnownX, High(KnownY)+1);
end;

function StandardErrorY16(const KnownY; const KnownX;
  NData : Integer) : Double;
var
  LF : TStLinEst;
begin
  LinEst16(KnownY, KnownX, NData, LF, true);
  Result := LF.sigma;
end;

{------------------------------------------------------------------------}

function BetaCf(a, b, x : Single) : Single;
  {-Evaluates continued fraction for incomplete beta function}
const
  MAXIT = 100;
  EPS = 3.0E-7;
  FPMIN = 1.0E-30;
var
  m, m2 : Integer;
  aa, c, d, del, h, qab, qam, qap : Double;
begin
  qab := a+b;
  qap := a+1.0;
  qam := a-1.0;
  c := 1.0;
  d := 1.0-qab*x/qap;
  if (abs(d) < FPMIN) then
    d := FPMIN;
  d := 1.0/d;
  h := d;
  for m := 1 to MAXIT do begin
    m2 := 2*m;
    aa := m*(b-m)*x/((qam+m2)*(a+m2));
    d := 1.0+aa*d;
    if (abs(d) < FPMIN) then
      d := FPMIN;
    c := 1.0+aa/c;
    if (abs(c) < FPMIN) then
      c := FPMIN;
    d := 1.0/d;
    h := h*d*c;
    aa := -(a+m)*(qab+m)*x/((a+m2)*(qap+m2));

    d := 1.0+aa*d;
    if (abs(d) < FPMIN) then
      d := FPMIN;
    c := 1.0+aa/c;
    if (abs(c) < FPMIN) then
      c := FPMIN;
    d := 1.0/d;
    del := d*c;
    h := h*del;

    {check for convergence}
    if (abs(del-1.0) < EPS) then
      break;
    if m = MAXIT then
      RaiseStatError(stscStatNoConverge);
  end;
  Result := h;
end;

function BetaDist(X, Alpha, Beta, A, B : Single) : Single;
var
  bt : Double;
begin
  if (X < A) or (X > B) or (A = B) or (Alpha <= 0.0) or (Beta <= 0.0) then
    RaiseStatError(stscStatBadParam);

  {normalize X}
  X := (X-A)/(B-A);

  {compute factors in front of continued fraction expansion}
  if (X = 0.0) or (X = 1.0) then
    bt := 0.0
  else
    bt := exp(GammaLn(Alpha+Beta)-GammaLn(Alpha)-GammaLn(Beta)+
      Alpha*ln(X)+Beta*ln(1.0-X));

  {use symmetry relationship to make continued fraction converge quickly}
  if (X < (Alpha+1.0)/(Alpha+Beta+2.0)) then
    Result := bt*BetaCf(Alpha, Beta, X)/Alpha
  else
    Result := 1.0-bt*BetaCf(Beta, Alpha, 1.0-X)/Beta;
end;

function Sign(a, b : Double) : Double;
  {-Returns abs(a) if b >= 0.0, else -abs(a)}
begin
  if (b >= 0.0) then
    Result := abs(a)
  else
    Result := -abs(a);
end;

function BetaInv(Probability, Alpha, Beta, A, B : Single) : Single;
const
  MAXIT = 100;
  UNUSED = -1.11e30;
  XACC = 3e-7;
var
  j : Integer;
  ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew, dsign : Double;
begin
  if (Probability < 0.0) or (Probability > 1.0) or
  (A = B) or (Alpha <= 0.0) or (Beta <= 0.0) then
    RaiseStatError(stscStatBadParam);

  if (Probability = 0.0) then
    Result := A
  else if (Probability = 1.0) then
    Result := B
  else begin
    {Ridder's method of finding the root of BetaDist = Probability}
    fl := -Probability; {BetaDist(A, Alpha, Beta, A, B)-Probability}
    fh := 1.0-Probability; {BetaDist(B, Alpha, Beta, A, B)-Probability}
    xl := A;
    xh := B;
    ans := UNUSED;

    for j := 1 to MAXIT do begin
      xm := 0.5*(xl+xh);
      fm := BetaDist(xm, Alpha, Beta, A, B)-Probability;
      s := sqrt(fm*fm-fl*fh);
      if (s = 0.0) then begin
        Result := ans;
        exit;
      end;
      if (fl >= fh) then
        dsign := 1.0
      else
        dsign := -1.0;
      xnew := xm+(xm-xl)*(dsign*fm/s);
      if (abs(xnew-ans) <= XACC) then begin
        Result := ans;
        exit;
      end;
      ans := xnew;

      fnew := BetaDist(ans, Alpha, Beta, A, B)-Probability;
      if (fnew = 0.0) then begin
        Result := ans;
        exit;
      end;

      {keep root bracketed on next iteration}
      if (Sign(fm, fnew) <> fm) then begin
        xl := xm;
        fl := fm;
        xh := ans;
        fh := fnew;
      end else if (Sign(fl, fnew) <> fl) then begin
        xh := ans;
        fh := fnew;
      end else if (Sign(fh, fnew) <> fh) then begin
        xl := ans;
        fl := fnew;
      end else
        {shouldn't get here}
        RaiseStatError(stscStatNoConverge);

      if (abs(xh-xl) <= XACC) then begin
        Result := ans;
        exit;
      end;
    end;
    BetaInv := A; {avoid compiler warning}
    RaiseStatError(stscStatNoConverge);
  end;
end;

function BinomDistPmf(N, K : Integer; p : Extended) : Double;
  {-Returns the Probability mass function of the binomial distribution}
begin
  Result := Combinations(N, K)*IntPower(p, K)*IntPower(1.0-p, N-K);
end;

function BinomDist(NumberS, Trials : Integer; ProbabilityS : Single;
  Cumulative : Boolean) : Single;
begin
  if (Trials < 0) or (NumberS < 0) or (NumberS > Trials) or
  (ProbabilityS < 0.0) or (ProbabilityS > 1.0) then
    RaiseStatError(stscStatBadParam);

  if (Cumulative) then
    Result := 1.0+BinomDistPmf(Trials, NumberS, ProbabilityS)-
      BetaDist(ProbabilityS, NumberS, Trials-NumberS+1, 0.0, 1.0)
  else
    Result := BinomDistPmf(Trials, NumberS, ProbabilityS);
end;

function CritBinom(Trials : Integer; ProbabilityS, Alpha : Single) : Integer;
var
  s : Integer;
  B : Double;
begin
  if (Trials < 0) or (ProbabilityS < 0.0) or (ProbabilityS > 1.0) or
  (Alpha < 0.0) or (Alpha > 1.0) then
    RaiseStatError(stscStatBadParam);

  B := 0.0;
  for s := 0 to Trials do begin
    B := B+BinomDistPmf(Trials, s, ProbabilityS);
    if (B >= Alpha) then begin
      Result := s;
      exit;
    end;
  end;
  {in case of roundoff problems}
  Result := Trials;
end;

function GammSer(a, x : Single) : Single;
  {-Returns the series computation for GammP}
const
  MAXIT = 100;
  EPS = 3.0E-7;
var
  N : Integer;
  sum, del, ap : Double;
begin
  Result := 0.0;
  if (x > 0.0) then begin
    {x shouldn't be < 0.0, tested by caller}
    ap := a;
    sum := 1.0/a;
    del := sum;
    for N := 1 to MAXIT do begin
      ap := ap+1;
      del := del*x/ap;
      sum := sum+del;
      if (abs(del) < abs(sum)*EPS) then begin
        Result := sum*exp(-X+a*ln(X)-GammaLn(a));
        exit;
      end;
    end;
    RaiseStatError(stscStatNoConverge);
  end;
end;

function GammCf(a, x : Single) : Single;
  {-Returns the continued fraction computation for GammP}
const
  MAXIT = 100;
  EPS = 3.0e-7;
  FPMIN = 1.0e-30;
var
  i : Integer;
  an, b, c, d, del, h : Double;
begin
  b := x+1.0-a;
  c := 1.0/FPMIN;
  d := 1.0/b;
  h := d;
  for i := 1 to MAXIT do begin
    an := -i*(i-a);
    b := b+2.0;
    d := an*d+b;
    if (abs(d) < FPMIN) then
      d := FPMIN;
    c := b+an/c;
    if (abs(c) < FPMIN) then
      c := FPMIN;
    d := 1.0/d;
    del := d*c;
    h := h*del;
    if (abs(del-1.0) < EPS) then
      break;
    if i = MAXIT then
      RaiseStatError(stscStatNoConverge);
  end;
  Result := h*exp(-x+a*ln(x)-GammaLn(a));
end;

function GammP(a, x : Single) : Single;
  {-Returns the incomplete gamma function P(a, x)}
begin
  if (x < 0.0) or (a <= 0.0) then
    RaiseStatError(stscStatBadParam);
  if (x < a+1.0) then
    {use the series representation}
    Result := GammSer(a, x)
  else
    {use the continued fraction representation}
    Result := 1.0-GammCf(a, x);
end;

function ChiDist(X : Single; DegreesFreedom : Integer) : Single;
begin
  if (DegreesFreedom < 1) or (X < 0.0) then
    RaiseStatError(stscStatBadParam);
  Result := 1.0-GammP(DegreesFreedom/2.0, X/2.0);
end;

function ChiInv(Probability : Single; DegreesFreedom : Integer) : Single;
const
  MAXIT = 100;
  UNUSED = -1.11e30;
  XACC = 3e-7;
  FACTOR = 1.6;
var
  j : Integer;
  ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew, dsign : Double;
begin
  if (Probability < 0.0) or (Probability > 1.0) or (DegreesFreedom < 1) then
    RaiseStatError(stscStatBadParam);

  if (Probability = 0.0) then
    Result := 0.0
  else begin
    {bracket the interval}
    xl := 0.0;
    xh := 100.0;
    fl := ChiDist(xl, DegreesFreedom)-Probability;
    fh := ChiDist(xh, DegreesFreedom)-Probability;
    for j := 1 to MAXIT do begin
      if (fl*fh < 0.0) then
        {bracketed the root}
        break;
      if (abs(fl) < abs(fh)) then begin
        xl := xl+FACTOR*(xl-xh);
        fl := ChiDist(xl, DegreesFreedom)-Probability;
      end else begin
        xh := xh+FACTOR*(xh-xl);
        fh := ChiDist(xh, DegreesFreedom)-Probability;
      end;
    end;
    if (fl*fh >= 0.0) then
      {couldn't bracket it, means Probability too close to 1.0}
      RaiseStatError(stscStatNoConverge);

    {Ridder's method of finding the root of ChiDist = Probability}
    ans := UNUSED;

    for j := 1 to MAXIT do begin
      xm := 0.5*(xl+xh);
      fm := ChiDist(xm, DegreesFreedom)-Probability;
      s := sqrt(fm*fm-fl*fh);
      if (s = 0.0) then begin
        Result := ans;
        exit;
      end;
      if (fl >= fh) then
        dsign := 1.0
      else
        dsign := -1.0;
      xnew := xm+(xm-xl)*(dsign*fm/s);
      if (abs(xnew-ans) <= XACC) then begin
        Result := ans;
        exit;
      end;
      ans := xnew;

      fnew := ChiDist(ans, DegreesFreedom)-Probability;
      if (fnew = 0.0) then begin
        Result := ans;
        exit;
      end;

      {keep root bracketed on next iteration}
      if (Sign(fm, fnew) <> fm) then begin
        xl := xm;
        fl := fm;
        xh := ans;
        fh := fnew;
      end else if (Sign(fl, fnew) <> fl) then begin
        xh := ans;
        fh := fnew;
      end else if (Sign(fh, fnew) <> fh) then begin
        xl := ans;
        fl := fnew;
      end else
        {shouldn't get here}
        RaiseStatError(stscStatNoConverge);

      if (abs(xh-xl) <= XACC) then begin
        Result := ans;
        exit;
      end;
    end;
    Result := 0.0; {avoid compiler warning}
    RaiseStatError(stscStatNoConverge);
  end;
end;

function ExponDist(X, Lambda : Single; Cumulative : Boolean) : Single;
begin
  if (X < 0.0) or (Lambda <= 0.0) then
    RaiseStatError(stscStatBadParam);

  if (Cumulative) then
    Result := 1.0-exp(-Lambda*X)
  else
    Result := Lambda*exp(-Lambda*X);
end;

function FDist(X : Single;
  DegreesFreedom1, DegreesFreedom2 : Integer) : Single;
begin
  if (X < 0) or (DegreesFreedom1 < 1) or (DegreesFreedom2 < 1) then
    RaiseStatError(stscStatBadParam);

  Result := BetaDist(DegreesFreedom2/(DegreesFreedom2+DegreesFreedom1*X),
    DegreesFreedom2/2.0, DegreesFreedom1/2.0, 0.0, 1.0);
end;

function FInv(Probability : Single;
  DegreesFreedom1, DegreesFreedom2 : Integer) : Single;
const
  MAXIT = 100;
  UNUSED = -1.11e30;
  XACC = 3e-7;
  FACTOR = 1.6;
var
  j : Integer;
  ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew, dsign : Double;
begin
  if (Probability < 0.0) or (Probability > 1.0) or
  (DegreesFreedom1 < 1) or (DegreesFreedom2 < 1) then
    RaiseStatError(stscStatBadParam);

  if (Probability = 1.0) then
    Result := 0.0
  else begin
    {bracket the interval}
    xl := 0.0;
    xh := 100.0;
    fl := FDist(xl, DegreesFreedom1, DegreesFreedom2)-Probability;
    fh := FDist(xh, DegreesFreedom1, DegreesFreedom2)-Probability;
    for j := 1 to MAXIT do begin
      if (fl*fh < 0.0) then
        {bracketed the root}
        break;
      if (abs(fl) < abs(fh)) then begin
        xl := xl+FACTOR*(xl-xh);
        fl := FDist(xl, DegreesFreedom1, DegreesFreedom2)-Probability;
      end else begin
        xh := xh+FACTOR*(xh-xl);
        fh := FDist(xh, DegreesFreedom1, DegreesFreedom2)-Probability;
      end;
    end;
    if (fl*fh >= 0.0) then
      {couldn't bracket it, means Probability too close to 0.0}
      RaiseStatError(stscStatNoConverge);

    {Ridder's method of finding the root of FDist = Probability}
    ans := UNUSED;

    for j := 1 to MAXIT do begin
      xm := 0.5*(xl+xh);
      fm := FDist(xm, DegreesFreedom1, DegreesFreedom2)-Probability;
      s := sqrt(fm*fm-fl*fh);
      if (s = 0.0) then begin
        Result := ans;
        exit;
      end;
      if (fl >= fh) then
        dsign := 1.0
      else
        dsign := -1.0;
      xnew := xm+(xm-xl)*(dsign*fm/s);
      if (abs(xnew-ans) <= XACC) then begin
        Result := ans;
        exit;
      end;
      ans := xnew;

      fnew := FDist(ans, DegreesFreedom1, DegreesFreedom2)-Probability;
      if (fnew = 0.0) then begin
        Result := ans;
        exit;
      end;

      {keep root bracketed on next iteration}
      if (Sign(fm, fnew) <> fm) then begin
        xl := xm;
        fl := fm;
        xh := ans;
        fh := fnew;
      end else if (Sign(fl, fnew) <> fl) then begin
        xh := ans;
        fh := fnew;
      end else if (Sign(fh, fnew) <> fh) then begin
        xl := ans;
        fl := fnew;
      end else
        {shouldn't get here}
        RaiseStatError(stscStatNoConverge);

      if (abs(xh-xl) <= XACC) then begin
        Result := ans;
        exit;
      end;
    end;
    Result := 0.0; {avoid compiler warning}
    RaiseStatError(stscStatNoConverge);
  end;
end;

function LogNormDist(X, Mean, StandardDev : Single) : Single;
begin
  if (X <= 0.0) or (StandardDev <= 0) then
    RaiseStatError(stscStatBadParam);
  Result := NormSDist((ln(X)-Mean)/StandardDev);
end;

function LogInv(Probability,  Mean, StandardDev : Single) : Single;
begin
  if (Probability < 0.0) or (Probability > 1.0) or (StandardDev <= 0) then
    RaiseStatError(stscStatBadParam);
  Result := exp(Mean+StandardDev*NormSInv(Probability));
end;

function NormDist(X, Mean, StandardDev : Single;
  Cumulative : Boolean) : Single;
var
  Z : Extended;
begin
  if (StandardDev <= 0) then
    RaiseStatError(stscStatBadParam);
  Z := (X-Mean)/StandardDev;
  if (Cumulative) then
    Result := NormSDist(Z)
  else
    Result := exp(-Z*Z/2.0)/(StandardDev*sqrt2pi);
end;

function NormInv(Probability, Mean, StandardDev : Single) : Single;
begin
  if (Probability < 0.0) or (Probability > 1.0) or (StandardDev <= 0) then
    RaiseStatError(stscStatBadParam);
  Result := Mean+StandardDev*NormSInv(Probability);
end;

function Erfc(X : Single) : Single;
var
  t, z, ans : Double;
begin
  z := abs(X);
  t := 1.0/(1.0+0.5*z);
  ans := t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
    t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
    t*(-0.82215223+t*0.17087277)))))))));
  if (X >= 0.0) then
    Result := ans
  else
    Result := 2.0-ans;
end;

function NormSDist(Z : Single) : Single;
const
  sqrt2 = 1.41421356237310;
begin
  Result := 1.0-0.5*Erfc(Z/sqrt2);
end;

function NormSInv(Probability : Single) : Single;
const
  MAXIT = 100;
  UNUSED = -1.11e30;
  XACC = 3e-7;
  FACTOR = 1.6;
var
  j : Integer;
  ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew, dsign : Double;
begin
  if (Probability < 0.0) or (Probability > 1.0) then
    RaiseStatError(stscStatBadParam);
  Result := 0.0;

  {bracket the interval}
  xl := -2.0;
  xh := +2.0;
  fl := NormSDist(xl)-Probability;
  fh := NormSDist(xh)-Probability;
  for j := 1 to MAXIT do begin
    if (fl*fh < 0.0) then
      {bracketed the root}
      break;
    if (abs(fl) < abs(fh)) then begin
      xl := xl+FACTOR*(xl-xh);
      fl := NormSDist(xl)-Probability;
    end else begin
      xh := xh+FACTOR*(xh-xl);
      fh := NormSDist(xh)-Probability;
    end;
  end;
  if (fl*fh >= 0.0) then
    {couldn't bracket it, means Probability too close to 0.0 or 1.0}
    RaiseStatError(stscStatNoConverge);

  {Ridder's method of finding the root of NormSDist = Probability}
  ans := UNUSED;

  for j := 1 to MAXIT do begin
    xm := 0.5*(xl+xh);
    fm := NormSDist(xm)-Probability;
    s := sqrt(fm*fm-fl*fh);
    if (s = 0.0) then begin
      Result := ans;
      exit;
    end;
    if (fl >= fh) then
      dsign := 1.0
    else
      dsign := -1.0;
    xnew := xm+(xm-xl)*(dsign*fm/s);
    if (abs(xnew-ans) <= XACC) then begin
      Result := ans;
      exit;
    end;
    ans := xnew;

    fnew := NormSDist(ans)-Probability;
    if (fnew = 0.0) then begin
      Result := ans;
      exit;
    end;

    {keep root bracketed on next iteration}
    if (Sign(fm, fnew) <> fm) then begin
      xl := xm;
      fl := fm;
      xh := ans;
      fh := fnew;
    end else if (Sign(fl, fnew) <> fl) then begin
      xh := ans;
      fh := fnew;
    end else if (Sign(fh, fnew) <> fh) then begin
      xl := ans;
      fl := fnew;
    end else
      {shouldn't get here}
      RaiseStatError(stscStatNoConverge);

    if (abs(xh-xl) <= XACC) then begin
      Result := ans;
      exit;
    end;
  end;
  RaiseStatError(stscStatNoConverge);
end;

function Poisson(X : Integer; Mean : Single; Cumulative : Boolean) : Single;
begin
  if (X < 0) or (Mean <= 0.0) then
    RaiseStatError(stscStatBadParam);
  if (Cumulative) then
    Result := 1.0-GammP(X+1.0, Mean)
  else
    Result := IntPower(Mean, X)*exp(-Mean)/Factorial(X);
end;

function TDist(X : Single; DegreesFreedom : Integer;
  TwoTails : Boolean) : Single;
var
  a : Double;
begin
  if (DegreesFreedom < 1) then
    RaiseStatError(stscStatBadParam);

  a := BetaDist(DegreesFreedom/(DegreesFreedom+X*X), DegreesFreedom/2.0,
    0.5, 0.0, 1.0);
  if TwoTails then
    Result := a
  else
    Result := 0.5*a;
end;

function TInv(Probability : Single; DegreesFreedom : Integer) : Single;
const
  MAXIT = 100;
  UNUSED = -1.11e30;
  XACC = 3e-7;
  FACTOR = 1.6;
var
  j : Integer;
  ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew, dsign : Double;
begin
  if (Probability < 0.0) or (Probability > 1.0) or (DegreesFreedom < 1) then
    RaiseStatError(stscStatBadParam);

  Result := 0.0;
  if (Probability = 1.0) then
    exit;

  {bracket the interval}
  xl := 0.0;
  xh := +2.0;
  fl := TDist(xl, DegreesFreedom, true)-Probability;
  fh := TDist(xh, DegreesFreedom, true)-Probability;
  for j := 1 to MAXIT do begin
    if (fl*fh < 0.0) then
      {bracketed the root}
      break;
    if (abs(fl) < abs(fh)) then begin
      xl := xl+FACTOR*(xl-xh);
      fl := TDist(xl, DegreesFreedom, true)-Probability;
    end else begin
      xh := xh+FACTOR*(xh-xl);
      fh := TDist(xh, DegreesFreedom, true)-Probability;
    end;
  end;
  if (fl*fh >= 0.0) then
    {couldn't bracket it, means Probability too close to 1.0}
    RaiseStatError(stscStatNoConverge);

  {Ridder's method of finding the root of TDist = Probability}
  ans := UNUSED;

  for j := 1 to MAXIT do begin
    xm := 0.5*(xl+xh);
    fm := TDist(xm, DegreesFreedom, true)-Probability;
    s := sqrt(fm*fm-fl*fh);
    if (s = 0.0) then begin
      Result := ans;
      exit;
    end;
    if (fl >= fh) then
      dsign := 1.0
    else
      dsign := -1.0;
    xnew := xm+(xm-xl)*(dsign*fm/s);
    if (abs(xnew-ans) <= XACC) then begin
      Result := ans;
      exit;
    end;
    ans := xnew;

    fnew := TDist(ans, DegreesFreedom, true)-Probability;
    if (fnew = 0.0) then begin
      Result := ans;
      exit;
    end;

    {keep root bracketed on next iteration}
    if (Sign(fm, fnew) <> fm) then begin
      xl := xm;
      fl := fm;
      xh := ans;
      fh := fnew;
    end else if (Sign(fl, fnew) <> fl) then begin
      xh := ans;
      fh := fnew;
    end else if (Sign(fh, fnew) <> fh) then begin
      xl := ans;
      fl := fnew;
    end else
      {shouldn't get here}
      RaiseStatError(stscStatNoConverge);

    if (abs(xh-xl) <= XACC) then begin
      Result := ans;
      exit;
    end;
  end;
  RaiseStatError(stscStatNoConverge);
end;

procedure Initialize;
  {-Fully initializes factorial lookup tables}
var
  i : Integer;
begin
  FactA[2] := 2.0;
  for i := 3 to MFactA do
    FactA[i] := i*FactA[i-1];
  for i := 2 to MFactLna do
    FactLnA[i] := GammaLn(i+1.0);
end;

initialization
  Initialize;
end.
