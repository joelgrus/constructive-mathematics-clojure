# ConstructiveMathematics

Blog post <a href = "http://joelgrus.com/2013/08/26/constructive-mathematics-in-f-and-clojure/">here</a>

Mostly for my own education, I decided to port my [Constructive Mathematics in F#](https://github.com/joelgrus/constructive-mathematics-fsharp) project to Clojure.  (The debugging of which reminded me how much I liked static typing.)

As the F# project is still a work in progress, this is *very much* a work in progress.  In particular, as I have much less Clojure experience than I do F# experience, I am a lot less sure about some of the design decisions.  Here's how they look:

## Natural Numbers

In F#, I defined these as 

```F#
type Natural = One | SuccessorOf of Natural
```

So that you had 

```F#
let Two = SuccessorOf One
```

and so on.

In Clojure, after trying a lot of things, it seemed to make the most sense to do 

```
(defn successor-of [n] {:predecessor n})
(defn predecessor-of [n] (:predecessor n))

(def one (successor-of nil))
(def two (successor-of one))
```

And then proceed accordingly.

## Integers

In F# these were 

```
type Integer =
| Positive of Natural.Natural
| Zero
| Negative of Natural.Natural
```

In Clojure I decided on the similar

```
(def zero {:sign :zero})
(defn positive [n] {:sign :positive, :n n})
(defn negative [n] {:sign :negative, :n n})
```

## Rationals

In F# these were a class

```
type Rational(numerator : Integer.Integer, denominator : Integer.Integer) =
    let gcd = 
        if Integer.EqualTo Integer.Zero denominator then failwithf "Cannot have a Zero denominator"
        else Integer.GCD numerator denominator
        
    // want denominator to be positive always
    let reSign =
        match denominator with
        | Integer.Negative _ -> Integer.Negate
        | _ -> id

    // divide by GCD to get to relatively prime
    let _numerator = (Integer.Divide (reSign numerator) gcd)
    let _denominator = (Integer.Divide (reSign denominator) gcd)

    member this.numerator with get () = _numerator
    member this.denominator with get () = _denominator
```
	
In Clojure I used a similar function

```
(defn rational [numerator denominator]
	  (let [gcd (if (integers/equal-to integers/zero denominator)
	              (throw (Exception. "cannot have a zero denominator!"))
	              (integers/gcd numerator denominator))
	        re-sign (if (integers/less-than denominator integers/zero)
	                  integers/negate
	                  (fn [i] i))]
	    {:numerator (integers/divide (re-sign numerator) gcd),
	     :denominator (integers/divide (re-sign denominator) gcd)}))
```

## License

No license.