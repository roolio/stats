#light
namespace Roolio
module Stats = 

    open System
    // open System.Collections.Generic
    
    //////////////////////////////////////////
    ///SOME STATS :REALLY BASIC,REALLY INCOMPLETE, REALLY NOT FULLY TESTED
    //////////////////////////////////////////
    //Adding this to Array type
    
    type System.Array with 
        static member sum x = 
            Array.fold (+) 0.0 x
        static member mean x = 
            Array.sum x / (float x.Length)
        static member cumsum (x:float array) =  
            let y = Array.create (x.Length) 0.
            y.[0] <- x.[0]
            for i in 1 .. (x.Length - 1) do 
                y.[i] <- y.[i-1] + x.[i]
            done
            y
        static member stdev (x : float array) =
            let m = Array.mean x 
            let x2 = Array.map (fun t -> (t-m)**2.) x |> Array.sum
            sqrt(x2 / float(Array.length x - 1))
        static member max_array (a : float array) = 
            Array.fold max a.[0]  a
        static member min_array (a : float array) = 
            Array.fold min a.[0] a
    end
    
    //open Array    
    
    ///Lower Partial Moment, 2nd order
    let lpm_2 (x: float array) (target : float) = 
        let x_minus = Array.filter (fun t -> t < target) x
        let res = 
            if x_minus.Length = 0 
                then 1e-12
                else Array.map (fun t -> (t - target)**2.) x_minus |> Array.sum |> ( * ) (1. / float x.Length)
        res
    
    ///Semi-Variance 
    let semi_variance (x : float array) = 
        lpm_2 x (Array.mean x)
    
    
    ///Semi-Standard Dev
    let semi_stdev (x : float array) = 
        sqrt (semi_variance x)
    
    let  pow (x : float array) b =
        Array.map (fun t -> t**b) x 
    
    ///Skewness
    let skewness (x : float array) = 
        let m = Array.mean x in 
        let z = Array.map (fun t-> t-m) x
        let s2 = Array.mean(pow z 2.) in 
        let m3 = Array.mean(pow z 3.) in 
        m3 / (s2 ** 1.5)
    
    ///unbiased Skewness
    let skewness_unbiased (x: float array) = 
        let s = skewness x
        let n = float x.Length
        s * sqrt((n - 1.) / n) * n  / (n - 2.)
        
    
    ///Kurtosis (un-normaized , so worth 3 for a gaussian)
    let kurtosis (x : float array) = 
            let m = Array.mean x
            let z = Array.map (fun t-> t-m) x
            let s2 = Array.mean(pow z 2.) in 
            let m4 = Array.mean(pow z 4.) in 
            m4 / (s2 ** 2.)
    
    ///Kurtosis (normalized ,ie 0 for a gaussian)
    let kurtosis_normed (x : float array) = 
        (kurtosis x) - 3. 
    
    ///Kurtosis unbiased
    let kurtosis_unbiased (x:float array) = 
        let k = kurtosis x
        let n = float x.Length
        ((n + 1. ) * k - 3. * (n - 1.)) * (n - 1. )/((n - 2.) * (n - 3.)) 
