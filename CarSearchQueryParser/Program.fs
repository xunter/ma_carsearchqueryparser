// Learn more about F# at http://fsharp.net
open System
open System.Text

let reservedTokens = ["город"; "марка"; "модель"; "год"; "цена"]

type Car = { mutable Brand : string; mutable Model : string; mutable Year : int; mutable Amount : float; mutable City : string }

let carsStorage = [
    { Brand = "BMW"; Model = "330"; Year = 2005; Amount = 900000.0; City = "Москва" };
    { Brand = "ВАЗ"; Model = "2112"; Year = 2003; Amount = 200000.0; City = "Рязань" };
    { Brand = "Chevrolet"; Model = "Camaro"; Year = 2007; Amount = 1200000.0; City = "Los Angeles" }
]

type YearCriteria = { mutable YearFrom : int; mutable YearTo : int; mutable FixedYear : int }
type AmountCriteria = { mutable AmountFrom : float; mutable AmountTo : float; mutable AmountFixed : float }
type SyntaxAnalisysTree = { mutable City : string; mutable Brand : string; mutable Model : string; mutable Year : YearCriteria; mutable Amount : AmountCriteria }


    
let isTokenReserved (token : string) = 
    reservedTokens |> List.tryFind (fun each -> each = token.ToLower()) <> option.None

 
let readNumber (token : string) : int =
    let mutable num : int = -1
    let success = Int32.TryParse(token, ref num)
    num

let readFloat (token : string) : float =
    let mutable num : float = 0.0
    let success = Double.TryParse(token, ref num)
    num

let readStringValue (startIndex : int) (tokens : string[]) = 
    let sb = new StringBuilder()
    let space = " ";
    let rec iterTokens = fun (i : int) (each : string) -> 
        let tokenReserved = isTokenReserved each
        if (tokenReserved = false) then
            let tempsb = sb.Append(space).Append(each)
            if (i + 1 < tokens.Length) then
                let nextIndex = i + 1
                let nextToken = Array.get tokens nextIndex
                iterTokens nextIndex nextToken
            
    let startToken = Array.get tokens startIndex
    iterTokens startIndex startToken
    let startIndexToRemove = (sb.Length - 1);
    let mutable str = sb.ToString();
    if (str.EndsWith(" ")) then
        str <- str.Remove(str.Length - 1)
    if (str.StartsWith(" ")) then
        str <- str.Remove(0, 1)
    str
    
    
let processBrand (i : int) (tokens : string[]) (tree : SyntaxAnalisysTree) =
    let brandValue = readStringValue i tokens;
    if (String.IsNullOrEmpty(brandValue)) then
        false
    else
        tree.Brand <- brandValue
        true

let processCity (i : int) (tokens : string[]) (tree : SyntaxAnalisysTree) =
    let cityValue = readStringValue i tokens;
    if (String.IsNullOrEmpty(cityValue)) then
        false
    else
        tree.City <- cityValue
        true
        
let processModel (i : int) (tokens : string[]) (tree : SyntaxAnalisysTree) =
    let modelValue = readStringValue i tokens;
    if (String.IsNullOrEmpty(modelValue)) then
        false
    else
        tree.Model <- modelValue
        true


let processYearCriteria (i : int) (tokens : string[]) (tree : SyntaxAnalisysTree) = 
    let firstToken : string = Array.get tokens i;
    if (firstToken.ToLower() = "от") then
        if (Array.length tokens - 1 >= i + 3) then
            let fromToken = Array.get tokens (i + 1)
            let toToken = Array.get tokens (i + 3)
            let fromYear = readNumber fromToken
            let toYear = readNumber toToken
            let yearCriteria = { FixedYear = 0; YearFrom = fromYear; YearTo = toYear }
            if (fromYear > 0 && toYear > 0) then
                tree.Year <- yearCriteria
                true
            else
                false
        else
            false
    else
        let year = readNumber firstToken;
        if (year > 0) then
            tree.Year <- { FixedYear = year; YearFrom = 0; YearTo = 0 }
            true
        else
            false
            
            
let processAmountCriteria (i : int) (tokens : string[]) (tree : SyntaxAnalisysTree) = 
    let firstToken : string = Array.get tokens i;
    if (firstToken.ToLower() = "от") then
        if (Array.length tokens - 1 >= i + 3) then
            let fromToken = Array.get tokens (i + 1)
            let toToken = Array.get tokens (i + 3)
            let fromAmount = readFloat fromToken
            let toAmount = readFloat toToken
            let amountCriteria = { AmountFixed = 0.0; AmountFrom = fromAmount; AmountTo = toAmount }
            if (fromAmount > 0.0 && toAmount > 0.0) then
                tree.Amount <- amountCriteria
                true
            else
                false
        else
            false
    else
        let amount = readFloat firstToken;
        if (amount > 0.0) then
            tree.Amount <- { AmountFixed = amount; AmountFrom = 0.0; AmountTo = 0.0 }
            true
        else
            false

let processToken (i : int) (token : string) (tokens : string[]) (tree : SyntaxAnalisysTree) =
    let mutable processResult = true
    if (isTokenReserved token) then
        let nextIndex = i + 1
        match token.ToLower() with
            | "город" -> processResult <- processCity nextIndex tokens tree
            | "марка" -> processResult <- processBrand nextIndex tokens tree
            | "модель" -> processResult <-  processModel nextIndex tokens tree
            | "год" -> processResult <- processYearCriteria nextIndex tokens tree
            | "цена" -> processResult <- processAmountCriteria nextIndex tokens tree
            | _ -> processResult <- true
    else
        processResult <- true
    processResult

let parseQuery (tree : SyntaxAnalisysTree) (query : string) : bool =
    let tokens = query.Trim().Split([|' '|]);
    let rec iteriOverToken = fun (i : int) (token : string) ->
        let success = processToken i token tokens tree
        if (success) then
            let nextIndex = i + 1
            if (nextIndex <= tokens.Length - 1) then
                let nextToken = Array.get tokens nextIndex
                let iterResult = iteriOverToken nextIndex nextToken
                iterResult
            else
                true
        else
            false
            
    
    let firstToken = Array.get tokens 0
    let success = iteriOverToken 0 firstToken;

    success

let yearCriteriaToString yearCriteria = 
    if (yearCriteria.FixedYear > 0) then
        yearCriteria.FixedYear.ToString()
    else
        String.Format("от {0} до {1}", yearCriteria.YearFrom, yearCriteria.YearTo)
        
let amountCriteriaToString amountCriteria = 
    if (amountCriteria.AmountFixed > 0.0) then
        amountCriteria.AmountFixed.ToString()
    else
        String.Format("от {0} до {1}", amountCriteria.AmountFrom, amountCriteria.AmountTo)

let treeToString tree = 
    let yearCriteriaString = yearCriteriaToString tree.Year
    let amountCriteriaString = amountCriteriaToString tree.Amount
    String.Format("город {4} марка {0} модель {1} год {2} цена {3}", tree.Brand, tree.Model, yearCriteriaString, amountCriteriaString, tree.City)


let findCars tree = 
    let foundCars = carsStorage |> List.filter (fun (car : Car) ->
        let mutable matches = true

        if (String.IsNullOrEmpty(tree.Brand) = false) then
            matches <- matches && (String.Equals(tree.Brand.ToLower(), car.Brand.ToLower()))

        if (String.IsNullOrEmpty(tree.Model) = false) then
            matches <- matches && (tree.Model.ToLower() = car.Model.ToLower())

        if (String.IsNullOrEmpty(tree.City) = false) then
            matches <- matches && (tree.City.ToLower() = car.City.ToLower())
            
        if (tree.Year.FixedYear > 0) then
            matches <- matches && tree.Year.FixedYear = car.Year
        elif (tree.Year.YearFrom > 0 && tree.Year.YearTo > 0) then
            matches <- matches && car.Year >= tree.Year.YearFrom && car.Year <= tree.Year.YearTo

        if (tree.Amount.AmountFixed > 0.0) then
            matches <- matches && tree.Amount.AmountFixed = car.Amount
        elif (tree.Amount.AmountFrom > 0.0 && tree.Amount.AmountTo > 0.0) then
            matches <- matches && car.Amount >= tree.Amount.AmountFrom && car.Amount <= tree.Amount.AmountTo
        matches)
    foundCars

let carToString (car : Car) =
    String.Format("город {0} марка {1} модель {2} год {3} цена {4}", car.City, car.Brand, car.Model, car.Year, car.Amount)

[<EntryPoint>]
let main args = 
    let query = "город Рязань марка ВАЗ модель 2112";
    //let query = Array.get args 0;
    Console.WriteLine("Входное предложение: {0}", query);
    let mutable tree = { City = String.Empty; Brand = String.Empty; Model = String.Empty; Year = { YearFrom = 0; YearTo = 0; FixedYear = 0 } ; Amount = { AmountFixed = 0.0; AmountFrom = 0.0; AmountTo = 0.0 } } : SyntaxAnalisysTree
    let processResult = parseQuery tree query
    if (processResult = false) then
        Console.WriteLine "Входное предложение не соответствует заданной грамматике!"
        let readKey = Console.ReadKey()
        1
    else
        let treeString = treeToString tree
        Console.WriteLine("Разобранный запрос: {0}", treeString);
        let foundCars = findCars tree
        Console.WriteLine("Число совпадающих авто: {0}", foundCars.Length)
        foundCars |> List.iteri (fun (i : int) (car : Car) -> 
            Console.WriteLine("{0}: {1}", i, carToString car))
        let readKey = Console.ReadKey()
        0