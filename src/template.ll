target triple = "x86_64-pc-linux-gnu" ; Set target architecture to 64-bit Linux

; From the C standard library
declare i32 @printf(i8*, ...)                ; Declare printf
declare double @fmod(double, double)         ; Declare fmod
declare double @llvm.floor.f64(double)       ; Declare floor
declare double @llvm.pow.f64(double, double) ; Declare pow

; Strings for printing values
@-nil = constant [5 x i8] c"nil\0A\00"
@-true = constant [5 x i8] c"true\00"
@-false = constant [6 x i8] c"false\00"

; Format strings for printing
@-formatDouble = constant [4 x i8] c"%f\0A\00"
@-formatString = constant [4 x i8] c"%s\0A\00"

; Function for floor division
define double @-ffdiv(double %t0, double %t1) {
    %d = fdiv double %t0, %t1 ; Perform float division
    %r = call double @llvm.floor.f64(double %d) ; Floor the result
    ret double %r ; Return the result
}

; Function for printing a boolean
define void @-printBool(i1 %bool) {
    %r = alloca i8* ; String to print
    %c = icmp eq i1 %bool, 0 ; Get if true or false
    br i1 %c, label %1, label %2
1: ; If false
    %f = getelementptr [6 x i8], [6 x i8]* @-false, i32 0, i32 0 ; Get the false string
    store i8* %f, i8** %r ; Store the false string in the string to print
    br label %3
2: ; If true
    %t = getelementptr [6 x i8], [6 x i8]* @-true, i32 0, i32 0 ; Get the true string
    store i8* %t, i8** %r ; Store the true string in the string to print
    br label %3
3:
    %s = load i8*, i8** %r ; Load the string to print
    call i32 (i8*, ...) @printf(i8* @-formatString, i8* %s) ; Print the string
    ret void
}

; ======= Variables ========
; ==========================

; = Functions and Strings ==
; ==========================

; Function to run the compiled code
define void @-run() {
; ========== Code ==========
; ==========================
ret void
}

; Main function - entry point for the program
define i32 @main() {
    call void @-run() ; Run the compiled code

    ; Return 0 for successful execution
    ret i32 0
}