; ModuleID = 'json_obj.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.token = type { i32, %struct.token*, %struct.token*, i8*, i32, %struct.token*, i8*, i32, float, i32 }

@.str = private unnamed_addr constant [4 x i8] c"Bob\00", align 1
@.str.1 = private unnamed_addr constant [10 x i8] c"        {\00", align 1
@.str.2 = private unnamed_addr constant [6 x i8] c"%s : \00", align 1
@.str.3 = private unnamed_addr constant [6 x i8] c"%s,/n\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"%d,\0A\00", align 1
@.str.5 = private unnamed_addr constant [5 x i8] c"%f,\0A\00", align 1
@.str.6 = private unnamed_addr constant [5 x i8] c"Name\00", align 1
@.str.7 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.8 = private unnamed_addr constant [4 x i8] c"Age\00", align 1
@.str.9 = private unnamed_addr constant [15 x i8] c"Favorite Color\00", align 1
@.str.10 = private unnamed_addr constant [5 x i8] c"Blue\00", align 1
@.str.11 = private unnamed_addr constant [40 x i8] c"This is token 1 key from 3 in main: %s\0A\00", align 1
@.str.12 = private unnamed_addr constant [25 x i8] c"This is token 1 key: %s\0A\00", align 1
@.str.13 = private unnamed_addr constant [40 x i8] c"This is token 3 key from 1 in main: %s\0A\00", align 1
@.str.14 = private unnamed_addr constant [25 x i8] c"This is token 3 key: %s\0A\00", align 1
@.str.15 = private unnamed_addr constant [13 x i8] c"Contact Info\00", align 1
@.str.16 = private unnamed_addr constant [8 x i8] c"Address\00", align 1
@.str.17 = private unnamed_addr constant [15 x i8] c"22 Jump Street\00", align 1
@.str.18 = private unnamed_addr constant [6 x i8] c"Phone\00", align 1

; Function Attrs: nounwind uwtable
define i32 @add_sibling(%struct.token* %parent_token, %struct.token* %sibling_token) #0 {
  %1 = alloca %struct.token*, align 8
  %2 = alloca %struct.token*, align 8
  store %struct.token* %parent_token, %struct.token** %1, align 8
  store %struct.token* %sibling_token, %struct.token** %2, align 8
  %3 = load %struct.token*, %struct.token** %2, align 8
  %4 = load %struct.token*, %struct.token** %1, align 8
  %5 = getelementptr inbounds %struct.token, %struct.token* %4, i32 0, i32 2
  store %struct.token* %3, %struct.token** %5, align 8
  %6 = load %struct.token*, %struct.token** %1, align 8
  %7 = load %struct.token*, %struct.token** %2, align 8
  %8 = getelementptr inbounds %struct.token, %struct.token* %7, i32 0, i32 1
  store %struct.token* %6, %struct.token** %8, align 8
  %9 = load %struct.token*, %struct.token** %1, align 8
  %10 = getelementptr inbounds %struct.token, %struct.token* %9, i32 0, i32 3
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i8** %10, align 8
  ret i32 0
}

; Function Attrs: nounwind uwtable
define i32 @add_child(%struct.token* %parent_token, %struct.token* %child_token) #0 {
  %1 = alloca %struct.token*, align 8
  %2 = alloca %struct.token*, align 8
  store %struct.token* %parent_token, %struct.token** %1, align 8
  store %struct.token* %child_token, %struct.token** %2, align 8
  %3 = load %struct.token*, %struct.token** %2, align 8
  %4 = load %struct.token*, %struct.token** %1, align 8
  %5 = getelementptr inbounds %struct.token, %struct.token* %4, i32 0, i32 5
  store %struct.token* %3, %struct.token** %5, align 8
  ret i32 0
}

; Function Attrs: nounwind uwtable
define i32 @print_obj(%struct.token* %obj_start) #0 {
  %1 = alloca %struct.token*, align 8
  %curr = alloca %struct.token*, align 8
  store %struct.token* %obj_start, %struct.token** %1, align 8
  %2 = load %struct.token*, %struct.token** %1, align 8
  store %struct.token* %2, %struct.token** %curr, align 8
  br label %3

; <label>:3                                       ; preds = %86, %0
  %4 = load %struct.token*, %struct.token** %curr, align 8
  %5 = getelementptr inbounds %struct.token, %struct.token* %4, i32 0, i32 2
  %6 = load %struct.token*, %struct.token** %5, align 8
  %7 = icmp ne %struct.token* %6, null
  br i1 %7, label %13, label %8

; <label>:8                                       ; preds = %3
  %9 = load %struct.token*, %struct.token** %curr, align 8
  %10 = getelementptr inbounds %struct.token, %struct.token* %9, i32 0, i32 5
  %11 = load %struct.token*, %struct.token** %10, align 8
  %12 = icmp ne %struct.token* %11, null
  br label %13

; <label>:13                                      ; preds = %8, %3
  %14 = phi i1 [ true, %3 ], [ %12, %8 ]
  br i1 %14, label %15, label %87

; <label>:15                                      ; preds = %13
  %16 = load %struct.token*, %struct.token** %curr, align 8
  %17 = getelementptr inbounds %struct.token, %struct.token* %16, i32 0, i32 2
  %18 = load %struct.token*, %struct.token** %17, align 8
  %19 = icmp ne %struct.token* %18, null
  br i1 %19, label %20, label %24

; <label>:20                                      ; preds = %15
  %21 = load %struct.token*, %struct.token** %curr, align 8
  %22 = getelementptr inbounds %struct.token, %struct.token* %21, i32 0, i32 2
  %23 = load %struct.token*, %struct.token** %22, align 8
  store %struct.token* %23, %struct.token** %curr, align 8
  br label %35

; <label>:24                                      ; preds = %15
  %25 = load %struct.token*, %struct.token** %curr, align 8
  %26 = getelementptr inbounds %struct.token, %struct.token* %25, i32 0, i32 5
  %27 = load %struct.token*, %struct.token** %26, align 8
  %28 = icmp ne %struct.token* %27, null
  br i1 %28, label %29, label %34

; <label>:29                                      ; preds = %24
  %30 = load %struct.token*, %struct.token** %curr, align 8
  %31 = getelementptr inbounds %struct.token, %struct.token* %30, i32 0, i32 5
  %32 = load %struct.token*, %struct.token** %31, align 8
  store %struct.token* %32, %struct.token** %curr, align 8
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.1, i32 0, i32 0))
  br label %34

; <label>:34                                      ; preds = %29, %24
  br label %35

; <label>:35                                      ; preds = %34, %20
  %36 = load %struct.token*, %struct.token** %curr, align 8
  %37 = getelementptr inbounds %struct.token, %struct.token* %36, i32 0, i32 3
  %38 = load i8*, i8** %37, align 8
  %39 = icmp ne i8* %38, null
  br i1 %39, label %40, label %45

; <label>:40                                      ; preds = %35
  %41 = load %struct.token*, %struct.token** %curr, align 8
  %42 = getelementptr inbounds %struct.token, %struct.token* %41, i32 0, i32 3
  %43 = load i8*, i8** %42, align 8
  %44 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.2, i32 0, i32 0), i8* %43)
  br label %45

; <label>:45                                      ; preds = %40, %35
  %46 = load %struct.token*, %struct.token** %curr, align 8
  %47 = getelementptr inbounds %struct.token, %struct.token* %46, i32 0, i32 6
  %48 = load i8*, i8** %47, align 8
  %49 = icmp ne i8* %48, null
  br i1 %49, label %50, label %55

; <label>:50                                      ; preds = %45
  %51 = load %struct.token*, %struct.token** %curr, align 8
  %52 = getelementptr inbounds %struct.token, %struct.token* %51, i32 0, i32 6
  %53 = load i8*, i8** %52, align 8
  %54 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i32 0, i32 0), i8* %53)
  br label %55

; <label>:55                                      ; preds = %50, %45
  %56 = load %struct.token*, %struct.token** %curr, align 8
  %57 = getelementptr inbounds %struct.token, %struct.token* %56, i32 0, i32 7
  %58 = load i32, i32* %57, align 8
  %59 = icmp ne i32 %58, 0
  br i1 %59, label %60, label %65

; <label>:60                                      ; preds = %55
  %61 = load %struct.token*, %struct.token** %curr, align 8
  %62 = getelementptr inbounds %struct.token, %struct.token* %61, i32 0, i32 7
  %63 = load i32, i32* %62, align 8
  %64 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0), i32 %63)
  br label %65

; <label>:65                                      ; preds = %60, %55
  %66 = load %struct.token*, %struct.token** %curr, align 8
  %67 = getelementptr inbounds %struct.token, %struct.token* %66, i32 0, i32 8
  %68 = load float, float* %67, align 4
  %69 = fcmp une float %68, 0.000000e+00
  br i1 %69, label %70, label %76

; <label>:70                                      ; preds = %65
  %71 = load %struct.token*, %struct.token** %curr, align 8
  %72 = getelementptr inbounds %struct.token, %struct.token* %71, i32 0, i32 8
  %73 = load float, float* %72, align 4
  %74 = fpext float %73 to double
  %75 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.5, i32 0, i32 0), double %74)
  br label %76

; <label>:76                                      ; preds = %70, %65
  %77 = load %struct.token*, %struct.token** %curr, align 8
  %78 = getelementptr inbounds %struct.token, %struct.token* %77, i32 0, i32 9
  %79 = load i32, i32* %78, align 8
  %80 = icmp ne i32 %79, 0
  br i1 %80, label %81, label %86

; <label>:81                                      ; preds = %76
  %82 = load %struct.token*, %struct.token** %curr, align 8
  %83 = getelementptr inbounds %struct.token, %struct.token* %82, i32 0, i32 9
  %84 = load i32, i32* %83, align 8
  %85 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0), i32 %84)
  br label %86

; <label>:86                                      ; preds = %81, %76
  br label %3

; <label>:87                                      ; preds = %13
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %token0 = alloca %struct.token*, align 8
  %token1 = alloca %struct.token*, align 8
  %token2 = alloca %struct.token*, align 8
  %token3 = alloca %struct.token*, align 8
  %token4 = alloca %struct.token*, align 8
  %token5 = alloca %struct.token*, align 8
  %token6 = alloca %struct.token*, align 8
  %token1_from_token3 = alloca %struct.token*, align 8
  %token3_from_token1 = alloca %struct.token*, align 8
  %token1_again = alloca %struct.token*, align 8
  store i32 0, i32* %1, align 4
  %2 = alloca i8, i64 72
  %3 = bitcast i8* %2 to %struct.token*
  store %struct.token* %3, %struct.token** %token0, align 8
  %4 = alloca i8, i64 72
  %5 = bitcast i8* %4 to %struct.token*
  store %struct.token* %5, %struct.token** %token1, align 8
  %6 = alloca i8, i64 72
  %7 = bitcast i8* %6 to %struct.token*
  store %struct.token* %7, %struct.token** %token2, align 8
  %8 = alloca i8, i64 72
  %9 = bitcast i8* %8 to %struct.token*
  store %struct.token* %9, %struct.token** %token3, align 8
  %10 = alloca i8, i64 72
  %11 = bitcast i8* %10 to %struct.token*
  store %struct.token* %11, %struct.token** %token4, align 8
  %12 = alloca i8, i64 72
  %13 = bitcast i8* %12 to %struct.token*
  store %struct.token* %13, %struct.token** %token5, align 8
  %14 = alloca i8, i64 72
  %15 = bitcast i8* %14 to %struct.token*
  store %struct.token* %15, %struct.token** %token6, align 8
  %16 = load %struct.token*, %struct.token** %token0, align 8
  %17 = load %struct.token*, %struct.token** %token1, align 8
  %18 = call i32 @add_child(%struct.token* %16, %struct.token* %17)
  %19 = load %struct.token*, %struct.token** %token1, align 8
  %20 = getelementptr inbounds %struct.token, %struct.token* %19, i32 0, i32 3
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.6, i32 0, i32 0), i8** %20, align 8
  %21 = load %struct.token*, %struct.token** %token1, align 8
  %22 = getelementptr inbounds %struct.token, %struct.token* %21, i32 0, i32 3
  %23 = load i8*, i8** %22, align 8
  %24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.7, i32 0, i32 0), i8* %23)
  %25 = load %struct.token*, %struct.token** %token2, align 8
  %26 = getelementptr inbounds %struct.token, %struct.token* %25, i32 0, i32 3
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.8, i32 0, i32 0), i8** %26, align 8
  %27 = load %struct.token*, %struct.token** %token2, align 8
  %28 = getelementptr inbounds %struct.token, %struct.token* %27, i32 0, i32 7
  store i32 22, i32* %28, align 8
  %29 = load %struct.token*, %struct.token** %token3, align 8
  %30 = getelementptr inbounds %struct.token, %struct.token* %29, i32 0, i32 3
  store i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.9, i32 0, i32 0), i8** %30, align 8
  %31 = load %struct.token*, %struct.token** %token3, align 8
  %32 = getelementptr inbounds %struct.token, %struct.token* %31, i32 0, i32 6
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.10, i32 0, i32 0), i8** %32, align 8
  %33 = load %struct.token*, %struct.token** %token3, align 8
  %34 = load %struct.token*, %struct.token** %token1, align 8
  %35 = getelementptr inbounds %struct.token, %struct.token* %34, i32 0, i32 2
  store %struct.token* %33, %struct.token** %35, align 8
  %36 = load %struct.token*, %struct.token** %token1, align 8
  %37 = load %struct.token*, %struct.token** %token3, align 8
  %38 = getelementptr inbounds %struct.token, %struct.token* %37, i32 0, i32 1
  store %struct.token* %36, %struct.token** %38, align 8
  %39 = load %struct.token*, %struct.token** %token3, align 8
  %40 = getelementptr inbounds %struct.token, %struct.token* %39, i32 0, i32 1
  %41 = load %struct.token*, %struct.token** %40, align 8
  store %struct.token* %41, %struct.token** %token1_from_token3, align 8
  %42 = load %struct.token*, %struct.token** %token1, align 8
  %43 = getelementptr inbounds %struct.token, %struct.token* %42, i32 0, i32 2
  %44 = load %struct.token*, %struct.token** %43, align 8
  store %struct.token* %44, %struct.token** %token3_from_token1, align 8
  %45 = load %struct.token*, %struct.token** %token1_from_token3, align 8
  %46 = getelementptr inbounds %struct.token, %struct.token* %45, i32 0, i32 3
  %47 = load i8*, i8** %46, align 8
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([40 x i8], [40 x i8]* @.str.11, i32 0, i32 0), i8* %47)
  %49 = load %struct.token*, %struct.token** %token1, align 8
  %50 = getelementptr inbounds %struct.token, %struct.token* %49, i32 0, i32 3
  %51 = load i8*, i8** %50, align 8
  %52 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.12, i32 0, i32 0), i8* %51)
  %53 = load %struct.token*, %struct.token** %token3_from_token1, align 8
  %54 = getelementptr inbounds %struct.token, %struct.token* %53, i32 0, i32 3
  %55 = load i8*, i8** %54, align 8
  %56 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([40 x i8], [40 x i8]* @.str.13, i32 0, i32 0), i8* %55)
  %57 = load %struct.token*, %struct.token** %token3, align 8
  %58 = getelementptr inbounds %struct.token, %struct.token* %57, i32 0, i32 3
  %59 = load i8*, i8** %58, align 8
  %60 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.14, i32 0, i32 0), i8* %59)
  %61 = load %struct.token*, %struct.token** %token1, align 8
  %62 = load %struct.token*, %struct.token** %token2, align 8
  %63 = call i32 @add_sibling(%struct.token* %61, %struct.token* %62)
  %64 = load %struct.token*, %struct.token** %token2, align 8
  %65 = load %struct.token*, %struct.token** %token3, align 8
  %66 = call i32 @add_sibling(%struct.token* %64, %struct.token* %65)
  %67 = load %struct.token*, %struct.token** %token1, align 8
  %68 = getelementptr inbounds %struct.token, %struct.token* %67, i32 0, i32 3
  %69 = load i8*, i8** %68, align 8
  %70 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.7, i32 0, i32 0), i8* %69)
  %71 = load %struct.token*, %struct.token** %token2, align 8
  %72 = getelementptr inbounds %struct.token, %struct.token* %71, i32 0, i32 1
  %73 = load %struct.token*, %struct.token** %72, align 8
  store %struct.token* %73, %struct.token** %token1_again, align 8
  %74 = load %struct.token*, %struct.token** %token1_again, align 8
  %75 = getelementptr inbounds %struct.token, %struct.token* %74, i32 0, i32 3
  %76 = load i8*, i8** %75, align 8
  %77 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.7, i32 0, i32 0), i8* %76)
  %78 = load %struct.token*, %struct.token** %token3, align 8
  %79 = load %struct.token*, %struct.token** %token4, align 8
  %80 = call i32 @add_sibling(%struct.token* %78, %struct.token* %79)
  %81 = load %struct.token*, %struct.token** %token4, align 8
  %82 = getelementptr inbounds %struct.token, %struct.token* %81, i32 0, i32 3
  store i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.15, i32 0, i32 0), i8** %82, align 8
  %83 = load %struct.token*, %struct.token** %token4, align 8
  %84 = load %struct.token*, %struct.token** %token5, align 8
  %85 = call i32 @add_child(%struct.token* %83, %struct.token* %84)
  %86 = load %struct.token*, %struct.token** %token5, align 8
  %87 = getelementptr inbounds %struct.token, %struct.token* %86, i32 0, i32 3
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.16, i32 0, i32 0), i8** %87, align 8
  %88 = load %struct.token*, %struct.token** %token5, align 8
  %89 = getelementptr inbounds %struct.token, %struct.token* %88, i32 0, i32 6
  store i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.17, i32 0, i32 0), i8** %89, align 8
  %90 = load %struct.token*, %struct.token** %token5, align 8
  %91 = load %struct.token*, %struct.token** %token6, align 8
  %92 = call i32 @add_sibling(%struct.token* %90, %struct.token* %91)
  %93 = load %struct.token*, %struct.token** %token6, align 8
  %94 = getelementptr inbounds %struct.token, %struct.token* %93, i32 0, i32 3
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.18, i32 0, i32 0), i8** %94, align 8
  %95 = load %struct.token*, %struct.token** %token6, align 8
  %96 = getelementptr inbounds %struct.token, %struct.token* %95, i32 0, i32 7
  store i32 999, i32* %96, align 8
  %97 = load %struct.token*, %struct.token** %token0, align 8
  %98 = call i32 @print_obj(%struct.token* %97)
  ret i32 0
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}
