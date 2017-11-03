; ModuleID = 'json_obj.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.JSON = type { %struct.JSON*, %struct.JSON*, %struct.JSON*, i8*, i32, float, i32, i8* }

@.str = private unnamed_addr constant [4 x i8] c"Joe\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: nounwind uwtable
define %struct.JSON* @new_token() #0 {
  %token = alloca %struct.JSON*, align 8
  %1 = alloca i8, i64 56
  %2 = bitcast i8* %1 to %struct.JSON*
  store %struct.JSON* %2, %struct.JSON** %token, align 8
  %3 = load %struct.JSON*, %struct.JSON** %token, align 8
  %4 = getelementptr inbounds %struct.JSON, %struct.JSON* %3, i32 0, i32 7
  store i8* null, i8** %4, align 8
  %5 = load %struct.JSON*, %struct.JSON** %token, align 8
  %6 = getelementptr inbounds %struct.JSON, %struct.JSON* %5, i32 0, i32 3
  store i8* null, i8** %6, align 8
  %7 = load %struct.JSON*, %struct.JSON** %token, align 8
  %8 = getelementptr inbounds %struct.JSON, %struct.JSON* %7, i32 0, i32 0
  store %struct.JSON* null, %struct.JSON** %8, align 8
  %9 = load %struct.JSON*, %struct.JSON** %token, align 8
  %10 = getelementptr inbounds %struct.JSON, %struct.JSON* %9, i32 0, i32 1
  store %struct.JSON* null, %struct.JSON** %10, align 8
  %11 = load %struct.JSON*, %struct.JSON** %token, align 8
  %12 = getelementptr inbounds %struct.JSON, %struct.JSON* %11, i32 0, i32 2
  store %struct.JSON* null, %struct.JSON** %12, align 8
  %13 = load %struct.JSON*, %struct.JSON** %token, align 8
  ret %struct.JSON* %13
}

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %token = alloca %struct.JSON*, align 8
  store i32 0, i32* %1, align 4
  %2 = call %struct.JSON* @new_token()
  store %struct.JSON* %2, %struct.JSON** %token, align 8
  %3 = load %struct.JSON*, %struct.JSON** %token, align 8
  %4 = getelementptr inbounds %struct.JSON, %struct.JSON* %3, i32 0, i32 7
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i8** %4, align 8
  %5 = load %struct.JSON*, %struct.JSON** %token, align 8
  %6 = getelementptr inbounds %struct.JSON, %struct.JSON* %5, i32 0, i32 7
  %7 = load i8*, i8** %6, align 8
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0), i8* %7)
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}
