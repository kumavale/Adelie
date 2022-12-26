//$ ilasm /output:test/adelie_std.dll /dll library/std.il

.assembly extern mscorlib {}
.assembly 'adelie_std' {}

.class public auto ansi sealed 'std' extends [mscorlib]System.Object {
    // panic!
    .method public static hidebysig specialname void 'panic'(string msg, string locate) cil managed {
        .maxstack 4
        ldstr "paniced at '"
        ldarg msg
        ldstr "', "
        ldarg locate
        call string [mscorlib]System.String::Concat(string, string, string, string)
        call void [mscorlib]System.Console::WriteLine(string)
        ldc.i4 101
        call void [mscorlib]System.Environment::Exit(int32)
        ret
    }

    // assert!
    .method public static hidebysig specialname void 'assert'(bool cond, string stringify, string locate) cil managed {
        .maxstack 2
        ldarg cond
        brtrue IL_assert_end
        ldstr "assertion failed: "
        ldarg stringify
        call string [mscorlib]System.String::Concat(string, string)
        ldarg locate
        call void [adelie_std]std::'panic'(string, string)
        IL_assert_end:
        ret
    }

    // assert_eq!
    .method public static hidebysig specialname void 'assert_eq'(object left, object right, string locate) cil managed {
        .maxstack 32
        ldarg left
        ldarg right
        call bool [mscorlib]System.Object::Equals(object, object)
        brtrue IL_assert_eq_end
        ldstr "assertion failed: `(left == right)`\n"
        ldstr "  left: `"
        ldarg left
        callvirt instance string [mscorlib]System.Object::ToString()
        ldstr "`\n"
        call string [mscorlib]System.String::Concat(string, string, string)
        ldstr " right: `"
        ldarg right
        callvirt instance string [mscorlib]System.Object::ToString()
        ldstr "`"
        call string [mscorlib]System.String::Concat(string, string, string)
        call string [mscorlib]System.String::Concat(string, string, string)
        ldarg locate
        call void [adelie_std]std::'panic'(string, string)
        IL_assert_eq_end:
        ret
    }
}
