.assembly tmp {}
.method static int32 Main() cil managed {
	.entrypoint
	.locals init (
		[0] string V_0
	)
	ldstr "Hello, World"
	stloc 0
	ldc.i4.0
	ret
}
