main:
	call count 0
	halt

count:
	lit 0

begin0:
	load 1
	lit 50
	lt
	jnz while0
	pop
	ret

while0:
	load 1
	call fib 1
	print
	load 1
	lit 1
	add
	store 1
	j begin0

fib:
	lit 0
	lit 1

begin1:
	load -3
	jnz while1
	pop
	ret

while1:
	load 1
	load 2
	add
	load 2
	store 1
	store 2
	load -3
	lit 1
	sub
	store -3
	j begin1
