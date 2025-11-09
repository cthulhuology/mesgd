
export async function encryption() {                                                                        
	const keypair = await crypto.subtle.generateKey({
		name: "RSA-OAEP",
                modulusLength: 2048,
                publicExponent: new Uint8Array([1,0,1]),
                hash: "SHA-256"},
                true,
                ['encrypt','decrypt','sign','verify'])
        console.log(keypair)
        return keypair
}               

export async function signature() {
	const keypair = await crypto.subtle.generateKey({
                name: "RSASSA-PKCS1-v1_5",
                modulusLength: 2048,
                publicExponent: new Uint8Array([1,0,1]),
                hash: "SHA-256"},
                true,
                ['sign','verify'])
        console.log(keypair)
        return keypair
}               

