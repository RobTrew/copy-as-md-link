(() => {
    "use strict";

    // Rob Trew @2021

    // Value of KM mdLink variable placed in clipboard,
    // both as plain text,
    // and also as a labelled RTF hyperlink.

    ObjC.import("AppKit");

    // main :: IO ()
    const main = () => {
        const
            md = Application("Keyboard Maestro Engine")
            .getvariable("mdLink"),
            kv = mdLinkParts(md),
            label = htmlEncoded(kv[0]);

        return (
            copyTypedString(true)(
                "public.utf8-plain-text"
            )(md),
            copyTypedString(false)(
                "public.rtf"
            )(
                either(x => x)(x => x)(
                    rtfFromHTML(
                        // The url field is not empty ?
                        Boolean(kv[1]) ? (
                            `<a href="${kv[1]}">` + (
                                `${label}</a>`
                            )
                        ) : `<p>${label}</p>`
                    )
                )
            ),
            md
        );
    };


    // ---------------------- LINKS ----------------------

    // copyTypedString :: Bool -> String -> String -> IO ()
    const copyTypedString = blnClear =>
        // public.html, public.rtf, public.utf8-plain-text
        pbType => s => {
            const pb = $.NSPasteboard.generalPasteboard;

            return (
                blnClear && pb.clearContents,
                pb.setStringForType(
                    $(s),
                    $(pbType)
                )
            );
        };


    // htmlEncoded :: String -> String
    const htmlEncoded = s => {
        const rgx = /[\w\s]/u;

        return [...s].map(
            c => rgx.test(c) ? (
                c
            ) : `&#${c.codePointAt(0)};`
        ).join("");
    };


    // mdLinkParts :: String -> (String, String)
    const mdLinkParts = s => {
        const ab = s.trim().split("](");

        return 2 !== ab.length ? (
            Tuple(s)("")
        ) : Tuple(ab[0].slice(1))(
            ab[1].slice(0, -1)
        );
    };


    // rtfFromHTML :: String -> Either String String
    const rtfFromHTML = strHTML => {
        const
            as = $.NSAttributedString.alloc
            .initWithHTMLDocumentAttributes($(strHTML)
                .dataUsingEncoding($.NSUTF8StringEncoding),
                0
            );

        return bindLR(
            "function" !== typeof as
            .dataFromRangeDocumentAttributesError ? (
                Left("String could not be parsed as HTML")
            ) : Right(as)
        )(
            // Function bound if Right value obtained above:
            htmlAS => {
                const
                    error = $(),
                    rtfData = htmlAS
                    .dataFromRangeDocumentAttributesError({
                            "location": 0,
                            "length": htmlAS.length
                        }, {
                            DocumentType: "NSRTF"
                        },
                        error
                    );

                return Boolean(
                    ObjC.unwrap(rtfData) && !error.code
                ) ? Right(
                    ObjC.unwrap($.NSString.alloc
                        .initWithDataEncoding(
                            rtfData,
                            $.NSUTF8StringEncoding
                        ))
                ) : Left(ObjC.unwrap(
                    error.localizedDescription
                ));
            }
        );
    };


    // --------------------- GENERIC ---------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: "Either",
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: "Either",
        Right: x
    });

    // bindLR (>>=) :: Either a ->
    // (a -> Either b) -> Either b
    const bindLR = m =>
        mf => m.Left ? (
            m
        ) : mf(m.Right);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = fl =>
        // Application of the function fl to the
        // contents of any Left value in e, or
        // the application of fr to its Right value.
        fr => e => e.Left ? (
            fl(e.Left)
        ) : fr(e.Right);

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a =>
        b => ({
            type: "Tuple",
            "0": a,
            "1": b,
            length: 2
        });

    return main();
})();