(() => {
    "use strict";

    // Rob Trew @2021, @2022

    // Value of KM mdLink variable placed in clipboard,
    // both as plain text,
    // and also as a labelled (and styled) RTF hyperlink.

    // Ver 0.2 added RTF link style options.
    // Ver 0.3 updated link style font, size, color.
    // Ver 0.4 added a pasteboard item for Bike Outliner

    ObjC.import("AppKit");

    // main :: IO ()
    const main = () => {
        const linkStyle = {
            "color": "#ACACAC",
            "font-family": "Helvetica Neue, sans-serif",
            "font-size": "13px"
        };

        const
            md = Application("Keyboard Maestro Engine")
            .getvariable("mdLink"),
            labelLinks = mdLinkPartPairs(md);

        return (
            copyTypedString(true)(
                "public.utf8-plain-text"
            )(md),
            copyTypedString(false)(
                "com.hogbaysoftware.bike.xml"
            )(
                bikeLinksXML(labelLinks)
            ),
            either(
                alert("Copy as RTF Link")
            )(
                copyTypedString(false)("public.rtf")
            )(
                rtfFromHTML(
                    styledLinks(linkStyle)(
                        labelLinks
                    )
                )
            ),
            md
        );
    };


    // ---------------------- LINKS ----------------------

    const bikeLinksXML = labelLinks => {
        const
            ps = labelLinks.map(
                ([k, url]) =>
                    `<li><p><a href="${url}">${k}</a></p></li>`
            ).join("\n");

        return `<html><body><ul>${ps}</ul></body></html>`;
    };

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

    // cssTag :: Dict {String :: String} -> String
    const cssTag = settings => {
        const
            kvs = Object.entries(settings)
            .map(([k, v]) => `${k}: ${v};`)
            .join(" "),
            css = `p { ${kvs} }`;

        return `<style type="text/css">${css}</style>`;
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


    // mdLinkPartPairs :: String -> [(String, String)]
    const mdLinkPartPairs = s =>
        lines(s).map(x => {
            const ab = x.trim().split("](");

            return 2 !== ab.length ? (
                Tuple(s)("")
            ) : Tuple(ab[0].slice(1))(
                ab[1].slice(0, -1)
            );
        });


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

    // styledLinks :: Dict -> [(String, String)] -> String
    const styledLinks = styleDict =>
        // One or more <a href> lines, wrapped in <p>...</p>
        // and preceded by a <style> tag based on styleDict.
        kvs => {
            const
                css = cssTag(styleDict),
                linkTags = kvs.map(kv => {
                    const [label, url] = biList(kv).map(
                            htmlEncoded
                        ),
                        labelOrFullLink = Boolean(url) ? (
                            `<a href="${url}">${label}</a>`
                        ) : label;

                    return `${labelOrFullLink}`;
                })
                .join("<br>");

            return `${css}\n<p>${linkTags}</p>`;
        };

    // ----------------------- JXA -----------------------

    // alert :: String => String -> IO String
    const alert = title =>
        s => {
            const sa = Object.assign(
                Application("System Events"), {
                    includeStandardAdditions: true
                });

            return (
                sa.activate(),
                sa.displayDialog(s, {
                    withTitle: title,
                    buttons: ["OK"],
                    defaultButton: "OK"
                }),
                s
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

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a =>
    // A pair of values, possibly of
    // different types.
        b => ({
            type: "Tuple",
            "0": a,
            "1": b,
            length: 2,
            *[Symbol.iterator]() {
                for (const k in this) {
                    if (!isNaN(k)) {
                        yield this[k];
                    }
                }
            }
        });

    // biList :: (a, a) -> [a]
    const biList = ab =>
        // A list of two items derived from a tuple.
        Array.from(ab);

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
        fr => e => "Left" in e ? (
            fl(e.Left)
        ) : fr(e.Right);

    // lines :: String -> [String]
    const lines = s =>
        // A list of strings derived from a single
        // string delimited by newline and or CR.
        0 < s.length ? (
            s.split(/[\r\n]+/u)
        ) : [];

    return main();
})();