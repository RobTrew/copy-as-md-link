(() => {
    'use strict';

    // Rob Trew @ 2020

    // Copy Markdown Link to front document, URL, or resource.
    // Ver 0.10

    // Switched to running app-specific macros by UUID
    // fetched from a JSON dictionary stored in a 
    // uuidsForMDLink KM variable.

    // If this variable is not found, or a UUID retrieved
    // from it is not found, then the dictionary is regenerated.

    // The regeneration, which will happen on the first
    // run, but should only be needed thereafter when 
    // new sub-macros are added, will activate Keyboard Maestro.app

    // Normally use of the macro will, however, normally
    // bypass Keyboard Maestro.app and run through 
    // Keyboard Maestro Engine instead.

    ObjC.import('AppKit');

    const kmGroupName = 'MD link tools';

    // ---------------------- MAIN -----------------------
    // main :: IO ()
    const main = () => {
        const bundleID = frontAppBundleId();
        return either(
            msg => (
                alert('Copy as Markdown link')(msg),
                msg
            )
        )(
            mdLink => mdLink
        )(
            bindLR(
                void 0 !== bundleID ? (
                    Right(bundleID)
                ) : Left(
                    'No active application detected'
                )
            )(
                bundleID => {
                    // ------------ BROWSER ? ------------
                    const fnBrowser = browserCode()[bundleID];
                    return Boolean(fnBrowser) ? (
                        fnBrowser(bundleID)
                    ) : (() => {
                        // ---- APP-SPECIFIC MACRO ? -----
                        const
                            kme = Application('Keyboard Maestro Engine'),
                            dctUUID = either(
                                msg => (
                                    console.log(
                                        'BundleID map had to be regenerated',
                                        msg
                                    ),
                                    // Regenerated UUID dictionary
                                    updatedUUIDMap()
                                )
                            )(
                                // UUID dictionary from existing
                                // KM Variable
                                dct => dct
                            )(
                                jsonParseLR(
                                    kme.getvariable('uuidsForMDLink')
                                )
                            ),
                            maybeUUID = dctUUID[bundleID];
                        return Boolean(maybeUUID) ? (
                            either(
                                // If the UUID wasn't found,
                                // then run a new one from an 
                                // updated dictionary.
                                msg => (
                                    bindLR(
                                        doScriptLR(kme)(
                                            updatedUUIDMap()[bundleID]
                                        )
                                    )(
                                        // Link after use of alternate UUID
                                        _ => Right(kme.getvariable('mdLink'))
                                    )
                                )
                            )(
                                // Link read after execution with UUID
                                _ => Right(kme.getvariable('mdLink'))
                            )(
                                // Run macro with this UUID if possible.
                                doScriptLR(kme)(maybeUUID)
                            )
                        ) : appFrontWindowMDLinkLR(bundleID);
                    })();
                }
            )
        );
    };

    const browserCode = () => ({
        'com.apple.Safari': browserLinkLR,
        'com.google.Chrome': browserLinkLR
    });


    // doScriptLR :: UUID -> Either String String
    const doScriptLR = kme =>
        uuid => {
            try {
                return (
                    kme.doScript(uuid),
                    Right(uuid)
                );
            } catch (e) {
                return Left(
                    `Macro UUID :: ${uuid}\n\n${e.message}`
                );
            }
        };

    // -------------- BUNDLEID -> UUID MAP ---------------

    // updatedUUIDMap :: IO () -> { bundleID :: UUID }
    const updatedUUIDMap = () => {
        const
            macroGroupName = 'MD Link tools',
            mdLinkToolsGroups = Application(
                'Keyboard Maestro'
            ).macroGroups.where({
                name: macroGroupName
            });
        return either(
            alert('Copy as MD Link - Map bundle to UUID')
        )(
            dictUUIDs => (
                Application('Keyboard Maestro Engine')
                .setvariable('uuidsForMDLink', {
                    to: JSON.stringify(
                        dictUUIDs, null, 2
                    )
                }),
                dictUUIDs
            )
        )(
            0 < mdLinkToolsGroups.length ? (() => {
                const
                    instances = mdLinkToolsGroups.at(0)
                    .macros()
                    .flatMap(macro => {
                        const k = macro.name();
                        return k.includes('.') ? (
                            [
                                [k, macro.id()]
                            ]
                        ) : [];
                    });
                return Right(
                    instances.reduce(
                        (a, [bundle, uuid]) => Object.assign(
                            a, {
                                [bundle]: uuid
                            }
                        ), {}
                    )
                );
            })() : Left(
                `Macro group not found:\n\n\t${macroGroupName}`
            )
        );
    };


    // --------------------- BROWSERS ----------------------

    // browserLinkLR :: String -> Either String IO String
    const browserLinkLR = bundleID => {
        const
            app = Application(bundleID),
            ws = app.windows;
        return bindLR(
            0 < ws.length ? (
                Right(ws.at(0))
            ) : Left('No windows open in ' + bundleID)
        )(
            w => {
                const tabs = w.tabs;
                return 0 < tabs.length ? (() => {
                    const
                        tab = w[
                            'com.apple.Safari' === bundleID ? (
                                'currentTab'
                            ) : 'activeTab'
                        ]();
                    return Right(
                        `[${tab.name()}](${tab.url()})`
                    );
                })() : Left(
                    'No open tabs in front window of ' + bundleID
                );
            }
        );
    };

    // ----------------------- JXA -----------------------

    // frontAppBundleId :: () -> String
    const frontAppBundleId = () => {
        const uw = ObjC.unwrap;
        return uw(uw(
            $.NSWorkspace.sharedWorkspace.activeApplication
        ).NSApplicationBundleIdentifier);
    };

    // ------- DEFAULT - DOCUMENT OF FRONT WINDOW --------

    // appFrontWindowMDLinkLR :: String -> Either String String
    const appFrontWindowMDLinkLR = bundleID => {
        const
            procs = Object.assign(
                Application('System Events'), {
                    includeStandardAdditions: true
                })
            .applicationProcesses.where({
                bundleIdentifier: bundleID
            });
        return bindLR(
            bindLR(
                procs.length > 0 ? (
                    Right(procs.at(0).windows)
                ) : Left('Application not found: ' + bundleID)
            )(ws => ws.length > 0 ? (
                Right(ws.at(0))
            ) : Left(`No windows found for ${bundleID}`))
        )(w => {
            const
                uw = ObjC.unwrap,
                [winTitle, maybeDocURL] = map(
                    bundleID => uw(w.attributes.byName(bundleID).value())
                )(['AXTitle', 'AXDocument']);
            return Boolean(maybeDocURL) ? (
                Right(`[${winTitle}](${maybeDocURL})`)
            ) : Left(
                `Window '${winTitle}' of:\n\n\t${bundleID}` + [
                    '\n\nmay not be a document window.',
                    `\nConsider adding a macro named '${bundleID}'`,
                    `to the KM Group '${kmGroupName}'.`,
                    '\n(Or request such a macro, which should',
                    'save a [label](url) string) in the',
                    'KM variable "mdLink")',
                    `on the Keyboard Maestro forum).`
                ].join('\n')
            );
        });
    };

    // alert :: String => String -> IO String
    const alert = title =>
        s => {
            const sa = Object.assign(
                Application('System Events'), {
                    includeStandardAdditions: true
                });
            return (
                sa.activate(),
                sa.displayDialog(s, {
                    withTitle: title,
                    buttons: ['OK'],
                    defaultButton: 'OK'
                }),
                s
            );
        };


    // ----------------- GENERIC FUNCTIONS -----------------
    // https://github.com/RobTrew/prelude-jxa

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });


    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });


    // bindLR (>>=) :: Either a ->
    // (a -> Either b) -> Either b
    const bindLR = m =>
        mf => undefined !== m.Left ? (
            m
        ) : mf(m.Right);


    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = fl =>
        // Application of the function fl to the
        // contents of any Left value in e, or
        // the application of fr to its Right value.
        fr => e => 'Either' === e.type ? (
            undefined !== e.Left ? (
                fl(e.Left)
            ) : fr(e.Right)
        ) : undefined;


    // jsonParseLR :: String -> Either String a
    const jsonParseLR = s => {
        // Either a message, or a JS value obtained
        // from a successful parse of s.
        try {
            return Right(JSON.parse(s));
        } catch (e) {
            return Left(
                `${e.message} (line:${e.line} col:${e.column})`
            );
        }
    };


    // map :: (a -> b) -> [a] -> [b]
    const map = f =>
        // The list obtained by applying f
        // to each element of xs.
        // (The image of xs under f).
        xs => xs.map(f);

    // MAIN ---
    return main();
})();