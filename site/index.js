(() => {
    const TOUCHES = new Map();

    window.addEventListener("load", () => {
        const tapestry = document.querySelector("div.tapestry");

        fetch("/site/routes.json").then((response) => {
            return response.json();
        }).then((routes) => {
            for (const route of routes) {
                if (route.route === document.location.pathname) {
                    continue;
                }

                const articleDiv = document.createElement("div");
                const article = document.createElement("article");

                articleDiv.setAttribute("class", "top left layer");
                articleDiv.dataset.height = "-1";

                article.setAttribute("class", route.class);
                article.dataset.route = route.route;
                article.dataset.loaded = "false";
                article.dataset.title = route.title;
                article.dataset.scrollbar = "none";
                article.dataset.visible = "false";

                articleDiv.appendChild(article);
                tapestry.appendChild(articleDiv);

                if ((route.route.match(/\//g) || []).length % 2 === 0) {
                    continue;
                }

                const navDiv = document.createElement("div");
                const nav = document.createElement("nav");

                navDiv.setAttribute("class", "bottom right layer");
                navDiv.dataset.width = "full";
                navDiv.dataset.height = "1";

                nav.setAttribute("class", "horizontal component");
                nav.dataset.route = route.route;
                nav.dataset.routes = routes.filter((r) => {
                    return r.route.startsWith(route.route) && (route.route.match(/\//g) || []).length + 1 === (r.route.match(/\//g) || []).length;
                }).map((r) => r.route).join(" ");
                nav.dataset.loaded = "false";
                nav.dataset.scrollbar = "none";
                nav.dataset.visible = "false";

                navDiv.appendChild(nav);
                tapestry.appendChild(navDiv);
            }
        });

        initialize(document);
    });

    window.addEventListener("popstate", () => {
        select(history.state?.route || document.location.pathname, false);
    });

    window.addEventListener("touchstart", (e) => {
        const route = window.location.pathname;
        const article = document.querySelector("article[data-route=\"" + route + "\"]");

        for (const touch of e.touches) {
            if (!article.contains(touch.target)) {
                continue;
            }

            if (article.getAttribute("class").split(/\s+/).includes("vertical-writing")) {
                TOUCHES.set(touch.identifier, {
                    route,
                    forward: article.offsetWidth - article.scrollLeft >= article.scrollWidth && article.offsetHeight - article.scrollTop >= article.scrollHeight,
                    backward: article.scrollLeft >= 0 && article.scrollTop >= 0,
                    clientX: touch.clientX,
                    clientY: touch.clientY
                });
            }
        }
    });

    window.addEventListener("touchend", (e) => {
        for (const touch of e.changedTouches) {
            if (!TOUCHES.has(touch.identifier)) {
                continue;
            }

            const {
                route,
                forward,
                backward,
                clientX,
                clientY
            } = TOUCHES.get(touch.identifier);

            const article = document.querySelector("article[data-route=\"" + route + "\"]");

            const nav = document.querySelector(
                "nav[data-route=\"" + route + "\"]," +
                "nav[data-routes~=\"" + route + "\"]"
            );

            const link = nav.querySelector("a[data-route=\"" + route + "\"]");

            if (article.getAttribute("class").split(/\s+/).includes("vertical-writing")) {
                const vector = [
                    clientX - touch.clientX,
                    clientY - touch.clientY
                ];

                if (vector[0] < -32 && forward) {
                    const sibling = link.nextSibling;

                    if (sibling != null) {
                        sibling.click();
                    }
                } else if (vector[0] > 32 && backward && link.dataset.route !== nav.dataset.route) {
                    const sibling = link.previousSibling;

                    if (sibling != null) {
                        sibling.click();
                    }
                }
            }

            break;
        }

        TOUCHES.clear();
    });

    function initialize(element) {
        const links = element.querySelectorAll("a[data-route]");

        for (const link of links) {
            const route = link.dataset.route;

            link.addEventListener("click", (e) => {
                select(route, true);
                e.preventDefault();
            });
        }
    }

    function select(route, pushState = true) {
        ROUTE = route;

        const articles = document.querySelectorAll("article");
        const navs = document.querySelectorAll("nav");

        for (const article of articles) {
            if (article.dataset.route === route) {
                article.dataset.visible = "true";
                document.title = article.dataset.title;

                if (pushState) {
                    history.pushState({
                        route,
                        title: document.title
                    }, document.title, route);
                }
            } else {
                article.dataset.visible = "false";
            }

            if (article.dataset.visible === "true" && article.dataset.loaded !== "true") {
                fetch(route + "article.html", {cache: "no-cache"}).then((response) => {
                    if (response.ok) {
                        return response.text();
                    } else {
                        return Promise.reject("unrecognized routing: `" + route + "article.html'");
                    }
                }).then((html) => {
                    article.innerHTML = html;
                    article.dataset.loaded = "true";
                    initialize(article);
                });
            }
        }

        for (const nav of navs) {
            const links = nav.querySelectorAll("a[data-route]");

            for (const link of links) {
                if (link.dataset.route === route) {
                    link.dataset.focus = "true";
                    link.dataset.focusWithin = "false";
                } else if (link.dataset.route === nav.dataset.route) {
                    link.dataset.focus = "false";
                    link.dataset.focusWithin = "true";
                } else {
                    link.dataset.focus = "false";
                    link.dataset.focusWithin = "false";
                }
            }

            if (nav.dataset.route === route) {
                nav.dataset.visible = "true";
            } else if (nav.dataset.routes.split(/\s+/).includes(route)) {
                nav.dataset.visible = "true";
            } else {
                nav.dataset.visible = "false";
            }

            if (nav.dataset.visible === "true" && nav.dataset.loaded !== "true") {
                fetch(route + "nav.html", {cache: "no-cache"}).then((response) => {
                    if (response.ok) {
                        return response.text();
                    } else {
                        return Promise.reject("unrecognized routing: `" + route + "nav.html'");
                    }
                }).then((html) => {
                    nav.innerHTML = html;
                    nav.dataset.loaded = "true";
                    initialize(nav);
                });
            }
        }
    }
})();
