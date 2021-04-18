window.addEventListener("load", () => {
  const inputStates = document.querySelectorAll("input.state");

  for (const inputState of inputStates) {
    const route = inputState.dataset.route;

    inputState.addEventListener("change", () => {
      const article = document.querySelector("article.component[data-route=\"" + route + "\"]");
      const nav = document.querySelector("nav.component[data-route=\"" + route + "\"], nav.component[data-routes~=\"" + route + "\"]");

      document.title = article.dataset.title;

      history.pushState({
        route,
        title: article.dataset.title
      }, article.dataset.title, route);

      if (article.dataset.loaded !== "true") {
        fetch(route + "article.html", {cache: "no-cache"}).then((response) => {
          if (response.ok) {
            return response.text();
          } else {
            return Promise.reject("unrecognized routing: `" + route + "article.html'");
          }
        }).then((html) => {
          article.innerHTML = html;
          article.dataset.loaded = "true";

          refreshLinks();
        });
      }

      if (nav != null && nav.dataset.loaded !== "true") {
        fetch(nav.dataset.route + "nav.html", {cache: "no-cache"}).then((response) => {
          if (response.ok) {
            return response.text();
          } else {
            return Promise.reject("unrecognized routing: `" + route + "nav.html'");
          }
        }).then((html) => {
          nav.innerHTML = html;
          nav.dataset.loaded = "true";

          refreshLinks();
        });
      }
    });
  }

  refreshLinks();

  function refreshLinks() {
    const routeLinks = document.querySelectorAll("a[data-route]");

    for (const routeLink of routeLinks) {
      const routeLabel = document.createElement("label");
      routeLabel.dataset.route = routeLink.dataset.route;
      routeLabel.setAttribute("for", routeLink.dataset.route);
      routeLabel.setAttribute("class", "component");
      routeLabel.innerHTML = routeLink.innerHTML;
      routeLink.parentNode.replaceChild(routeLabel, routeLink);
    }
  }
});

window.addEventListener("popstate", () => {
  document.title = history.state.title;

  const inputState = document.getElementById(history.state.route);
  inputState.checked = true;
});

(() => {
  let startClientX = null;
  let startClientY = null;
  let endClientX = null;
  let endClientY = null;

  const TOUCHES = new Map();
  let TOUCH = null;

  window.addEventListener("touchstart", (e) => {
    const route = document.querySelector("input.state:checked").dataset.route;

    const article = document.querySelector("article.component[data-route=\"" + route + "\"]");

    if (!((article.scrollLeft === 0 && article.scrollTop === 0) || (article.offsetWidth + Math.abs(article.scrollLeft) >= article.scrollWidth && article.offsetHeight + Math.abs(article.scrollTop) >= article.scrollHeight))) {
      return;
    }

    TOUCHES.clear();

    for (const touch of e.touches) {
      if (!article.contains(touch.target)) {
        continue;
      }

      if (!TOUCHES.has(touch.identifier)) {
        TOUCHES.set(touch.identifier, {
          route,
          forward: article.offsetWidth + Math.abs(article.scrollLeft) >= article.scrollWidth && article.offsetHeight + Math.abs(article.scrollTop) >= article.scrollHeight,
          backward: article.scrollLeft === 0 && article.scrollTop === 0,
          startClientX: touch.clientX,
          startClientY: touch.clientY
        });
      } else {
        const t = TOUCHES.get(touch.identifier);

        t.startClientX = touch.clientX;
        t.startClientY = touch.clientY;
      }
    }
  });

  window.addEventListener("touchend", (e) => {
    for (const touch of e.changedTouches) {
      if (!TOUCHES.has(touch.identifier)) {
        continue;
      } else {
        const t = TOUCHES.get(touch.identifier);

        t.endClientX = touch.clientX;
        t.endClientY = touch.clientY;

        TOUCH = t;
      }
    }

    if (TOUCH == null) {
      return;
    }

    TOUCHES.clear();

    const article = document.querySelector("article.component[data-route=\"" + TOUCH.route + "\"]");
    const nav = document.querySelector("nav.component[data-route=\"" + TOUCH.route + "\"], nav.component[data-routes~=\"" + TOUCH.route + "\"]");
    const label = nav.querySelector("label.component[data-route=\"" + TOUCH.route + "\"]");

    const forward = TOUCH.forward;
    const backward = TOUCH.backward;
    const vector = [TOUCH.startClientX - TOUCH.endClientX, TOUCH.startClientY - TOUCH.endClientY];

    TOUCH = null;

    if (vector[0] === 0 && vector[1] === 0) {
      return;
    }

    const articleClasses = article.getAttribute("class").split(/\s+/);

    if (articleClasses.includes("vertical-writing")) {
      if (vector[0] < -32 && forward) {
        const sibling = label.nextSibling;

        if (sibling != null) {
          sibling.click();

          const nextArticle = document.querySelector("article.component[data-route=\"" + sibling.dataset.route + "\"]");
          nextArticle.scrollLeft = 0;
          nextArticle.scrollTop = 0;
        }
      } else if (vector[0] > 32 && backward) {
        if (label.dataset.route === nav.dataset.route) {
          return;
        }

        const sibling = label.previousSibling;

        if (sibling != null && sibling.dataset.route !== "/") {
          sibling.click();

          const previousArticle = document.querySelector("article.component[data-route=\"" + sibling.dataset.route + "\"]");

          previousArticle.scrollLeft = 0;
          previousArticle.scrollTop = 0;
        }
      }
    }
  });
})();
