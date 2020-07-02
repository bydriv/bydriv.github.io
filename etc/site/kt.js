var KT = {
    KT: function KT(timestamp) {
        var date = new Date(timestamp);
        var year = date.getFullYear();
        var month = date.getMonth();
        var day = date.getDate();

        var ktDay = 0;

        for (var i = 0; i < 12; ++i) {
            if (i + 1 < month) {
                ktDay += new Date(Date.UTC(year, i + 1, 0)).getDate();
                continue;
            }

            ktDay += day - 1;
            break;
        }

        return {
            year: year - 1970,
            day: ktDay,
            week: date.getDay()
        };
    },
    month: function month(year, month, f) {
        var days = new Date(Date.UTC(year, month, 0)).getDate();

        for (var i = 0; i < days; ++i)
            f(KT.KT(Date.UTC(year, month - 1, i + 1)));
    },
    monthCalendar: function monthCalendar(year, month, f) {
        var week = [];

        KT.month(year, month, function (kt) {
            var fill = kt.week - week.length;

            for (var i = 0; i < fill; ++i)
                week.push(null);

            week.push(kt);

            if (kt.week === 6) {
                f(week);
                week = [];
            }
        });

        if (week.length === 0)
            return;

        var fill = 7 - week.length;

        for (var i = 0; i < fill; ++i)
            week.push(null);

        f(week);
    }
};

window.addEventListener("load", function () {
    var containers = document.querySelectorAll(".kt-month-calendar");

    for (var i = 0; i < containers.length; ++i) {
        var container = containers[i];

        if (Boolean(container.dataset.today)) {
            var date = new Date(Date.now());
            var year = date.getFullYear();
            var month = date.getMonth();
            var day = date.getDate();
        } else {
            var year = Number(container.dataset.year);
            var month = Number(container.dataset.month);

            var day = container.dataset.day == null
                ? null
                : Number(container.dataset.day);
        }

        var monthHeader = document.createElement("div");
        monthHeader.setAttribute("class", "header");

        switch (month) {
        case 1:
            monthHeader.innerText = "January";
            break;
        case 2:
            monthHeader.innerText = "February";
            break;
        case 3:
            monthHeader.innerText = "March";
            break;
        case 4:
            monthHeader.innerText = "April";
            break;
        case 5:
            monthHeader.innerText = "May";
            break;
        case 6:
            monthHeader.innerText = "June";
            break;
        case 7:
            monthHeader.innerText = "July";
            break;
        case 8:
            monthHeader.innerText = "August";
            break;
        case 9:
            monthHeader.innerText = "September";
            break;
        case 10:
            monthHeader.innerText = "October";
            break;
        case 11:
            monthHeader.innerText = "November";
            break;
        case 12:
            monthHeader.innerText = "December";
            break;
        }

        container.appendChild(monthHeader);

        var weekHeader = document.createElement("div");
        weekHeader.setAttribute("class", "week");

        var sunHeader = document.createElement("div");
        sunHeader.setAttribute("class", "day");
        sunHeader.innerText = "Sun.";
        weekHeader.appendChild(sunHeader);

        var monHeader = document.createElement("div");
        monHeader.setAttribute("class", "day");
        monHeader.innerText = "Mon.";
        weekHeader.appendChild(monHeader);

        var tueHeader = document.createElement("div");
        tueHeader.setAttribute("class", "day");
        tueHeader.innerText = "Tue.";
        weekHeader.appendChild(tueHeader);

        var wedHeader = document.createElement("div");
        wedHeader.setAttribute("class", "day");
        wedHeader.innerText = "Wed.";
        weekHeader.appendChild(wedHeader);

        var thuHeader = document.createElement("div");
        thuHeader.setAttribute("class", "day");
        thuHeader.innerText = "Thu.";
        weekHeader.appendChild(thuHeader);

        var friHeader = document.createElement("div");
        friHeader.setAttribute("class", "day");
        friHeader.innerText = "Fri.";
        weekHeader.appendChild(friHeader);

        var satHeader = document.createElement("div");
        satHeader.setAttribute("class", "day");
        satHeader.innerText = "Sat.";
        weekHeader.appendChild(satHeader);

        container.appendChild(weekHeader);

        var d = 0;

        KT.monthCalendar(year, month, function (week) {
            var weekContainer = document.createElement("div");
            weekContainer.setAttribute("class", "week");

            for (var j = 0; j < week.length; ++j) {
                var dayContainer = document.createElement("div");

                if (week[j] == null) {
                    dayContainer.setAttribute("class", "day");
                } else {
                    ++d;

                    if (day != null && d === day) {
                        dayContainer.setAttribute("class", "today");
                    } else {
                        dayContainer.setAttribute("class", "day");
                    }

                    dayContainer.innerText = week[j].day.toString();
                }

                weekContainer.appendChild(dayContainer);
            }

            container.appendChild(weekContainer);
        });
    }
});
