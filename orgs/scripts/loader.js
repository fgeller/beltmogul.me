(function () {
    $(document).ready(function (){
        var headers = $(".outline-2 h2 a").appear();
        var loading = false;
        var load = function ($next) {
            loading = true;
            var href = $next.attr("href");
            _.remove(headers, function (a) { return $(a).attr("href") == href; });
            var outline = $next.parent().parent();

            $.ajax(href)
                .done(function (data) {
                    var $entry = findEntry(data).hide();
                    outline.append($entry);
                    $entry.wrapAll("<div class=\"entry\" />").fadeIn(800);
                })
                .always(function () { loading = false; });
        };

        var findEntry = function (data) {
            var htmlData = $.parseHTML(data);
            var $content = $(_.find(htmlData, function (el) { return el.nodeName == "DIV" && el.id == "content"; }));
            return $(_.rest($content.children())); // first: h1.title
        };

        var shouldLoad = function ($next, visible) {
            var nextHref = $next.attr("href");
            var hasNextHref = function (el) { return $(el).attr("href") == nextHref; };
            var nextHeaderVisible = -1 < _.findIndex(visible, hasNextHref);
            return !loading && nextHeaderVisible;
        };

        var maybeLoadNext = function (e, visible) {
            var $next = $(_.first(headers));
            if (shouldLoad($next, visible)) {
                load($next);
            }
        };

        $(document.body).on('appear', '.outline-2 h2 a', maybeLoadNext);
        $.force_appear();
    });
})();
