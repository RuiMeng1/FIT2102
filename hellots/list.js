"use strict";
var lists;
(function (lists) {
    const list = {
        data: 1,
        next: {
            data: 2,
            next: {
                data: 3,
                next: null
            }
        }
    };
    function length(l) {
        return l ? length(l.next) + 1 : 0;
    }
    console.log(length(list));
})(lists || (lists = {}));
