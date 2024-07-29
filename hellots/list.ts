module lists{

const list: List = {
    data: 1,
    next:{
        data:2,
        next:{
            data:3,
            next: null
        }
    }
}

type List = IListNode | null;

interface IListNode {
    data: number,
    next: List
}

function length(l:List): number{
    return l ? length(l.next) + 1 : 0;
}

console.log(length(list));

}