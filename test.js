const calcThaiIdChecksum = (listId) => {
  let len = listId.length;
  let lastElem = listId.pop();
  return (
    11 -
    (listId
      .map((elem, i) => elem * (len - i))
      .reduce((acc, cur) => acc + cur, 0) %
      11)
  );
};

let result = calcThaiIdChecksum([1, 5, 0, 0, 7, 0, 1, 2, 4, 8, 9, 2, 4]);
console.log(result);
