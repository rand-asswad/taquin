


class Grid {
    constructor(table, data) {
        this._table = document.getElementById(table);
        this.data = data;
    }
    
    set data(data) {
        if (data) {
            this._table.innerHTML = "";
            this._data = data;
            let n = Math.sqrt(data.length);
            let i,j;
            for (i = 0; i < data.length; i += n) {
                let row = this._table.insertRow();
                for (j = i; j < i + n; j++) {
                    let cell = row.insertCell();
                    if (data[j] > 0) {
                        let text = document.createTextNode(data[j]);
                        cell.appendChild(text);
                    }
                }
            }
        }
    }

    get data() {return this._data;}
}