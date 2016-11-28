const fs = require('fs');
const request = require('request');

const baseUrl = "http://www.kauppalehti.fi";

const loadStocks = new Promise((resolve, reject) => {
    fs.readFile(__dirname + '/stocks/index.txt', (err, data) => {
        return err ? reject(err) : resolve(data.toString());
    });
});

const parseStocks = new Promise((resolve, reject) => {
    loadStocks.then((data) => {
        const stocks = [];
        const stocksPairs = data.split("\n");
        stocksPairs.forEach((pair) => {
            pair = pair.split(';');
            if (pair[0] && pair[1]) {
                stocks.push({id: pair[0], name: pair[1]});
            }
        });
        resolve(stocks);
    }).catch(reject);
});

parseStocks.then((stocks) => {
    saveStockData(0, stocks);
}).catch((err) => {
    console.log(err);
});

function saveStockData(stockIndex, stocks) {
    if (!stocks[stockIndex]) {
        return;
    }

    const stock = stocks[stockIndex];

    const loadStock = new Promise((resolve, reject) => {
        const stockUrl = getStockUrl(stock.id);
        console.log(stockUrl);

        request({method: "GET", url: stockUrl}, (err, res, data) => {
            if (err) {
                return reject(err);
            }

            fs.writeFile(__dirname + '/stocks/' + stock.id + ".txt", data, (err) => {
                return err ? reject(err) : resolve();
            })
        })
    });

    loadStock.then(() => {
        saveStockData(stockIndex + 1, stocks);
    }).catch((err) => {
        console.log(err);
    });
}

function getStockUrl(stockId) {
    return baseUrl + "/5/i/amstock/historydata.jsp?period=20&turnover=false&klid=" + stockId;
}
