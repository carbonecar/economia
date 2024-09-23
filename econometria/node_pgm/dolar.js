const fs = require("fs");
const dayjs = require("dayjs")
const customParseFormat = require("dayjs/plugin/customParseFormat");
const { parse } = require("csv-parse");

dayjs.extend(customParseFormat);


let rawdata = fs.readFileSync('dolar.json');
let dolar_data = JSON.parse(rawdata);



const cotizaciones = new Array();

for (let index = 0; index < dolar_data.length; index++) {
    month = dolar_data[index][0].split("-")[1];
    actualMonth = dolar_data[index][0].split("-")[1];
    do {

        element = dolar_data[index++];
        actualMonth = element[0].split("-")[1];
        if (month !== actualMonth) {
            cotizacion = (element[0] + ',' + '"' + element[1] + '"');
            cotizaciones.push(cotizacion);
        }

    } while (month === actualMonth && dolar_data.length > index)

}

console.log(JSON.stringify(cotizaciones.reverse()));