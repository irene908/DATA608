d3.csv('https://raw.githubusercontent.com/irene908/CUNY_DATA_608/master/module6/d3_lab/ue_industry.csv', data => {
	
	// Define your scales and generator here.

    console.log(data);
              
    const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([20, 1180]);

                  
    const yScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.Agriculture))
        .range([580, 20]);   

                
                   
        var line = d3.line()
            .x(d => xScale(+d.index))
            .y(d => yScale(+d.Agriculture))
            .curve(d3.curveCardinal);
        
        d3.select('#answer1')
		// append more elements here
            .append('path')
            .attr('d', line(data))
            .attr('stroke', 'black')
    
});
