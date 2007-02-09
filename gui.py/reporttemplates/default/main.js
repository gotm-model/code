var showdefaults = false;
function showHideDefaults() {
	newdisplaystyle = '';
	newweight = 'bold';
	if (showdefaults) {
		newdisplaystyle = 'none';
		newweight = 'normal';
	}
	
	rows = document.all.tableScenario.rows;
	for (i=0; i<rows.length; i++) {
		tr = rows(i);
		if (tr.getAttribute('default')!=null)
			tr.style.display = newdisplaystyle;
		else
			tr.cells(tr.cells.length-1).style.fontWeight = newweight;
	}
	
	showdefaults = (!showdefaults);
	if (showdefaults) {
		document.all.aShowHideDefaults.innerHTML = 'Hide default values';
	} else {
		document.all.aShowHideDefaults.innerHTML = 'Show default values';
	}
}
