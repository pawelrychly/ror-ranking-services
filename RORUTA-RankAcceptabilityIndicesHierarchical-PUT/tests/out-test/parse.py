from xml.dom import minidom
import decimal
xmldoc = minidom.parse('rank-acceptability-indices-hierarchical.xml')
cNodes = xmldoc.getElementsByTagName('alternativesValues') 
for node in cNodes:
	print node.attributes['id'].value
	alternatives = node.getElementsByTagName('alternativeValue')
	for alt in alternatives:
		id = alt.getElementsByTagName("alternativeID")[0].childNodes[0].nodeValue
		values = [ value.getElementsByTagName("real")[0].childNodes[0].nodeValue for value in alt.getElementsByTagName("values")[0].getElementsByTagName("value")]
		values = [str(round(decimal.Decimal(value),1)) for value in values]
		for i in range(len(values)):
			if float(values[i]) == 0:
				values[i] = ""
		print '{0}\t& {1} \\\\'.format(id, "\t& ".join(values))


	#variants = node.childNodes
	#for variant in variants:
	#variant.getElementsByTagName('alternativesValue') 
	
#print cNodes[0].toxml()
#itemlist = xmldoc.getElementsByTagName('alternativesValues') 
#for node in itemlist:
#	print node.attributes['id'].value
	#variants = node.childNodes
	#for variant in variants:
#		variant.getElementsByTagName('alternativesValue') 
		