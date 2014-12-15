pro init_burp_objects

r=obj_new('burp_obj_rpt')
b=obj_new('burp_obj_block')
d=obj_new('burp_obj_file')

c=obj_new('arcad_obj_config')

obj_destroy,[r,b,d]
obj_destroy,c

end
