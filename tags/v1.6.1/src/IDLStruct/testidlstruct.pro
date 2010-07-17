pro testidlstruct

    print,'testidlstruct'
    t = testidlstruct({a:35, b:[1.2, 3.3], c:66.2}, 'b')
    if not tag_exist(t, 'b') then message,'Error occured: tag "b" not found'
    help,t,/str


    print,'testidlstruct2'
    t = testidlstruct2({a:35, b:[1.2, 3.3], c:66.2})
    if not tag_exist(t, 'b') then message,'Error occured: tag "b" not found'
    help,t,/str
end
