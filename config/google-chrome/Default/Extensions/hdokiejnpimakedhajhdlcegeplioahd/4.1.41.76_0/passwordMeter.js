(function(g){var k=function(b,c,a){a=0;if(""===c&&""===b)return 0;var d=b.toLowerCase();c=c.toLowerCase();if(d===c||c&&-1!==d.indexOf(c)||c&&-1!==c.indexOf(d))return 1;a+=b.length;if(0<b.length&&7>=b.length)return 1;8<=b.length&&15>=b.length?a+=12:16<=b.length&&(a+=18);b.match(/[a-z]/)&&(a+=1);b.match(/[A-Z]/)&&(a+=5);b.match(/\d/)&&(a+=5);b.match(/.*\d.*\d.*\d/)&&(a+=5);b.match(/[!,@,#,$,%,^,&,*,?,_,~]/)&&(a+=5);b.match(/.*[!,@,#,$,%,^,&,*,?,_,~].*[!,@,#,$,%,^,&,*,?,_,~]/)&&(a+=5);b.match(/(?=.*[a-z])(?=.*[A-Z])/)&&
(a+=2);b.match(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)&&(a+=2);b.match(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[!,@,#,$,%,^,&,*,?,_,~])/)&&(a+=2);c={};for(var e=d=0,f=b.length;e<f;++e){var g=b.charAt(e);void 0===c[g]&&(c[g]=1,++d)}if(1===d)return 2;a*=2;0>a?a=0:100<a&&(a=100);return a},h=function(b,c,a,d){a=k(b,a,d);var e="poor",f=Strings.translateString("Invalid");17>a||(34>a?(e="bad",f=Strings.translateString("Weak")):51>a?(e="ok",f=Strings.translateString("Okay")):68>a?(e="good",f=Strings.translateString("Good")):
85>a?(e="great",f=Strings.translateString("Secure")):(e="best",f=Strings.translateString("Super!")));c.attr("class",e);c.css("width",a+"%");d&&(b?(g(d.parent()).css("opacity",1),d.text(f),d.removeClass("strength")):(g(d.parent()).css("opacity",.5),d.text(Strings.translateString("Strength")),c.attr("class","strength"),c.css("width","100%")))};jQuery.fn.LP_addGeneratePasswordMeter=function(){if(this&&0<this.length){var b=g(LPTools.createElement("div","meter-gen-pass")),c=g(LPTools.createElement("div"));
this.parent().append(b.append(c));b=function(){h(g(this).val(),c,"somestringthathopefullydoesnotmatchpassword")};this.on("input",b).on("change",b)}};jQuery.fn.LP_addPasswordMeter=function(b,c){var a=LPTools.createElement("div","meterContainer"),d=LPTools.createElement("div","meter"),e=LPTools.createElement("div"),f=null;d.appendChild(e);a.appendChild(d);d=g(d);e=g(e);c&&(f=LPTools.createElement("label","meterLabel","Strength"),a.appendChild(f),f=g(f),d.css("width","88%"),f.css("width","12%"));this.parent().append(a);
this.LP_input("passwordMeter",function(a){h(a,e,b?b.val():"",f)})}})(jQuery);