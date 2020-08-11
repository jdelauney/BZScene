<script>
$(window).resize(function(){
var height = $('.navigation').height() + 15;//take the header height
$('.content').css({'padding-top':height});//alter the margin of the wrapped content
}).trigger('resize');//trigger the margin resize when page is loaded
</script>