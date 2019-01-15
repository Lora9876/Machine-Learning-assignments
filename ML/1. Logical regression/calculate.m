function calculate(theta,data1,data2,data3,data4,data5)
data=[0,0,0,0,0];

data(1,2) = strcmp("M", data2);
data(1,3) = data3;
data(1,4) = strcmp("Car", data4);
data(1,5) = data5;

if(strcmp(data1,"Hospital readmission") == 1) data(1,1) = 0;
else if(strcmp(data1,"Other obligations") == 1) data(1,1) = 1; 
else if(strcmp(data1,"Resumed work") == 1) data(1,1) = 2;
else if(strcmp(data1,"Medical reasons") == 1) data(1,1) = 3;
else if(strcmp(data1,"Own facilities") == 1) data(1,1) = 4;
else if(strcmp(data1,"Lost interest") == 1) data(1,1) = 5;
else if(strcmp(data1,"Disliked therapist") == 1) data(1,1) = 6;
else if(strcmp(data1,"Forgot") == 1) data(1,1) = 7;
else if(strcmp(data1,"Moved") == 1) data(1,1) = 8;
else data(1,1) = 9;
end;end;end;end;end;end;end;end;
end;

data = [1, data] ;
z = dot(theta,data) ;
response = 1.0 ./ (1.0 +exp(-z)); 

end;
end;

disp(response);
