%g = inline('1.0 ./ (1.0 + exp(-z))'); 

file = fopen("Data.txt");
data = fscanf(file,"%f", [6,inf]);
y = data'(:,6);
x = [data'(:,1), data'(:,2), data'(:,3), data'(:,4), data'(:,5)];

x = [ones(size(x, 1),1) x];

m = size(x,1);
n = size(x,2);

theta = zeros(n,1);
old_th = ones(n,1);
number_of_interactions = 0;
in = (theta-old_th)'*(theta-old_th);

while ( in > 0.0000001 )
    %gradient
	++number_of_interactions
	
    gradient = zeros(n,1);
    for i=1:n,
        for j=1:m,
			h = (1/(1 + exp(-theta'*x(j,:)')));
		
            gradient(i,1) = gradient(i,1) + [y(j,1)-h]*x(j,i);
        end;
    end;
    %hessian
    H = zeros(n, n);
    for i=1:n,
        for j=1:n,
                for k=1:m,
					h= 1/(1 + exp(-theta'*x(k,:)'));
                    H(i,j) = H(i,j) -h*[1-h]*[x(k,i)]*[x(k,j)];
                end;
        end;
    end;
    old_th = theta;
    theta = theta - inv(H)*gradient;
    in = (theta-old_th)'*(theta-old_th);
end;
