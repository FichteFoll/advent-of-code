## Equations
$$
a * a_y + b * b_y = p_y
a * a_x + b * b_x = p_x
$$

### Solve 1 for b

$$
a * a_y + b * b_y = p_y
b * b_y = p_y - a * a_y
b = (p_y - a * a_y) / b_y
$$

### Insert into 2 and solve for a

$$
a * a_x + b * b_x = p_x
a * a_x + ((p_y - a * a_y) / b_y) * b_x = p_x
a * a_x + p_y * b_x / b_y - a * a_y * b_x / b_y = p_x
a * a_x - a * a_y * b_x / b_y = p_x - p_y * b_x / b_y
a * (a_x - a_y * b_x / b_y) = p_x - p_y * b_x / b_y
a = (p_x - p_y * b_x / b_y) / (a_x - a_y * b_x / b_y)
a = (p_x * b_y - p_y * b_x) / (a_x * b_y - a_y * b_x)
$$
