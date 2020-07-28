
# Reliability-R

> Code to assist lifetime analisys.

> #Reliability #R #maintenance 

- Code
- examples
- issues
- downloads



[![INSERT YOUR GRAPHIC HERE](https://i.imgur.com/zWn0VWq.png)]()
---

## Example 
```

#Create a lifetime data
lifetime<- c(4,5,6,4,5,6,7,4,5,6,7,5,5,6,7,8)

#Checking which distribution better fit the data
plot_comp(lifetime,"Exemplo")

#checking the lifetime analisys
plot_rely(lifetime,"Exemplo")

#Finding the reliability on 7 days
Reliability_conf(lifetime,7)

#Finding the days that matches 0.7 of reliability
Reliability_dias(lifetime,0.7)
```
---


## Support

Reach out to me at one of the following places!

- Linkedin at <a href="https://www.linkedin.com/in/anderson-sales-26a380b5/?locale=pt_BR">`linkedin_suaveslife`</a>
- email at <a href="anderson_s@poli.ufrj.br">`anderson_s@poli.ufrj.br`</a>
