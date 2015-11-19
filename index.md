---
layout: default
---

{% include about.md %}
- - - 
{% include scores_mov.md %}
{% include scores_dat.md %}
- - -

## Clases, material y avisos

La lista completa de clases y material está en el [Archivo](https://felipegonzalez.github.io/aprendizaje_estadistico_2015/archivo/)


<ul class="post-list">
    {% for post in site.posts limit:5 %}
      <li>  <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }} ~</span> 
          <a class="post-link" href="{{ post.url | prepend: site.baseurl }}"> {{ post.title }}</a>
	{{ post.excerpt }}
      </li>
    {% endfor %}
</ul>



