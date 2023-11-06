# embedR

The `embedR` package is an open-source R package to **generate and analyze text embeddings**. It gives access to open and paid APIs from [Hugging Face](https://huggingface.co/inference-api), [OpenAI](https://openai.com/blog/openai-api), and [Cohere](https://cohere.com/) to gnerate text embeddings and offers methods to group, project, relabel, and visualize them.  

## General Information

The `embedR` package is developed by [Dirk U. Wulff](https://github.com/dwulff), with contributions from [Samuel Aeschbach](https://samuelaeschbach.com/), [Zak Hussain](https://github.com/Zak-Hussain), and [Rui Mata](https://github.com/matarui). It is published under the GNU General Public License.

An overview of the package can be accessed online or from within R using ?embedR.

# Installation

The latest development version on GitHub can be installed via `devtools::install_github("dwulff/embedR")`. Note that this requires prior installation of the `devtools` package.  

# Caution

Use of this package can result in data protection violations. The package contains functions that send data to the servers of external APIs providers, including [Hugging Face](https://huggingface.co/inference-api), [OpenAI](https://openai.com/blog/openai-api), and [Cohere](https://cohere.com/). 


# Usage

```r
# load package
library(embedR)

# vector of texts
texts = c("This is text 1", "This is text 2", ...)

# set api tokens
er_set_token("openai" = "TOKEN",
             "huggingface" = "TOKEN",
             "cohere" = "TOKEN")

# generate embedding
embedding = texts %>% 

  # generate text embedding
  er_embed(api = "openai") 

# analyze embedding  
result = embedding %>% 

  # group similar texts
  er_group(method = "fuzzy") %>% 
  
  # generate 2D projection
  er_project(method = "umap") %>% 
  
  # cluster projection
  er_cluster(method = "louvain") %>% 
  
  # produce data frame
  er_frame()
  
# re-label text groups
result = embedding %>% 

  # relabel groups
  er_mutate(labels = label(group_texts, 
                           api = "openai"))
                        
# visualize
result %>% plot()
```

## Citation

To be added.
