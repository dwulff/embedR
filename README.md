# embedR

The `embedR` package is an open-source R package to generate and analyze text embeddings. It uses open and paid APIs from [Hugging Face](https://huggingface.co/inference-api), [OpenAI](https://openai.com/blog/openai-api), and [Cohere](https://cohere.com/) and offers various options to generate, group, project, relabel, and visualize text embeddings.  

## General Information

The `text2sdg` package is developed by [Dirk U. Wulff](https://github.com/dwulff), with contributions from [Samuel Aeschbach](https://samuelaeschbach.com/), [Zak Hussain](https://github.com/Zak-Hussain), and [Rui Mata](https://github.com/matarui). It is published under the GNU General Public License.

An overview of the package can be accessed online or from within R using ?embedR.

# Installation

The latest development version on GitHub can be installed via `devtools::install_github("dwulff/embedR")`. Note that this requires prior installation of the `devtools` package.  

# Caution

Use of this package can result in data security violations. The package involves function that send data to the servers of external APIs providers, including [Hugging Face](https://huggingface.co/inference-api), [OpenAI](https://openai.com/blog/openai-api), and [Cohere](https://cohere.com/). 


# Usage

```r
# load package
library(embedR)

# vector of texts
texts = c("This is text 1", "This is text 2")

# set api tokens
set_token("openai" = "TOKEN",
          "huggingface" = "TOKEN",
          "cohere" = "TOKEN")

# analyze
result = texts %>% 

  # generate text embedding
  embed(api = "openai") %>% 
  
  # group similar texts
  group(method = "fuzzy") %>% 
  
  # generate 2D projection
  project(method = "umap") %>% 
  
  # cluster projection
  cluster(method = "dbscan") %>% 
  
  # produce data frame
  frame() %>% 
  
  # relabel groups
  mutate(labels = label(group_texts, 
                        api = "openai"))
                        
# visualize
result %>% visualize()
```

## Citation

To be added.
