# embedR

The `embedR` package is an open-source R package to generate and analyze text embeddings. It uses open and paid APIs from [Hugging Face](), [OpenAI](), and [Cohere]()and provides multiple options to generate, group, project, relabel, and visualize text embeddings.  

## General Information

The `text2sdg` package is developed by [Dirk U. Wulff](), with contributions from Samuel Aeschbach, Zak Hussain, and Rui Mata. It is published under the GNU General Public License.

An overview of the package can be accessed online or from within R using ?text2sdg.

# Installation

The latest development version on GitHub can be installed via `devtools::install_github("dwulff/embedR")`. Note that this requires prior installation of the `devtools` package.  

# Usage

```r
# vector of texts
texts = c("This is text 1", "This is text 2")

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
                        api = "openai")) %>% 
                        
  # visualize
  visualize()
```

## Citation

To be added.
