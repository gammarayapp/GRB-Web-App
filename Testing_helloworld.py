import streamlit as st
import subprocess
from PIL import Image

st.header('🎈 R x Python Streamlit App')

st.sidebar.markdown('''
# About
This demo shows the use of R in a Streamlit App by showcasing 3 example use cases.

The R code for all 3 examples are rendered on-the-fly in this app.

R packages used:
- `ggplot2`
- `cowplot`
''')

st.subheader('1. Printing text in R')
with st.expander('See code'):
  code1 = '''print("Hello world ...")
  '''
  st.code(code1, language='R')
process1 = subprocess.Popen(["Rscript", "helloworld.R"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
result1 = process1.communicate()
st.write(result1)


st.subheader('2. Creating a plot using `ggplot2`')
with st.expander('See code'):
  code2 = '''library(ggplot2)

  ggplot(mtcars, aes(mpg, wt)) +
    geom_point()

  ggsave('plot.png')
  '''
  st.code(code2, language='R')
process2 = subprocess.Popen(["Rscript", "plot.R"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
result2 = process2.communicate()
image = Image.open('plot.png')
st.image(image)
st.caption('**Figure 1.** A simple scatter plot of *wt* as a function of *mpg* from the mtcars dataset.')
