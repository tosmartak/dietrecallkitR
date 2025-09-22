
# dietrecallkit Package - R

<!-- badges: start -->
<!-- badges: end -->

**dietrecallkit** is a cross-platform package designed to enhance the
efficiency of researchers and analysts in cleaning, processing, and
calculating complex dietary and nutrient indicators from dietary recall
data. Built for both **Python** and **R**, it provides tools to
streamline workflows, ensuring accurate and reproducible results in
dietary and nutrition-related studies.

------------------------------------------------------------------------

## **Key Features**

- Efficient cleaning of data collected using the built in XLSForm
- Efficient computation of nutrient and dietary indicators.
- Compatible with **Python** and **R**, catering to a wide range of
  researchers. ([See Python
  Repository](https://github.com/tosmartak/dietrecallkitPy))
- Designed for reproducibility and accuracy in research.
- Simplifies complex calculations, saving time and effort.

------------------------------------------------------------------------

## **Who Is This For?**

- **Nutrition Researchers**: Calculate indicators for dietary
  assessments with ease.
- **Data Analysts**: Streamline workflows for nutrient analysis.
- **Academics and Students**: Learn and apply dietary metrics in a
  research-ready framework.

------------------------------------------------------------------------

## Installation in R

You can install the development version of dietrecallkit from
[GitHub](https://github.com/tosmartak/dietrecallkitR) with:

``` r
# install.packages("pak")
pak::pak("tosmartak/dietrecallkitR")
```

or install from R-CRAN with

``` r
install.packages("dietrecallkit")
```

## Quick Start

### Example

The package comes with a lightweight, relational example dataset called
dietrecall_example. This dataset contains three linked tables:
`maintable`, `food_details`, and `food_ingredients_group` similar to
what you would have when you used the dietary recall questionnaire from
the 24hour recall toolkit.

Note: the dataset is just a sample and not a real dataset and must be
loaded explicitly with data() since LazyData is set to false”. Ideally
this should be replaced with your own dataset

``` r
# Load the package after installation
library(dietrecallkit)

# Load the example dataset
data("dietrecall_example")

# Explore maintable
head(dietrecall_example$maintable)
#>           survey_id household_id survey_date recall_number  county subcounty
#> 1 0111251492-161123   0111251492  2023-11-16 Repeat recall NAIROBI  MAKADARA
#> 2 0111251492-241023   0111251492  2023-10-24  First recall NAIROBI  MAKADARA
#> 3 0111323284-291023   0111323284  2023-10-29  First recall NAIROBI  MAKADARA
#> 4 0111333496-161123   0111333496  2023-11-16 Repeat recall NAIROBI  MAKADARA
#> 5 0111333496-241023   0111333496  2023-10-24  First recall NAIROBI  MAKADARA
#> 6 0112183531-091123   0112183531  2023-11-09 Repeat recall NAIROBI  MAKADARA
#>        ward                cu mothers_age_in_years
#> 1 VIWANDANI            UCHUMI                   30
#> 2 VIWANDANI            UCHUMI                   30
#> 3 VIWANDANI      PARADISE TUI                   36
#> 4 VIWANDANI             DAIMA                   35
#> 5 VIWANDANI             DAIMA                   35
#> 6 VIWANDANI PARADISE ORIGINAL                   27

# Linked food details
head(dietrecall_example$food_details)
#>           survey_id food_details_rowid    food_item_selected
#> 1 0111251492-161123                  1                Orange
#> 2 0111251492-161123                  2 Managu/spinach/terere
#> 3 0111251492-161123                  3                 Ugali
#> 4 0111251492-161123                  4                 Kales
#> 5 0111251492-161123                  5                 Ugali
#> 6 0111251492-161123                  6                 Bread
#>   food_preparation_place ready_to_eat                desc_of_food
#> 1           Outside Home           NA Orange (chungwa), pulp, raw
#> 2                   Home            0                        <NA>
#> 3                   Home            0                        <NA>
#> 4                   Home            0                        <NA>
#> 5                   Home            0                        <NA>
#> 6           Outside Home           NA                Bread, White
#>                dish_foodgroup food_consumption_place food_cooking_method
#> 1                Other fruits           Outside Home                <NA>
#> 2                        <NA>                   Home         Stir frying
#> 3                        <NA>                   Home             Boiling
#> 4                        <NA>                   Home         Stir frying
#> 5                        <NA>                   Home             Boiling
#> 6 Cereals and Cereal Products                   Home                <NA>
#>   food_cooking_method_other amt_of_food_cooked unit_amt_of_food_cooked
#> 1                        NA                 NA                    <NA>
#> 2                        NA                316                     mls
#> 3                        NA                760        g from photobook
#> 4                        NA                208                     mls
#> 5                        NA                760        g from photobook
#> 6                        NA                 NA                    <NA>
#>   qty_food_consumed unit_qty_food_consumed food_item_price_prop_consumed
#> 1                10            Price (kes)                             1
#> 2                69                    mls                            NA
#> 3               285       g from photobook                            NA
#> 4                69                    mls                            NA
#> 5               380       g from photobook                            NA
#> 6               104       g from photobook                            NA
#>                                rowuuid
#> 1 d84a66a5-21f7-4eb6-92f2-0b776087b41a
#> 2 a012b128-567d-46da-a5aa-b2cfd96b5c0f
#> 3 3038d9ef-7e1b-4674-ab97-1ba5b595d114
#> 4 5c9cf62b-5a1a-4dff-b2c8-c5b505191f9c
#> 5 cfec0b9c-1eb6-4808-9449-6598481a4118
#> 6 27b5c807-b11c-4c66-a449-061f95f28f06
```

------------------------------------------------------------------------

## **Contributing**

Contributions are welcome! If you’d like to contribute, please: 1. Fork
the repository. 2. Create a feature branch
(`git checkout -b feature-name`). 3. Commit your changes
(`git commit -m 'Add a new feature'`). 4. Push to the branch
(`git push origin feature-name`). 5. Create a pull request.

------------------------------------------------------------------------

## **License**

`dietrecallkit` is licensed under the [GNU License](LICENSE).

------------------------------------------------------------------------

## **Acknowledgments**

Special thanks to all the contributors and users who have supported the
development of this package.

------------------------------------------------------------------------

## **Contact**

For questions or feedback, please reach out at
\[<t.akingbemisilu@cgiar.org>\].
