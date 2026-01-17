# PM Sensors

Inspired by a blog post [Air Quality Monitors: When Paying More Does Not Get You More Accuracy](https://www.airgradient.com/blog/expensive-air-quality-monitors-not-more-accurate/) by Achim Haug, this repository will compare various air quality monitors using data from [South Coast Air Quality Management District (SC-AQMD)](https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm).

# TODO

- [x] Scrape data from [SC-AQMD](https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm)
- [ ] Compare price to R^2^
- [ ] Show which sensors meet the Washington Wildfire Smoke Rule requirements for [Measuring PM~2.5~ levels at the workplace](https://apps.leg.wa.gov/WAC/default.aspx?cite=296-820&full=true#296-820-845).
  - [ ] Create R Shiny webapp that could be deployed, automatically updating acceptable sensors, sortable by price, R^2^, etc.
  - [ ] Create a Quarto document reporting which sensors meet the WA Wildfire Smoke Rule requirements
