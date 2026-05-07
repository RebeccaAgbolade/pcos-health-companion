# ============================================================
# PCOS HEALTH COMPANION
# A personalised PCOS assessment, subtype identification
# and management planning tool for women
# Built by: Rebecca Agbolade
# Date: May 2026
# ============================================================
# DISCLAIMER:
# This app is for informational purposes only and does not
# constitute medical advice. Always consult your GP or
# specialist before making changes to your health routine.
# ============================================================

# Load libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(DT)

# ============================================================
# CUSTOM CSS - BLOOM THEME
# ============================================================

bloom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;600;700&family=Lato:wght@300;400;700&display=swap');

  body {
    background-color: #FFF8F8;
    font-family: 'Lato', sans-serif;
    color: #2D2D2D;
  }

  h1, h2, h3, h4 {
    font-family: 'Playfair Display', serif;
    color: #C2185B;
  }

  .navbar {
    background: linear-gradient(135deg, #C2185B, #AD1457) !important;
    border: none !important;
    box-shadow: 0 2px 10px rgba(194,24,91,0.3);
  }

  .navbar-brand {
    font-family: 'Playfair Display', serif !important;
    font-size: 22px !important;
    color: white !important;
    font-weight: 700 !important;
  }

  .navbar-nav > li > a {
    color: rgba(255,255,255,0.9) !important;
    font-family: 'Lato', sans-serif !important;
    font-weight: 600 !important;
    font-size: 14px !important;
  }

  .navbar-nav > li > a:hover {
    color: white !important;
    background-color: rgba(255,255,255,0.15) !important;
    border-radius: 6px !important;
  }

  .navbar-nav > .active > a {
    background-color: rgba(255,255,255,0.2) !important;
    color: white !important;
    border-radius: 6px !important;
  }

  .card {
    background: white;
    border-radius: 16px;
    padding: 24px;
    margin-bottom: 20px;
    box-shadow: 0 2px 15px rgba(194,24,91,0.08);
    border: 1px solid #FCE4EC;
  }

  .hero-banner {
    background: linear-gradient(135deg, #C2185B 0%, #AD1457 50%, #880E4F 100%);
    border-radius: 16px;
    padding: 40px;
    margin-bottom: 28px;
    color: white;
    text-align: center;
  }

  .hero-banner h1 {
    color: white !important;
    font-size: 32px;
    margin-bottom: 10px;
  }

  .hero-banner p {
    color: rgba(255,255,255,0.85);
    font-size: 16px;
    margin: 0;
  }

  .btn-primary {
    background: linear-gradient(135deg, #C2185B, #AD1457) !important;
    border: none !important;
    border-radius: 25px !important;
    padding: 12px 32px !important;
    font-family: 'Lato', sans-serif !important;
    font-weight: 700 !important;
    font-size: 15px !important;
    color: white !important;
    box-shadow: 0 4px 15px rgba(194,24,91,0.3) !important;
    transition: all 0.3s ease !important;
  }

  .btn-primary:hover {
    transform: translateY(-2px) !important;
    box-shadow: 0 6px 20px rgba(194,24,91,0.4) !important;
  }

  .btn-success {
    background: linear-gradient(135deg, #81C784, #66BB6A) !important;
    border: none !important;
    border-radius: 25px !important;
    padding: 10px 24px !important;
    font-weight: 700 !important;
    color: white !important;
  }

  .result-card {
    border-radius: 16px;
    padding: 28px;
    margin: 16px 0;
    text-align: center;
    color: white;
  }

  .result-card.insulin {
    background: linear-gradient(135deg, #E57373, #EF5350);
  }

  .result-card.inflammatory {
    background: linear-gradient(135deg, #64B5F6, #42A5F5);
  }

  .result-card.postpill {
    background: linear-gradient(135deg, #81C784, #66BB6A);
  }

  .result-card.androgen {
    background: linear-gradient(135deg, #FFB74D, #FFA726);
  }

  .result-card h2 {
    color: white !important;
    font-size: 26px;
    margin-bottom: 8px;
  }

  .result-card p {
    color: rgba(255,255,255,0.9);
    font-size: 15px;
    margin: 0;
  }

  .stat-box {
    background: white;
    border-radius: 12px;
    padding: 20px;
    text-align: center;
    box-shadow: 0 2px 10px rgba(194,24,91,0.08);
    border-top: 4px solid #C2185B;
    margin-bottom: 16px;
  }

  .stat-box h3 {
    font-size: 32px;
    color: #C2185B !important;
    margin: 8px 0;
  }

  .stat-box p {
    color: #888;
    font-size: 13px;
    margin: 0;
  }

  .plan-section {
    background: #FFF8F8;
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 16px;
    border-left: 4px solid #C2185B;
  }

  .plan-section h4 {
    color: #C2185B !important;
    margin-bottom: 12px;
  }

  .phase-item {
    background: white;
    border-radius: 10px;
    padding: 16px;
    margin-bottom: 12px;
    border-left: 4px solid #F8BBD9;
    box-shadow: 0 1px 6px rgba(194,24,91,0.06);
  }

  .phase-item h5 {
    color: #AD1457 !important;
    margin-bottom: 6px;
    font-family: 'Playfair Display', serif;
  }

  .disclaimer-box {
    background: #FFF3E0;
    border-radius: 12px;
    padding: 16px 20px;
    border-left: 4px solid #FFB74D;
    margin: 20px 0;
    font-size: 13px;
    color: #666;
  }

  .consent-box {
    background: #F3E5F5;
    border-radius: 12px;
    padding: 16px 20px;
    border-left: 4px solid #C2185B;
    margin: 20px 0;
    font-size: 13px;
    color: #444;
  }

  .question-card {
    background: white;
    border-radius: 12px;
    padding: 20px 24px;
    margin-bottom: 14px;
    box-shadow: 0 2px 8px rgba(194,24,91,0.06);
    border: 1px solid #FCE4EC;
  }

  .question-card label {
    font-weight: 600;
    color: #2D2D2D;
    font-size: 15px;
  }

  .progress-bar {
    background: linear-gradient(90deg, #C2185B, #F06292) !important;
  }

  .period-log {
    background: white;
    border-radius: 12px;
    padding: 20px;
    margin-bottom: 16px;
    box-shadow: 0 2px 8px rgba(194,24,91,0.06);
  }

  .cycle-stat {
    background: linear-gradient(135deg, #FCE4EC, #F8BBD9);
    border-radius: 10px;
    padding: 16px;
    text-align: center;
    margin-bottom: 12px;
  }

  .cycle-stat h3 {
    color: #C2185B !important;
    font-size: 28px;
    margin: 6px 0;
  }

  .gp-letter {
    background: white;
    border-radius: 12px;
    padding: 32px;
    font-family: 'Lato', sans-serif;
    line-height: 1.8;
    box-shadow: 0 2px 15px rgba(194,24,91,0.08);
    border: 1px solid #FCE4EC;
  }

  .footer {
    text-align: center;
    padding: 20px;
    color: #aaa;
    font-size: 12px;
    margin-top: 40px;
    border-top: 1px solid #FCE4EC;
  }

  select, input[type='text'], input[type='number'], input[type='date'] {
    border-radius: 8px !important;
    border: 1px solid #F8BBD9 !important;
    padding: 8px 12px !important;
  }

  select:focus, input:focus {
    border-color: #C2185B !important;
    box-shadow: 0 0 0 3px rgba(194,24,91,0.1) !important;
    outline: none !important;
  }

  .shiny-input-radiogroup label {
    font-weight: 400 !important;
  }

  .well {
    background: white !important;
    border: 1px solid #FCE4EC !important;
    border-radius: 12px !important;
  }
"

# ============================================================
# MANAGEMENT PLANS DATA
# ============================================================

management_plans <- list(
  
  "Insulin Resistant PCOS" = list(
    colour = "insulin",
    emoji = "🍎",
    description = "Your symptom profile most closely matches Insulin Resistant PCOS. This is the most common type and is driven by your body's reduced ability to respond to insulin, leading to higher androgen production.",
    
    nutrition = c(
      "Eat low glycaemic index foods like oats, lentils, quinoa, and sweet potato",
      "Avoid refined sugar, white bread, white rice, and processed foods",
      "Eat every 3 to 4 hours to keep blood sugar stable",
      "Include inositol rich foods like citrus fruits, beans, and whole grains",
      "Add cinnamon to meals, it improves insulin sensitivity naturally",
      "Prioritise protein at every meal to slow glucose absorption",
      "Limit fruit juice and opt for whole fruits instead"
    ),
    
    exercise = c(
      "Strength training 3 times per week is the most effective exercise for insulin resistance",
      "Walk for 30 minutes after meals to lower blood sugar spikes",
      "Try resistance bands or body weight exercises if you are new to strength training",
      "Avoid excessive cardio as it can spike cortisol and worsen symptoms",
      "Yoga twice a week to support stress reduction and cortisol balance",
      "Aim for 150 minutes of moderate activity per week"
    ),
    
    supplements = c(
      "Myo-Inositol 2000mg twice daily, clinically shown to improve insulin sensitivity",
      "Magnesium glycinate 300mg at night, supports blood sugar regulation and sleep",
      "Vitamin D 2000IU daily, most PCOS women are deficient",
      "Chromium picolinate 200mcg, helps regulate blood sugar",
      "Berberine 500mg with meals, similar effect to Metformin in studies",
      "Omega 3 fish oil 2000mg daily, reduces inflammation"
    ),
    
    medications = c(
      "Metformin is most commonly prescribed for this type and improves insulin sensitivity",
      "Your GP may also consider Letrozole or Clomid if you are trying to conceive",
      "Combined oral contraceptive pill to regulate periods if not trying to conceive",
      "Discuss progesterone supplementation if you have had miscarriages"
    ),
    
    phases = list(
      list(time = "Week 1 to 4", title = "Foundation",
           description = "Remove refined sugar and processed carbs. Start walking daily. Begin Myo-Inositol. You may notice reduced bloating and more stable energy."),
      list(time = "Month 2 to 3", title = "Building Momentum",
           description = "Introduce strength training. Track your cycle. Blood sugar cravings should reduce noticeably. Some women see their first more regular period."),
      list(time = "Month 3 to 6", title = "Visible Changes",
           description = "Expect more regular cycles, gradual weight stabilisation, clearer skin, and reduced hair growth rate. Energy levels significantly improved."),
      list(time = "Month 6 Plus", title = "Hormonal Shift",
           description = "Blood tests should show improving insulin levels, reducing androgens, and more balanced FSH and LH ratio. Fertility window becoming more predictable.")
    )
  ),
  
  "Inflammatory PCOS" = list(
    colour = "inflammatory",
    emoji = "🫐",
    description = "Your symptom profile most closely matches Inflammatory PCOS. This type is driven by chronic low grade inflammation in the body which triggers androgen overproduction. Lifestyle and diet are powerful tools for this type.",
    
    nutrition = c(
      "Follow a strict anti inflammatory Mediterranean style diet",
      "Eat fatty fish like salmon, mackerel, and sardines at least 3 times per week",
      "Add turmeric and ginger to meals daily, powerful natural anti inflammatories",
      "Eliminate gluten and dairy for 8 weeks as a trial to identify triggers",
      "Load up on colourful vegetables especially leafy greens, berries, and beetroot",
      "Avoid processed vegetable oils and replace with olive oil and coconut oil",
      "Reduce alcohol completely as it drives inflammation"
    ),
    
    exercise = c(
      "Start gently with walking, swimming, and yoga until inflammation reduces",
      "Avoid high intensity exercise initially as it can worsen inflammation",
      "Prioritise sleep above all else, poor sleep is a major inflammatory driver",
      "Try tai chi or gentle pilates for stress and inflammation reduction",
      "Once symptoms improve after 6 to 8 weeks gradually introduce moderate cardio",
      "Cold water exposure like cool showers may help reduce systemic inflammation"
    ),
    
    supplements = c(
      "Omega 3 fish oil 3000mg daily, the most important supplement for this type",
      "Vitamin D 3000IU daily, low Vitamin D drives inflammation",
      "N-Acetyl Cysteine 600mg twice daily, powerful antioxidant and anti inflammatory",
      "Curcumin with black pepper 500mg daily, active compound in turmeric",
      "Magnesium glycinate 400mg at night, reduces inflammatory markers",
      "Zinc 30mg daily, supports immune regulation and reduces inflammation"
    ),
    
    medications = c(
      "Your GP may consider anti inflammatory medication short term",
      "Thyroid medication if your TSH levels are elevated",
      "Combined oral contraceptive pill to reduce androgen driven symptoms",
      "Discuss low dose naltrexone with your specialist, emerging evidence for inflammatory PCOS"
    ),
    
    phases = list(
      list(time = "Week 1 to 4", title = "Reduce the Fire",
           description = "Eliminate inflammatory foods. Start Omega 3 and Vitamin D. Prioritise 8 hours sleep. You may notice reduced bloating, joint pain, and fatigue."),
      list(time = "Month 2 to 3", title = "Body Responding",
           description = "Energy levels improving. Skin starting to clear. Introduce gentle exercise. Some women notice reduced period pain and slightly more regular cycles."),
      list(time = "Month 3 to 6", title = "Symptoms Shifting",
           description = "Clearer skin, reduced hair growth, more predictable cycles. Inflammatory markers in blood tests beginning to normalise."),
      list(time = "Month 6 Plus", title = "Sustained Improvement",
           description = "Hormonal balance improving significantly. Many women in this group see the most dramatic transformation when inflammation is properly addressed.")
    )
  ),
  
  "Androgen Excess PCOS" = list(
    colour = "androgen",
    emoji = "🌸",
    description = "Your symptom profile most closely matches Androgen Excess PCOS. This type is characterised by elevated male hormones causing physical symptoms like hair growth, hair loss, and acne. You may have a normal BMI which can make diagnosis harder.",
    
    nutrition = c(
      "Drink spearmint tea twice daily, clinically shown to reduce testosterone levels",
      "Avoid dairy products which can increase IGF-1 and worsen androgen symptoms",
      "Eat high zinc foods like pumpkin seeds, chickpeas, and cashews daily",
      "Include flaxseeds daily as they help bind and excrete excess androgens",
      "Reduce red meat and replace with plant proteins and fish",
      "Eat cruciferous vegetables like broccoli and cauliflower to support hormone detox",
      "Avoid soy products which can interfere with hormonal balance"
    ),
    
    exercise = c(
      "Moderate cardio like swimming, cycling, and dancing works best for this type",
      "Avoid very high intensity exercise which can raise androgen levels further",
      "Yoga and pilates are particularly beneficial for stress and hormone balance",
      "Aim for 30 to 45 minutes of moderate exercise 5 days per week",
      "Include relaxation practices as cortisol drives androgen production",
      "Try cycle syncing your workouts to your menstrual cycle phases"
    ),
    
    supplements = c(
      "Spearmint extract 900mg daily if you do not drink the tea",
      "Zinc 30mg daily, one of the most effective supplements for androgen excess",
      "Saw Palmetto 320mg daily, blocks DHT conversion which causes hair loss",
      "Vitamin B6 50mg daily, supports hormone metabolism",
      "DIM 200mg daily, helps balance oestrogen and androgen ratio",
      "Reishi mushroom extract, adaptogen that supports hormonal balance"
    ),
    
    medications = c(
      "Spironolactone is the most effective medication for androgen excess symptoms",
      "Combined oral contraceptive pill with anti androgenic progestin",
      "Finasteride for hair loss in severe cases, discuss with your GP",
      "Note: Spironolactone cannot be taken if you are trying to conceive"
    ),
    
    phases = list(
      list(time = "Week 1 to 4", title = "Starting the Shift",
           description = "Start spearmint tea twice daily. Remove dairy. Begin zinc and Saw Palmetto. Hair growth rate may begin slowing within 4 to 6 weeks."),
      list(time = "Month 2 to 3", title = "Skin and Hair Responding",
           description = "Acne should begin clearing noticeably. Hair growth slowing. Energy and mood improving. Cycles may become slightly more regular."),
      list(time = "Month 3 to 6", title = "Visible Transformation",
           description = "Significant reduction in unwanted hair growth. Skin much clearer. Hair loss stabilising. Testosterone levels reducing in blood tests."),
      list(time = "Month 6 Plus", title = "Hormonal Balance",
           description = "Androgen levels normalising. Many women report feeling like themselves again. Cycles regular and predictable. Hair growing back in areas of loss.")
    )
  ),
  
  "Post Pill PCOS" = list(
    colour = "postpill",
    emoji = "🌿",
    description = "Your symptom profile most closely matches Post Pill PCOS. This type often develops after stopping hormonal contraception and is usually temporary. Your body is recalibrating its own hormone production which was suppressed by the pill.",
    
    nutrition = c(
      "Eat liver supporting foods like cruciferous vegetables, beetroot, and artichoke",
      "Include high fibre foods to support oestrogen elimination from the body",
      "Eat Brazil nuts daily for selenium which supports thyroid and hormone balance",
      "Avoid alcohol completely as your liver needs to focus on hormone clearance",
      "Include fermented foods like kefir and sauerkraut to support gut microbiome",
      "Eat phytoestrogen rich foods like flaxseeds to help balance oestrogen",
      "Stay well hydrated as water supports hormone detoxification"
    ),
    
    exercise = c(
      "Moderate consistent exercise works best, avoid extreme highs and lows",
      "Walking daily is the most supportive exercise during hormonal recalibration",
      "Yoga is particularly helpful for HPA axis regulation and stress response",
      "Cycle syncing your workouts to your returning cycle is very effective",
      "Avoid overexercising as excessive exercise can delay cycle return",
      "Prioritise rest and recovery as your body is doing significant internal work"
    ),
    
    supplements = c(
      "Vitex Agnus Castus 400mg daily, most studied supplement for post pill hormone balance",
      "Vitamin B6 50mg daily, depleted by the pill and essential for hormone production",
      "Magnesium glycinate 300mg at night, also depleted by the pill",
      "Zinc 25mg daily, supports ovulation and hormone production",
      "Probiotics with Lactobacillus strains, restore gut microbiome affected by the pill",
      "Folate 400mcg daily, especially important if trying to conceive"
    ),
    
    medications = c(
      "This type usually resolves naturally within 6 to 12 months without medication",
      "Your GP may monitor hormone levels quarterly to track recovery",
      "If trying to conceive Letrozole may be considered to stimulate ovulation",
      "Discuss progesterone supplementation in early pregnancy if you have had miscarriages"
    ),
    
    phases = list(
      list(time = "Month 1 to 2", title = "Supporting Recovery",
           description = "Start Vitex and B vitamins. Support liver detoxification. Your body is clearing synthetic hormones. Be patient, this phase requires rest and nourishment."),
      list(time = "Month 2 to 4", title = "First Signs",
           description = "Cycle may begin returning, even if irregular. Track everything with the period tracker. Energy improving. Some women see their first natural period."),
      list(time = "Month 4 to 8", title = "Recalibrating",
           description = "Hormones recalibrating steadily. Cycles becoming more regular and predictable. Skin and mood improving as oestrogen and progesterone balance returns."),
      list(time = "Month 8 to 12", title = "Full Restoration",
           description = "Most women see full cycle restoration by month 12. Hormone levels normalising in blood tests. Many women successfully conceive naturally in this window.")
    )
  )
)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(tags$style(HTML(bloom_css))),
  
  # App header
  div(class = "hero-banner",
      h1("🌸 PCOS Health Companion"),
      p("Your personalised PCOS assessment, subtype identification and management planning tool"),
      p(style = "font-size: 13px; margin-top: 8px; opacity: 0.7;",
        "Built with real patient data | For informational purposes only")
  ),
  
  navbarPage(
    title = "🌸 PCOS Health Companion",
    theme = shinytheme("flatly"),
    
    # ============================================================
    # TAB 1 - ASSESSMENT
    # ============================================================
    tabPanel("📋 Assessment",
             fluidRow(
               column(8, offset = 2,
                      
                      div(class = "card",
                          h3("Your PCOS Assessment"),
                          p("Answer the questions below honestly based on your experience
                            over the last 3 months. This takes about 3 minutes."),
                          div(class = "disclaimer-box",
                              "⚠️ This assessment is for informational purposes only and
                              is not a medical diagnosis. Please share your results with your GP."
                          )
                      ),
                      
                      # Personal details
                      div(class = "card",
                          h4("About You"),
                          
                          div(class = "question-card",
                              numericInput("age", "How old are you?",
                                           value = 25, min = 13, max = 60)
                          ),
                          
                          div(class = "question-card",
                              numericInput("bmi",
                                           "What is your BMI? (Weight in kg divided by height in metres squared)",
                                           value = 22, min = 14, max = 50)
                          ),
                          
                          div(class = "question-card",
                              radioButtons("family_history",
                                           "Does your mother, sister or aunt have PCOS or very irregular periods?",
                                           choices = c("Yes" = "yes", "No" = "no", "Not sure" = "unsure"),
                                           inline = TRUE)
                          ),
                          
                          div(class = "question-card",
                              radioButtons("miscarriage",
                                           "Have you experienced one or more miscarriages?",
                                           choices = c("Yes" = "yes", "No" = "no",
                                                       "Prefer not to say" = "na"),
                                           inline = TRUE)
                          ),
                          
                          div(class = "question-card",
                              radioButtons("trying_conceive",
                                           "Are you currently trying to conceive?",
                                           choices = c("Yes" = "yes", "No" = "no",
                                                       "Not right now" = "maybe"),
                                           inline = TRUE)
                          ),
                          
                          div(class = "question-card",
                              radioButtons("pill_history",
                                           "Have you recently stopped hormonal contraception in the last 12 months?",
                                           choices = c("Yes" = "yes", "No" = "no"),
                                           inline = TRUE)
                          )
                      ),
                      
                      # Cycle questions
                      div(class = "card",
                          h4("Your Cycle"),
                          
                          div(class = "question-card",
                              radioButtons("cycle_regular",
                                           "How would you describe your periods?",
                                           choices = c(
                                             "Regular, every 21 to 35 days" = "regular",
                                             "Irregular, unpredictable timing" = "irregular",
                                             "Very infrequent, fewer than 8 per year" = "infrequent",
                                             "Absent for 3 or more months" = "absent"
                                           ))
                          ),
                          
                          div(class = "question-card",
                              numericInput("cycle_length",
                                           "If you know it, what is your average cycle length in days?",
                                           value = 28, min = 15, max = 90)
                          )
                      ),
                      
                      # Symptoms
                      div(class = "card",
                          h4("Your Symptoms"),
                          p("Rate how much each symptom affects you (0 = not at all, 5 = severely):"),
                          
                          div(class = "question-card",
                              sliderInput("weight_gain",
                                          "Unexplained weight gain or difficulty losing weight",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("hair_growth", "Unwanted facial or body hair growth",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("hair_loss", "Hair thinning or loss on the scalp",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("skin_darkening",
                                          "Skin darkening especially on neck, underarms or groin",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("pimples",
                                          "Acne or persistent breakouts especially on chin and jaw",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("fatigue", "Chronic fatigue or low energy",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          ),
                          
                          div(class = "question-card",
                              sliderInput("bloating", "Bloating or digestive issues",
                                          min = 0, max = 5, value = 0, step = 1, ticks = FALSE)
                          )
                      ),
                      
                      # Lifestyle
                      div(class = "card",
                          h4("Your Lifestyle"),
                          
                          div(class = "question-card",
                              radioButtons("fast_food",
                                           "How often do you eat fast food or highly processed food?",
                                           choices = c(
                                             "Rarely or never" = 0,
                                             "Once or twice a week" = 1,
                                             "Most days" = 2,
                                             "Every day" = 3
                                           ))
                          ),
                          
                          div(class = "question-card",
                              radioButtons("exercise_freq",
                                           "How often do you exercise?",
                                           choices = c(
                                             "Rarely or never" = 0,
                                             "Once or twice a week" = 1,
                                             "Three to four times a week" = 2,
                                             "Five or more times a week" = 3
                                           ))
                          ),
                          
                          div(class = "question-card",
                              radioButtons("stress_level",
                                           "How would you rate your average stress level?",
                                           choices = c(
                                             "Low, generally calm" = 0,
                                             "Moderate, manageable" = 1,
                                             "High, often overwhelmed" = 2,
                                             "Very high, chronic stress" = 3
                                           ))
                          )
                      ),
                      
                      # Consent section
                      div(class = "card",
                          h4("🔒 Privacy & Consent"),
                          
                          div(class = "consent-box",
                              p("Your privacy matters to us. Please read and confirm the following
                                before submitting your assessment.")
                          ),
                          
                          div(class = "question-card",
                              checkboxInput("consent_data",
                                            "✅ I consent to my anonymous responses being used to
                                            improve this tool. No personal identifying information
                                            is stored. My name, email or location are never collected.
                                            My responses cannot be traced back to me.",
                                            value = FALSE)
                          ),
                          
                          div(class = "question-card",
                              checkboxInput("consent_terms",
                                            "✅ I understand this tool is for informational purposes
                                            only and is not a substitute for medical advice from a
                                            qualified healthcare professional. I will consult my GP
                                            before making any changes to my health routine.",
                                            value = FALSE)
                          )
                      ),
                      
                      # Submit button
                      div(style = "text-align: center; margin: 30px 0;",
                          uiOutput("submit_button_ui")
                      ),
                      
                      # Results section
                      uiOutput("assessment_results")
               )
             )
    ),
    
    # ============================================================
    # TAB 2 - MANAGEMENT PLAN
    # ============================================================
    tabPanel("💊 My Plan",
             fluidRow(
               column(8, offset = 2,
                      uiOutput("management_plan_ui")
               )
             )
    ),
    
    # ============================================================
    # TAB 3 - PERIOD TRACKER
    # ============================================================
    tabPanel("📅 Period Tracker",
             fluidRow(
               column(8, offset = 2,
                      
                      div(class = "card",
                          h3("Period Tracker"),
                          p("Log your periods to track your cycle length and identify patterns.")
                      ),
                      
                      div(class = "card",
                          h4("Log a Period"),
                          fluidRow(
                            column(6,
                                   div(class = "question-card",
                                       dateInput("period_start", "Period Start Date",
                                                 value = Sys.Date())
                                   )
                            ),
                            column(6,
                                   div(class = "question-card",
                                       dateInput("period_end", "Period End Date",
                                                 value = Sys.Date())
                                   )
                            )
                          ),
                          div(style = "text-align: center; margin-top: 10px;",
                              actionButton("log_period", "Log Period 📅", class = "btn-primary")
                          )
                      ),
                      
                      uiOutput("cycle_stats"),
                      
                      div(class = "card",
                          h4("Period History"),
                          DTOutput("period_table")
                      )
               )
             )
    ),
    
    # ============================================================
    # TAB 4 - GP LETTER
    # ============================================================
    tabPanel("📄 GP Letter",
             fluidRow(
               column(8, offset = 2,
                      uiOutput("gp_letter_ui")
               )
             )
    ),
    
    # ============================================================
    # TAB 5 - ABOUT PCOS
    # ============================================================
    tabPanel("ℹ️ About PCOS",
             fluidRow(
               column(8, offset = 2,
                      
                      div(class = "card",
                          h3("Understanding PCOS"),
                          p("Polycystic Ovary Syndrome affects 1 in 10 women of reproductive age.
                            It is one of the most common hormonal conditions yet it takes an average
                            of 2 years to get a diagnosis. Understanding your type is the first step
                            to managing it effectively.")
                      ),
                      
                      div(class = "result-card insulin",
                          h2("🍎 Insulin Resistant PCOS"),
                          p("The most common type, affecting around 70% of PCOS women. Driven by
                            the body's reduced ability to respond to insulin, leading to higher
                            androgen production. Key signs: weight gain, sugar cravings,
                            skin darkening, irregular periods.")
                      ),
                      
                      div(class = "result-card inflammatory",
                          h2("🫐 Inflammatory PCOS"),
                          p("Driven by chronic low grade inflammation which triggers androgen
                            overproduction. Key signs: fatigue, digestive issues, skin problems,
                            joint pain, brain fog. This type responds dramatically to dietary changes.")
                      ),
                      
                      div(class = "result-card androgen",
                          h2("🌸 Androgen Excess PCOS"),
                          p("Characterised by elevated male hormones causing visible physical symptoms.
                            Often seen in women with normal BMI which can make diagnosis harder.
                            Key signs: facial hair, hair loss, acne especially on chin and jaw.")
                      ),
                      
                      div(class = "result-card postpill",
                          h2("🌿 Post Pill PCOS"),
                          p("Develops after stopping hormonal contraception. Usually temporary as
                            the body recalibrates its own hormone production. Key signs: cycle
                            disruption, hormone imbalance, symptoms appearing after stopping the pill.")
                      ),
                      
                      div(class = "card",
                          h4("PCOS and Fertility"),
                          p("PCOS is a leading cause of infertility but most women with PCOS can
                            and do get pregnant. Women with PCOS have a 30 to 50% higher risk of
                            miscarriage, often related to high LH levels or insulin resistance.
                            Treating the underlying PCOS type significantly improves pregnancy outcomes."),
                          br(),
                          h4("PCOS and Family History"),
                          p("Women with a mother or sister with PCOS are 2 to 3 times more likely
                            to develop it. Around 50% of first degree female relatives show some
                            hormonal features of PCOS. If PCOS runs in your family, early monitoring
                            is recommended."),
                          br(),
                          h4("Trusted Resources"),
                          tags$ul(
                            tags$li(tags$a("Verity PCOS Charity UK",
                                           href = "https://www.verity-pcos.org.uk",
                                           target = "_blank")),
                            tags$li(tags$a("NHS PCOS Information",
                                           href = "https://www.nhs.uk/conditions/polycystic-ovary-syndrome-pcos/",
                                           target = "_blank")),
                            tags$li(tags$a("PCOS Awareness Association",
                                           href = "https://www.pcosaa.org",
                                           target = "_blank"))
                          )
                      ),
                      
                      div(class = "footer",
                          p("Built with ❤️ by Rebecca Agbolade | Data Source: Kaggle PCOS Dataset"),
                          p("This app is for informational purposes only. Always consult your GP.")
                      )
               )
             )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    pcos_type = NULL,
    pcos_score = NULL,
    periods = data.frame(
      Start = as.Date(character()),
      End = as.Date(character()),
      Duration = numeric(),
      Cycle_Length = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # ============================================================
  # SUBMIT BUTTON - only active when terms accepted
  # ============================================================
  
  output$submit_button_ui <- renderUI({
    if(isTRUE(input$consent_terms)) {
      actionButton("submit", "Get My Results 🌸",
                   class = "btn-primary",
                   style = "font-size: 18px; padding: 16px 48px;")
    } else {
      div(
        tags$button("Please accept the terms above to continue",
                    class = "btn btn-primary",
                    style = "font-size: 16px; padding: 16px 48px;
                             opacity: 0.5; cursor: not-allowed;",
                    disabled = NA)
      )
    }
  })
  
  # ============================================================
  # ASSESSMENT LOGIC
  # ============================================================
  
  observeEvent(input$submit, {
    
    symptom_score <- input$weight_gain + input$hair_growth +
      input$hair_loss + input$skin_darkening +
      input$pimples + input$fatigue + input$bloating
    
    score <- 0
    
    if(input$cycle_regular == "irregular") score <- score + 20
    if(input$cycle_regular == "infrequent") score <- score + 30
    if(input$cycle_regular == "absent") score <- score + 35
    
    score <- score + (symptom_score * 3)
    
    if(input$bmi > 25) score <- score + 10
    if(input$bmi > 30) score <- score + 5
    
    if(input$family_history == "yes") score <- score + 15
    if(input$family_history == "unsure") score <- score + 5
    
    score <- score + (as.numeric(input$fast_food) * 3)
    score <- score - (as.numeric(input$exercise_freq) * 2)
    score <- score + (as.numeric(input$stress_level) * 3)
    
    score <- min(score, 100)
    rv$pcos_score <- score
    
    insulin_score <- input$weight_gain + input$skin_darkening +
      as.numeric(input$fast_food) +
      ifelse(input$bmi > 25, 3, 0) +
      ifelse(input$cycle_regular %in% c("irregular", "infrequent"), 2, 0)
    
    inflammatory_score <- input$fatigue + input$bloating +
      input$skin_darkening + input$pimples +
      as.numeric(input$stress_level) +
      as.numeric(input$fast_food)
    
    androgen_score <- input$hair_growth + input$hair_loss +
      input$pimples + input$skin_darkening +
      ifelse(input$bmi < 25, 2, 0)
    
    postpill_score <- ifelse(input$pill_history == "yes", 10, 0) +
      input$fatigue +
      ifelse(input$cycle_regular == "irregular", 3, 0) +
      ifelse(input$cycle_regular == "absent", 5, 0)
    
    scores <- c(
      "Insulin Resistant PCOS" = insulin_score,
      "Inflammatory PCOS" = inflammatory_score,
      "Androgen Excess PCOS" = androgen_score,
      "Post Pill PCOS" = postpill_score
    )
    
    rv$pcos_type <- names(which.max(scores))
  })
  
  # ============================================================
  # ASSESSMENT RESULTS
  # ============================================================
  
  output$assessment_results <- renderUI({
    req(rv$pcos_score)
    
    plan <- management_plans[[rv$pcos_type]]
    
    tagList(
      
      div(class = "card",
          h3("Your Results"),
          fluidRow(
            column(6,
                   div(class = "stat-box",
                       p("PCOS Likelihood Score"),
                       h3(paste0(rv$pcos_score, "%")),
                       p(ifelse(rv$pcos_score >= 60,
                                "High likelihood. Please discuss with your GP.",
                                ifelse(rv$pcos_score >= 35,
                                       "Moderate likelihood. Worth monitoring and discussing with GP.",
                                       "Lower likelihood. Continue monitoring your symptoms.")))
                   )
            ),
            column(6,
                   div(class = "stat-box",
                       p("Predicted PCOS Type"),
                       h3(plan$emoji),
                       p(rv$pcos_type)
                   )
            )
          )
      ),
      
      div(class = paste("result-card", plan$colour),
          h2(paste(plan$emoji, rv$pcos_type)),
          p(plan$description)
      ),
      
      if(input$miscarriage == "yes") {
        div(class = "plan-section",
            h4("🤍 A Note on Miscarriage"),
            p("We are sorry for your loss. PCOS can contribute to miscarriage risk,
              particularly through high LH levels, insulin resistance, and progesterone
              deficiency. Your GP letter will include a specific request to discuss
              this with your doctor. Treatment of your specific PCOS type can significantly
              improve pregnancy outcomes.")
        )
      },
      
      if(input$family_history == "yes") {
        div(class = "plan-section",
            h4("👨‍👩‍👧 Family History Noted"),
            p("Having a first degree relative with PCOS increases your likelihood 2 to 3 times.
              This has been factored into your score and will be included in your GP letter.")
        )
      },
      
      if(isTRUE(input$consent_data)) {
        div(class = "consent-box",
            p("✅ Thank you for consenting to share your anonymous data.
              Your responses will help improve this tool for other women.")
        )
      },
      
      div(style = "text-align: center; margin: 20px 0;",
          p("✅ Your personalised management plan is ready in the My Plan tab"),
          p("✅ Your GP letter is ready in the GP Letter tab")
      )
    )
  })
  
  # ============================================================
  # MANAGEMENT PLAN
  # ============================================================
  
  output$management_plan_ui <- renderUI({
    
    if(is.null(rv$pcos_type)) {
      return(
        div(class = "card", style = "text-align: center; padding: 60px;",
            h3("Complete Your Assessment First"),
            p("Go to the Assessment tab and answer the questions to unlock your personalised plan."),
            p("🌸", style = "font-size: 48px;")
        )
      )
    }
    
    plan <- management_plans[[rv$pcos_type]]
    
    tagList(
      
      div(class = paste("result-card", plan$colour),
          h2(paste(plan$emoji, rv$pcos_type, "Management Plan")),
          p("Your personalised plan based on your symptom profile")
      ),
      
      div(class = "card",
          div(class = "plan-section",
              h4("🥗 Nutrition Recommendations"),
              tags$ul(lapply(plan$nutrition, function(x) tags$li(x)))
          )
      ),
      
      div(class = "card",
          div(class = "plan-section",
              h4("🏃‍♀️ Exercise Recommendations"),
              tags$ul(lapply(plan$exercise, function(x) tags$li(x)))
          )
      ),
      
      div(class = "card",
          div(class = "plan-section",
              h4("💊 Supplements to Discuss with Your GP"),
              div(class = "disclaimer-box",
                  "Always consult your GP before starting any supplements,
                  especially if you are pregnant, trying to conceive, or on medication."
              ),
              tags$ul(lapply(plan$supplements, function(x) tags$li(x)))
          )
      ),
      
      div(class = "card",
          div(class = "plan-section",
              h4("💉 Medications Your GP May Consider"),
              div(class = "disclaimer-box",
                  "These are medications commonly prescribed for your PCOS type.
                  Only your GP can prescribe medication. Use this list to have
                  an informed conversation."
              ),
              tags$ul(lapply(plan$medications, function(x) tags$li(x)))
          )
      ),
      
      if(!is.null(input$trying_conceive) && input$trying_conceive == "yes") {
        div(class = "card",
            div(class = "plan-section",
                h4("🤱 Fertility Specific Advice"),
                p("Since you are trying to conceive, here are additional recommendations:"),
                tags$ul(
                  tags$li("Start taking folic acid 400mcg daily immediately if not already"),
                  tags$li("Track ovulation using OPK strips and the Premom app"),
                  tags$li("Ask your GP to test progesterone 7 days after suspected ovulation"),
                  tags$li("Request an AMH test to assess ovarian reserve"),
                  tags$li("Ask about Letrozole or Clomid if you are not ovulating regularly"),
                  if(!is.null(input$miscarriage) && input$miscarriage == "yes")
                    tags$li("Discuss progesterone supplementation in early pregnancy
                             given your miscarriage history")
                )
            )
        )
      },
      
      div(class = "card",
          h4("📅 Your Recovery Timeline"),
          p("Here is what to expect as you implement your plan:"),
          lapply(plan$phases, function(phase) {
            div(class = "phase-item",
                h5(paste(phase$time, "—", phase$title)),
                p(phase$description)
            )
          })
      )
    )
  })
  
  # ============================================================
  # PERIOD TRACKER
  # ============================================================
  
  observeEvent(input$log_period, {
    
    new_period <- data.frame(
      Start = input$period_start,
      End = input$period_end,
      Duration = as.numeric(input$period_end - input$period_start) + 1,
      Cycle_Length = NA,
      stringsAsFactors = FALSE
    )
    
    rv$periods <- rbind(rv$periods, new_period)
    rv$periods <- rv$periods %>% arrange(Start)
    
    if(nrow(rv$periods) > 1) {
      for(i in 2:nrow(rv$periods)) {
        rv$periods$Cycle_Length[i-1] <- as.numeric(
          rv$periods$Start[i] - rv$periods$Start[i-1]
        )
      }
    }
  })
  
  output$cycle_stats <- renderUI({
    req(nrow(rv$periods) > 0)
    
    avg_cycle <- if(nrow(rv$periods) > 1) {
      round(mean(rv$periods$Cycle_Length, na.rm = TRUE), 0)
    } else { NA }
    
    avg_duration <- round(mean(rv$periods$Duration), 0)
    
    irregular <- if(!is.na(avg_cycle)) {
      avg_cycle < 21 | avg_cycle > 35
    } else { FALSE }
    
    div(class = "card",
        h4("Your Cycle Summary"),
        fluidRow(
          column(4,
                 div(class = "cycle-stat",
                     p("Average Cycle Length"),
                     h3(ifelse(is.na(avg_cycle), "Log more periods",
                               paste0(avg_cycle, " days"))),
                     p(ifelse(!is.na(avg_cycle) & irregular,
                              "⚠️ Irregular cycle detected",
                              "✅ Within normal range"))
                 )
          ),
          column(4,
                 div(class = "cycle-stat",
                     p("Average Period Duration"),
                     h3(paste0(avg_duration, " days")),
                     p("Normal range is 3 to 7 days")
                 )
          ),
          column(4,
                 div(class = "cycle-stat",
                     p("Periods Logged"),
                     h3(nrow(rv$periods)),
                     p("Log at least 3 for accurate averages")
                 )
          )
        )
    )
  })
  
  output$period_table <- renderDT({
    req(nrow(rv$periods) > 0)
    
    rv$periods %>%
      mutate(
        Start = format(Start, "%d %b %Y"),
        End = format(End, "%d %b %Y"),
        Duration = paste0(Duration, " days"),
        Cycle_Length = ifelse(is.na(Cycle_Length), "—", paste0(Cycle_Length, " days"))
      ) %>%
      rename(
        "Period Start" = Start,
        "Period End" = End,
        "Duration" = Duration,
        "Days Since Last Period" = Cycle_Length
      ) %>%
      datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
  
  # ============================================================
  # GP LETTER
  # ============================================================
  
  output$gp_letter_ui <- renderUI({
    
    if(is.null(rv$pcos_type)) {
      return(
        div(class = "card", style = "text-align: center; padding: 60px;",
            h3("Complete Your Assessment First"),
            p("Go to the Assessment tab and answer the questions to generate your GP letter."),
            p("📄", style = "font-size: 48px;")
        )
      )
    }
    
    today <- format(Sys.Date(), "%d %B %Y")
    
    tagList(
      div(class = "card",
          h3("Your GP Letter"),
          p("Print or screenshot this letter and bring it to your next GP appointment."),
          div(class = "disclaimer-box",
              "This letter is a conversation starter, not a diagnosis.
              Your GP will make the final assessment."
          )
      ),
      
      div(class = "gp-letter",
          p(today),
          br(),
          p("Dear Doctor,"),
          br(),
          p("I am writing to request an assessment for Polycystic Ovary Syndrome (PCOS).
            I have completed a symptom assessment tool and my results suggest I may have
            a significant likelihood of PCOS. I would like to discuss my symptoms and
            request appropriate testing."),
          br(),
          p(strong("My key symptoms include:")),
          tags$ul(
            if(input$cycle_regular != "regular")
              tags$li(paste("Menstrual irregularity:", input$cycle_regular, "cycles")),
            if(input$weight_gain >= 3) tags$li("Significant unexplained weight gain"),
            if(input$hair_growth >= 3) tags$li("Unwanted facial and body hair growth"),
            if(input$hair_loss >= 3) tags$li("Hair thinning and loss on the scalp"),
            if(input$skin_darkening >= 3) tags$li("Skin darkening on neck and underarms"),
            if(input$pimples >= 3) tags$li("Persistent hormonal acne especially on chin and jaw"),
            if(input$fatigue >= 3) tags$li("Chronic fatigue and low energy"),
            if(input$bloating >= 3) tags$li("Persistent bloating and digestive issues")
          ),
          br(),
          if(input$family_history == "yes") {
            tagList(
              p(strong("Family History:")),
              p("I have a first degree female relative with PCOS or significant menstrual
                irregularity. Research indicates this increases my likelihood of PCOS
                by 2 to 3 times."),
              br()
            )
          },
          if(input$miscarriage == "yes") {
            tagList(
              p(strong("Obstetric History:")),
              p("I have experienced one or more miscarriages. I understand that PCOS,
                particularly elevated LH levels and insulin resistance, can contribute
                to miscarriage risk. I would like to discuss whether treating underlying
                PCOS may improve future pregnancy outcomes."),
              br()
            )
          },
          p(strong("Tests I would like to request:")),
          tags$ul(
            tags$li("LH and FSH levels, ideally on day 2 to 4 of my cycle"),
            tags$li("AMH (Anti-Mullerian Hormone) to assess ovarian reserve"),
            tags$li("Testosterone and SHBG levels"),
            tags$li("Fasting insulin and glucose to assess insulin resistance"),
            tags$li("TSH and thyroid function"),
            tags$li("Vitamin D levels"),
            tags$li("Pelvic ultrasound to assess ovarian morphology"),
            if(input$miscarriage == "yes")
              tags$li("Progesterone levels and recurrent miscarriage panel")
          ),
          br(),
          if(input$trying_conceive == "yes") {
            tagList(
              p(strong("Fertility:")),
              p("I am currently trying to conceive. I would like to discuss options for
                supporting ovulation and improving pregnancy outcomes in the context of
                potential PCOS."),
              br()
            )
          },
          p(strong("My symptom assessment suggested my profile most closely matches:"),
            rv$pcos_type),
          br(),
          p("I would be grateful for your assessment and guidance on next steps including
            appropriate testing, referral to a gynaecologist or endocrinologist if warranted,
            and discussion of management options."),
          br(),
          p("Thank you for your time."),
          br(),
          p("Yours sincerely,"),
          br(),
          p("________________________"),
          p(paste("Date:", today))
      ),
      
      div(style = "text-align: center; margin: 20px 0;",
          p("💡 Tip: Screenshot this letter or use Ctrl+P to print it before your appointment")
      )
    )
  })
}

# ============================================================
# LAUNCH APP
# ============================================================

shinyApp(ui = ui, server = server)
