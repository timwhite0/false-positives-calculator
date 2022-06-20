###########################################################################################

##### Packages
library(shiny)
library(shinydashboard)
library(reactable)
library(readxl)


##### Data

## Studies
studies = read_excel("Studies spreadsheet.xlsx")
studies$Disease = as.factor(studies$Disease)
studies$`Screening test` = as.factor(studies$`Screening test`)
studies$`Study ID` = as.factor(studies$`Study ID`)
studies$`USPSTF recommendation` = ifelse(studies$Disease == "Breast cancer",
                                         "https://uspreventiveservicestaskforce.org/uspstf/recommendation/breast-cancer-screening",
                                         ifelse(studies$Disease == "Cervical cancer",
                                                "https://uspreventiveservicestaskforce.org/uspstf/recommendation/cervical-cancer-screening",
                                                ifelse(studies$Disease == "Chlamydia" | studies$Disease == "Gonorrhea",
                                                       "https://uspreventiveservicestaskforce.org/uspstf/recommendation/chlamydia-and-gonorrhea-screening",
                                                       ifelse(studies$Disease == "Colorectal cancer",
                                                              "https://uspreventiveservicestaskforce.org/uspstf/recommendation/colorectal-cancer-screening",
                                                              ifelse(studies$Disease == "Hepatitis B",
                                                                     "https://uspreventiveservicestaskforce.org/uspstf/recommendation/hepatitis-b-virus-infection-screening",
                                                                     ifelse (studies$Disease == "Hepatitis C",
                                                                             "https://uspreventiveservicestaskforce.org/uspstf/recommendation/hepatitis-c-screening",
                                                                             ifelse(studies$Disease == "HIV",
                                                                                    "https://uspreventiveservicestaskforce.org/uspstf/recommendation/human-immunodeficiency-virus-hiv-infection-screening",
                                                                                    ifelse (studies$Disease == "Lung cancer",
                                                                                            "https://uspreventiveservicestaskforce.org/uspstf/recommendation/lung-cancer-screening",
                                                                                            ifelse (studies$Disease == "Prostate cancer",
                                                                                                    "https://uspreventiveservicestaskforce.org/uspstf/recommendation/prostate-cancer-screening",
                                                                                                    ifelse (studies$Disease == "Syphilis",
                                                                                                            "https://uspreventiveservicestaskforce.org/uspstf/recommendation/syphilis-infection-in-nonpregnant-adults-and-adolescents", NA))))))))))


## Lifetime number of tests
lifetime.tests = read_excel("Lifetime number of tests spreadsheet.xlsx")
lifetime.tests$Disease = as.factor(lifetime.tests$Disease)
lifetime.tests$`Screening test` = as.factor(lifetime.tests$`Screening test`)
lifetime.tests = lifetime.tests[order(lifetime.tests$Disease),]


## Abbreviations
abbreviations = read_excel("Subpopulation abbreviations.xlsx")
abbreviations$Subpopulation = as.factor(abbreviations$Subpopulation)
abbreviations$Abbreviation = as.factor(abbreviations$Abbreviation)

###########################################################################################

###########################################################################################

##### UI
ui = dashboardPage(
    
    skin = "blue",
    
    title = "The False Positives Calculator",
    
    dashboardHeader(
        title = span(tagList(icon("plus-circle"), "The False Positives Calculator")),
        titleWidth = 325
    ),
    
    dashboardSidebar(
        width = 325,
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
            menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
            menuItem("Screening Guidelines", tabName = "guidelines", icon = icon("clipboard-list")),
            menuItem("Data", tabName = "data", icon = icon("table"))
        )
    ),
    
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "overview",
                    
                    htmlOutput("welcome"),
                    
                    hr(),
                    
                    box(htmlOutput("howthisworks"), width = 13, status = "primary", solidHeader = TRUE),
                    
                    hr(),
                    hr(style="border-color: black;"),
                    hr(),
                    
                    box(htmlOutput("introduction"), width = 13, status = "primary", solidHeader = TRUE)
            ),
            
            tabItem(tabName = "calculator",

                    h4("Hello! I have a few questions for you:"),
                    
                    fluidRow(
                        box(title = "What is your sex?",
                            selectInput("sex", label = NULL, selected = "Female",
                                        choices = c("Female", "Male")),
                            width = 6, collapsible = FALSE, solidHeader = FALSE, status = "primary"),
                        
                        box(title = "Do you have a history of smoking?",
                            selectInput("smoke", label = NULL, selected = "No",
                                        choices = c("No", "Yes")),
                            width = 6, collapsible = FALSE, solidHeader = FALSE, status = "primary"),
                    ),
                    
                    conditionalPanel(
                        condition = "input.sex == 'Female'",
                        
                        fluidRow(
                            box(title = "How many pregnancies do you expect to experience in your lifetime?",
                                selectInput("pregnancy", label = NULL, selected = 0,
                                            choices = c(0, 1, 2)),
                                width = 6, collapsible = FALSE, solidHeader = FALSE, status = "primary")
                        )
                    ),
                    
                    conditionalPanel(
                        condition = "input.sex == 'Male'",
                        
                        fluidRow(
                            box(title = "Are you a man who has sex with men (MSM)?",
                                selectInput("MSM", label = NULL, selected = "No",
                                            choices = c("No", "Yes")),
                                width = 6, collapsible = FALSE, solidHeader = FALSE, status = "primary"),
                            
                            box(title = "Prostate screening is optional for men. Do you intend to get screened for prostate cancer?",
                                selectInput("prostate", label = NULL, selected = "No",
                                            choices = c("No", "Yes")),
                                width = 6, collapsible = FALSE, solidHeader = FALSE, status = "primary")
                        )
                    ),
                    
                    hr(),
                    h4("According to the U.S. Preventive Services Task Force (USPSTF), you should get screened for the following cancers and STDs:"),

                    fluidPage(
                        conditionalPanel(
                            condition = "input.sex == 'Female'",
                            
                            box(title = "Breast cancer",
                                htmlOutput("breast"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE),
                            
                            box(title = "Cervical cancer",
                                htmlOutput("cervical"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        conditionalPanel(
                            condition = "input.sex == 'Female' || (input.sex == 'Male' && input.MSM == 'Yes')",
                            
                            box(title = "Chlamydia",
                                htmlOutput("chlamydia"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        box(title = "Colorectal cancer",
                            htmlOutput("colorectal"),
                            width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE),
                        
                        conditionalPanel(
                            condition = "input.sex == 'Female' || (input.sex == 'Male' && input.MSM == 'Yes')",
                            
                            box(title = "Gonorrhea",
                                htmlOutput("gonorrhea"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        conditionalPanel(
                            condition = "input.pregnancy > 0 && input.sex == 'Female'",
                            
                            box(title = "Hepatitis B",
                                htmlOutput("hepatitisB"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        box(title = "Hepatitis C",
                            htmlOutput("hepatitisC"),
                            width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE),
                        
                        box(title = "HIV",
                            htmlOutput("hiv"),
                            width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE),
                        
                        conditionalPanel(
                            condition = "input.smoke == 'Yes'",
                            
                            box(title = "Lung cancer",
                                htmlOutput("lung"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        conditionalPanel(
                            condition = "input.prostate == 'Yes' && input.sex == 'Male'",
                            
                            box(title = "Prostate cancer",
                                htmlOutput("prostate"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        ),
                        
                        conditionalPanel(
                            condition = "(input.pregnancy > 0 && input.sex == 'Female') || input.MSM == 'Yes'",
                            
                            box(title = "Syphilis",
                                htmlOutput("syphilis"),
                                width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
                        )
                    ),
                    
                    hr(),
                    
                    h4("The estimated probability that you will receive at least one false positive in a lifetime for one of these cancers or STDs is:"),
                    
                    fluidRow(
                        column(width = 6, offset = 3,
                                box(h1(htmlOutput("lifetimeFP")),
                                    width = 12, background = "olive"),
                               align = "center",
                        ),
                        
                    ),
                    
            ),
            
            tabItem(tabName = "guidelines",
                    
                    box(htmlOutput("instructionsforguidelines"), status = "primary", solidHeader = TRUE, width = 12),

                    fluidRow(
                        column(width = 4, offset = 4,
                               box(uiOutput("selecteddiseases"), status = "primary", width = 12),
                               align = "center")
                    ),
                    
                    fluidPage(
                        conditionalPanel(
                            condition = "$.inArray('Breast cancer', input.mydiseases) > -1",
                            
                            box(title = "Breast cancer", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinesbreast"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Cervical cancer', input.mydiseases) > -1",
                            
                            box(title = "Cervical cancer", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinescervical"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Chlamydia', input.mydiseases) > -1",
                            
                            box(title = "Chlamydia", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelineschlamydia"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Colorectal cancer', input.mydiseases) > -1",
                            
                            box(title = "Colorectal cancer", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinescolorectal"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Gonorrhea', input.mydiseases) > -1",
                            
                            box(title = "Gonorrhea", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinesgonorrhea"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Hepatitis B', input.mydiseases) > -1",
                            
                            box(title = "Hepatitis B", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelineshepatitisB"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Hepatitis C', input.mydiseases) > -1",
                            
                            box(title = "Hepatitis C", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelineshepatitisC"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('HIV', input.mydiseases) > -1",
                            
                            box(title = "HIV", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelineshiv"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Lung cancer', input.mydiseases) > -1",
                            
                            box(title = "Lung cancer", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelineslung"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Prostate cancer', input.mydiseases) > -1",
                            
                            box(title = "Prostate cancer", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinesprostate"))
                        ),
                        
                        conditionalPanel(
                            condition = "$.inArray('Syphilis', input.mydiseases) > -1",
                            
                            box(title = "Syphilis", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                htmlOutput("guidelinessyphilis"))
                        )
                        
                    )
                    
                ),
            
            tabItem(tabName = "data",
                    
                    reactableOutput("table"))
            )
        
    )
)





##### Functions
probability_of_union = function(num.tests, false.positive.prob) {
    # This function computes the false positive probability of the union of sum(num.tests) independent 
    # screening tests.
    #
    # Inputs:
    #   num.tests = Number of independent screening tests
    #   false.positive.prob = False positive probability for each individual test
    #
    #   Note that num.tests and false.positive.prob can be single values or vectors, but they
    #   must have the same length. If the user supplies vectors of different lengths for num.tests and
    #   false.positive.prob, then the function returns a message instructing them to supply vectors of
    #   identical lengths. Given that both arguments are vectors of length n, the function will compute
    #   the cumulative false positive probability of the union of num.tests[1] tests that each have a
    #   false positive probability of false.positive.prob[1], num.tests[2] tests that each have a false
    #   positive probability of false.positive.prob[2], ..., and num.tests[n] tests that each have a false
    #   positive probability of false.positive.prob[n].
    #
    # Returns:
    #   The false positive probability of the union of sum(num.tests) independent screening tests, which
    #   is given by the following formula:
    #   1 - ((1 - false.positive.prob[1])^num.tests[1])*((1 - false.positive.prob[2])^num.tests[2])*...*((1 - false.positive.prob[n])^num.tests[n])
    #
    
    if (length(num.tests) != length(false.positive.prob)) {
        return("Vector length of num.tests must equal vector length of false.positive.prob")
    }
    else {
        return(1 - prod((1 - false.positive.prob)^num.tests))
    }
}

run_bootstrap = function(subpopulation, uncertainty.method = "standard error", cancers.only = FALSE, STDs.only = FALSE, only.most.common = TRUE, B = 10000) {
    # This function uses the parametric bootstrap to compute four sets of estimates for the given subpopulation:
    #   1) marginal: The marginal false positive probability for each test for each disease for which the subpopulation is tested.
    #   2) test.cumulative: The cumulative false positive probability for each test for each disease for which the subpopulation is tested.
    #   3) disease.cumulative: The cumulative false positive probability for each disease for which the subpopulation is tested.
    #   4) lifetime: The probability of receiving at least one false positive result for any disease in a lifetime.
    #
    # Inputs:
    #   subpopulation: Subpopulation for which the estimates are to be computed. Must be an exact match of one of the
    #                  column names of the data frame lifetime.tests. Otherwise, the function returns an error message.
    #   uncertainty.method: Indicates whether the user would like to communicate uncertainty via 95% percentile CIs or
    #                       standard errors. Must be either "percentile CI" or "standard error", otherwise the function
    #                       returns an error message. The default value is "standard error".
    #   cancers.only: Indicates whether the user would like to include only the cancers in the analysis (and hence exclude
    #                 the STDs). If cancers.only is TRUE, then STDs.only must be FALSE. Otherwise, the function returns an
    #                 error message. The default value for both cancers.only and STDs.only is FALSE (which indicates that
    #                 both cancers and STDs should be included in the analysis).
    #   STDs.only: Indicates whether the user would like to include only the STDs in the analysis (and hence exclude the
    #              cancers). If STDs.only is TRUE, then cancers.only must be FALSE. Otherwise, the function returns an
    #              error message. The default value for both STDs.only and cancers.only is FALSE (which indicates that both
    #              STDs and cancers should be included in the analysis).
    #   only.most.common: Indicates whether all screening tests should be included for each disease, or if only the most common
    #                     one should be used for the analysis. The only diseases for which there is more than one potential
    #                     screening test are colorectal cancer (colonoscopy, gFOBT, FIT, and sDNA-FIT) and syphilis (VDRL and
    #                     RPR). If only.most.common is TRUE, then only colonoscopy is used for colorectal cancer and only
    #                     RPR is used for syphilis. The default value is TRUE.
    #   B: Number of bootstrap iterations. The default value is 10,000.
    #
    # Returns:
    #   List of 6 objects:
    #     1) Diseases: Vector of diseases for which the given subpopulation is to be tested at least once.
    #     2) Tests: Vector of all potential screening tests for the diseases in vector Diseases.
    #     3) Marginal.FP.prob: Data frame that reports the estimated marginal false positive probability
    #                          for each test for each of the diseases for which the subpopulation is to
    #                          be tested, along with some form of uncertainty (CI or standard error)
    #                          determined by the argument uncertainty.method.
    #     4) Test.cumulative.FP.prob: Data frame that reports the estimated cumulative false positive probability
    #                                 for each test for each of the diseases for which the subpopulation is to be
    #                                 tested, along with some form of uncertainty (CI or standard error) determined
    #                                 by the argument uncertainty.method.
    #     5) Disease.cumulative.FP.prob: Data frame that reports the estimated cumulative false positive probability
    #                                    for each of the diseases for which the subpopulation is to be tested, along
    #                                    with some form of uncertainty (CI or standard error) determined by the
    #                                    argument uncertainty.method.
    #     6) Lifetime.FP.prob: Data frame that reports the estimated probability of receiving a false positive
    #                          result for any of the diseases in vector Diseases in a lifetime, along with
    #                          some form of uncertainty (CI or standard error) determined by uncertainty.method.
    #                          The function will also plot a histogram of the bootstrap estimates for the
    #                          lifetime false positive probability.
    
    
    # Return an error message if input(s) are invalid
    if (!(uncertainty.method == "percentile CI" | uncertainty.method == "standard error")) {
        return("uncertainty.method must be either 'percentile CI' or 'standard error'.")
    }
    if (!any(colnames(lifetime.tests) == subpopulation)) {
        return("Invalid subpopulation.")
    }
    if (cancers.only == TRUE & STDs.only == TRUE) {
        return("At least one of cancers.only and STDs.only must be FALSE. To run the analysis for cancers AND STDs, set both arguments to FALSE (this is also the function's default).")
    }
    
    # Trim studies and lifetime.tests to include only the cancers if cancers.only == TRUE or only the STDs if STDs.only == TRUE
    # If both arguments are FALSE, then no action is needed here
    if (cancers.only == TRUE) {
        lifetime.tests = lifetime.tests[which(grepl("cancer", lifetime.tests$Disease)),]
        studies = studies[which(grepl("cancer", studies$Disease)),]
    }
    if (STDs.only == TRUE) {
        lifetime.tests = lifetime.tests[-which(grepl("cancer", lifetime.tests$Disease)),]
        studies = studies[-which(grepl("cancer", studies$Disease)),]
    }
    
    # If only.most.common is TRUE, trim studies and lifetime.tests to include only colonoscopy for colorectal cancer, only Pap test for cervical cancer, and only RPR for syphilis.
    # Otherwise, include all potential screening tests for colorectal cancer and syphilis.
    if (only.most.common == TRUE) {
        studies = studies[-which(studies$Disease == "Syphilis" & studies$`Screening test` != "RPR" |
                                     studies$Disease == "Colorectal cancer" & studies$`Screening test` != "Colonoscopy" |
                                     studies$Disease == "Cervical cancer" & studies$`Screening test` != "Pap"),]
        
        lifetime.tests = lifetime.tests[-which(lifetime.tests$Disease == "Syphilis" & lifetime.tests$`Screening test` != "RPR" |
                                                   lifetime.tests$Disease == "Colorectal cancer" & lifetime.tests$`Screening test` != "Colonoscopy" |
                                                   lifetime.tests$Disease == "Cervical cancer" & lifetime.tests$`Screening test` != "Pap" & lifetime.tests$`Screening test` != "Pap (before age 30)"),]
        
        # If necessary, combine "Pap (before age 30)" and "Pap" to get total lifetime number of Pap tests, then delete the "Pap (before age 30)" row
        if (any(lifetime.tests$`Screening test` == "Pap (before age 30)")) {
            for (i in 3:ncol(lifetime.tests)) {
                lifetime.tests[which(lifetime.tests$`Screening test` == "Pap"), i] = sum(lifetime.tests[which(lifetime.tests$`Screening test` == "Pap"), i],
                                                                                         lifetime.tests[which(lifetime.tests$`Screening test` == "Pap (before age 30)"), i])
            }
            lifetime.tests = lifetime.tests[-which(lifetime.tests$`Screening test` == "Pap (before age 30)"),]
        }
    }
    
    # Find column number of lifetime tests spreadsheet corresponding to the given subpopulation
    column.num = which(colnames(lifetime.tests) == subpopulation)
    
    # Get set of diseases for which the given subpopulation should be tested at least once
    diseases = levels(as.factor(as.character(lifetime.tests$Disease[which(lifetime.tests[,column.num] != 0)])))
    
    
    
    ## Compute point estimates for marginal, cumulative (test), cumulative (disease), and lifetime false positive probabilities
    
    # Initialize list to store point estimates for the marginal false positive probability for each test for each disease
    point.estimates.marginal = vector(mode = "list", length = length(diseases))
    # Initialize list to store point estimates for the cumulative false positive probability for each test for each disease
    point.estimates.test.cumulative = vector(mode = "list", length = length(diseases))
    # Initialize vector to store point estimates for the cumulative false positive probability for each disease
    point.estimates.disease.cumulative = numeric(length(diseases))
    
    for (d in 1:length(diseases)) {
        # Get set of all screening tests for the diseases for which the given subpopulation is to be tested
        if (diseases[d] == "Cervical cancer" & only.most.common == FALSE) {
            tests = as.character(lifetime.tests$`Screening test`[which(lifetime.tests$Disease == diseases[d] & lifetime.tests$`Screening test` != "Pap (before age 30)")])
        }
        else {
            tests = as.character(lifetime.tests$`Screening test`[which(lifetime.tests$Disease == diseases[d])])
        }
        
        # Initialize the dth vector of list point.estimates.marginal
        point.estimates.marginal[[d]] = numeric(length(tests))
        # Initialize the dth vector of list point.estimates.test.cumulative
        point.estimates.test.cumulative[[d]] = numeric(length(tests))
        
        for (k in 1:length(tests)) {
            # Create new data frame that only includes studies for test k for disease d
            studies.sub = studies[which(studies$`Screening test` == tests[k]),]
            
            # Compute point estimate for the marginal false positive probability for test k for disease d
            point.estimates.marginal[[d]][k] = sum(studies.sub$FP)/sum(studies.sub$FP, studies.sub$TN)
        }
        
        for (k in 1:length(tests)) {
            # Compute point estimate for the cumulative false positive probability for test k for disease d
            if (diseases[d] == "Cervical cancer" & only.most.common == FALSE) {
                point.estimates.test.cumulative[[d]][k] = probability_of_union(num.tests = c(as.numeric(lifetime.tests[lifetime.tests$`Screening test` == "Pap (before age 30)", column.num]),
                                                                                             as.numeric(lifetime.tests[lifetime.tests$`Screening test` == tests[k], column.num])),
                                                                               false.positive.prob = c(point.estimates.marginal[[d]][which(tests == "Pap")],
                                                                                                       point.estimates.marginal[[d]][k]))
            }
            else {
                point.estimates.test.cumulative[[d]][k] = probability_of_union(num.tests = lifetime.tests[lifetime.tests$`Screening test` == tests[k], column.num],
                                                                               false.positive.prob = point.estimates.marginal[[d]][k])
            }
            
        }
        
        # Compute point estimate for the cumulative false positive probability for disease d
        point.estimates.disease.cumulative[d] = mean(point.estimates.test.cumulative[[d]])
    }
    
    # Compute point estimate for the cumulative probability of receiving a false positive result for any disease in a lifetime
    point.estimate.lifetime = probability_of_union(num.tests = rep(1, times = length(diseases)),
                                                   false.positive.prob = point.estimates.disease.cumulative)
    
    
    
    ## Compute standard errors for marginal, cumulative (test), cumulative (disease), and lifetime false positive probabilities 
    
    # Initialize vector to store the cumulative probability of receiving a false positive result for any disease in a lifetime
    lifetime.FP = numeric(B)
    # Initialize list to store the marginal false positive probability for each test for each disease for each bootstrap realization
    marginal.FP = vector(mode = "list", length = length(diseases))
    # Initialize list to store the cumulative false positive probability for each test for each disease for each bootstrap realization
    test.cumulative.FP = vector(mode = "list", length = length(diseases))
    # Initialize matrix to store the cumulative false positive probability for each disease for each bootstrap realization
    disease.cumulative.FP = matrix(nrow = B, ncol = length(diseases))
    
    for (d in 1:length(diseases)) {
        
        # Get set of all screening tests for the diseases for which the given subpopulation is to be tested
        if (diseases[d] == "Cervical cancer" & only.most.common == FALSE) {
            tests = as.character(lifetime.tests$`Screening test`[which(lifetime.tests$Disease == diseases[d] & lifetime.tests$`Screening test` != "Pap (before age 30)")])
        }
        else {
            tests = as.character(lifetime.tests$`Screening test`[which(lifetime.tests$Disease == diseases[d])])
        }
        
        # Initialize the dth matrix of list marginal.FP
        marginal.FP[[d]] = matrix(nrow = B, ncol = length(tests))
        # Initialize the dth matrix of list test.cumulative.FP
        test.cumulative.FP[[d]] = matrix(nrow = B, ncol = length(tests))
        
        for (b in 1:B) {
            
            for (k in 1:length(tests)) {
                # Create new data frame that only includes studies for test k for disease d
                studies.sub = studies[which(studies$`Screening test` == tests[k]),]
                
                # Initialize matrix to store each bootstrap realization
                studies.multinom = matrix(nrow = nrow(studies.sub), ncol = 3)
                
                for (s in 1:nrow(studies.sub)) {
                    # Generate a bootstrap realization for each study for disease d by randomly sampling the number
                    # of FP, TN, and (TP+FN) from a uniform multinomial distribution
                    studies.multinom[s,] = rmultinom(n = 1,
                                                     size = studies.sub$N[s],
                                                     prob = c(studies.sub$`% FP`[s],
                                                              studies.sub$`% TN`[s],
                                                              studies.sub$`% TP`[s] + studies.sub$`% FN`[s]))
                }
                
                # Compute the bth estimate of the marginal false positive probability for test k for disease d
                marginal.FP[[d]][b,k] = sum(studies.multinom[,1])/sum(studies.multinom[,1:2])
            }
            
            for (k in 1:length(tests)) {
                # Compute the bth estimate of the cumulative false positive probability for test k for disease d
                if (diseases[d] == "Cervical cancer" & only.most.common == FALSE) {
                    test.cumulative.FP[[d]][b,k] = probability_of_union(num.tests = c(as.numeric(lifetime.tests[lifetime.tests$`Screening test` == "Pap (before age 30)", column.num]),
                                                                                      as.numeric(lifetime.tests[lifetime.tests$`Screening test` == tests[k], column.num])),
                                                                        false.positive.prob = c(marginal.FP[[d]][b, which(tests == "Pap")], marginal.FP[[d]][b,k]))
                }
                else {
                    test.cumulative.FP[[d]][b,k] = probability_of_union(num.tests = lifetime.tests[lifetime.tests$`Screening test` == tests[k], column.num],
                                                                        false.positive.prob = marginal.FP[[d]][b,k])
                }
            }
            
            # Compute the bth estimate of the cumulative false positive probability for disease d
            disease.cumulative.FP[b,d] = mean(test.cumulative.FP[[d]][b,])
        }
    }
    
    for (b in 1:B) {
        # Compute the bth estimate of the cumulative probability of receiving a false positive result for any disease in a lifetime
        lifetime.FP[b] = probability_of_union(num.tests = rep(1, times = length(diseases)),
                                              false.positive.prob = disease.cumulative.FP[b,])
    }
    
    
    
    ## Construct data frames for point estimates and CIs/standard errors, where the type of uncertainty is determined by uncertainty.method
    
    # Make list of diseases and tests
    Disease = sort(as.factor(as.character(lifetime.tests$Disease[which(lifetime.tests[,column.num] != 0 & lifetime.tests$`Screening test` != "Pap (before age 30)")])))
    Test = as.factor(as.character(lifetime.tests$`Screening test`[which(lifetime.tests[,column.num] != 0 & lifetime.tests$`Screening test` != "Pap (before age 30)")]))
    
    if (uncertainty.method == "percentile CI") {
        # Point estimate and 95% CI for marginal false positive probability for each disease
        marginal = data.frame(Disease = Disease,
                              Test = Test,
                              Estimate = unlist(point.estimates.marginal),
                              Lower = unlist(lapply(marginal.FP, apply, 2, quantile, probs = 0.025)),
                              Upper = unlist(lapply(marginal.FP, apply, 2, quantile, probs = 0.975)))
        
        # Point estimate and 95% CI for cumulative false positive probability for each test for each disease
        test.cumulative = data.frame(Disease = Disease,
                                     Test = Test,
                                     Estimate = unlist(point.estimates.test.cumulative),
                                     Lower = unlist(lapply(test.cumulative.FP, apply, 2, quantile, probs = 0.025)),
                                     Upper = unlist(lapply(test.cumulative.FP, apply, 2, quantile, probs = 0.975)))
        
        # Point estimate and 95% CI for cumulative false positive probability for each disease
        disease.cumulative = data.frame(Disease = diseases,
                                        Estimate = unlist(point.estimates.disease.cumulative),
                                        Lower = apply(disease.cumulative.FP, 2, quantile, probs = 0.025),
                                        Upper = apply(disease.cumulative.FP, 2, quantile, probs = 0.975))
        
        # Point estimate and 95% CI for cumulative probability of receiving a false positive result for any disease in a lifetime
        hist(lifetime.FP, breaks = 50, xlim = c(0, 1),
             main = subpopulation,
             xlab = "Cumulative FP probability in a lifetime")
        
        lifetime = data.frame(Estimate = point.estimate.lifetime,
                              Lower = quantile(lifetime.FP, 0.025),
                              Upper = quantile(lifetime.FP, 0.975),
                              row.names = "")
    }
    else if (uncertainty.method == "standard error") {
        # Point estimate and standard error for marginal false positive probability for each disease
        marginal = data.frame(Disease = Disease,
                              Test = Test,
                              Estimate = unlist(point.estimates.marginal),
                              SE = unlist(lapply(marginal.FP, apply, 2, sd)))
        
        # Point estimate and SE for cumulative false positive probability for each test for each disease
        test.cumulative = data.frame(Disease = Disease,
                                     Test = Test,
                                     Estimate = unlist(point.estimates.test.cumulative),
                                     SE = unlist(lapply(test.cumulative.FP, apply, 2, sd)))
        
        # Point estimate and SE for cumulative false positive probability for each disease
        disease.cumulative = data.frame(Disease = diseases,
                                        Estimate = unlist(point.estimates.disease.cumulative),
                                        SE = apply(disease.cumulative.FP, 2, sd))
        
        # Point estimate and SE for cumulative probability of receiving a false positive result for any disease in a lifetime
        hist(lifetime.FP, breaks = 50, xlim = c(0, 1),
             main = subpopulation,
             xlab = "Cumulative FP probability in a lifetime")
        
        lifetime = data.frame(Estimate = point.estimate.lifetime,
                              SE = sd(lifetime.FP))
    }
    
    
    
    ## Return the list of objects mentioned in the function description
    return(list(Diseases = diseases,
                Tests = levels(as.factor(as.character(lifetime.tests$`Screening test`[which(lifetime.tests[,column.num] != 0 & lifetime.tests$`Screening test` != "Pap (before age 30)")]))),
                Marginal.FP.prob = marginal,
                Test.cumulative.FP.prob = test.cumulative,
                Disease.cumulative.FP.prob = disease.cumulative,
                Lifetime.FP.prob = lifetime))
}

identify_subpop = function(sex, smoke, pregnancy, MSM, prostate) {
    
    if (sex == "Female") {
        if (smoke == "No") {
            if (pregnancy == 0) {
                return("Women (baseline)")
            }
            else if (pregnancy == 1) {
                return("Women with one pregnancy")
            }
            else if (pregnancy == 2) {
                return("Women with two pregnancies")
            }
        }
        else if (smoke == "Yes") {
            if (pregnancy == 0) {
                return("Female smokers (one lung screening)")
            }
            else if (pregnancy == 1) {
                return("Women with one pregnancy who smoke (one lung screening)")
            }
            else if (pregnancy == 2) {
                return("Women with two pregnancies who smoke (one lung screening)")
            }
        }
    }
    else if (sex == "Male") {
        if (smoke == "No") {
            if (MSM == "No") {
                if (prostate == "No") {
                    return("Men (baseline)")
                }
                else if (prostate == "Yes") {
                    return("Men who get tested for prostate")
                }
            }
            else if (MSM == "Yes") {
                if (prostate == "No") {
                    return("Men who have sex with men")
                }
                else if (prostate == "Yes") {
                    return("MSM who get tested for prostate")
                }
            }
        }
        else if (smoke == "Yes") {
            if (MSM == "No") {
                if (prostate == "No") {
                    return("Male smokers (one lung screening)")
                }
                else if (prostate == "Yes") {
                    return("Men who get tested for prostate and smoke (one lung screening)")
                }
            }
            else if (MSM == "Yes") {
                if (prostate == "No") {
                    return("MSM who smoke (one lung screening)")
                }
                else if (prostate == "Yes") {
                    return("MSM who get tested for prostate and smoke (one lung screening)")
                }
            }
        }
    }
}





##### Server
server = function(input, output, session) {
    
    output$welcome = renderText(
        paste0("<h2><b><font color = '444444'>Welcome!</font></b></h3>")
    )
    
    output$howthisworks = renderText(
        paste0("<font size = '+0.1'><b>Here's how this dashboard works:</b>",
               "<br><br>",
               "If you scroll down on this tab, you'll find a description of the dashboard and the project on which it's based.",
               "<br><br>",
               "On the <font size = '+1.5', color = '2d802a'><b>CALCULATOR</b></font> tab, the dashboard will ask you to answer a few basic demographic and behavioral questions. Then it will estimate the probability that you'll receive a false positive in your lifetime, assuming that you adhere to the screening guidelines of the ",
               a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/", target = "_blank"),
               ".",
               "<br><br>",
               "On the <font size = '+1.5', color = '2d802a'><b>SCREENING GUIDELINES</b></font> tab, you can learn more about when and how often you should get tested for the cancers and STDs for which you meet the USPSTF screening criteria. You can also read about the guidelines for all the other diseases, if you're curious.",
               "<br><br>",
               "Finally, on the <font size = '+1.5', color = '2d802a'><b>DATA</b></font> tab, you can explore the data that are used to estimate these probabilities. We also provide links to all 116 studies in the dataset.",
               "<br><br>",
               "The data and source code for this project are available ",
               a("here", href = "https://github.com/timwhite0/false-positives-calculator", target = "_blank"),
               "."),
    )
    
    output$introduction = renderText(
        paste0("<font size = '+0.1'><b>Have you ever received a screening test for cancer, an STD, or some other medical condition?</b>",
               "<br><br>",
               "If not, you've come to the right place. Even if you have, you've <i>still</i> come to the right place! Programs of repeated screening are beneficial because they allow for the early detection of diseases. Routine screening is widely advocated by medical professionals, and expert panels like the USPSTF issue guidelines about who should be tested for which diseases and when.",
               "<br><br>",
               "Unfortunately, screening technology is not perfect, so there's a chance that your test result will wind up being either a <font color = '316e91'><b>false negative</b></font> (indicating that you <font color = '2d802a'><b>don't</b></font> have the disease when you actually <font color = '2d802a'><b>do</b></font>) or a <font color = '316e91'><b>false positive</b></font> (indicating that you <font color = '2d802a'><b>do</b></font> have the disease when you actually <font color = '2d802a'><b>don't</b></font>).",
               "<br><br>",
               "False positives are a necessary evil — we can't get rid of them without simultaneously increasing the prevalence of false negatives, which we would also like to avoid. Still, it's important to recognize that <font color = '316e91'><b>false positives have serious consequences</b></font>. They can generate stress and strain personal relationships. They can discourage you from getting screened again in the future. They can even lead you to undergo unnecessary and costly medical procedures.",
               "<br><br>",
               "Because of these adverse effects, you might find yourself asking, <font color = '2d802a'><b>\"What are the chances that the result of my next screening test — say, a colonoscopy — will be a false positive?\"</b></font>",
               "<br><br>After thinking about it some more, you might wonder, <font color = '2d802a'><b>\"My doctor told me that I'm supposed to get multiple colonoscopies in my lifetime. What's the likelihood that I'll get a false positive from at least one of them?\"</b></font>",
               "<br><br>And you might even realize, <font color = '2d802a'><b>\"I'm supposed to get screened for a bunch of diseases over time. What's the probability that I'll receive at least one false positive from <i>any</i> screening test in my lifetime?\"</b></font>",
               "<br><br>",
               "This dashboard is designed to help you answer these questions and improve your perception of screening test precision. It is a complementary tool for a forthcoming paper entitled ",
               a("\"Estimating the lifetime risk of a false positive screening test result,\"", href = "https://arxiv.org/abs/2206.08463", target = "_blank"),
               " written by ",
               a("Tim White", href = "https://timwhite0.github.io", target = "_blank"),
               " and ",
               a("Sara Algeri", href = "https://salgeri.umn.edu", target = "_blank"),
               " at the University of Minnesota - Twin Cities.",
               "<br><br>",
               "Please note that <font color = '316e91'><b>this dashboard is no substitute for your healthcare provider</b></font>, who can supply more personalized advice about the screening practices that are best for you. For instance, they will be able to tell you if you should get screened for any diseases <i>not</i> included in our final analysis, such as diabetes, osteoporosis, and several types of cancer for which routine screening is not recommended (e.g., oral, ovarian, pancreatic, skin, testicular, thyroid).")
    )
    
    subpop = reactive(
        if (input$sex == "Female") {
            identify_subpop(sex = input$sex, smoke = input$smoke, pregnancy = input$pregnancy, MSM = NULL, prostate = NULL)
        }
        else if (input$sex == "Male") {
            identify_subpop(sex = input$sex, smoke = input$smoke, pregnancy = NULL, MSM = input$MSM, prostate = input$prostate)
        }
    )
    
    compute = reactive(
        run_bootstrap(subpop(),
                      uncertainty.method = "standard error",
                      cancers.only = FALSE,
                      STDs.only = FALSE,
                      only.most.common = TRUE,
                      B = 100)
    )
    
    output$breast = renderText(
        paste0("The primary screening modality for breast cancer is <b>mammography</b>. The false positive rate of each mammogram is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Mammography"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Mammography"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Mammography"), which(colnames(lifetime.tests) == subpop())]),
               " mammograms is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Mammography"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$cervical = renderText(
        paste0("The primary screening test for cervical cancer is the <b>Pap test</b>. The false positive rate of each Pap test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Pap"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>15</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these 15 Pap tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Pap"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$chlamydia = renderText(
        paste0("The primary screening test for chlamydia is the <b>nucleic acid amplification test (NAAT)</b>. The false positive rate of each NAAT is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "NAAT (chlamydia)"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "NAAT (chlamydia)"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "NAAT (chlamydia)"), which(colnames(lifetime.tests) == subpop())]),
               " NAATs is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "NAAT (chlamydia)"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$colorectal = renderText(
        paste0("The primary screening modality for colorectal cancer is <b>colonoscopy</b>. The false positive rate of each colonoscopy is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Colonoscopy"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Colonoscopy"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Colonoscopy"), which(colnames(lifetime.tests) == subpop())]),
               " colonoscopies is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Colonoscopy"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$gonorrhea = renderText(
        paste0("The primary screening test for gonorrhea is the <b>nucleic acid amplification test (NAAT)</b>. The false positive rate of each NAAT is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "NAAT (gonorrhea)"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "NAAT (gonorrhea)"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "NAAT (gonorrhea)"), which(colnames(lifetime.tests) == subpop())]),
               " NAATs is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "NAAT (gonorrhea)"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$hepatitisB = renderText(
        paste0("The primary screening test for hepatitis B is the <b>hepatitis B surface antigen (HBsAg) test</b>. The false positive rate of each HBsAg test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "HBsAg"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "HBsAg"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "HBsAg"), which(colnames(lifetime.tests) == subpop())]),
               " HBsAg tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "HBsAg"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$hepatitisC = renderText(
        paste0("The primary screening test for hepatitis C is the <b>anti-HCV antibody test</b>. The false positive rate of each anti-HCV antibody test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Anti-HCV antibody"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Anti-HCV antibody"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Anti-HCV antibody"), which(colnames(lifetime.tests) == subpop())]),
               " anti-HCV antibody tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Anti-HCV antibody"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$hiv = renderText(
        paste0("The primary screening test for HIV is the <b>antigen/antibody test</b>. The false positive rate of each antigen/antibody test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Antigen/antibody"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Antigen/antibody"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Antigen/antibody"), which(colnames(lifetime.tests) == subpop())]),
               " antigen/antibody tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Antigen/antibody"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$lung = renderText(
        paste0("The primary screening modality for lung cancer is <b>low-dose computed tomography (LDCT)</b>. The false positive rate of each LDCT scan is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "Low-dose CT"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Low-dose CT"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "Low-dose CT"), which(colnames(lifetime.tests) == subpop())]),
               " low-dose CT scans is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "Low-dose CT"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$prostate = renderText(
        paste0("The primary screening modality for prostate cancer is <b>prostate-specific antigen (PSA) testing</b>. The false positive rate of each PSA test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "PSA"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "PSA"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "PSA"), which(colnames(lifetime.tests) == subpop())]),
               " PSA tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "PSA"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )
    
    output$syphilis = renderText(
        paste0("The primary screening test for syphilis is the <b>rapid plasma reagin (RPR) test</b>. The false positive rate of each RPR test is approximately <b>",
               formatC(round(100*compute()$Marginal.FP.prob[which(compute()$Marginal.FP.prob$Test == "RPR"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>. You should receive <b>",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "RPR"), which(colnames(lifetime.tests) == subpop())]),
               "</b> of these tests in your lifetime. The cumulative probability that you will receive a false positive from any of these ",
               as.numeric(lifetime.tests[which(lifetime.tests$`Screening test` == "RPR"), which(colnames(lifetime.tests) == subpop())]),
               " RPR tests is approximately <b>",
               formatC(round(100*compute()$Test.cumulative.FP.prob[which(compute()$Test.cumulative.FP.prob$Test == "RPR"), "Estimate"], 1), format = "f", digits = 1),
               "%</b>."
        )
    )

    output$lifetimeFP = renderText(
        paste0("<b>", formatC(round(100*as.numeric(compute()$Lifetime.FP.prob[1]), 1), format = "f", digits = 1), "% (± ",
               formatC(round(100*as.numeric(compute()$Lifetime.FP.prob[2]), 1), format = "f", digits = 1), "%)", "</b>")
    )
    
    output$instructionsforguidelines = renderText(
        paste0("<h4>Mark the boxes in the checklist below to learn more about the screening guidelines for certain cancers and STDs.",
               "<br><br>",
               "To get you started, we'll mark all the diseases for which you are recommended to get screened (as determined by the <font color = '2d802a'><b>CALCULATOR</b></font> tab):</h4>")
    )
    
    output$selecteddiseases = renderUI(
        if (input$sex == "Female" & input$smoke == "No" & input$pregnancy == 0) {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea",
                                            "Hepatitis C", "HIV"),
                               inline = FALSE)
        }
        else if (input$sex == "Female" & input$smoke == "No" & input$pregnancy > 0) {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                            "Hepatitis C", "HIV", "Syphilis"),
                               inline = FALSE)
        }
        else if (input$sex == "Female" & input$smoke == "Yes" & input$pregnancy == 0) {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea",
                                            "Hepatitis C", "HIV", "Lung cancer"),
                               inline = FALSE)
        }
        else if (input$sex == "Female" & input$smoke == "Yes" & input$pregnancy > 0) {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                            "Hepatitis C", "HIV", "Lung cancer", "Syphilis"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "No" & input$MSM == "No" & input$prostate == "No") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Colorectal cancer", "Hepatitis C", "HIV"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "Yes" & input$MSM == "No" & input$prostate == "No") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Colorectal cancer", "Hepatitis C", "HIV", "Lung cancer"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "No" & input$MSM == "Yes" & input$prostate == "No") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis C", "HIV", "Syphilis"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "Yes" & input$MSM == "Yes" & input$prostate == "No") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis C", "HIV", "Lung cancer", "Syphilis"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "No" & input$MSM == "No" & input$prostate == "Yes") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Colorectal cancer", "Hepatitis C", "HIV", "Prostate cancer"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "Yes" & input$MSM == "No" & input$prostate == "Yes") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Colorectal cancer", "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "No" & input$MSM == "Yes" & input$prostate == "Yes") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis C", "HIV", "Prostate cancer", "Syphilis"),
                               inline = FALSE)
        }
        else if (input$sex == "Male" & input$smoke == "Yes" & input$MSM == "Yes" & input$prostate == "Yes") {
            checkboxGroupInput("mydiseases", label = NULL,
                               choices = c("Breast cancer", "Cervical cancer", "Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis B",
                                           "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               selected = c("Chlamydia", "Colorectal cancer", "Gonorrhea", "Hepatitis C", "HIV", "Lung cancer", "Prostate cancer", "Syphilis"),
                               inline = FALSE)
        }
    )
    
    output$guidelinesbreast = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/breast-cancer-screening", target = "_blank"),
               " recommends that <b>women</b> get screened for breast cancer <b>every two years between the ages of 50 and 74</b>. Women can also choose to start this biennial screening earlier, between the ages of 40 and 49. The primary screening modality for breast cancer is <b>mammography</b>.")
    )
    
    output$guidelinescervical = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/cervical-cancer-screening", target = "_blank"),
               " recommends that <b>women</b> get screened for cervical cancer with a <b>Pap test</b> every three years between the ages of <b>21 and 29</b>. For women between the ages of <b>30 and 65</b>, the USPSTF recommends screening with a Pap test every three years, a <b>high-risk HPV (hrHPV) test</b> every five years, or <b>cotesting (Pap test + hrHPV test)</b> every five years.")
    )
    
    output$guidelineschlamydia = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/chlamydia-and-gonorrhea-screening", target = "_blank"),
               " and ", a("CDC", href = "https://www.cdc.gov/std/treatment-guidelines/screening-recommendations.htm", target = "_blank"), 
               " recommend screening for chlamydia in <b>sexually active women</b> under age 25 and in women 25 or older who have a higher risk of infection. The CDC also recommends screening for chlamydia in <b>men who have sex with men (MSM)</b>. For this analysis, we assume that women and MSM receive one test for every sexual partner they have in their lifetime — ",
               a("four", href = "https://www.cdc.gov/nchs/nsfg/key_statistics/n_2015-2017.htm", target = "_blank"),
               " for women and ",
               a("six", href = "https://pubmed.ncbi.nlm.nih.gov/27386950/", target = "_blank"),
               " for MSM. The primary screening test for chlamydia is the <b>nucleic acid amplification test (NAAT)</b>.")
    )
    
    output$guidelinescolorectal = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/colorectal-cancer-screening", target = "_blank"),
               " recommends screening for colorectal cancer in <b>adults aged 45 to 75</b>. Individuals can choose to continue getting screened for colorectal cancer between the ages of 76 and 85, if they wish.",
               " The primary screening modality for colorectal cancer is <b>colonoscopy</b>, which should be conducted <b>every ten years</b>.",
               " Stool-based screening tests for colorectal cancer — namely the gFOBT (every year), FIT (every year), and sDNA-FIT (every one to three years) — are also relatively common. CT colonography (every five years) and flexible sigmoidoscopy (every five years) are two other options.")
    )
    
    output$guidelinesgonorrhea = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/chlamydia-and-gonorrhea-screening", target = "_blank"),
               " and ", a("CDC", href = "https://www.cdc.gov/std/treatment-guidelines/screening-recommendations.htm", target = "_blank"), 
               " recommend screening for gonorrhea in <b>sexually active women</b> under age 25 and in women 25 or older who have a higher risk of infection. The CDC also recommends screening for gonorrhea in <b>men who have sex with men (MSM)</b>. For this analysis, we assume that women and MSM receive one test for every sexual partner they have in their lifetime — ",
               a("four", href = "https://www.cdc.gov/nchs/nsfg/key_statistics/n_2015-2017.htm", target = "_blank"),
               " for women and ",
               a("six", href = "https://pubmed.ncbi.nlm.nih.gov/27386950/", target = "_blank"),
               " for MSM. The primary screening test for gonorrhea is the <b>nucleic acid amplification test (NAAT)</b>.")
    )
    
    output$guidelineshepatitisB = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/hepatitis-b-virus-infection-screening", target = "_blank"),
               " recommends screening for hepatitis B in <b>individuals at increased risk for infection</b> and in all <b>",
               a("pregnant women", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/hepatitis-b-virus-infection-in-pregnant-women-screening", target = "_blank"),
               "</b> at the first prenatal visit, regardless of prior hepatitis B testing or vaccination. For this analysis, we assume individuals to be at average risk for infection, so we assume <b>one test per pregnancy</b> for pregnant women and zero lifetime tests for everyone else.",
               " The primary screening test for hepatitis B is the <b>hepatitis B surface antigen (HBsAg) test</b>.")
    )
    
    output$guidelineshepatitisC = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/hepatitis-c-screening", target = "_blank"),
               " recommends hepatitis C screening for <b>adults between the ages of 18 and 79</b>. <b>One-time screening</b> is sufficient for most adults, but individuals at high risk for infection might benefit from repeated screening.",
               " The USPSTF recommendation for hepatitis C screening includes <b>pregnant women</b>. According to the ",
               a("CDC", href = "https://www.cdc.gov/nchhstp/pregnancy/screening/clinician-timeline.html", target = "_blank"),
               ", women should be screened for hepatitis C <b>once per pregnancy</b>, at the first prenatal visit. The primary screening test for hepatitis C is the <b>anti-HCV antibody test</b>.")
    )
    
    output$guidelineshiv = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/human-immunodeficiency-virus-hiv-infection-screening", target = "_blank"),
               " recommends screening for HIV in <b>adolescents and adults between the ages of 15 and 65</b>, as well as in <b>pregnant women</b>. There is limited evidence about the optimal interval for HIV screening; <b>one-time screening</b> might be sufficient for individuals at average risk for infection.",
               " According to the ",
               a("CDC", href = "https://www.cdc.gov/nchhstp/pregnancy/screening/clinician-timeline.html", target = "_blank"),
               ", women should be screened <b>once per pregnancy</b> at the first prenatal visit, and pregnant women at high risk for infection may benefit from repeated screening.",
               " The primary screening test for HIV is the <b>antigen/antibody test</b>. Rapid tests — usually antibody or antigen/antibody — are widely available.")
    )
    
    output$guidelineslung = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/lung-cancer-screening", target = "_blank"),
               " recommends <b>annual screening</b> for lung cancer in adults between the ages of <b>50 and 80</b> who <b>(1) have a 20 pack-year smoking history</b> and <b>(2) currently smoke or have quit within the past 15 years</b>.",
               " Once a person hasn't smoked for 15 years or develops a serious medical condition that would prevent them from having lung surgery, they should no longer be screened for lung cancer.",
               " The primary screening test for lung cancer is the <b>low-dose CT (LDCT) scan</b>. Uptake of lung cancer screening is low, so for this analysis, we make the conservative assumption that individuals meeting the USPSTF criteria receive only one LDCT scan in their lifetime.")
    )
    
    output$guidelinesprostate = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/prostate-cancer-screening", target = "_blank"),
               " states that screening for prostate cancer is optional for <b>men</b> between the ages of <b>55 and 69</b>, and recommends against prostate screening in men aged 70 or older.",
               " There is no conclusive evidence about the optimal screening interval for prostate cancer. For this analysis, we assume that men who choose to get screened for prostate cancer are tested <b>every two years</b>, a relatively conservative estimate based on the USPSTF recommendation and the ",
               a("American Cancer Society guidelines", href = "https://www.cancer.org/cancer/prostate-cancer/detection-diagnosis-staging/acs-recommendations.html", target = "_blank"),
               ". The primary screening modality for prostate cancer is <b>prostate-specific antigen (PSA) testing</b>.")
    )
    
    output$guidelinessyphilis = renderText(
        paste0("The ", a("U.S. Preventive Services Task Force (USPSTF)", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/syphilis-infection-in-nonpregnant-adults-and-adolescents", target = "_blank"),
               " recommends screening for syphilis in <b>individuals at increased risk for infection</b> — including <b>men who have sex with men (MSM)</b> — and in all <b>",
               a("pregnant women", href = "https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/syphilis-infection-in-pregnancy-screening", target = "_blank"),
               "</b>, as early as possible in their prenatal care. The ",
               a("CDC", href = "https://www.cdc.gov/nchhstp/pregnancy/screening/clinician-timeline.html", target = "_blank"),
               " recommends screening for syphilis in all pregnant women at the first prenatal visit and suggests that repeat screening may be beneficial for some pregnant women.",
               " For this analysis, we assume <b>one test per pregnancy</b> for pregnant women. For MSM, we assume one test per sexual partner, and we estimate that they have ",
               a("six", href = "https://pubmed.ncbi.nlm.nih.gov/27386950/", target = "_blank"),
               " sexual partners in their lifetime. The primary screening tests for syphilis are the <b>rapid plasma reagin (RPR) test</b> and the <b>Venereal Disease Research Laboratory (VDRL) test</b>.")
    )
    
    output$table = renderReactable(
        reactable(studies[which(studies$`Screening test` == "Mammography" |
                                    studies$`Screening test` == "Pap" |
                                    studies$`Screening test` == "NAAT (chlamydia)" |
                                    studies$`Screening test` == "Colonoscopy" |
                                    studies$`Screening test` == "NAAT (gonorrhea)" |
                                    studies$`Screening test` == "HBsAg" |
                                    studies$`Screening test` == "Anti-HCV antibody" |
                                    studies$`Screening test` == "Antigen/antibody" |
                                    studies$`Screening test` == "Low-dose CT" |
                                    studies$`Screening test` == "PSA" |
                                    studies$`Screening test` == "RPR"),
                          c(1:4, 6:7, 5, 8:12)],
                  bordered = TRUE,
                  highlight = TRUE,
                  resizable = FALSE,
                  filterable = FALSE,
                  searchable = TRUE,
                  defaultPageSize = 35,
                  showSortable = TRUE,
                  wrap = FALSE,
                  columns = list(Disease = colDef(minWidth = 140,
                                                  cell = function(value, index) {
                                                      # Render as a link
                                                      a(href = paste(studies[which(studies$`Screening test` == "Mammography" |
                                                                                       studies$`Screening test` == "Pap" |
                                                                                       studies$`Screening test` == "NAAT (chlamydia)" |
                                                                                       studies$`Screening test` == "Colonoscopy" |
                                                                                       studies$`Screening test` == "NAAT (gonorrhea)" |
                                                                                       studies$`Screening test` == "HBsAg" |
                                                                                       studies$`Screening test` == "Anti-HCV antibody" |
                                                                                       studies$`Screening test` == "Antigen/antibody" |
                                                                                       studies$`Screening test` == "Low-dose CT" |
                                                                                       studies$`Screening test` == "PSA" |
                                                                                       studies$`Screening test` == "RPR"),][index, "USPSTF recommendation"]), target = "_blank", value)
                                                  }),
                                 'Screening test' = colDef(minWidth = 140),
                                 'Study ID' = colDef(minWidth = 140,
                                                     cell = function(value, index) {
                                                         # Render as a link
                                                         a(href = paste(studies[which(studies$`Screening test` == "Mammography" |
                                                                                          studies$`Screening test` == "Pap" |
                                                                                          studies$`Screening test` == "NAAT (chlamydia)" |
                                                                                          studies$`Screening test` == "Colonoscopy" |
                                                                                          studies$`Screening test` == "NAAT (gonorrhea)" |
                                                                                          studies$`Screening test` == "HBsAg" |
                                                                                          studies$`Screening test` == "Anti-HCV antibody" |
                                                                                          studies$`Screening test` == "Antigen/antibody" |
                                                                                          studies$`Screening test` == "Low-dose CT" |
                                                                                          studies$`Screening test` == "PSA" |
                                                                                          studies$`Screening test` == "RPR"),][index, "Source"]), target = "_blank", value)
                                                     }),
                                 TP = colDef(minWidth = 80),
                                 FN = colDef(minWidth = 80),
                                 TN = colDef(minWidth = 80),
                                 FP = colDef(minWidth = 80),
                                 N = colDef(minWidth = 80),
                                 Sensitivity = colDef(format = colFormat(percent = TRUE, digits = 1), minWidth = 100),
                                 Specificity = colDef(format = colFormat(percent = TRUE, digits = 1), minWidth = 100),
                                 PPV = colDef(format = colFormat(percent = TRUE, digits = 1), minWidth = 80),
                                 NPV = colDef(format = colFormat(percent = TRUE, digits = 1), minWidth = 80)),
                  defaultColDef = colDef(
                      align = "center",
                      headerStyle = list(background = "#f2f2f2"),
                  ),
                  theme = reactableTheme(
                      searchInputStyle = list(width = "40%"),
                  ))
    )
}





##### Run app
shinyApp(ui, server)




