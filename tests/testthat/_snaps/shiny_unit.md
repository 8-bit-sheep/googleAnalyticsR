# Shiny unit tests

    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-accounts-label" for="test1-accounts">Accounts</label>
      <div>
        <select id="test1-accounts"></select>
        <script type="application/json" data-for="test1-accounts" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>
    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-web.prop-label" for="test1-web.prop">WebProperty</label>
      <div>
        <select id="test1-web.prop"></select>
        <script type="application/json" data-for="test1-web.prop" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>
    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-view-label" for="test1-view">Select View</label>
      <div>
        <select id="test1-view"></select>
        <script type="application/json" data-for="test1-view" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>

---

    <div class="col-sm-4">
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test1-accounts-label" for="test1-accounts">Accounts</label>
        <div>
          <select id="test1-accounts"></select>
          <script type="application/json" data-for="test1-accounts" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
    </div>
    <div class="col-sm-4">
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test1-web.prop-label" for="test1-web.prop">WebProperty</label>
        <div>
          <select id="test1-web.prop"></select>
          <script type="application/json" data-for="test1-web.prop" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
    </div>
    <div class="col-sm-4">
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test1-view-label" for="test1-view">Select View</label>
        <div>
          <select id="test1-view"></select>
          <script type="application/json" data-for="test1-view" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
    </div>

---

    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-multi_select-label" for="test1-multi_select">Metric</label>
      <div>
        <select id="test1-multi_select" multiple="multiple"></select>
        <script type="application/json" data-for="test1-multi_select">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>

---

    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-account_name-label" for="test1-account_name">Accounts</label>
      <div>
        <select id="test1-account_name"></select>
        <script type="application/json" data-for="test1-account_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>
    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-property_name-label" for="test1-property_name">Select Property</label>
      <div>
        <select id="test1-property_name"></select>
        <script type="application/json" data-for="test1-property_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>

---

    <div class="col-sm-6">
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test1-account_name-label" for="test1-account_name">Accounts</label>
        <div>
          <select id="test1-account_name"></select>
          <script type="application/json" data-for="test1-account_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
    </div>
    <div class="col-sm-6">
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test1-property_name-label" for="test1-property_name">Select Property</label>
        <div>
          <select id="test1-property_name"></select>
          <script type="application/json" data-for="test1-property_name" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
    </div>

---

    <div class="form-group shiny-input-container">
      <label class="control-label" id="test1-multi_select-label" for="test1-multi_select">Metric</label>
      <div>
        <select id="test1-multi_select" multiple="multiple"></select>
        <script type="application/json" data-for="test1-multi_select">{"plugins":["selectize-plugin-a11y"]}</script>
      </div>
    </div>

---

    [1] "## basic ui.R"                           
    [2] "fluidPage(title = \"{{ shiny_title }}\","
    [3] "                {{ auth_ui }},"          
    [4] "                {{{ date_range }}},"     
    [5] "                h2(\"Model Output\"),"   
    [6] "{{{ model_ui }}}"                        
    [7] ")"                                       

---

     [1] "# ---start header_boilerplate.R"                                                
     [2] "library(shiny)"                                                                 
     [3] "library(googleAuthR)"                                                           
     [4] "library(googleAnalyticsR)"                                                      
     [5] "library(googleAnalyticsR)"                                                      
     [6] ""                                                                               
     [7] "gar_set_client(web_json = \"dummy_web.json\","                                  
     [8] "               scopes = \"https://www.googleapis.com/auth/analytics.readonly\")"
     [9] "options(googleAuthR.redirect = \"\")"                                           
    [10] ""                                                                               
    [11] "# loads pre-existing models"                                                    
    [12] "# ---end header_boilerplate.R"                                                  
    [13] ""                                                                               
    [14] "## basic ui.R"                                                                  
    [15] "fluidPage(title = \"ga_model_shiny\","                                          
    [16] "                authDropdownUI('auth_menu', inColumns = TRUE),"                 
    [17] "                dateRangeInput(\"date_range\", \"Date Range\", "                
    [18] "          start = Sys.Date() - 400, end = Sys.Date() - 1),"                     
    [19] "                h2(\"Model Output\"),"                                          
    [20] "model1$ui('model1')"                                                            
    [21] ")"                                                                              

---

     [1] "# ---start server_boilerplate.R"                                  
     [2] "# loads pre-existing models"                                      
     [3] ""                                                                 
     [4] "function(input, output, session){"                                
     [5] "  "                                                               
     [6] "  token <- gar_shiny_auth(session)"                               
     [7] "  "                                                               
     [8] "  al <- reactive({req(token);ga_account_list()})"                 
     [9] "  "                                                               
    [10] "  # module for authentication"                                    
    [11] "  view_id <- callModule(authDropdown, 'auth_menu', ga.table = al)"
    [12] "  "                                                               
    [13] "  # module to display model results"                              
    [14] "  model1$server('model1', view_id = view_id, "                    
    [15] "                         date_range = reactive(input$date_range))"
    [16] "  "                                                               
    [17] "}"                                                                
    [18] "# ---end server_boilerplate.R"                                    

