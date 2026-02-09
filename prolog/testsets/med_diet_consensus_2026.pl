% ============================================================================
% CONSTRAINT STORY: med_diet_consensus_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The one diet that’s good for everything" by Carissa Wong (Jan 2026)
% ============================================================================

:- module(constraint_med_diet_consensus_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: med_diet_consensus_2026
 * human_readable: Mediterranean Diet Scientific Hegemony
 * domain: health/scientific/economic
 * temporal_scope: 1999-2026 (The "RCT Era" of nutrition)
 * spatial_scope: Global (Scientific/Medical community)
 * * SUMMARY:
 * The establishment of the Mediterranean diet as the "gold standard" of nutrition 
 * through Randomized Controlled Trials (RCTs). This creates a normative health 
 * constraint where alternative dietary patterns are classified as "fads" or 
 * "sub-optimal," influencing insurance, public health policy, and social status.
 * * KEY AGENTS:
 * - Institutional Scientist: (e.g., Luigi Fontana) - Views the diet as a verified 
 * biological tool for longevity.
 * - Health-Conscious Citizen: (Individual Moderate) - Experiences the diet as a 
 * voluntary coordination mechanism for better health.
 * - Low-Income Subject: (Individual Powerless) - Experiences the diet as a 
 * normative "Snare" due to the high cost of fresh legumes, fish, and olive oil.
 * * NARRATIVE ARC:
 * From 1940s observation (Ancel Keys) to 1999 heart attack trials, to 2026 
 * consensus. The constraint moves from a "suggestion" to a "scientific truth" 
 * that dictates what is considered "delicious" and "planetary-friendly."
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(med_diet_consensus_2026, 0, 10).
narrative_ontology:constraint_claim(med_diet_consensus_2026, snare).

% Base extractiveness score: 0.2 (Low)
% Rationale: The primary beneficiary is the subject's own health and the planet. 
% However, there is mild asymmetry in "health-washing" and the premium pricing 
% of Mediterranean staples.
domain_priors:base_extractiveness(med_diet_consensus_2026, 0.2).

% Suppression score: 0.4 (Moderate)
% Rationale: The text explicitly dismisses "dietary fads" and contrasts the 
% Mediterranean diet with the "low-fat" alternative which was beaten in RCTs. 
% Alternatives are visible but scientifically delegitimized.
domain_priors:suppression_score(med_diet_consensus_2026, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(med_diet_consensus_2026, extractiveness, 0.2).
narrative_ontology:constraint_metric(med_diet_consensus_2026, suppression_requirement, 0.4).

% Enforcement: Emerges naturally through consensus/scientific validation.
domain_priors:emerges_naturally(med_diet_consensus_2026).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, global_healthcare_systems).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, planetary_ecology).
narrative_ontology:constraint_victim(med_diet_consensus_2026, processed_food_industry).
narrative_ontology:constraint_victim(med_diet_consensus_2026, low_income_populations). % (Economic extraction via food-pricing)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: INSTITUTIONAL SCIENTIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making/Scientific consensus)
   WHEN: generational (Thinking in terms of decades of evidence)
   WHERE: mobile (Can choose different research focuses)
   SCOPE: global (The "Scientific Gold Standard")
   
   WHY THIS CLASSIFICATION:
   The scientist sees the diet as a "Rope"—a beneficial coordination mechanism 
   grounded in RCT data. It is a tool to be used to improve public health 
   outcomes, with high degrees of freedom for the expert to recommend or tweak.
   
   NARRATIVE EVIDENCE:
   "It’s not only healthy, it’s also extremely tasty... established it as 
   the scientific gold standard."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    med_diet_consensus_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: LOW-INCOME CITIZEN - Snare
   --------------------------------------------------------------------------
   WHO: powerless (No control over food supply/pricing)
   WHEN: biographical (Short-term survival/budget)
   WHERE: trapped (Living in food deserts or economic constraints)
   SCOPE: local (Limited to local supermarket inventory)
   
   WHY THIS CLASSIFICATION:
   For those who cannot afford fresh fish, nuts, and high-quality olive oil, 
   the "Gold Standard" acts as a Snare. It is a normative requirement for 
   health that is economically inaccessible, creating a sense of failure or 
   systemic exclusion while the "experts" celebrate its deliciousness.
   
   NARRATIVE EVIDENCE:
   The article focuses on "fibre, vegetables, legumes, fruit, nuts, fish" 
   without addressing the income levels of participants in the Seven Countries 
   Study, which Ancel Keys ignored.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    med_diet_consensus_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of physiology)
   WHEN: civilizational (Evolutionary human metabolism)
   WHERE: analytical (Universal application)
   SCOPE: global (Biological reality)
   
   WHY THIS CLASSIFICATION:
   From an evolutionary biology perspective, the human body's positive response 
   to these specific nutrient ratios (Omega-3s, fibre) is a "Mountain." It is 
   not a choice but a fixed biological law of human health that we must 
   navigate, similar to the law of gravity.
   
   NARRATIVE EVIDENCE:
   "Time and time again, scientists have found that one diet beats all others... 
   backed up by decades of evidence."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    med_diet_consensus_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================

   4. TESTS (What We Learn About Constraints)

   ========================================================================== */



:- begin_tests(med_diet_consensus_2026_tests).



/**

 * TEST 1: Multi-perspective variance

 * Demonstrates that the Mediterranean diet is viewed differently across agents.

 */

test(multi_perspective_variance) :-

    % Institutional Scientist (Rope)

    constraint_indexing:constraint_classification(

        med_diet_consensus_2026,

        Type1,

        context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))

    ),

    % Low-Income Subject (Snare)

    constraint_indexing:constraint_classification(

        med_diet_consensus_2026,

        Type2,

        context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))

    ),

    % Biological Analyst (Mountain)

    constraint_indexing:constraint_classification(

        med_diet_consensus_2026,

        Type3,

        context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))

    ),

    % Verify they differ

    Type1 \= Type2,

    Type2 \= Type3,

    Type1 \= Type3. % Ensure all three are distinct



/**

 * TEST 2: Power-based extractiveness scaling

 * Demonstrates that low-income subjects (powerless) experience higher extraction (economic inaccessibility) than institutional scientists.

 */

test(power_extractiveness_scaling) :-

    ContextPowerless = context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local)),

    ContextPowerful = context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global)),

    constraint_indexing:extractiveness_for_agent(med_diet_consensus_2026, ContextPowerless, Score1),

    constraint_indexing:extractiveness_for_agent(med_diet_consensus_2026, ContextPowerful, Score2),

    Score1 > Score2.  % Powerless experience more extraction



/**

 * TEST 3: Time-horizon immutability

 * Demonstrates that while individual choices are flexible, biological realities are immutable over long time horizons.

 */

test(time_immutability) :-

    % Short horizon (biographical) sees it as a chore/choice (Rope)

    constraint_indexing:effective_immutability(time_horizon(biographical), exit_options(mobile), rope),

    % Long horizon (civilizational) sees it as a fixed biological law (Mountain).

    constraint_indexing:effective_immutability(time_horizon(civilizational), exit_options(analytical), mountain).



/**

 * TEST 4: Domain-specific insight - Economic Accessibility as a Constraint

 * Demonstrates that economic factors can transform a scientifically proven "Rope" into a "Snare".

 */

test(economic_accessibility) :-

    constraint_indexing:constraint_classification(med_diet_consensus_2026, ClassificationLowIncome, context(agent_power(powerless), _, _, _)),

    constraint_indexing:constraint_classification(med_diet_consensus_2026, ClassificationScientist, context(agent_power(institutional), _, _, _)),

    ClassificationLowIncome = snare,

    ClassificationScientist = rope.



:- end_tests(med_diet_consensus_2026_tests).



/* ==========================================================================

   5. MODEL INTERPRETATION (Commentary)

   ========================================================================== */



/**

 * LLM GENERATION NOTES

 * 

 * Model: Gemini 2.0 Flash

 * Date: 2026-01-23

 * 

 * KEY DECISIONS:

 * 

 * 1. BASE EXTRACTIVENESS (0.2): Chose low because the diet genuinely helps the individual. 

 * The "extraction" is largely symbolic (social pressure) or economic 

 * (market premium on Mediterranean goods).

 * 

 * 2. PERSPECTIVE SELECTION:

 *    Highlighted the "Low-Income Subject" as a Snare because dietary advice often 

 *    ignores the material constraints of poverty, effectively punishing those who 

 *    can't comply. Contrasted this with the Institutional Scientist (Rope) and 

 *    Biological Analyst (Mountain) to show the multi-faceted nature.

 * 

 * 3. OMEGAS 

 *    Define uncertainty so your analysis is cleaner

 *    omega_variable(

 *        med_diet_genetic_universality,

 *        "Does the Mediterranean diet benefit all genetic haplotypes equally, or is the 'gold standard' biased toward European metabolic history?",

 *        resolution_mechanism("Genome-wide association studies across diverse global populations (beyond the Seven Countries Study)"),

 *        impact("If biased: Snare for non-European populations. If universal: Mountain."),

 *        confidence_without_resolution(medium)

 *    ).

 * 

 *    omega_variable(

 *        med_diet_supply_chain_collapse,

 *        "Will climate change render the Mediterranean staples (olive oil, specific fish) so scarce that the diet becomes an elitist Snare?",

 *        resolution_mechanism("Monitoring agricultural yields in the Mediterranean basin vs. global demand over next 20 years"),

 *        impact("If scarce: Becomes a marker of class (Snare). If adaptable: Remains a Rope."),

 *        confidence_without_resolution(low)

 *    ).

 */



/* ==========================================================================

   6. ALTERNATIVE ANALYSIS (If Applicable)

   ========================================================================== */



/**

 * VIABLE ALTERNATIVES

 * 

 * ALTERNATIVE 1: Low-Fat Diet

 *    Viability: Was the standard in the late 20th century, representing a scientifically backed alternative approach.

 *    Suppression: Explicitly rejected by the 1999 RCT mentioned in the text, which scientifically delegitimized it in favor of the Mediterranean diet.

 * 

 * ALTERNATIVE 2: Ancestral/Regional Diets (e.g., Nordic, Japanese)

 *    Viability: High health outcomes reported in populations adhering to these diets, suggesting regional dietary patterns can be equally or more effective within their specific contexts.

 *    Suppression: Largely ignored by the "Mediterranean" branding and scientific consensus, leading to a monoculture of dietary advice that overlooks other effective, culturally relevant approaches.

 * 

 * CONCLUSION:

 * The scientific consensus around the Mediterranean diet, while well-intentioned, suppresses awareness and scientific exploration of equally viable alternative dietary patterns. This suppression, especially when combined with economic inaccessibility for some populations, solidifies its "Snare" aspect for those outside the "gold standard."

 */



/* ==========================================================================

   7. INTEGRATION HOOKS

   ========================================================================== */



/**

 * TO USE THIS CONSTRAINT:

 * 

 * 1. Load into main system:

 *    ?- [constraints/med_diet_consensus_2026].

 * 

 * 2. Run multi-perspective analysis:

 *    ?- constraint_indexing:multi_index_report(med_diet_consensus_2026).

 * 

 * 3. Run tests:

 *    ?- run_tests(med_diet_consensus_2026_tests).

 * 

 * 4. Generate pedagogical report:

 *    ?- pedagogical_report(med_diet_consensus_2026).

 * 

 * 5. Compare with other constraints:

 *    ?- compare_constraints(med_diet_consensus_2026, [other_id]).

 */



/* ==========================================================================

   END OF CONSTRAINT STORY

   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Extraction is substantive — the constraint's costs are real, not theatrical
domain_priors:theater_ratio(med_diet_consensus_2026, 0.17).
narrative_ontology:constraint_metric(med_diet_consensus_2026, theater_ratio, 0.17).
