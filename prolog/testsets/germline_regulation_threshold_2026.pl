% ============================================================================
% CONSTRAINT STORY: germline_regulation_threshold_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "We can rewrite our genetic code" by Michael Le Page
% ============================================================================

:- module(germline_regulation_threshold_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: germline_regulation_threshold_2026
 * human_readable: International Germline Regulatory Threshold
 * domain: political/technological
 * temporal_scope: 2026-2040
 * spatial_scope: Global (International Governance)
 * * SUMMARY:
 * This constraint focuses on the regulatory barrier preventing the routine use 
 * of CRISPR for germline (inheritable) editing. While somatic editing is 
 * approved, germline editing is currently restricted because it 
 * "isn't yet safe enough to attempt". The "threshold" represents 
 * the scientific and political consensus point where "safe enough" is reached.
 * * KEY AGENTS:
 * - Global Regulator: (Institutional) - Enforces the moratorium until safety 
 * benchmarks are met.
 * - Biotech Innovator: (Individual Powerful) - Views the threshold as a 
 * bottleneck for "better versions of ourselves".
 * - Future Progeny Advocate: (Analytical/Powerless) - Views the threshold 
 * as a protective shield against "errors".
 * * NARRATIVE ARC:
 * The success of somatic treatments like Victoria Gray's puts 
 * pressure on the germline threshold. As clinical trials increase, 
 * regulators must decide if the "rewrite" capability should extend to 
 * future generations.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(germline_regulation_threshold_2026, 0, 10).
narrative_ontology:constraint_claim(germline_regulation_threshold_2026, snare).

% Base extractiveness score: 0.5 (Moderate)
% Rationale: Regulation extracts time and capital from developers to ensure 
% safety, but also prevents "errors" from being hard-coded into 
% the human lineage, which is a form of protection.
domain_priors:base_extractiveness(germline_regulation_threshold_2026, 0.5).

% Suppression score: 0.8 (High)
% Rationale: Non-compliant germline research is actively suppressed or 
% de-funded globally until the "safety threshold" is met.
domain_priors:suppression_score(germline_regulation_threshold_2026, 0.8).

% Enforcement: Requires active enforcement
% Rationale: Regulatory bodies must actively monitor and approve CRISPR 
% therapies.
domain_priors:requires_active_enforcement(germline_regulation_threshold_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(germline_regulation_threshold_2026, human_genetic_integrity).
constraint_victim(germline_regulation_threshold_2026, radical_enhancement_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GLOBAL REGULATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power)
   WHEN: generational (Thinking about "altering our children's genomes")
   WHERE: mobile (Can adjust thresholds as safety data arrives)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the regulator, the threshold is a "Rope"—a coordination mechanism 
   used to guide the technology safely toward a future where it is 
   "routinely used".
   
   NARRATIVE EVIDENCE:
   The technology "isn't yet safe enough," implying a deliberate 
   hold-and-release strategy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: BIOTECH INNOVATOR - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Significant influence and capital)
   WHEN: biographical (Seeking ROI on "extraordinarily powerful tools")
   WHERE: constrained (Must wait for safety approvals)
   SCOPE: national/regional
   
   WHY THIS CLASSIFICATION:
   For the innovator, the threshold acts as a "Snare." It strangles the 
   commercialization of germline editing even though the "rewrite" capability 
   already exists.
   
   NARRATIVE EVIDENCE:
   "It does seem likely that in the future, CRISPR will be routinely used," 
   yet the current "not safe enough" status prevents immediate use.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    snare,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNBORN PROGENY - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (No agency in their own genome editing)
   WHEN: civilizational (Permanent changes to genetic code)
   WHERE: trapped (No exit from their own DNA)
   SCOPE: local (Biological self)
   
   WHY THIS CLASSIFICATION:
   For the subject being edited, the regulatory threshold is a "Mountain." 
   It is the only unchangeable barrier protecting their biological 
   blueprint from "errors" until absolute safety is proven.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE BIOETHICIST - Tangled Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the ethical and social implications)
   WHEN: historical (Considering long-term societal impact)
   WHERE: analytical (Detached from commercial or regulatory pressures)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the bioethicist, the regulation is a "Tangled Rope." It's a necessary
   coordination mechanism (Rope) to prevent harm, but it also creates
   unintended consequences, such as stifling innovation that could prevent
   hereditary diseases, and raises complex questions about equity and access
   (Snare-like extractive potential).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    germline_regulation_threshold_2026,
    tangled_rope,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(germline_regulation_threshold_2026_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that the germline regulation threshold is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % Global Regulator (Rope)
    constraint_indexing:constraint_classification(
        germline_regulation_threshold_2026,
        Type1,
        context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))
    ),
    % Biotech Innovator (Snare)
    constraint_indexing:constraint_classification(
        germline_regulation_threshold_2026,
        Type2,
        context(agent_power(individual_powerful), time_horizon(biographical), exit_options(constrained), spatial_scope(national))
    ),
    % Unborn Progeny (Mountain)
    constraint_indexing:constraint_classification(
        germline_regulation_threshold_2026,
        Type3,
        context(agent_power(individual_powerless), time_horizon(civilizational), exit_options(trapped), spatial_scope(local))
    ),
    % Bioethicist (Tangled Rope)
    constraint_indexing:constraint_classification(
        germline_regulation_threshold_2026,
        Type4,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3,
    Type3 \= Type4.

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that innovators (powerful) experience higher extraction (of opportunity) than regulators.
 */
test(power_extractiveness_scaling) :-
    ContextInnovator = context(agent_power(individual_powerful), time_horizon(biographical), exit_options(constrained), spatial_scope(national)),
    ContextRegulator = context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(germline_regulation_threshold_2026, ContextInnovator, Score1),
    constraint_indexing:extractiveness_for_agent(germline_regulation_threshold_2026, ContextRegulator, Score2),
    Score1 > Score2.  % The innovator experiences more extraction in this case.

/**
 * TEST 3: Domain-specific insight - The "Safety Threshold"
 * Demonstrates that the definition of "safe enough" is the central point of contention.
 */
test(safety_threshold_insight) :-
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, ClassificationInnovator, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(germline_regulation_threshold_2026, ClassificationRegulator, context(agent_power(institutional), _, _, _)),
    ClassificationInnovator = snare,
    ClassificationRegulator = rope.

:- end_tests(germline_regulation_threshold_2026_tests).

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
 * 1. PERSPECTIVE GAP: The conflict lies between the Innovator (who sees 
 * the threshold as an arbitrary Snare) and the Progeny (who sees it as 
 * a protective Mountain).
 * 
 * 2. REGULATORY ANALYSIS: The Omega variable (safety threshold) is what 
 * prevents the Rope from becoming a Snare for society at large. 
 * If the threshold is set too low, it becomes a "Snare" (permanent errors); 
 * if set too high, it becomes a "Mountain" (denied cures).
 * 
 * 3. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        germline_editing_safety_threshold,
 *        "At what off-target mutation rate does 'not safe enough' become 'routinely safe'?",
 *        resolution_mechanism("Meta-analysis of hundreds of clinical trials to establish parity with natural mutation rates"),
 *        impact("If resolved low: Rapid transition to a global Rope. If resolved high: Remains an institutional Mountain."),
 *        confidence_without_resolution(medium)
 *    ).
 * 
 *    omega_variable(
 *        regulatory_arbitrage_risk,
 *        "Will 'bio-haven' nations lower the threshold early, creating a Snare for international harmonization?",
 *        resolution_mechanism("Monitoring of national approval timelines for the first germline therapies"),
 *        impact("If arbitrage occurs: The international 'Rope' breaks, leading to a fragmented Mountain/Snare landscape."),
 *        confidence_without_resolution(low)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Permanent Global Ban
 *    Viability: Historically the default for germline editing.
 *    Suppression: Being eroded by the "likely" future routine use mentioned 
 *    in the source.
 * 
 * ALTERNATIVE 2: Somatic-Only Limitation
 *    Viability: Current status quo.
 * 
 * CONCLUSION:
 * The "Safety Threshold" Omega is the only factor determining if the world 
 * stays at Alternative 2 or moves to full germline "rewriting".
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/germline_regulation_threshold_2026].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(germline_regulation_threshold_2026).
 * 
 * 3. Run tests:
 *    ?- run_tests(germline_regulation_threshold_2026_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(germline_regulation_threshold_2026).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(germline_regulation_threshold_2026, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
