% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Institutional Rent Extraction
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the tenure system in higher education from
 *   an 'institutional extraction' reading. It posits that tenure, while
 *   originally intended to protect academic freedom, has evolved into a
 *   mechanism for permanent rent extraction by early winners (tenured
 *   faculty), creating employment rigidity that prevents efficient resource
 *   reallocation and loads significant costs onto contingent labor and
 *   students. This reading emphasizes the economic and power dynamics over
 *   the idealized function of academic freedom.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '9fd16dc3-3356-4296-836b-f03ad7a68192').
narrative_ontology:cs_kernel_codification('9fd16dc3-3356-4296-836b-f03ad7a68192', formalized).
narrative_ontology:cs_authority_grounding('9fd16dc3-3356-4296-836b-f03ad7a68192', extraction).
narrative_ontology:cs_interpretation_layer_present('9fd16dc3-3356-4296-836b-f03ad7a68192').
narrative_ontology:cs_reading_relation('9fd16dc3-3356-4296-836b-f03ad7a68192', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fd16dc3-3356-4296-836b-f03ad7a68192', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('9fd16dc3-3356-4296-836b-f03ad7a68192', foundational, tenure_as_permanent_resource_claim).
narrative_ontology:cs_axiom_status(tenure_as_permanent_resource_claim, holdable).
narrative_ontology:cs_axiom_grounding('9fd16dc3-3356-4296-836b-f03ad7a68192', tenure_as_permanent_resource_claim, empirically_contingent).
narrative_ontology:cs_axiom('9fd16dc3-3356-4296-836b-f03ad7a68192', foundational, employment_rigidity_impedes_reallocation).
narrative_ontology:cs_axiom_status(employment_rigidity_impedes_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('9fd16dc3-3356-4296-836b-f03ad7a68192', employment_rigidity_impedes_reallocation, empirically_contingent).
narrative_ontology:cs_reference_frame('9fd16dc3-3356-4296-836b-f03ad7a68192', post_war_academic_expansion_era).
narrative_ontology:cs_drift_state('9fd16dc3-3356-4296-836b-f03ad7a68192', contemporary_neoliberal_university, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9fd16dc3-3356-4296-836b-f03ad7a68192', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent, high-status positions with significant autonomy and job security, effectively a lifetime claim on institutional resources. They benefit from the system's rigidity and the transfer of costs to other groups.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    institutional, biographical, arbitrage, national).

% Bear the costs of employment rigidity, working precarious, low-paid positions with limited benefits and no job security. They perform much of the teaching labor but are excluded from the benefits of tenure, making them direct victims of the system's extractive nature.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, constrained, local).

% Pay increasing tuition fees that subsidize the tenured system, often receiving instruction from contingent faculty. They bear the costs of reduced institutional flexibility and investment in new programs due to locked-in resources.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, local).

% Manages the tenure system, benefiting from the prestige associated with tenured faculty while using contingent labor to maintain flexibility and reduce costs. They enforce the rules that perpetuate the system, balancing institutional stability with financial pressures.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Subsidize public universities, indirectly funding the tenure system. They bear the costs of inefficient resource allocation and may see reduced returns on public investment in higher education due to employment rigidity.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, taxpayers, payer,
    organized, generational, mobile, national).

% Advocate for changes to the tenure system, arguing for greater flexibility, reduced reliance on contingent labor, and more equitable resource distribution. They analyze the system's economic and social impacts.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_reformers, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The system coordinates a stable academic workforce, ensuring a core of experienced faculty for teaching and research, and providing a clear career path for those who achieve tenure.
% TRANSFER_FUNCTION: Transfers a permanent claim on institutional resources (salary, benefits, research support) from the university's general budget (funded by tuition, grants, and public funds) to tenured faculty, while transferring instructional labor costs and employment precarity to contingent faculty.
% ABSENT_VOICES: Prospective faculty who are deterred by the lack of tenured positions, and students who would benefit from more flexible and responsive academic programs, are largely absent from the decision-making processes that perpetuate the current tenure structure.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, universities would immediately gain immense flexibility in resource allocation and employment. The academic labor market would be fundamentally reshaped, with a massive shift in power dynamics between faculty and administration, and a likely increase in faculty turnover. The entire structure of academic employment and university budgeting would rearrange.
% FOUNDING_PROBLEM: The tenure system was established to protect academic freedom, ensuring scholars could pursue controversial research and express unpopular opinions without fear of arbitrary dismissal, thereby promoting intellectual inquiry and the pursuit of truth.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and some university administrators attest that academic freedom remains a live problem requiring tenure's protection. Contingent faculty, students, and academic reformers argue that the problem of academic freedom is largely solved or that tenure no longer serves this function effectively, instead acting as a barrier to entry and a mechanism for rent extraction; their arguments are supported by labor market data and institutional budget analyses from outside the benefiting parties.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the substantial, permanent claim on resources by tenured faculty, disproportionate to current productivity or market value, and the costs borne by contingent faculty and students. Suppression (0.78) is high due to the structural barriers to entry for new faculty, the limited exit options for contingent faculty, and the institutional inertia that resists reform. The theater ratio (0.45) indicates that while some aspects of academic freedom protection remain, a significant portion of the system's maintenance is performative, masking its extractive function. The increasing trend in extractiveness and suppression over time reflects the growing reliance on contingent labor and rising tuition costs.
 *
 * PERSPECTIVAL GAP:
 *   The 'institutional extraction' reading sharply diverges from the 'academic freedom' reading. Tenured faculty, from their beneficiary seat, would likely perceive the system as a necessary 'rope' for intellectual inquiry. Contingent faculty and students, from their victim seats, experience it as a 'snare' of economic precarity and inflated costs. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (d=0.0-0.1) due to their job security and resource claims. Contingent faculty and students are primary victims (d=0.9-1.0) bearing the costs of precarity and tuition. University administration acts as an agenda-setter, benefiting from the system's stability and prestige while managing its extractive aspects (d=0.2-0.3). Taxpayers are diffuse payers (d=0.7-0.8) through public subsidies.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the founding problem of protecting academic freedom is contested, with strong evidence suggesting the system's primary function has drifted towards rent extraction. The persistence of the system, despite its costs, is maintained by the concentrated benefits to tenured faculty and the institutional power of university administration, rather than its original mandate. This prevents mislabeling it as a pure coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_freedom_vs_extraction_priority,
    'To what extent does tenure primarily protect academic freedom versus enabling rent extraction?',
    'Empirical studies correlating tenure status with research output, willingness to pursue controversial topics, and institutional financial models that disaggregate costs and benefits of tenured vs. contingent labor.',
    'If academic freedom protection is demonstrably low, the constraint''s ''snare'' classification is strengthened. If it''s high, the ''tangled_rope'' classification (hybrid coordination/extraction) might be more appropriate, acknowledging a genuine coordination function alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_extraction_priority, empirical, 'Ambiguity in the primary function of tenure.').

omega_variable(
    resource_reallocation_flexibility,
    'What is the actual cost of employment rigidity imposed by tenure on university budgets and program development?',
    'Comparative analysis of universities with different tenure policies (e.g., European vs. US models, or institutions with tenure-track vs. non-tenure-track faculty ratios) on metrics of program innovation, faculty hiring flexibility, and budget responsiveness.',
    'Higher demonstrated costs of rigidity would further support the ''snare'' classification by highlighting the negative externalities on institutional adaptability and resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_reallocation_flexibility, empirical, 'Economic impact of tenure-induced employment rigidity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of contingent faculty primarily structural (lack of tenured positions, limited bargaining power) or internalized (self-censorship, fear of reprisal)?',
    'Post-exit career trajectories and qualitative interviews with former contingent faculty: if precarity-induced self-censorship persists after leaving academia, it suggests internalized suppression. If suppression lifts immediately, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as contingent faculty carry the suppression with them even after exit, impacting their future careers and advocacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for contingent faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__institutional_extraction_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__institutional_extraction_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__institutional_extraction_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__institutional_extraction_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__institutional_extraction_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__institutional_extraction_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__institutional_extraction_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__institutional_extraction_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__institutional_extraction_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__institutional_extraction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__institutional_extraction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__institutional_extraction_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__institutional_extraction_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__institutional_extraction_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__institutional_extraction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, university_tuition_model).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_labor_market_precarity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tenure_contract' kernel. This 'institutional_extraction_reading' focuses on the economic and power dynamics, distinct from the 'academic_freedom_reading' and 'demographic_reproduction_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
