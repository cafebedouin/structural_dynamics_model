% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Precarity-Driven Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story describes 'flexible employment' from the 'precarity
 *   extraction' reading, where it functions as a snare. It highlights how the
 *   structural flexibility of gig work, while offering some autonomy,
 *   primarily serves to externalize costs and risks onto workers, enabling
 *   platform companies to extract surplus value. The narrative focuses on the
 *   mechanisms of algorithmic control, the absence of traditional labor
 *   protections, and the resulting precarity for workers. This is one reading
 *   of the 'flexible_employment_legitimacy' kernel, distinct from
 *   'market_efficiency_reading' and 'developmental_state_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.85).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Precarity-Driven Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, 'cf6829b5-c831-466f-9a92-1ecde4e927c7').
narrative_ontology:cs_kernel_codification('cf6829b5-c831-466f-9a92-1ecde4e927c7', formalized).
narrative_ontology:cs_authority_grounding('cf6829b5-c831-466f-9a92-1ecde4e927c7', extraction).
narrative_ontology:cs_interpretation_layer_present('cf6829b5-c831-466f-9a92-1ecde4e927c7').
narrative_ontology:cs_reading_relation('cf6829b5-c831-466f-9a92-1ecde4e927c7', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf6829b5-c831-466f-9a92-1ecde4e927c7', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('cf6829b5-c831-466f-9a92-1ecde4e927c7', foundational, labor_is_not_a_commodity).
narrative_ontology:cs_axiom_status(labor_is_not_a_commodity, holdable).
narrative_ontology:cs_axiom_grounding('cf6829b5-c831-466f-9a92-1ecde4e927c7', labor_is_not_a_commodity, deontological).
narrative_ontology:cs_axiom('cf6829b5-c831-466f-9a92-1ecde4e927c7', foundational, platform_control_constitutes_employment).
narrative_ontology:cs_axiom_status(platform_control_constitutes_employment, holdable).
narrative_ontology:cs_axiom_grounding('cf6829b5-c831-466f-9a92-1ecde4e927c7', platform_control_constitutes_employment, empirically_contingent).
narrative_ontology:cs_reference_frame('cf6829b5-c831-466f-9a92-1ecde4e927c7', post_industrial_labor_protections).
narrative_ontology:cs_drift_state('cf6829b5-c831-466f-9a92-1ecde4e927c7', contemporary_gig_economy_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cf6829b5-c831-466f-9a92-1ecde4e927c7', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_investors).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the platforms, setting terms of service, payment algorithms, and worker classification. They benefit from low labor costs, minimal overhead for benefits, and the ability to scale labor supply on demand. They actively lobby against reclassification of workers as employees.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_companies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Provide labor through platforms, often as their primary or supplementary income. They bear the costs of self-employment (no benefits, unstable income, lack of collective bargaining) while being subject to algorithmic management. Their 'flexibility' is often a necessity due to limited alternatives, and their identity as 'independent contractors' is reinforced by platform narratives.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, identity_locked, local).

% Public and private entities responsible for unemployment insurance, healthcare, and retirement. They bear the externalized costs of flexible employment as gig workers fall outside traditional benefit structures, increasing demand for public services without corresponding contributions from platforms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_providers, payer,
    institutional, generational, constrained, national).

% Fund platform companies, benefiting from business models that promise high returns through lean operations and flexible labor. Their investment decisions reinforce the current employment model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Seek to organize gig workers and advocate for improved labor protections, minimum wages, and benefits. They are often legally excluded from traditional collective bargaining with platform companies due to worker classification, and face significant structural barriers to organizing a dispersed workforce.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Analyze the economic impacts of flexible employment, including wage trends, income inequality, and the distribution of risk between workers and platforms. Their research often highlights the extractive nature of the model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to demand for specific tasks or services, providing immediate access to work for individuals and flexible staffing for businesses, often leveraging technology for efficient allocation.
% TRANSFER_FUNCTION: Moves surplus value from gig workers (through low wages, lack of benefits, and externalized risks) to platform companies and their investors, enabled by the classification of workers as independent contractors.
% ABSENT_VOICES: Organized labor and worker advocacy groups are systematically excluded from the policy-making and platform governance discussions that define flexible employment terms. They would argue for reclassification, collective bargaining rights, and robust social protections.
% DISAPPEARANCE_RATIONALE: If the legal and economic framework enabling flexible employment as precarity-driven extraction vanished, platform companies would face significantly higher labor costs and regulatory burdens, forcing a fundamental restructuring of their business models. Many gig workers would seek traditional employment or demand better conditions, leading to a reorganization of labor markets and social safety nets.
% FOUNDING_PROBLEM: The need for highly flexible, on-demand labor in a rapidly changing economy, coupled with individuals seeking supplementary income or alternative work arrangements outside traditional employment structures.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some workers attest that the founding problem of matching flexible labor to demand remains live. Labor economists, social policy experts, and worker advocates argue that while the demand for flexibility is real, the current arrangement has evolved into a mechanism for systemic precarity and extraction, with the original problem largely superseded by rent-seeking behavior. Independent research and worker surveys corroborate the shift towards precarity.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the significant transfer of risk and cost from platforms to workers, coupled with low wages and lack of benefits. Suppression (0.78) is maintained through worker classification, algorithmic management that limits autonomy, and legal barriers to collective action. The theater ratio (0.45) reflects the gap between the proclaimed 'flexibility' and 'entrepreneurship' for workers, and the reality of their constrained economic situation. The metrics show a clear trend of increasing extraction and suppression over time as the model matures.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies and investors perceive this as an efficient, innovative market mechanism (closer to a Rope or even Mountain in their view), while gig workers and social safety net providers experience it as a highly extractive Snare. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and investors are clear beneficiaries (d near 0.0) as they capture the extracted surplus value. Gig workers and social safety net providers are the primary targets (d near 1.0), bearing the costs of precarity and externalized risks. Labor unions are excluded, their efforts to shift directionality for workers actively suppressed by the constraint's design.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (efficient matching of flexible labor) has been substantially superseded by its function as a mechanism for cost externalization and surplus value extraction. The 'flexibility' narrative now largely serves as cover for a system that benefits platforms at the expense of worker security and public welfare. The persistence is due to active enforcement and the suppression of alternatives, not genuine coordination benefits for all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    worker_classification_ambiguity,
    'Are gig workers genuinely independent contractors or should they be classified as employees, given the level of platform control and economic dependence?',
    'Legal rulings and legislative action that clarify worker classification criteria, potentially leading to reclassification and associated labor rights.',
    'Reclassification would fundamentally alter the constraint''s extractiveness and suppression, likely transforming it from a Snare towards a more regulated Tangled Rope or even a Rope, by mandating benefits and collective bargaining rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_classification_ambiguity, conceptual, 'The legal and economic status of gig workers is a core ambiguity driving the constraint''s extractive power.').

omega_variable(
    social_cost_quantification,
    'What is the full societal cost of externalized risks (e.g., healthcare, unemployment, retirement insecurity) borne by gig workers and the public, compared to the economic benefits generated by platforms?',
    'Comprehensive econometric studies and public accounting that quantify the social welfare impacts, including the burden on public services and the long-term effects on worker well-being.',
    'Clear quantification of high social costs would strengthen arguments for regulatory intervention, potentially leading to policies that internalize these costs for platforms and reduce the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_quantification, empirical, 'The true economic burden of flexible employment on society is not fully accounted for.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gig worker agency structural (platform control, legal barriers) or internalized (belief in ''entrepreneurship'', lack of awareness of rights)?',
    'Post-exit worker surveys and longitudinal studies: if suppression of agency persists after leaving platform work, it suggests internalized components. Worker organizing success rates also provide evidence.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, requiring different interventions (e.g., education, community building) in addition to legal reforms. If purely structural, legal and regulatory changes alone would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gig workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, labor_law_enforcement_gaps).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_funding_crisis).

% DUAL FORMULATION NOTE:
% This constraint is the 'precarity_extraction_reading' of the 'flexible_employment_legitimacy' kernel. It is linked to other readings of the same kernel, such as 'market_efficiency_reading' and 'developmental_state_reading', which offer alternative interpretations of flexible employment's legitimacy and function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
