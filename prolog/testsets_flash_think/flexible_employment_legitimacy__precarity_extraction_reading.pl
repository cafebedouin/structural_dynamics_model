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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Platform Precarity and Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'precarity_extraction_reading' of
 *   the 'flexible_employment_legitimacy' kernel. It describes how the concept
 *   and practice of flexible employment, particularly within the platform
 *   economy, functions as a mechanism for structural precarity and the
 *   extraction of surplus value from workers. The narrative of 'flexibility'
 *   serves as a cover for a system that externalizes risk and costs onto
 *   individual workers while concentrating benefits with platform operators
 *   and, indirectly, consumers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.85).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.9).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Platform Precarity and Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, 'bc90a690-090c-476b-9f68-aa6506bbfac6').
narrative_ontology:cs_kernel_codification('bc90a690-090c-476b-9f68-aa6506bbfac6', implicit).
narrative_ontology:cs_authority_grounding('bc90a690-090c-476b-9f68-aa6506bbfac6', extraction).
narrative_ontology:cs_interpretation_layer_present('bc90a690-090c-476b-9f68-aa6506bbfac6').
narrative_ontology:cs_reading_relation('bc90a690-090c-476b-9f68-aa6506bbfac6', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc90a690-090c-476b-9f68-aa6506bbfac6', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('bc90a690-090c-476b-9f68-aa6506bbfac6', foundational, labor_is_a_disposable_input).
narrative_ontology:cs_axiom_status(labor_is_a_disposable_input, holdable).
narrative_ontology:cs_axiom_grounding('bc90a690-090c-476b-9f68-aa6506bbfac6', labor_is_a_disposable_input, empirically_contingent).
narrative_ontology:cs_axiom('bc90a690-090c-476b-9f68-aa6506bbfac6', secondary, risk_externalization_is_efficient).
narrative_ontology:cs_axiom_status(risk_externalization_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('bc90a690-090c-476b-9f68-aa6506bbfac6', risk_externalization_is_efficient, instrumental).
narrative_ontology:cs_reference_frame('bc90a690-090c-476b-9f68-aa6506bbfac6', traditional_employment_contract).
narrative_ontology:cs_drift_state('bc90a690-090c-476b-9f68-aa6506bbfac6', contemporary_platform_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bc90a690-090c-476b-9f68-aa6506bbfac6', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the terms of 'flexible' employment, including algorithmic management, payment structures, and dispute resolution. They benefit directly from lower labor costs, externalized risks, and surplus value extraction. They frame flexibility as innovation and opportunity.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the full costs of precarity: unstable income, lack of benefits (health insurance, retirement, paid leave), no collective bargaining power, and exposure to algorithmic control. Many are economically dependent, making exit difficult despite poor conditions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, identity_locked, local).

% Benefit from convenient, on-demand services at lower prices, which are often subsidized by the externalized costs borne by gig workers. They indirectly pay for the social costs of precarity through public services, but directly benefit from the efficiency.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, consumers, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, consumers, payer).

% Experience downward pressure on wages, benefits, and job security as 'flexible' models are introduced or used as a benchmark by employers. Their collective bargaining power is eroded by the fragmentation of labor and the threat of replacement by gig models.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employees, payer,
    organized, biographical, constrained, national).

% Are largely excluded from organizing gig workers due to their classification as independent contractors, which undermines their ability to advocate for better conditions and wages. They actively resist the expansion of precarious work but face legal and structural barriers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Analyze the societal costs of flexible employment, such as increased reliance on public assistance, health crises, and widening inequality. They advocate for policy changes to reclassify workers, ensure benefits, and regulate platform power.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_policy_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches a highly elastic supply of labor to fluctuating consumer demand for services, enabling rapid scaling and efficient resource allocation for platforms.
% TRANSFER_FUNCTION: Transfers surplus value from gig workers (through low wages, lack of benefits, and externalized risks) to platform operators (as profit) and consumers (as lower service costs).
% ABSENT_VOICES: Traditional labor unions and social security agencies are structurally excluded from the direct negotiation of terms for gig workers, whose voices are fragmented and suppressed by algorithmic management. They would advocate for worker classification and social protections.
% DISAPPEARANCE_RATIONALE: If the current model of 'flexible employment' (as precarity) vanished overnight, platform business models would collapse or be forced to fundamentally restructure, labor markets would re-equilibrate with potentially higher wages and benefits, and social safety nets would face less strain, leading to a significant reorganization of the economy.
% FOUNDING_PROBLEM: The need for highly flexible, on-demand labor to meet unpredictable consumer demand, and to provide supplementary income opportunities for individuals.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and some workers attest the problem is still live, citing the need for flexible work and income. Labor economists, unions, and social policy advocates attest that while the demand for flexibility is real, the current arrangement primarily serves platform extraction, and the founding problem is largely a pretext for cost-shifting; independent research and worker testimonies corroborate this shifted-function reading.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The high extractiveness (0.85) reflects the significant portion of labor value transferred from workers to platforms and consumers, coupled with the externalization of social costs. Suppression (0.90) is severe due to algorithmic control, the legal classification of workers as independent contractors (limiting collective action), and the economic necessity that traps many workers in precarious roles. The rising theater ratio (0.60) indicates that the narrative of 'flexibility' and 'entrepreneurship' increasingly serves to mask the underlying extractive structure, with less genuine coordination function over time. Accessibility collapse is high (0.75) because for many gig workers, alternative stable employment is scarce, and the platform model itself limits their options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of platform operators, flexible employment is a legitimate, efficient market mechanism. From the perspective of gig workers and labor advocates, it is a snare designed for extraction. The engine's classification will highlight this divergence, showing a claimed 'rope' (from the market efficiency reading) operating as a 'snare' (from this precarity reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are clear beneficiaries (d=0.0) as they design and profit from the system. Gig workers are full targets (d=1.0), bearing the costs of precarity and extraction. Consumers are partial beneficiaries (d=0.2) due to lower costs and convenience, but also indirectly bear social costs. Traditional employees are targets (d=0.8) due to downward pressure on their labor conditions. Labor unions are excluded (d=0.9) as their ability to intervene is structurally suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing flexible work and income) is increasingly outlived by its function (enabling extraction). The 'flexibility' narrative is maintained theatrically to legitimize the arrangement, even as the founding problem of fragmented labor is largely solved by the platforms themselves, which then leverage their market power. This analysis prevents mislabeling the arrangement as genuine coordination when its primary function has shifted to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexibility_vs_precarity_ambiguity,
    'Is the ''flexibility'' offered by platform employment a genuine benefit for workers, or primarily a mechanism for platforms to externalize risk and costs, leading to precarity?',
    'Longitudinal studies tracking worker well-being, income stability, and access to benefits, comparing self-reported benefits of flexibility against objective measures of economic security.',
    'If flexibility is primarily a cover for precarity, the constraint''s extractiveness and suppression are accurately high; if genuine worker benefit is substantial, these metrics might be slightly lower, and the ''theater_ratio'' would decrease.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flexibility_vs_precarity_ambiguity, empirical, 'Ambiguity between genuine worker flexibility and platform-driven precarity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic dependency, legal classification) or internalized (belief in entrepreneurial freedom, individual responsibility)?',
    'Post-exit suppression trajectory: if workers continue to resist collective action or seek precarious work even after structural barriers are removed (e.g., reclassification as employees), it suggests a partially internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — workers carry the suppression with them after exit, making collective resistance harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in gig work.').

omega_variable(
    platform_power_source_ambiguity,
    'Is platform power derived primarily from network effects and technological innovation (legitimate market advantage) or from regulatory arbitrage and labor misclassification (structural manipulation)?',
    'Legal rulings on worker classification, economic analysis of platform profitability independent of labor cost advantages, and comparative studies with regulated labor markets.',
    'If power is primarily from structural manipulation, the ''snare'' classification is strongly reinforced; if from legitimate innovation, the ''extractiveness'' might be seen as a higher but justifiable cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_power_source_ambiguity, conceptual, 'Source of platform market power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_regulation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
