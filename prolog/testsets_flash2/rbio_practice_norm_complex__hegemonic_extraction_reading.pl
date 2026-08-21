% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Norms: Hegemonic Extraction Reading
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint describes the 'rules-based international order' (RBIO)
 *   norms as a frozen hegemonic project, where formal revisability is
 *   undermined by P5 veto power and institutional path-dependency. The
 *   selective enforcement of these norms, particularly against Global South
 *   states, reveals an underlying extractive intent. This is one reading of
 *   the 'rbio_practice_norm_complex' kernel, focusing on the structural
 *   mechanisms of extraction and suppression.
 *
 * KEY AGENTS:
 *   - us_european_capital: Primary beneficiary (institutional/arbitrage)
 *   - p5_states: Agenda-setter (institutional/constrained)
 *   - global_south_states: Primary target (powerless/trapped)
 *   - global_south_populations: Primary target (powerless/trapped)
 *   - international_financial_institutions: Agenda-setter (institutional/constrained)
 *   - liberal_institutional_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Norms: Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '72a1b25d-867d-413c-9697-cc621770d168').
narrative_ontology:cs_kernel_codification('72a1b25d-867d-413c-9697-cc621770d168', formalized).
narrative_ontology:cs_authority_grounding('72a1b25d-867d-413c-9697-cc621770d168', extraction).
narrative_ontology:cs_interpretation_layer_present('72a1b25d-867d-413c-9697-cc621770d168').
narrative_ontology:cs_reading_relation('72a1b25d-867d-413c-9697-cc621770d168', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('72a1b25d-867d-413c-9697-cc621770d168', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('72a1b25d-867d-413c-9697-cc621770d168', foundational, intervention_without_consent_is_illegitimate).
narrative_ontology:cs_axiom_status(intervention_without_consent_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('72a1b25d-867d-413c-9697-cc621770d168', intervention_without_consent_is_illegitimate, deontological).
narrative_ontology:cs_axiom('72a1b25d-867d-413c-9697-cc621770d168', foundational, conditionality_is_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('72a1b25d-867d-413c-9697-cc621770d168', conditionality_is_coerced_contract, conventional).
narrative_ontology:cs_reference_frame('72a1b25d-867d-413c-9697-cc621770d168', post_colonial_self_determination).
narrative_ontology:cs_drift_state('72a1b25d-867d-413c-9697-cc621770d168', contemporary_global_south_experience, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('72a1b25d-867d-413c-9697-cc621770d168', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and predictability of the RBIO system, which facilitates global trade, investment, and resource access. The norms, as enforced, protect their interests and allow for capital accumulation in developing economies, often through conditional lending and structural adjustment programs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% The permanent members of the UN Security Council (P5) wield veto power, effectively freezing the RBIO norms against substantive amendment. They selectively enforce norms to protect their strategic and economic interests, often aligning with the interests of their domestic capital. They administer the formal revision process but block changes that would undermine their hegemonic position.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states, agenda_setter,
    institutional, generational, constrained, global).

% Subjected to conditionalities and interventions justified by RBIO norms, which often lead to structural adjustment, resource extraction, and limited policy autonomy. Their formal sovereignty is undermined by the practical inability to resist or amend the norms. Exit from the system carries prohibitive costs, including economic isolation and potential intervention.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    powerless, generational, trapped, global).

% Bear the direct social and economic costs of policies imposed under RBIO conditionalities, including austerity measures, privatization, and environmental degradation. They have no direct voice in the norm-setting or enforcement process and are largely trapped by the decisions of their states and international institutions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, biographical, trapped, local).

% Administer and enforce RBIO norms through lending conditionalities and technical assistance. While formally neutral, their policies often align with the interests of dominant capital and P5 states, acting as a key mechanism for the extractive application of norms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Analyze RBIO norms from a perspective that emphasizes their potential for cooperation and mutual benefit, often downplaying or reinterpreting evidence of hegemonic extraction. Their analytical framework often assumes good faith and universal applicability.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international economic and political interaction, aiming to stabilize global markets, facilitate trade, and manage interstate disputes, ostensibly for collective benefit.
% TRANSFER_FUNCTION: Transfers wealth, resources, and policy autonomy from Global South states and populations to dominant global capital and P5 states, through mechanisms like conditional lending, market access rules, and selective enforcement.
% ABSENT_VOICES: Subaltern groups, indigenous communities, and non-state actors in the Global South whose livelihoods are directly impacted by RBIO norms are largely excluded from formal decision-making processes. Their perspectives would highlight the coercive and extractive nature of the norms.
% DISAPPEARANCE_RATIONALE: If the RBIO norm complex vanished overnight, the existing global economic order would face immediate and profound disruption. Dominant capital would lose its primary legal and institutional framework for global accumulation, and Global South states might reclaim policy space, leading to a significant rearrangement of global power and wealth distribution.
% FOUNDING_PROBLEM: To establish a stable and predictable international order after World War II, preventing future conflicts and fostering economic interdependence through rules-based cooperation.
% FOUNDING_PROBLEM_CORROBORATION: P5 states and international financial institutions assert the founding problem of global instability and economic fragmentation remains live. Global South states and critical scholars argue the problem has evolved into one of hegemonic control and extraction, with the original coordination function largely superseded by rent-seeking; this is corroborated by historical analysis of structural adjustment programs and debt crises from independent academic sources.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the continuous transfer of wealth and policy autonomy from the Global South. Suppression (0.78) is high due to the P5 veto, the lack of viable exit options for Global South states, and the coercive power of international financial institutions. The theater ratio (0.45) indicates that while some coordination functions remain, a significant portion of the 'rules-based' discourse serves to legitimize and obscure the underlying extractive practices. The historical measurements show a clear trend of increasing extractiveness and suppression over time, as the initial coordination function has atrophied and been replaced by hegemonic rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of P5 states and dominant capital, the RBIO norms are a legitimate and necessary framework for global stability and prosperity (a 'rope' or 'scaffold'). From the perspective of Global South states and populations, the same norms operate as a 'snare,' trapping them in a system of unequal exchange and limited sovereignty. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   US and European capital are full beneficiaries (d=0.0) as the norms facilitate their global accumulation. P5 states are beneficiaries (d=0.15) as they control the system and benefit from its stability, though they bear some maintenance costs. Global South states and populations are full targets (d=1.0) as they bear the costs of extraction and suppression with virtually no exit options. International financial institutions act as agenda-setters, enforcing the norms in ways that align with the beneficiaries, placing them closer to the beneficiary end (d=0.25).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has outlived its original function of fostering equitable global cooperation. While formally revisable, the P5 veto and institutional inertia prevent meaningful reform, leading to mandatrophy. The classification as a 'snare' prevents mislabeling this as a coordination mechanism, highlighting the coercive and extractive nature of its persistence despite its stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p5_veto_structural_necessity,
    'Is the P5 veto power a structural necessity for global stability, or an instrument of hegemonic control that freezes RBIO norms?',
    'Counterfactual analysis of alternative UN Security Council structures and their impact on international conflict and cooperation, or empirical study of veto usage patterns and their correlation with P5 national interests versus global collective goods.',
    'If a structural necessity, the constraint''s suppression might be re-evaluated as an unavoidable cost of global governance. If an instrument of hegemonic control, it reinforces the ''snare'' classification by highlighting the deliberate suppression of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_veto_structural_necessity, conceptual, 'Ambiguity regarding the P5 veto''s function: stability vs. hegemony.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression experienced by Global South states and populations structural (external barriers, economic coercion) versus internalized (belief in the legitimacy of RBIO norms, lack of perceived alternatives)?',
    'Post-exit suppression trajectory: if states that successfully exit or resist RBIO conditionalities continue to face internal challenges (e.g., elite capture, lack of capacity) that mimic external suppression, it suggests a degree of internalized suppression. Comparative analysis of states with similar structural positions but different levels of resistance.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would deepen the ''snare'' classification by revealing a more insidious form of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for Global South actors.').

omega_variable(
    rbio_framing_underdetermination,
    'Is the ''hegemonic extraction'' framing the only defensible interpretation of the RBIO norm complex, or do alternative framings (e.g., ''liberal institutionalism,'' ''sovereignty maximalism'') offer equally coherent, albeit different, structural patterns?',
    'Analysis of the explanatory power and empirical fit of each framing across diverse case studies, focusing on which framing best accounts for observed outcomes of power, wealth distribution, and conflict. This involves evaluating the ''liberal_institutional_reading'' and ''sovereignty_maximalist_reading'' as separate constraints.',
    'If alternative framings are equally coherent, it highlights the conceptual contestability of the RBIO kernel itself, suggesting that the ''snare'' classification is reading-dependent. If this framing proves superior, it strengthens the ''snare'' verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rbio_framing_underdetermination, conceptual, 'The RBIO norm complex is a kernel with multiple coherent, competing readings, each yielding a different constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'rbio_practice_norm_complex' kernel. Other readings include 'liberal_institutional_reading' and 'sovereignty_maximalist_reading', which offer different structural interpretations and classifications of the same underlying international order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
