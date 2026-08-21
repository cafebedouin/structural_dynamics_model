% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norms (Liberal Institutional Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the liberal institutional reading of the
 *   Rule-Based International Order (RBIO) norms. It posits that RBIO norms
 *   are universal, founded on state consent, and can be legitimately revised
 *   through multilateral processes. Enforcement selectivity is viewed as a
 *   practical capacity issue rather than a fundamental flaw in legitimacy.
 *   Interventions are justified under UN Security Council authorization or in
 *   cases of grave atrocities, and economic conditionality is seen as
 *   acceptable contractual terms. This reading emphasizes the coordination
 *   function of RBIO in maintaining global order and addressing shared
 *   challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.45).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.3).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norms (Liberal Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, 'de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a').
narrative_ontology:cs_kernel_codification('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', formalized).
narrative_ontology:cs_authority_grounding('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', lineage).
narrative_ontology:cs_interpretation_layer_present('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a').
narrative_ontology:cs_reading_relation('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', foundational, universal_consent_based_legitimacy).
narrative_ontology:cs_axiom_status(universal_consent_based_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', universal_consent_based_legitimacy, deontological).
narrative_ontology:cs_axiom('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', foundational, multilateral_process_for_revisability).
narrative_ontology:cs_axiom_status(multilateral_process_for_revisability, holdable).
narrative_ontology:cs_axiom_grounding('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', multilateral_process_for_revisability, conventional).
narrative_ontology:cs_reference_frame('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', post_un_charter_ideal).
narrative_ontology:cs_drift_state('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', contemporary_multipolar_challenges, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('de5a1fa5-f5eb-433c-bad8-8d2b1df30e1a', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, contractors_and_aid_agencies).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_during_sanctions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that participate in UN-authorized interventions or provide aid under conditionality, benefiting from perceived legitimacy and influence. They see RBIO norms as a framework for collective security and human rights protection.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    institutional, generational, mobile, global).

% Organizations that receive contracts or funding for post-intervention reconstruction, humanitarian aid, or development projects, operating under the umbrella of RBIO norms. They benefit from the institutional framework that legitimizes their operations.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, contractors_and_aid_agencies, beneficiary,
    organized, biographical, constrained, global).

% States that are subject to interventions or economic conditionality, often experiencing loss of sovereignty or economic hardship. They bear the direct costs of RBIO enforcement, viewing it as an imposition rather than a consensual process.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, generational, trapped, national).

% Populations within targeted states who suffer the consequences of sanctions, including shortages of essential goods, economic decline, and humanitarian crises. They bear the human cost of RBIO enforcement.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_during_sanctions, payer,
    powerless, immediate, trapped, local).

% The primary body for authorizing interventions and legitimizing RBIO enforcement actions. Its resolutions provide the legal and political basis for collective action, though its processes are subject to veto power.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the evolution and application of RBIO norms, assessing their consistency with international law and principles of state sovereignty. They provide critical commentary and contribute to the conceptual development of the framework.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral framework for states to collectively address threats to international peace and security, human rights violations, and economic stability, ensuring a degree of legitimacy and shared responsibility.
% TRANSFER_FUNCTION: Transfers legitimacy and resources to intervening states and their contractors, while imposing costs (sovereignty infringement, economic hardship) on targeted states and their populations.
% ABSENT_VOICES: Populations directly affected by interventions and sanctions often lack direct representation in the multilateral processes that shape RBIO norms. Their perspectives on consent, revisability, and the impact of enforcement are often mediated or unheard.
% DISAPPEARANCE_RATIONALE: If RBIO norms vanished, the international system would lose a key framework for collective action. Interventions would lack legitimacy, economic conditionality would be seen as pure coercion, and the global governance landscape would become significantly more anarchic, leading to a rearrangement of power dynamics and state behavior.
% FOUNDING_PROBLEM: The need for a framework to manage international relations, prevent conflict, protect human rights, and foster economic cooperation in a post-WWII world, moving beyond pure power politics.
% FOUNDING_PROBLEM_CORROBORATION: International organizations, many states (especially those benefiting from the current order), and a significant body of international legal scholarship attest that the founding problems of global governance and collective security remain live. While specific applications are contested, the underlying need for a rule-based order is widely acknowledged outside of purely revisionist states.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs imposed on targeted states and populations through interventions and sanctions, which are seen as legitimate tools for upholding norms. Suppression (0.30) is present due to the coercive aspects of enforcement, but it's framed as necessary for maintaining order rather than pure extraction. The theater ratio (0.20) indicates that while there's some performative aspect to multilateral processes, a substantial functional core remains. The claimed type is 'rope' because this reading emphasizes the genuine coordination function and the consent-based nature of the norms, despite the acknowledged costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening states and international organizations, RBIO norms are a legitimate and necessary framework for global governance. From the perspective of targeted states and their populations, the same norms can appear highly extractive and suppressive, particularly when enforcement is perceived as selective or disproportionate. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and their contractors are beneficiaries, as the norms provide legitimacy and opportunities for influence/contracts. Targeted states and civilian populations are victims, bearing the direct costs of enforcement. The UN Security Council acts as an agenda-setter, legitimizing actions within the framework. International legal scholars serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading implicitly rejects mandatrophy by asserting that the founding problems of international order are still live and that enforcement selectivity is a capacity issue, not a sign of a decayed mandate. The classification as 'rope' reflects this belief in the ongoing, legitimate coordination function, distinguishing it from a 'piton' where the function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_legitimacy,
    'Is enforcement selectivity primarily a capacity problem (as this reading claims) or a fundamental legitimacy problem reflecting underlying power asymmetries?',
    'Empirical analysis of enforcement patterns over time, correlating selectivity with the geopolitical interests of powerful states versus objective criteria for intervention. If selectivity consistently aligns with power interests, it suggests a legitimacy problem.',
    'If primarily a legitimacy problem, the constraint''s effective extractiveness and suppression would be higher, and its classification would shift towards ''tangled_rope'' or ''snare'' for targeted states, as the coordination story would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_legitimacy, empirical, 'Ambiguity regarding the true nature of enforcement selectivity in RBIO.').

omega_variable(
    consent_vs_coercion_boundary,
    'To what extent is state ''consent'' to RBIO norms genuinely voluntary, versus being a product of structural coercion or the absence of viable alternatives for less powerful states?',
    'Analysis of exit options and power dynamics for states joining or adhering to RBIO. If exit is prohibitively costly or identity-locked for many states, the ''consent'' basis is weakened.',
    'If consent is largely coerced, the ''rope'' classification would be challenged, pushing towards ''tangled_rope'' or ''snare'' due to higher effective suppression and extractiveness for many participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_boundary, conceptual, 'The conceptual boundary between voluntary consent and structural coercion in international law.').

omega_variable(
    rbio_reading_framing_choice,
    'Is this ''liberal institutional'' framing the most accurate representation of RBIO norms, or do alternative framings (hegemonic extraction, sovereignty maximalism) better capture its structural reality?',
    'Comparative analysis of the predictive power and explanatory scope of each reading against empirical outcomes (e.g., patterns of intervention, economic inequality, state behavior).',
    'If a sibling reading (e.g., hegemonic_extraction_reading) proves more robust, the constraint''s classification would shift significantly, likely to a more extractive type, and the identified beneficiaries/victims would change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rbio_reading_framing_choice, conceptual, 'The choice of analytical framing for the RBIO kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'RBIO practice norm complex' kernel. Each reading represents a distinct structural claim about the nature and function of RBIO norms, with different ε values and stakeholder positions. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
