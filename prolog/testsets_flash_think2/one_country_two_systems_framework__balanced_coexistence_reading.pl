% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'balanced coexistence' reading of
 *   the 'One Country, Two Systems' framework. This reading emphasizes
 *   ongoing, substantive political negotiation between the PRC's sovereignty
 *   and Hong Kong's autonomy, where neither is absolute. Contested boundaries
 *   are resolved through accommodation rather than strict legal supremacy,
 *   and civil society retains some bargaining power. The framework functions
 *   as a Tangled Rope, providing coordination for coexistence but with
 *   inherent power asymmetries leading to ongoing, moderate extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.5).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '2f5faca5-525c-471c-a0ae-991c18646e18').
narrative_ontology:cs_kernel_codification('2f5faca5-525c-471c-a0ae-991c18646e18', formalized).
narrative_ontology:cs_authority_grounding('2f5faca5-525c-471c-a0ae-991c18646e18', lineage).
narrative_ontology:cs_interpretation_layer_present('2f5faca5-525c-471c-a0ae-991c18646e18').
narrative_ontology:cs_reading_relation('2f5faca5-525c-471c-a0ae-991c18646e18', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f5faca5-525c-471c-a0ae-991c18646e18', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('2f5faca5-525c-471c-a0ae-991c18646e18', foundational, sovereignty_and_autonomy_are_negotiable).
narrative_ontology:cs_axiom_status(sovereignty_and_autonomy_are_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('2f5faca5-525c-471c-a0ae-991c18646e18', sovereignty_and_autonomy_are_negotiable, conventional).
narrative_ontology:cs_axiom('2f5faca5-525c-471c-a0ae-991c18646e18', foundational, political_accommodation_is_primary_resolution).
narrative_ontology:cs_axiom_status(political_accommodation_is_primary_resolution, holdable).
narrative_ontology:cs_axiom_grounding('2f5faca5-525c-471c-a0ae-991c18646e18', political_accommodation_is_primary_resolution, conventional).
narrative_ontology:cs_reference_frame('2f5faca5-525c-471c-a0ae-991c18646e18', post_handover_negotiated_settlement).
narrative_ontology:cs_drift_state('2f5faca5-525c-471c-a0ae-991c18646e18', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f5faca5-525c-471c-a0ae-991c18646e18', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereignty over Hong Kong, but engages in political accommodation to maintain stability and international standing. Benefits from the framework's recognition of 'One Country' while managing 'Two Systems'.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates with a high degree of autonomy under the framework, benefiting from stability and continued economic prosperity. Navigates the contested boundaries through negotiation and implementation of policies that balance both sides.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, beneficiary,
    institutional, biographical, constrained, regional).

% Bears the costs of political accommodation, experiencing limits on absolute autonomy and civil liberties. However, retains some bargaining power through economic influence, international advocacy, and internal resistance, preventing total suppression.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Monitors the implementation of 'One Country, Two Systems' due to its implications for international law, trade, and human rights. Exerts diplomatic and economic pressure, influencing the dynamics of political accommodation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the complex coexistence of two fundamentally different legal, economic, and political systems (socialist mainland and capitalist Hong Kong) under a single sovereign entity, ensuring stability and mutual benefit.
% TRANSFER_FUNCTION: Transfers ultimate sovereign authority to the PRC while granting Hong Kong a high degree of functional autonomy. This involves a continuous transfer of political influence and legal interpretation, with Hong Kong ceding some absolute autonomy in exchange for stability and continued distinctiveness.
% ABSENT_VOICES: Those advocating for absolute, unconstrained sovereignty for the PRC, or for absolute, internationally guaranteed autonomy for Hong Kong, are structurally marginalized by this reading which prioritizes negotiation and accommodation.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework, as understood by this reading, vanished overnight, the entire political, legal, and economic relationship between Hong Kong and the mainland would collapse. It would force an immediate choice between full integration (ending Hong Kong's distinctiveness) or full independence (a geopolitical crisis), both requiring massive, disruptive reorganization.
% FOUNDING_PROBLEM: The framework was established to facilitate the peaceful and prosperous return of Hong Kong to Chinese sovereignty after the British handover, preserving its capitalist system and way of life while integrating it into the PRC.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for negotiation and accommodation, as evidenced by periodic political crises and diplomatic statements from both the PRC and international bodies, corroborates that the core problem of managing this unique coexistence remains live. Independent constitutional scholars and political analysts also attest to its ongoing, dynamic nature.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate, reflecting the ongoing negotiation and the fact that neither side achieves absolute claims, but the PRC's ultimate sovereign power allows for some extraction of autonomy. Suppression (0.50) is also moderate; while political accommodation requires limiting certain actions, civil society's continued bargaining power and international scrutiny prevent total suppression. Theater ratio (0.20) is low, indicating that the negotiation and functional division of powers are substantive, not merely performative. Accessibility collapse (0.45) is moderate, as alternatives (full independence or full integration) are constrained but not entirely foreclosed in public discourse. Resistance (0.55) is present, reflecting civil society's ongoing efforts to assert autonomy within the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC's perspective, this framework ensures national unity and stability, while from Hong Kong's perspective, it preserves a distinct way of life and economic system. The 'balanced coexistence' reading attempts to bridge these, but the underlying power asymmetry means the experience of the constraint differs significantly between the agenda-setter and the payer seats. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government and the Hong Kong Government are beneficiaries, as both gain from the stability and functional operation of the framework, albeit with different priorities. Hong Kong Civil Society is a payer, bearing the costs of compromises and limitations on autonomy, but their organized power and international leverage mean they are not fully targeted. The international community acts as an observer, influencing the dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a ''balanced coexistence'' reading, or is it a transitional phase towards either ''sovereignty primacy'' or ''autonomy primacy''?',
    'Long-term observation of political accommodation outcomes, judicial interpretations, and the relative bargaining power of civil society over several decades. If the balance consistently shifts towards one pole without effective counter-negotiation, the reading''s structural validity would be challenged.',
    'If it''s a transitional phase, the classification would shift towards the dominant sibling reading (e.g., higher extraction/suppression for sovereignty primacy, lower for autonomy primacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Ambiguity of the ''balanced coexistence'' reading''s long-term stability.').

omega_variable(
    political_accommodation_vs_legal_supremacy,
    'To what extent does ''political accommodation'' genuinely resolve contested boundaries, versus merely deferring to an unstated legal supremacy?',
    'Analysis of specific boundary disputes: do outcomes reflect genuine compromise, or are they consistently aligned with the interests of the more powerful party, regardless of legal arguments? Examination of judicial independence in resolving such disputes.',
    'If accommodation consistently defers to unstated legal supremacy, the constraint''s extractiveness and suppression would be higher, and its claimed ''rope'' elements would be more theatrical, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_accommodation_vs_legal_supremacy, conceptual, 'The true nature of ''political accommodation'' in resolving contested boundaries.').

omega_variable(
    civil_society_bargaining_power,
    'How much effective bargaining power does Hong Kong civil society truly retain, and how resilient is it to increasing pressure?',
    'Empirical study of civil society''s ability to influence policy outcomes, mobilize public opinion, and resist legislative or executive actions over time, particularly during periods of increased pressure from the PRC.',
    'If civil society''s bargaining power is found to be negligible or rapidly eroding, the constraint''s suppression would be higher, and its classification would lean more heavily towards a Snare, as a key counter-balancing force would be absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_bargaining_power, empirical, 'The actual extent and resilience of civil society''s influence within the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.16).
narrative_ontology:measurement(one__tr_t2009, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2009, 0.17).
narrative_ontology:measurement(one__tr_t2015, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.45).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(one__be_t2009, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(one__be_t2015, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.4).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.43).
narrative_ontology:measurement(one__su_t2009, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2009, 0.46).
narrative_ontology:measurement(one__su_t2015, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.49).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2024, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
