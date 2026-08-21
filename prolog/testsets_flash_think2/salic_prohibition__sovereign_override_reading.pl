% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Revocable by Sovereign Act
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint instantiates the 'sovereign override' reading of the
 *   Salic Law kernel, where the law is understood as revocable positive law
 *   subject to the monarch's legislative authority. This reading posits that
 *   a sovereign act (e.g., a Pragmatic Sanction) can legitimately alter the
 *   rules of succession, even if they contradict traditional Salic
 *   prohibitions. Challengers to such an overridden succession are considered
 *   rebels against legitimate authority, and their claims are denied by the
 *   sovereign's will. This contrasts with the 'immutable mandate' reading
 *   (Salic Law as divine/natural law) and the 'cognatic reversion' reading
 *   (Salic Law as never truly binding).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.65).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.75).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Revocable by Sovereign Act").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '5f83078e-aba7-49c1-b38d-54dd3fae70b3').
narrative_ontology:cs_kernel_codification('5f83078e-aba7-49c1-b38d-54dd3fae70b3', formalized).
narrative_ontology:cs_authority_grounding('5f83078e-aba7-49c1-b38d-54dd3fae70b3', lineage).
narrative_ontology:cs_interpretation_layer_present('5f83078e-aba7-49c1-b38d-54dd3fae70b3').
narrative_ontology:cs_reading_relation('5f83078e-aba7-49c1-b38d-54dd3fae70b3', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('5f83078e-aba7-49c1-b38d-54dd3fae70b3', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('5f83078e-aba7-49c1-b38d-54dd3fae70b3', foundational, sovereign_legislative_supremacy).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('5f83078e-aba7-49c1-b38d-54dd3fae70b3', sovereign_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('5f83078e-aba7-49c1-b38d-54dd3fae70b3', foundational, dynastic_continuity_paramount).
narrative_ontology:cs_axiom_status(dynastic_continuity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('5f83078e-aba7-49c1-b38d-54dd3fae70b3', dynastic_continuity_paramount, instrumental).
narrative_ontology:cs_reference_frame('5f83078e-aba7-49c1-b38d-54dd3fae70b3', sovereign_legislative_supremacy_framework).
narrative_ontology:cs_drift_state('5f83078e-aba7-49c1-b38d-54dd3fae70b3', post_succession_wars_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5f83078e-aba7-49c1-b38d-54dd3fae70b3', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch_dynasty).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, state_realm).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, other_dynastic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rebellious_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The current ruling house, whose legitimacy and continuity are secured by the sovereign act overriding traditional Salic Law. They benefit from stable succession and the ability to choose an heir, even a female one, when politically expedient.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch_dynasty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, reigning_monarch_dynasty, beneficiary).

% The political entity (kingdom, empire) that benefits from avoiding succession crises, civil wars, and foreign interventions by having a clear, albeit flexible, legal mechanism for dynastic continuity. Stability is paramount.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, state_realm, beneficiary,
    institutional, civilizational, constrained, national).

% Male-line relatives or other female-line claimants whose traditional claims to succession are bypassed or denied by the sovereign's override. They bear the cost of exclusion and may resort to political maneuvering or armed conflict.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, other_dynastic_claimants, payer,
    powerful, biographical, constrained, regional).

% Groups (nobles, military, populace) who actively resist the sovereign override, viewing it as illegitimate or a violation of fundamental law. They bear the cost of suppression, potential defeat, and loss of status or life.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rebellious_factions, payer,
    organized, immediate, trapped, local).

% The council, parliament, or legal advisors who formally enact or advise on the sovereign act (e.g., Pragmatic Sanction), lending it legal weight and institutional support. They shape the legal framework for succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, legislative_body_advisors, agenda_setter,
    institutional, biographical, mobile, national).

% Academics and jurists who analyze the historical and legal precedents of Salic Law and sovereign overrides. Their interpretations influence public and elite opinion on the legitimacy and historical context of dynastic succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, legal_scholars_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_monarch_dynasty).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes dynastic succession by establishing a clear, albeit flexible, legal mechanism for determining the heir, even when traditional Salic Law would prevent it. This prevents power vacuums and civil war by providing a recognized path for continuity.
% TRANSFER_FUNCTION: Transfers the right to rule (and associated power, wealth, and legitimacy) to the sovereign's chosen heir, potentially bypassing other claimants based on strict Salic primogeniture. It transfers legitimacy from the sovereign act to the new succession order.
% ABSENT_VOICES: Those who believe in an immutable, divinely ordained Salic Law (the 'immutable mandate' reading) or those who believe Salic Law was never truly binding on their territory (the 'cognatic reversion' reading) are structurally excluded from the sovereign's legislative process that enables the override. They would argue against the legitimacy of such an act.
% DISAPPEARANCE_RATIONALE: If this legal framework (Salic Law as revocable by sovereign act) vanished overnight, dynastic succession would revert to a state of extreme contestation. The legitimacy of any non-Salic heir would be entirely undermined, likely leading to civil wars, foreign intervention, and profound political instability as various claimants vie for the throne without a clear legal basis.
% FOUNDING_PROBLEM: Preventing dynastic crises and wars of succession when the direct male line failed, or when a strong female heir was available and politically advantageous, but traditional Salic Law stood in the way. It aimed to provide a legal solution to ensure continuity and stability.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of succession crises (e.g., the War of the Austrian Succession following the Pragmatic Sanction), diplomatic treaties, and constitutional documents from various European monarchies corroborate the problem and the solution's intent. Legal scholars and historians outside the benefiting dynasties attest to the ongoing relevance of succession laws for state stability.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (stabilizing dynastic succession and preventing civil war) but simultaneously involves significant asymmetric extraction from other dynastic claimants whose rights are bypassed. Its persistence relies on active enforcement by the state, often through military means, against those who resist the sovereign's decree. Extractiveness is high because the sovereign's act can be driven by political expediency, bypassing traditional claims. Suppression is high due to the state's willingness to use force to uphold the new order. Theater ratio is low, as the sovereign act is a genuine exercise of power, though some rhetoric may be used to legitimize it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reigning monarch and the state, this constraint is a necessary and legitimate mechanism for dynastic continuity and stability. From the perspective of bypassed claimants and rebellious factions, it is an act of arbitrary power and extraction, violating fundamental, perhaps even divine, law. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch/dynasty and the state/realm are clear beneficiaries, gaining stability and the ability to secure succession. Other dynastic claimants and rebellious factions are targets, bearing the cost of exclusion and suppression. Legislative bodies and advisors act as agenda-setters, facilitating the sovereign's will. Legal scholars and historians serve as observers, analyzing and influencing the discourse around legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_authority_scope,
    'Is the sovereign''s legislative authority truly absolute in matters of fundamental law, or are there unwritten constitutional limits that even a sovereign cannot override without losing legitimacy?',
    'Analysis of historical constitutional crises and the long-term stability of dynasties that enacted such overrides, particularly in cases where the override was widely contested by internal or external powers.',
    'If unwritten limits exist and are violated, the constraint''s long-term legitimacy and stability are lower than perceived, increasing the effective suppression required to maintain it. If authority is truly absolute, the constraint is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_authority_scope, conceptual, 'The extent of sovereign power in altering fundamental law.').

omega_variable(
    legitimacy_cost_of_override,
    'Does the act of overriding a long-standing tradition, even legally, incur a long-term cost to the dynasty''s perceived legitimacy, making it more vulnerable to future challenges?',
    'Comparative historical analysis of monarchies that enacted such overrides versus those that adhered strictly to tradition, examining rates of rebellion, civil war, and dynastic collapse over centuries.',
    'If a significant legitimacy cost is incurred, the constraint''s effective extractiveness and suppression are higher than measured, as the ''cost'' of the override is borne by the dynasty''s future stability. If no such cost, the override is a more efficient coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_cost_of_override, empirical, 'The long-term legitimacy impact of overriding traditional law.').

omega_variable(
    true_coordination_vs_power_play,
    'To what extent is the sovereign override genuinely aimed at dynastic stability (coordination) versus consolidating power or pursuing political advantage for the reigning house (extraction)?',
    'Detailed historical analysis of the political context, motivations of key actors, and outcomes for both the dynasty and the realm, distinguishing between genuine threats to stability and opportunistic power grabs.',
    'If primarily a power play, the constraint''s extractiveness is higher, and its coordination function is more of a cover story. If genuinely for stability, the extractiveness is a necessary cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_coordination_vs_power_play, empirical, 'Distinguishing genuine coordination from power consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__sovereign_override_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__sovereign_override_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__sovereign_override_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(sali_tr_t50, salic_prohibition__sovereign_override_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__sovereign_override_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__sovereign_override_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__sovereign_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(sali_be_t50, salic_prohibition__sovereign_override_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__sovereign_override_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__sovereign_override_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__sovereign_override_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(sali_su_t50, salic_prohibition__sovereign_override_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
