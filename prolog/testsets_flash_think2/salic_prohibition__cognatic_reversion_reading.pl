% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Frankish Anachronism (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint is the 'cognatic reversion' reading of the
 *   `salic_prohibition` kernel. It argues that Salic Law was a Frankish
 *   anachronism never properly binding on non-Frankish territories,
 *   advocating for female succession via cognatic primogeniture and
 *   prioritizing territorial integrity. Sibling readings include
 *   `immutable_mandate_reading` (Salic Law as irrevocable divine/natural law)
 *   and `sovereign_override_reading` (Salic Law as revocable positive law).
 *   This reading frames Salic Law as a historically limited and often
 *   misapplied constraint, rather than a universally binding one, thus its
 *   low extractiveness and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.15).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.1).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law as Frankish Anachronism (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'd386ca34-5575-40bc-9b9b-aa7e71eed09a').
narrative_ontology:cs_kernel_codification('d386ca34-5575-40bc-9b9b-aa7e71eed09a', fixed_text).
narrative_ontology:cs_authority_grounding('d386ca34-5575-40bc-9b9b-aa7e71eed09a', lineage).
narrative_ontology:cs_interpretation_layer_present('d386ca34-5575-40bc-9b9b-aa7e71eed09a').
narrative_ontology:cs_reading_relation('d386ca34-5575-40bc-9b9b-aa7e71eed09a', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('d386ca34-5575-40bc-9b9b-aa7e71eed09a', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('d386ca34-5575-40bc-9b9b-aa7e71eed09a', foundational, cognatic_succession_natural_order).
narrative_ontology:cs_axiom_status(cognatic_succession_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('d386ca34-5575-40bc-9b9b-aa7e71eed09a', cognatic_succession_natural_order, deontological).
narrative_ontology:cs_axiom('d386ca34-5575-40bc-9b9b-aa7e71eed09a', foundational, territorial_integrity_supremacy).
narrative_ontology:cs_axiom_status(territorial_integrity_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d386ca34-5575-40bc-9b9b-aa7e71eed09a', territorial_integrity_supremacy, conventional).
narrative_ontology:cs_reference_frame('d386ca34-5575-40bc-9b9b-aa7e71eed09a', historical_cognatic_custom).
narrative_ontology:cs_drift_state('d386ca34-5575-40bc-9b9b-aa7e71eed09a', post_enlightenment_legal_critique, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d386ca34-5575-40bc-9b9b-aa7e71eed09a', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, non_frankish_territories).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, dynastic_stability_advocates).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_purists).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, salic_law_adherents).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_unigeniture_principle).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a legitimate claim to dynastic succession, which was historically denied by strict agnatic Salic Law. Their identity is intrinsically tied to the dynastic line.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs, beneficiary,
    powerless, generational, identity_locked, national).

% Avoid the imposition of a foreign legal tradition (Salic Law) that was not historically binding in their jurisdiction, preserving local customs and legal autonomy. Their 'exit' is maintaining their distinct legal identity.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_territories, beneficiary,
    moderate, generational, mobile, regional).

% Benefit from a more flexible succession rule that can prevent succession crises and maintain the integrity of the realm by allowing the most suitable heir, regardless of sex, to ascend. Their options are limited by the dynastic framework itself.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_stability_advocates, beneficiary,
    organized, generational, constrained, national).

% Lose their exclusive claim to succession based on male-only lineage and see their traditional interpretation of dynastic law challenged. Their identity and status are deeply intertwined with agnatic principles.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_purists, payer,
    powerful, civilizational, identity_locked, national).

% See the legal and historical basis for their power and claims eroded by an interpretation that limits Salic Law's scope and applicability. Their institutional identity is fused with the defense of this ancient legal tradition.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, salic_law_adherents, payer,
    institutional, civilizational, identity_locked, global).

% Analyze the historical application, interpretation, and contestation of Salic Law, providing academic insights into its origins and evolution without direct involvement in dynastic claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_legal_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, diffuse).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for dynastic succession that prioritizes territorial integrity and broader dynastic stability over strict agnatic purity, coordinating the transfer of power in a way that avoids foreign imposition and respects local legal traditions.
% TRANSFER_FUNCTION: Transfers the right of succession to female heirs and local legal traditions, away from strict agnatic lines and the extraterritorial imposition of Frankish legal principles.
% ABSENT_VOICES: Historically, the voices of female claimants and local populations in non-Frankish territories were often suppressed or ignored; they would argue for their inherent rights and local customs, challenging the universal applicability of Salic Law.
% DISAPPEARANCE_RATIONALE: If this reading (that Salic Law was anachronistic and non-binding outside its original Frankish jurisdiction) were universally accepted, it would retroactively invalidate many historical succession disputes and fundamentally alter the legal basis for numerous European monarchies, leading to significant re-evaluation of dynastic claims and historical legitimacy.
% FOUNDING_PROBLEM: The problem of dynastic instability and territorial fragmentation caused by rigid agnatic succession rules being applied anachronistically or extraterritorially, leading to wars of succession and challenges to local legal traditions.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal scholars and political historians, independent of specific dynastic claims, corroborate the historical and legal ambiguities surrounding Salic Law's application and the resulting conflicts, supporting the view that its universal binding nature was often contested.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).
:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect this reading's core argument: that Salic Law, in its universal application, was never truly legitimate or binding outside its original Frankish context. Therefore, from this perspective, it did not 'extract' or 'suppress' in the same way a genuinely enforced law would. The moderate resistance (0.40) acknowledges that this reading itself represents a historical and legal challenge to dominant interpretations, indicating an active intellectual contestation. The low accessibility collapse (0.20) implies that alternatives (cognatic succession) were always conceptually or historically available.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of agnatic purists, this reading is highly extractive and suppressive, as it challenges their foundational principles. However, from the 'cognatic reversion' perspective, the constraint (universal Salic Law) is seen as an illegitimate imposition, and thus its 'extraction' is minimal because its legitimacy is denied. The engine's classification will reflect the low metrics authored from this reading's viewpoint, highlighting the divergence from how other readings might perceive the same historical phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Female heirs and non-Frankish territories are beneficiaries as this reading legitimizes their claims and autonomy. Dynastic stability advocates also benefit by promoting a more flexible and stable succession. Agnatic purists and Salic Law adherents are 'victims' as their traditional claims and power structures are undermined by this interpretation. Historical legal scholars act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently performs a mandatrophy analysis on the 'immutable mandate' reading of Salic Law, arguing that its mandate (to ensure agnatic purity) was never universally valid or has long outlived its functional justification for non-Frankish territories. By asserting its anachronistic nature, it prevents mislabeling a historically contested imposition as a legitimate, functional coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_binding_intent_ambiguity,
    'Was Salic Law genuinely intended by its original framers to be universally binding across all territories and future dynasties, or was its application always context-dependent?',
    'Further historical and philological analysis of early Frankish legal texts and contemporary legal commentaries, focusing on explicit statements of scope and intent.',
    'If universal intent is strongly corroborated, this reading''s claim of anachronism is weakened, potentially increasing the perceived extractiveness of Salic Law from the perspective of those it historically bound. If context-dependence is confirmed, this reading''s position is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_binding_intent_ambiguity, empirical, 'Ambiguity regarding the original intended scope and binding nature of Salic Law.').

omega_variable(
    territorial_integrity_vs_agnatic_purity_priority,
    'To what extent should the principle of territorial integrity (avoiding fragmentation or foreign rule) override strict agnatic purity in dynastic succession, from a normative perspective?',
    'Conceptual analysis of political philosophy and constitutional theory regarding the foundations of state legitimacy and dynastic purpose, or a preference-based decision by a sovereign or constitutional body.',
    'If territorial integrity is normatively prioritized, this reading gains stronger justification. If agnatic purity is deemed paramount, this reading''s normative force is diminished, potentially shifting its classification towards a more ''contested'' or ''snare-like'' status from the perspective of those who uphold agnatic principles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_integrity_vs_agnatic_purity_priority, conceptual, 'Normative priority between territorial integrity and agnatic purity in succession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(sali_tr_t1740, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1740, 0.05).
narrative_ontology:measurement(sali_tr_t1780, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(sali_tr_t1820, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1820, 0.05).
narrative_ontology:measurement(sali_tr_t1860, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(sali_be_t1740, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1740, 0.14).
narrative_ontology:measurement(sali_be_t1780, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1780, 0.13).
narrative_ontology:measurement(sali_be_t1820, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement(sali_be_t1860, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1860, 0.15).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(sali_su_t1740, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1740, 0.09).
narrative_ontology:measurement(sali_su_t1780, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1780, 0.08).
narrative_ontology:measurement(sali_su_t1820, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1820, 0.09).
narrative_ontology:measurement(sali_su_t1860, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1860, 0.1).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel. It represents the 'cognatic reversion' perspective, arguing for limited applicability and female succession, in contrast to the 'immutable mandate' and 'sovereign override' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
