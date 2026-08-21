% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'transactional-provisional'
 *   reading of the JCPOA's bindingness. In this reading, the agreement is
 *   viewed as a temporary, transactional arrangement, voidable upon a
 *   unilateral determination of 'bad faith' by any party, particularly
 *   concerning Iranian compliance. This interpretation prioritizes national
 *   sovereignty and the flexibility to reimpose sanctions, benefiting
 *   domestic political coalitions that oppose the deal and advocates for
 *   unconstrained national action. It imposes significant costs on the
 *   Iranian economy and undermines multilateral diplomacy frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.7).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '69d07356-71e2-49d7-88a9-f6e6f71eba0c').
narrative_ontology:cs_kernel_codification('69d07356-71e2-49d7-88a9-f6e6f71eba0c', formalized).
narrative_ontology:cs_authority_grounding('69d07356-71e2-49d7-88a9-f6e6f71eba0c', extraction).
narrative_ontology:cs_interpretation_layer_present('69d07356-71e2-49d7-88a9-f6e6f71eba0c').
narrative_ontology:cs_reading_relation('69d07356-71e2-49d7-88a9-f6e6f71eba0c', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('69d07356-71e2-49d7-88a9-f6e6f71eba0c', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('69d07356-71e2-49d7-88a9-f6e6f71eba0c', foundational, national_sovereignty_trumps_multilateral_commitment).
narrative_ontology:cs_axiom_status(national_sovereignty_trumps_multilateral_commitment, holdable).
narrative_ontology:cs_axiom_grounding('69d07356-71e2-49d7-88a9-f6e6f71eba0c', national_sovereignty_trumps_multilateral_commitment, deontological).
narrative_ontology:cs_axiom('69d07356-71e2-49d7-88a9-f6e6f71eba0c', foundational, treaty_bindingness_is_contingent_on_reciprocal_good_faith).
narrative_ontology:cs_axiom_status(treaty_bindingness_is_contingent_on_reciprocal_good_faith, holdable).
narrative_ontology:cs_axiom_grounding('69d07356-71e2-49d7-88a9-f6e6f71eba0c', treaty_bindingness_is_contingent_on_reciprocal_good_faith, conventional).
narrative_ontology:cs_reference_frame('69d07356-71e2-49d7-88a9-f6e6f71eba0c', unilateral_sovereign_prerogative).
narrative_ontology:cs_drift_state('69d07356-71e2-49d7-88a9-f6e6f71eba0c', post_jcpoa_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69d07356-71e2-49d7-88a9-f6e6f71eba0c', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, national_sovereignty_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_frameworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the interpretation that allows unilateral withdrawal based on national interest, preserving maximum flexibility for state action without external constraint. This group sees international agreements as subordinate to national sovereignty.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, national_sovereignty_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Gains political capital and policy leverage from an interpretation that allows for easy repudiation of the JCPOA, aligning with their ideological stance against the agreement. They benefit from the ability to reimpose sanctions quickly.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal, beneficiary,
    organized, immediate, mobile, national).

% Bears the direct costs of sanctions reimposition, leading to economic hardship, inflation, and reduced access to international markets. Its situation is directly worsened by the unilateral withdrawal enabled by this reading.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_economy, payer,
    powerless, biographical, trapped, national).

% Suffers erosion of legitimacy and effectiveness when international agreements are treated as unilaterally voidable. This reading undermines the principles of collective security and consensus-based international relations.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_frameworks, payer,
    institutional, generational, constrained, global).

% Monitors Iran's nuclear program and reports on compliance, but its findings are treated as one input among others, not the sole determinant of 'bad faith' or grounds for withdrawal. Its technical assessments are subject to political interpretation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to engage in transactional diplomacy, where commitments are contingent on perceived good faith and can be adjusted or terminated based on national interest, allowing for flexible responses to perceived threats.
% TRANSFER_FUNCTION: Transfers the burden of compliance and the risk of unilateral action onto the party perceived to be in 'bad faith' (Iran), while granting the withdrawing party (e.g., US) the flexibility to reimpose sanctions and pursue national security objectives.
% ABSENT_VOICES: Advocates for robust international law and multilateral treaty enforcement, who would argue against the unilateral voidability of agreements, are marginalized in this framework. Their perspective emphasizes collective security over individual state discretion.
% DISAPPEARANCE_RATIONALE: If this transactional-provisional reading of the JCPOA's bindingness vanished, states would be more constrained in their ability to unilaterally withdraw from international agreements, leading to a more stable but less flexible international legal order. The calculus for entering and exiting treaties would fundamentally shift.
% FOUNDING_PROBLEM: The problem of managing nuclear proliferation risks while preserving national sovereignty and the ability to respond to perceived threats without being permanently bound by agreements that may no longer serve national interests.
% FOUNDING_PROBLEM_CORROBORATION: This reading is primarily corroborated by political actors and legal scholars who prioritize national sovereignty and flexible foreign policy over strict adherence to multilateral frameworks. International legal bodies and multilateral diplomacy advocates often contest this framing, arguing it undermines the stability of international law.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading allows for the unilateral imposition of severe economic costs (sanctions) based on a subjective determination of non-compliance, rather than a consensus-based or internationally adjudicated process. Suppression (0.7) is also high, as the threat of sanctions reimposition is a powerful coercive tool that limits Iran's economic and diplomatic options. The theater ratio (0.4) reflects that while there is genuine concern for non-proliferation, a significant portion of the 'compliance' discourse serves to justify the option of unilateral withdrawal and sanctions, rather than solely ensuring nuclear safeguards. The claimed type is 'snare' because the coordination story (managing proliferation) is largely a cover for the extraction of political leverage and the imposition of costs on a target state, with identifiable victims and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this reading is a pragmatic exercise of sovereign power, ensuring national security. From the victims' perspective, it is an arbitrary and coercive mechanism that undermines stability and imposes undue costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National sovereignty advocates and domestic political coalitions opposing the deal are beneficiaries (low directionality) as this reading grants them maximum flexibility and political wins. The Iranian economy and multilateral diplomacy frameworks are victims (high directionality) as they bear the direct costs of unilateral withdrawal and the erosion of international norms. The IAEA, while an observer, finds its technical findings subject to political interpretation, reducing its effective power in this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_bad_faith_determination,
    'What constitutes ''bad faith'' or ''significant non-compliance'' sufficient to void the agreement, and who has the legitimate authority to make such a determination unilaterally?',
    'Establishment of an independent, internationally recognized arbitration body with binding authority to assess compliance and determine grounds for withdrawal, or a clear, pre-negotiated set of objective triggers for voidability.',
    'If the determination of ''bad faith'' is subject to objective, multilateral review, the constraint''s extractiveness and suppression would decrease, shifting it closer to a Tangled Rope or even a Rope. If it remains unilateral and subjective, its Snare-like qualities are reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_bad_faith_determination, conceptual, 'Ambiguity regarding the criteria and authority for unilateral withdrawal from the JCPOA.').

omega_variable(
    impact_on_future_treaty_negotiations,
    'Does this transactional-provisional reading of the JCPOA''s bindingness create a precedent that significantly undermines the willingness of states to enter into future complex multilateral agreements?',
    'Empirical analysis of subsequent treaty negotiation failures or successes, specifically examining whether the JCPOA''s fate is cited as a reason for reluctance to commit to similar agreements.',
    'If it demonstrably chills future multilateral diplomacy, the long-term costs to global governance frameworks are higher than currently measured, amplifying the ''payer'' role of multilateral diplomacy frameworks. If states continue to engage in complex agreements, the impact is less severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_future_treaty_negotiations, empirical, 'The systemic impact of unilateral withdrawal on the stability and viability of future international agreements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
