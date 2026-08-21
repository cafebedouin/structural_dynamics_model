% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause: Maximal Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'maximal withdrawal' reading of UN
 *   Security Council Resolution 242, which mandates withdrawal from *all*
 *   occupied territories based on the French definite article ('des
 *   territoires') and the UN Charter's Article 2(4) prohibition on
 *   territorial acquisition by force. This reading views the resolution as a
 *   binding 'Rope' for the occupying state, ensuring full retrocession and
 *   upholding international legal order. The high extractiveness reflects the
 *   significant cost imposed on the occupying state by this comprehensive
 *   interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.7).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Withdrawal Clause: Maximal Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '5ad6d50e-d818-419e-bbf8-21e1a8634570').
narrative_ontology:cs_kernel_codification('5ad6d50e-d818-419e-bbf8-21e1a8634570', fixed_text).
narrative_ontology:cs_authority_grounding('5ad6d50e-d818-419e-bbf8-21e1a8634570', lineage).
narrative_ontology:cs_interpretation_layer_present('5ad6d50e-d818-419e-bbf8-21e1a8634570').
narrative_ontology:cs_reading_relation('5ad6d50e-d818-419e-bbf8-21e1a8634570', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ad6d50e-d818-419e-bbf8-21e1a8634570', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('5ad6d50e-d818-419e-bbf8-21e1a8634570', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5ad6d50e-d818-419e-bbf8-21e1a8634570', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('5ad6d50e-d818-419e-bbf8-21e1a8634570', foundational, french_text_controls_interpretation).
narrative_ontology:cs_axiom_status(french_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('5ad6d50e-d818-419e-bbf8-21e1a8634570', french_text_controls_interpretation, conventional).
narrative_ontology:cs_reference_frame('5ad6d50e-d818-419e-bbf8-21e1a8634570', un_charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('5ad6d50e-d818-419e-bbf8-21e1a8634570', contemporary_diplomatic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ad6d50e-d818-419e-bbf8-21e1a8634570', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parties whose territories were occupied and who seek full retrocession based on the principle of territorial integrity. Their legal position is strong under this reading, but their practical ability to enforce it is limited without international backing.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    powerless, generational, trapped, regional).

% The abstract system of international law and norms, which benefits from the upholding of foundational principles like territorial integrity and the prohibition of conquest. Its legitimacy is reinforced by this reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).

% The state that occupied territories in conflict. Under this reading, it is legally bound to withdraw from all territories, incurring significant strategic and political costs. Its exit options are limited to compliance or continued international condemnation/sanctions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    powerful, biographical, constrained, national).

% The permanent and non-permanent members of the UN Security Council, responsible for drafting and enforcing resolutions. They interpret and apply the resolution, with their actions shaping its effective scope. Their power is to mandate enforcement or allow non-compliance.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_members, agenda_setter,
    institutional, generational, constrained, global).

% The International Court of Justice, which provides advisory opinions and adjudicates disputes based on international law. Its interpretations can lend significant weight to particular readings of resolutions, though its jurisdiction is often contested by states.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, mandatory framework for resolving territorial disputes arising from armed conflict, preventing the permanent acquisition of territory by force and coordinating international efforts towards peaceful resolution.
% TRANSFER_FUNCTION: Mandates the transfer of sovereign control over all occupied territories from the occupying state back to the dispossessed claimants, upholding the principle of territorial integrity.
% ABSENT_VOICES: Historical drafters of the resolution, particularly those who emphasized the English text's ambiguity, are absent from contemporary legal interpretation debates, their original intent often overridden by subsequent legal developments and the French text's clarity.
% DISAPPEARANCE_RATIONALE: If this maximal withdrawal reading vanished, the international legal landscape regarding territorial integrity would be fundamentally altered. Occupying states would face fewer legal constraints, potentially leading to more permanent annexations and destabilizing international relations. Dispossessed claimants would lose a key legal basis for their claims, forcing a reorganization of diplomatic and legal strategies.
% FOUNDING_PROBLEM: The problem of states acquiring territory by force in violation of the UN Charter, leading to prolonged conflicts and undermining international peace and security.
% FOUNDING_PROBLEM_CORROBORATION: The UN Charter itself, numerous subsequent UN resolutions, and the consistent jurisprudence of international legal bodies (e.g., ICJ advisory opinions) corroborate that the problem of territorial acquisition by force remains live and that the principle of territorial integrity is foundational. Legal scholars and human rights organizations outside the direct beneficiaries also attest to its ongoing relevance.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading demands full withdrawal, imposing substantial strategic and political costs on the occupying state. Suppression (0.7) is also high, as the international community, through various mechanisms, actively seeks to enforce this interpretation and delegitimize any claims to retained territory. Theater ratio (0.2) is low, indicating that while diplomatic maneuvering exists, the core legal obligation is taken seriously by proponents of this reading. Resistance (0.8) is high due to the occupying state's continued presence and contestation of this interpretation. Accessibility collapse (0.4) is moderate, as while the legal path to retention is largely closed, political and diplomatic avenues for partial retention are still pursued by the occupying state.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dispossessed claimants, this reading is a just and necessary 'Rope' that upholds fundamental rights and international law. From the occupying state's perspective, it is a highly extractive 'Snare' that ignores its security concerns and the complexities of the conflict. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The dispossessed claimants and the international legal order are the primary beneficiaries, as this reading vindicates their claims and principles. The occupying state is the clear target, facing mandatory and comprehensive withdrawal. UNSC members act as agenda-setters, wielding the power to enforce or permit non-compliance. The ICJ serves as an analytical observer, providing legal interpretations that can strengthen this reading's authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Is the French definite article (''des territoires'') definitively controlling over the English indefinite article (''territories occupied'') in UNSC Resolution 242, or does the ambiguity persist?',
    'A definitive ruling by the International Court of Justice with universal acceptance, or a new, unambiguous Security Council resolution clarifying the scope of withdrawal.',
    'If the French text is definitively controlling, this maximal withdrawal reading is strengthened, increasing the extractiveness on the occupying state. If ambiguity persists, the ''partial_withdrawal_reading'' gains legitimacy, reducing extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'Ambiguity in the resolution''s text regarding the scope of withdrawal.').

omega_variable(
    customary_international_law_drift,
    'Has customary international law evolved to permit retention of some occupied territories for security purposes, overriding the strict territorial integrity default?',
    'Analysis of state practice and opinio juris over decades, particularly in cases of prolonged occupation, to determine if a new customary norm has emerged.',
    'If a new customary norm is recognized, the ''maximal_withdrawal_reading'' would be weakened, potentially reclassifying it towards a ''Piton'' or ''Tangled Rope'' as its enforcement becomes more theatrical or contested. If not, its ''Rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_international_law_drift, empirical, 'Potential for customary international law to drift from strict territorial integrity.').

omega_variable(
    interpretive_authority_legitimacy,
    'Who holds the legitimate authority to definitively interpret UNSC Resolution 242: the ICJ, the drafting states, or the Security Council itself?',
    'A consensus among states on the binding nature of ICJ advisory opinions on this matter, or a clear delegation of interpretive authority by the Security Council.',
    'If the ICJ''s interpretive authority is universally accepted, this reading''s legal force is amplified. If interpretive authority remains contested, the constraint''s effective suppression and extractiveness are reduced due to persistent challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, preference, 'Contestation over the legitimate interpretive authority for the resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1995, 0.83).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1980, 0.67).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
