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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause: Maximal Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'maximal withdrawal' reading of UNSC
 *   Resolution 242, which mandates the withdrawal of the occupying state from
 *   'all' territories occupied in the 1967 conflict. This reading emphasizes
 *   the French definite article 'des' ('the' territories) and the UN
 *   Charter's Article 2(4) principle of territorial integrity. It is claimed
 *   as a Rope because it aims to coordinate international behavior around a
 *   clear legal principle, but its high extractiveness and active enforcement
 *   against the occupying state give it Snare-like qualities from that seat.
 *   The metrics reflect the ongoing, high-stakes nature of this
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
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e459329d-68dd-4112-a613-061120f98fef').
narrative_ontology:cs_kernel_codification('e459329d-68dd-4112-a613-061120f98fef', fixed_text).
narrative_ontology:cs_authority_grounding('e459329d-68dd-4112-a613-061120f98fef', lineage).
narrative_ontology:cs_interpretation_layer_present('e459329d-68dd-4112-a613-061120f98fef').
narrative_ontology:cs_reading_relation('e459329d-68dd-4112-a613-061120f98fef', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e459329d-68dd-4112-a613-061120f98fef', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('e459329d-68dd-4112-a613-061120f98fef', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e459329d-68dd-4112-a613-061120f98fef', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('e459329d-68dd-4112-a613-061120f98fef', secondary, french_text_controls_treaty_interpretation).
narrative_ontology:cs_axiom_status(french_text_controls_treaty_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('e459329d-68dd-4112-a613-061120f98fef', french_text_controls_treaty_interpretation, conventional).
narrative_ontology:cs_reference_frame('e459329d-68dd-4112-a613-061120f98fef', post_charter_territorial_integrity_regime).
narrative_ontology:cs_drift_state('e459329d-68dd-4112-a613-061120f98fef', contemporary_diplomatic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e459329d-68dd-4112-a613-061120f98fef', '').
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

% Parties whose territory was occupied and who seek full retrocession based on the maximal withdrawal interpretation. Their legal position is strong but their practical ability to enforce it is limited without international backing.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    powerless, generational, trapped, regional).

% The abstract system of international law, which benefits from the upholding of territorial integrity and the mandatory nature of withdrawal. Its legitimacy is reinforced by this interpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).

% The state that occupied territories in conflict. This reading mandates its full withdrawal, imposing significant strategic and political costs. Its exit options are constrained by international pressure and potential sanctions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    powerful, biographical, constrained, regional).

% The permanent and non-permanent members of the UN Security Council, responsible for drafting and enforcing resolutions. They interpret and apply the resolution, but their consensus is often difficult to achieve.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_members, agenda_setter,
    institutional, generational, mobile, global).

% The International Court of Justice, which provides advisory opinions and adjudicates disputes based on international law, including treaty interpretation. Its rulings can reinforce or challenge specific readings of UNSC 242.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, mandatory legal framework for the resolution of territorial disputes arising from conflict, aiming to prevent annexation and ensure the principle of territorial integrity.
% TRANSFER_FUNCTION: Mandates the transfer of full territorial control from the occupying state back to the dispossessed claimants, along with the associated resources and sovereignty.
% ABSENT_VOICES: Future generations of dispossessed populations, whose claims to full territorial integrity are enshrined in this reading but who have no direct voice in current diplomatic negotiations.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal reading vanished, the legal basis for full retrocession would collapse, legitimizing partial occupation and fundamentally altering the international legal landscape regarding territorial integrity and post-conflict resolution. Dispossessed claimants would lose their primary legal leverage.
% FOUNDING_PROBLEM: The problem of territorial acquisition by force following armed conflict, threatening international peace and security and undermining the principle of non-aggression.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, the ICJ, and numerous UN member states (excluding the occupying state and its allies) consistently corroborate that the problem of territorial acquisition by force remains live and that UNSC 242 is a foundational instrument against it. Historical records of UN debates and subsequent resolutions also support this.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading demands comprehensive and non-negotiable territorial retrocession, imposing significant costs on the occupying state. Suppression is also high (0.7) due to the active diplomatic, legal, and potential economic pressure required to enforce this interpretation against a powerful state. Theater ratio is low (0.2) as the commitment to territorial integrity is generally genuine, though enforcement can be performative when political will is lacking. The metrics reflect the persistent contestation and the substantial demands this reading places on the target.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dispossessed claimants and the international legal order, this is a just and necessary Rope for global stability. From the occupying state's perspective, it is a highly extractive Snare, demanding concessions that threaten its perceived security interests. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed claimants and the international legal order are beneficiaries, as this reading vindicates their core principles and claims. The occupying state is the primary target, facing mandatory and comprehensive demands for withdrawal. UNSC members are agenda-setters, mediating enforcement. The ICJ is an observer, providing analytical input. The structural relationships drive the high extractiveness and suppression.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Is the French definite article ''des'' (''the'' territories) truly controlling for the scope of withdrawal, or does the English indefinite article ''from territories'' allow for partial withdrawal?',
    'A definitive, universally accepted ruling by the ICJ or a new, unambiguous UNSC resolution that explicitly clarifies the scope of withdrawal.',
    'If the French article is definitively controlling, this maximal withdrawal reading is reinforced. If the English article is deemed to allow for discretion, the ''partial withdrawal'' reading gains legitimacy, reducing the extractiveness of this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'Ambiguity in the dual-language text of UNSC 242 regarding the scope of withdrawal.').

omega_variable(
    customary_international_law_drift,
    'Has customary international law evolved to permit partial retention of occupied territory for ''secure boundaries,'' overriding the strict territorial integrity principle?',
    'Analysis of state practice (usus) and opinio juris (acceptance as law) by international legal bodies over a sustained period, particularly in other post-conflict scenarios.',
    'If customary law is found to have drifted, the ''partial withdrawal'' reading gains strength, and the maximal reading''s claim to be a Rope (coordinating around a fixed principle) weakens, potentially reclassifying it as a Tangled Rope or Snare due to its contested legal basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_international_law_drift, empirical, 'Whether state practice has eroded the strict interpretation of territorial integrity.').

omega_variable(
    enforcement_capacity_vs_political_will,
    'Is the observed suppression level a function of the UN''s actual enforcement capacity, or primarily a reflection of the political will of key UNSC members?',
    'Comparative analysis of UN enforcement actions in similar contexts where political will was either unified or divided, holding constant the legal clarity of the mandate.',
    'If political will is the primary driver, the constraint''s ''requires_active_enforcement'' is more fragile than the suppression metric suggests, making it more susceptible to becoming a Piton if political consensus erodes. If capacity is the limit, strengthening UN mechanisms could make the constraint more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_political_will, empirical, 'Distinguishing between structural enforcement capacity and political will in UN resolutions.').


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
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This is one reading of the UNSC 242 withdrawal clause kernel. It is linked to the 'partial withdrawal' reading and the 'interpretive authority structure' reading, which represent alternative interpretations and contestations over the resolution's meaning and enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
