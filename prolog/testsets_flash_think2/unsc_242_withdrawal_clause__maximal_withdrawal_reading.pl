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
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Maximal Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint is the 'maximal withdrawal' reading of UN Security
 *   Council Resolution 242's withdrawal clause, which asserts mandatory
 *   withdrawal from all occupied territories based on Charter Article 2(4)
 *   and the French definite article. This reading posits a strong, binding
 *   legal obligation on the occupying state to fully retrocede territories.
 *   Sibling readings include the 'partial withdrawal' reading, which argues
 *   for discretionary scope, and the 'interpretive authority structure'
 *   itself, which contests who has the final say on interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause: Maximal Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '0528a5c7-bfa0-4878-87ca-1d2c829bbd4c').
narrative_ontology:cs_kernel_codification('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', fixed_text).
narrative_ontology:cs_authority_grounding('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', lineage).
narrative_ontology:cs_interpretation_layer_present('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c').
narrative_ontology:cs_reading_relation('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', foundational, territorial_integrity_is_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', territorial_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', foundational, definite_article_controls_treaty_scope).
narrative_ontology:cs_axiom_status(definite_article_controls_treaty_scope, holdable).
narrative_ontology:cs_axiom_grounding('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', definite_article_controls_treaty_scope, conventional).
narrative_ontology:cs_reference_frame('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', un_charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', post_1967_occupation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0528a5c7-bfa0-4878-87ca-1d2c829bbd4c', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_community).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, france_drafting_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the populations and states whose territories were occupied in 1967. They are the direct beneficiaries of the maximal withdrawal reading, as it mandates full retrocession of their lands. Their power to enforce this is limited, relying on international pressure.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    powerless, generational, constrained, regional).

% The broader international community benefits from the upholding of international law, particularly the principle of territorial integrity. This reading reinforces the stability of the international system and the authority of UN resolutions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_community, beneficiary,
    institutional, generational, analytical, global).

% The state occupying the territories (Israel) is the primary target of this constraint, as it demands full withdrawal. Its resistance is high, citing security concerns and historical claims, and its identity is deeply intertwined with the territories.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, identity_locked, national).

% As the body that passed Resolution 242, the UNSC is the primary agenda-setter. It is responsible for maintaining international peace and security, and this reading of the resolution is a key instrument in that mandate, though its enforcement power is constrained by political dynamics and vetoes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% The International Court of Justice serves as an authoritative interpreter of international law. While not directly enforcing the resolution, its legal opinions and advisory roles significantly influence the international community's understanding and application of this reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, observer,
    institutional, civilizational, analytical, global).

% France, as a key drafting state of Resolution 242, strongly advocates for the maximal withdrawal reading, emphasizing the significance of the French definite article. Its diplomatic standing benefits from the upholding of this interpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, france_drafting_state, beneficiary,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to resolve the Israeli-Palestinian conflict by establishing a clear legal framework for the return of occupied territories, thereby promoting peace and stability in the region and upholding the principle of territorial integrity.
% TRANSFER_FUNCTION: Mandates the transfer of territorial control and sovereignty from the occupying state to the dispossessed claimants, and transfers legitimacy from unilateral occupation to a framework of international law and UN resolutions.
% ABSENT_VOICES: The populations under occupation, whose daily lives are directly affected by the constraint but who lack direct representation in the UN Security Council or other high-level diplomatic forums. Their voices are often mediated through their national representatives or NGOs.
% DISAPPEARANCE_RATIONALE: If this maximal withdrawal interpretation vanished, the primary legal basis for demanding full retrocession of occupied territories would collapse. This would legitimize continued occupation, destabilize international relations, potentially ignite new conflicts, and fundamentally undermine the principle of territorial integrity enshrined in the UN Charter.
% FOUNDING_PROBLEM: The immediate aftermath of the 1967 Six-Day War, specifically the occupation of territories by Israel, and the urgent need to establish a framework for a just and lasting peace based on withdrawal from occupied territories and secure, recognized boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The UN General Assembly, numerous international legal scholars, and the dispossessed claimant states consistently corroborate the ongoing nature of the problem and the necessity of withdrawal. This is further supported by decades of UN resolutions reaffirming the illegality of occupation and the right to self-determination.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness is high (0.85) because this reading demands full retrocession, imposing a significant cost on the occupying state which resists this outcome. Suppression is also high (0.75) due to the mandatory nature of a UN Security Council resolution and the international pressure it implies, even if enforcement is often politically constrained. The theater ratio is low (0.10) because the demand for withdrawal is a core, active function of the resolution, not a performative one. Resistance is very high (0.90) from the occupying state, which has consistently refused full withdrawal. The metrics remain stable over time, reflecting the persistent legal claim, even if its effective enforcement fluctuates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dispossessed claimants and the international community, this constraint is a legitimate and necessary legal instrument for justice and peace. From the occupying state's perspective, it is an overly extractive and politically motivated demand that disregards its security concerns and historical claims. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The dispossessed claimants and the international community are clear beneficiaries, as this reading upholds their rights and the international legal order. France, as a key drafting state, also benefits from its interpretation being upheld. The occupying state is the primary target, bearing the full cost of withdrawal. The UNSC acts as the agenda-setter, while the ICJ observes and influences interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'How should the textual ambiguity between the French (definite article ''des'') and English (indefinite article ''from'') versions of Resolution 242''s withdrawal clause be resolved?',
    'Authoritative ruling by the International Court of Justice, or a new, unambiguous Security Council resolution. Historical drafting records could also provide clarity.',
    'If the English indefinite article is deemed controlling, the constraint''s extractiveness would decrease, potentially shifting it towards a ''partial withdrawal'' reading. If the French definite article is confirmed, this maximal withdrawal reading is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'Ambiguity in the French vs. English text of Resolution 242 regarding the scope of withdrawal.').

omega_variable(
    interpretive_authority_legitimacy,
    'Which body or actor holds the legitimate authority to definitively interpret UN Security Council Resolution 242?',
    'Universal acceptance of ICJ''s advisory opinions, or a clear mandate from the UN General Assembly or Security Council. Consensus among drafting states could also contribute.',
    'If the interpretive authority is definitively assigned to a body supporting maximal withdrawal, the constraint''s legitimacy and potential for enforcement increase. If it''s assigned to an actor favoring partial withdrawal, this reading''s force diminishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, preference, 'Contested authority over the interpretation of UN Resolution 242.').

omega_variable(
    enforcement_gap_impact,
    'To what extent does the persistent lack of full enforcement of Resolution 242 undermine the maximal withdrawal reading''s de jure status and international legal force?',
    'Analysis of state practice and opinio juris over time; legal scholarship on the erosion of customary international law due to non-compliance. A shift in UN member states'' diplomatic positions.',
    'If the lack of enforcement is deemed to have significantly eroded the constraint''s legal force, its effective extractiveness and suppression would decrease, potentially leading to a reclassification towards a ''piton'' or ''tangled_rope'' if the coordination function is still claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_impact, empirical, 'The gap between the legal mandate for withdrawal and its actual implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of UN Security Council Resolution 242's withdrawal clause, each with different structural properties and implications. This 'maximal withdrawal' reading emphasizes full retrocession based on the French text and Charter Article 2(4).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
