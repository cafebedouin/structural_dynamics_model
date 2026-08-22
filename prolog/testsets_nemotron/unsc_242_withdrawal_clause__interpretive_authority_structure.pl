% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: Interpretive Authority Structure of UNSC Resolution 242 Withdrawal Clause
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UNSC Resolution 242's withdrawal clause ('withdrawal of Israeli armed
 *   forces from territories occupied in the recent conflict') contains a
 *   deliberate textual ambiguity between English ('from territories') and
 *   French ('des territoires' — 'from the territories'). This constraint
 *   story addresses not the substantive withdrawal obligation, but the
 *   meta-dispute over WHO has authority to resolve that ambiguity. Three
 *   actors claim interpretive authority: the ICJ (judicial interpretation),
 *   the drafting states/authorial intent, and the occupying state (customary
 *   practice/secure boundaries). The meta-dispute itself functions as a
 *   constraint: it prevents definitive legal resolution, allowing the
 *   occupying state to maintain de facto control while the substantive
 *   ambiguity persists. The beneficiaries are those with veto power or
 *   non-cooperation capacity; the victims are those seeking legal closure.
 *   This reading instantiates a Snare — the coordination story (legal
 *   interpretive hierarchy) is cover; persistence depends on coercion (veto,
 *   non-recognition) and on suppressing authoritative judicial resolution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "Interpretive Authority Structure of UNSC Resolution 242 Withdrawal Clause").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '3a39985e-a3a0-4adf-8815-905f29ab9d5e').
narrative_ontology:cs_kernel_codification('3a39985e-a3a0-4adf-8815-905f29ab9d5e', formalized).
narrative_ontology:cs_authority_grounding('3a39985e-a3a0-4adf-8815-905f29ab9d5e', extraction).
narrative_ontology:cs_interpretation_layer_present('3a39985e-a3a0-4adf-8815-905f29ab9d5e').
narrative_ontology:cs_reading_relation('3a39985e-a3a0-4adf-8815-905f29ab9d5e', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a39985e-a3a0-4adf-8815-905f29ab9d5e', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('3a39985e-a3a0-4adf-8815-905f29ab9d5e', foundational, interpretive_authority_requires_enforcement_capacity).
narrative_ontology:cs_axiom_status(interpretive_authority_requires_enforcement_capacity, holdable).
narrative_ontology:cs_axiom_grounding('3a39985e-a3a0-4adf-8815-905f29ab9d5e', interpretive_authority_requires_enforcement_capacity, conventional).
narrative_ontology:cs_axiom('3a39985e-a3a0-4adf-8815-905f29ab9d5e', secondary, textual_ambiguity_serves_diplomatic_function).
narrative_ontology:cs_axiom_status(textual_ambiguity_serves_diplomatic_function, holdable).
narrative_ontology:cs_axiom_grounding('3a39985e-a3a0-4adf-8815-905f29ab9d5e', textual_ambiguity_serves_diplomatic_function, instrumental).
narrative_ontology:cs_reference_frame('3a39985e-a3a0-4adf-8815-905f29ab9d5e', un_charter_judicial_interpretation_hierarchy).
narrative_ontology:cs_drift_state('3a39985e-a3a0-4adf-8815-905f29ab9d5e', contemporary_occupation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a39985e-a3a0-4adf-8815-905f29ab9d5e', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_security_council_members_with_veto).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, interpretive_authority_is_inherently_political).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, textual_ambiguity_enables_strategic_noncompliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the territory in question and administers settlement activity. Claims customary practice and 'secure boundaries' principle justify retention. Benefits from interpretive paralysis because it maintains de facto control while legal ambiguity persists. Can veto or ignore adverse interpretations through non-cooperation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary).

% Original drafters (US, UK, USSR, France) who authored the deliberate English/French textual ambiguity. Benefit from retaining authorial intent as a live interpretive claim that preserves their historical diplomatic compromise. Their institutional successors (current permanent Security Council members) inherit this veto-position.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, beneficiary,
    institutional, generational, mobile, global).

% P5 members who can block any authoritative resolution that would fix the withdrawal scope. Benefit from the meta-dispute because it preserves Security Council primacy over judicial interpretation and prevents precedent that could constrain their own future actions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_security_council_members_with_veto, beneficiary,
    institutional, generational, analytical, global).

% Palestinian population under occupation whose territorial rights, self-determination, and daily life are governed by the unresolved ambiguity. Bears the material costs of settlement expansion, movement restrictions, and legal limbo. No exit from the constraint's effects; international legal mechanisms are the only available recourse but are blocked by the interpretive authority dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population, payer,
    powerless, biographical, trapped, local).

% Arab states, EU members, and other UN members who need definitive legal resolution for diplomatic normalization, trade relations, and treaty compliance. Pay diplomatic and economic costs of perpetual ambiguity. Can pursue ICJ advisory opinions or GA resolutions but cannot overcome Security Council veto or occupying state non-cooperation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_legal_closure, payer,
    organized, biographical, constrained, global).

% Claims judicial interpretation authority under UN Charter Article 96 and Statute Article 65. Has issued advisory opinions (Wall Opinion 2004) but lacks enforcement power. Structurally excluded from binding resolution by Security Council primacy and occupying state non-recognition. Its interpretive claim remains live but ineffective without enforcement mechanism.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, excluded,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The interpretive authority structure coordinates nothing — it is a meta-dispute that prevents coordination on the substantive withdrawal obligation. The ambiguity itself is the mechanism.
% TRANSFER_FUNCTION: Transfers the cost of legal uncertainty from the occupying state and drafting states (who retain strategic flexibility and veto power) to the occupied population and states seeking legal closure (who bear the material and diplomatic costs of ambiguity).
% ABSENT_VOICES: The occupied Palestinian population is structurally excluded from the interpretive authority contest — they are the object of the dispute, not a recognized party to it. Their representatives have no standing in Security Council proceedings, ICJ advisory processes, or bilateral negotiations over the clause's meaning.
% DISAPPEARANCE_RATIONALE: If the interpretive authority dispute vanished overnight — i.e., if a single authoritative interpreter were recognized — the substantive withdrawal obligation would become determinate, forcing either full withdrawal, negotiated territorial exchange, or explicit Security Council authorization for retention. The diplomatic, legal, and material situation on the ground would reorganize around that determinate obligation.
% FOUNDING_PROBLEM: The interpretive authority structure was not 'built' — it emerged from the deliberate textual ambiguity of Resolution 242 (1967), which was a diplomatic compromise to secure adoption. The founding problem was: how to get a ceasefire resolution adopted when parties fundamentally disagreed on the territorial endpoint. The ambiguity was the solution.
% FOUNDING_PROBLEM_CORROBORATION: The original drafters (Rostow, Jarring, Kuznetsov) explicitly documented the deliberate ambiguity as a compromise mechanism in memoirs and diplomatic cables. The ceasefire context (1967 war) is long past. The arrangement persists not because the founding problem lives, but because the ambiguity now serves as a structural shield for the occupying state's territorial control and the P5's institutional prerogatives. No non-beneficiary party attests the founding problem is still live.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) because the meta-dispute extracts the cost of legal uncertainty from the occupied population and states seeking closure, transferring strategic flexibility to the occupying state and drafting states. Suppression (0.72) because the constraint's persistence requires active veto use, non-recognition of ICJ jurisdiction, and settlement activity that creates facts on the ground. Theater ratio (0.55) is elevated because the legal interpretive apparatus (ICJ opinions, UN debates, diplomatic processes) performs the appearance of legal resolution while the meta-dispute ensures it remains non-binding. Accessibility collapse (0.45) is moderate — alternatives (ICJ binding judgment, GA resolution, bilateral treaty) exist but are structurally blocked. Resistance (0.68) is significant — from ICJ advisory opinions, GA resolutions, BDS movement, and state recognition of Palestine — but has not shifted the meta-dispute.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state/drafting states' seats, the interpretive ambiguity is a feature — a diplomatic tool that preserves flexibility. From the occupied population/ICJ seats, it is a bug — a structural denial of legal rights. The engine computes this divergence from the structural data: the same meta-dispute is coordination for the powerful, extraction for the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state sits at d ≈ 0.1 (full beneficiary): it controls the territory, sets facts on the ground, and benefits from ambiguity. Drafting states/P5 at d ≈ 0.2 (beneficiaries): they retain veto control and avoid precedent constraining their own actions. Occupied population at d ≈ 0.95 (full target): trapped, bears all material costs, no exit. States seeking closure at d ≈ 0.7 (target): organized but constrained exit (diplomatic costs). ICJ at d ≈ 0.8 (excluded target): claims authority but structurally blocked from exercising it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 ceasefire diplomacy) is dead. The arrangement persists because the ambiguity now serves new extractive functions: it shields settlement expansion, preserves Security Council primacy over judicial interpretation, and enables strategic noncompliance. This is classic mandatrophy — the mandate (ceasefire resolution) outlived its function, and the constraint (interpretive authority dispute) was repurposed for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_ambiguity,
    'Is the meta-dispute over interpretive authority a genuine legal indeterminacy, or a strategic construction by beneficiaries to prevent definitive resolution?',
    'Comparative analysis of other UNSC resolutions with similar textual ambiguities: if they were resolved authoritatively without meta-dispute, this one''s persistence is strategic.',
    'If strategic construction, the constraint is a pure snare with ε driven by deliberate maintenance. If genuine indeterminacy, ε reflects the cost of resolving a real legal gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_ambiguity, conceptual, 'Whether the interpretive authority dispute is endogenous to the text or exogenous to it').

omega_variable(
    icj_authority_erosion_trajectory,
    'Does the ICJ''s repeated exclusion from binding resolution of this dispute represent a systemic erosion of judicial authority in international law, or a case-specific political blockage?',
    'Longitudinal study of ICJ advisory opinion compliance rates and Security Council referral patterns across comparable territorial disputes.',
    'If systemic erosion, the constraint''s suppression mechanism is structural and likely to persist. If case-specific, a political shift could open the judicial path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(icj_authority_erosion_trajectory, empirical, 'Whether the ICJ''s structural exclusion generalizes beyond this kernel').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''unsc_242_withdrawal_clause'' properly name a single commitment, or does it conflate the textual provision with the interpretive authority structure that governs it?',
    'Decompose the kernel into (a) the textual provision as a freestanding commitment and (b) the interpretive authority structure as a meta-commitment; assess whether they have distinct stakeholder sets and ε profiles.',
    'If the kernel conflates two structures, the sibling readings may be reading different constraints rather than the same constraint differently — requiring further decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel boundary is correctly drawn or conflates object and meta-level').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(unsc_tr_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1973, 0.35).
narrative_ontology:measurement(unsc_tr_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1982, 0.42).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1993, 0.48).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(unsc_tr_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2004, 0.54).
narrative_ontology:measurement(unsc_tr_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2016, 0.55).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(unsc_be_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(unsc_be_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1982, 0.62).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(unsc_be_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2004, 0.75).
narrative_ontology:measurement(unsc_be_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2016, 0.77).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc_su_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1973, 0.5).
narrative_ontology:measurement(unsc_su_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1982, 0.58).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(unsc_su_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(unsc_su_t2016, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2016, 0.71).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single natural-language concept 'UNSC 242 withdrawal clause' into three structurally distinct constraints: (1) the substantive maximal withdrawal reading, (2) the substantive partial withdrawal reading, and (3) this meta-constraint on interpretive authority. Their ε values differ substantially: the substantive readings have moderate ε (coordination function with contested scope), while this meta-reading has high ε (authority ambiguity as extraction mechanism). The meta-dispute structurally enables both substantive readings to remain live by preventing authoritative resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.15).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
