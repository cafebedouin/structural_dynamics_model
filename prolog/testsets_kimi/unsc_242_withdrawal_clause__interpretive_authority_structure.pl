% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: UNSC 242 Interpretive Authority Contest
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) calls for Israeli withdrawal
 *   from 'territories occupied in the recent conflict,' but textual variance
 *   between the English indefinite and French definite articles creates
 *   ambiguity over whether withdrawal must be total or partial. This
 *   constraint story does not adjudicate substantive scope; it models the
 *   meta-constraint in which the authority to resolve that ambiguity is
 *   itself contested among the ICJ (judicial interpretation), the drafting
 *   states (authorial intent), and the occupying state (customary practice
 *   and security necessity). The contest perpetuates the ambiguity, allowing
 *   powerful parties with veto or non-cooperation capacity to block
 *   definitive resolution while territorial populations and status-seeking
 *   states bear the costs of perpetual uncertainty. Claimed type is snare:
 *   the coordination story (pluralist legal interpretation) serves as cover
 *   for an extraction mechanism (indefinite deferral of territorial
 *   settlement).
 *
 * KEY AGENTS:
 *   - permanent_council_members: Primary beneficiary (institutional/arbitrage) â veto power converts authority contest into diplomatic leverage
 *   - occupying_state: Primary beneficiary (powerful/mobile) â non-cooperation prevents any interpretive authority from enforcing withdrawal
 *   - occupied_territory_populations: Primary target (powerless/trapped) â bear the human cost of unresolved territorial status
 *   - member_states_seeking_closure: Secondary target (moderate/constrained) â diplomatic capacity consumed by recurring unresolved debate
 *   - icj: Agenda-setter (institutional/constrained) â claims interpretive authority but lacks enforcement
 *   - drafting_states: Agenda-setter (powerful/mobile) â invoke authorial intent to preserve interpretive influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Interpretive Authority Contest").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'b34726ce-ec66-4084-b3c0-145e1cc6ecb8').
narrative_ontology:cs_kernel_codification('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', fixed_text).
narrative_ontology:cs_authority_grounding('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', distributed).
narrative_ontology:cs_reading_relation('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', interpretive_authority_is_contested, conventional).
narrative_ontology:cs_axiom('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', foundational, substantive_resolution_requires_authority_settlement).
narrative_ontology:cs_axiom_status(substantive_resolution_requires_authority_settlement, holdable).
narrative_ontology:cs_axiom_grounding('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', substantive_resolution_requires_authority_settlement, conventional).
narrative_ontology:cs_reference_frame('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', pluralist_interpretive_regime).
narrative_ontology:cs_drift_state('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', contemporary_un_paralysis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b34726ce-ec66-4084-b3c0-145e1cc6ecb8', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_council_members).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_territory_populations).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, member_states_seeking_closure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise veto power or the threat thereof to block Chapter VII enforcement actions or binding interpretations that would compel territorial withdrawal; the unresolved authority contest lets them defer costly enforcement decisions indefinitely while maintaining diplomatic leverage over the dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_council_members, beneficiary,
    institutional, generational, arbitrage, global).

% Maintains territorial control by invoking customary practice and security necessity; non-cooperation with UN mechanisms and rejection of ICJ jurisdiction prevent any single interpretive authority from mandating withdrawal; ambiguity is the legal environment in which occupation persists.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, biographical, mobile, national).

% Live under prolonged military administration and displacement without sovereign resolution; cannot exit the ambiguity because their status is the object of the contest; they bear the direct human and economic costs of indefinite occupation and stalled self-determination.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_territory_populations, payer,
    powerless, generational, trapped, local).

% Seek definitive legal interpretation to mobilize sanctions, diplomatic isolation, or peace enforcement; paralyzed by the authority contest because ICJ advisory opinions lack enforcement and Security Council action is vetoed; their diplomatic capital is consumed in recurring debates that produce no resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, member_states_seeking_closure, payer,
    moderate, biographical, constrained, global).

% Claims primacy in treaty interpretation under the Vienna Convention on the Law of Treaties and its own statute; issues advisory opinions and judgments on territorial questions but faces non-recognition and non-compliance from states that reject its authority over the resolution; its docket reflects the contest but cannot resolve it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj, agenda_setter,
    institutional, civilizational, constrained, global).

% Assert that original negotiating history and textual choices in the authenticated language versions should control interpretation; invoke archival records and diplomatic memoirs to claim a privileged interpretive voice; their continued relevance depends on the text never being settled by an external authority.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, agenda_setter,
    powerful, civilizational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedural framework in which multiple claimants to legal authorityâjudicial, diplomatic, and state practiceâcan contest the meaning of a binding resolution without any single actor unilaterally imposing an interpretation, theoretically preserving legitimacy through inclusivity.
% TRANSFER_FUNCTION: Moves the political capacity to defer definitive territorial withdrawal from the international community and occupied populations to states possessing veto power in the Security Council or the practical capacity to ignore UN mechanisms.
% ABSENT_VOICES: Occupied territory populations are not direct parties to the interpretive authority contest; their interests are represented by states or UN bodies but they have no seat in the ICJ-drafting state-occupier triangle. Field-level UN bureaucrats and special coordinators who observe the daily costs of ambiguity are similarly absent from the high-level authority claims.
% DISAPPEARANCE_RATIONALE: If a single interpretive authority were universally acceptedâwhether the ICJ, the drafting states, or a new mechanismâthe substantive ambiguity of Resolution 242 would become resolvable, enforcement pathways would open or close, and the current distribution of veto and non-cooperation benefits would collapse; the territorial and diplomatic status quo would reorganize.
% FOUNDING_PROBLEM: How to adopt a Security Council resolution on Middle East withdrawal in 1967 that preserved diplomatic consensus among permanent members while papering over irreconcilable differences on precise territorial scope.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and critical international law scholars outside the permanent member and occupying state camps attest that the textual ambiguity was intentional and that the current authority contest is an emergent pathology of the original diplomatic compromise, not its designed continuation; no neutral party claims the current deadlock serves the resolution's original purpose.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the authority contest is not epistemically productive; it systematically defers resolution to the benefit of veto-wielding and non-cooperating parties. Suppression is high (0.78) because the only accessible alternativesâICJ adjudication, Security Council enforcement, or drafting-state consensusâare each blocked by another claimant's refusal to accept the authority. Theater is moderate (0.45): UN debates, advisory opinions, and scholarly conferences perform interpretive diligence without resolving the underlying contest, normalizing the deadlock. Accessibility collapse is substantial (0.72) because once an actor understands the authority structure, the apparent exits (litigation, council action, diplomatic history) each collapse into another veto point. Resistance is moderate (0.55): victim states and populations continually challenge the ambiguity, but the institutional architecture absorbs their resistance without conversion into resolution.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats experience the authority contest as a legitimate pluralism that protects their prerogatives; the payer seats experience the identical structure as an institutional trap that consumes their resources without resolution. The engine computes this divergence from the structural dataâbeneficiary/victim declarations plus exit asymmetryârather than from any reconciled narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   permanent_council_members and occupying_state are declared beneficiaries: they collect political flexibility and territorial retention from the perpetual ambiguity. Their directionality sits near the full-beneficiary end (low d). occupied_territory_populations and member_states_seeking_closure are declared victims: they pay in sovereignty deferred and diplomatic capital exhausted. Their directionality sits near the full-target end (high d). icj and drafting_states are not in either array; their structural position is mixedâthe ICJ claims authority it cannot enforce (constrained exit, near-symmetric d), while drafting_states leverage historical position but lack current enforcement (mobile exit, moderate d toward beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a rope because alternatives are suppressed rather than enabled by the pluralism; it is not a tangled rope because there is no genuine coordination function being extracted fromâthe coordination story (inclusive interpretive pluralism) is cover for strategic deferral. It is not a piton because the beneficiaries are concentrated and actively profit from maintenance. The founding problem (1967 diplomatic consensus) is dead, yet the arrangement persists because it serves live extraction interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_contest_strategic_or_epistemic,
    'Is the contest over interpretive authority a genuine methodological pluralism in international law, or a strategic deferral mechanism benefiting powerful states?',
    'Comparative analysis of authority contests across Security Council resolutions: if authority is only contested when powerful states face unfavorable substantive readings, the pattern supports strategic deferral.',
    'If strategic, the constraint''s coordination function (pluralist interpretation) is cover for extraction and the effective extraction is higher than a genuine pluralism would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_contest_strategic_or_epistemic, conceptual, 'Whether authority pluralism is genuine or strategic deferral').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment system framing (fixed textual kernel with distributed authority) fully capture the constraint, or is the operative mechanism better modeled as raw power politics using legal form?',
    'Examine whether removing the legal-textual frame (treating the situation as pure geopolitics) changes the stakeholder cost-benefit structure.',
    'If the legal frame is inessential, the constraint''s classification as a commitment system is mistaken and the extraction is better modeled as direct coercion without commitment-system mediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framing between commitment system and power politics').

omega_variable(
    victim_coalition_possibility,
    'Could the payer states (member_states_seeking_closure) form a coalition capable of overriding the veto and non-cooperation capacity of beneficiaries?',
    'Historical analysis of General Assembly Uniting for Peace procedures and state practice to assess whether a coalition mechanism exists or has been attempted.',
    'If coalition power is structurally available but unmobilized, the victims'' powerlessness is contingent rather than structural, lowering effective extraction for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_possibility, empirical, 'Coalition possibility among victim states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_ias_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unsc_242_ias_tr_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 8, 0.2).
narrative_ontology:measurement(unsc_242_ias_tr_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 16, 0.25).
narrative_ontology:measurement(unsc_242_ias_tr_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 24, 0.3).
narrative_ontology:measurement(unsc_242_ias_tr_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 32, 0.35).
narrative_ontology:measurement(unsc_242_ias_tr_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 40, 0.4).
narrative_ontology:measurement(unsc_242_ias_tr_t48, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 48, 0.43).
narrative_ontology:measurement(unsc_242_ias_tr_t56, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(unsc_242_ias_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(unsc_242_ias_be_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(unsc_242_ias_be_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(unsc_242_ias_be_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(unsc_242_ias_be_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(unsc_242_ias_be_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(unsc_242_ias_be_t48, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 48, 0.81).
narrative_ontology:measurement(unsc_242_ias_be_t56, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 56, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(unsc_242_ias_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(unsc_242_ias_su_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(unsc_242_ias_su_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(unsc_242_ias_su_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(unsc_242_ias_su_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(unsc_242_ias_su_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(unsc_242_ias_su_t48, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(unsc_242_ias_su_t56, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 56, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'UNSC 242 withdrawal clause' conflates three structurally distinct constraints: two substantive scope readings (maximal, partial) and one meta-level authority contest (this reading). They form a constraint family linked by shared textual kernel but separated by epsilon-invariance: the authority contest has high extraction because it perpetuates ambiguity, while the substantive readings have lower extraction if considered in isolation. This decomposition follows the BGS gold standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
