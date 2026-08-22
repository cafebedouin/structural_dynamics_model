% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Contested Authority to Adjudicate UNSC 242's Withdrawal Ambiguity
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israeli armed forces
 *   from territories occupied in the recent conflict' — a phrase famously
 *   ambiguous between the definite French 'des territoires' and the
 *   indefinite English 'from territories.' Beyond that textual dispute lies a
 *   deeper structural problem: even if the textual ambiguity could be
 *   resolved by some authoritative reading, no single body is recognized by
 *   all parties as having the authority to render that reading binding. The
 *   ICJ claims judicial interpretive competence under customary international
 *   law and the VCLT; the original drafting states (chiefly the US, UK, USSR
 *   at the time) claim their diplomatic record of authorial intent should
 *   control; the occupying state claims that decades of subsequent state
 *   practice and negotiated partial withdrawals (Sinai, parts of the West
 *   Bank) constitute the operative customary interpretation. Because Security
 *   Council action requires consensus among veto-holders who benefit from the
 *   standing ambiguity, no procedural mechanism exists to force a choice
 *   among these competing authority claims.
 *
 * KEY AGENTS:
 *   - occupying_state_government: primary beneficiary of unresolved authority — selects favorable interpretive frame at will
 *   - drafting_states_bloc: claims authorial-intent authority; institutionally constrained by veto dynamics
 *   - international_court_of_justice: claims judicial authority; agenda-setting in scholarship, excluded from enforcement
 *   - permanent_security_council_members_with_veto: benefit from standing ambiguity as diplomatic leverage
 *   - displaced_populations_seeking_legal_closure: bear the accumulated cost, no standing, no lever
 *   - neighboring_states_seeking_border_certainty: bear planning and security costs of unresolved borders
 *   - un_administered_territories_populations: live under indefinitely provisional governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "Contested Authority to Adjudicate UNSC 242's Withdrawal Ambiguity").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc').
narrative_ontology:cs_kernel_codification('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', fixed_text).
narrative_ontology:cs_authority_grounding('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', distributed).
narrative_ontology:cs_reading_relation('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', foundational, no_single_body_holds_binding_interpretive_authority).
narrative_ontology:cs_axiom_status(no_single_body_holds_binding_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', no_single_body_holds_binding_interpretive_authority, conventional).
narrative_ontology:cs_axiom('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', secondary, customary_state_practice_can_substitute_for_adjudicated_meaning).
narrative_ontology:cs_axiom_status(customary_state_practice_can_substitute_for_adjudicated_meaning, holdable).
narrative_ontology:cs_axiom_grounding('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', customary_state_practice_can_substitute_for_adjudicated_meaning, conventional).
narrative_ontology:cs_reference_frame('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', id_1967_diplomatic_compromise_ambiguity).
narrative_ontology:cs_drift_state('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', contemporary_five_decade_impasse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbf802fd-0535-4d6e-9bb5-5e5c5b99cbcc', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_security_council_members_with_veto).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_seeking_border_certainty).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, un_administered_territories_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes customary practice and subsequent state conduct as the controlling interpretive authority, arguing that decades of administration, settlement, and negotiated partial withdrawals have themselves generated binding practice. Because the identity of the authoritative interpreter is unresolved, this state can select whichever interpretive frame (textual, intentionalist, practice-based) yields the most favorable reading at any given moment, and no external body can compel a different selection. Its exit option is effectively arbitrage across interpretive frames.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, agenda_setter).

% The states that negotiated Resolution 242 (primarily the five permanent members at the time) claim their own drafting-history record and diplomatic correspondence should control interpretation, since they authored the ambiguous language deliberately as a diplomatic compromise. They benefit from continued deference to their authorial intent claims but are also constrained: they cannot force a definitive ruling without one of their own number using or facing a veto, since the authority question routes back through Security Council procedure.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, payer).

% Claims judicial interpretation under the Vienna Convention on the Law of Treaties as the proper authority to resolve the ambiguity, and has issued advisory opinions bearing on related territorial questions. Its rulings are treated as authoritative by scholars and by parties who find them convenient, and functionally ignored by parties who do not, because the Court has no independent enforcement mechanism and no party is compelled to refer the specific question to it. It sets the interpretive agenda in legal scholarship while being excluded from actual enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, excluded).

% Each permanent member can block any Security Council resolution that would designate a single authoritative interpreter or endorse one reading over another. This veto capacity is itself a form of benefit from the standing ambiguity: as long as no single interpretive authority is fixed, each permanent member retains leverage to condition its support for either reading on unrelated diplomatic concessions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, permanent_security_council_members_with_veto, beneficiary,
    institutional, civilizational, arbitrage, global).

% Populations displaced by the 1967 conflict and its aftermath have no standing to appear before the ICJ, no seat at the Security Council, and no capacity to compel any party to accept a definitive interpretive authority. Every year the meta-dispute over who decides remains unresolved is a year in which their claims to return, compensation, or territorial status remain legally undecided. They bear the accumulated cost of the ambiguity with no lever to force resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_legal_closure, payer,
    powerless, biographical, trapped, regional).

% States bordering the disputed territories need stable, recognized boundaries for security planning, water rights administration, and economic development. They can lobby international bodies and file diplomatic protests, but cannot themselves adjudicate the authority question, and their bilateral leverage is limited relative to the great-power dynamics that keep the interpretive question unresolved at the Security Council level.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_seeking_border_certainty, payer,
    moderate, generational, constrained, regional).

% Residents of the territories under continued occupation or contested administration live under provisional arrangements that persist precisely because no body has been recognized as authoritative to end the provisionality. They have no direct channel to the ICJ, the drafting states, or the Security Council, and their daily governance, movement, and property rights remain suspended in the unresolved interpretive contest.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, un_administered_territories_populations, payer,
    powerless, generational, trapped, local).

% Academics and practitioners debate which interpretive authority should control — judicial, intentionalist, or customary-practice-based — and publish extensively on the French/English textual discrepancy. Their analysis shapes diplomatic argument and ICJ reasoning but cannot itself resolve which authority governs; the debate has itself become a durable feature of the field.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_scholarship_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a recognized interpretive authority would coordinate expectations among all parties to a contested treaty text, allowing states, populations, and courts to plan around a single settled meaning rather than multiple live readings — this is the genuine function that a resolved authority structure would serve.
% TRANSFER_FUNCTION: The unresolved authority question transfers the cost of legal uncertainty from parties who can exploit ambiguity (states with veto power, states benefiting from continued de facto arrangements) to parties who cannot (displaced populations, residents of administered territories, neighboring states needing settled borders). It moves the burden of proof and the burden of waiting onto those least equipped to bear it.
% ABSENT_VOICES: Displaced populations and residents of the territories under contested administration have no standing before the ICJ on this specific question, no vote at the Security Council, and no diplomatic delegation empowered to press for a definitive authority ruling. Their interest — closure — is structurally excluded from a debate conducted entirely among states and the Court.
% DISAPPEARANCE_RATIONALE: If a single recognized interpretive authority were established overnight (say, binding compulsory ICJ jurisdiction over the question, accepted by all parties), the substantive withdrawal dispute would still require resolution, but the mechanism for resolving it would exist — parties currently able to select among the ICJ, authorial-intent, and customary-practice frames depending on convenience would lose that selection power, and pressure toward a definitive territorial settlement would increase sharply.
% FOUNDING_PROBLEM: Resolution 242 was drafted in November 1967 to end a war and establish principles for a negotiated peace, deliberately leaving withdrawal scope ambiguous (via the French/English textual discrepancy) as the price of unanimous Security Council adoption at the time. The authority-to-interpret question was never separately addressed because the immediate diplomatic problem was securing any resolution at all, not anticipating how the ambiguity would be adjudicated decades later.
% FOUNDING_PROBLEM_CORROBORATION: The immediate 1967 diplomatic crisis that necessitated a deliberately ambiguous compromise text no longer exists as an active emergency; contemporary international legal scholars (writing independently of any party to the dispute) and several former UN officials involved in later mediation efforts have stated on the record that the drafting ambiguity has outlived its original diplomatic function and now serves primarily to preserve negotiating leverage for parties benefiting from the status quo — a reading corroborated by sources outside both the occupying state and the drafting-states bloc.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 by interval end) because the unresolved authority question is not neutral — it systematically favors parties who can act unilaterally while the dispute remains open (the occupying state) and parties who can block procedural resolution (veto-holding permanent members), at the direct expense of parties who need closure and cannot compel it. Suppression is moderate-high (0.62): there is no single coercive enforcer, but the veto mechanism and the absence of compulsory jurisdiction function as structural suppression of any resolution pathway. Theater ratio is authored high and rising (0.2 to 0.58) because an increasing share of diplomatic and legal activity around the resolution — position papers, advisory opinion requests, scholarly conferences — functions as performance of engagement with the question rather than progress toward resolving who may resolve it. Accessibility collapse is authored moderate (0.4) rather than high because the substantive interpretive options (maximal/partial withdrawal) remain visibly, publicly contested — they have not collapsed into a single settled reading; what has collapsed is any credible path to choosing among interpretive authorities. Resistance is authored high (0.72) reflecting decades of active diplomatic and legal contestation by drafting states, the ICJ's own assertions of competence, and sustained advocacy from affected populations and neighboring states.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and veto-holding permanent members are authored as beneficiaries because the standing ambiguity in WHO decides converts directly into optionality for them: they can invoke whichever authority claim is momentarily convenient and are never bound to a single test. Drafting states occupy a hybrid position — they benefit from deference to their own authorial-intent claim but are institutionally constrained because they cannot force the Security Council to adopt that claim without unanimity they may not have. The ICJ is authored as agenda-setting but structurally excluded from enforcement — it can claim authority but cannot compel deference to it. Populations bearing the territorial and legal uncertainty (displaced persons, residents of administered territories, neighboring states) are authored as victims/payers: they have no standing in the authority contest and bear its accumulated costs indefinitely, mapping to high derived directionality toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing any Security Council resolution at all in November 1967 — was solved the moment the resolution passed unanimously; that emergency is dead. But the authority-to-interpret question was never a founding problem at all; it emerged afterward as a byproduct of the deliberate textual ambiguity, and its persistence now serves interests unrelated to the original 1967 diplomatic necessity. Classifying this as snare rather than treating it as ongoing legitimate legal process prevents mislabeling institutional paralysis as principled deference to multiple valid legal traditions: the coordination story (respecting multiple legitimate interpretive methods — textual, intentionalist, customary) is real as an abstract legal matter, but its persistence here specifically serves parties who benefit from non-resolution, which is the asymmetric-extraction signature that distinguishes a tangled/snare structure from a genuine, functioning pluralist interpretive framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_designation_counterfactual,
    'If the Security Council had, in 1967 or shortly after, explicitly designated the ICJ (or any single body) as the binding interpretive authority for Resolution 242, would the substantive withdrawal dispute have been resolved by now, or would it merely have relocated to a dispute over that body''s specific ruling?',
    'Comparative case study of other UNSC resolutions with textual ambiguity that DID have a designated interpretive authority (e.g., boundary commissions with binding arbitration clauses) versus this one, tracking time-to-resolution.',
    'If designated-authority cases resolve markedly faster, it strongly supports the claim that the authority ambiguity itself (not merely the underlying substantive dispute) is doing extractive work; if comparable cases also remain unresolved for other reasons, the authority-ambiguity story is weaker and more of the persistence is attributable to the underlying political conflict rather than this specific structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_designation_counterfactual, empirical, 'Whether designating a binding interpretive authority would have accelerated substantive resolution.').

omega_variable(
    committer_reading_boundary,
    'This story treats the meta-level authority contest as analytically separable from the two substantive readings (maximal/partial withdrawal) it sits above. Is that separation stable, or does the authority contest simply collapse into a restatement of whichever substantive position a party already holds (i.e., states favor whichever authority happens to validate their preferred substantive reading)?',
    'Track whether any party has ever endorsed an interpretive-authority claim that produced a substantive outcome adverse to its own preferred reading — genuine separability requires observing at least one such instance across the dispute''s history.',
    'If no such instance exists, the ''interpretive authority'' dispute may be epiphenomenal on the substantive dispute rather than an independent structural constraint, which would argue for merging this reading back into the two substantive constraint stories rather than treating it as a third, freestanding kernel reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether the authority-contest reading is analytically independent of the substantive readings it purports to sit above.').

omega_variable(
    veto_holder_symmetry,
    'Do all five veto-holding permanent Security Council members benefit symmetrically from the standing authority ambiguity, or do some benefit while others would prefer resolution but are blocked by the others'' veto capacity?',
    'Diplomatic archive review and voting-record analysis on subsequent Security Council resolutions and draft resolutions addressing withdrawal-authority questions since 1967.',
    'If benefit is asymmetric among veto-holders, the beneficiary group in base_properties should be narrowed to specific permanent members rather than the full P5, changing the directionality computation for that stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_holder_symmetry, empirical, 'Whether veto-holder benefit from the authority ambiguity is uniform across the P5 or concentrated among a subset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 10, 0.3).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 20, 0.38).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 30, 0.46).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 40, 0.52).
narrative_ontology:measurement(unsc_tr_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 55, 0.58).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(unsc_be_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 55, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(unsc_su_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 55, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.1).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the unsc_242_withdrawal_clause kernel family, alongside maximal_withdrawal_reading and partial_withdrawal_reading. Where those two stories each author ε for a specific substantive claim about withdrawal scope (with distinct beneficiary/victim structures depending on which reading is adopted), this story authors ε for the second-order fact that no body is recognized as authoritative to choose between them. Its high extractiveness (0.81) reflects that the authority vacuum itself perpetuates both substantive ambiguities simultaneously, functioning as a force multiplier on whichever substantive reading a mobile-exit party finds convenient at a given moment. Changes in this constraint's classification (e.g., if a future binding arbitration mechanism were established) would be expected to propagate pressure toward resolution in both sibling stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
