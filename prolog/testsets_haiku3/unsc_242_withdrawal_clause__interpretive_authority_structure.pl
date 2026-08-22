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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC 242 Withdrawal Clause: Interpretive Authority Structure
 *   domain: international_law/treaty_interpretation/diplomacy
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for Israeli withdrawal from 'territories
 *   occupied' in the Six-Day War. The textual ambiguity is intentional: the
 *   English version uses the indefinite article ('from territories
 *   occupied'), while the French version uses the definite article ('des
 *   territoires occupés' — 'the territories occupied'). This difference
 *   permits two readings: mandatory withdrawal from all occupied territories
 *   (maximal) versus discretionary withdrawal from some territories
 *   (partial). The authority to resolve this ambiguity is itself contested.
 *   The ICJ claims judicial interpretation authority over the clause;
 *   drafting states (particularly UK and US) assert their authorial intent
 *   controls; the occupying state and its allies invoke customary practice
 *   and state sovereignty. This reading instantiates the meta-dispute over
 *   WHICH SEAT has authority to resolve the clause — not which substantive
 *   reading is correct, but who gets to decide. The constraint operates as a
 *   snare because the authority ambiguity prevents definitive legal closure:
 *   beneficiaries are parties whose interests are served by the lack of
 *   settlement (occupying state, non-withdrawal coalition); victims are
 *   parties seeking definitive legal determination (claimant states, the
 *   international legal system as a conflict-resolution apparatus). The
 *   extractiveness is high because the unresolved authority structure
 *   perpetuates the substantive ambiguity indefinitely, allowing territorial
 *   occupation to persist under the cover of 'legal ambiguity.'
 *
 * KEY AGENTS:
 *   - icj: claims judicial interpretation supremacy; institutional seat; seeks definitive, binding legal reading
 *   - drafting_state_coalition_non_withdrawal: UK, US, and allied states who authored the clause with intentional ambiguity; claim authorial intent authority; sovereign veto over reinterpretation
 *   - occupying_state: Israel (or any occupying party invoking the clause); claims customary practice authority; benefits from interpretive pluralism; constrained exit from the territory
 *   - claimant_states_seeking_closure: Arab states, Palestinian Authority, other parties demanding definitive withdrawal; victims of the unresolved authority structure; powerful nominal status but trapped by coalition veto
 *   - international_legal_system: the meta-agent (non_agent=true) of conflict resolution; vindicated by the definitional claim of ICJ authority but harmed by the persistent ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.76).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Withdrawal Clause: Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/treaty_interpretation/diplomacy").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '0fb709f1-723d-4e0e-b68a-3cad68298f6e').
narrative_ontology:cs_kernel_codification('0fb709f1-723d-4e0e-b68a-3cad68298f6e', fixed_text).
narrative_ontology:cs_authority_grounding('0fb709f1-723d-4e0e-b68a-3cad68298f6e', extraction).
narrative_ontology:cs_interpretation_layer_present('0fb709f1-723d-4e0e-b68a-3cad68298f6e').
narrative_ontology:cs_reading_relation('0fb709f1-723d-4e0e-b68a-3cad68298f6e', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('0fb709f1-723d-4e0e-b68a-3cad68298f6e', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('0fb709f1-723d-4e0e-b68a-3cad68298f6e', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('0fb709f1-723d-4e0e-b68a-3cad68298f6e', interpretive_authority_is_contested, conventional).
narrative_ontology:cs_axiom('0fb709f1-723d-4e0e-b68a-3cad68298f6e', secondary, authority_ambiguity_perpetuates_substantive_ambiguity).
narrative_ontology:cs_axiom_status(authority_ambiguity_perpetuates_substantive_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('0fb709f1-723d-4e0e-b68a-3cad68298f6e', authority_ambiguity_perpetuates_substantive_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('0fb709f1-723d-4e0e-b68a-3cad68298f6e', un_charter_judicial_review_framework).
narrative_ontology:cs_drift_state('0fb709f1-723d-4e0e-b68a-3cad68298f6e', contemporary_post_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0fb709f1-723d-4e0e-b68a-3cad68298f6e', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_state_coalition_non_withdrawal).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, claimant_states_seeking_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, icj).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, state_sovereignty_in_treaty_interpretation).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, interpretive_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims interpretive supremacy over international legal disputes, including UNSC 242's withdrawal clause. Benefits from recognition of judicial authority but is harmed by the same authority ambiguity that permits non-compliance. Sits in dual position: as judicial institution, it is a beneficiary of the authority claim; as observer of the constraint system, it recognizes the authority is contested and unenforceable.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, icj, observer).

% UK, US, and allied states who drafted UNSC 242 with intentional ambiguity to secure a ceasefire without committing to any single reading. Control the drafting narrative and invoke authorial intent as their preferred interpretive authority. Can exit by unilaterally reaffirming or reconsidering their intent; maintain veto power over Security Council actions seeking to clarify the resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_state_coalition_non_withdrawal, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the authority ambiguity by avoiding definitive legal obligation to withdraw while maintaining occupation under a legal cloud. Invokes customary practice and state sovereignty as alternative authority grounds. Faces domestic political pressure to maintain territory and international legal pressure to withdraw; the unresolved authority structure permits simultaneous compliance claims to both constituencies.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, payer).

% Arab states, Palestinian Authority, and other parties seeking definitive withdrawal and territorial restoration. Trapped by coalition veto (drafting states defend their authority reading), ICJ authority denial (their preferred interpreter is not recognized), and occupying state non-cooperation. Exit would require capitulation (accepting occupation) or escalation (military conflict, sustained legal challenge); neither is acceptable.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, claimant_states_seeking_closure, payer,
    organized, generational, trapped, regional).

% States without direct involvement in the dispute but with standing in the General Assembly and international legal forums. Would advocate for definitive ICJ interpretation or for General Assembly clarification of the resolution, but are structurally excluded from Security Council decision-making and lack individual veto power. Their input is solicited in advisory opinions but has no binding force.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, non_aligned_states, excluded,
    moderate, generational, constrained, global).

% The meta-agent of dispute resolution and rule-based international order. Bears the reputational and functional cost of the unresolved authority structure: each year the ambiguity persists, the credibility of international law as a settlement mechanism declines, and non-state and weak-state actors lose faith in the system's capacity to produce definitive rulings.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_system, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_state_coalition_non_withdrawal).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilize a ceasefire (1967 Six-Day War) without committing any party to a specific territorial endpoint, permitting all signatories to claim compatibility with their preferred outcome (maximal withdrawal for claimants, secure boundaries for occupying state, non-binding flexibility for drafting states).
% TRANSFER_FUNCTION: Transfers legal authority and interpretive power from the Security Council (gridlocked by Cold War vetoes) to disputant reinterpretation and, implicitly, to the occupying state (which can maintain territory by invoking authorial intent ambiguity and customary practice authority). Also transfers legitimacy from the international legal system to coalition state preference.
% ABSENT_VOICES: Non-aligned states excluded from drafting and from Security Council veto power; the Palestinian Authority and stateless populations whose territorial interests are at stake but who lack Security Council standing; international human rights bodies whose mandate includes territorial occupation assessment but who have no authority seat in the resolution's interpretation.
% DISAPPEARANCE_RATIONALE: If the authority ambiguity disappeared overnight (authority definitively allocated to ICJ, drafting states, or occupying state), the constraint would collapse into one of its sibling readings (maximal or partial withdrawal). Territorial arrangements would shift, the occupying state would face either binding withdrawal obligation or definitive permission to retain, and the legal status of the occupation would become determinate rather than perpetually contested. The entire Middle East territorial status would be forced to rearrange around the resolved meaning of 242.
% FOUNDING_PROBLEM: Secure a ceasefire and Israeli withdrawal from territories conquered in 1967 without dictating the scope or mechanism of withdrawal, allowing drafting states to support the resolution without committing their favored parties to maximalist or minimalist readings.
% FOUNDING_PROBLEM_CORROBORATION: Drafting states (UK, US) attest the founding problem was solved: the resolution secured a ceasefire and remains a foundation for peace negotiations. Claimant states attest the problem was deferred, not solved: the ambiguity replaced definitive obligation with perpetual dispute. International law scholars (non-aligned academics, ICJ bench members) attest the founding problem was structurally unsolvable given the Cold War Security Council gridlock: the ambiguity was the only text that could pass. NO corroboration exists from outside the benefiting parties that the authority ambiguity was a successful solution rather than a deferred failure.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.81 at interval end) is high because the authority ambiguity is itself extractive: it permits the occupying state to persist in occupation under a legal cloud, avoiding both definitive obligation to withdraw and definitive permission to stay. The ambiguity is maintained by coalition power (drafting states, occupying state), not by genuine legal uncertainty. Suppression (0.76) reflects institutional barriers to resolving the authority question: the Security Council is gridlocked by veto power; ICJ authority over UNSC resolutions is itself contested; state practice diverges (some follow maximal reading, others partial). Theater ratio (0.62 at interval end) rises over the interval because procedural and rhetorical energy increases — more litigations, more UN debates, more scholarly commentary — but the underlying authority structure stays frozen. This is theatrical activity in defense of the status quo, not functional dispute resolution. Accessibility collapse (0.58) is moderate-low because the readings remain accessible to legal actors (the textual ambiguity is documented, the competing authorities are named), but alternatives to accepting the ambiguity are collapsed by coalition power. Resistance (0.72) is high because claimant states and the international legal community actively resist the authority ambiguity, mounting legal challenges, scholarly critiques, and diplomatic pressure; but their resistance meets coalition suppression. The measurement series show extraction and theater rising over the 54-year interval (1967–2021), while suppression intensifies as institutional gridlock deepens. Divergence: From the ICJ's analytical seat, this is a governance failure (authority not properly allocated); from the occupying state's seat, this is a coordination arrangement (ambiguity provides cover for multiple readings, avoiding forced choice); from claimant states' seats, this is pure snare (trapped by coalition authority denial and stuck in occupation).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (claimant states) compute this as snare from every institutional angle: they face judicial blockage (ICJ has no enforcement), diplomatic blockage (drafting states veto), and sovereignty blockage (occupying state invokes customary practice). From the drafting state coalition seat, the constraint is a coordination solution to the problem of binding reluctant signatories to a text without forcing definitive scope on any party — it is rope-like arrangement serving mutual interests. From the occupying state seat, it is a beneficial ambiguity providing legal cover. From the analytical seat (ICJ, international law scholars), it is a governance failure where authority is misallocated, but not a snare — it is a broken institution, resolvable by authority clarification.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and drafting state coalition are structural beneficiaries (d near 1.0, full target inversion): they benefit from the lack of definitive closure, which permits occupation to persist. They are not targeted — they are the parties suppressing closure. Claimant states are structural victims (d near 1.0, full target): they bear the cost of unresolved authority and persistent occupation; their exit would require either capitulation (accepting the occupation as permanent) or escalation (military or sustained legal pressure), both costly. The ICJ is a beneficiary in principle (authority claim) but a victim in practice (authority is not respected, reducing its functional power). This divergence is the core perspectival gap: analytical seats (competition authorities, human rights bodies, NGOs) see a snare maintained by coalition veto; coalition seats see a workable ambiguity preventing forced positions; victim seats see pure extraction of territory and legal standing. Directionality overrides: The ICJ's analytical power (institutional but authority-contested) sits between beneficiary and victim — d override to 0.45 (slight beneficiary lean from authority claim, but contested). Drafting state coalition: institutional power with veto, high exit options (they can exit by reaffirming or reconsidering their authorial intent interpretation), so d override to 0.15 (strong beneficiary). Occupying state: powerful, mobile relative to the legal system (can ignore ICJ), trapped only by coalition politics, so d override to 0.20 (beneficiary but with some exposure to coalition pressure shifts).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967): secure a ceasefire and Israeli withdrawal from occupied territories without dictating scope (maximal withdrawal to favor Arab claimants versus secure boundaries principle to favor Israel). The founding problem status: contested — drafting states and occupying state claim the ambiguity was intentional and solved the problem; claimant states claim it merely deferred the problem without solving it. The constraint's mandate (extract from the text a definitive withdrawal obligation or permission) has become contested because the authority to interpret the text is itself contested. This is precisely mandatrophy: the authority structure that was supposed to execute the mandate (UN Security Council, then ICJ judicial review) has become gridlocked and unable to resolve even the meta-question (who decides?). The theater ratio rising to 0.62 indicates increasing procedural performance (UN debates, legal briefs, mediation sessions) with no functional change in the underlying constraint. The constraint persists not because the mandate is live (withdrawing vs. retaining territory is still contested), but because no seat has authority to enforce a resolution. The absence of a remitting mechanism (no party can exit by deferring to an authority) is the snare signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_contestation,
    'Which authoritative seat (ICJ judicial interpretation, drafting states'' authorial intent, occupying state''s customary practice reading) has legitimate interpretive supremacy over UNSC 242''s withdrawal clause?',
    'A binding decision by an authoritative interpreter (ICJ ruling, Security Council resolution clarifying the clause, or sustained state practice establishing new custom) would assign interpretive authority. Short of that, the question remains systematically open.',
    'If ICJ authority is established as supreme, the maximal_withdrawal_reading becomes binding and the occupying state loses its veto; if drafting state intent prevails, partial_withdrawal_reading becomes canonical; if occupying state''s reading prevails, interpretive pluralism is cemented. The persistence of this omega IS the extraction mechanism — authority ambiguity perpetuates substantive ambiguity and prevents closure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_contestation, conceptual, 'The meta-dispute over which seat has authority to resolve the textual ambiguity').

omega_variable(
    kernel_reading_coexistence,
    'Do the three readings (interpretive_authority_structure, maximal_withdrawal, partial_withdrawal) represent logically incompatible interpretations of the same clause, or structurally distinct constraint problems that happen to touch the same text?',
    'Decomposition analysis: if the three readings share identical ε referent (the same standing arrangement being assessed) and differ only in substantive scope conclusion, they coexist. If they instantiate different enforcement predicates or different beneficiary structures stemming from the authority question itself, they are structurally distinct constraints masquerading as interpretation variance.',
    'If coexisting interpretations, the constraint family is correctly modeled as three linked stories with reading_relations reflecting interdependence. If structurally distinct, the decomposition requires refactoring the kernel to separate the authority-meta-dispute (THIS reading) from the scope-dispute (the siblings), requiring new constraint IDs for each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether this reading describes a genuine kernel reading or a meta-level constraint distinct from the siblings').

omega_variable(
    suppression_mechanism_authority,
    'Is the measured suppression (0.76) a structural feature of legal ambiguity itself, or an institutionally maintained suppression of competing interpretations by the occupying state and its coalition allies?',
    'Counterfactual: if authority were delegated to ICJ with enforcement power, would resistance to the withdrawal clause drop precipitously (indicating structural suppression was institutional), or would resistance persist at similar levels (indicating ambiguity itself is the suppressive force)?',
    'If institutional suppression predominates, the constraint is a snare maintained by coalition power over interpretive authority. If ambiguity itself is suppressive (prevents exit by keeping all parties in legal limbo), the constraint is closer to an inert coordination failure than to active extraction. The distinction affects remediation: institutional suppression requires redistribution of authority; ambiguity suppression requires normative or legislative closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_authority, empirical, 'Whether suppression stems from institutional control or from unresolved legal ambiguity').

omega_variable(
    false_summit_authority_claim,
    'Is the ICJ''s claim to interpretive authority over UNSC 242 a genuine judicial function rooted in the UN Charter and customary international law, or is it a constructed institutional veto that benefits judicial supremacy advocates?',
    'Historical genealogy: did the ICJ''s interpretive authority predate the UNSC 242 dispute, or was the authority claim layered on afterward to justify judicial review of the clause? Who attests the authority from outside the beneficiary coalition?',
    'If the authority claim predates the dispute and is corroborated by non-interested parties (non-aligned states, prior treaty precedent), it is likely genuine authority. If the authority was asserted post-hoc and corroborated only by states with interest in judicial supremacy, the reading itself may be a false-summit mountain (natural judicial function) masking a snare (constructed authority to serve particular state interests).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_authority_claim, conceptual, 'Whether ICJ interpretive authority over this clause is a genuine institutional function or a strategic authority claim benefiting judicial supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 9, 0.42).
narrative_ontology:measurement_basis(unsc_tr_t9, observed).
narrative_ontology:measurement(unsc_tr_t18, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(unsc_tr_t18, observed).
narrative_ontology:measurement(unsc_tr_t27, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 27, 0.54).
narrative_ontology:measurement_basis(unsc_tr_t27, observed).
narrative_ontology:measurement(unsc_tr_t36, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 36, 0.59).
narrative_ontology:measurement_basis(unsc_tr_t36, observed).
narrative_ontology:measurement(unsc_tr_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 45, 0.61).
narrative_ontology:measurement_basis(unsc_tr_t45, observed).
narrative_ontology:measurement(unsc_tr_t54, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 54, 0.62).
narrative_ontology:measurement_basis(unsc_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 9, 0.68).
narrative_ontology:measurement_basis(unsc_be_t9, observed).
narrative_ontology:measurement(unsc_be_t18, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 18, 0.74).
narrative_ontology:measurement_basis(unsc_be_t18, observed).
narrative_ontology:measurement(unsc_be_t27, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 27, 0.78).
narrative_ontology:measurement_basis(unsc_be_t27, observed).
narrative_ontology:measurement(unsc_be_t36, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 36, 0.79).
narrative_ontology:measurement_basis(unsc_be_t36, observed).
narrative_ontology:measurement(unsc_be_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 45, 0.8).
narrative_ontology:measurement_basis(unsc_be_t45, observed).
narrative_ontology:measurement(unsc_be_t54, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 54, 0.81).
narrative_ontology:measurement_basis(unsc_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 9, 0.63).
narrative_ontology:measurement_basis(unsc_su_t9, observed).
narrative_ontology:measurement(unsc_su_t18, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 18, 0.68).
narrative_ontology:measurement_basis(unsc_su_t18, observed).
narrative_ontology:measurement(unsc_su_t27, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 27, 0.72).
narrative_ontology:measurement_basis(unsc_su_t27, observed).
narrative_ontology:measurement(unsc_su_t36, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 36, 0.74).
narrative_ontology:measurement_basis(unsc_su_t36, observed).
narrative_ontology:measurement(unsc_su_t45, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 45, 0.75).
narrative_ontology:measurement_basis(unsc_su_t45, observed).
narrative_ontology:measurement(unsc_su_t54, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 54, 0.76).
narrative_ontology:measurement_basis(unsc_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.18).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause constraint family consists of three structurally distinct constraints: (1) interpretive_authority_structure (THIS reading) — the meta-dispute over which seat has authority to resolve the ambiguity; (2) maximal_withdrawal_reading — the substantive constraint from the perspective of mandatory-withdrawal interpretation; (3) partial_withdrawal_reading — the substantive constraint from the perspective of discretionary-withdrawal interpretation. Each reading is a separate ε-invariant constraint because each assesses a different standing arrangement from a different interpretive authority seat. The authority structure reading (this one) is upstream: it creates the conditions allowing both substantive readings to coexist. Authority ambiguity → both readings live → both interpretations can be maintained → both substantive territorial arrangements can persist → no definitive legal closure. The family's extractiveness emerges from this meta-level authority contestation, not from any single reading's content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.45).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
