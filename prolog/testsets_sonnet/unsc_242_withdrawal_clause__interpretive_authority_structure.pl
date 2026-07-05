% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Contested Authority to Interpret UNSC 242's Withdrawal Clause
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   This story isolates one specific claim within the UNSC Resolution 242
 *   kernel: not what the withdrawal clause substantively means (that is the
 *   maximal_withdrawal_reading and partial_withdrawal_reading siblings), but
 *   WHO has the authority to say what it means. Three institutional actors
 *   each assert a different interpretive method as controlling — the ICJ
 *   asserts judicial/textual interpretation under treaty-law norms, the
 *   drafting states assert authorial intent recoverable from negotiating
 *   history, and the occupying state asserts customary practice established
 *   through decades of administration. Because no single body can compel the
 *   others to defer, the meta-dispute itself becomes a structure that
 *   perpetuates the underlying substantive ambiguity indefinitely. This is
 *   analytically distinct from either substantive reading: even if one could
 *   stipulate what the 'correct' interpretation would be under any single
 *   method, the persistence of the unresolved authority question means no
 *   method is ever authoritatively applied. ε is high here specifically
 *   because the authority-ambiguity constraint operates independently of and
 *   prior to the substantive dispute — it is what keeps both substantive
 *   readings permanently live rather than settling either.
 *
 * KEY AGENTS:
 *   - occupying_state_government: primary beneficiary of indefinite deferral, controls facts on the ground
 *   - drafting_states_bloc: controls archival evidence for authorial-intent claims, benefits from discretion over disclosure
 *   - unsc_permanent_members_with_veto: benefit from unresolved leverage, block referral to binding adjudication
 *   - icj_and_international_legal_bodies: asserts genuine interpretive authority but lacks compulsory jurisdiction
 *   - displaced_populations_seeking_legal_closure: bear the accumulated cost with no standing in any forum
 *   - neighboring_states_awaiting_boundary_settlement: need closure but cannot compel it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "Contested Authority to Interpret UNSC 242's Withdrawal Clause").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '119cda07-5fac-4521-b0ab-ab3b6b82c910').
narrative_ontology:cs_kernel_codification('119cda07-5fac-4521-b0ab-ab3b6b82c910', distributed).
narrative_ontology:cs_authority_grounding('119cda07-5fac-4521-b0ab-ab3b6b82c910', distributed).
narrative_ontology:cs_reading_relation('119cda07-5fac-4521-b0ab-ab3b6b82c910', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('119cda07-5fac-4521-b0ab-ab3b6b82c910', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('119cda07-5fac-4521-b0ab-ab3b6b82c910', foundational, interpretive_authority_must_be_designated_before_substance_can_bind).
narrative_ontology:cs_axiom_status(interpretive_authority_must_be_designated_before_substance_can_bind, holdable).
narrative_ontology:cs_axiom_grounding('119cda07-5fac-4521-b0ab-ab3b6b82c910', interpretive_authority_must_be_designated_before_substance_can_bind, conventional).
narrative_ontology:cs_axiom('119cda07-5fac-4521-b0ab-ab3b6b82c910', foundational, unresolved_authority_is_itself_a_governing_structure_not_a_null_state).
narrative_ontology:cs_axiom_status(unresolved_authority_is_itself_a_governing_structure_not_a_null_state, holdable).
narrative_ontology:cs_axiom_grounding('119cda07-5fac-4521-b0ab-ab3b6b82c910', unresolved_authority_is_itself_a_governing_structure_not_a_null_state, empirically_contingent).
narrative_ontology:cs_reference_frame('119cda07-5fac-4521-b0ab-ab3b6b82c910', vienna_convention_treaty_interpretation_default).
narrative_ontology:cs_drift_state('119cda07-5fac-4521-b0ab-ab3b6b82c910', post_cold_war_multilateral_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('119cda07-5fac-4521-b0ab-ab3b6b82c910', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_permanent_members_with_veto).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_awaiting_boundary_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the disputed territories and invokes customary practice — decades of continuous administration, security arrangements, and bilateral negotiation precedent — as the authoritative interpretive method. Because no body can compel a single reading, the occupying state's facts-on-the-ground continue to accumulate and become their own evidentiary claim to authority. Exit from the dispute costs it nothing; time itself is an asset.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government, agenda_setter).

% The states that negotiated the resolution's language (principally the US and UK) assert that authorial intent — recoverable from drafting history, negotiating cables, and the deliberate choice of the indefinite article — is the correct interpretive method. They benefit from this claim because their own diplomatic archives and testimony become the controlling evidence, and they retain discretion over how much of that record to disclose or endorse.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_bloc, beneficiary).

% Hold veto power over any Security Council resolution that would authoritatively settle the interpretive question or refer it for binding adjudication. Because settling the authority question would foreclose their ongoing leverage over the underlying territorial dispute, they have structural incentive to let the meta-dispute persist rather than force a vote that could bind them.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_permanent_members_with_veto, beneficiary,
    institutional, civilizational, arbitrage, global).

% Claims judicial interpretation — textual analysis under the Vienna Convention on the Law of Treaties framework — as the authoritative method, and has issued advisory opinions bearing on related questions. But it has no compulsory jurisdiction here absent state consent, and no party with the power to force a referral has an interest in doing so. Its interpretive claim is real but structurally unenforceable.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj_and_international_legal_bodies, excluded,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, icj_and_international_legal_bodies, observer).

% Have no standing in any of the three competing interpretive fora and bear the accumulated human cost of the unresolved territorial status — displacement, statelessness, blocked return, uncertain property rights. Every year the authority question remains open is a year their situation is not adjudicated by anyone with power to enforce a remedy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations_seeking_legal_closure, payer,
    powerless, generational, trapped, local).

% Have signed or sought agreements premised on eventual territorial settlement but cannot obtain definitive boundary resolution because the underlying withdrawal scope question is never authoritatively closed. They can pursue bilateral tracks around the impasse but cannot compel a ruling that would settle it for all parties at once.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, neighboring_states_awaiting_boundary_settlement, payer,
    moderate, generational, constrained, regional).

% Document and analyze the three competing interpretive-authority claims without power to adjudicate between them. Their scholarship establishes that the meta-dispute is not an oversight but a durable structural feature of how the resolution was drafted and has been maintained.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_government).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, settling WHO has authority to interpret the treaty text would let any single interpretive method (judicial, authorial-intent, or customary-practice) resolve the underlying withdrawal-scope dispute once and for all, coordinating expectations for all parties around a single binding reading.
% TRANSFER_FUNCTION: The unresolved authority question transfers the cost of ambiguity from the parties who can tolerate open-endedness (the occupying state, the veto-holding powers, the drafting states with archival discretion) to the parties who cannot (displaced populations, neighboring states needing settled boundaries). No resource literally moves, but negotiating leverage, delay-as-strategy, and the option value of an unresolved status all accrue to the powerful seats while foreclosure costs accrue to the powerless ones.
% ABSENT_VOICES: Displaced populations and civil-society legal advocates have no seat in any of the three competing interpretive fora — not before the ICJ (no standing), not in bilateral drafting-history disputes (not party to the original negotiation), and not before the Security Council (no vote). They would argue for a fourth authority claim: that unresolved suffering itself creates urgency obligations that override procedural deadlock, but this claim has no institutional home.
% DISAPPEARANCE_RATIONALE: If the meta-dispute over interpretive authority were resolved tomorrow — if all parties accepted a single body's jurisdiction to construe the withdrawal clause — the underlying substantive dispute would still need adjudication, but it would proceed on a track with an endpoint. Diplomatic strategy for at least one bloc would lose its principal asset (indefinite delay), and negotiations premised on ambiguity as leverage would have to shift to negotiations over substance.
% FOUNDING_PROBLEM: The 1967 drafters faced a genuine problem: reconciling two states' incompatible negotiating positions (immediate, unconditional withdrawal vs. conditional withdrawal tied to secure and recognized boundaries) required language ambiguous enough that both blocs could vote for the same resolution.
% FOUNDING_PROBLEM_CORROBORATION: Declassified negotiating records and multiple participants in the 1967 drafting sessions (British and American diplomats among them) have publicly acknowledged the ambiguity was intentional — a deliberate drafting compromise, not an oversight. This is corroborated by historians and legal scholars outside any of the interested state parties, though the drafting states themselves now selectively invoke or downplay that same record depending on litigation posture.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high and rising (0.42 to 0.81 across the interval) because the cost of unresolved authority compounds over time — each year of non-adjudication adds administrative facts, displacement, and diplomatic sunk cost that make eventual resolution harder and more contested, not less. Suppression (0.72) reflects that alternatives to the current non-adjudicated state are actively foreclosed: no party with power to force a binding referral has an interest in doing so, and dissenting analysis (from scholars, from the excluded ICJ) has no enforcement mechanism. Theater ratio is substantial and rising (0.58) because a great deal of diplomatic and quasi-judicial activity — repeated Security Council statements, advisory proceedings, bilateral 'peace process' negotiation rounds — performs the appearance of progress toward resolving the authority question while the underlying deadlock over WHO decides remains untouched.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state's seat, the customary-practice claim looks like a legitimate, evidence-based interpretive method consistent with how international law actually develops through state practice. From the displaced-populations seat, the same claim looks like a legal fiction cover for indefinite non-resolution. The engine should compute these as structurally different experiences of the identical authority-ambiguity structure, not as competing opinions about the same experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and the veto-holding powers sit near the full-beneficiary end: they lose nothing and gain strategic flexibility from the authority question staying open, and their exit options (arbitrage — they can walk away from any given negotiating track without consequence) reinforce this. The drafting states occupy an intermediate position — they benefit from discretion over their own archives but are institutionally implicated in having created the ambiguity, giving them a constrained rather than fully arbitrage exit. Displaced populations and neighboring states sit at the full-target end: trapped or constrained exit options, generational time horizons that mean the cost compounds across their lifetimes, and zero standing in any of the three competing interpretive fora.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling two incompatible 1967 negotiating positions through deliberately ambiguous language — was a genuine, temporary diplomatic necessity. But the authority-to-interpret question was never given its own resolution mechanism precisely because doing so would have forced the substantive question at the time the resolution was passed. Fifty-plus years later, the original problem (getting a resolution passed at all) is dead, but the interpretive-authority vacuum it required persists and has become an independently load-bearing structure that several parties now actively benefit from maintaining. This is the mandatrophy signature: an arrangement whose founding justification has expired while its operative form — and the benefits flowing through that form — continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure,
    'This story is one reading (interpretive_authority_structure) of the unsc_242_withdrawal_clause kernel; the maximal_withdrawal_reading and partial_withdrawal_reading are sibling constraints instantiating the substantive dispute this authority-vacuum keeps open. Which sibling reading eventually prevails, if any, depends on which authority claim (judicial, authorial-intent, customary-practice) is eventually accepted as controlling — but this story treats that acceptance as unresolved and structurally likely to remain unresolved.',
    'A binding referral to the ICJ with universal state consent to its jurisdiction over this specific question, or a Security Council resolution explicitly designating an interpretive authority, would resolve which reading controls. Neither has occurred in fifty-plus years, which is itself evidence for this story''s high suppression/extraction values.',
    'If the authority question were resolved in favor of judicial interpretation, the maximal_withdrawal_reading (textual/treaty-law analysis favoring the definite-article construction) would likely gain force. If resolved in favor of customary practice, the partial_withdrawal_reading would likely be vindicated retroactively. This story does not adjudicate that outcome; it documents that the absence of resolution is itself the extractive structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'This constraint is one reading of a contested kernel; sibling readings depend on how this authority-vacuum is eventually resolved, if ever.').

omega_variable(
    genuine_vs_strategic_ambiguity,
    'Was the absence of a designated interpretive authority a genuine oversight in 1967 drafting (no one anticipated fifty years of non-resolution) or a foreseeable and strategically useful feature that at least some drafters intended?',
    'Close historical analysis of drafting-session records and private correspondence among the resolution''s principal authors, cross-checked against contemporaneous statements about anticipated timelines for resolution.',
    'If foreseeable and intended, the interpretive-authority vacuum should be read as tangled_rope-adjacent from inception (coordination function of passing the resolution, combined with knowing extractive deferral) rather than as a snare that emerged only later through opportunistic exploitation of an accidental gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_strategic_ambiguity, empirical, 'Whether the interpretive-authority gap was foreseeable strategic design or genuine unanticipated oversight.').

omega_variable(
    icj_authority_enforceability_ceiling,
    'Does the ICJ''s claimed judicial-interpretation authority carry any real weight in this dispute, or is it purely aspirational given the absence of compulsory jurisdiction and enforcement power?',
    'Track whether any ICJ advisory opinion touching on the underlying territorial dispute produces observable behavioral change in the parties, versus being cited then ignored.',
    'If ICJ opinions produce zero behavioral change even when parties formally acknowledge them, the ICJ''s interpretive-authority claim should be weighted near zero in any composite assessment of which authority actually governs outcomes — strengthening the case that customary practice (backed by occupation) is the de facto controlling method regardless of its formal legal standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icj_authority_enforceability_ceiling, empirical, 'Whether the ICJ''s interpretive claim has any operative force given its lack of compulsory jurisdiction here.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 10, 0.32).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 20, 0.4).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 30, 0.47).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 40, 0.53).
narrative_ontology:measurement(unsc_tr_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 55, 0.58).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(unsc_be_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 55, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(unsc_su_t55, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 55, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint is the interpretive-authority-structure member of the unsc_242_withdrawal_clause kernel family. It is upstream of and structurally enables both substantive-reading siblings (maximal_withdrawal_reading, partial_withdrawal_reading) to remain simultaneously live: because no body has the compulsory authority to select between the ICJ's judicial-interpretation claim, the drafting states' authorial-intent claim, or the occupying state's customary-practice claim, neither substantive reading is ever authoritatively displaced. Where the substantive-reading stories measure ε with respect to the territorial and legal consequences of each reading, this story measures ε with respect to the cost of the meta-dispute itself remaining unresolved — a distinct and higher-extraction constraint occupying a different structural position in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
