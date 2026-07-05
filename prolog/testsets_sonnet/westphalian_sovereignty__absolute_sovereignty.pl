% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty / Non-Interference Doctrine
 *   domain: international law / political philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolute-sovereignty reading of the
 *   Westphalian kernel: sovereignty as unconditional, categorical protection
 *   of domestic affairs from external interference, codified in the strong
 *   reading of UN Charter Article 2(7). It is a single, ε-stable constraint
 *   distinct from its two siblings — the conditional-sovereignty reading
 *   (Responsibility to Protect) and the graduated-sovereignty reading
 *   (capacity/legitimacy-indexed spectrum) — which are separate constraint
 *   stories, not alternative measurements of this one. Under this reading,
 *   the doctrine functions as genuine interstate coordination (preventing
 *   invasion and forced regime change) that has, since 1945, increasingly
 *   doubled as a shield authoritarian leadership and veto-wielding powers use
 *   to block accountability for internal atrocities.
 *
 * KEY AGENTS:
 *   - authoritarian_state_leadership: primary beneficiary/agenda_setter (institutional/arbitrage) — invokes the norm to block external scrutiny of internal repression
 *   - un_security_council_permanent_members: enforcement gatekeepers (institutional/arbitrage) — apply the doctrine selectively via veto
 *   - domestic_populations_under_repression: primary target (powerless/trapped) — bears the cost of foreclosed intervention
 *   - ethnic_and_religious_minorities_within_states: concentrated target (powerless/trapped) — treatment framed as internal affair regardless of severity
 *   - weaker_states_seeking_protection: genuine coordination beneficiary (moderate/constrained) — uses the same doctrine defensively against great-power domination
 *   - international_law_scholars: analytical observer (analytical/analytical) — traces doctrine's asymmetric application across history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty / Non-Interference Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international law / political philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'd43c3e70-b13f-46f9-b302-1bbf0c4e6505').
narrative_ontology:cs_kernel_codification('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', formalized).
narrative_ontology:cs_authority_grounding('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', distributed).
narrative_ontology:cs_reading_relation('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', foundational, internal_conduct_categorically_immune_from_external_judgment).
narrative_ontology:cs_axiom_status(internal_conduct_categorically_immune_from_external_judgment, holdable).
narrative_ontology:cs_axiom_grounding('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', internal_conduct_categorically_immune_from_external_judgment, conventional).
narrative_ontology:cs_axiom('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', secondary, state_equality_requires_uniform_non_interference_regardless_of_conduct).
narrative_ontology:cs_axiom_status(state_equality_requires_uniform_non_interference_regardless_of_conduct, holdable).
narrative_ontology:cs_axiom_grounding('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', state_equality_requires_uniform_non_interference_regardless_of_conduct, conventional).
narrative_ontology:cs_reference_frame('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', peace_of_westphalia_princely_non_interference).
narrative_ontology:cs_drift_state('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', post_un_charter_human_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d43c3e70-b13f-46f9-b302-1bbf0c4e6505', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_leadership).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities_within_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, weaker_states_seeking_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the non-interference norm at the UN and in bilateral relations to shield internal governance — including suppression of dissent, ethnic policy, and electoral practice — from external scrutiny or sanction. Actively lobbies for the norm's expansion and cites it reciprocally to protect peer states, trading votes and diplomatic cover in exchange.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_leadership, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_leadership, agenda_setter).

% Control the enforcement machinery (veto power, Security Council referral) that decides when sovereignty claims are honored or overridden. Apply the doctrine selectively — invoking it to block intervention against allies or their own conduct while permitting exceptions against adversaries — which sustains the doctrine's coercive force without binding its enforcers.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members, beneficiary).

% Live under governance that the sovereignty norm insulates from external accountability. Have no standing in the international system that created the norm; the doctrine that would otherwise let allies or institutions intervene on their behalf is precisely what forecloses that intervention. Exit means clandestine emigration or armed resistance, both high-risk.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, biographical, trapped, national).

% Bear concentrated costs when a state's 'domestic affairs' include policy targeting their group. The categorical framing of non-interference treats their treatment as internal and therefore off-limits to external redress, regardless of severity.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities_within_states, payer,
    powerless, biographical, trapped, national).

% Also invoke sovereignty defensively against great-power pressure, sanctions, or forced regime change — for these states the doctrine functions as a genuine shield against external domination, not merely as cover for internal repression. Their benefit is real and distinct from the authoritarian-capture case.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, weaker_states_seeking_protection, beneficiary,
    moderate, generational, constrained, national).

% Document violations and issue findings but have no enforcement authority that can override a sovereignty claim absent Security Council action, which permanent members can veto. Their voice is structurally present in reports but absent from the decision that actually binds action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_bodies, excluded,
    organized, generational, constrained, global).

% Analyze the doctrine's history from the 1648 Peace of Westphalia through the UN Charter's Article 2(4)/2(7) framework, tracing how the absolute reading has been invoked asymmetrically and how it interacts with the competing conditional and graduated readings.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a return to unrestricted great-power intervention in each other's internal affairs; gives every recognized state — regardless of size — a formal claim against invasion or forced restructuring by more powerful neighbors, which was a genuine achievement relative to pre-Westphalian imperial and religious-war dynamics.
% TRANSFER_FUNCTION: Moves accountability away from populations subject to a state's internal conduct and toward the state apparatus itself; moves enforcement discretion to whichever powers hold Security Council veto authority, who apply the norm asymmetrically to their own advantage.
% ABSENT_VOICES: Domestic populations under repression and persecuted minorities have no seat in the international system whose foundational norm determines whether their treatment is even a legitimate subject of external concern. Human rights bodies can document but not act without permanent-member consent.
% DISAPPEARANCE_RATIONALE: If absolute non-interference vanished overnight, the coordination benefit weaker states currently draw from it (protection against great-power invasion) would also vanish alongside the shield authoritarian regimes use against accountability — the international system would need some other mechanism (likely one of the sibling readings) to perform the protective function without the accountability-blocking function; this is precisely the contest the kernel's three readings represent.
% FOUNDING_PROBLEM: Post-1648 Europe needed a principle to stop wars fought over which prince could dictate another prince's internal religious and political order; sovereign equality and non-interference ended a specific cycle of interconfessional and dynastic warfare.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Peace of Westphalia and mainstream international-relations scholarship attest the original problem (interstate war over internal religious/political order) is largely resolved among major powers. Human rights scholars, UN Special Rapporteurs, and comparative genocide-prevention research — sources outside the beneficiary set of authoritarian leadership and veto-holding powers — attest that the doctrine, as currently invoked in its absolute form, now primarily functions to block accountability for internal atrocities rather than to prevent interstate war, which is a different problem than the one it was built to solve.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.52, mid-high, reflecting a genuine coordination core (interstate non-aggression) substantially compromised by asymmetric shielding of internal repression — this is a tangled_rope profile, not pure extraction, because weaker_states_seeking_protection derive real, non-illusory benefit from the same norm that shields authoritarian leadership. Suppression (0.71) is high and rising across the interval because the doctrine's enforcement depends on active veto exercise and diplomatic reciprocity among powerful states, not on the norm being self-evidently correct. Theater ratio (0.40) reflects the growing gap between the doctrine's stated justification (protecting weaker states from domination) and its increasingly common invocation as cover for internal conduct that has nothing to do with interstate aggression.
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian leadership seat, the doctrine reads as legitimate coordination — respect for equal sovereignty, no different in kind from any state's claim to non-interference. From the domestic population seat, the identical doctrine operates as the mechanism that specifically forecloses the intervention that could stop ongoing harm. The UNSC permanent member seat experiences the doctrine as a discretionary tool: binding when applied to rivals, elastic when applied to allies or to themselves. This is exactly the seat divergence the tangled_rope classification is built to hold without collapsing into either 'sovereignty is pure coordination' or 'sovereignty is pure extraction.'
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations split into two structurally distinct groups: authoritarian_state_leadership and UNSC permanent members derive strategic, discretionary benefit (low d, near-full beneficiary) because they control both the invocation and the enforcement of the norm. weaker_states_seeking_protection derive a different, less discretionary benefit — genuine protection against domination — that the derivation chain should not conflate with the authoritarian-capture beneficiary case; this justifies treating them as a separate stakeholder with moderate power rather than folding them into the same beneficiary bucket. Victims (domestic populations, minorities) sit at powerless/trapped, driving d toward the full-target end — they cannot exit the jurisdiction whose internal conduct the doctrine insulates, and they have no standing in the system that adjudicates the norm's application.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing interstate war over internal religious/political order among European powers) is substantially resolved among major powers today — but the doctrine has not sunset; it has been redirected toward a different function (blocking accountability for internal atrocities) that its original architects were not solving for. This is the mandatrophy signature: mandate outlived its founding function and persists via institutional inertia (veto structure) plus active interest (authoritarian capture), not because the original coordination problem remains unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is absolute sovereignty the historically dominant reading of the Westphalian kernel, or has state practice since 1945 (UN Charter, humanitarian intervention precedents, R2P adoption in 2005) already shifted the operative international-law reading toward conditional or graduated sovereignty, making the absolute reading increasingly a rhetorical fallback rather than the governing norm?',
    'Systematic review of Security Council resolution language, ICJ jurisprudence, and state practice (opinio juris) across the interval to determine which reading actually governs outcomes versus which is merely invoked.',
    'If state practice has substantively shifted toward conditional/graduated readings, this story''s high extractiveness score describes a doctrine in decline rather than a stable equilibrium — the temporal measurements would need to show extractiveness peaking mid-interval and declining, not monotonically rising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether absolute sovereignty remains the operative reading or is being displaced by sibling readings in actual practice.').

omega_variable(
    coordination_extraction_inseparability,
    'Can the interstate-non-aggression coordination function of absolute sovereignty be structurally separated from its use as a shield against internal-accountability claims, or are these two functions inherent to the same categorical logic (any exception carved out for internal accountability necessarily creates an exception exploitable for external aggression justification)?',
    'Comparative analysis of conditional_sovereignty (R2P) implementation: does empowering external intervention for human rights violations demonstrably increase pretextual invasions, or can the two be legally and practically distinguished?',
    'If inseparable, the high extractiveness measured here is an irreducible cost of maintaining any non-aggression coordination and cannot be reduced without sacrificing the coordination function itself. If separable, the extraction is a contingent, correctable feature rather than a structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether the doctrine''s coordination and extraction functions are structurally fused or contingently linked.').

omega_variable(
    veto_asymmetry_naturalization,
    'Is the asymmetric application of the absolute-sovereignty norm by UNSC permanent members a contingent feature of current institutional design (the veto), or an inherent feature of any enforcement mechanism for a categorical non-interference norm (someone must have final say, and final say will always be exercised self-interestedly)?',
    'Comparative study of enforcement mechanisms in other categorical-norm regimes (e.g., nuclear non-proliferation, genocide convention) to see whether asymmetric enforcement is a general feature of hard-law regimes lacking supranational adjudication.',
    'If asymmetric enforcement is inherent to any enforcement design for categorical norms, the extraction measured here is not specific to sovereignty doctrine but a general feature of international law absent a supranational enforcer — this would affect how the omega interacts with the tangled_rope classification''s active-enforcement requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_asymmetry_naturalization, empirical, 'Whether veto-based asymmetric enforcement is contingent institutional design or structurally inevitable for categorical international norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1960, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1960, 0.24).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(west_be_t1960, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1960, 0.36).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(west_su_t1960, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalian_sovereignty kernel. absolute_sovereignty (this story) treats domestic authority as categorical and unconditional, classified tangled_rope with ε≈0.52 favoring authoritarian capture of the non-interference shield. conditional_sovereignty treats sovereignty as conditioned on rights performance, triggering legitimate intervention on systematic violation — a structurally distinct constraint with a different beneficiary/victim map (intervening powers and protected populations vs. targeted regimes) and a different, likely lower or differently-shaped ε given its own capture risks (pretextual intervention). graduated_sovereignty treats sovereignty as a capacity/legitimacy-indexed spectrum, functioning as a background scaling variable compatible in principle with either categorical stance. Each reading is authored as its own constraint with its own stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
