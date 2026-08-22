% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: The Rhetorical Contraction of War-Winnability (Public Taboo vs. Classified Planning)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story authors the 'rhetorical_contraction' reading of the
 *   war-winnability kernel: the claim that declaratory discourse and
 *   classified planning diverged rather than converged after 1945. On this
 *   reading, 'nuclear war cannot be won' became a near-universal public and
 *   diplomatic axiom precisely while employment guidance documents (from the
 *   1950s counterforce debates through PD-59, NSDD-13, and their successors)
 *   continued to specify limited options, damage-limitation postures, and
 *   war-termination objectives. The taboo did real coordination work —
 *   stabilizing declaratory rhetoric, easing arms control, reassuring
 *   adversaries — while the operational planning apparatus retained
 *   flexibility never subjected to that same public constraint. This is a
 *   distinct claim from the deterrence_unthinkable reading (which holds the
 *   elimination is real and total) and from the countervailing_thinkable
 *   reading (which holds limited victory is openly, not just secretly,
 *   embraced). Only this reading's ε concerns the GAP between the two layers;
 *   the sibling readings are separate constraints with their own ε values,
 *   linked here for network traceability only.
 *
 * KEY AGENTS:
 *   - strategic_planning_establishment: Primary beneficiary (institutional/arbitrage) — retains classified operational flexibility
 *   - nuclear_weapons_laboratories: Beneficiary (institutional/arbitrage) — capability development shielded from taboo
 *   - national_security_executive: Agenda-setter and beneficiary (institutional/arbitrage) — controls both declaratory and classified layers
 *   - legislative_oversight_committees: Primary payer (organized/constrained) — oversight hollowed by classification
 *   - general_public: Payer (powerless/trapped) — bears risk without visibility
 *   - arms_control_advocacy_community: Payer/excluded (organized/constrained) — advocacy built on a premise the classified record complicates
 *   - strategic_studies_scholars: Analytical observer — reconstructs the gap from declassified fragments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "The Rhetorical Contraction of War-Winnability (Public Taboo vs. Classified Planning)").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '947f0a88-5e90-4508-b741-7f0377bed87e').
narrative_ontology:cs_kernel_codification('947f0a88-5e90-4508-b741-7f0377bed87e', distributed).
narrative_ontology:cs_authority_grounding('947f0a88-5e90-4508-b741-7f0377bed87e', extraction).
narrative_ontology:cs_interpretation_layer_present('947f0a88-5e90-4508-b741-7f0377bed87e').
narrative_ontology:cs_reading_relation('947f0a88-5e90-4508-b741-7f0377bed87e', war_winnability_post_1945__deterrence_unthinkable, influences).
narrative_ontology:cs_reading_relation('947f0a88-5e90-4508-b741-7f0377bed87e', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('947f0a88-5e90-4508-b741-7f0377bed87e', foundational, declaratory_operational_divergence_is_structural).
narrative_ontology:cs_axiom_status(declaratory_operational_divergence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('947f0a88-5e90-4508-b741-7f0377bed87e', declaratory_operational_divergence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('947f0a88-5e90-4508-b741-7f0377bed87e', foundational, classification_regime_constitutes_the_taboo).
narrative_ontology:cs_axiom_status(classification_regime_constitutes_the_taboo, holdable).
narrative_ontology:cs_axiom_grounding('947f0a88-5e90-4508-b741-7f0377bed87e', classification_regime_constitutes_the_taboo, conventional).
narrative_ontology:cs_reference_frame('947f0a88-5e90-4508-b741-7f0377bed87e', mutual_vulnerability_declaratory_consensus).
narrative_ontology:cs_drift_state('947f0a88-5e90-4508-b741-7f0377bed87e', post_cold_war_declassification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('947f0a88-5e90-4508-b741-7f0377bed87e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, national_security_executive).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, general_public).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, civilian_control_of_nuclear_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and maintains classified nuclear employment plans (SIOP and successors) that include counterforce options, damage-limitation strategies, and escalation-management sequences premised on some form of survivable, war-terminating outcome. Operates inside classification boundaries that exempt this work from the public taboo governing what may be said about nuclear war. Gains the ability to plan for contingencies without having to defend that planning in open political debate.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, beneficiary).

% Design and refine weapons systems (accuracy improvements, yield options, delivery diversification) whose primary rationale is counterforce and damage-limitation utility — capabilities that only make sense if some notion of a fought-and-survived war persists operationally. Budget justifications are framed in deterrence-stability language even where the underlying requirement is warfighting capability. Insulated from the rhetorical taboo by classification and by technical framing that avoids the word 'winnable.'
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories, beneficiary,
    institutional, generational, arbitrage, national).

% The presidency and its national security apparatus retain sole classified authority over nuclear employment guidance. Publicly reiterates that nuclear war 'cannot be won and must never be fought' while approving guidance documents (e.g., successive Nuclear Weapons Employment Policy revisions) that specify options short of full exchange, phased use, and post-strike objectives. Benefits from never having to reconcile the public and classified postures in the same forum.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, national_security_executive, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, national_security_executive, beneficiary).

% Members with security clearances receive compartmented briefings on employment planning but are bound by classification from discussing specifics with colleagues, staff, or constituents. Cannot legislate against or publicly debate plans they are permitted to see only in restricted form. Their oversight function is structurally reduced to acknowledging receipt of information they cannot act on in the open chamber.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees, payer,
    organized, biographical, constrained, national).

% Absorbs the political consensus that nuclear war-winning talk is taboo, forms views on deterrence policy based on that public consensus, and votes accordingly — without access to the classified planning that actually governs what would happen in a crisis. Bears the ultimate risk of any employment decision made under plans it has no visibility into and no channel to contest.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, general_public, payer,
    powerless, generational, trapped, global).

% Builds public campaigns and treaty advocacy around the premise that policymakers have accepted mutual vulnerability and abandoned war-winning postures — a premise the classified planning record complicates or contradicts. Cannot obtain declassified confirmation of employment doctrine sufficient to test their own advocacy claims, and is regularly dismissed by officials citing the same public taboo the advocates helped normalize.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community, excluded).

% Rely on extended deterrence guarantees whose credibility depends on some classified theory of how a nuclear exchange would actually be fought and terminated, but receive only selective, need-to-know disclosure of the planning underlying those guarantees. Cannot independently verify whether the guarantee they depend on rests on a warfighting theory or a pure-retaliation theory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, allied_governments, excluded,
    powerful, generational, constrained, continental).

% Reconstruct the gap between declaratory and employment policy from declassified archives, leaked documents, and inference from procurement patterns. Document the persistence of counterforce and limited-option planning across administrations that publicly disavowed war-winning rhetoric, without having contemporaneous access sufficient to fully close the gap.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_studies_scholars, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The public taboo genuinely coordinates crisis stability and arms-race restraint: a shared declaratory norm that no one talks about winning discourages provocative doctrine, reassures adversaries about restraint, and lowers the political cost of arms control agreements. This is a real coordination function, not merely cover.
% TRANSFER_FUNCTION: The arrangement moves accountability away from the planners and executives who retain warfighting options and toward the public and legislature, who bear the consequence of decisions made under doctrine they cannot see, debate, or vote on. It also moves reputational cost: officials collect the stability benefits of the taboo's public face while avoiding scrutiny of the classified planning that departs from it.
% ABSENT_VOICES: The general public and most of the legislature are structurally absent from any forum where the actual employment doctrine is debated: classification excludes them by design, not by choice. Arms control advocates are present in public debate but excluded from the classified planning record they would need to test their own claims against.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished and declaratory rhetoric matched classified planning, planners argue crisis stability would erode (adversaries would read explicit warfighting talk as provocative, arms control would become harder to sell domestically) — the world would rearrange around a more dangerous declaratory environment. Oversight advocates argue the world would rearrange toward accountability instead: legislatures and publics could finally evaluate the doctrine that already governs their exposure. Both camps agree removing the taboo changes something real; they dispute in which direction the change cuts.
% FOUNDING_PROBLEM: In the early Cold War, explicit public war-winning rhetoric (e.g., massive retaliation-era doctrine, 1950s counterforce advocacy) was seen as destabilizing — it invited preemption fears, complicated allied reassurance, and made arms control negotiations politically toxic. The taboo was built to remove winnability from public discourse so that declaratory policy could converge on mutual vulnerability and stabilize crisis behavior.
% FOUNDING_PROBLEM_CORROBORATION: Declassified employment guidance (PD-59, NSDD-13, and successor documents obtained via FOIA and scholarly archival work) corroborates from outside the planning establishment that counterforce and limited-option planning persisted continuously through and after the periods of strongest public disavowal — supporting the reading that the founding stability problem was real but the taboo's operational scope never matched its rhetorical scope. No corroborating source from outside the classified planning and national-security-executive seats attests that the gap has closed; scholars and former officials speaking after leaving office are the closest available corroboration, and even they work from incomplete declassification.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, contested).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at 2025) reflects the accountability transfer: planners and executives capture the benefit of operating unconstrained by public debate while oversight bodies and the public absorb the risk of doctrine they cannot evaluate. Suppression (0.72) is high because the mechanism depends on active classification enforcement, not mere social convention — the taboo's public half is maintained partly BY the same institutions that violate it in the classified half, which requires continuous information control. Theater ratio (0.61) captures that a majority of the public declaratory apparatus (arms control diplomacy language, 'cannot be won' restatements) functions as performance relative to the classified planning it does not describe — this crossed above 0.5 by the early 1980s, consistent with a metric-substitution read: rhetorical restraint replaced actual operational restraint as the signal of stability. Accessibility collapse (0.58) is moderate: alternatives (open declaratory debate matching classified doctrine) are foreclosed by classification law and political cost, but scholarly reconstruction and periodic leaks show the collapse is not complete. Resistance (0.47) reflects sustained but only partially effective pushback from arms control advocates, freedom-of-information litigation, and congressional minority efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners, weapons labs, and the national security executive derive low d (beneficiary end): they set the classification boundary, retain the operational flexibility, and bear none of the accountability cost the taboo displaces. Legislative oversight, the general public, and arms control advocates derive high d (target end): they bear the cost of a discourse constrained to match only the public half of the arrangement, with no institutional avenue to close the gap. Allied governments sit in an intermediate, constrained position — dependent on the classified doctrine's credibility without full visibility into it; their exit is limited by alliance dependency, not by preference. The override entry below adjusts allied governments' derived d upward slightly given their partial, need-to-know access, which the raw beneficiary/victim declaration alone underrepresents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding stability problem (destabilizing public war-winning rhetoric in the early Cold War) was genuinely live at founding and the coordination function it solved was real — this prevents a simple 'pure extraction' read. But the founding_problem_status is authored 'contested' rather than 'dead' because planners maintain the coordination logic still holds (an open declaratory shift to matching the classified doctrine would itself be destabilizing), while oversight-side corroboration (declassified employment guidance) shows the OPERATIONAL half of the bargain was never actually constrained the way the public was told. This is exactly the tangled_rope signature: a genuine coordination function (crisis-stability rhetoric) riding alongside asymmetric extraction (accountability transferred away from the planners who retain the very options rhetoric disavows), sustained by active classification enforcement rather than voluntary restraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gap_magnitude_uncertainty,
    'How large is the actual gap between declaratory ''unthinkable'' rhetoric and classified employment planning at any given period — is it a narrow, defensible hedge or a wide, structurally deceptive divergence?',
    'Systematic declassification review comparing contemporaneous public statements (presidential addresses, arms control negotiating positions) against employment guidance documents (PD-59, NSDD-13, and successors) at matched time points, with independent historian coding of divergence magnitude.',
    'A narrow, well-hedged gap would support reading this as a defensible operational-security scaffold with modest extraction; a wide, sustained divergence supports the tangled_rope classification with substantial accountability transfer, closer to a snare in its oversight-defeating function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_magnitude_uncertainty, empirical, 'Uncertainty about the actual size of the declaratory/operational divergence across periods.').

omega_variable(
    coordination_function_genuineness,
    'Is the public taboo''s stability-coordination function genuine and load-bearing, or is it primarily a legitimating cover story for planners who would maintain classified flexibility regardless of the public rhetoric?',
    'Comparative analysis of crisis behavior (Cuban Missile Crisis, 1983 Able Archer, post-Cold War crises) under varying degrees of declaratory-operational alignment to test whether rhetorical restraint measurably affected adversary behavior independent of the classified posture.',
    'If the coordination function is genuine and load-bearing, the tangled_rope classification (real coordination plus extraction) holds. If the taboo does no independent stabilizing work and exists only to shield planning from accountability, the constraint is better read as a snare with a coordination narrative as pure cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the declaratory taboo''s stated coordination function is structurally load-bearing or purely legitimating.').

omega_variable(
    cs_framing_underdetermination,
    'Is the correct commitment-system kernel the declaratory norm (''nuclear war cannot be won'') itself, or the classification regime that determines who may know the operational doctrine — i.e., is the contested kernel a PROPOSITION or an ACCESS RULE?',
    'Trace whether disputes historically center on contesting the truth of the winnability claim (proposition-framing) or on contesting who is cleared to see the planning that bears on it (access-framing) — legislative and FOIA litigation history would show which framing actually structures the fights.',
    'Under proposition-framing, authority_grounding centers on expertise/practice among strategists; under access-framing, authority_grounding centers on extraction via classification power itself. The two framings would assign different axioms and could shift which sibling relation (coexists_with vs influences) best describes ties to deterrence_unthinkable. This story adopts the access-framing (extraction via classification) as the primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the kernel as a contested proposition versus a contested access/classification rule, and their differing classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1962, 0.35).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.5).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1991, 0.53).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2005, 0.57).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.61).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1962, 0.44).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1991, 0.61).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_winnability_post_1945 kernel. deterrence_unthinkable authors the claim that winnability was categorically eliminated (Mountain-leaning, low ε, near-universal declaratory consensus). countervailing_thinkable authors the claim that limited victory remained openly, operationally embraced through counterforce doctrine (contested Tangled Rope or Snare depending on transparency, moderate-to-high ε). This story (rhetorical_contraction) authors the claim that the two other readings' premises BOTH partially describe reality at different layers — public discourse resembling deterrence_unthinkable, classified planning resembling countervailing_thinkable — and that the maintained GAP between the layers is itself the extractive structure, with its own distinct ε (0.68) reflecting the accountability transfer enabled by that gap. The three ε values are not averaged or reconciled; each story stands as a separate, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
