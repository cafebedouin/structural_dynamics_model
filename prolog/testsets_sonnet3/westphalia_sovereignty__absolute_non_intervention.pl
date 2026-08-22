% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Reading of Westphalian Sovereignty
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the absolute non-intervention reading of the
 *   Westphalian sovereignty kernel: the claim that territorial sovereignty is
 *   categorically inviolable, such that external interference in a state's
 *   domestic affairs is per se illegitimate irrespective of internal conduct,
 *   including mass atrocity. Under this reading, the coordination function
 *   (protecting weaker states from opportunistic external intervention) is
 *   real and historically grounded, but it is fused with an asymmetric
 *   extraction function: authoritarian elites and veto-holding great powers
 *   use the categorical bar to shield internal repression from any external
 *   remedy, while the populations subjected to that repression are
 *   structurally excluded from the negotiation that sets the bar. This is a
 *   Tangled Rope, not a Mountain or a Rope in isolation: the coordination
 *   story is genuine but is not the whole story, and active enforcement
 *   (recognition regimes, UN Charter Article 2(4)/2(7) invocation, Security
 *   Council veto practice) is required to keep the norm categorical rather
 *   than conditional.
 *
 * KEY AGENTS:
 *   - authoritarian_state_elites: Primary beneficiary (institutional/arbitrage) — uses the norm to block accountability
 *   - permanent_security_council_members: Agenda-setter and secondary beneficiary (institutional/arbitrage) — controls selective enforcement
 *   - populations_under_authoritarian_rule: Primary target (powerless/trapped) — bears the extraction with no external remedy
 *   - ethnic_and_religious_minorities_facing_atrocity: Acute target (powerless/trapped) — bears the sharpest form of the extraction
 *   - smaller_and_postcolonial_states: Secondary beneficiary (moderate/constrained) — defends the norm as protection against great-power intervention despite its costs elsewhere
 *   - international_law_scholars: Analytical observer — traces the doctrinal contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.62).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.71).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Reading of Westphalian Sovereignty").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '980dd864-75c4-444a-9c5f-be118c3cfdfc').
narrative_ontology:cs_kernel_codification('980dd864-75c4-444a-9c5f-be118c3cfdfc', formalized).
narrative_ontology:cs_authority_grounding('980dd864-75c4-444a-9c5f-be118c3cfdfc', practice).
narrative_ontology:cs_interpretation_layer_present('980dd864-75c4-444a-9c5f-be118c3cfdfc').
narrative_ontology:cs_reading_relation('980dd864-75c4-444a-9c5f-be118c3cfdfc', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('980dd864-75c4-444a-9c5f-be118c3cfdfc', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('980dd864-75c4-444a-9c5f-be118c3cfdfc', foundational, internal_conduct_categorically_inadmissible_to_legitimacy).
narrative_ontology:cs_axiom_status(internal_conduct_categorically_inadmissible_to_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('980dd864-75c4-444a-9c5f-be118c3cfdfc', internal_conduct_categorically_inadmissible_to_legitimacy, conventional).
narrative_ontology:cs_axiom('980dd864-75c4-444a-9c5f-be118c3cfdfc', foundational, territorial_jurisdiction_is_exhaustive_and_exclusive).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_is_exhaustive_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('980dd864-75c4-444a-9c5f-be118c3cfdfc', territorial_jurisdiction_is_exhaustive_and_exclusive, conventional).
narrative_ontology:cs_reference_frame('980dd864-75c4-444a-9c5f-be118c3cfdfc', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('980dd864-75c4-444a-9c5f-be118c3cfdfc', post_rwanda_r2p_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('980dd864-75c4-444a-9c5f-be118c3cfdfc', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, neighboring_states_absorbing_refugee_flows).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, smaller_and_postcolonial_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern territory with internal coercive apparatus and invoke the non-intervention norm to block external scrutiny or action regardless of how they treat their own population. They actively lobby international bodies to keep the bar against intervention high, and benefit directly whenever the norm holds.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, agenda_setter).

% Hold veto power that operationalizes the non-intervention norm selectively: they invoke it to shield allies and ignore it to justify interventions against rivals. Their structural position lets them treat the norm as leverage rather than as a binding constraint on themselves.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members, beneficiary).

% Live under a government protected from external accountability by the very norm that governs the state's relations with other states. Domestic repression, disappearance, or mass violence against them is classified as an internal affair, closing off international legal or forcible remedy regardless of severity.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, payer,
    powerless, biographical, trapped, national).

% Face targeted violence, often organized or tolerated by the state itself, and find that the categorical reading of sovereignty places their situation beyond the threshold that would legitimate outside intervention. Their only recourse is a discretionary Security Council process controlled by parties with no obligation to act.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_atrocity, payer,
    powerless, immediate, trapped, national).

% Bear the downstream costs of internal atrocities the sovereignty norm insulates from response — refugee flows, border instability, and regional destabilization — without having contributed to or having standing to resolve the originating conduct.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, neighboring_states_absorbing_refugee_flows, payer,
    moderate, biographical, constrained, regional).

% Rely on the categorical non-intervention rule as their primary protection against great-power intervention, having experienced intervention historically as a tool of domination rather than protection. They defend the norm even when it also shields other states' abuses, because weakening it structurally exposes them to the same great powers who selectively enforce exceptions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, smaller_and_postcolonial_states, beneficiary,
    moderate, generational, constrained, national).

% Document internal conduct that would trigger action under a conditional or graded reading of sovereignty, but have no enforcement standing under the absolute reading — their findings can be acknowledged and then set aside as inadmissible interference in domestic affairs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, human_rights_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Analyze the doctrinal history and contest between readings of the sovereignty kernel, tracing how the absolute non-intervention reading has been invoked, selectively suspended, and defended across different historical episodes.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, diffuse).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a return to the pre-Westphalian pattern of continuous external military and religious intervention across state borders by fixing a bright-line rule: territorial jurisdiction is exclusive and internal conduct is not a legitimate basis for external coercion. This genuinely solves a coordination problem among states of unequal power by giving weaker states a stable, legible baseline against opportunistic intervention.
% TRANSFER_FUNCTION: Moves the cost of internal state violence from the international system (which would otherwise bear intervention costs and legal exposure) onto the populations subject to that violence, who have no external recourse; simultaneously moves discretionary power to whichever states control the enforcement machinery (the Security Council permanent members), who can waive the norm at will for their own strategic purposes.
% ABSENT_VOICES: The populations actually being harmed inside the sovereign boundary have no seat in the interstate system that adjudicates whether intervention is legitimate — the norm is negotiated entirely among states, and the people whose treatment is at issue are structurally absent from the negotiation.
% DISAPPEARANCE_RATIONALE: If the categorical non-intervention reading vanished overnight, the baseline legal presumption against intervention would collapse, opening internal state conduct to routine external legal and coercive challenge; this would remove the shield currently used by authoritarian elites, but would also remove the protection weaker states currently rely on against great-power intervention justified on other pretexts — both effects would be immediate and structural.
% FOUNDING_PROBLEM: The Peace of Westphalia (1648) was built to end a century of religiously justified cross-border intervention and total war in Central Europe by establishing that rulers, not external religious or imperial authorities, would have final say over their own territories.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and international law scholars outside the group of states that benefit from the current reading attest that the 1648 problem (competing external claims to religious/dynastic jurisdiction) is largely resolved, while the reading persists to cover a structurally different problem (accountability for internal atrocity) that the original settlement never addressed. Authoritarian state elites and some postcolonial governments attest the founding problem — protection from external domination — remains fully live today, citing continued great-power intervention practices as evidence.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects that a substantial share of what the categorical reading actually does in practice is shield internal repression rather than merely coordinate interstate non-aggression; it is not maximal because the norm also performs real coordination work that benefits weak states broadly, including many that are not themselves repressive. Suppression (0.71) is high because the norm's persistence depends on active enforcement — walking back from an atrocity-triggered intervention requires overcoming institutional resistance (veto practice, non-intervention doctrine in customary international law) that exists specifically to keep the bar categorical. Accessibility collapse (0.58) is moderate: alternative readings (conditional responsibility, graded sovereignty) are visible and actively argued in the same institutions, so alternatives have not collapsed the way they would for a genuine natural law — populations inside affected states, however, experience something closer to full collapse since the interstate debate does not reach them. Resistance (0.66) is substantial: R2P advocacy, humanitarian intervention doctrine, and postcolonial critique of selective enforcement are all active, organized resistance to the categorical reading, not passive acquiescence.
 *
 * PERSPECTIVAL GAP:
 *   From the permanent Security Council members' seat, the categorical bar looks like a tool they wield selectively — a resource, not a binding constraint. From a smaller state's seat, the same bar looks like the only available shield against intervention by more powerful states. From a population living under an authoritarian government invoking the bar, it looks like a wall between them and any external remedy, regardless of what happens on their side of the border. The engine computing three structurally different experiences from one set of positional atoms is exactly the point: no single seat's experience is the correct account of the whole constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian state elites and the permanent Security Council members sit near the full-beneficiary end: they set or exploit the categorical bar and collect the benefit of insulation from external accountability, with mobile/arbitrage-grade exit from any consequence. Populations under authoritarian rule and targeted minorities sit at the full-target end: trapped exit, no standing in the norm's own adjudicating forum, and the extraction (denial of external remedy for internal violence) flows directly from the categorical reading being upheld. Smaller and postcolonial states occupy a genuinely mixed position — they are structurally closer to beneficiary than target because the categorical bar protects them against great-power intervention, even though the same bar, applied elsewhere, produces victims they do not share in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending competitive external claims to internal jurisdiction after the Wars of Religion) is largely resolved among peer sovereign states with comparable power — nobody seriously proposes restoring papal or imperial adjudication of German princely territories. But the categorical reading persists at full strength for a different, later-arising problem (domestic mass atrocity) that the 1648 settlement never contemplated. Classifying this as Tangled Rope rather than Mountain or Snare prevents two mislabeling errors: treating the norm as pure natural law (which would erase the documented selective-enforcement practice and the identifiable beneficiary class) and treating it as pure extraction (which would erase the real protective function it performs for weaker states against opportunistic great-power intervention — a function corroborated by postcolonial states' own defense of the norm, not merely claimed by the norm's worst users).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_vs_shielding_function_boundary,
    'Is the categorical non-intervention bar structurally necessary to protect weaker states from opportunistic great-power intervention, or is that protective function separable from the shielding of internal atrocity that rides on the same categorical rule?',
    'Comparative analysis of state practice under conditional-responsibility and graded-sovereignty regimes in periods and regions where they have been partially applied (e.g. post-1990s R2P invocations) to see whether weaker states experienced increased opportunistic intervention when the categorical bar was relaxed.',
    'If separable, the shielding-of-atrocity component is pure extraction riding on a genuinely separable coordination function, strengthening the case for a conditional reading; if inseparable, part of the measured extraction here is the unavoidable price of the protective function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_vs_shielding_function_boundary, conceptual, 'Whether protective and extractive functions of the categorical bar can be structurally separated.').

omega_variable(
    selective_enforcement_evidentiary_status,
    'How much of the Security Council''s selective invocation of the non-intervention norm reflects genuine doctrinal disagreement about thresholds, versus naked strategic calculation by veto-holders?',
    'Systematic coding of Security Council voting and veto records against declared doctrinal positions, cross-checked against alliance and strategic-interest data for each veto-holder.',
    'A finding of predominantly strategic calculation would strengthen the tangled_rope classification and increase confidence that the coordination story is substantially cover; a finding of genuine doctrinal disagreement would suggest more of the enforcement pattern reflects contested interpretation rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_evidentiary_status, empirical, 'Whether selective enforcement reflects doctrine or strategic self-interest.').

omega_variable(
    kernel_committer_disagreement_location,
    'Where exactly does the absolute_non_intervention reading''s disagreement with conditional_responsibility and graded_sovereignty live — in the threshold for what counts as sufficiently severe internal conduct, or in whether internal conduct is admissible evidence for legitimacy assessment at all?',
    'Textual and doctrinal comparison of R2P framework documents, UN Charter Article 2(7) jurisprudence, and failed-state intervention doctrine to locate the precise axiom each reading rejects.',
    'If the disagreement is purely about threshold calibration, the readings are closer to a single graded framework than three distinct kernels would suggest; if the disagreement is about admissibility of internal conduct as such, the readings are genuinely categorically distinct, supporting the decomposition into separate constraint files.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_disagreement_location, conceptual, 'Locating the precise structural disagreement among sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.18).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 15, 0.2).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.22).
narrative_ontology:measurement(west_tr_t45, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 45, 0.25).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.28).
narrative_ontology:measurement(west_tr_t75, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(west_be_t45, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(west_be_t75, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(west_su_t45, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(west_su_t75, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.1).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the westphalia_sovereignty kernel. absolute_non_intervention (this file) authors high extraction concentrated on populations excluded from the interstate bargaining process; conditional_responsibility authors a different beneficiary/victim structure (intervening powers become potential beneficiaries, and a different victim set — states subject to intervention on contested atrocity findings — emerges); graded_sovereignty authors extraction distributed along a capacity gradient rather than a categorical bar. Each reading emits a different ε from a shared kernel text; per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
