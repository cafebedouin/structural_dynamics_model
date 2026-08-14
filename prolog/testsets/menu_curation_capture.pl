% ============================================================================
% CONSTRAINT STORY: menu_curation_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_menu_curation_capture, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: menu_curation_capture
 *   human_readable: Self-Selected Kill-Condition Menu Curation (LLM-Assisted Falsifier Selection)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   Falling inference cost (the upstream mountain,
 *   omega_production_cost_asymmetry) makes it cheap for anyone to generate a
 *   large menu of candidate kill conditions for their own claims. This looked
 *   like a pure epistemic gain: declarants who previously could not afford
 *   disciplined falsifiability could now produce it. But generation and
 *   selection are separate acts. Selecting from the menu is itself a
 *   discretionary move, and the selector — the declarant — systematically has
 *   an incentive to choose the least-threatening, least-likely-to-fire
 *   candidate. The result is documented openness (a declared kill condition
 *   exists, with an artifact trail) with no increase in actual exposure. This
 *   story is one reading (the instrumentalist reading) of the contested
 *   kernel 'positional disagreement as evidence' — see kernel_context. The
 *   excluded stakeholder (structurally analogous to a parent whose report
 *   would actually falsify a claim about a child's welfare) never sees the
 *   menu and cannot contest the selection.
 *
 * KEY AGENTS:
 *   - the_declarant_with_slack: beneficiary/agenda_setter (moderate/arbitrage) — curates the menu and selects the safest kill condition
 *   - the_excluded_stakeholder_e_g_the_parent: payer (powerless/trapped) — bears the cost of a falsifiability performance calibrated against their own disconfirming evidence
 *   - adversarial_reviewers: excluded (moderate/constrained) — never see the full menu, only the curated output
 *   - llm_tooling_provider: observer (institutional/analytical) — supplies the generation substrate without adjudicating selection
 *   - epistemic_community_observers: observer (analytical/analytical) — can detect the capture only by comparing firing rates across the three condition-generation regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(menu_curation_capture, 0.68).
domain_priors:suppression_score(menu_curation_capture, 0.58).
domain_priors:theater_ratio(menu_curation_capture, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(menu_curation_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(menu_curation_capture, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(menu_curation_capture, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(menu_curation_capture, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(menu_curation_capture, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(menu_curation_capture, snare).
narrative_ontology:human_readable(menu_curation_capture, "Self-Selected Kill-Condition Menu Curation (LLM-Assisted Falsifier Selection)").
narrative_ontology:topic_domain(menu_curation_capture, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(menu_curation_capture, '88dbac35-ac84-49c1-8a62-cf52fc8a7261').
narrative_ontology:cs_kernel_codification('88dbac35-ac84-49c1-8a62-cf52fc8a7261', distributed).
narrative_ontology:cs_authority_grounding('88dbac35-ac84-49c1-8a62-cf52fc8a7261', distributed).
narrative_ontology:cs_reading_relation('88dbac35-ac84-49c1-8a62-cf52fc8a7261', menu_curation_capture__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('88dbac35-ac84-49c1-8a62-cf52fc8a7261', menu_curation_capture__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('88dbac35-ac84-49c1-8a62-cf52fc8a7261', menu_curation_capture__proceduralist_reading, forecloses).
narrative_ontology:cs_axiom('88dbac35-ac84-49c1-8a62-cf52fc8a7261', foundational, cheap_generation_can_yield_legitimate_falsifiers).
narrative_ontology:cs_axiom_status(cheap_generation_can_yield_legitimate_falsifiers, holdable).
narrative_ontology:cs_axiom_grounding('88dbac35-ac84-49c1-8a62-cf52fc8a7261', cheap_generation_can_yield_legitimate_falsifiers, instrumental).
narrative_ontology:cs_axiom('88dbac35-ac84-49c1-8a62-cf52fc8a7261', foundational, selection_discretion_is_the_locus_of_extraction_not_production_cost).
narrative_ontology:cs_axiom_status(selection_discretion_is_the_locus_of_extraction_not_production_cost, holdable).
narrative_ontology:cs_axiom_grounding('88dbac35-ac84-49c1-8a62-cf52fc8a7261', selection_discretion_is_the_locus_of_extraction_not_production_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('88dbac35-ac84-49c1-8a62-cf52fc8a7261', cheap_generation_as_epistemic_democratization).
narrative_ontology:cs_drift_state('88dbac35-ac84-49c1-8a62-cf52fc8a7261', post_curated_menu_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88dbac35-ac84-49c1-8a62-cf52fc8a7261', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:constraint_victim(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent).
narrative_ontology:constraint_vindicates(menu_curation_capture, declared_kill_conditions_constitute_genuine_falsifiability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Has the time, tooling, and survivable-public-error slack to run an LLM, generate a menu of candidate kill conditions, and pick one to declare publicly as their falsifiable commitment. Controls which candidate becomes 'the' declared condition. Because generation is cheap, they can silently discard menu items that would actually threaten their position and keep the one least likely to ever fire, while pointing to the artifact of a documented, model-assisted kill condition as evidence of good-faith openness to disconfirmation.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_declarant_with_slack, beneficiary,
    moderate, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(menu_curation_capture, the_declarant_with_slack, agenda_setter).

% Is the person whose contrary report about the arrangement (e.g. the parent's account of a child's welfare, or an equivalent structurally subordinate party's account) would actually falsify the declarant's position, but has no access to the menu-generation step, no say in which candidate is selected, and often no visibility into the fact that a menu existed at all. Bears the cost of a system that looks self-correcting but is calibrated to never register their disconfirming evidence.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent, payer,
    powerless, biographical, trapped, local).

% Would, if given the full unfiltered LLM-generated menu, likely select or propose a genuinely threatening kill condition rather than the declarant's chosen one. They are not consulted on menu curation; they only see the single condition already selected, and their adversarial function is preempted before they ever engage.
narrative_ontology:constraint_stakeholder(menu_curation_capture, adversarial_reviewers, excluded,
    moderate, immediate, constrained, local).

% Supplies the cheap generation capacity that makes the menu possible at all. Does not adjudicate which item is selected and has no stake in any particular declarant's curation choice, but the tool's agreeableness and breadth of candidate generation is the substrate the capture rides on.
narrative_ontology:constraint_stakeholder(menu_curation_capture, llm_tooling_provider, observer,
    institutional, generational, analytical, global).

% Researchers and commentators evaluating whether declared kill conditions function as real falsifiers, by comparing firing rates across self-selected-from-menu, unassisted self-generated, and adversarially-assigned condition sets. Their comparison is the primary observable that could expose the capture, but requires access to the full menu history, which declarants have no incentive to preserve or disclose.
narrative_ontology:constraint_stakeholder(menu_curation_capture, epistemic_community_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:fixing_cost_class(menu_curation_capture, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-cost mechanism for anyone to generate a bank of candidate falsifiers for their own claims, in principle raising the discipline available to ordinary declarants who previously could not afford to construct rigorous kill conditions unassisted.
% TRANSFER_FUNCTION: Moves epistemic credibility from the excluded stakeholder (whose disconfirming report is the thing that should count) to the declarant (who gets to wear the appearance of falsifiability without bearing its risk), by converting a discretionary curation act into an invisible, undocumented step inside an otherwise-legible artifact.
% ABSENT_VOICES: The excluded stakeholder (e.g. the parent, or any structurally subordinate reporter) never sees the generated menu and has no channel to argue that a different, more threatening candidate should have been selected. Adversarial reviewers are shown only the outcome of curation, not the curation process itself.
% DISAPPEARANCE_RATIONALE: If menu curation discretion were removed — for example by requiring publication of the full generated menu, or by assigning the kill condition adversarially rather than allowing self-selection — the declarant's exposure would rise to match the appearance of openness, empirical firing rates on self-selected conditions would converge toward those of adversarially-assigned conditions, and a documented gap currently invisible to observers would become visible and costly to the declarant.
% FOUNDING_PROBLEM: Before cheap generation, almost no one could afford to construct disciplined, pre-registered kill conditions for their own claims; the tool was meant to democratize a form of intellectual honesty previously available only to well-resourced institutions running formal adversarial collaborations.
% FOUNDING_PROBLEM_CORROBORATION: The declarant class attests the founding problem remains live and that any declared kill condition, however chosen, represents net epistemic improvement over none. Epistemic-community observers comparing firing rates across self-selected, unassisted, and adversarially-assigned condition sets — a comparison run from outside the declarant's own incentive structure — attest that self-selected-from-menu conditions fire at markedly lower empirical rates than adversarially-assigned ones, corroborating that the founding problem has been structurally repurposed into a legitimacy-conferring performance rather than solved.
narrative_ontology:disappearance_verdict(menu_curation_capture, world_rearranges).
narrative_ontology:founding_problem_status(menu_curation_capture, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(menu_curation_capture, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(menu_curation_capture, 'none', 1).
narrative_ontology:epsilon_provenance(menu_curation_capture, 0.68, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(menu_curation_capture_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(menu_curation_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(menu_curation_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.35 to 0.68) as the practice of LLM-assisted menu generation becomes normalized and declarants gain more experience curating favorable outputs; theater_ratio rises faster and higher (0.40 to 0.71) because the documented-openness artifact becomes increasingly performative relative to actual exposure as curation skill improves. Suppression is moderate (0.58): there is no active coercion preventing adversarial reviewers or excluded stakeholders from demanding the full menu, but the menu's existence and contents are simply never disclosed by default, which functions as a soft suppression of the counter-evidence channel. Resistance is comparatively low (0.35) because the capture is largely invisible — the artifact looks like good epistemic practice, so there is little organized pushback until firing-rate comparisons expose the gap. Accessibility_collapse (0.62) reflects that once the practice of self-curated declaration becomes normalized as 'sufficient' falsifiability, the harder discipline of adversarial or unassisted self-generation becomes reputationally unnecessary to pursue.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarant sits near the beneficiary end: they control curation, bear no cost from the discarded threatening candidates (which are simply never spoken), and enjoy reputational credit for the declared condition regardless of its actual bite. The excluded stakeholder sits near the full-target end: trapped exit options (they cannot exit the relationship that makes their disconfirming report relevant), no access to the curation step, and their report is precisely the evidence the curation is calibrated to exclude. Adversarial reviewers are excluded rather than coordinated — they are shown outcomes, not process, which forecloses their proper adversarial function before it can operate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratizing disciplined falsifiability) is only partially dead: cheap generation genuinely lowered the cost of producing SOME real kill conditions, so the mandate is not fully obsolete. But the mechanism has been captured at the selection step — the arrangement persists and even expands (more declarants adopt it, theater_ratio climbs) while its actual falsification function stagnates or declines relative to its documented appearance. This is exactly the kind of drift the founding_problem/disappearance_verdict mismatch check is built to catch: founding_problem_status is contested rather than flatly dead, because unassisted and adversarially-assigned conditions drawn from the same tooling can and do fire at expected rates — it is specifically the self-selected-from-menu pathway that has decoupled from exposure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    curation_versus_generation_locus,
    'Is the extraction located in the generation step (the model''s tendency toward agreeable, non-threatening candidate outputs) or in the selection step (the declarant''s discretionary choice among a genuinely diverse menu)?',
    'Audit a sample of full unfiltered LLM-generated menus against the declarant''s selected condition: if the menu itself is narrow and non-threatening across the board, the extraction is upstream in generation (implicating the tooling); if the menu contains genuinely threatening candidates that are then passed over, the extraction is downstream in selection (implicating the declarant).',
    'If generation-located, the correct fix is tooling-level (adversarial prompting, diversity-forcing generation); if selection-located, the correct fix is disclosure-based (mandatory publication of the full menu). These point to different remedies and different responsible parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curation_versus_generation_locus, empirical, 'Whether the capture originates in model output bias or declarant curation choice.').

omega_variable(
    menu_disclosure_counterfactual,
    'Would mandatory disclosure of the full generated menu (not just the selected condition) restore the practice''s original falsifiability function, or would declarants adapt by curating prompts to bias the generation step instead?',
    'Natural or designed experiment: compare firing rates of declared kill conditions before and after a disclosure norm is introduced in a research or public-commitment community.',
    'If disclosure restores function, the constraint is closer to a fixable tangled_rope (genuine coordination value recoverable with a transparency fix); if declarants adapt around disclosure, the extraction is more deeply structural and the snare classification is robust to procedural patches.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(menu_disclosure_counterfactual, empirical, 'Whether transparency remedies are sufficient or merely relocate the capture.').

omega_variable(
    cross_reading_classification_divergence,
    'Given that this story is authored under the instrumentalist reading of the kernel, would the standpoint or proceduralist readings of the same underlying arrangement classify it as something other than a snare?',
    'Author sibling constraint stories under standpoint_reading and proceduralist_reading with their own beneficiary/victim structures per the kernel manifest''s expected_structural_deltas, and compare computed types.',
    'If sibling readings compute to rope or tangled_rope rather than snare, this documents that the snare classification is reading-specific to the instrumentalist framing''s exposure of the curated-menu pathway, not a topic-level verdict about positional disagreement as evidence generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_classification_divergence, conceptual, 'Whether the snare classification is stable across the kernel''s sibling readings or an artifact of the instrumentalist framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(menu_curation_capture, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(menu_tr_t0, menu_curation_capture, theater_ratio, 0, 0.4).
narrative_ontology:measurement(menu_tr_t4, menu_curation_capture, theater_ratio, 4, 0.48).
narrative_ontology:measurement(menu_tr_t8, menu_curation_capture, theater_ratio, 8, 0.55).
narrative_ontology:measurement(menu_tr_t12, menu_curation_capture, theater_ratio, 12, 0.61).
narrative_ontology:measurement(menu_tr_t16, menu_curation_capture, theater_ratio, 16, 0.65).
narrative_ontology:measurement(menu_tr_t20, menu_curation_capture, theater_ratio, 20, 0.69).
narrative_ontology:measurement(menu_tr_t24, menu_curation_capture, theater_ratio, 24, 0.71).

% Extraction over time
narrative_ontology:measurement(menu_be_t0, menu_curation_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(menu_be_t4, menu_curation_capture, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(menu_be_t8, menu_curation_capture, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(menu_be_t12, menu_curation_capture, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(menu_be_t16, menu_curation_capture, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(menu_be_t20, menu_curation_capture, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(menu_be_t24, menu_curation_capture, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(menu_curation_capture, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(menu_curation_capture, information_standard).
narrative_ontology:boltzmann_floor_override(menu_curation_capture, 0.03).
narrative_ontology:affects_constraint(menu_curation_capture, omega_production_cost_asymmetry).

% DUAL FORMULATION NOTE:
% menu_curation_capture is downstream of omega_production_cost_asymmetry (claimed mountain: the falling cost of generating candidate falsifiers is treated as a structural fact of current tooling economics, not a choice any party makes). The mountain establishes that cheap generation is available to everyone; this story documents a second-order discretionary act — selection from the generated menu — that the mountain's own cost structure does not determine and that reintroduces exactly the extraction the mountain's cheapness was supposed to dissolve. The two are not the same constraint under different measurement: the mountain's ε is near-zero (an availability fact) while this constraint's ε is substantial and rising (a selection-incentive fact). Decomposed per the ε-invariance principle rather than treated as one constraint at two observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
