% ============================================================================
% CONSTRAINT STORY: arbitrary_selection_under_competence_signaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arbitrary_selection_under_competence_signaling, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arbitrary_selection_under_competence_signaling
 *   human_readable: Discretionary Culling Dressed as Competence Selection
 *   domain: organizational/narrative
 *
 * SUMMARY:
 *   This story reads a narrative pattern rather than a real-world
 *   institution: a captain and his lieutenant (Voss) repeatedly select crew
 *   members for removal using language of competence — steady hands,
 *   punctuality, soundness — while the actual selection outcomes are
 *   uncorrelated with those signals. Duvray, established in detail as
 *   competent, is selected anyway. A predecessor mate, established the same
 *   way, was selected before him. The detail invested in establishing merit
 *   functions structurally as evidence of merit's irrelevance to the outcome:
 *   the text shows you the criteria are stated, then shows you they are not
 *   applied. This is downstream of measurement_authority_decoupling — the
 *   upstream tangled_rope where the authority to measure competence has
 *   already separated from any accountability for how that measurement is
 *   used, creating the exact discretionary gap this constraint exploits at
 *   the point of selection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arbitrary_selection_under_competence_signaling, 0.81).
domain_priors:suppression_score(arbitrary_selection_under_competence_signaling, 0.72).
domain_priors:theater_ratio(arbitrary_selection_under_competence_signaling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arbitrary_selection_under_competence_signaling, extractiveness, 0.81).
narrative_ontology:constraint_metric(arbitrary_selection_under_competence_signaling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(arbitrary_selection_under_competence_signaling, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(arbitrary_selection_under_competence_signaling, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(arbitrary_selection_under_competence_signaling, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arbitrary_selection_under_competence_signaling, snare).
narrative_ontology:human_readable(arbitrary_selection_under_competence_signaling, "Discretionary Culling Dressed as Competence Selection").
narrative_ontology:topic_domain(arbitrary_selection_under_competence_signaling, "organizational/narrative").

domain_priors:requires_active_enforcement(arbitrary_selection_under_competence_signaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arbitrary_selection_under_competence_signaling, captain_and_voss_as_unaccountable_selectors).
narrative_ontology:constraint_victim(arbitrary_selection_under_competence_signaling, duvray).
narrative_ontology:constraint_victim(arbitrary_selection_under_competence_signaling, unnamed_predecessor_quartermasters_mate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the sole power to name who is culled from the crew roster at each selection point. Invokes competence language — steadiness, punctuality, soundness of hand — to narrate the choice publicly, but the actual selection tracks something else: loyalty tests, personal grudges, or simple convenience. Because no external party can audit the criteria against the outcome, the selector's authority is never falsifiable by the crew and never costs the selector anything to exercise.
narrative_ontology:constraint_stakeholder(arbitrary_selection_under_competence_signaling, captain_and_voss_as_unaccountable_selectors, agenda_setter,
    institutional, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(arbitrary_selection_under_competence_signaling, captain_and_voss_as_unaccountable_selectors, beneficiary).

% Established across the narrative as capable — steady hands, punctual, sound in every visible respect. Is selected for removal (culled, sacrificed, or otherwise ejected from the crew's protection) despite this. Has no venue to contest the decision, no record of the stated criteria to point to, and no exit from the vessel or company that does not itself require surviving the selection.
narrative_ontology:constraint_stakeholder(arbitrary_selection_under_competence_signaling, duvray, payer,
    powerless, immediate, trapped, local).

% The prior instance of the same pattern: a competent mate, selected out under the same unstated criteria before Duvray's tenure. Exists in the narrative mainly as a precedent, establishing that the mismatch between competence and survival is not a single anomalous event but a repeating structural feature of how selection is exercised aboard.
narrative_ontology:constraint_stakeholder(arbitrary_selection_under_competence_signaling, unnamed_predecessor_quartermasters_mate, payer,
    powerless, immediate, trapped, local).

% Witnesses the selection and its competence-irrelevant outcome but has no standing to challenge it. Some crew privately note the mismatch between who is skilled and who is culled, but voicing this risks becoming the next selection target — so objection stays private, never reaching the selectors.
narrative_ontology:constraint_stakeholder(arbitrary_selection_under_competence_signaling, remaining_crew, excluded,
    powerless, biographical, constrained, local).

% Reads the text's deliberate juxtaposition — pages establishing Duvray's competence set immediately against his selection for removal — as the story's own evidence that stated criteria are cover, not cause. The detail invested in demonstrating merit is legible only as demonstrating merit's irrelevance.
narrative_ontology:constraint_stakeholder(arbitrary_selection_under_competence_signaling, narrative_observer, observer,
    analytical, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(arbitrary_selection_under_competence_signaling, captain_and_voss_as_unaccountable_selectors).
narrative_ontology:fixing_cost_class(arbitrary_selection_under_competence_signaling, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves a real problem: someone must decide who stays aboard when resources, berths, or trust are scarce, and a single empowered decider avoids paralysis by committee.
% TRANSFER_FUNCTION: Moves security of position and continued livelihood away from whoever is selected and toward the selectors' unaccountable discretion — the selected party loses standing, protection, and often life or livelihood; the selectors retain total control over who bears that loss with no cost to themselves.
% ABSENT_VOICES: Duvray and the unnamed predecessor mate would object that stated criteria were never actually applied to their selection, but neither has any forum in which to raise it — the selection is final at the moment it is announced, and dissent from the remaining crew stays private for fear of becoming the next target.
% DISAPPEARANCE_RATIONALE: If the selectors' unaccountable discretion vanished and selection were instead bound to the stated competence criteria, the outcome distribution would shift sharply — Duvray and his predecessor, both established as competent, would not have been selected. The crew's entire calculus of behavior (which currently optimizes for pleasing selectors rather than performing competently) would reorganize around the actual stated criteria.
% FOUNDING_PROBLEM: The crew needed some mechanism for culling headcount or resolving scarce-berth competition without collapsing into open conflict — a decision authority was needed to keep the vessel functioning under resource or personnel pressure.
% FOUNDING_PROBLEM_CORROBORATION: No party outside the selectors attests that competence is actually the deciding factor. The remaining crew's private observations, and the narrative's own juxtaposition of Duvray's established competence against his selection, corroborate from outside the beneficiary group that the stated criteria and the actual mechanism have diverged; no independent audit exists because none is permitted.
narrative_ontology:disappearance_verdict(arbitrary_selection_under_competence_signaling, world_rearranges).
narrative_ontology:founding_problem_status(arbitrary_selection_under_competence_signaling, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(arbitrary_selection_under_competence_signaling, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(arbitrary_selection_under_competence_signaling, 'none', 1).
narrative_ontology:epsilon_provenance(arbitrary_selection_under_competence_signaling, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arbitrary_selection_under_competence_signaling_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(arbitrary_selection_under_competence_signaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arbitrary_selection_under_competence_signaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.55 to 0.81) because each successive selection event further demonstrates that stated criteria carry no predictive weight — the pattern compounds rather than resolving. Suppression is substantial (0.72) because crew members who might object risk becoming the next selection target, a structural silencer independent of the selectors' overt power. Theater ratio climbs (0.35 to 0.58) because the invocation of competence language becomes increasingly performative with each demonstrated mismatch — it persists as ritual justification even as its descriptive accuracy collapses.
 *
 * PERSPECTIVAL GAP:
 *   From the selectors' seat, the arrangement looks like necessary command discretion — someone must decide, and command prerogative is the coordination story. From Duvray's seat, the same structure is naked extraction: his competence was established and then proven irrelevant to his survival. The engine should compute these as genuinely different types from the two structural positions; the narrative's insistence on establishing competence in detail is precisely the data that makes the payer-seat computation diverge sharply from the agenda-setter's self-narration.
 *
 * DIRECTIONALITY LOGIC:
 *   Captain and Voss sit at the full-beneficiary end: they hold total discretion, bear no cost for exercising it, and can exit any consequence by simply not stating true criteria. Duvray and the predecessor mate sit at the full-target end: trapped aboard, no forum for appeal, selected regardless of the competence they visibly possess. The remaining crew occupy a constrained middle position — not yet selected, but their exit options (protest, refusal) are foreclosed by fear of becoming the next case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing some decision mechanism under scarcity — was real once. But the narrative's own repeated demonstration that stated criteria don't predict outcomes shows the mechanism has drifted from solving that problem to serving as unaccountable discretion dressed in the vocabulary of the original problem. Classifying this as snare rather than tangled_rope avoids mislabeling residual command necessity as ongoing coordination: there is no coordination function left being served by the mismatch between stated and actual criteria — only extraction wearing the coordination function's old clothes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_existence_ambiguity,
    'Do the captain and Voss actually apply some criteria (however cruel or non-competence-based) consistently, or is the selection genuinely random/whim-driven with competence language as pure post-hoc cover?',
    'Cross-reference every selection event in the narrative against a full set of candidate criteria (loyalty, personal grievance, resemblance to a past betrayer, sheer whim) to see if any single alternative criterion predicts the outcome better than competence does.',
    'If a consistent non-competence criterion is found (e.g., always culling those who question orders), the constraint is better read as extraction organized around loyalty enforcement rather than pure arbitrariness — still a snare, but with a different suppression mechanism to name.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_existence_ambiguity, conceptual, 'Whether an unstated but consistent criterion exists beneath the stated competence language.').

omega_variable(
    narrative_intent_vs_structural_reading,
    'Is the mismatch between competence and selection an intentional authorial device (to critique arbitrary authority) or would a naive first read miss it, with the extraction only visible on structural analysis?',
    'Compare reader response data or critical commentary on the text to determine whether the mismatch is broadly legible without deliberate framing.',
    'If the mismatch requires structural analysis to surface, the in-narrative suppression (crew silence, private-only dissent) may be doing more work than a casual reading would credit — raising the effective suppression estimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_intent_vs_structural_reading, conceptual, 'Whether the extraction pattern is legible without deliberate structural reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arbitrary_selection_under_competence_signaling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arbi_tr_t0, arbitrary_selection_under_competence_signaling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arbi_tr_t4, arbitrary_selection_under_competence_signaling, theater_ratio, 4, 0.4).
narrative_ontology:measurement(arbi_tr_t8, arbitrary_selection_under_competence_signaling, theater_ratio, 8, 0.46).
narrative_ontology:measurement(arbi_tr_t12, arbitrary_selection_under_competence_signaling, theater_ratio, 12, 0.5).
narrative_ontology:measurement(arbi_tr_t16, arbitrary_selection_under_competence_signaling, theater_ratio, 16, 0.54).
narrative_ontology:measurement(arbi_tr_t20, arbitrary_selection_under_competence_signaling, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(arbi_be_t0, arbitrary_selection_under_competence_signaling, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arbi_be_t4, arbitrary_selection_under_competence_signaling, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(arbi_be_t8, arbitrary_selection_under_competence_signaling, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(arbi_be_t12, arbitrary_selection_under_competence_signaling, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(arbi_be_t16, arbitrary_selection_under_competence_signaling, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(arbi_be_t20, arbitrary_selection_under_competence_signaling, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(arbi_su_t0, arbitrary_selection_under_competence_signaling, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arbi_su_t4, arbitrary_selection_under_competence_signaling, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(arbi_su_t8, arbitrary_selection_under_competence_signaling, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(arbi_su_t12, arbitrary_selection_under_competence_signaling, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(arbi_su_t16, arbitrary_selection_under_competence_signaling, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(arbi_su_t20, arbitrary_selection_under_competence_signaling, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arbitrary_selection_under_competence_signaling, enforcement_mechanism).
narrative_ontology:affects_constraint(arbitrary_selection_under_competence_signaling, measurement_authority_decoupling).

% DUAL FORMULATION NOTE:
% measurement_authority_decoupling (tangled_rope) establishes the upstream structure: the authority to assess competence has decoupled from accountability for what that assessment is used for. arbitrary_selection_under_competence_signaling is the downstream instance where that decoupled authority is exercised at the point of culling specific individuals — the coordination function (someone must decide) that partially justified the upstream tangled_rope has fully atrophied here, leaving pure extraction. The two are linked but structurally distinct: the upstream retains a genuine (if abused) coordination function; the downstream selection event does not.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
