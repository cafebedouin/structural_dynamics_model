% ============================================================================
% CONSTRAINT STORY: omega_production_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omega_production_cost_asymmetry, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: omega_production_cost_asymmetry
 *   human_readable: Asymmetry Between Falsifier-Generation Cost and Belief-Revision Cost
 *   domain: epistemology/philosophy_of_technology
 *
 * SUMMARY:
 *   Between 2023 and 2026 the per-token cost of LLM inference fell by roughly
 *   99%, and with it the cost of generating a falsifier, an
 *   alternative-position sample, or a taxonomy label for any given claim
 *   collapsed to near zero. Anyone can now ask a model to produce ten
 *   counterarguments, five adversarial readings, or a taxonomy of positions
 *   on a contested question in seconds. What has not moved, and cannot be
 *   moved by any tooling improvement, is the cost of actually abiding a
 *   precommitment: changing one's mind in public, absorbing a disconfirming
 *   result that undercuts a funded research program, retracting a stated
 *   position at professional or social cost, or triggering a declared kill
 *   condition and following through on it. This is not a claim about any
 *   single institution's behavior — it is a claim about the structure of what
 *   generation and revision each require. Generation is computational;
 *   revision is social, reputational, and identity-bound, and none of those
 *   costs are denominated in tokens. The constraint is claimed as a mountain
 *   because the asymmetry is a structural fact about the difference between
 *   producing an artifact and being changed by one, not a policy choice any
 *   single actor made. It is authored with declared beneficiaries — analysts
 *   and institutions who can now cheaply generate the appearance of
 *   adversarial rigor — because the mountain claim needs testing against the
 *   false-summit possibility: is 'omega production is now trivial' actually
 *   being used to launder a lack of real belief revision as procedural
 *   virtue?
 *
 * KEY AGENTS:
 *   - llm_assisted_analysts: Primary beneficiary of the cost collapse (institutional/arbitrage) — can generate large volumes of falsifiers and omega variables cheaply, and the appearance of rigor this creates
 *   - institutions_claiming_procedural_rigor: Secondary beneficiary (institutional/constrained) — cite abundant generated counter-evidence as proof of epistemic seriousness without any corresponding change in what they actually do when disconfirmed
 *   - domain_experts_facing_disconfirmation: Bears the unchanged cost (moderate/trapped) — the people whose actual belief revision, retraction, or policy reversal the artifact production was supposed to be evidence toward
 *   - external_auditors: Analytical observer (analytical/analytical) — tries to distinguish real precommitment-honoring from generated theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omega_production_cost_asymmetry, 0.15).
domain_priors:suppression_score(omega_production_cost_asymmetry, 0.05).
domain_priors:theater_ratio(omega_production_cost_asymmetry, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, extractiveness, 0.15).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omega_production_cost_asymmetry, mountain).
narrative_ontology:human_readable(omega_production_cost_asymmetry, "Asymmetry Between Falsifier-Generation Cost and Belief-Revision Cost").
narrative_ontology:topic_domain(omega_production_cost_asymmetry, "epistemology/philosophy_of_technology").

domain_priors:emerges_naturally(omega_production_cost_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, llm_assisted_analysts).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, institutions_claiming_procedural_rigor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, domain_experts_facing_disconfirmation).
narrative_ontology:constraint_vindicates(omega_production_cost_asymmetry, declared_precommitments_are_not_self_enforcing).
narrative_ontology:constraint_vindicates(omega_production_cost_asymmetry, cheap_falsifier_generation_does_not_imply_cheap_belief_revision).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can generate large batches of falsifiers, counter-readings, and taxonomy labels for any contested claim in minutes at near-zero marginal cost. This capacity did not exist at scale before 2023 and now lets them produce the visible trappings of adversarial rigor cheaply, regardless of whether they themselves would pay any cost to be wrong.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, llm_assisted_analysts, beneficiary,
    moderate, biographical, arbitrage, global).

% Point to volumes of generated adversarial material, declared kill conditions, and taxonomies of alternative positions as evidence of epistemic seriousness. They set the terms under which 'we considered the counter-arguments' counts as having discharged an obligation, without a parallel accounting of how often stated kill conditions actually triggered a reversal.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, institutions_claiming_procedural_rigor, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(omega_production_cost_asymmetry, institutions_claiming_procedural_rigor, agenda_setter).

% Hold positions, funded programs, or public commitments that a disconfirming result would require them to actually revise or retract. The cost of doing so — reputational, financial, identity-level — has not fallen at all, no matter how cheaply the disconfirming material itself can now be produced or surfaced by others.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, domain_experts_facing_disconfirmation, payer,
    moderate, biographical, trapped, national).

% Try to distinguish institutions that actually abide their declared precommitments from institutions that merely generate abundant adversarial-looking material. They have no standard metric for belief-revision or retraction rates comparable to the well-documented LLM cost-decline curve, which is itself part of what they are trying to measure the absence of.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, external_auditors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(omega_production_cost_asymmetry, diffuse).
narrative_ontology:fixing_cost_class(omega_production_cost_asymmetry, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the strict sense — this is not an arrangement anyone runs for mutual benefit. It is a structural fact about two different operations (producing an artifact vs. being changed by one) having radically different, and diverging, cost curves. To the extent there is a coordination story, it is downstream: institutions coordinate around citing cheap artifact production as if it satisfies norms of intellectual honesty that actually require costly revision.
% TRANSFER_FUNCTION: No direct transfer of resources occurs at the mountain level. What moves, when the fact is exploited, is reputational credit: institutions and analysts who generate abundant falsifiers or omega menus accrue credibility for rigor that domain experts who never get to see a matching revision-rate metric cannot easily contest.
% ABSENT_VOICES: Anyone trying to hold an institution to a declared kill condition after the fact has no comparable public metric to point to — there is no 'retraction rate' dashboard the way there is a token-cost dashboard. Their objection ('you said you'd change your mind and didn't') is real but structurally undocumented and easy to wave away with a fresh batch of generated counter-considerations.
% DISAPPEARANCE_RATIONALE: If the cost asymmetry itself vanished — if belief-revision costs fell as fast as generation costs — the world would rearrange substantially: institutions would face real pressure to make good on declared kill conditions, and the current practice of citing artifact volume as a proxy for rigor would lose its cover. But the parties disagree on what 'disappearing' would even mean here: the beneficiary seats treat the fact as a permanent feature of cognition and institutions (nothing to rearrange around, since it is a mountain), while the payer seat treats it as evidence of a fixable design gap that institutions have chosen not to close.
% FOUNDING_PROBLEM: There was no founding event — this constraint was not built to solve a problem; it emerged as a byproduct of LLM inference costs falling while the social and psychological costs of actually changing one's mind remained governed by entirely separate, non-computational mechanisms (status, identity, sunk investment, institutional face).
% FOUNDING_PROBLEM_CORROBORATION: The token-cost decline is independently and extensively documented by API pricing data across providers (a matter of public record, not asserted by any beneficiary). No comparable independent metric exists for belief-revision rates; this absence is itself attested by methodologists and philosophers of science studying preregistration and adversarial collaboration outcomes, who note the field lacks a standard instrument for measuring how often declared kill conditions are actually honored — a corroboration from outside any beneficiary of the asymmetry, since it is a complaint about missing data rather than a claim in anyone's favor.
narrative_ontology:disappearance_verdict(omega_production_cost_asymmetry, contested).
narrative_ontology:founding_problem_status(omega_production_cost_asymmetry, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(omega_production_cost_asymmetry, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(omega_production_cost_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(omega_production_cost_asymmetry, 0.15, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omega_production_cost_asymmetry_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(omega_production_cost_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(omega_production_cost_asymmetry),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(omega_production_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15) because the constraint itself extracts nothing directly — it is a structural fact about differential cost, not an enforced transfer. What rises over the interval is theater_ratio (0.2 to 0.62): as generation cost fell, the volume of produced omegas, falsifiers, and taxonomy labels rose sharply, but nothing in the record shows a matching rise in actual retraction or policy reversal rates. The theater_ratio series tracks the growing gap between documented artifact volume and any comparable metric for revision behavior — there simply is no such metric, which is itself part of the constraint's structure. accessibility_collapse is authored high (0.88): once you see the asymmetry, there is no alternative way to interpret cheap falsifier generation as evidence of costly belief revision — the two are definitionally different operations. resistance is low (0.12) because almost no one actively contests the underlying fact once stated; the friction is in getting anyone to act on it, not in disputing it.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (an analyst or institution generating abundant adversarial material), the mountain reads as genuine epistemic progress — more perspectives considered, more falsifiers on the table, procedural virtue demonstrated. From the seat of someone whose actual disconfirmation would need to be absorbed, the same mountain reads as a distraction: the volume of generated counter-evidence has no bearing on whether the costly step — actually changing a funded position, a career trajectory, or a public commitment — will occur. The engine should show this divergence as a function of exit_options and power, not as a disagreement about whether the underlying cost trend is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (llm_assisted_analysts, institutions_claiming_procedural_rigor) sit near the beneficiary end of directionality: they can produce the artifacts of rigor at near-zero marginal cost and are not the ones who bear the cost when a real disconfirmation lands. domain_experts_facing_disconfirmation sit closer to the target end: for them the constraint is lived as the unmoved, still-expensive requirement to actually revise, retract, or pay the social price — precisely the thing cheap generation does nothing to discount. No victim group is declared because this is authored as a mountain: the structural fact of the cost gap is not itself an extraction mechanism, though its use by beneficiaries to simulate rigor is what the omega variables interrogate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here runs in the false-summit direction: a genuine structural fact (falling generation cost, flat revision cost) can be, and evidently is being, cited as though it discharges the harder obligation (actually revising beliefs). The mountain classification protects against two opposite errors: treating the cost-asymmetry claim as itself extractive (it is not — it is a true description of two different cost structures), and treating heavy citation of 'we generated many falsifiers' as equivalent to 'we changed our position when disconfirmed' (it is not, and conflating them is exactly the theater the theater_ratio series tracks). The constraint's mandate — describing an underlying fact about cost structures — has not outlived its function; if anything the function has intensified as generation costs kept falling. What could go stale is any specific institution's *use* of the fact as a shield, which is a downstream, institution-specific question the mountain claim does not adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_incentive_gap,
    'Is the gap between falsifier-production cost and belief-revision cost a structural feature of minds and institutions (a fact about what changing one''s mind actually requires — social standing, sunk identity, career risk), or is it a constructed gap that could be closed by different institutional design (binding consequences for declared kill conditions, reputational markets that price retraction favorably)?',
    'Compare domains with genuine binding precommitment infrastructure (registered clinical trials with mandatory reporting, prediction markets with real settlement) against domains with only declarative kill conditions (blog-post ''I will change my mind if X'' pledges). If binding infrastructure measurably raises revision rates independent of production cost, the gap is at least partly constructed and remediable, not a pure mountain.',
    'If constructed, the constraint reclassifies toward tangled_rope or piton (an institution could close the gap but the cost of doing so is borne unevenly, or the declarative apparatus persists as theater after its remediable function was never built). If structural, the mountain claim holds and the theater_ratio measures cope, not fixable cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_incentive_gap, conceptual, 'Whether the cost asymmetry is an irreducible fact about cognition/institutions or a remediable design gap dressed as one.').

omega_variable(
    cs_framing_underdetermination,
    'Two coherent framings of the underlying kernel produce different verdicts here: (a) treating ''positional disagreement as evidence'' as the kernel, with this constraint describing the material precondition (cheap generation) that the instrumentalist reading claims realizes that kernel; or (b) treating the falling cost of LLM inference itself as a separate, freestanding mountain independent of any epistemic-normative kernel at all, with the epistemological kernel riding on top of it as commentary. Framing (a) makes this story a component within the kernel network (linked via cs_structure); framing (b) makes it a bare technological/cognitive fact with no kernel dependency.',
    'Ask whether the extraction pattern (curated omega menus, model agreeableness) disappears if the epistemological kernel is stipulated away — i.e., if no one claimed disagreement was evidence at all. It does not: cheap generation still creates the same asymmetric incentive to perform falsifiability. This suggests framing (b) is closer to true and the kernel linkage is downstream commentary, not constitutive.',
    'Under framing (a), this constraint would need reading_relations to the kernel''s four siblings; under framing (b), the mountain classification is self-standing and the instrumentalist reading is better modeled as a distinct, dependent constraint story linked via network.affects_constraints rather than as sibling axioms on this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether this story is itself a kernel reading or a freestanding mountain that the instrumentalist reading depends upon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omega_production_cost_asymmetry, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omeg_tr_t0, omega_production_cost_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement(omeg_tr_t6, omega_production_cost_asymmetry, theater_ratio, 6, 0.3).
narrative_ontology:measurement(omeg_tr_t12, omega_production_cost_asymmetry, theater_ratio, 12, 0.4).
narrative_ontology:measurement(omeg_tr_t18, omega_production_cost_asymmetry, theater_ratio, 18, 0.48).
narrative_ontology:measurement(omeg_tr_t24, omega_production_cost_asymmetry, theater_ratio, 24, 0.55).
narrative_ontology:measurement(omeg_tr_t30, omega_production_cost_asymmetry, theater_ratio, 30, 0.6).
narrative_ontology:measurement(omeg_tr_t36, omega_production_cost_asymmetry, theater_ratio, 36, 0.62).

% Extraction over time
narrative_ontology:measurement(omeg_be_t0, omega_production_cost_asymmetry, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(omeg_be_t6, omega_production_cost_asymmetry, base_extractiveness, 6, 0.08).
narrative_ontology:measurement(omeg_be_t12, omega_production_cost_asymmetry, base_extractiveness, 12, 0.1).
narrative_ontology:measurement(omeg_be_t18, omega_production_cost_asymmetry, base_extractiveness, 18, 0.11).
narrative_ontology:measurement(omeg_be_t24, omega_production_cost_asymmetry, base_extractiveness, 24, 0.13).
narrative_ontology:measurement(omeg_be_t30, omega_production_cost_asymmetry, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(omeg_be_t36, omega_production_cost_asymmetry, base_extractiveness, 36, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(omega_production_cost_asymmetry, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omega_production_cost_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(omega_production_cost_asymmetry, 0.02).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, declared_kill_condition_theater).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, adversarial_collaboration_procedural_capture).

% DUAL FORMULATION NOTE:
% This story authors the underlying structural/cognitive fact (cost asymmetry as mountain) rather than any specific institution's use of that fact. A sibling story, declared_kill_condition_theater, would decompose the instrumentalist kernel reading (per the CS recognition block) into its own constraint — the institutional practice of citing cheap-to-produce omega menus as evidence of rigor without matching revision behavior — with its own ε (substantially higher, since that story has identifiable beneficiaries and victims and requires active enforcement of the theatrical framing). Per the ε-invariance principle, these are not the same constraint measured two ways: the cost-asymmetry fact is near-zero extraction and mountain-like; the institutional exploitation of that fact is a distinct, more extractive claim and belongs in its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
