% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__physical_claim_reading, []).

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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis as Physical Claim: No Physical Process Exceeds Turing-Computability
 *   domain: philosophy_of_mathematics/philosophy_of_physics/foundations_of_computer_science
 *
 * SUMMARY:
 *   This story isolates the PHYSICAL-CLAIM reading of the Church-Turing
 *   thesis: the empirical assertion that no physical process, however exotic,
 *   can compute a function that is not Turing-computable. This is distinct
 *   from the mathematical-definition reading (a stipulative convention true
 *   by fiat, ε near zero, no victims) and the epistemological-boundary
 *   reading (a claim about what is formally provable computable, agnostic
 *   about physical possibility). Under the physical-claim reading, the thesis
 *   functions as a load-bearing background assumption for funding and
 *   publication decisions across computer science and physics, despite having
 *   the epistemic status of an untested (not disproven, but also not
 *   physically derived) generalization. The gap between the thesis's
 *   treatment as settled fact and its actual empirical status is what
 *   produces the tangled-rope structure: genuine coordination value for the
 *   vast majority of computability-dependent work, combined with asymmetric
 *   costs borne by the small community actively investigating whether the
 *   physical claim is true.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.42).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Claim: No Physical Process Exceeds Turing-Computability").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics/philosophy_of_physics/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'e66e166a-2008-4bcd-9dd9-81a310be8494').
narrative_ontology:cs_kernel_codification('e66e166a-2008-4bcd-9dd9-81a310be8494', distributed).
narrative_ontology:cs_authority_grounding('e66e166a-2008-4bcd-9dd9-81a310be8494', expertise).
narrative_ontology:cs_interpretation_layer_present('e66e166a-2008-4bcd-9dd9-81a310be8494').
narrative_ontology:cs_reading_relation('e66e166a-2008-4bcd-9dd9-81a310be8494', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('e66e166a-2008-4bcd-9dd9-81a310be8494', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('e66e166a-2008-4bcd-9dd9-81a310be8494', foundational, physical_reality_is_bounded_by_turing_computability).
narrative_ontology:cs_axiom_status(physical_reality_is_bounded_by_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('e66e166a-2008-4bcd-9dd9-81a310be8494', physical_reality_is_bounded_by_turing_computability, empirically_contingent).
narrative_ontology:cs_axiom('e66e166a-2008-4bcd-9dd9-81a310be8494', secondary, absence_of_counterexample_constitutes_confirmation).
narrative_ontology:cs_axiom_status(absence_of_counterexample_constitutes_confirmation, holdable).
narrative_ontology:cs_axiom_grounding('e66e166a-2008-4bcd-9dd9-81a310be8494', absence_of_counterexample_constitutes_confirmation, empirically_contingent).
narrative_ontology:cs_reference_frame('e66e166a-2008-4bcd-9dd9-81a310be8494', physical_church_turing_as_confirmed_law).
narrative_ontology:cs_drift_state('e66e166a-2008-4bcd-9dd9-81a310be8494', post_quantum_computing_and_exotic_spacetime_proposals_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e66e166a-2008-4bcd-9dd9-81a310be8494', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_complexity_theory_establishment).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, funding_agencies_evaluating_computation_proposals).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, physical_super_turing_proposal_authors).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, exotic_quantum_gravity_computation_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, exotic_quantum_gravity_computation_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold tenured positions, textbooks, and curricula built on the Church-Turing thesis as settled physical fact. Their research programs, journal gatekeeping roles, and grant review authority all presuppose the thesis's physical truth. They benefit from the thesis functioning as a closed question — it stabilizes an entire field's foundations and their standing within it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computability_theorists, beneficiary,
    institutional, civilizational, arbitrage, global).

% Sets peer-review standards, conference program committees, and hiring criteria that treat the physical Church-Turing thesis as a background axiom rather than an open empirical question. Papers proposing physical hypercomputation are typically routed to reject or to physics venues skeptical of the framing, reinforcing the thesis's status as settled.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_complexity_theory_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, classical_complexity_theory_establishment, agenda_setter).

% Propose or investigate physical mechanisms (Malament-Hogarth spacetimes, exotic relativistic computers, certain quantum gravity constructions) that would, if realized, compute beyond Turing-computability. They struggle to publish in top venues, face funding rejection framed as 'physically impossible' rather than 'physically unverified,' and often must relabel work as purely mathematical exploration to survive peer review. Exit means abandoning the research program or working in obscurity.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, national).

% Early-career or independent researchers who submit specific physical models attempting to exceed Turing-computability. They bear reputational risk for challenging what is treated as established fact; their work is frequently dismissed without engagement with the specific physical claim, on the grounds that the thesis is 'known' rather than empirically open.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physical_super_turing_proposal_authors, payer,
    powerless, biographical, trapped, national).

% Explore whether quantum gravitational effects (e.g. at black hole horizons or in exotic causal structures) could permit computation beyond Turing limits. They benefit from legitimate physics funding when framed conservatively, but pay a cost whenever their work's implications for the thesis are foregrounded — reviewers often demand the thesis be treated as inviolable background rather than as the actual object under test.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, exotic_quantum_gravity_computation_researchers, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, exotic_quantum_gravity_computation_researchers, beneficiary).

% Allocate research funding using review panels that treat the physical Church-Turing thesis as settled. This shapes which proposals are fundable — foundational hypercomputation proposals are systematically deprioritized relative to work that assumes the thesis rather than tests it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, funding_agencies_evaluating_computation_proposals, agenda_setter,
    institutional, generational, analytical, national).

% Analyze whether the thesis, as an empirical claim, has actually been tested or merely presupposed. They note the thesis has near-universal informal acceptance among physicists despite no experiment ever having directly falsified a hypercomputation proposal at scale, and observe the gap between the thesis's treatment as physical law and its actual epistemic status as an untested (and perhaps untestable in practice) generalization.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_physics_and_computation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Treating the Church-Turing thesis as physically true lets the computer science and physics communities share a stable foundational assumption: computability theory, complexity theory, and the design of physical computers can all proceed without re-litigating whether any given physical process might compute non-recursive functions. This genuinely coordinates enormous amounts of downstream work that would be paralyzed by permanent foundational doubt.
% TRANSFER_FUNCTION: The arrangement moves research legitimacy, funding priority, and publication access away from researchers pursuing physical hypercomputation proposals and toward researchers whose work presupposes the thesis's physical truth. It also moves the burden of proof: challengers must overturn a presumption of settledness rather than the mainstream needing to actively defend the claim against live empirical contestation.
% ABSENT_VOICES: Physicists working on genuinely exotic causal structures (rotating black holes, closed timelike curve models, certain quantum gravity regimes) whose proposals bear directly on the thesis's truth are frequently absent from computability-theory venues where the thesis's status is discussed, and computability theorists are frequently absent from the physics venues where such exotic models are seriously entertained — the two communities that would need to jointly adjudicate the physical claim rarely occupy the same review process.
% DISAPPEARANCE_RATIONALE: If the physical-claim reading of the thesis were abandoned tomorrow (i.e., the field openly treated it as a live empirical question rather than settled fact), mainstream computability theory and complexity theory would be largely unaffected in their day-to-day mathematics, since that content depends on the mathematical-definition reading, not this one. But funding panels, hiring committees, and publication gatekeeping that currently use the physical claim as an unstated axiom would have to re-open a body of decisions; hypercomputation researchers argue the world would rearrange substantially in their favor, while mainstream theorists argue almost nothing of substance would change because no credible physical mechanism has yet been demonstrated.
% FOUNDING_PROBLEM: Turing, Church, and contemporaries needed a rigorous, shared notion of 'effective procedure' to settle the Entscheidungsproblem and related foundational questions in logic; the physical-claim extension arose later as a way of asserting that this mathematical notion also bounds what any physically realizable device — brain, computer, or exotic future machine — could ever compute.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream computability theorists and complexity theorists (the benefiting parties) attest the physical claim remains live and essentially settled by seven decades of failed counterexamples. Independent corroboration from outside that group is thinner than often assumed: philosophers of physics (Piccinini, Copeland) and some quantum-gravity theorists explicitly note the physical claim has never been derived from physical law and remains an extrapolation from the absence of a demonstrated counterexample rather than a proven impossibility — a status closer to a strong inductive generalization than to a physical law like conservation of energy. No experimental physics body has certified the physical-claim reading as tested and confirmed in the way a physical law is normally certified.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).
:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the primary harm is opportunity-cost — foreclosed funding, foreclosed publication venues, foreclosed career paths — rather than direct material extraction; it rises modestly over the measured interval as quantum computing's mainstream success has, paradoxically, hardened rather than loosened the presumption that all physical computation reduces to Turing-equivalent power. Suppression (0.55) reflects that the thesis functions less as an actively policed rule and more as an unstated axiom that reviewers and funders apply without treating it as contestable — this is suppression through unquestioned presumption rather than through explicit prohibition, which is why it sits at moderate rather than high. Theater ratio is comparatively low (0.28) because most of the activity built on the thesis (complexity theory, algorithm design, hardware architecture) is genuinely functional, not performative; the theater component is the portion of review and gatekeeping language that invokes the thesis as settled physical law when it has not actually been tested against the most exotic candidate physical processes.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the physical Church-Turing thesis functions as settled coordination infrastructure — a mountain-like background fact that simply IS the shape of physical computation, requiring no defense because no serious challenge exists. From the payer seats, the same arrangement functions as an actively enforced tangled rope: a genuine coordination function (shared foundational assumptions) riding on top of asymmetric costs imposed on a specific, identifiable research population whose work is judged against a presumption rather than a demonstrated impossibility. The engine's per-seat computation should reflect this: institutional beneficiary seats likely compute near mountain/rope, powerless payer seats likely compute near snare or tangled_rope, and this divergence is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computability theorists, the complexity-theory establishment, and funding agencies are structural beneficiaries: the thesis-as-settled-fact stabilizes their entire research and evaluative apparatus, so their directionality sits near the beneficiary end. Hypercomputation researchers, physical super-Turing proposal authors, and exotic quantum-gravity computation researchers are structural targets: the presumption of settledness is exactly what forecloses their work's legitimacy, so their directionality sits near the target end, amplified for the powerless independent researchers who have no institutional buffer and dampened somewhat for the quantum-gravity researchers who retain adjacent legitimate funding channels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a rigorous shared notion of effective computability to make foundational logic tractable — remains genuinely live for the mathematical-definition reading, which is why that sibling reading is closer to a rope or mountain. But the PHYSICAL extension of that mathematical convention into a claim about all possible physical processes was a later addition whose empirical status has never been definitively resolved either way. Treating the physical claim as though its founding problem is as settled as the mathematical one — when the physical claim has a much thinner evidentiary basis — is exactly the mandatrophy risk this story flags: an arrangement whose justification (settled physical law) has drifted from what was actually established (a strong but untested inductive generalization), while continuing to exercise the same gatekeeping authority as if it were fully vindicated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirically_tested_vs_definitionally_true,
    'Is the physical Church-Turing thesis an empirically tested and confirmed claim about physical law, or is it a background assumption inherited from the mathematical-definition reading and never independently subjected to physical test?',
    'A systematic survey of proposed physical hypercomputation mechanisms (Malament-Hogarth spacetimes, exotic quantum gravity models, analog computation with unbounded precision) assessing whether any has been definitively ruled out by experiment or observation, versus merely deemed implausible by consensus.',
    'If the thesis has genuinely been tested and no counterexample survives scrutiny, the constraint is closer to a rope with a real coordination function and low residual extraction. If it has never been tested and merely inherited authority from the settled mathematical convention, the constraint is closer to a snare — enforcing a presumption as if it were a proven physical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirically_tested_vs_definitionally_true, empirical, 'Whether the physical claim has actual empirical confirmation or borrowed authority from the mathematical convention.').

omega_variable(
    kernel_reading_conflation_in_practice,
    'When funding panels and reviewers invoke ''the Church-Turing thesis'' to reject a hypercomputation proposal, are they actually invoking the physical-claim reading, or are they conflating it with the much better-supported mathematical-definition or epistemological-boundary readings?',
    'Discourse analysis of grant rejection letters and peer review comments citing the thesis, checking whether the specific physical mechanism proposed is engaged with on its own terms or dismissed via appeal to the thesis''s general authority.',
    'If conflation is common, much of the measured suppression in THIS reading is actually borrowed legitimacy from the siblings'' stronger epistemic status — meaning the physical-claim reading''s institutional authority is partly a category error, and disambiguating the readings in review practice would reduce the suppression this story measures without requiring any new physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_conflation_in_practice, conceptual, 'Whether institutional enforcement conflates this reading''s weaker empirical status with the siblings'' stronger ones.').

omega_variable(
    victim_set_growth_with_quantum_computing,
    'Does the empirical and commercial success of quantum computing (which remains within Turing-equivalent power, merely with different complexity characteristics) strengthen or weaken the case for the physical-claim reading?',
    'Track whether quantum computing''s success is cited in review and funding contexts as evidence FOR the thesis (no exotic physical process has yet exceeded Turing power) or is understood as orthogonal to the thesis (quantum speedup is a complexity phenomenon, not a computability phenomenon).',
    'If quantum computing''s success is being miscited as confirming the physical-claim thesis when it is actually orthogonal to computability class, the suppression measured here is being reinforced by a conceptual error, which would be independently correctable and would reduce the victim class''s structural disadvantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_growth_with_quantum_computing, conceptual, 'Whether quantum computing progress is being incorrectly cited as evidence for the physical Church-Turing thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__physical_claim_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__physical_claim_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__physical_claim_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__physical_claim_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(chur_tr_t70, church_turing_thesis__physical_claim_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__physical_claim_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__physical_claim_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__physical_claim_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__physical_claim_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(chur_be_t70, church_turing_thesis__physical_claim_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__physical_claim_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__physical_claim_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__physical_claim_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__physical_claim_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(chur_su_t70, church_turing_thesis__physical_claim_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the church_turing_thesis kernel. The mathematical_definition_reading (a stipulative convention, ε near zero, no coercive apparatus, essentially a rope or mountain) is upstream of this reading in the sense that its settled status lends borrowed authority to the physical claim, even though the two are logically independent. The epistemological_boundary_reading (a claim about formal provability, agnostic to physical possibility) shares vocabulary with this reading but has a different, much smaller victim set (formal logicians debating proof-theoretic boundaries rather than physicists and hypercomputation researchers). All three readings should be read together to see how a single colloquial label ('the Church-Turing thesis') hides three structurally distinct claims with three different ε values and three different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
