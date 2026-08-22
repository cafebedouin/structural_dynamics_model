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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Physical Church-Turing Thesis (physical-process computability claim)
 *   domain: philosophy_of_computation/theoretical_physics
 *
 * SUMMARY:
 *   This story isolates one of three structurally distinct readings collapsed
 *   under the label 'the Church-Turing thesis.' Under the physical-claim
 *   reading, the thesis asserts an empirical fact about the universe: no
 *   physical process — no matter how exotic the underlying physics — can
 *   compute a function outside the Turing-computable set. Unlike the
 *   mathematical-definition reading (a stipulative convention, ε near zero,
 *   no victims) and the epistemological-boundary reading (a claim about
 *   formal provability, not physical possibility), this reading makes a
 *   falsifiable claim about nature and therefore has real stakes: it
 *   structurally disadvantages researchers proposing physical mechanisms
 *   (exotic spacetimes, novel quantum-gravity effects, analog continuous
 *   computation) that might exceed Turing limits. The thesis functions as
 *   genuine scientific coordination (a stable shared assumption for
 *   complexity theory and CS pedagogy) while simultaneously gatekeeping a
 *   specific class of physically-motivated research as fringe, which is why
 *   this reading is authored as tangled_rope rather than pure mountain or
 *   pure snare.
 *
 * KEY AGENTS:
 *   - classical_computability_theorists: institutional beneficiaries of settled-thesis status
 *   - mainstream_computer_science_funding_bodies: agenda-setters who administer the gatekeeping via grants and review
 *   - hypercomputation_researchers: primary payers, career and funding cost
 *   - physical_superturing_proposal_authors: most exposed individual payers
 *   - analog_and_relativistic_computation_researchers: dual-positioned, benefit when compliant, pay when not
 *   - digital_physics_researchers: secondary beneficiaries via cultural authority transfer
 *   - theoretical_physicists_studying_quantum_gravity: analytical observers positioned to actually adjudicate the physical claim
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
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis (physical-process computability claim)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/theoretical_physics").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '7197c08d-6913-476e-a998-41e295188f77').
narrative_ontology:cs_kernel_codification('7197c08d-6913-476e-a998-41e295188f77', formalized).
narrative_ontology:cs_authority_grounding('7197c08d-6913-476e-a998-41e295188f77', expertise).
narrative_ontology:cs_interpretation_layer_present('7197c08d-6913-476e-a998-41e295188f77').
narrative_ontology:cs_reading_relation('7197c08d-6913-476e-a998-41e295188f77', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('7197c08d-6913-476e-a998-41e295188f77', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7197c08d-6913-476e-a998-41e295188f77', foundational, physical_reality_is_computationally_bounded_by_turing_limit).
narrative_ontology:cs_axiom_status(physical_reality_is_computationally_bounded_by_turing_limit, holdable).
narrative_ontology:cs_axiom_grounding('7197c08d-6913-476e-a998-41e295188f77', physical_reality_is_computationally_bounded_by_turing_limit, empirically_contingent).
narrative_ontology:cs_axiom('7197c08d-6913-476e-a998-41e295188f77', secondary, absence_of_counterexample_warrants_near_certainty).
narrative_ontology:cs_axiom_status(absence_of_counterexample_warrants_near_certainty, holdable).
narrative_ontology:cs_axiom_grounding('7197c08d-6913-476e-a998-41e295188f77', absence_of_counterexample_warrants_near_certainty, instrumental).
narrative_ontology:cs_reference_frame('7197c08d-6913-476e-a998-41e295188f77', turing_1936_effective_procedure_formalization).
narrative_ontology:cs_drift_state('7197c08d-6913-476e-a998-41e295188f77', post_quantum_computing_and_exotic_spacetime_proposals, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7197c08d-6913-476e-a998-41e295188f77', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computer_science_funding_bodies).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, digital_physics_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, physical_superturing_proposal_authors).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, analog_and_relativistic_computation_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, analog_and_relativistic_computation_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the dominant textbook and funding position built on the physical CT thesis. Every complexity-theoretic and computability result they teach and publish assumes no physical process exceeds Turing computability. They benefit from the thesis being treated as settled physics rather than an open empirical question, since their entire field's foundational claims and journal review norms are built on it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_computability_theorists, beneficiary,
    institutional, civilizational, arbitrage, global).

% Grant agencies, hiring committees, and journal editorial boards treat the physical CT thesis as a background assumption when evaluating proposals. They administer the norm by declining to fund or publish hypercomputation research as a live physical possibility, routing it instead to philosophy-of-science or crank-adjacent venues. They could revise this stance if compelling physical evidence emerged but bear none of the cost of the current gatekeeping themselves.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computer_science_funding_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Propose physical models (Malament-Hogarth spacetimes, infinite-time Turing machines instantiated via relativistic effects, closed timelike curves, certain quantum-gravity scenarios) that would compute non-Turing-computable functions if physically realizable. They face structural difficulty publishing in top venues, securing funding, or being taken seriously for tenure, because the physical CT thesis is treated as near-settled rather than as the open empirical claim it structurally is. Exit means leaving the research program or rebranding work as purely mathematical exploration disconnected from physical claims.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Individual physicists or mathematicians who propose specific physical mechanisms (e.g., certain black hole information scenarios, novel field theories) that could in principle exceed Turing computability. They are the most exposed: their specific claims can be falsified or dismissed on physical grounds, but the background assumption that no such mechanism could ever work colors peer review before the physics is even assessed.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physical_superturing_proposal_authors, payer,
    powerless, biographical, trapped, global).

% Study analog and continuous physical computation models that sit near the CT boundary. Some of their results are absorbed into mainstream complexity theory when they stay within Turing-computable bounds (a mild benefit of legitimacy), but work suggesting even modest super-Turing analog power is treated with heightened skepticism disproportionate to the physical evidence, because it threatens the physical CT thesis's status as settled.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, analog_and_relativistic_computation_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, analog_and_relativistic_computation_researchers, beneficiary).

% Proponents of the view that physical reality is fundamentally computational (e.g., cellular-automaton or digital-physics programs) benefit from the physical CT thesis's popularity, since it supports their broader claim that the universe itself is Turing-computable at bottom. The thesis's cultural authority lends their program credibility even though it is a distinct empirical claim.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, digital_physics_researchers, beneficiary,
    moderate, civilizational, mobile, global).

% Work on quantum gravity, black hole thermodynamics, and exotic spacetime structures where the physical CT thesis's truth is directly relevant but not their primary object of study. They can assess whether specific proposed spacetimes or physical mechanisms would violate the thesis, without having a stake in the outcome either way.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, theoretical_physicists_studying_quantum_gravity, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides computer science and physics with a stable, shared working assumption that lets researchers build complexity theory, algorithm design, and computational modeling on a common foundation without re-litigating the physical limits of computation in every paper.
% TRANSFER_FUNCTION: Moves funding, publication access, career legitimacy, and citation authority away from researchers pursuing physically-grounded super-Turing computation proposals and toward researchers whose work presupposes the thesis's truth as settled background.
% ABSENT_VOICES: Hypercomputation researchers and physical super-Turing proposal authors are structurally underrepresented on the grant panels and editorial boards that gatekeep their work; they would argue the thesis is unfalsified rather than falsified-as-false-alternative, and that treating it as near-certain forecloses inquiry prematurely. Their objections mostly surface in niche philosophy-of-computation venues rather than mainstream physics or CS venues.
% DISAPPEARANCE_RATIONALE: If the physical CT thesis's status as settled/near-settled were to disappear overnight (i.e., if it were widely treated as a genuinely open empirical question rather than established fact), mainstream computability theory and complexity theory would be structurally unaffected in their internal mathematics — but funding patterns, publication gatekeeping, and career viability for hypercomputation research would shift substantially. Whether the 'world rearranges' depends on which world you mean: the mathematics is untouched (favors world_unchanged); the sociology of who gets to do physically-motivated computation research would rearrange considerably (favors world_rearranges) — hence contested.
% FOUNDING_PROBLEM: The thesis was originally proposed (Church, Turing, 1936) to pin down an intuitive, pre-formal notion of 'effective procedure' or 'mechanical computation' so that results about decidability and computability could be stated precisely. The physical-claim reading extended this into an empirical conjecture about what nature itself permits, motivated by the observation that every known physical computing device and every proposed physical mechanism has so far respected Turing limits.
% FOUNDING_PROBLEM_CORROBORATION: Physicists working on exotic spacetime computation (e.g., researchers studying Malament-Hogarth spacetime proposals, published in venues like the British Journal for the Philosophy of Science and Studia Logica) attest that the physical claim remains genuinely open and empirically untested at the relevant regimes (extreme gravity, quantum gravity unification) — this corroboration comes from outside the beneficiary set of mainstream CS funding bodies and computability theorists, who instead treat the thesis as functionally settled for practical and pedagogical purposes.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) is moderate rather than high because the thesis has never been formally falsified and much of the disadvantage to hypercomputation researchers is reputational/funding rather than coercive suppression of publication outright. Suppression (0.55) is higher and has risen steadily (0.10 in 1936 to 0.55 in 2026) as the thesis calcified from a working hypothesis into a near-axiomatic assumption embedded in funding criteria, textbook framing, and peer-review defaults — this is the enforcement-intensification trajectory the temporal series is meant to capture. Theater ratio (0.28) is moderate: some invocations of the thesis in review contexts are genuine physical reasoning, but a growing share is pattern-matching ('this smells like hypercomputation, therefore crank') rather than engagement with the specific physical proposal. Accessibility collapse (0.4) is moderate-low because physically well-specified super-Turing proposals CAN still get a hearing in specialist venues — the alternative pathway has not fully closed, just narrowed. Resistance (0.6) is substantial: hypercomputation research persists as an active, if marginalized, subfield precisely because the physical claim is not settled.
 *
 * PERSPECTIVAL GAP:
 *   From the classical computability theorist's seat, the physical CT thesis looks like settled, load-bearing scientific consensus — a mountain, not a construct with victims. From the hypercomputation researcher's seat, the same thesis looks like an unfalsified conjecture wielded as gatekeeping machinery against a legitimate physical research question. The engine should compute these as genuinely different seat-level classifications from the same structural data, which is exactly the divergence this reading is authored to expose — I am not reconciling the claimed tangled_rope to either seat's local view.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical computability theorists and funding bodies sit near the beneficiary end: the thesis's settled status protects the foundational assumptions their careers and institutions are built on, and they bear essentially none of the cost of maintaining that status. Hypercomputation researchers and physical super-Turing proposal authors sit near the target end: they pay in funding denial, publication friction, and career risk for pursuing a research direction the thesis (as currently enforced) treats as near-impossible. Analog/relativistic researchers are genuinely dual-positioned — the derivation should not force them to one pole, hence the secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pinning down 'effective computability' for foundational mathematics) is largely dead in its original form — nobody today needs the thesis to define computability, since Turing machines, lambda calculus, and register machines are independently well-defined. What persists as 'live' is the physical extension, which was never definitively established and remains genuinely open per corroboration from quantum-gravity researchers outside the beneficiary set. Classifying this as tangled_rope (not scaffold, not mountain) prevents two mislabeling errors: treating it as pure natural law (mountain) would erase the real research-suppression cost to hypercomputation proponents; treating it as pure extraction (snare) would erase the genuine coordination value the thesis provides to the vast majority of computer science that has no stake in exotic physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_ct_thesis_truth_value,
    'Is the physical Church-Turing thesis actually true — does the universe in fact permit no physical process exceeding Turing computability — or is it an unfalsified working assumption mistaken for established fact?',
    'A confirmed physical mechanism (e.g., a experimentally verified Malament-Hogarth-type spacetime effect, or a quantum-gravity result demonstrating a concrete super-Turing computation) would falsify the thesis. Absent that, the thesis remains an extrapolation from ''no counterexample has been found'' rather than a proof.',
    'If false (a physical super-Turing mechanism is eventually confirmed), the current gatekeeping of hypercomputation research would be retroactively revealed as suppression of a correct research program — reclassifying this constraint toward snare. If true and eventually proven impossible in principle (e.g., via a deeper physical law), the constraint would shift toward mountain, since the coordination function would then rest on genuine natural law rather than an extrapolated empirical generalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physical_ct_thesis_truth_value, empirical, 'Whether the physical CT thesis is actually true, false, or presently undecidable given known physics.').

omega_variable(
    kernel_reading_conflation,
    'How much of the physical-claim reading''s apparent authority derives from illegitimate borrowing from the mathematical-definition reading''s genuinely settled status?',
    'Track citation and argument patterns in peer review and grant denials: do reviewers invoke the mathematically settled Church-Turing equivalence (Turing machines = lambda calculus = general recursive functions) as if it settles the physical question? This conflation would be directly observable in review language.',
    'If conflation is substantial, much of the suppression measured here (0.55) is illegitimately borrowed authority from a different, genuinely mountain-grade reading — meaning the physical-claim reading''s real, unborrowed epistemic standing is weaker than its practical gatekeeping power suggests, which would push the classification further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_conflation, conceptual, 'Whether the physical reading''s authority is partly borrowed illegitimately from the mathematical-definition reading''s settled status.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel here better framed as ''the Church-Turing thesis text'' (three readings of one historical formulation) or as ''the boundary-drawing authority of computability theory as a discipline'' (a broader institutional legitimacy claim that the physical reading is one instrument of)?',
    'Examine whether disciplinary gatekeeping persists even when the specific physical-claim wording is bracketed or disclaimed — if reviewers still reject hypercomputation proposals on ''this isn''t real computer science'' grounds absent explicit CT-thesis invocation, the broader institutional-authority framing is doing more work than the textual kernel framing.',
    'Under the narrower textual-kernel framing (adopted here), this constraint is CS-structured with authority_grounding practice/expertise; under the broader institutional-authority framing, the relevant kernel would be ''what counts as legitimate computer science'' with authority_grounding closer to extraction, and the classification would likely shift toward snare given the higher-stakes, harder-to-falsify institutional boundary being defended.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the CT-thesis text specifically or the broader disciplinary-legitimacy claim it instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement_basis(chur_tr_t1936, observed).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__physical_claim_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement_basis(chur_tr_t1970, observed).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__physical_claim_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(chur_tr_t1990, observed).
narrative_ontology:measurement(chur_tr_t2005, church_turing_thesis__physical_claim_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement_basis(chur_tr_t2005, observed).
narrative_ontology:measurement(chur_tr_t2015, church_turing_thesis__physical_claim_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(chur_tr_t2015, observed).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__physical_claim_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(chur_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.1).
narrative_ontology:measurement_basis(chur_be_t1936, observed).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__physical_claim_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement_basis(chur_be_t1970, observed).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__physical_claim_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(chur_be_t1990, observed).
narrative_ontology:measurement(chur_be_t2005, church_turing_thesis__physical_claim_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(chur_be_t2005, observed).
narrative_ontology:measurement(chur_be_t2015, church_turing_thesis__physical_claim_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement_basis(chur_be_t2015, observed).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__physical_claim_reading, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement_basis(chur_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.1).
narrative_ontology:measurement_basis(chur_su_t1936, observed).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__physical_claim_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement_basis(chur_su_t1970, observed).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__physical_claim_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(chur_su_t1990, observed).
narrative_ontology:measurement(chur_su_t2005, church_turing_thesis__physical_claim_reading, suppression_requirement, 2005, 0.46).
narrative_ontology:measurement_basis(chur_su_t2005, observed).
narrative_ontology:measurement(chur_su_t2015, church_turing_thesis__physical_claim_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement_basis(chur_su_t2015, observed).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__physical_claim_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(chur_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'the Church-Turing thesis' per the ε-invariance principle. The mathematical_definition_reading has ε near zero (a stipulative convention, no empirical stakes, no victims). The epistemological_boundary_reading concerns the limits of formal provability about computability, independent of physical realizability, and has its own distinct victim set (researchers whose informal computability arguments lack rigorous encodings) and ε profile. This physical_claim_reading has the highest ε of the three because it alone makes a falsifiable claim about nature with an identifiable victim set (hypercomputation and physical super-Turing researchers). All three should be read as siblings under one kernel, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
