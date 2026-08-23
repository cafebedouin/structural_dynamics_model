% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Physical Church-Turing Thesis: Empirical Bound on Physical Computation
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   The physical Church-Turing thesis asserts that no physical process
 *   computes functions beyond Turing-machine computability. Unlike its
 *   definitional and epistemological siblings, this reading is an empirical
 *   claim about the universe — and it has never been proven or refuted. Its
 *   operative form in the research world is a community-wide treatment of the
 *   bound as settled: complexity-theoretic physical-relevance claims,
 *   cryptographic adversary models, and review practice all presuppose it,
 *   while the small community probing its possible failure bears systematic
 *   career and funding costs. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope from its structure (a genuine,
 *   heavily-used coordination foundation plus an asymmetrically burdened
 *   minority program plus active gatekeeping), while the metrics are authored
 *   descriptively of its actual operation — the engine measures any
 *   divergence; the claim is not tuned to predicted output. This file
 *   instantiates ONE reading of the church_turing_thesis kernel; the
 *   definitional and epistemological readings are separate constraints linked
 *   via network.affects_constraints, per the epsilon-invariance decomposition
 *   rule.
 *
 * KEY AGENTS:
 *   - hypercomputation_researchers: Primary target (powerless/identity_locked) — bears the arrangement's career, funding, and publication costs
 *   - exotic_physical_computation_proposers: Secondary target (moderate/constrained) — physicists whose relativity and quantum-foundations results brush the boundary
 *   - turing_framework_computer_science: Primary beneficiary (institutional/constrained) — the discipline whose shared foundation the bound secures
 *   - complexity_theory_researchers and cryptographic_protocol_designers: Secondary beneficiaries (organized-to-institutional/constrained)
 *   - computational_gatekeeping_institutions: Agenda-setter (institutional/arbitrage) — administers the boundary through review and funding decisions
 *   - quantum_supremacy_experimentalists: Conditional seat (institutional/mobile) — sheltered inside the bound today, first exposed if it moves
 *   - experimental_hypercomputation_builders: Excluded voice (powerless/trapped) — would test the boundary empirically but has no seat
 *   - philosophy_of_computation_analysts: Analytical observer (analytical/analytical) — documents the claim's unproven status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.48).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis: Empirical Bound on Physical Computation").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '15c6aa09-0a3e-4891-b9ad-463d12bfc4c5').
narrative_ontology:cs_kernel_codification('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', formalized).
narrative_ontology:cs_authority_grounding('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', expertise).
narrative_ontology:cs_interpretation_layer_present('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5').
narrative_ontology:cs_reading_relation('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', foundational, physical_processes_turing_bounded).
narrative_ontology:cs_axiom_status(physical_processes_turing_bounded, holdable).
narrative_ontology:cs_axiom_grounding('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', physical_processes_turing_bounded, empirically_contingent).
narrative_ontology:cs_axiom('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', secondary, computability_boundary_is_testable_not_stipulated).
narrative_ontology:cs_axiom_status(computability_boundary_is_testable_not_stipulated, holdable).
narrative_ontology:cs_axiom_grounding('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', computability_boundary_is_testable_not_stipulated, instrumental).
narrative_ontology:cs_reference_frame('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', empirical_universal_generalization).
narrative_ontology:cs_drift_state('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', post_quantum_supremacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15c6aa09-0a3e-4891-b9ad-463d12bfc4c5', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, turing_framework_computer_science).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, complexity_theory_researchers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, cryptographic_protocol_designers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, exotic_physical_computation_proposers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_supremacy_experimentalists).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_experimentalists).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, gandy_machine_boundedness_theorem).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, deutsch_church_turing_principle).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_adequacy_for_physical_devices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The international discipline of computer science organizes its curricula, textbooks, and result-claims around the Turing-machine model. Every subfield inherits the assurance that physical devices implement Turing-computable functions, which lets theorems, compilers, and security arguments compose across areas. Members experience the framework as simply what computation is; leaving it would mean rebuilding the field's shared language.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, turing_framework_computer_science, beneficiary,
    institutional, generational, constrained, global).

% Define hardness classes, reductions, and completeness results relative to Turing-bounded machine models. Their physical-relevance statements ('no efficient algorithm exists for X') presuppose that no physical device beats the model. A demonstrated device computing beyond the model would force re-examination of decades of such statements, though the abstract mathematics would survive untouched.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, complexity_theory_researchers, beneficiary,
    organized, generational, constrained, global).

% Design protocols whose security arguments assume adversaries are Turing-bounded algorithms, converting 'unbreakable' into a precise statement. If physical processes could compute non-recursive functions, standard assumptions would need rederivation against a larger adversary class.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, cryptographic_protocol_designers, beneficiary,
    institutional, biographical, constrained, global).

% Journal editors, program committees, and funding panels decide which computation papers and grants circulate. Submissions claiming physical processes that compute beyond the Turing model are typically screened out as speculative or erroneous, with the thesis cited as settled background rather than examined on its physical merits. The reviewers themselves are not bound by the screening rules they apply.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computational_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% A small, scattered community studies whether any physical process — analog devices with unlimited precision, relativistic spacetimes permitting infinite observation time, quantum-gravitational regimes — could compute functions no Turing machine computes. Their papers face rejection rates far above field norms, grant applications rarely survive panel review, and graduate students are advised away from the topic. Leaving means abandoning the research identity they trained into; staying means career precarity.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    powerless, biographical, identity_locked, global).

% Physicists, mostly in relativity and quantum foundations, who point out that known solutions of general relativity (spacetimes with infinite proper-time compression) or idealized quantum measurements appear to permit non-Turing computation. They publish mainly in physics venues, frame proposals carefully to avoid the 'hypercomputation' label, and can redirect to mainstream questions in their fields at the cost of dropping the computational claim.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, exotic_physical_computation_proposers, payer,
    moderate, biographical, constrained, global).

% Large quantum-hardware groups demonstrate sampling tasks intractable for classical machines. Their claims are defined and verified inside Turing-computable territory — quantum circuits compute recursive functions — so the framework currently shelters their priority disputes. If any future hardware regime appeared to compute beyond the model, their verification methods and claims would be the first casualties; they monitor the boundary while benefiting from its current placement.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_experimentalists, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, quantum_supremacy_experimentalists, payer).

% Engineers and experimentalists who would attempt bench-top tests of non-Turing computation proposals if any venue funded them. No program exists; proposals die at the whiteboard stage because review treats the underlying question as already answered. They have no seat in the conversations that set computational orthodoxy.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, experimental_hypercomputation_builders, excluded,
    powerless, immediate, trapped, global).

% Philosophers of computer science and logic who catalog what the thesis asserts, distinguish its definitional, epistemic, and physical formulations, and document that neither proof nor refutation of the physical version exists. They publish analyses of the boundary's status but set no research agendas.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_of_computation_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, turing_framework_computer_science).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single closed answer to 'what can be computed' that every subfield of computer science builds on: results proved for Turing machines transfer to physical devices, so theorems, compilers, protocols, and verification arguments compose without each area re-deriving the physical reach of computation.
% TRANSFER_FUNCTION: Moves epistemic closure and legitimacy toward Turing-framework programs: publication space, grant funds, graduate-student labor, and citation flow route to model-internal work, while proposals touching non-Turing physical computation lose access to those resources.
% ABSENT_VOICES: Experimental builders who would test candidate non-Turing processes have no seat — no funding stream or journal section exists for them. Physicists outside the Turing frame (analog and continuous-computation traditions) are heard only when they translate into discrete models. Both would object that the boundary was declared settled without a decisive physical test.
% DISAPPEARANCE_RATIONALE: If the community overnight stopped treating Turing-computability as the bound of physical computation, physical-relevance claims across complexity theory would need requalification, cryptographic adversary models would widen, and a large experimental program to test candidate non-Turing processes would mobilize; textbooks and curricula would revise. The rearrangement is epistemic rather than infrastructural — no bridges fall — but the research world reorganizes around a reopened question.
% FOUNDING_PROBLEM: Hilbert's program left a precise question — which number-theoretic functions are effectively calculable — answered formally by Church and Turing in 1936. The physical variant asks whether the universe's actual processes respect that formal answer: whether every finitely realizable physical procedure computes only Turing-computable functions (formalized by Gandy in 1980; restated for quantum physics by Deutsch in 1985).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the general-relativity literature (Etesi and Nemeti's construction of Malament-Hogarth spacetimes, published in physics venues) attests that the question of physical non-Turing computation arises from physics itself, not from the discipline's internal needs; philosophical surveys (Copeland, Ord) independently document that neither proof nor refutation exists; and hypercomputation researchers — who bear the arrangement's costs rather than collect its benefits — attest the problem is live. No corroboration rests on the benefiting parties alone.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the direct costs concentrate on a tiny research community (systematic funding denial, above-norm rejection rates, advised-away students), while a diffuse epistemic cost — a possibility space closed by consensus rather than by evidence — falls on everyone. Suppression (0.58) is real but non-coercive: nothing is banned, yet gatekeeping reliably filters boundary-touching work out of mainstream venues and funding streams. Theater ratio (0.28) reflects a growing share of ritual thesis-invocations — papers and textbooks citing the bound as established fact without examining its physical content — alongside a core bounding function that still does daily work. Accessibility collapse (0.40): alternatives remain reachable (hypercomputation analyses do get published in peripheral venues) but at heavy reputational cost. Resistance (0.50): persistent counter-pressure from relativity-based constructions, Pour-El-Richards analog results, and ongoing philosophical critique. The temporal series run on ONE shared grid (T=0..90 at steps of 15) with every tracked metric authored at every point; the suppression_requirement series is authored because the story specifically traces enforcement-capacity change — gatekeeping machinery maturing from loose mid-century consensus into institutionalized review practice — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Three seats experience structurally different arrangements. From the hypercomputation researcher's position the bound operates as a closed door: an orthodoxy that ends careers and deflects funding without engaging the physics. From the mainstream discipline's position the same structure is the floor everything stands on — a settled foundation with no experienced cost. From the gatekeeper's position it is ordinary quality control: rejecting speculation. The engine computes these divergent per-seat classifications from the power, exit, and role data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: the discipline, complexity theorists, and cryptographers collect closure and composability while paying almost nothing (damped chi). Payer declarations map to high-directionality seats: hypercomputation researchers sit near the full-target end, amplified by identity_locked exit — their professional selves are fused with the question the arrangement forecloses. Exotic proposers carry high d but their constrained-but-real mobility dampens effective extraction. Gatekeepers derive near-beneficiary directionality with arbitrage exit — they set the terms they administer. Quantum supremacy experimentalists sit near symmetric: sheltered beneficiaries now, conditional payers on any boundary collision. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already produce the correct relationships, and the schema's power-atom-keyed override mechanism cannot cleanly separate the multiple institutional seats with different roles.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — does physics exceed formal computability — is live, corroborated from outside the benefiting parties, and the mismatch consumer reads status=live x verdict=world_rearranges as consistent (no zombie flag). The classification prevents mislabeling in both directions: reading the bound as a mountain (a natural law) would erase the identifiable payers and the enforcement machinery that sustains the arrangement; reading it as a snare would erase the genuine, universally-used coordination function that every subfield of computer science consumes daily. Tangled rope holds both facts: real coordination, real asymmetric cost, active enforcement holding the joint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Does the operative constraint on research practice correspond to this physical reading, or to one of the sibling readings (definitional or epistemological)?',
    'Author the sibling stories and compare per-seat classifications and epsilon across the family; the reading whose structural data reproduce the observed gatekeeping and payer costs is the operative one.',
    'If the definitional reading is operative, epsilon collapses toward zero (conventions have no victims) and the payer set identified here dissolves; if the epistemological reading, the victim set shifts to proof-seeking programs and the physical-test question drops out entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'Which reading of the CT kernel the practiced constraint actually instantiates.').

omega_variable(
    physical_truth_value,
    'Is the thesis true of our universe — does any physically realizable process compute a non-recursive function?',
    'Either a completed fundamental physics yielding a Gandy-style boundedness theorem from accepted laws, or observation of computation in a regime (Malament-Hogarth spacetime, unlimited-precision analog measurement) realizing a non-recursive function.',
    'If true, the arrangement loses its payers (they are chasing a genuine impossibility) and trends toward pure coordination; if false, the arrangement is suppressing a real capability and hardens toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physical_truth_value, empirical, 'Truth-value of the physical CT claim, undecided by current physics.').

omega_variable(
    victim_status_vs_risk_selection,
    'Are hypercomputation researchers genuine payers of the arrangement, or volunteers bearing ordinary speculative-research risk?',
    'Matched-proposal audit: resubmit identical proposals with the hypercomputation framing stripped and compare review outcomes against field baselines.',
    'If outcomes match baseline, measured extraction is overstated and the structure trends rope; if systematically penalized beyond risk, the payer designation stands and strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_vs_risk_selection, empirical, 'Whether observed career costs exceed ordinary speculative-science risk.').

omega_variable(
    quantum_boundary_collision,
    'Will any quantum-supremacy-class claim ever require computing beyond Turing-computable functions, rather than merely beating classical complexity?',
    'Track the verifiability literature on sampling tasks; watch for hardware regimes invoking beyond-standard-model or gravitational effects whose claimed outputs are non-recursive.',
    'On collision, the payer set expands from a marginal community to flagship industrial programs and epsilon rises sharply; absent collision, the conditional-payer seat stays dormant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_boundary_collision, empirical, 'Conditional expansion of the victim set via quantum supremacy claims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (journal and funding gatekeeping) or internalized (self-censorship instilled by training)?',
    'Post-gatekeeping trajectory: if submission rates to newly receptive interdisciplinary venues stay low after formal barriers drop, the internalized component dominates.',
    'Internalized suppression raises effective suppression above the structural measure and persists after institutional reform; purely structural suppression would fall quickly if gatekeeping relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ct_physical_reading_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t0, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t15, church_turing_thesis__physical_claim_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t15, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t30, church_turing_thesis__physical_claim_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t30, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t45, church_turing_thesis__physical_claim_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t45, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t60, church_turing_thesis__physical_claim_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t60, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t75, church_turing_thesis__physical_claim_reading, theater_ratio, 75, 0.27).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t75, observed).
narrative_ontology:measurement(ct_physical_reading_tr_t90, church_turing_thesis__physical_claim_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(ct_physical_reading_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(ct_physical_reading_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(ct_physical_reading_be_t0, observed).
narrative_ontology:measurement(ct_physical_reading_be_t15, church_turing_thesis__physical_claim_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement_basis(ct_physical_reading_be_t15, observed).
narrative_ontology:measurement(ct_physical_reading_be_t30, church_turing_thesis__physical_claim_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(ct_physical_reading_be_t30, observed).
narrative_ontology:measurement(ct_physical_reading_be_t45, church_turing_thesis__physical_claim_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement_basis(ct_physical_reading_be_t45, observed).
narrative_ontology:measurement(ct_physical_reading_be_t60, church_turing_thesis__physical_claim_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement_basis(ct_physical_reading_be_t60, observed).
narrative_ontology:measurement(ct_physical_reading_be_t75, church_turing_thesis__physical_claim_reading, base_extractiveness, 75, 0.49).
narrative_ontology:measurement_basis(ct_physical_reading_be_t75, observed).
narrative_ontology:measurement(ct_physical_reading_be_t90, church_turing_thesis__physical_claim_reading, base_extractiveness, 90, 0.48).
narrative_ontology:measurement_basis(ct_physical_reading_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(ct_physical_reading_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(ct_physical_reading_su_t0, observed).
narrative_ontology:measurement(ct_physical_reading_su_t15, church_turing_thesis__physical_claim_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement_basis(ct_physical_reading_su_t15, observed).
narrative_ontology:measurement(ct_physical_reading_su_t30, church_turing_thesis__physical_claim_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(ct_physical_reading_su_t30, observed).
narrative_ontology:measurement(ct_physical_reading_su_t45, church_turing_thesis__physical_claim_reading, suppression_requirement, 45, 0.4).
narrative_ontology:measurement_basis(ct_physical_reading_su_t45, observed).
narrative_ontology:measurement(ct_physical_reading_su_t60, church_turing_thesis__physical_claim_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(ct_physical_reading_su_t60, observed).
narrative_ontology:measurement(ct_physical_reading_su_t75, church_turing_thesis__physical_claim_reading, suppression_requirement, 75, 0.54).
narrative_ontology:measurement_basis(ct_physical_reading_su_t75, observed).
narrative_ontology:measurement(ct_physical_reading_su_t90, church_turing_thesis__physical_claim_reading, suppression_requirement, 90, 0.58).
narrative_ontology:measurement_basis(ct_physical_reading_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Church-Turing thesis' per the epsilon-invariance principle. The label conflates three structurally distinct claims: a stipulative definition (true by convention, epsilon approximately zero, no victims), an epistemic boundary (about provability, independent of physics), and this physical empirical claim (testable, contested, with a victim set and enforcement surface). Each member gets its own epsilon, stakeholders, and classification; this file is the physical reading. The upstream members (definitional, epistemological) are cited as settled background by the enforcement this story documents, so edges run from this reading to both siblings: its empirical fate changes their reception conditions without foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
