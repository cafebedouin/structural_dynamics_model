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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis as Physical Claim: No Physical Process Exceeds Turing Computability
 *   domain: philosophy_of_mathematics/philosophy_of_computation/theoretical_physics
 *
 * SUMMARY:
 *   The Church-Turing thesis, read as a physical claim, asserts that the
 *   universe itself contains no process capable of computing functions
 *   outside the Turing-computable set. This reading has become
 *   institutionally load-bearing: computability theory, complexity theory,
 *   and the entire digital computing industry proceed as though this
 *   empirical claim were established fact, even though it has never been
 *   proven and remains, in principle, falsifiable by a single robust
 *   counterexample (a physical process that reliably computes a
 *   non-Turing-computable function). The coordination function is real — a
 *   shared boundary condition lets disparate fields build cumulative theory
 *   without re-litigating foundations. But the same boundary condition is
 *   enforced well beyond what the evidence supports, and it does real work
 *   suppressing a specific research population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.42).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Claim: No Physical Process Exceeds Turing Computability").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics/philosophy_of_computation/theoretical_physics").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '7ed6b235-0ddf-4d61-892d-2f638d24fd09').
narrative_ontology:cs_kernel_codification('7ed6b235-0ddf-4d61-892d-2f638d24fd09', formalized).
narrative_ontology:cs_authority_grounding('7ed6b235-0ddf-4d61-892d-2f638d24fd09', expertise).
narrative_ontology:cs_interpretation_layer_present('7ed6b235-0ddf-4d61-892d-2f638d24fd09').
narrative_ontology:cs_reading_relation('7ed6b235-0ddf-4d61-892d-2f638d24fd09', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('7ed6b235-0ddf-4d61-892d-2f638d24fd09', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7ed6b235-0ddf-4d61-892d-2f638d24fd09', foundational, physical_universe_is_turing_bounded).
narrative_ontology:cs_axiom_status(physical_universe_is_turing_bounded, holdable).
narrative_ontology:cs_axiom_grounding('7ed6b235-0ddf-4d61-892d-2f638d24fd09', physical_universe_is_turing_bounded, empirically_contingent).
narrative_ontology:cs_axiom('7ed6b235-0ddf-4d61-892d-2f638d24fd09', secondary, absence_of_counterexample_warrants_institutional_closure).
narrative_ontology:cs_axiom_status(absence_of_counterexample_warrants_institutional_closure, holdable).
narrative_ontology:cs_axiom_grounding('7ed6b235-0ddf-4d61-892d-2f638d24fd09', absence_of_counterexample_warrants_institutional_closure, conventional).
narrative_ontology:cs_reference_frame('7ed6b235-0ddf-4d61-892d-2f638d24fd09', turing_1936_formal_equivalence_result).
narrative_ontology:cs_drift_state('7ed6b235-0ddf-4d61-892d-2f638d24fd09', contemporary_hypercomputation_and_quantum_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ed6b235-0ddf-4d61-892d-2f638d24fd09', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, digital_computing_industry).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, complexity_theory_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_overclaim_challengers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, analog_and_exotic_computation_researchers).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_machine_model_universality).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, digital_computer_foundational_adequacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built an entire research program, textbook canon, and funding structure on the premise that Turing computability is the correct boundary of physically realizable computation. Grant panels, journal editors, and hiring committees in this community treat the physical-claim reading as settled background. Their disciplinary authority and citation networks depend on the boundary holding as stated.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_computability_theorists, beneficiary,
    institutional, civilizational, analytical, global).

% Has built trillion-dollar infrastructure, chip design, and software theory entirely within the Turing-computable frame. Benefits from the thesis being treated as physically settled because it forecloses the possibility that competitors could leapfrog with fundamentally different (non-Turing) physical computation, and it validates the theoretical foundations underlying decades of hardware and compiler design.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, digital_computing_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Sets the terms of publishable computational theory by treating physical Turing-computability as the frame within which all complexity results are meaningful. Peer review, conference program committees, and prize committees (Turing Award lineage) enforce this frame by routing hypercomputation submissions to the margins or rejecting them as physically unmotivated.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, complexity_theory_establishment, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, complexity_theory_establishment, agenda_setter).

% Study models (infinite-time Turing machines, Malament-Hogarth spacetimes, analog neural nets with real-valued weights, relativistic computers) that formally exceed Turing computability. They are structurally trapped inside a discipline whose top journals and funding agencies presuppose the physical-claim reading is true, which makes their work read as either science fiction or a category error rather than live physics. Exit means leaving academic computer science and physics entirely.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, trapped, global).

% Researchers who argue that certain quantum sampling demonstrations, if interpreted literally, would show physical processes computing something outside classical Turing-equivalent complexity classes in practice, not just in polynomial-time efficiency. They face intense pushback because the physical-claim reading (properly, its complexity-theoretic cousin, the extended CT thesis) is treated as a boundary condition on legitimate interpretation of their own experimental results.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_overclaim_challengers, payer,
    moderate, biographical, constrained, global).

% Work on continuous-time analog computers, DNA computing, and other substrates that might in principle exploit real-valued physical quantities to exceed discrete Turing computation. They struggle to secure funding because grant reviewers, operating under the physical-claim reading, treat any claim of super-Turing physical computation as prima facie evidence of a flawed experimental design or measurement error rather than a genuine result.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, analog_and_exotic_computation_researchers, payer,
    powerless, biographical, trapped, national).

% Study whether exotic spacetime geometries (closed timelike curves, naked singularities) could physically instantiate super-Turing computation. Their work sits at the boundary between physics and computability theory but is rarely invited into either community's core discourse: computability theorists consider it too speculative-physical, physicists consider it too computability-theoretic. They would object to the physical-claim reading's confident empirical closure but are structurally outside both rooms where the thesis is adjudicated.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, theoretical_physicists_studying_computation_in_spacetime, excluded,
    moderate, civilizational, constrained, universal).

% Analyze the thesis's logical status: is it empirical, conventional, or a hybrid? They document that the physical-claim reading is the only one of the three that is genuinely falsifiable by discovering or ruling out a super-Turing physical process, and that its current 'settled' status rests more on absence of counterexamples than on a proof of impossibility.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable boundary condition that lets physicists, computer scientists, and engineers build cumulative theory and infrastructure without each subfield re-deriving what 'computable' means from first principles for every new physical substrate proposed.
% TRANSFER_FUNCTION: Moves research legitimacy, funding, and publication access from research programs premised on super-Turing physical computation toward research programs premised on Turing-equivalence, regardless of whether any physical experiment has actually falsified super-Turing computation.
% ABSENT_VOICES: Hypercomputation theorists and analog/exotic computation researchers would argue the thesis-as-physical-claim is unfalsified rather than confirmed, and that treating it as settled forecloses experimental programs before they are run. They publish in specialty venues (relativistic computation literature, hypercomputation workshops) that mainstream computability and complexity venues rarely cite or engage.
% DISAPPEARANCE_RATIONALE: If the physical-claim reading were abandoned as unsettled, the digital computing industry and complexity theory establishment would see no immediate operational change (existing machines remain Turing-equivalent regardless of the thesis's truth), but funding agencies and journals would need to treat hypercomputation and exotic-substrate proposals as live empirical questions rather than presumptively closed ones — a real reallocation of research attention and legitimacy, even though no existing artifact would stop working.
% FOUNDING_PROBLEM: Formalize, in the 1930s, a rigorous and stable notion of 'effective procedure' or 'algorithm' that would settle foundational questions in logic (decidability, the Entscheidungsproblem) and let mathematicians agree on what counts as a computation at all.
% FOUNDING_PROBLEM_CORROBORATION: Working physicists studying computation in exotic spacetimes (a seat outside the classical computability establishment) attest that the founding mathematical problem was solved decades ago, but that the physical-claim extension — no physical process exceeds Turing computability — remains an open empirical conjecture, not a settled result; no experiment has definitively tested it, and no proof from physical law rules out counterexamples. The classical computability establishment, by contrast, treats the physical extension as effectively settled, which is exactly the corroboration gap the omega variables below are meant to register.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.42, rising slowly over the interval: the thesis-as-physical-claim was largely unchallenged and low-stakes in 1936 (no competing physical computation models had matured), but as hypercomputation theory, DNA computing, and exotic-spacetime computation proposals accumulated from the 1980s onward, the cost of the presumption — foreclosed funding, foreclosed publication, foreclosed careers — grew commensurately. Suppression is authored higher (0.55) and rising faster than extraction because the mechanism is primarily gatekeeping (grant panels, peer review, hiring) rather than direct rent extraction; the constraint's persistence depends on active enforcement of the boundary in review processes, not merely on passive disciplinary consensus. Theater ratio stays low (0.18) because the coordination function (a stable shared definition of computation) is genuinely functional for the overwhelming majority of computer science and does real work; the constraint is not mostly performance.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the complexity theory establishment, the thesis is a mountain: an obviously true, natural boundary that no one seriously contests and that requires no enforcement because it is simply correct. From the seat of a hypercomputation researcher denied funding on the grounds that their proposed physical process 'cannot' exceed Turing computability, the same thesis operates as an actively enforced boundary that forecloses their entire research program without ever having been empirically tested against it. The engine's per-seat computation should register this divergence directly: same structural facts, different position relative to the boundary, different classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical computability theorists, the digital computing industry, and the complexity theory establishment are structural beneficiaries: their institutional authority, funding flows, and cumulative theoretical apparatus depend on the physical-claim reading being treated as settled, regardless of whether it has been proven. Hypercomputation researchers, quantum-supremacy overclaim challengers, and analog/exotic computation researchers are structural targets: they bear the cost of the presumption through foreclosed funding, journal rejection, and career risk, with limited exit (leaving the field entirely, or reframing their work in terms the establishment will accept). Theoretical physicists studying computation in exotic spacetimes occupy an excluded position — genuinely relevant to adjudicating the claim, but institutionally homeless in the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (formalizing 'effective procedure' for 1930s logic) is genuinely dead as a live crisis — the mathematical question was settled decades ago and nobody seriously disputes the formal equivalence of the standard computability models. What survives is the physical extension, which was never the founding problem at all; it is a later addition that inherited the founding problem's settled authority without inheriting its proof. This is close to a zombie-mandate pattern: the coordination function that justified enforcement (settling 1930s decidability questions) is dead, but the enforcement apparatus (grant panels, peer review gatekeeping) persists and has been redirected to defend an unrelated, unproven empirical extension. Classifying this as tangled_rope rather than mountain or pure snare captures both halves honestly: real coordination value from the original formal result, real asymmetric extraction from treating its physical extension as equally settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_claim_falsifiability_status,
    'Has the physical Church-Turing thesis actually been tested against any candidate super-Turing physical process, or does its ''settled'' status rest entirely on the absence of a demonstrated counterexample?',
    'A survey of the hypercomputation and exotic-computation literature identifying whether any proposed physical process has been rigorously tested and definitively falsified (versus merely deemed implausible or under-resourced to test).',
    'If no genuine test has occurred, the thesis''s institutional treatment as settled fact is unsupported by the evidentiary standard the reading itself claims to be answerable to, strengthening the tangled_rope reading over a mountain reading. If rigorous tests have failed to produce counterexamples across decades, the case for near-mountain status strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_claim_falsifiability_status, empirical, 'Whether the physical-claim reading has been empirically tested or merely presumed.').

omega_variable(
    kernel_reading_conflation_in_practice,
    'When institutions (funding agencies, journals) invoke ''the Church-Turing thesis'' to reject hypercomputation proposals, are they actually invoking the physical_claim_reading, or silently substituting the mathematical_definition_reading (which is unfalsifiable and therefore an even stronger foreclosure) without acknowledging the substitution?',
    'Discourse analysis of grant rejection letters and peer review comments citing ''the Church-Turing thesis'' as grounds for rejection, coded for which reading is actually being invoked.',
    'If institutions are substituting the unfalsifiable definitional reading while claiming the falsifiable empirical reading''s epistemic humility, the suppression this story documents is understated — the true mechanism is a conflation across kernel readings, not a defensible empirical judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_conflation_in_practice, conceptual, 'Whether gatekeeping conflates the physical reading with the unfalsifiable definitional reading.').

omega_variable(
    quantum_computation_as_counterexample_candidate,
    'Do any existing or near-term quantum computing demonstrations constitute genuine physical super-Turing computation, or are they Turing-equivalent-but-more-efficient (i.e., consistent with the physical-claim reading, just not with classical tractability assumptions)?',
    'Formal complexity-theoretic analysis distinguishing computability (what can be computed at all) from complexity (how efficiently), applied to specific quantum supremacy experimental claims.',
    'If quantum computers remain strictly Turing-equivalent in computability terms (differing only in complexity class), the physical-claim reading survives untouched and the quantum_supremacy_overclaim_challengers'' victim status is about complexity-class rhetoric, not the CT boundary itself — narrowing this story''s actual victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_computation_as_counterexample_candidate, empirical, 'Whether quantum computing challenges computability (CT thesis) or only complexity (a separate question).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__physical_claim_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(chur_tr_t1985, church_turing_thesis__physical_claim_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__physical_claim_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(chur_tr_t2012, church_turing_thesis__physical_claim_reading, theater_ratio, 2012, 0.17).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__physical_claim_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.1).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__physical_claim_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(chur_be_t1985, church_turing_thesis__physical_claim_reading, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__physical_claim_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(chur_be_t2012, church_turing_thesis__physical_claim_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__physical_claim_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.15).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__physical_claim_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(chur_su_t1985, church_turing_thesis__physical_claim_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__physical_claim_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(chur_su_t2012, church_turing_thesis__physical_claim_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__physical_claim_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language 'Church-Turing thesis' per the ε-invariance principle: the mathematical_definition_reading (true by convention, unfalsifiable, near-zero ε, no genuine victims — a mountain or rope depending on enforcement), the epistemological_boundary_reading (a claim about the boundary of formal provability, agnostic on physical possibility, moderate-low ε), and this physical_claim_reading (an empirical claim about physical law, moderate ε, identifiable victims among hypercomputation and exotic-substrate researchers, tangled_rope). The upstream mathematical_definition_reading is cited as evidentiary support for treating this physical reading as equally settled, which is precisely the conflation flagged in the kernel_reading_conflation_in_practice omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
