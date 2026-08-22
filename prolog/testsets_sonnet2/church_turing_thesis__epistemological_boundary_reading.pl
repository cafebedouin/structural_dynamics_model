% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Provable Computability
 *   domain: philosophy_of_mathematics/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint is the epistemological-boundary reading of the
 *   Church-Turing kernel: the claim that Turing-computability marks the
 *   boundary of what can be FORMALLY KNOWN to be computable — that 'we can
 *   prove function f is computable' and 'f is Turing-computable' are treated
 *   as coextensive within the proof-theoretic practice of mathematics and
 *   computer science, independent of any claim about physical computation.
 *   This is distinct from the mathematical-definition reading (which treats
 *   the thesis as a stipulative convention true by fiat) and the
 *   physical-claim reading (which makes an empirical claim about what the
 *   universe can compute). The epistemological reading is narrower than both:
 *   it is a claim about the boundary of PROOF, enforced through referee
 *   practice and curricular transmission, and it has identifiable payers —
 *   researchers whose results fall outside the Turing-reducible proof
 *   standard.
 *
 * KEY AGENTS:
 *   - classical_recursion_theorists: institutional beneficiaries who set and rely on the boundary
 *   - computability_proof_referees: agenda-setters who actively enforce the boundary at the point of publication
 *   - hypercomputation_researchers: powerless, trapped payers whose formal results cannot count as computability proofs under this reading
 *   - alternative_proof_paradigm_advocates: excluded payers whose alternative formalizations are judged only by reduction to the Turing standard
 *   - philosophy_of_mathematics_observers: analytical seat documenting the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.34).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Provable Computability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '5bfba18c-6cae-47c8-baf8-af5f4383b2de').
narrative_ontology:cs_kernel_codification('5bfba18c-6cae-47c8-baf8-af5f4383b2de', formalized).
narrative_ontology:cs_authority_grounding('5bfba18c-6cae-47c8-baf8-af5f4383b2de', expertise).
narrative_ontology:cs_interpretation_layer_present('5bfba18c-6cae-47c8-baf8-af5f4383b2de').
narrative_ontology:cs_reading_relation('5bfba18c-6cae-47c8-baf8-af5f4383b2de', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bfba18c-6cae-47c8-baf8-af5f4383b2de', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('5bfba18c-6cae-47c8-baf8-af5f4383b2de', foundational, provability_gates_computability_status).
narrative_ontology:cs_axiom_status(provability_gates_computability_status, holdable).
narrative_ontology:cs_axiom_grounding('5bfba18c-6cae-47c8-baf8-af5f4383b2de', provability_gates_computability_status, conventional).
narrative_ontology:cs_axiom('5bfba18c-6cae-47c8-baf8-af5f4383b2de', secondary, turing_reduction_is_sole_valid_proof_target).
narrative_ontology:cs_axiom_status(turing_reduction_is_sole_valid_proof_target, holdable).
narrative_ontology:cs_axiom_grounding('5bfba18c-6cae-47c8-baf8-af5f4383b2de', turing_reduction_is_sole_valid_proof_target, instrumental).
narrative_ontology:cs_reference_frame('5bfba18c-6cae-47c8-baf8-af5f4383b2de', turing_1936_formal_equivalence_consensus).
narrative_ontology:cs_drift_state('5bfba18c-6cae-47c8-baf8-af5f4383b2de', contemporary_hypercomputation_debate, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5bfba18c-6cae-47c8-baf8-af5f4383b2de', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_proof_referees).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_science_curriculum_designers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, alternative_proof_paradigm_advocates).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, recursive_function_equivalence_thesis).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_machine_canonical_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work entirely within the proof-theoretic framework in which Turing-computability is the accepted formal proxy for 'computable.' Their theorems, textbooks, and career-defining results are stated and cited in terms of Turing-equivalence. They set the reviewing standard for what counts as a valid computability proof and collect the professional and pedagogical benefits of that standard's stability.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists, agenda_setter).

% Referee papers submitted to logic and theoretical computer science venues. They enforce the boundary directly: a claimed computability or incomputability result that does not reduce to a Turing-machine argument (or a construction provably equivalent to one) is rejected or required to be reframed. Their gatekeeping is the active-enforcement mechanism that keeps the boundary sharp.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_proof_referees, agenda_setter,
    institutional, generational, arbitrage, global).

% Teach computability theory using Turing machines (or an equivalent formalism) as the sole canonical model. This gives them a stable, transmissible curriculum and a clean pedagogical story, but it also means students are trained to treat the Turing boundary as identical with 'computable' rather than as one formal proxy among historically competing ones.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_science_curriculum_designers, beneficiary,
    organized, generational, constrained, global).

% Mathematicians who wish to make computability-adjacent claims using non-constructive methods (e.g. existence proofs via compactness, non-effective enumerations, choice-dependent constructions) find such claims excluded from the category of 'proven computable' regardless of their mathematical validity elsewhere. They must either reframe their results in Turing-reducible terms or accept that the result does not count as a computability result at all.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants, payer,
    moderate, biographical, constrained, global).

% Study models that purport to compute beyond the Turing barrier (infinite-time Turing machines, oracle constructions, analog/relativistic hypercomputation proposals). Their formal results are structurally barred from counting as 'proofs of computability' under the epistemological-boundary reading, since the thesis defines provable computability as coextensive with Turing-computability by methodological fiat, not by settling their models' physical status. Exit is essentially impossible while remaining inside mainstream computability theory — their work is relegated to a separate, lower-status literature.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, payer,
    powerless, biographical, trapped, global).

% Constructive mathematicians, proof-theorists exploring realizability semantics outside classical recursion theory, and researchers proposing alternative formal models of 'effective procedure' find that any alternative formalization is judged valid only insofar as it is shown equivalent to Turing-computability. Their voice on what 'formally knowable' should mean is structurally foreclosed by the boundary the thesis draws.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, alternative_proof_paradigm_advocates, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, alternative_proof_paradigm_advocates, excluded).

% Study the epistemological status of the boundary itself — whether 'formally knowable computation' is a discovered fact about proof or a methodologically entrenched convention. They document the contest between readings without being party to the enforcement machinery.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophy_of_mathematics_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared standard for what counts as a valid proof of computability, letting mathematicians and computer scientists communicate results, build on each other's theorems, and referee submissions without re-litigating the definition of 'computable' in every paper.
% TRANSFER_FUNCTION: Moves epistemic legitimacy — the status of 'this is a proven computability result' — toward work expressible in or reducible to Turing-machine terms, and away from non-constructive, hypercomputational, or alternative-formalism results, regardless of those results' independent mathematical validity.
% ABSENT_VOICES: Hypercomputation researchers and alternative-formalism advocates would argue that 'formally knowable' should not be pinned exclusively to one 1936-era formalism, especially given oracle machines, infinite-time Turing machines, and non-classical logics developed since. They publish in specialized venues largely outside the main computability-theory conversation, so their objection rarely reaches the referees who enforce the boundary.
% DISAPPEARANCE_RATIONALE: If the boundary vanished as an enforced methodological standard, classical recursion theorists dispute that anything would change (they hold the boundary tracks something real about effective procedure, not a convention). Hypercomputation researchers and non-constructive claimants hold that a wide range of currently-excluded results would enter the mainstream computability literature, curricula would need to teach multiple competing models side by side, and the referee standard for 'proof of computability' would fragment — a genuine rearrangement from their seat.
% FOUNDING_PROBLEM: In the 1930s, multiple independent formalizations of 'effective calculability' (Turing machines, lambda calculus, general recursive functions, Post systems) needed to be shown equivalent so mathematicians could agree on what 'computable' meant well enough to prove theorems about it and resolve Hilbert's Entscheidungsproblem.
% FOUNDING_PROBLEM_CORROBORATION: Classical recursion theorists and curriculum designers (the benefiting parties) attest the founding problem remains live: proof needs a fixed formal target. Independent corroboration is thinner outside that circle — philosophers of mathematics studying alternative computability paradigms and hypercomputation theorists note that the equivalence-of-formalisms problem was solved decades ago, and what persists now is a methodological convention functioning as an unrevisable boundary rather than an open mathematical question; no fully external attestor (a body with no stake in classical recursion theory's proof standard) has been identified.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.34, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.34 at interval end) because the boundary genuinely does coordinate a shared, highly productive proof practice — this is not primarily a rent-extraction mechanism. But it is not zero: identifiable communities (hypercomputation researchers, non-constructive-methods mathematicians) pay a real cost in excluded legitimacy, and that cost has crept upward mildly over the modeled interval as oracle machines, infinite-time Turing machines, and quantum/analog computability proposals have multiplied without displacing the Turing standard as the arbiter of 'proven computable.' Suppression (0.42) reflects the referee-enforcement mechanism — real but not severe, since excluded work can still be published, just not as computability results within the mainstream target venues. Theater ratio is low (0.12) because the enforcement is functionally load-bearing (it really does let researchers avoid re-litigating foundational definitions each time) rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat, the boundary is simply what 'computable' correctly and stably means — no extraction is visible because the frame IS the standard. From the payer seat (hypercomputation researchers, alternative-formalism advocates), the same boundary is a methodological wall that converts a historically contingent 1936 formal equivalence into an unrevisable gate on what proof can establish. The engine's per-seat computation should reflect this: institutional/arbitrage seats compute near rope, powerless/trapped seats compute nearer snare-adjacent tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical recursion theorists and referees sit near the beneficiary end: they built the standard, they collect the professional and pedagogical stability it provides, and their own work is automatically compliant. Hypercomputation researchers and alternative-formalism advocates sit near the target end: trapped, because remaining within mainstream computability theory requires accepting a standard that structurally excludes their central claims, and powerless, because they lack the referee-seat leverage to renegotiate what counts as a computability proof. Non-constructive-computability claimants are moderate-power payers — established mathematicians who can publish elsewhere but are denied the specific epistemic credit of a 'computability result.'
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than mountain or pure snare) is deliberate: there IS a genuine coordination function — a stable shared definition lets an entire field build cumulative results without re-deriving equivalence-of-formalisms every time — and there IS active enforcement extracting epistemic legitimacy from a genuine minority of researchers whose formal work does not reduce to Turing-machine terms. Reading this purely as a mountain (a fact about proof, no victims, no enforcement) would erase the referee mechanism and the real cost paid by hypercomputation researchers. Reading it purely as a snare would erase the genuine, decades-proven value of having ONE formal target for computability claims. The tangled_rope reading holds both facts without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_versus_epistemic_gate,
    'Is the Turing-equivalence proof standard a neutral, near-costless convention (as the beneficiary seats experience it) or a substantive methodological gate that excludes independently valid results (as the payer seats experience it)?',
    'Track whether results from hypercomputation or non-constructive frameworks that are later shown Turing-reducible retroactively gain ''computability result'' status, versus results that remain permanently excluded despite internal mathematical rigor — a persistent, non-shrinking excluded set over decades would support the substantive-gate reading.',
    'If the boundary is a near-costless convention, this reading collapses toward rope; if it is a persistent substantive exclusion, it stays tangled_rope or drifts toward snare as enforcement hardens relative to any residual coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_versus_epistemic_gate, conceptual, 'Whether the boundary functions as low-cost convention or substantive epistemic exclusion.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the epistemological-boundary reading''s victim set stop and the physical-claim reading''s victim set begin — i.e., is a hypercomputation proposal excluded because it fails to be a PROOF (epistemological reading) or because it is judged physically impossible (physical-claim reading)?',
    'Examine specific referee rejections of hypercomputation papers: do rejections cite ''this is not a valid computability proof under accepted formal standards'' (epistemological) or ''this requires physically impossible resources'' (physical)? The two grounds are often conflated in practice.',
    'If rejections predominantly invoke physical impossibility, extraction attributed to this reading should be lower and reassigned to the physical_claim_reading sibling constraint; if they predominantly invoke proof-standard failure, this reading''s ε is correctly sited here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Disentangling epistemological from physical grounds for excluding hypercomputation claims.').

omega_variable(
    alternative_formalism_equivalence_completeness,
    'Have all serious alternative formalizations of ''effective procedure'' proposed since 1936 actually been shown Turing-equivalent, or are some merely presumed equivalent without rigorous demonstration?',
    'Systematic survey of alternative computability formalisms (relativized computation, infinite-time Turing machines, various hypercomputation models) checking which have formal equivalence or non-equivalence proofs versus which are simply excluded by convention without a demonstrated reduction.',
    'If genuine non-equivalent-but-rigorous alternatives exist and are simply excluded rather than refuted, this strengthens the case that the boundary functions partly as extraction rather than pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_formalism_equivalence_completeness, empirical, 'Whether exclusion of alternative formalisms rests on proof or on unexamined convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.08).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(chur_tr_t75, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 90, 0.12).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(chur_be_t75, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 75, 0.33).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 90, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 45, 0.36).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(chur_su_t75, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 90, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'Church-Turing thesis' per the ε-invariance principle: the mathematical_definition_reading (stipulative convention, near-zero ε, no victims), the physical_claim_reading (empirical claim about physical computation, contested by hypercomputation-in-physics proposals), and this epistemological_boundary_reading (methodological exclusion within mathematical proof practice, low-to-moderate ε, victims are excluded mathematical claimants). The three share the label 'Church-Turing thesis' but are structurally distinct constraints with different ε, different victim sets, and different enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
