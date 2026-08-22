% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Effective Computability as Stipulated Definition (Definitional Reading of the Church-Turing Thesis)
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   The colloquial label 'Church-Turing thesis' covers at least three
 *   structurally distinct claims; per the epsilon-invariance principle this
 *   file instantiates exactly one — the definitional reading, on which the
 *   thesis is a stipulated mathematical definition fixing 'effective
 *   computability' as Turing-machine computability, true by convention and
 *   not empirically testable. On this reading the arrangement is a
 *   terminological coordination device: it solves a real collective-action
 *   problem (impossibility results require a bounded formal notion of
 *   'method'), imposes near-zero extraction (definitions cannot be violated,
 *   only declined), and benefits the mathematical community broadly. KEY
 *   AGENTS (by structural relationship): computability_theorists — primary
 *   beneficiary (organized/constrained), gain a fixed subject matter;
 *   theoretical_computer_scientists — secondary beneficiary
 *   (organized/constrained), inherit the predicate their field is phrased in;
 *   academic_publishing_gatekeepers — agenda_setter (institutional/mobile),
 *   maintain usage norms; hypercomputation_researchers — principal payer
 *   (moderate/identity_locked), bear terminology-collision friction;
 *   mathematics_students — payer and incidental beneficiary
 *   (powerless/mobile); human_computation_theorists — excluded
 *   (moderate/constrained), their question is defined away rather than
 *   answered; philosophy_of_math_analysts — analytical observer. The sibling
 *   readings are separate constraints linked in network.affects_constraints;
 *   committer content enters this file only through omega variables.
 *
 * KEY AGENTS:
 *   - computability_theorists: primary beneficiary (organized/constrained) — the stipulated predicate gives their field a fixed object of study
 *   - theoretical_computer_scientists: secondary beneficiary (organized/constrained) — phrase complexity classes and hardness results in the inherited predicate
 *   - academic_publishing_gatekeepers: agenda_setter (institutional/mobile) — journals, textbooks, curricula sustain uniform usage; collect no fees for custody
 *   - hypercomputation_researchers: principal payer (moderate/identity_locked) — their objects get relabeled as extensions rather than engaged; exit means abandoning their research programs
 *   - mathematics_students: payer with incidental beneficiary position (powerless/mobile) — bear learning overhead and conflation confusion, receive entry into precise practice
 *   - human_computation_theorists: excluded (moderate/constrained) — hold that stipulation fixes a word rather than answering their question; sit outside the maintenance venues
 *   - philosophy_of_math_analysts: analytical observer (analytical/analytical) — analyze the definitional status and track the reading dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.04).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.03).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Effective Computability as Stipulated Definition (Definitional Reading of the Church-Turing Thesis)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '6a5d287d-38a1-4cab-a043-2659788964d6').
narrative_ontology:cs_kernel_codification('6a5d287d-38a1-4cab-a043-2659788964d6', formalized).
narrative_ontology:cs_authority_grounding('6a5d287d-38a1-4cab-a043-2659788964d6', expertise).
narrative_ontology:cs_interpretation_layer_present('6a5d287d-38a1-4cab-a043-2659788964d6').
narrative_ontology:cs_reading_relation('6a5d287d-38a1-4cab-a043-2659788964d6', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_reading_relation('6a5d287d-38a1-4cab-a043-2659788964d6', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('6a5d287d-38a1-4cab-a043-2659788964d6', foundational, effective_computability_is_stipulative_definition).
narrative_ontology:cs_axiom_status(effective_computability_is_stipulative_definition, holdable).
narrative_ontology:cs_axiom_grounding('6a5d287d-38a1-4cab-a043-2659788964d6', effective_computability_is_stipulative_definition, conventional).
narrative_ontology:cs_axiom('6a5d287d-38a1-4cab-a043-2659788964d6', secondary, informal_notion_admits_precise_formalization).
narrative_ontology:cs_axiom_status(informal_notion_admits_precise_formalization, holdable).
narrative_ontology:cs_axiom_grounding('6a5d287d-38a1-4cab-a043-2659788964d6', informal_notion_admits_precise_formalization, instrumental).
narrative_ontology:cs_reference_frame('6a5d287d-38a1-4cab-a043-2659788964d6', canonical_stipulative_definition).
narrative_ontology:cs_drift_state('6a5d287d-38a1-4cab-a043-2659788964d6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6a5d287d-38a1-4cab-a043-2659788964d6', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematics_students).
narrative_ontology:constraint_victim(church_turing_thesis__mathematical_definition_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__mathematical_definition_reading, mathematics_students).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formalism_equivalence_convergence).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, entscheidungsproblem_undecidability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Logicians and mathematicians working in recursion and computability theory. The stipulated predicate gives their field a fixed object of study: decidability, reducibility, and degree structures are stated and proved against one shared definition. Leaving the convention would mean losing contact with the community's accumulated results; adopting private variants costs intelligibility with peers.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computability_theorists, beneficiary,
    organized, generational, constrained, global).

% Complexity theorists and algorithms researchers. They phrase complexity classes, reductions, and hardness results in terms inherited from the definition, and their daily work depends on the predicate staying fixed. Switching formalisms is possible in principle but would sever their results from the literature built on the standard predicate.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists, beneficiary,
    organized, biographical, constrained, global).

% Journal editors, textbook authors, and curriculum committees. They sustain uniform usage by requiring standard terminology in publications and courses. They collect no fees for this custodial work; their leverage is acceptance and citation, and they can move between venues freely.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, academic_publishing_gatekeepers, agenda_setter,
    institutional, generational, mobile, global).

% Researchers proposing models that compute beyond Turing-machine computable functions (oracle-relativized, analog, relativistic, or infinite-time schemes). Under the stipulated definition their objects fall outside 'effective computability', so their results tend to be relabeled as extensions rather than engaged as refutations; they bear review skepticism and terminology-collision friction. Their research programs are constitutive of their professional identities, so leaving the fray would mean abandoning the work they are known for.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, hypercomputation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Undergraduate and graduate students encountering the definition in coursework. They bear the learning overhead and occasional confusion when popular sources blur the definition into empirical or epistemic claims, and they receive entry into precise practice in exchange. Exiting the field is cheap and common.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematics_students, payer,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__mathematical_definition_reading, mathematics_students, beneficiary).

% Philosophers and cognitive scientists who treat effective calculability as tied to what idealized disciplined human computers can do. Their objection — that fixing the word by stipulation does not answer whether the formal predicate exhausts mechanical method — is voiced in philosophy venues and rarely reaches the journals, curricula, and standards processes where usage is maintained.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, human_computation_theorists, excluded,
    moderate, biographical, constrained, global).

% Philosophers of mathematics and of computation who analyze the definitional status of the thesis, track the dispute among its readings, and publish accounts of what the convention does and does not settle. They hold no material stake in the definition's maintenance.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophy_of_math_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__mathematical_definition_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__mathematical_definition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single precise referent for the informal term 'effective computability' (mechanical procedure, algorithm), so that impossibility results, reduction arguments, and complexity claims mean the same thing across logic, mathematics, and computer science.
% TRANSFER_FUNCTION: Transfers almost no material goods; it transfers terminological authority — the right to fix what 'effective computability' means — from dispersed informal usage to the canonical formal definition, and routes citation capital toward the founding sources (Church 1936, Turing 1936) and their textbook expositors.
% ABSENT_VOICES: Constituencies that treat effective calculability as a substantive property of idealized disciplined human calculation would object that a stipulation fixes a word rather than answering their question; they sit in philosophy of mind and mathematics venues, outside the journals, curricula, and standards processes where the definition's usage is maintained (stakeholder human_computation_theorists). Proponents of absorbed alternative formalisms (Post systems, lambda calculus, recursive functions) were integrated via equivalence proofs rather than consulted as rivals.
% DISAPPEARANCE_RATIONALE: Thousands of theorems, proofs, and curricula use 'effectively computable' as a fixed predicate; overnight removal would strip decidability and incompleteness results of their stated form until the community re-coordinated terminology — almost certainly reconverging on the same definition, since the equivalence convergence makes it the unique fixed point, but the rearrangement would be real and costly.
% FOUNDING_PROBLEM: Before 1936, questions like Hilbert's Entscheidungsproblem asked whether 'any mechanical method' settles a problem class, but 'mechanical method' had no precise meaning — negative results were unprovable because there was no formal bound on what counts as a method. The definition was built to make undecidability provable.
% FOUNDING_PROBLEM_CORROBORATION: The historical record corroborates independently of the convention's current beneficiaries: Hilbert and Ackermann posed the Entscheidungsproblem in 1928 in terms that presuppose no formal computability notion, and histories of the period document the gap the definition closed. Standard logic curricula worldwide still teach undecidability results that are unintelligible without the fixed predicate — attestation from outside any benefiting faction's self-report.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.04, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.04: the definition's only costs are a one-time learning overhead and friction imposed on users who prefer divergent terminology — no rents are collected and no seat captures gains. Suppression is 0.03 and is a raw structural property, unscaled by power or scope: nothing forbids private definitions; deviation costs only intelligibility. Theater is 0.36 and rising (see the shared-grid measurement series): a growing minority of thesis-invocations are ornamental citations rather than load-bearing uses, though the majority of activity remains functional — every decidability proof exercises the definition directly. Accessibility collapse is 0.45: alternative formalizations existed (lambda calculus, recursive functions, Post systems) and were absorbed by equivalence proofs rather than suppressed; within shared discourse the Turing formulation dominates, but private variants remain available at communication cost. Resistance is 0.15: mild, unorganized pushback from those who hold the stipulation trivializes a substantive question; no faction campaigns against the convention. Claimed type is rope — genuine coordination, net beneficiaries, negligible coercion — authored independently of these metric values. Receipt surface: the tiny residual costs accrue to no named seat, so gain_flow is diffuse (affirmatively checked across all seven stakeholders); replacing the convention would cost the discipline a full re-coordination against a near-zero benefit, so fixing_cost is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the beneficiary seats (computability_theorists, theoretical_computer_scientists) the arrangement is experienced as pure clarification — the kind of thing that barely registers as a constraint at all. From the payer seats it registers mildly: hypercomputation_researchers find their objects relabeled rather than engaged ('outside effective computability' rather than refuted), and students absorb the stipulation as received doctrine before understanding why. The agenda_setter seat experiences custodial duty without material gain. Because base epsilon is tiny, even a full-target directional read yields small effective extraction — the structural asymmetry is real but shallow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality (the arrangement subsidizes them with a fixed shared predicate); payers derive elevated directionality, but the amplification operates on a 0.04 base, so effective extraction stays negligible for every seat. No trapped full-target seat exists at the story level: hypercomputation_researchers are identity_locked, which pushes their d toward the target end, but what flows from them is friction, not transfer. No directionality overrides were needed — the beneficiary declarations plus exit options reproduce the intended relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the thesis as a definition explains its peculiar immunity to refutation without invoking dogma: conventions are not the kind of thing evidence refutes, so the absence of falsification tests is a category fact, not protection-racket behavior. The founding problem — making undecidability provable — remains live and is served daily, so no mandate has outlived its function and mandatrophy_resolved is not declared. The monitored risk is slow ritualization: theater_ratio climbing from 0.12 to 0.36 could, if unchecked, convert a living definition into a citation shibboleth; omega ritualization_trajectory tracks that line. The classification prevents mislabeling in both directions: it blocks an extraction reading (there is no victim set to name — definitions cannot be violated) and blocks complacency about drift (rising theater is measured, not assumed away).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This file instantiates only the definitional reading of the church_turing_thesis kernel: is the stipulative-definition reading the arrangement that actually governs practice, or do the sibling readings (physical_claim_reading, epistemological_boundary_reading) govern how the community treats the thesis?',
    'Examine how the community responds to apparent counterexamples: if proposed beyond-Turing processes are treated as outside the defined term (relabeled as extensions, not refuted), the definitional reading governs; if they are treated as empirical surprises about physics or as challenges to a knowability boundary, a sibling reading is operative.',
    'If a sibling reading is operative, the structural picture changes drastically: the physical reading carries potential victims (dismissed hypercomputation programs) and contested empirical status; the epistemological reading carries extraction wherever careers ride on the boundary claim. Victim sets and epsilon would need re-authoring per sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the church_turing_thesis kernel this constraint''s structural data actually governs.').

omega_variable(
    informal_notion_capture_fidelity,
    'Does the stipulated formal predicate (Turing-machine computability) actually capture the pre-formal notion of mechanical procedure it was built to fix, or does the alignment fail at load-bearing margins (resource-bounded human computers, probabilistic and interactive methods, oracle access)?',
    'Marginal-case analysis: test whether practitioners'' informal uses of ''algorithm'' and ''mechanical method'' diverge systematically from the formal predicate in ways that matter to proofs, claims, and teaching.',
    'If capture fails at margins that matter, the coordination benefit degrades and the residual costs imposed on divergent users rise; if capture holds, the low-extraction coordination reading is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_notion_capture_fidelity, empirical, 'Whether the definition faithfully fixes the informal target notion.').

omega_variable(
    ritualization_trajectory,
    'Is the rising theater_ratio (0.12 to 0.36 across the interval) benign consensus consolidation, or early Goodhart drift in which invocations of the thesis replace engagement with the definition''s function?',
    'Code the share of thesis-invocations in contemporary literature that are load-bearing (used inside a proof or definition) versus ornamental (rhetorical closure); track whether the ornamental share keeps climbing.',
    'Sustained climb past roughly 0.6 with flat functional use would signal a piton trajectory — a definition maintained by citation ritual rather than use; a plateau indicates healthy settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritualization_trajectory, empirical, 'Direction of performative-citation drift around the definition.').

omega_variable(
    newcomer_cost_distribution,
    'The definition''s residual costs (learning overhead, terminology-collision friction) fall disproportionately on newcomers and dissenters while coordination benefits accrue to established researchers — is this distribution an acceptable coordination cost or a regressive feature deserving separate accounting?',
    'Compare time-to-fluency and conflation-error rates across cohorts taught the stipulation under different framings; survey dissenting researchers on the friction costs they actually bear.',
    'If costs concentrate on the least powerful seats, per-seat effective extraction for those seats exceeds the aggregate 0.04 and per-seat classifications may diverge from the story-level coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(newcomer_cost_distribution, preference, 'Normative weighting of who bears the definition''s small residual costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(chur_tr_t0, observed).
narrative_ontology:measurement(chur_tr_t17, church_turing_thesis__mathematical_definition_reading, theater_ratio, 17, 0.18).
narrative_ontology:measurement_basis(chur_tr_t17, observed).
narrative_ontology:measurement(chur_tr_t34, church_turing_thesis__mathematical_definition_reading, theater_ratio, 34, 0.24).
narrative_ontology:measurement_basis(chur_tr_t34, observed).
narrative_ontology:measurement(chur_tr_t51, church_turing_thesis__mathematical_definition_reading, theater_ratio, 51, 0.29).
narrative_ontology:measurement_basis(chur_tr_t51, observed).
narrative_ontology:measurement(chur_tr_t68, church_turing_thesis__mathematical_definition_reading, theater_ratio, 68, 0.33).
narrative_ontology:measurement_basis(chur_tr_t68, observed).
narrative_ontology:measurement(chur_tr_t85, church_turing_thesis__mathematical_definition_reading, theater_ratio, 85, 0.36).
narrative_ontology:measurement_basis(chur_tr_t85, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(chur_be_t0, observed).
narrative_ontology:measurement(chur_be_t17, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 17, 0.05).
narrative_ontology:measurement_basis(chur_be_t17, observed).
narrative_ontology:measurement(chur_be_t34, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 34, 0.04).
narrative_ontology:measurement_basis(chur_be_t34, observed).
narrative_ontology:measurement(chur_be_t51, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 51, 0.04).
narrative_ontology:measurement_basis(chur_be_t51, observed).
narrative_ontology:measurement(chur_be_t68, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 68, 0.04).
narrative_ontology:measurement_basis(chur_be_t68, observed).
narrative_ontology:measurement(chur_be_t85, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 85, 0.04).
narrative_ontology:measurement_basis(chur_be_t85, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0, 0.09).
narrative_ontology:measurement_basis(chur_su_t0, observed).
narrative_ontology:measurement(chur_su_t17, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 17, 0.07).
narrative_ontology:measurement_basis(chur_su_t17, observed).
narrative_ontology:measurement(chur_su_t34, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 34, 0.06).
narrative_ontology:measurement_basis(chur_su_t34, observed).
narrative_ontology:measurement(chur_su_t51, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 51, 0.05).
narrative_ontology:measurement_basis(chur_su_t51, observed).
narrative_ontology:measurement(chur_su_t68, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 68, 0.04).
narrative_ontology:measurement_basis(chur_su_t68, observed).
narrative_ontology:measurement(chur_su_t85, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 85, 0.03).
narrative_ontology:measurement_basis(chur_su_t85, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Church-Turing thesis'. The label conflates three structurally distinct claims with different epsilon values, beneficiary/victim structures, and failure modes: (1) this file — the definitional reading, a terminological convention with negligible extraction and no victims; (2) physical_claim_reading — a contested empirical claim about what physical processes can compute, with potential victims where hypercomputation programs are dismissed; (3) epistemological_boundary_reading — a claim about the limits of formal provability on which research careers ride. This story is upstream: fixing the referent of 'effective computability' is what makes the physical claim well-posed and supplies the formal predicate the epistemological boundary claim relates to provability. Each member links the others via network.affects_constraints; the epsilon differences are documented in each file's narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
