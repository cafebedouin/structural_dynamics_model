% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of the Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates the many-worlds reading of the contested
 *   quantum-formalism kernel: the universal wavefunction never collapses,
 *   evolves unitarily and deterministically for all time, and what observers
 *   experience as a single definite measurement outcome is
 *   decoherence-induced apparent branching into causally isolated but equally
 *   real worlds. The reading eliminates the observer as a privileged physical
 *   category and treats the Born rule's probabilities as an emergent,
 *   self-locating uncertainty within a fully deterministic multiverse. This
 *   is one of three sibling readings of the same underlying formalism
 *   (Copenhagen, pilot-wave); this story evaluates only the many-worlds
 *   reading's own institutional and epistemic structure, not the kernel-level
 *   dispute among all three.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.28).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of the Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, 'bf6c4adf-859e-4fb6-8b95-448cd51a532a').
narrative_ontology:cs_kernel_codification('bf6c4adf-859e-4fb6-8b95-448cd51a532a', formalized).
narrative_ontology:cs_authority_grounding('bf6c4adf-859e-4fb6-8b95-448cd51a532a', expertise).
narrative_ontology:cs_interpretation_layer_present('bf6c4adf-859e-4fb6-8b95-448cd51a532a').
narrative_ontology:cs_reading_relation('bf6c4adf-859e-4fb6-8b95-448cd51a532a', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf6c4adf-859e-4fb6-8b95-448cd51a532a', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('bf6c4adf-859e-4fb6-8b95-448cd51a532a', foundational, wavefunction_never_collapses).
narrative_ontology:cs_axiom_status(wavefunction_never_collapses, holdable).
narrative_ontology:cs_axiom_grounding('bf6c4adf-859e-4fb6-8b95-448cd51a532a', wavefunction_never_collapses, empirically_contingent).
narrative_ontology:cs_axiom('bf6c4adf-859e-4fb6-8b95-448cd51a532a', foundational, observer_is_eliminable_physical_category).
narrative_ontology:cs_axiom_status(observer_is_eliminable_physical_category, holdable).
narrative_ontology:cs_axiom_grounding('bf6c4adf-859e-4fb6-8b95-448cd51a532a', observer_is_eliminable_physical_category, conventional).
narrative_ontology:cs_axiom('bf6c4adf-859e-4fb6-8b95-448cd51a532a', secondary, parsimony_favors_no_added_postulate).
narrative_ontology:cs_axiom_status(parsimony_favors_no_added_postulate, holdable).
narrative_ontology:cs_axiom_grounding('bf6c4adf-859e-4fb6-8b95-448cd51a532a', parsimony_favors_no_added_postulate, instrumental).
narrative_ontology:cs_reference_frame('bf6c4adf-859e-4fb6-8b95-448cd51a532a', everett_1957_relative_state_formulation).
narrative_ontology:cs_drift_state('bf6c4adf-859e-4fb6-8b95-448cd51a532a', post_decision_theoretic_probability_program, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bf6c4adf-859e-4fb6-8b95-448cd51a532a', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, many_worlds_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, unitary_only_pedagogy_advocates).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, graduate_students_seeking_measurement_resolution).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, rival_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, philosophy_of_science_funding_applicants).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, schrodinger_equation_universality).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_explains_apparent_collapse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and defend the claim that the universal wavefunction never collapses and that decoherence alone explains why measurement looks probabilistic and branching looks like a single classical outcome. They set the interpretive agenda in departments and journals where the reading is dominant, publish the papers that adjudicate what counts as a serious objection, and gain career standing, grant eligibility, and citation authority from the reading's acceptance as a live research program rather than a settled minority view.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, many_worlds_theorists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, many_worlds_theorists, beneficiary).

% Work on the mathematics of decoherence and einselection, which many-worlds advocates cite as vindicating the reading's central claim that branching is derivative of unitary dynamics. Their technical output is real and independently useful, but its interpretive framing as solving the measurement problem (rather than merely explaining classicality) inflates the reading's evidentiary standing beyond what the mathematics alone establishes.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, biographical, constrained, global).

% Teach and popularize the many-worlds picture as the parsimonious, no-collapse-needed account, benefiting from its clean narrative appeal to students and lay audiences. Their standing rises with the reading's popularity even where the probability (Born rule) derivation from branch-counting remains genuinely contested among specialists.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, unitary_only_pedagogy_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Enter foundations research needing to know whether the measurement problem is solved or merely relabeled. They must adopt the many-worlds framing (or visibly dissent from it) to work with advisors committed to the program, and their thesis timelines and job prospects depend on producing work legible to the dominant reading's referees. Exit means leaving the subfield or a hostile relationship with their advisor's community.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, graduate_students_seeking_measurement_resolution, payer,
    powerless, biographical, trapped, national).

% Work on Copenhagen-style or pilot-wave programs and must contend with many-worlds' institutional prestige advantage — the claim that it alone avoids postulating collapse or hidden variables ties funding panel sympathies and journal gatekeeping toward the many-worlds framing, making rival programs fight uphill for the same review slots and citations.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, rival_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Seek grants to investigate probability, personal identity, and ontology under quantum branching. Funding bodies staffed or advised by many-worlds-sympathetic physicists and philosophers set the terms of what counts as a tractable proposal, disadvantaging applicants whose framing takes collapse or hidden variables as live physical possibilities rather than settled non-issues.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_science_funding_applicants, payer,
    powerless, biographical, constrained, national).

% Argue that the decision-theoretic and branch-counting derivations of the Born rule (Deutsch-Wallace program) are circular or presuppose what they aim to derive. Their technical objections are acknowledged in review articles but rarely shift the reading's institutional momentum or funding weight; they publish but are treated as a permanent minority footnote rather than a live falsifying threat.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, born_rule_derivation_critics, excluded,
    moderate, biographical, constrained, global).

% Assess the full interpretive landscape without a stake in any single reading's institutional fortunes, noting that all three major readings (many-worlds, Copenhagen, pilot-wave) are empirically equivalent at current experimental resolution and that the dispute is adjudicated by parsimony criteria, ontological taste, and institutional inertia rather than data.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophers_of_physics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, mathematically minimal account of quantum mechanics that requires no extra postulate beyond unitary Schrodinger evolution — solving the theoretical-parsimony problem of not needing a separate, unexplained collapse dynamics or hidden-variable guidance equation, and giving researchers a coherent research program (decoherence theory, branch counting, Everettian probability) to build careers and curricula around.
% TRANSFER_FUNCTION: Moves interpretive authority, funding eligibility, and pedagogical primacy toward researchers and departments committed to the no-collapse, all-branches-real picture, and away from researchers committed to rival readings — the transfer is institutional standing and resource access, not money directly, flowing from whichever community controls journal gatekeeping and hiring committees in a given department or era.
% ABSENT_VOICES: Working physicists who treat interpretation as practically irrelevant to their experimental or computational work are rarely in the room when interpretive disputes are adjudicated; their silence is read by many-worlds advocates as tacit acceptance and by rivals as institutional exhaustion, but it is not itself evidence for any reading.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished from the field overnight, the mathematics of quantum mechanics and its experimental predictions would be entirely unchanged — no measurable outcome depends on which interpretation is held. But the sociology of the field would rearrange substantially: hiring patterns in foundations groups, textbook framing, popular science narratives, and grant panel composition would shift toward whichever reading absorbed the vacated institutional space. Physicists dispute whether this counts as 'the world changing' since the disagreement is precisely about whether interpretation is metaphysically load-bearing or professionally load-bearing only.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics as formalized by von Neumann treats measurement as a special, non-unitary process (collapse) with no principled account of when or why it occurs, creating an explanatory gap between the deterministic Schrodinger dynamics and the definite outcomes observers report. Everett's 1957 proposal was built to close this gap without adding a new physical process.
% FOUNDING_PROBLEM_CORROBORATION: Decoherence theorists (Zurek and successors, largely outside the interpretive advocacy community, working primarily on einselection and quantum-to-classical transition as applied physics) attest that decoherence genuinely explains why superpositions become practically unobservable at macroscopic scale — but explicitly decline to attest that this settles whether branches are ontologically real worlds or an artifact of an incomplete description, which is the further metaphysical claim many-worlds advocates add on top of the decoherence mathematics. No consensus corroboration exists from outside the reading's own advocacy community on the ontological (as opposed to the purely mathematical) claim.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) and rising over the interval because the reading's core mathematical content (unitary evolution, decoherence) is genuinely load-bearing physics with negligible extraction on its own, but the further ontological claim (branches are real worlds, not merely formal artifacts) increasingly functions as a career and funding filter in foundations departments as the reading gained institutional traction post-1970s (Everett's original 1957 paper was largely ignored; DeWitt's 1970s popularization and the Deutsch-Wallace decision-theoretic program from the 1990s onward built the institutional apparatus that now shapes hiring and review). Suppression is authored low-moderate (0.28) — dissenting physicists are not coerced, but gatekeeping in journals, funding panels, and hiring committees imposes real friction on rival programs. Accessibility collapse is moderate (0.35): the formalism does not force acceptance of the ontological add-on, and working physicists routinely use the mathematics while remaining agnostic or hostile to the many-worlds metaphysics, so alternatives are far from foreclosed. Resistance is moderate-high (0.55), reflecting genuine, technically serious ongoing objections (Born-rule derivation critiques) that have not been resolved, only institutionally out-competed in some venues.
 *
 * DIRECTIONALITY LOGIC:
 *   Many-worlds theorists and the decoherence/pedagogy communities that gain standing from the reading's popularity sit near the beneficiary end: they collect citation authority, grant eligibility, and curricular primacy from the reading's institutional dominance in the departments where it holds sway. Graduate students, funding applicants, and rival-interpretation researchers sit nearer the target end: they bear the cost of a field whose gatekeeping increasingly treats the many-worlds ontological add-on as low-cost or default, making dissent or agnosticism a professional liability in some subfields even though the underlying mathematics does not require it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (closing the explanatory gap in von Neumann's measurement postulate) remains genuinely live at the level of the mathematics — decoherence theory is real, useful, ongoing physics, not a solved-and-abandoned problem being milked for rent. But the further claim that Everettian ontology specifically (as opposed to decoherence mathematics generally) has settled the interpretive question is where mandatrophy risk concentrates: a research program whose original founding puzzle (why definite outcomes?) is treated by advocates as closed, while the metaphysical add-on (are the other branches real?) that was supposed to close it remains exactly as contested as when Everett proposed it in 1957. Tangled-rope classification tracks this: genuine coordination function (a parsimonious, minimal-postulate formal account) coexists with asymmetric extraction (institutional gatekeeping that treats the ontological claim as more settled than the Born-rule derivation literature supports).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    born_rule_derivation_circularity,
    'Does the decision-theoretic (Deutsch-Wallace) derivation of the Born rule from branch-counting actually derive quantum probabilities from non-probabilistic premises, or does it covertly assume probabilistic structure it claims to derive?',
    'Continued technical work in decision theory and probability foundations; resolution would require either a derivation immune to the circularity charge or a formal proof that no such derivation is possible without additional probabilistic axioms.',
    'If the derivation is genuinely circular, the many-worlds reading''s central claimed advantage over rivals (deriving rather than postulating probability) collapses, substantially raising its effective extractiveness by removing the coordination justification for institutional primacy while enforcement of the reading''s dominance in some departments continues regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_derivation_circularity, empirical, 'Whether the Born-rule derivation from branch-counting is circular.').

omega_variable(
    ontology_vs_mathematics_separability,
    'Is the ontological claim (branches are real, separately existing worlds) separable from the mathematical claim (unitary evolution plus decoherence explains apparent classicality), or does accepting the mathematics commit one to the ontology?',
    'Conceptual analysis and philosophy-of-science work distinguishing instrumentalist decoherence accounts (branches as bookkeeping devices for an observer''s ignorance) from realist many-worlds accounts (branches as literally existing); no empirical experiment can adjudicate this at current technology.',
    'If separable, most of the reading''s institutional extraction rides on the unnecessary ontological add-on rather than the (largely uncontested) decoherence mathematics, meaning the coordination function and the extraction function occupy genuinely different parts of the same label — a further decomposition candidate under the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontology_vs_mathematics_separability, conceptual, 'Whether many-worlds ontology is separable from decoherence mathematics.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (many-worlds, Copenhagen, pilot-wave) locate their disagreement, given that all three reproduce identical experimental predictions?',
    'Structural analysis of each reading''s treatment of the measurement postulate: many-worlds eliminates it (measurement is derivative, decoherence-induced), Copenhagen retains it as a fundamental physical process (collapse is real and irreducible), pilot-wave replaces it with hidden-variable guidance (particles have definite positions all along). The disagreement is located precisely at what happens to the wavefunction upon measurement and whether the wavefunction is ontic (physically real) or epistemic (a calculational device) — no experiment at current technology distinguishes them.',
    'Because the disagreement is located at the ontological status of the wavefunction and the measurement postulate rather than at any measurable quantity, no future experiment under presently conceivable technology adjudicates among the three readings; institutional and pedagogical dominance therefore substitutes for empirical resolution, which is exactly the mechanism this story''s extraction measure tracks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the kernel-level disagreement structurally: at the wavefunction''s ontological status and the fate of the measurement postulate, not at any empirical prediction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__many_worlds_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(quan_tr_t1985, quantum_formalism__many_worlds_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(quan_tr_t2000, quantum_formalism__many_worlds_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(quan_tr_t2012, quantum_formalism__many_worlds_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__many_worlds_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.1).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__many_worlds_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(quan_be_t1985, quantum_formalism__many_worlds_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(quan_be_t2000, quantum_formalism__many_worlds_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(quan_be_t2012, quantum_formalism__many_worlds_reading, base_extractiveness, 2012, 0.37).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__many_worlds_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1957, quantum_formalism__many_worlds_reading, suppression_requirement, 1957, 0.08).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__many_worlds_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(quan_su_t1985, quantum_formalism__many_worlds_reading, suppression_requirement, 1985, 0.14).
narrative_ontology:measurement(quan_su_t2000, quantum_formalism__many_worlds_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(quan_su_t2012, quantum_formalism__many_worlds_reading, suppression_requirement, 2012, 0.23).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__many_worlds_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial label 'interpretation of quantum mechanics' into structurally distinct constraints per the epsilon-invariance principle: many_worlds_reading (this file, tangled_rope — genuine parsimony coordination plus institutional extraction on the ontological add-on), copenhagen_reading, and pilot_wave_reading. Each reading shares the identical predictive mathematics (the kernel) but differs in its treatment of the measurement postulate, its ontological commitments, and consequently its beneficiary/victim structure and epsilon. All three are linked via affects_constraints; none is the 'true' reading of the kernel within this framework — each is authored as its own clean constraint from its own advocates' structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
