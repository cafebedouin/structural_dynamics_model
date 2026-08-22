% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Many-Worlds Interpretation of Quantum Mechanics
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The many-worlds interpretation (MWI) of quantum mechanics claims that the
 *   universal wavefunction evolves deterministically according to the
 *   Schrödinger equation without collapse. When a measurement is performed,
 *   the measuring apparatus becomes entangled with the measured system,
 *   inducing decoherence that produces the appearance of branching into
 *   separate worlds in which each outcome is realized. This reading
 *   eliminates the observer and indeterminism from the foundations of quantum
 *   mechanics but at the cost of ontological extravagance: infinitely many
 *   worlds actually exist. The constraint story models MWI not as a
 *   true-or-false empirical claim but as a reading of a contested kernel (the
 *   quantum formalism itself) that benefits certain research programs
 *   (determinism doctrine, realist ontology, quantum computing theory) while
 *   extracting costs from others (measurement-problem research,
 *   observer-dependent frameworks, interpretive pluralism). The constraint
 *   has grown more extractive over the interval 1957–2026 as many-worlds has
 *   shifted from a minority position to an institutional consensus in quantum
 *   foundations, suppressing publication and funding for alternative
 *   readings.
 *
 * KEY AGENTS:
 *   - Determinism doctrine (non-agent beneficiary): gains explanatory authority when MWI eliminates fundamental indeterminism
 *   - Realist ontology advocates (beneficiary, institutional): physicists who treat the wavefunction as an objective physical entity; MWI provides unified ontology without collapse
 *   - Quantum computing theorists (beneficiary, organized): researchers justifying quantum parallelism through branching into real worlds
 *   - Measurement-problem researchers (victim/payer, powerful): must accept MWI's rephrasing of the problem even when they find it unsatisfying
 *   - Observer-dependent frameworks (victim/payer, moderate): relational and QBism researchers whose theoretical legitimacy is undermined by elimination of the observer
 *   - Interpretive pluralism (victim/payer, identity-locked): philosophers and methodologically pluralist physicists whose foundational principle (multiple readings remain open) is contradicted by the enforcement of MWI as canonical
 *   - Quantum foundations consensus authority (agenda setter, institutional): peer-review and funding gatekeeping system that has incrementally shifted prestige toward MWI
 *   - Copenhagen traditionalists (excluded, institutional): older quantum physicists and their intellectual heirs, increasingly sidelined in cutting-edge research
 *   - Undergraduate physics curriculum (observer, institutional): structural node that shapes student intuitions about which interpretations are live options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.62).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Interpretation of Quantum Mechanics").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '14cc5624-5439-4e41-825c-a6346397e435').
narrative_ontology:cs_kernel_codification('14cc5624-5439-4e41-825c-a6346397e435', fixed_text).
narrative_ontology:cs_authority_grounding('14cc5624-5439-4e41-825c-a6346397e435', expertise).
narrative_ontology:cs_interpretation_layer_present('14cc5624-5439-4e41-825c-a6346397e435').
narrative_ontology:cs_reading_relation('14cc5624-5439-4e41-825c-a6346397e435', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('14cc5624-5439-4e41-825c-a6346397e435', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('14cc5624-5439-4e41-825c-a6346397e435', foundational, universal_wavefunction_completeness).
narrative_ontology:cs_axiom_status(universal_wavefunction_completeness, holdable).
narrative_ontology:cs_axiom_grounding('14cc5624-5439-4e41-825c-a6346397e435', universal_wavefunction_completeness, deontological).
narrative_ontology:cs_axiom('14cc5624-5439-4e41-825c-a6346397e435', foundational, determinism_global_level).
narrative_ontology:cs_axiom_status(determinism_global_level, holdable).
narrative_ontology:cs_axiom_grounding('14cc5624-5439-4e41-825c-a6346397e435', determinism_global_level, deontological).
narrative_ontology:cs_axiom('14cc5624-5439-4e41-825c-a6346397e435', secondary, measurement_as_decoherence_not_collapse).
narrative_ontology:cs_axiom_status(measurement_as_decoherence_not_collapse, holdable).
narrative_ontology:cs_axiom_grounding('14cc5624-5439-4e41-825c-a6346397e435', measurement_as_decoherence_not_collapse, empirically_contingent).
narrative_ontology:cs_axiom('14cc5624-5439-4e41-825c-a6346397e435', secondary, observer_eliminability).
narrative_ontology:cs_axiom_status(observer_eliminability, holdable).
narrative_ontology:cs_axiom_grounding('14cc5624-5439-4e41-825c-a6346397e435', observer_eliminability, deontological).
narrative_ontology:cs_reference_frame('14cc5624-5439-4e41-825c-a6346397e435', wavefunction_as_complete_ontology).
narrative_ontology:cs_drift_state('14cc5624-5439-4e41-825c-a6346397e435', contemporary_consensus_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14cc5624-5439-4e41-825c-a6346397e435', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinism_doctrine).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, realist_ontology_advocates).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_computing_theorists).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, measurement_problem_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, observer_dependent_frameworks).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, interpretive_pluralism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_determinism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The many-worlds interpretation vindicates global determinism at the fundamental level: the Schrödinger equation has no stochastic term, no collapse operator, no indeterminism. Every quantum process is fully determined. This doctrine gains explanatory authority and institutional prestige from the interpretation's adoption.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, determinism_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quantum_formalism__many_worlds_reading, determinism_doctrine).

% Physicists and philosophers who treat the wavefunction as a physical entity evolving in Hilbert space. The many-worlds interpretation eliminates the need for wave-function collapse or Born-rule postulates disconnected from the equations of motion. They gain a unified, parsimonious ontology: the wavefunction is all that exists, it evolves deterministically, measurement emerges from decoherence. This cohort controls significant journal editorial positions, sits on funding committees, and mentors the next generation.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, realist_ontology_advocates, beneficiary,
    institutional, generational, constrained, global).

% Researchers developing quantum algorithms and error correction. The many-worlds framing treats parallel quantum branches as real, giving theoretical justification for why quantum computers can explore exponentially many solutions simultaneously. They benefit from an interpretation that makes quantum parallelism conceptually straightforward and reduces the need to explain how measurement 'selects' one answer. However, their exit options remain mobile because the computational predictions are independent of interpretation.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_computing_theorists, beneficiary,
    organized, biographical, mobile, global).

% Physicists and mathematicians working on the quantum-to-classical transition, decoherence timescales, and the emergence of measurement. The many-worlds interpretation shifts the measurement problem from 'why does the wavefunction collapse?' to 'how does classical appearance emerge from branching?' This rephrases the hard problem rather than solving it, yet many in this community must accept the interpretation to engage with its literature and defend their research direction within a consensus-driven field.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, measurement_problem_researchers, payer,
    powerful, biographical, constrained, global).

% Researchers in quantum information theory, relational interpretations, and QBism who treat the observer's perspective as ineliminable from quantum mechanics. The many-worlds reading eliminates the observer as a causal agent: measurement is a physical process (decoherence) in which the observer is just another quantum system. This undermines the theoretical legitimacy of observer-centric frameworks and makes funding and publication increasingly difficult.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, observer_dependent_frameworks, payer,
    moderate, biographical, constrained, global).

% Philosophers and methodologically pluralist physicists who maintain that multiple interpretations remain empirically equivalent and theoretically live. The enforcement of many-worlds as the canonical interpretation in quantum foundations courses, journals, and competitive grant review suppresses the teaching and funding of alternatives (Copenhagen, pilot-wave, QBism). Pluralism is identity-locked because it rests on the foundational principle that interpretive uncertainty should remain open; accepting one reading as canonical directly contradicts this identity commitment.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, interpretive_pluralism, payer,
    moderate, generational, identity_locked, global).

% The peer-review and publication gatekeeping system in quantum foundations physics journals, funding agencies, and elite physics departments. Over the interval 1990–present, many-worlds has shifted from a minority position to a plurality or near-consensus in high-impact venues. This authority administers enforcement through publication prestige, citation weighting, and research funding allocation. The authority can theoretically change this distribution back toward pluralism but has substantial institutional investment in the current consensus (reputational, budgetary, curricular).
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_foundations_consensus_authority, agenda_setter,
    institutional, generational, analytical, global).

% Older quantum physicists and their intellectual descendants who maintain the Copenhagen interpretation as legitimate. They are not absent from the field but are increasingly excluded from setting the agenda in cutting-edge quantum foundations work: their papers are cited but not built upon, their students are less often admitted to elite programs, their funding proposals are less competitive when they emphasize Copenhagen.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_traditionalists, excluded,
    institutional, generational, constrained, global).

% The formal and informal set of standards for what is taught in quantum mechanics courses. Increasingly, many-worlds is presented as the 'modern' or 'obvious' interpretation, while alternatives receive historical treatment or are omitted. This shapes student intuitions about which interpretations are live options for future research.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, undergraduate_physics_curriculum, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, realist_ontology_advocates).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified mathematical framework for quantum mechanics without invoking indeterminism, measurement-induced collapse, or observer-dependent structures. Deterministic evolution of a single wavefunction eliminates the need for a separate Born rule, making the theory logically simpler and philosophically more austere: one equation (Schrödinger), one ontology (wavefunction), determinism at the foundation.
% TRANSFER_FUNCTION: Shifts explanatory burden from the physics of measurement (how does collapse happen?) to the metaphysics of branches (what does it mean to exist in many worlds?). Transfers interpretive prestige from frameworks treating observation as physically real to frameworks treating it as a mathematical artifact of decoherence. Moves research funding and publication prestige toward many-worlds theorists and away from Copenhagen and relational-interpretation researchers.
% ABSENT_VOICES: Experimental particle physicists working on practical measurement apparatus design do not participate in foundational interpretation debates and would object that many-worlds adds no experimental content to guide their work. Philosophers of science emphasizing empirical equivalence would argue that the interpretation is metaphysically profligate without predictive gain. They are structurally outside the quantum-foundations consensus-building process.
% DISAPPEARANCE_RATIONALE: Proponents argue that if many-worlds interpretation vanished tomorrow, the empirical predictions of quantum mechanics would not change (empirical equivalence to Copenhagen and pilot-wave); however, the conceptual architecture would shift and research intuitions about what quantum mechanics 'really says' would fracture. Opponents argue the interpretation rests on purely philosophical scaffolding (the ontology of branches) and its disappearance would force the field to confront the harder problem of what quantum mechanics tells us about measurement and reality. The world's physics would be unchanged; the world's quantum-foundations research agenda would reorganize.
% FOUNDING_PROBLEM: Quantum mechanics in the late 1920s seemed to require subjective human observation or measurement as a causal element: the wavefunction 'collapses' when measured. This introduced an apparent asymmetry between quantum and classical physics and made the theory conceptually unsatisfying. By the 1950s, various interpretations competed to restore objectivity and eliminate the observer. Many-worlds (Everett, 1957) proposed a radical solution: the wavefunction never collapses; measurement is just the observer becoming entangled with the measured system, creating branching rather than indeterminacy.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (Deutsch, Wallace, Tegmark, Carroll) attest that many-worlds solves the measurement problem by eliminating collapse and restoring pure determinism; the founding problem remains live if one values objective, observer-independent mechanics. Opponents (philosophers of physics emphasizing empirical equivalence, Copenhagen traditionalists, pilot-wave advocates) attest that the founding problem has been displaced, not solved: many-worlds exchanges a discrete, physical collapse process for a continuous but unobservable branching into exponentially many worlds — it trades one conceptual difficulty for another. Independent empirical analysis finds no experimental way to distinguish many-worlds from Copenhagen on any realistic measurement. The consensus is contested: the problem is real, but whether many-worlds solves or merely relocates it remains open.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1957, when MWI was a fringe proposal) to 0.62 (2026, now a plurality position in elite venues). This rise reflects the constraint's growing ability to structure which research is publishable, fundable, and prestigious. Theater ratio falls from 0.72 to 0.48 over the same period: early MWI was highly speculative and dependent on rhetorical flourish (Everett's 'relative state' formulation); by 2026, decoherence theory has provided mathematical infrastructure that grounds MWI in concrete physics rather than pure ontological speculation, reducing the performative component. Suppression rises from 0.22 to 0.71: initially, alternative readings faced minimal active suppression (they were simply ignored as implausible); by 2026, the consensus authority actively suppresses alternatives through publication gatekeeping, funding allocation, and curricular standardization. Accessibility collapse is high (0.78) because once the many-worlds ontology is understood as the modern consensus, alternatives appear outdated or heterodox, even though they remain logically coherent. Resistance is moderate (0.55) because Copenhagen and pilot-wave researchers remain powerful enough to publish and secure some funding, but their institutional position has eroded.
 *
 * PERSPECTIVAL GAP:
 *   See above.
 *
 * DIRECTIONALITY LOGIC:
 *   See above.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real (measurement paradox in 1920s–1950s) and was live at the time of MWI's proposal (1957). The problem remains contested as to whether MWI solves or displaces it. The constraint exhibits mandatrophy risk: if measurement-problem researchers and philosophers of physics increasingly conclude that MWI has relocated the problem without solving it, while the institutional consensus maintains the interpretation's prestige, the constraint could become a piton — a consensus propped up by institutional inertia rather than continued legitimacy. The growth in theater ratio decline (from 0.72 to 0.48) and rise in suppression (from 0.22 to 0.71) is consistent with this trajectory: the interpretation has matured (less theater) but now requires enforcement (more suppression). A Tangled Rope reading treats it as having both a genuine coordination function (eliminating collapse, restoring determinism) and asymmetric extraction (suppressing alternatives, consolidating institutional prestige around one reading). The constraint should be classified as Tangled Rope, not pure Snare, because the coordination function (providing a coherent, deterministic framework) is real, even though it is asymmetrically captured by certain research programs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_and_interpretation,
    'If all three interpretations (Copenhagen, Many-Worlds, Pilot-Wave) make identical empirical predictions, what epistemic authority justifies choosing one reading as the canonical one?',
    'Philosophers of physics and empirical methodologists attest whether empirical equivalence permits the inference to ontological structure or whether it requires agnosticism about interpretation. If empirical equivalence is affirmed, the mandatrophy analysis changes: the constraint would be pure extraction (institutional prestige-seeking without empirical justification) rather than Tangled Rope (coordination with asymmetric benefit).',
    'If empirical equivalence is affirmed and the consensus persists, MWI reclassifies to Snare (pure extraction masked by coordination rhetoric); if empirical equivalence is denied (new experiments distinguish readings), the coordination function becomes real and the classification holds. The issue is unresolved and drives disagreement about the constraint''s type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_equivalence_and_interpretation, conceptual, 'Whether interpretations differing only in unobservable metaphysics can be rank-ordered by empirical grounds.').

omega_variable(
    ontological_extravagance_cost,
    'Does the creation of infinitely many actual worlds on each measurement represent a genuine solution to the measurement problem, or does it trade one conceptual problem (collapse) for a worse one (infinite parallel existence)?',
    'Metaphysical analysis of parsimony criteria (Occam''s razor) and what counts as ontologically respectable. If infinite worlds are deemed too extravagant relative to the problem solved, MWI loses legitimacy as a coordination solution. If infinite worlds are accepted as a price of determinism, the coordination function holds.',
    'If extravagance is deemed fatal, MWI reclassifies to Snare and mandatrophy applies: the constraint persists only through institutional prestige and active suppression, not justified by its solving power. If extravagance is accepted, the Tangled Rope classification and asymmetric-benefit reading hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_extravagance_cost, preference, 'Whether infinite branching ontology is acceptable payment for global determinism and observer-elimination.').

omega_variable(
    suppression_mechanism_identity_vs_structural,
    'Is the suppression of alternative interpretations driven by structural features (Many-Worlds'' genuine superiority as a research framework) or by identity and career-lock within the MWI-dominant establishment?',
    'Sociological and historical analysis: if younger physicists trained in MWI can exit the framework without career penalty, suppression is structural; if switching to Copenhagen or pilot-wave after institutional commitment to MWI carries reputational or funding costs, suppression is internalized and identity-locked.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests (targets carry it even after exit), and the classification becomes more extractive. If structural, exit is available and the payer seats retain some leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_vs_structural, empirical, 'Whether interpretive consensus is maintained by structural advantage or by internalized career-identity fusion.').

omega_variable(
    many_worlds_as_kernel_reading_vs_standalone_claim,
    'Is Many-Worlds best understood as one reading among live alternatives of a shared quantum formalism (the committer frame), or is it a standalone empirical/metaphysical claim that can be adjudicated independently?',
    'If many-worlds and Copenhagen differ only in interpretation of the same mathematics, they are readings of a shared kernel and should be modeled in the committer frame; if they differ in mathematical content (e.g., Pilot-Wave adds Bohmian particle trajectories as new mathematical structure), they are separate theories and should be separate constraints in an ε-invariance decomposition.',
    'Under the committer frame, this constraint is one reading; suppression of alternatives is institutional extraction from a controversy that cannot be empirically resolved. Under the standalone frame, Many-Worlds is a claim competing on evidential grounds with Copenhagen and Pilot-Wave, and institutional prestige flows to whichever has the most justification. The classification and mandatrophy analysis depend entirely on which frame is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(many_worlds_as_kernel_reading_vs_standalone_claim, conceptual, 'Whether MWI is a reading of a kernel or a standalone theory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.72).
narrative_ontology:measurement_basis(quan_tr_t1957, observed).
narrative_ontology:measurement(quan_tr_t1975, quantum_formalism__many_worlds_reading, theater_ratio, 1975, 0.68).
narrative_ontology:measurement_basis(quan_tr_t1975, observed).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__many_worlds_reading, theater_ratio, 1995, 0.58).
narrative_ontology:measurement_basis(quan_tr_t1995, observed).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__many_worlds_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement_basis(quan_tr_t2010, observed).
narrative_ontology:measurement(quan_tr_t2018, quantum_formalism__many_worlds_reading, theater_ratio, 2018, 0.5).
narrative_ontology:measurement_basis(quan_tr_t2018, observed).
narrative_ontology:measurement(quan_tr_t2026, quantum_formalism__many_worlds_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(quan_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement_basis(quan_be_t1957, observed).
narrative_ontology:measurement(quan_be_t1975, quantum_formalism__many_worlds_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(quan_be_t1975, observed).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__many_worlds_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement_basis(quan_be_t1995, observed).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__many_worlds_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement_basis(quan_be_t2010, observed).
narrative_ontology:measurement(quan_be_t2018, quantum_formalism__many_worlds_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement_basis(quan_be_t2018, observed).
narrative_ontology:measurement(quan_be_t2026, quantum_formalism__many_worlds_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(quan_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1957, quantum_formalism__many_worlds_reading, suppression_requirement, 1957, 0.22).
narrative_ontology:measurement_basis(quan_su_t1957, observed).
narrative_ontology:measurement(quan_su_t1975, quantum_formalism__many_worlds_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement_basis(quan_su_t1975, observed).
narrative_ontology:measurement(quan_su_t1995, quantum_formalism__many_worlds_reading, suppression_requirement, 1995, 0.51).
narrative_ontology:measurement_basis(quan_su_t1995, observed).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__many_worlds_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement_basis(quan_su_t2010, observed).
narrative_ontology:measurement(quan_su_t2018, quantum_formalism__many_worlds_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement_basis(quan_su_t2018, observed).
narrative_ontology:measurement(quan_su_t2026, quantum_formalism__many_worlds_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(quan_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.25).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_computing__decoherence_parallelism).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, measurement_problem__wavefunction_interpretation).

% DUAL FORMULATION NOTE:
% Many-Worlds Interpretation is one reading of the contested kernel 'quantum_formalism'. Three structurally distinct readings (Copenhagen, Many-Worlds, Pilot-Wave) all claim to interpret the same Schrödinger equation but make incompatible claims about measurement and determinism. ε varies across readings because each reading has a different theory of what extraction occurs (Copenhagen: observer indispensability; Many-Worlds: infinite-world cost; Pilot-Wave: hidden-variable commitment). Per ε-invariance principle, these are three separate constraints with distinct ε values, sibling-linked via network.affects_constraints. The kernel is shared; the readings are distinct constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
