% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Many-Worlds Interpretation of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The many-worlds interpretation of quantum formalism claims that the
 *   universal wavefunction evolves deterministically according to the
 *   Schrödinger equation, with no collapse postulate. Apparent measurement
 *   indeterminism is reinterpreted as branching of the wavefunction into
 *   multiple (infinite) equally real worlds, each containing one outcome. The
 *   measurement problem is reframed from 'How does indeterministic collapse
 *   occur?' to 'How do we experience one branch as definite when all branches
 *   are actualized?' This reading extracts a coordination benefit for
 *   determinism-preserving physicists and ontological realists (recovered
 *   global determinism, eliminated observer-dependence), but it suppresses
 *   the causal role of measurement and the observer's epistemic privilege,
 *   extracting costs from measurement foundationalists and observer-role
 *   theorists. The interpretation is enforced through institutional
 *   gatekeeping of what counts as rigorous derivation of Born-rule
 *   statistics, branch structure, and decoherence mechanisms.
 *
 * KEY AGENTS:
 *   - Determinism-preserving physicists: primary beneficiaries — adopt many-worlds to preserve classical determinism at the fundamental level
 *   - Ontological realism advocates: primary beneficiaries — favor the reading's claim that all branches are equally real and observer-independent
 *   - Measurement foundationalism defenders: primary victims — their interpretive program is challenged by treating collapse as emergent rather than fundamental
 *   - Observer-role theorists: primary victims — their position that observation is causally special is eliminated; identity-locked exit
 *   - Decoherence mechanism modelers: agenda-setter — set and enforce the technical apparatus through research programs and institutional gatekeeping
 *   - Empiricist instrumentalists: excluded — treat wavefunction as calculational tool and reject all ontological commitment
 *   - Experimental physicists: observer — treat interpretation as optional for laboratory work
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
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Interpretation of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, 'ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb').
narrative_ontology:cs_kernel_codification('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', formalized).
narrative_ontology:cs_authority_grounding('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', lineage).
narrative_ontology:cs_interpretation_layer_present('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb').
narrative_ontology:cs_reading_relation('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', foundational, universal_wavefunction_completeness_and_determinism).
narrative_ontology:cs_axiom_status(universal_wavefunction_completeness_and_determinism, holdable).
narrative_ontology:cs_axiom_grounding('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', universal_wavefunction_completeness_and_determinism, deontological).
narrative_ontology:cs_axiom('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', foundational, measurement_is_emergent_decoherence).
narrative_ontology:cs_axiom_status(measurement_is_emergent_decoherence, holdable).
narrative_ontology:cs_axiom_grounding('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', measurement_is_emergent_decoherence, empirically_contingent).
narrative_ontology:cs_axiom('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', secondary, observer_eliminability_from_fundamental_description).
narrative_ontology:cs_axiom_status(observer_eliminability_from_fundamental_description, holdable).
narrative_ontology:cs_axiom_grounding('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', observer_eliminability_from_fundamental_description, deontological).
narrative_ontology:cs_reference_frame('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', global_deterministic_wavefunction_evolution).
narrative_ontology:cs_drift_state('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', contemporary_quantum_foundations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac64b1f6-1375-4b7b-b08a-1fac4b4a3aeb', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinism_preserving_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, ontological_realism_advocates).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, measurement_foundationalism_defenders).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, observer_role_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They adopt the many-worlds reading because it restores global determinism to fundamental physics: the universal wavefunction evolves via the Schrödinger equation without any collapse postulate. They argue this preserves classical causal structure and eliminates the ad-hoc indeterministic modification of the dynamics. They benefit from the reading's internal coherence and the philosophical satisfaction of determinism at the foundation. Their research programs focus on deriving the Born rule from equal branching and understanding decoherence as the mechanism producing apparent outcomes.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, determinism_preserving_physicists, beneficiary,
    organized, generational, mobile, global).

% They favor many-worlds because all branches are equally real and the wavefunction is a complete objective description of reality. This supports their foundational physicalism: nothing beyond the wavefunction is needed. The reading eliminates observer-dependence (Copenhagen) and hidden variables (pilot-wave), offering what they see as the most parsimonious ontology: one universal state, deterministic evolution, all outcomes realized. They extract a philosophical victory in debates about realism, completeness, and observer independence.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, ontological_realism_advocates, beneficiary,
    powerful, generational, mobile, global).

% They maintain that measurement is a fundamental physical process producing genuine indeterminism through wavefunction collapse. The many-worlds reading directly challenges this by treating collapse as illusory — an emergent description of decoherence rather than a basic law. They bear the cost of having their interpretive program characterized as explanatorily inadequate, baroque, or ad-hoc by many-worlds advocates. Their research on collapse theories (GRW, Penrose) faces reduced institutional support and publication venues. Exit from this constraint means abandoning foundational measurement research as a core program.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, measurement_foundationalism_defenders, payer,
    organized, generational, constrained, global).

% They argue that the observer plays a causally special role in quantum mechanics: measurement actualizes outcomes from superposition. The many-worlds reading eliminates this role by declaring all outcomes actualized in separate branches simultaneously — the observer becomes a passive local recorder of one branch's events. This suppresses the causal importance of measurement and observation, redefining their research identity from 'interpreters of observation's role' to 'describers of decoherence mechanisms.' Exit would require abandoning the self-conception as an interpreter of the observer's centrality.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, observer_role_theorists, payer,
    moderate, biographical, identity_locked, global).

% They set and enforce the many-worlds reading's technical apparatus: decoherence theory, branch-structure analysis, emergence of classical worlds from quantum superposition, and Born-rule derivation from branching symmetry (Deutsch-Wallace theorem, envariance). They administer the framework through research grants, textbook chapters, peer review, and conference programs. They benefit from positioning decoherence research as foundational and from controlling the technical standards for what counts as a rigorous 'derivation' of classical outcomes from quantum dynamics.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_mechanism_modelers, agenda_setter,
    institutional, generational, arbitrage, global).

% They resist all ontological commitments about the wavefunction's reality, branching, or collapse, treating quantum formalism as a calculational tool for predicting experimental outcomes. They view all the interpretive readings (Copenhagen, many-worlds, pilot-wave) as metaphysical over-commitment. They are structurally excluded from the many-worlds debate because the reading requires commitment to wavefunction realism and branch ontology — core positions instrumentalism rejects.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, empiricist_instrumentalists, excluded,
    powerful, biographical, constrained, global).

% They test the predictions of quantum mechanics in the laboratory, treating interpretation as philosophically optional. All extant interpretations yield identical experimental predictions (given current technology), so the many-worlds reading does not affect their research practice. They observe the interpretive contest without directly bearing costs or benefits, as long as the mathematical formalism remains predictively accurate.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimental_quantum_physicists, observer,
    institutional, biographical, analytical, global).

% They design quantum computers and quantum communication systems using controllable superposition, entanglement, and decoherence. Many-worlds offers an intuitive picture ('all computational branches are real, we just experience one outcome'), but this is pragmatically optional — they can engineer systems without interpreting the wavefunction. They remain neutral observers of the interpretive debate, focusing on controllability and noise management rather than ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_information_engineers, observer,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, decoherence_mechanism_modelers).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores global determinism to fundamental physics by eliminating the collapse postulate and treating the universal wavefunction as the complete, deterministically evolving description of reality. Solves the problem of preserving classical causal structure in quantum mechanics by reinterpreting apparent indeterminism as decoherence-induced branching. Coordinates an ontologically clear picture: all possible outcomes are actualized in separate branches, determinism is universal, the observer is eliminable.
% TRANSFER_FUNCTION: Transfers the interpretive burden from 'measurement' to 'decoherence and branching.' The reading extracts the observer's causal role and measurement's foundational status, moving the explanatory focus to how the universal wavefunction separates into decohered branches. It imposes the requirement to explain how single-outcome experience emerges from multi-branching superposition without invoking collapse or observer-dependence. This transfer redistributes the cost from defending an ad-hoc collapse postulate to defending infinite branching and the Born-rule derivation.
% ABSENT_VOICES: Empiricist instrumentalists who treat the wavefunction as a calculational tool and reject all ontological commitment; collapse-theory researchers (GRW, Penrose dynamics) who argue measurement induces physical collapse; consciousness-based interpretations that treat observation as causally special; hidden-variable proponents who defend pilot-wave theory as determinism-preserving without infinite branching. These positions are excluded because they contradict the many-worlds reading's core commitments (wavefunction realism, branching actualization, elimination of special observer role).
% DISAPPEARANCE_RATIONALE: If the many-worlds reading disappeared, the quantum formalism's interpretation would revert to ambiguity: Copenhagen (measurement-induced collapse, observer-dependent) and pilot-wave (hidden variables, deterministic without branching) would compete for dominance. Dynamical collapse theories would gain institutional interest as a third deterministic option. The measurement problem would be re-opened rather than resolved, and the field would fragment into multiple interpretive schools. Empiricists and instrumentalists would argue nothing disappears because interpretation is optional for empirical practice, so the world remains unchanged. The contested verdict reflects genuine disagreement about whether the reading solves the foundational problem or merely relabels it.
% FOUNDING_PROBLEM: Quantum mechanics produced a measurement paradox: the formalism predicts superposition before measurement and appears to predict indeterminism at measurement, yet the theory is based on a deterministic equation (Schrödinger). How can a completely deterministic universal law produce apparent randomness and observer-dependence? The Copenhagen interpretation resolved this by treating collapse as a fundamental process, but this required an ad-hoc modification of the dynamics and a special role for measurement and observers. The many-worlds reading was developed (by Everett, 1957) to preserve universal determinism by abolishing collapse: all outcomes are actualized in separate branches, so there is no indeterminism, only apparent branching from the perspective of a localized observer.
% FOUNDING_PROBLEM_CORROBORATION: Determinism-preserving physicists and ontological realists attest that the many-worlds reading solves the founding problem by recovering global determinism and eliminating collapse. Measurement foundationalists and observer-role theorists attest the problem persists: redefining collapse as 'decoherence-induced branching' does not explain why an agent experiences one outcome rather than all branches simultaneously — this is merely a relabeling, not a solution. Empiricist philosophers (outside the benefiting parties) attest the founding problem was misdefined: if the wavefunction is treated as a calculational tool rather than a description of reality, the measurement paradox dissolves and all interpretations become philosophically optional. There is no external consensus that the reading solves the problem; corroboration is split among the contending interpretive schools.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.62 at the interval end because the reading imposes a specific ontological and epistemological framework (infinite branching, no collapse, determinism global) that forecloses alternative interpretive paths and extracts the observer's causal role. Suppression is high (0.71) because enforcement occurs through peer review, textbook gatekeeping, and institutional pressure to adopt the decoherence/branching apparatus rather than exploring measurement fundamentalism or observer-role theories. Theater is moderate (0.48) because the reading's technical apparatus (decoherence theory, branch structure derivation) is real and productive, but a substantial share of the activity defends the interpretation's philosophical commitments rather than advancing empirical understanding. Accessibility collapse is moderately high (0.68) because once the many-worlds framework is adopted, alternatives become technically difficult to explore within the same institutional context, though empirical work remains possible by treating interpretation as optional. Resistance is high (0.74) because measurement foundationalists and observer-role theorists actively resist the reading's core claims through publication, alternative research programs, and critiques of the Born-rule derivation. The measurement series track extraction and suppression increasing over the interval (0–50) as the reading gained institutional influence, decoherence research expanded, and alternatives (like collapse theories) faced reduced funding and publication venues.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (determinism-preserving physicists), the reading is a coordination solution: it restores determinism, eliminates the ad-hoc collapse postulate, and provides a coherent ontology. From the victim seat (measurement foundationalism defenders), the reading is extractive: it redefines their interpretive problem out of existence without solving it, relocating the explanatory burden to decoherence and branch structure while leaving the core mystery (why one outcome appears) unexplained. From the excluded seat (empiricists), both beneficiaries and victims are over-committed to metaphysics that empirical practice does not require. From the observer seat (experimental physicists), interpretation is optional — the reading's benefits and costs are irrelevant to laboratory work. The engine computes different effective extraction for each seat from the structural data (beneficiary vs. victim declarations, power atoms, exit options); the perspectival gap reflects these asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Determinism-preserving physicists (organized, mobile exit) derive d near the beneficiary end (~0.25–0.35): they benefit from the reading's internal coherence and deterministic ontology without strong exit barriers — they can switch interpretations if the reading fails. Ontological realism advocates (powerful, mobile exit) sit at similar d (~0.30–0.40): they gain philosophical victory but retain mobility. Measurement foundationalism defenders (organized, constrained exit) sit near the target end (~0.65–0.75): their research program is challenged, they face institutional pressure to adopt the many-worlds framework or justify their alternative, and exit means relinquishing the foundational-measurement research identity. Observer-role theorists (moderate power, identity-locked exit) sit at the highest d (~0.75–0.85): the reading eliminates their interpretive niche, and exit requires relinquishing professional identity as an interpreter of observation. This structural asymmetry drives the tangled-rope classification: genuine coordination function (determinism recovery, ontological clarity) paired with asymmetric extraction from observer-role theorists, enforced through institutional gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   The many-worlds reading exhibits potential mandatrophy between its founding problem (measurement indeterminism paradox) and its current function (deterministic ontological framework). The founding problem status is contested: determinism-preserving physicists attest it is solved, but measurement foundationalists and empiricists attest it persists or was misdefined. The disappearance verdict is also contested: if the reading disappeared, the interpretive problem would re-open (revert to Copenhagen, pilot-wave, or dynamical collapse debates), suggesting the world would rearrange around the absent reading. The mismatch (status=contested, verdict=contested) does not activate a mandatrophy flag because both readings acknowledge the original problem as still live in some form. However, the theater_ratio rising from 0.32 to 0.48 over the interval suggests that decoherence modeling and branch-structure derivation are increasingly performative — the technical machinery is real, but its primary function may be defending the interpretation's philosophical commitments rather than advancing new empirical understanding. This is the signature of a reading that has begun drifting toward theatrical self-maintenance while the founding problem remains inadequately addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    branching_actualization_mechanism,
    'If all branches are equally real and actualized, by what mechanism does the agent in one branch experience only that branch''s outcomes? How is the appearance of definite outcomes derived from a deterministic superposition without invoking an unexplained actualization process?',
    'Rigorous derivation of the Born rule from branch structure and decoherence (reviewed against proposals like Deutsch-Wallace, Vaidman''s envariance principle); empirical or theoretical proof that the appearance of single-outcome experience emerges from the quantum state without additional postulates.',
    'If no such derivation can be completed without introducing hidden assumptions or observer-dependence, the reading''s claim to eliminate the measurement problem fails — it merely relocates the explanatory burden from ''collapse'' to ''apparent branching.'' This would downgrade the reading from coordination solution to expanded extraction of unexamined assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(branching_actualization_mechanism, empirical, 'Whether apparent single-outcome experience can be fully derived from equal-branching actualization without residual mystery.').

omega_variable(
    ontological_extravagance_vs_explanation,
    'Does the many-worlds reading''s commitment to infinite actual branches (ontological extravagance) explain quantum phenomena more parsimoniously than single-world interpretations with collapse or hidden variables, or does it trade one explanatory debt (collapse process) for a larger ontological debt (infinite worlds)?',
    'Comparative analysis of explanatory cost: (1) Copenhagen''s cost (ad-hoc collapse postulate, observer role); (2) pilot-wave''s cost (hidden variables, nonlocal guidance); (3) many-worlds'' cost (infinite branches, no mechanism for experienced definiteness). Philosophical debate on whether invoking infinite unobservable entities is more parsimonious than invoking a stochastic process or hidden mechanism.',
    'If infinite ontology proves more costly than the alternatives, the reading''s claimed coordination benefit (restoring determinism without collapse) is offset by its extractive cost (implausible ontology). This would reclassify the reading from tangled_rope to snare: extracting determinism at the price of unfalsifiable excess.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_extravagance_vs_explanation, conceptual, 'Whether many-worlds ontological parsimony is genuine or merely relocates explanatory burden to infinite branches.').

omega_variable(
    measurement_problem_relabeling,
    'Does the many-worlds reading solve the measurement problem or merely relabel it? Specifically, does redefining ''collapse'' as ''decoherence-induced branching'' solve the problem of why agents experience one outcome rather than explaining that as an irreducible fact requiring further postulation?',
    'Critical analysis of whether the reading dissolves the explanandum (why do we observe definite outcomes?) or changes its name without addressing its root. Examination of whether ''apparent definite outcomes in one branch'' requires additional axioms (e.g., self-location in Hilbert space, branch identification) that were not needed for the collapse postulate.',
    'If the reading merely relabels collapse without solving the measurement problem, then the constraint''s claim to coordinate a solution dissolves — it extracts the benefit of determinism while suppressing the cost of introducing equivalent explanatory machinery elsewhere (branch structure, decoherence, observer localization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_problem_relabeling, conceptual, 'Whether the reading solves or relabels the measurement problem.').

omega_variable(
    kernel_rivalry_framing,
    'Is the many-worlds reading genuinely a different reading of a single kernel (quantum formalism), or is it a fundamentally distinct theory that happens to use the same mathematical formalism? If readings are merely interpretations of a single formalism, does many-worlds interpret or extend?',
    'Structural analysis of whether many-worlds and Copenhagen (and pilot-wave) share the same explanatory target and disagree on its meaning, or whether they make incompatible empirical or metaphysical claims that cannot coexist in any unified framework. Examination of whether all readings invoke the same mathematical apparatus (Schrödinger equation, Born rule, decoherence) differently or invoke different apparatuses dressed in the same notation.',
    'If many-worlds is a distinct theory rather than an interpretation, the kernel structure dissolves — there is no single contested commitment, only rival theories. This would reframe the constraint not as a reading of quantum formalism but as an alternative formalism competing for institutional adoption. The extraction/coordination analysis would shift from interpretive dominance to paradigm dominance (more severe extraction cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_rivalry_framing, conceptual, 'Whether many-worlds is a reading of quantum formalism or a distinct competing theory.').

omega_variable(
    observer_role_suppression_internalization,
    'Is the suppression of the observer''s causal role structural (the reading logically eliminates observer-dependence by construction) or internalized (theorists trained in the reading have absorbed the new framing such that the elimination persists without ongoing enforcement)? Can a researcher who accepts the unified wavefunction but questions whether all branches are ontologically real find coherent ground within the reading, or would such questioning require opting out entirely?',
    'Ethnographic and textual analysis of how many-worlds theorists discuss and enforce the framework in publication, teaching, and peer review. Examination of whether dissenting positions on observer role or branch reality are met with logical refutation (structural suppression) or with social pressure/career consequences (internalized suppression that persists post-exit).',
    'If suppression is purely internalized, then theorists exiting the reading retain the suppression (reduced observability of alternative observer roles), making effective suppression higher than the structural metric suggests. If structural, then exit from the reading allows immediate re-engagement with observer-role questions. This modulates the reading''s net extraction cost and the identity-lock severity for observer-role theorists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_role_suppression_internalization, empirical, 'Whether observer-role suppression is structural or internalized and whether exit allows recovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(quan_tr_t10, quantum_formalism__many_worlds_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__many_worlds_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__many_worlds_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__many_worlds_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(quan_tr_t50, quantum_formalism__many_worlds_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(quan_be_t10, quantum_formalism__many_worlds_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__many_worlds_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__many_worlds_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__many_worlds_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(quan_be_t50, quantum_formalism__many_worlds_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(quan_su_t10, quantum_formalism__many_worlds_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__many_worlds_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__many_worlds_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__many_worlds_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(quan_su_t50, quantum_formalism__many_worlds_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.1).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum-formalism kernel instantiates three structurally distinct constraint stories: copenhagen_reading (measurement-induced collapse, observer-dependent indeterminism, Tangled Rope); many_worlds_reading (deterministic branching, observer-eliminable, Tangled Rope); pilot_wave_reading (deterministic hidden variables, classical ontology recovery, Tangled Rope). All three share the same mathematical formalism (Schrödinger equation, Born rule) but instantiate different constraints through different interpretations of the wavefunction's ontology, collapse/decoherence, and observer role. They form a constraint family linked by network.affects_constraints: each reading's adoption influences the viability and institutional standing of the others. The epsilon-invariance principle applies: the three readings have different epsilon values (extractiveness of the constraint imposed by that reading's interpretive commitments on alternative research programs) because they impose different suppression structures and beneficiary/victim relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
