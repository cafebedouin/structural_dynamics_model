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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Interpretation: Universal Determinism via Decoherence
 *   domain: epistemology/physics/philosophy_of_science
 *
 * SUMMARY:
 *   The many-worlds reading of quantum mechanics proposes that the universal
 *   wavefunction evolves deterministically under the Schrödinger equation;
 *   apparent measurement indeterminism is an artifact of observer perspective
 *   within a branching structure. When a measurement occurs, decoherence
 *   induces an apparently non-unitary split into mutually inaccessible
 *   branches, each physically real, each realizing a different outcome. The
 *   observer becomes entangled with the measured system and the measurement
 *   apparatus, finding themselves in one branch with a definite outcome while
 *   other branches containing other outcomes remain equally real. This
 *   reading eliminates the collapse postulate, observer-dependence of the
 *   wavefunction, and the measurement problem—at the cost of ontological
 *   extravagance (infinite branching worlds) and the unresolved
 *   preferred-basis problem (why does decoherence pick out one basis as the
 *   'branching axis'). The constraint story models this reading as a
 *   epistemic coordination structure: it solves a genuine coordination
 *   problem (providing a deterministic ontology for quantum mechanics) but at
 *   substantial extractive cost (ontological profligacy, emergence of
 *   branch-count indeterminacy, observer-location indeterminacy). The
 *   claim/metric gap is intentional: the reading claims to be a pure
 *   coordination solution (determinism without collapse), while the authored
 *   metrics capture that adoption requires accepting substantial conceptual
 *   and ontological burdens whose bearers are those committed to
 *   classical-like determinism or foundational clarity.
 *
 * KEY AGENTS:
 *   - Everett-school foundations researchers: adopt many-worlds as primary interpretation; benefit from deterministic global evolution and observer-independent wavefunction. Power: institutional (university physicists, funded research programs). Exit: constrained—switching interpretations requires reworking conceptual foundations of quantum mechanics and retraining in alternative formalisms.
 *   - Copenhagen-interpretation practitioners: treat measurement as fundamental, collapse as real physical process. Resist many-worlds because it requires abandoning the epistemic-collapse distinction and accepting infinite branching worlds. Power: institutional (dominant in physics pedagogy and experimental practice). Exit: trapped—the reading directly contradicts their foundational commitments.
 *   - Quantum-mechanics pedagogues: teach quantum mechanics to students and experimental physicists. Must choose which interpretation to frame as primary. Many-worlds creates pedagogical burden (explaining branching, decoherence, entanglement structure, branch identity). Power: moderate to institutional (control curriculum framing). Exit: mobile—can adopt many-worlds, Copenhagen, or agnosticism depending on pedagogical context.
 *   - Experimental physicists: perform quantum measurements and design experiments. The reading is observationally equivalent to Copenhagen under standard laboratory conditions—neither interpretation changes predictions. Power: moderate to powerful (experimental access and data production). Exit: mobile—switching interpretations requires no change to experimental practice or analysis.
 *   - Philosophy of physics scholars: analyze quantum foundations and interpretation questions. Many-worlds reading attracts and repels depending on commitment to parsimony, determinism, or ontological realism. Power: institutional (academic standing, publication influence). Exit: mobile—scholars routinely adopt and abandon interpretations.
 *   - Foundational-physics research community (broadly): benefits from having a purportedly determinist, observer-independent interpretation available. Claim of coordination (solving the measurement problem) benefits the field's epistemology. Power: institutional. Exit: constrained—the field's legitimacy claim partly rests on having a coherent interpretation story.
 *   - Analytical observer (philosophy/physics interface): examines whether the reading's mathematical formalism and empirical claims cohere; whether branching is truly emergent or requires hidden assumption; whether observer-independence is achieved or relocated to branch-identity definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.58).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Interpretation: Universal Determinism via Decoherence").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "epistemology/physics/philosophy_of_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '0f76060f-bd74-4005-bab0-e99a5e56a9fa').
narrative_ontology:cs_kernel_codification('0f76060f-bd74-4005-bab0-e99a5e56a9fa', fixed_text).
narrative_ontology:cs_authority_grounding('0f76060f-bd74-4005-bab0-e99a5e56a9fa', expertise).
narrative_ontology:cs_interpretation_layer_present('0f76060f-bd74-4005-bab0-e99a5e56a9fa').
narrative_ontology:cs_reading_relation('0f76060f-bd74-4005-bab0-e99a5e56a9fa', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f76060f-bd74-4005-bab0-e99a5e56a9fa', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('0f76060f-bd74-4005-bab0-e99a5e56a9fa', foundational, universal_wavefunction_determinism).
narrative_ontology:cs_axiom_status(universal_wavefunction_determinism, holdable).
narrative_ontology:cs_axiom_grounding('0f76060f-bd74-4005-bab0-e99a5e56a9fa', universal_wavefunction_determinism, empirically_contingent).
narrative_ontology:cs_axiom('0f76060f-bd74-4005-bab0-e99a5e56a9fa', foundational, measurement_as_decoherence_emergence).
narrative_ontology:cs_axiom_status(measurement_as_decoherence_emergence, holdable).
narrative_ontology:cs_axiom_grounding('0f76060f-bd74-4005-bab0-e99a5e56a9fa', measurement_as_decoherence_emergence, empirically_contingent).
narrative_ontology:cs_axiom('0f76060f-bd74-4005-bab0-e99a5e56a9fa', secondary, observer_eliminable_from_fundamental_theory).
narrative_ontology:cs_axiom_status(observer_eliminable_from_fundamental_theory, holdable).
narrative_ontology:cs_axiom_grounding('0f76060f-bd74-4005-bab0-e99a5e56a9fa', observer_eliminable_from_fundamental_theory, deontological).
narrative_ontology:cs_axiom('0f76060f-bd74-4005-bab0-e99a5e56a9fa', secondary, all_outcomes_realized_in_separate_worlds).
narrative_ontology:cs_axiom_status(all_outcomes_realized_in_separate_worlds, holdable).
narrative_ontology:cs_axiom_grounding('0f76060f-bd74-4005-bab0-e99a5e56a9fa', all_outcomes_realized_in_separate_worlds, instrumental).
narrative_ontology:cs_reference_frame('0f76060f-bd74-4005-bab0-e99a5e56a9fa', quantum_determinism_framework).
narrative_ontology:cs_drift_state('0f76060f-bd74-4005-bab0-e99a5e56a9fa', contemporary_quantum_foundations_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0f76060f-bd74-4005-bab0-e99a5e56a9fa', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinism_advocates).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_mechanics_ontologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_pedagogues).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, philosophy_of_physics_scholars).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, foundational_physics_field).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, copenhagen_practitioners).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, quantum_pedagogues).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_determinism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, observer_independence).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, relative_state_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicists and philosophers committed to global determinism and classical-like ontology. The reading provides exactly what they seek: a deterministic evolution of reality under Schrödinger equation, no collapse, no indeterminism. They benefit from having a coherent interpretation available; adoption is voluntary and comes with the benefit of foundational elegance. Their cost is accepting infinite branching worlds and committing to a complex metaphysics.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, determinism_advocates, beneficiary,
    institutional, generational, mobile, global).

% Physicists and researchers whose work presupposes collapse as a fundamental physical process and measurement as an irreducible epistemic boundary. The reading directly contradicts their foundational commitments—it denies that measurement produces real indeterminism and treats the observer as part of the quantum system, not a special agent. They bear the cost of refuting the reading's claims or retraining in the new framework. Exit is identity-locked because Copenhagen is foundational to their professional identity and worldview; adopting many-worlds would require fundamental cognitive reorientation.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_practitioners, payer,
    institutional, biographical, identity_locked, global).

% Educators teaching quantum mechanics to undergraduate and graduate students. If they adopt the many-worlds reading as the primary interpretive framework, they must teach decoherence, branching structure, Hilbert-space entanglement, and the subtleties of branch-identity and observer-location. This increases pedagogical complexity and requires extensive conceptual scaffolding. They also benefit from having a determinist ontology to present as conceptually coherent and elegant. Exit is constrained because switching between major interpretations requires reworking curricula and retraining; institutional inertia and textbook availability limit choices.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_pedagogues, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, quantum_pedagogues, beneficiary).

% Researchers performing quantum measurements and designing experiments. The reading is empirically equivalent to Copenhagen and pilot-wave under standard laboratory conditions—measurement outcomes, statistics, and predictions are identical. They do not benefit or suffer from the reading because it does not change experimental practice, data analysis, or predictive power. They observe the theoretical dispute from the pragmatic vantage of 'all interpretations give the same results; I don't need to choose.' Their exit is unconstrained because the reading's adoption or rejection has no experimental consequences.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimental_physicists, observer,
    powerful, biographical, arbitrage, global).

% Academic researchers in philosophy of physics and quantum foundations. The reading provides a rich field for analysis—ontological questions, branch identity, decoherence mechanisms, preferred-basis problems—and benefits the discipline by offering a coherent interpretation to study and critique. Adoption is voluntary and motivated by intellectual interest in foundational coherence. Their cost is accepting the metaphysical extravagance and the unresolved preferred-basis problem. Exit is mobile because scholars routinely work across multiple interpretations.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_physics_scholars, beneficiary,
    institutional, generational, mobile, global).

% The quantum foundations and physics community broadly. The reading benefits the field by providing a purportedly complete, determinist, observer-independent interpretation. This enhances the field's epistemological credibility—the ability to claim that quantum mechanics has a coherent interpretation without collapse or special measurement rules. The community's standing partly rests on having such an interpretation available. Exit is constrained because the field's legitimacy is tied to foundational coherence; abandoning all non-collapse interpretations would raise questions about the theory's meaning.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, foundational_physics_field, beneficiary,
    institutional, generational, constrained, global).

% Analytical position examining the reading's internal coherence, empirical status, and relationship to sibling readings. Does not benefit or pay; observes whether the reading's claims about determinism, observer-independence, and decoherence-emergence are internally consistent and empirically distinguishable from alternatives.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, analyst_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, determinism_advocates).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic ontology for quantum mechanics: a coherent interpretation of the Schrödinger equation that restores global determinism, eliminates the collapse postulate, and removes observer-dependence from the wavefunction. Solves the measurement problem by treating apparent indeterminism as an artifact of observer perspective within branching worlds.
% TRANSFER_FUNCTION: Moves ontological commitment from those seeking pragmatic indifference (Copenhagen) to those seeking foundational determinism. Transfers the burden of infinite-world metaphysics and preferred-basis indeterminacy to adopters. Transfers conceptual complexity from experimental practice (where Copenhagen is pragmatic) to foundational theory (where many-worlds is elaborated).
% ABSENT_VOICES: Pragmatist physicists who find the interpretation question uninteresting because all interpretations give identical predictions—they would object to the imposition of ontological commitment where empirical equivalence licenses indifference. Quantum-information researchers who use the formalism without caring about interpretation. Classical physicists or anti-realists who deny the wavefunction is a real physical entity (in any interpretation)—the reading assumes realism about the wavefunction. These voices are structurally excluded from foundational discussions; experimental pragmatism is their escape route.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished, quantum mechanics would continue functioning identically under Copenhagen or pilot-wave or agnostic interpretations. Experimental predictions, measurement outcomes, and practical physics would be unchanged. No rearrangement of the physical world would occur. However, the foundational coherence of quantum mechanics would be disputed: determinism advocates would lose their preferred interpretation and would either adopt pilot-wave (with its classical hidden variables) or accept indeterminism. The theoretical elegance and unification claim ('one universal wavefunction evolving deterministically') would be unavailable. The field's ability to claim it has solved the measurement problem would be weakened. The controversy is real: those committed to foundational determinism see a significant loss; pragmatists see no loss.
% FOUNDING_PROBLEM: Quantum mechanics lacked a coherent interpretation that preserved global determinism, eliminated collapse as a fundamental postulate, and treated the observer as part of the quantum system rather than a special measuring agent. The measurement problem: how does the apparent indeterminism of measurement outcomes coexist with the deterministic Schrödinger equation?
% FOUNDING_PROBLEM_CORROBORATION: Everett (1957) and his successors (DeWitt, Wallace) attest that the problem is live and the reading solves it. Copenhagen practitioners (Bohr, Heisenberg tradition) attest that the problem is false—measurement indeterminism is real and fundamental, not apparent. Pragmatist physicists attest that the problem is a non-issue for experimental practice. Philosophers of physics (Albert, Wallace, Deutsch, Vaidman) analyze the problem's status; outside the quantum-foundations community, no broad corroboration exists. The corroboration is internal to the discipline; external arbitration is absent.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).

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
 *   Extractiveness (0.58 at interval end) captures that adoption requires accepting substantial conceptual costs: infinite worlds, branch indeterminacy, preferred-basis problem, no clear answer to 'which branch am I in?' at the level of fundamental theory. The reading offers genuine coordination benefit (deterministic ontology, observer independence, unified evolution equation) but the cost of that coordination is borne asymmetrically: those committed to determinism or classical ontology bear the cost of ontological extravagance; those satisfied with Copenhagen's pragmatism do not. Suppression (0.42) reflects moderate enforcement: many-worlds is taught in some programs, suppressed in others; experimental practice is indifferent (measurement outcomes identical); the suppression is primarily pedagogical and epistemic—alternative framings (Copenhagen, agnosticism) are available and widely adopted, so no coercive barrier prevents exit, but the reading's foundational prestige within some research communities creates inertia. Theater (0.31 and rising) reflects a trend: the reading's performative maintenance is growing—papers on decoherence and branching structure increasingly focus on mathematical elegance and philosophical coherence-building rather than empirical evidence, because empirical evidence is identical across readings. Accessibility_collapse (0.68) is moderate-high because understanding decoherence and branching structure requires technical expertise; most practitioners can escape into laboratory pragmatism (don't choose an interpretation). Resistance (0.54) reflects sustained objections from Copenhagen practitioners and from scholars skeptical of infinite ontologies—the reading meets real philosophical and conceptual resistance, not just adoption friction.
 *
 * PERSPECTIVAL GAP:
 *   The reading should diverge sharply when computed from different seats. From the determinism-advocate's seat: the constraint is a genuine rope (solves a real problem, coordination benefit, modest cost). From the Copenhagen practitioner's seat: the constraint is a snare (forced acceptance of an interpretation contradicting their foundational position, suppressed via pedagogy and publication pressure, no benefit to them, all cost). From the experimental physicist's seat: the constraint is closer to a mountain (empirically equivalent options, the reading is transparent to experimental practice, choice is philosophically optional). The engine computes these divergences from the structural data—the story's authored claim (rope) represents the determinism-advocate's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Determinism advocates benefit from the reading (low d toward beneficiary end, ~0.25-0.35): the reading solves their foundational problem by restoring global determinism and eliminating collapse indeterminism. Quantum-mechanics pedagogues are moderately targeted (d ~0.55-0.65): they must teach decoherence, branching, and entanglement, incurring pedagogical complexity, while gaining the benefit of a determinist ontology story. Copenhagen practitioners are substantially targeted (d ~0.75-0.85): the reading directly contradicts their foundational commitments (collapse is real, measurement is fundamental), forces conceptual reworking, and offers them no corresponding benefit. The foundational-physics research community is weakly coordinated (d ~0.45): the reading provides a valid interpretation and theoretical coherence, genuine coordination goods, but also carries the cost of ontological extravagance that some members refuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem: quantum mechanics lacked a coherent deterministic interpretation that eliminated collapse indeterminism and observer-dependence. The reading was developed (Everett 1957) to solve that problem. The founding-problem status is contested: determinism advocates say the problem is live and the reading solves it; Copenhagen practitioners say the problem is false (measurement IS fundamental, indeterminism is real); pragmatists say the problem is dead (empirical practice is indifferent to interpretation). The disappearance verdict: if the many-worlds reading vanished, theoretical physicists would continue using Copenhagen or pilot-wave or agnosticism—no rearrangement of experimental practice or empirical results. The theoretical structure of quantum mechanics would remain intact. Only the ontological framing would change. This suggests the constraint is performing a role (providing one of several ontologically coherent stories) rather than solving a coordination problem that would otherwise collapse. The mandatrophy analysis: the reading is not mandatropic in the strict sense because its founding problem is genuinely contested and coherence-building work continues (branching structure papers, decoherence analysis). But there is a theater-ratio rise, suggesting performative maintenance is increasing—more papers on the elegance of the ontology, fewer on empirical distinguishability. A plausible intermediate state: the reading is transitioning from rope (genuine coordination on determinism) toward piton (maintained mostly by elegance and theoretical fashion, not by solving an urgent coordination problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_decoherence_emergence,
    'Is decoherence-induced apparent branching a fundamental feature of the formalism, or does it emerge only from coarse-graining and measurement apparatus descriptions? Does this reading''s core claim require decoherence to be primitive physics or can it be derived from the universal wavefunction?',
    'Formalization of decoherence in first-principles quantum mechanics; mathematical proof of whether branching structure follows uniquely from Schrödinger equation or requires ancillary assumptions about environment/apparatus. Analysis of whether the derivation assumes the observer or apparatus-as-classical.',
    'If decoherence is derived and requires no special observer status, the reading''s claim is self-consistent — measurement emerges from the universal evolution. If decoherence requires background assumptions about macroscopic classical behavior, the reading shifts toward Copenhagen-like hidden circularity (measuring apparatus treated as classical to explain measurement). If decoherence is primitive, the reading''s claim to pure determinism is weakened (decoherence adds new physics beyond Schrödinger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_decoherence_emergence, conceptual, 'Whether decoherence grounds the reading''s measurement story or introduces unstated premises.').

omega_variable(
    reading_identity_ontological_cost,
    'The reading commits to all outcomes being realized in separate worlds — this is ontological extravagance. Is the infinite-branching multiverse empirically equivalent to Copenhagen (indistinguishable from outside any single branch) but carrying higher metaphysical cost? Or is the multiverse claim doing explanatory work that Copenhagen cannot?',
    'Proof of empirical equivalence class between readings; analysis of what explanatory or conceptual work the multiverse solves that collapse interpretations cannot. Examination of whether the reading''s appeal rests on metaphysical elegance (determinism) or explanatory superiority.',
    'If empirically identical but ontologically costlier, the reading is a metaphysical choice, not a scientific inference — it may shift toward snare classification if the cost is borne by those adopting it while the elegance benefit accrues to advocates. If explanatorily superior (e.g., solves the preferred-basis problem without additional axioms), the reading''s claim to coordination is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ontological_cost, preference, 'Whether the many-worlds framework makes explanatory claims Copenhagen cannot, or is a more-costly equivalent formulation.').

omega_variable(
    reading_identity_branch_reality_status,
    'In this reading, are unobserved branches equally real to the branch an observer finds themselves in? If so, what makes one branch real for a given observer — does observer location in Hilbert space (entanglement structure) define reality, reintroducing observer-dependence by the back door?',
    'Formal definition of branch identity and reality status in the reading''s preferred mathematical formulation (Everett''s original, Wallace''s decision-theoretic derivation, or others). Analysis of whether the definition preserves observer-independence or reintroduces it via entanglement structure. Empirical test via measurement experiments that would produce different branch counts under different interpretations of reality status.',
    'If branches are equally real independent of observation, the reading is consistent and observer-eliminable. If branch reality depends on observer location or entanglement perspective, the reading reintroduces observer-dependence in disguised form — it shifts toward Copenhagen-like observer-centrality and loses its claimed advantage over Copenhagen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_branch_reality_status, conceptual, 'Whether the reading truly eliminates observer-dependence or relocates it to the branch-reality definition.').

omega_variable(
    constraint_family_kernel_reading_sibling_distinction,
    'This constraint is one reading of the quantum_formalism kernel. The sibling readings (Copenhagen, pilot-wave) propose fundamentally different roles for the wavefunction (epistemic vs. ontic tool vs. physical field) and measurement (collapse vs. derivative vs. guidance interaction). How does this reading''s mathematical formalism disambiguate which sibling is correct? Is the disambiguation empirical, conceptual, or metaphysical?',
    'Ensemble of experiments designed to distinguish the readings (Bell tests, weak measurement, decoherence timescale experiments, quantum Zeno effects). Formalization of what each reading''s mathematics commits to and whether the commitments are logically independent or mutually exclusive. Analysis of whether present empirical data rules out any sibling.',
    'If empirically distinguishable, future evidence may foreclose a sibling or this reading. If indistinguishable, the readings coexist as rival interpretations, all Poincaré-equivalent, differing only in metaphysical commitment. This determines whether the reading_relations are forecloses, influences, or coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constraint_family_kernel_reading_sibling_distinction, empirical, 'Whether the readings are Poincaré-equivalent interpretations or empirically distinguishable theories with different ontological commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t5, quantum_formalism__many_worlds_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(quan_tr_t5, observed).
narrative_ontology:measurement(quan_tr_t10, quantum_formalism__many_worlds_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(quan_tr_t10, observed).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__many_worlds_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(quan_tr_t15, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__many_worlds_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(quan_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t5, quantum_formalism__many_worlds_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(quan_be_t5, observed).
narrative_ontology:measurement(quan_be_t10, quantum_formalism__many_worlds_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(quan_be_t10, observed).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__many_worlds_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(quan_be_t15, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__many_worlds_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(quan_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t5, quantum_formalism__many_worlds_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(quan_su_t5, observed).
narrative_ontology:measurement(quan_su_t10, quantum_formalism__many_worlds_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(quan_su_t10, observed).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__many_worlds_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(quan_su_t15, observed).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__many_worlds_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(quan_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.12).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel is decomposed into three separate constraint stories, one per reading: many_worlds_reading (this story), copenhagen_reading, and pilot_wave_reading. Each reading interprets the same fixed Schrödinger equation differently. The readings are structurally independent (each ε-invariant, each with its own beneficiary/victim structure) but linked via network.affects_constraints because adoption of one reading creates institutional pressure on the others. Empirical equivalence prevents forelosure; ontological commitment differences prevent simple coexistence. See commentary.kernel_context for the structural relationship between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
