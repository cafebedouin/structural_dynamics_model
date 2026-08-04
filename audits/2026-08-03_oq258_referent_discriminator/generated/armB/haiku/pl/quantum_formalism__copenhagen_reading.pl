% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Wavefunction Collapse as Irreducible Measurement Boundary (Copenhagen Reading)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Copenhagen reading of quantum mechanics holds that wavefunction
 *   collapse is a physical process occurring at measurement, marking an
 *   absolute epistemic boundary between quantum and classical domains.
 *   Measurement is treated as a primitive ontological category; the observer
 *   role becomes non-eliminable; determinism is abandoned irreducibly at
 *   measurement events. This reading has become institutionally
 *   canonical—taught in standard textbooks, embedded in physics curricula,
 *   and enforced through journal gatekeeping and funding structures. The
 *   reading extracts from determinism-defending researchers and
 *   alternative-interpretation researchers, who must continuously defend
 *   their work against institutional weight. It coordinates pedagogical
 *   practice and provides a unified formalism for prediction-making (the
 *   genuine coordination benefit). The constraint is CLAIMED as tangled_rope:
 *   genuine coordination function (a unified teachable formalism) coupled
 *   with asymmetric extraction (alternative interpretations face
 *   institutional resistance). The measurement series track extractiveness
 *   rising from 0.45 to plateau near 0.68, theater rising from 0.28 to 0.42
 *   (indicating increasing performative maintenance of the framework despite
 *   empirical saturation), and suppression requirement steady around 0.71
 *   (constant gatekeeping effort).
 *
 * KEY AGENTS:
 *   - copenhagen_interpretation_research_community: institutional beneficiary and agenda-setter; sets curriculum standards, controls journals, maintains the measurement doctrine as settled
 *   - determinism_advocates: powerful payers; face institutional resistance to research programs based on hidden-variable or deterministic foundations
 *   - alternative_interpretation_researchers: moderate payers with identity-lock; pay continuous legitimacy tax and publication friction; exit feels like abandoning disciplinary identity
 *   - quantum_mechanics_students: powerless beneficiaries/payers; benefit from unified standard but pay by treating contestable framework as brute fact
 *   - philosophical_foundations_community: structurally excluded; would challenge 'measurement' as theoretical primitive but lack institutional standing in physics
 *   - experimental_physics_community: observers; formally interpretation-agnostic but see Copenhagen entrenched in explanatory practice despite empirical predictions being interpretation-independent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Wavefunction Collapse as Irreducible Measurement Boundary (Copenhagen Reading)").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '283c8585-c201-419e-b659-673e99d34946').
narrative_ontology:cs_kernel_codification('283c8585-c201-419e-b659-673e99d34946', fixed_text).
narrative_ontology:cs_authority_grounding('283c8585-c201-419e-b659-673e99d34946', extraction).
narrative_ontology:cs_interpretation_layer_present('283c8585-c201-419e-b659-673e99d34946').
narrative_ontology:cs_reading_relation('283c8585-c201-419e-b659-673e99d34946', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('283c8585-c201-419e-b659-673e99d34946', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('283c8585-c201-419e-b659-673e99d34946', foundational, measurement_ontologically_primitive).
narrative_ontology:cs_axiom_status(measurement_ontologically_primitive, holdable).
narrative_ontology:cs_axiom_grounding('283c8585-c201-419e-b659-673e99d34946', measurement_ontologically_primitive, empirically_contingent).
narrative_ontology:cs_axiom('283c8585-c201-419e-b659-673e99d34946', foundational, indeterminism_irreducible_at_collapse).
narrative_ontology:cs_axiom_status(indeterminism_irreducible_at_collapse, holdable).
narrative_ontology:cs_axiom_grounding('283c8585-c201-419e-b659-673e99d34946', indeterminism_irreducible_at_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('283c8585-c201-419e-b659-673e99d34946', copenhagen_measurement_primitive).
narrative_ontology:cs_drift_state('283c8585-c201-419e-b659-673e99d34946', contemporary_quantum_gravity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('283c8585-c201-419e-b659-673e99d34946', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_community).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, determinism_advocates).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, quantum_mechanics_students).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, quantum_mechanics_students).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_primacy_doctrine).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_centrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physics departments, quantum mechanics textbooks, and pedagogical standards center Copenhagen interpretation as the canonical formalism. Researchers working within this framework benefit from institutional legitimacy, textbook canonicity, and lack of pressure to justify the measurement framework. They set the standard through journal gatekeeping, curriculum design, and the burden-of-proof allocation: alternative readings must demonstrate 'why not Copenhagen' rather than Copenhagen demonstrating 'why yes.' The measurement boundary doctrine is taught as settled physics, not as one contestable reading.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_community, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_community, beneficiary).

% Physicists and philosophers who hold that quantum phenomena must have deterministic foundations (classical determinism preserved at a deeper level). The Copenhagen reading's irreducible indeterminism at measurement forecloses their research program unless they abandon the conviction that nature is fundamentally deterministic. Their work must constantly defend itself against the institutional weight of Copenhagen canonicity; funding, publication, and career advancement remain constrained by the question 'but doesn't Copenhagen already settle this?' They cannot exit the physics establishment without abandoning their disciplinary identity.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, determinism_advocates, payer,
    powerful, generational, constrained, global).

% Researchers developing pilot-wave, many-worlds, objective collapse, or relational interpretations face a two-tier publication and funding landscape: Copenhagen-defending journals dominate; alternative-framework papers are often desk-rejected or sent to specialists who hold the orthodox view. They pay a continuous enforcement cost: defending basic conceptual premises that should be axiomatic (e.g., 'Does the universal wavefunction really exist?'), replicating results in Copenhagen language, and building citations against institutional inertia. Their identity as physicists is constituted partly through their commitment to their reading; exit from the research program feels like losing disciplinary standing.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Learn quantum mechanics exclusively through Copenhagen formalism: measurement collapses the state, indeterminism is fundamental, wavefunction is an epistemic tool or a physical entity depending on the lecturer's mood. Students benefit from a unified standard (no need to learn multiple formulations to pass exams and enter the field). They also pay: the measurement boundary is presented as a brute fact rather than one among competing interpretations; questions about determinism or alternative ontologies are treated as philosophical distractions from 'real physics.' Their constraint-facing options are limited to acceptance (to advance in physics) or exit (to another discipline).
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_mechanics_students, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, quantum_mechanics_students, payer).

% Philosophers of science and metaphysicians who work on quantum foundations would argue vigorously that the measurement problem is EXACTLY the subject matter of philosophy and that the Copenhagen reading's treatment of measurement as primitive is a philosophical move, not a physics discovery. They are excluded from the gatekeeping: physics journals rarely publish philosophical analysis of foundations; philosophical journals have low standing in physics departments. Their exclusion is structural: the constraint's enforcement depends on keeping the question out of the space where it would be thoroughly debated (philosophy departments). Were they in the room, they would immediately challenge the treatment of 'measurement' as a theoretical primitive and demand rigorous definition—which would expose the circularity.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophical_foundations_community, excluded,
    moderate, generational, trapped, global).

% Conducts Bell tests, quantum computing implementations, and precision measurement experiments. They are formally agnostic about interpretation: all interpretations give identical empirical predictions for standard experiments (within current experimental resolution). They observe that the Copenhagen reading has become so entrenched that alternative interpretations are often not discussed when explaining experimental results. They can measure the constraint's enforcement but have no institutional leverage to change it; their power is in generating data, not in shaping the theoretical framework that explains that data.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, experimental_physics_community, observer,
    institutional, biographical, analytical, global).

% Face an acute version of the measurement problem at Planck scales, where there is no external observer; the Copenhagen framework's reliance on external measurement becomes incoherent. They are partially excluded from the conversation: quantum gravity is technically separate from quantum foundations, so their objections to Copenhagen's measurement framework are treated as technical problems for quantum gravity to solve, not as conceptual objections to Copenhagen itself. Their attempts to reformulate quantum mechanics without a measurement primitive often face reception as 'not really physics' because they violate the standard framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_gravity_researchers, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_community).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, teachable formalism for making predictions about quantum systems: specify initial conditions, apply the Schrödinger equation until measurement, collapse the state according to Born rule, obtain probabilities for measurement outcomes. The framework solves the apparent problem of translating between quantum and classical descriptions by institutionalizing the boundary: the quantum domain ends where measurement begins.
% TRANSFER_FUNCTION: Transfers authority over the interpretation of quantum phenomena from alternative frameworks to the Copenhagen consensus. The mechanism: canonical textbooks, standard curricula, journal gatekeeping, and the burden-of-proof allocation (alternative readings must prove 'why not Copenhagen' rather than Copenhagen proving itself). Researchers working in alternative interpretations pay a continuous cost in legitimacy, funding visibility, and publication friction. The beneficiary is the Copenhagen research community, which gains institutional certainty and relief from having to justify the measurement framework.
% ABSENT_VOICES: Philosophers of science and metaphysicians are structurally excluded: their expertise in analyzing conceptual foundations and primitive notions would immediately surface the circularity in treating 'measurement' as undefined primitive. Quantum gravity researchers face partial exclusion: their arguments that Copenhagen breaks down at Planck scales are treated as technical problems for gravity theorists, not as conceptual objections to Copenhagen. Students questioning the framework are present but powerless.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading's claim to canonical status vanished overnight, quantum mechanics pedagogy would need to teach multiple interpretations with equal standing; research funding and journal gatekeeping would diversify; the measurement boundary would become an explicit interpretive choice rather than settled physics. Alternative interpretation researchers would no longer pay the institutional tax. The physics community would fragmentize into local consensus communities rather than a single global standard. The unified formalism would remain as a mathematical tool, but its claim to represent reality (or the boundary of knowability) would become explicitly interpretive and contested.
% FOUNDING_PROBLEM: Quantum mechanics in the 1920s produced uninterpretable formal machinery: the formalism worked perfectly for predictions but offered no account of what was 'really happening.' Measurement outcomes appeared random; the wave-particle duality was baffling; the role of the observer was unclear. Copenhagen provided a pragmatic solution: don't ask what is really happening; measurement marks an absolute boundary between quantum and classical; indeterminism is irreducible; the wavefunction is a tool for making predictions, not a description of physical reality.
% FOUNDING_PROBLEM_CORROBORATION: The Copenhagen research community attests the founding problem is live: quantum mechanics still requires a measurement framework to make sense. Determinism advocates and alternative-interpretation researchers attest the problem is foundational and unsolved: Copenhagen merely labeled the problem 'measurement' without explaining what measurement IS. Philosophers of science (excluded from institutional conversation) attest in their own journals that Copenhagen's treatment of measurement as primitive is exactly the problem that needs solving, not the solution. Quantum gravity researchers' published work demonstrates the problem becomes acute at scales where Copenhagen breaks down—their work, though not consistently cited in foundations discussions, provides independent corroboration that the founding problem has shifted, not been solved.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 endpoint) is high because the constraint systematically privileges one reading and imposes legitimacy costs on alternatives. The constraint is not empirically distinguishable from alternatives (all give identical predictions for standard experiments within current resolution), so the persistence depends on institutional enforcement rather than empirical advantage. Suppression (0.71) is substantial because maintaining the constraint requires active gatekeeping: excluding alternative readings from journals, burdening alternative researchers with 'prove your reading' rather than Copenhagen proving itself, treating foundational questions as settled, and excluding philosophers from the conversation where such questions would be rigorously analyzed. Theater (0.42 rising to plateau) indicates a growing fraction of enforcement activity defends the framework's legitimacy rather than its empirical function—as alternative interpretations produce empirically equivalent but conceptually cleaner frameworks, Copenhagen's enforcement must become increasingly pedagogical and gate-keeping rather than empirical. Accessibility collapse (0.81) is high because alternative frameworks are mathematically equivalent to Copenhagen but presented as exotic alternatives rather than canonical options; students and researchers face a steep learning cost to even understand alternatives exist as legitimate options. Resistance (0.58) is moderate: significant voices (determinism advocates, some quantum-gravity researchers, philosophers of science) resist the constraint, but resistance remains somewhat fragmented institutionally—alternative interpretations have research communities but lack institutional power equivalent to Copenhagen.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Copenhagen research community) experiences the constraint as a unified formalism that makes quantum mechanics teachable—genuine coordination benefit. From their position, the constraint is rope: everyone benefits from a standard language. The payer seats experience it as institutional pressure suppressing alternatives: they must justify themselves against canonical assumptions. The determinism advocate seat reads it as foreclosing their research program by treating indeterminism as irreducible rather than as failure-to-find-hidden-variables. The alternative-interpretation seat reads it as a two-tier publication system: Copenhagen papers get standard treatment; alternatives must prove 'why this over Copenhagen' in every paper. The engine computes per-seat classification from the structural data; the perspectival gap is encoded in the beneficiary/victim structure and the asymmetric exit options (Copenhagen researchers have arbitrage, alternatives have identity-lock). The claimed type (tangled_rope) sits between the beneficiary-seat reading (coordination + modest extraction) and the payer-seat reading (pure extraction). The metrics support this: genuine coordination function exists (a unified formalism that works), but extraction is substantial enough that the constraint's persistence depends on active enforcement, not voluntary participation by payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Copenhagen institutional community: beneficiary role, institutional power, arbitrage-grade exit (can shift to other areas of physics), global scope → d near 0.1 (strong beneficiary position). They can exit this particular constraint's enforcement without losing disciplinary standing; alternative frameworks don't require them to do anything, just to stop gatekeeping. Determinism advocates: payer role, powerful-but-specific power (intellectual capital in deterministic approaches), constrained exit (cannot exit physics without losing disciplinary identity but can exit determinism research), global scope → d near 0.75 (substantial target). Their work is continuously challenged by the institutional default; they cannot credibly pursue determinism within physics departments that treat Copenhagen indeterminism as settled. Alternative-interpretation researchers: payer role, moderate power, identity_locked exit (disciplinary identity fused with their interpretation), global scope → d near 0.82 (very high target). Their professional identity is constituted partly through their commitment to their reading; publication friction and legitimacy costs hit hardest at identity-locked targets because exit means losing the framework through which they understand themselves as physicists. Students: beneficiary+payer (benefit from unified standard, pay by being told the framework is brute fact), powerless, constrained exit, global scope → d near 0.6 (moderate-to-target, below determinism advocates because they have a genuine coordination benefit). No directionality override is needed; the derivation from role+power+exit yields accurate positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem—'How do we interpret quantum mechanics when the formalism works but makes no sense?'—was live in the 1920s. The Copenhagen reading solved part of it: provided a unified, teachable formalism that produced correct predictions. But the founding problem itself (What IS measurement? What IS the wavefunction?) was not solved; it was labeled and bracketed. The contemporary status is contested: Copenhagen researchers attest the founding problem is live (quantum mechanics still needs a measurement framework); alternative researchers and philosophers attest the founding problem is exactly what Copenhagen failed to solve and is now obscured by institutional canonicity. The mandatrophy (mandate outliving function) is partial but emerging: the unified formalism still functions (predictions still work), but the pedagogical burden of maintaining it against alternative frameworks is rising (theater ratio rising toward 0.42). If alternatives continue producing empirically equivalent but conceptually cleaner ontologies, Copenhagen's mandate will increasingly ride on institutional gatekeeping rather than on solving the founding problem. The measurement series show extractiveness plateauing around 0.68 while theater continues rising—classic trajectory for a constraint whose coordination function is intact but whose enforcement is increasingly performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_primitivity_vs_definition,
    'Is ''measurement'' a primitive ontological category that cannot be further reduced, or is this status adopted for convenience to avoid deeper analysis?',
    'Rigorous philosophical analysis (excluded from current physics-department gatekeeping) of what ''measurement'' means in quantum mechanics, with comparison to how other interpretations handle the same physical processes without treating measurement as primitive. Alternatively: quantum gravity at Planck scales, where the Copenhagen ''external observer'' assumption breaks down, forcing a definition of measurement in the absence of an external classical realm.',
    'If ''measurement'' can be rigorously defined in terms of other physical processes, Copenhagen''s foundational claim dissolves and alternatives (many-worlds, pilot-wave) become more competitive. If measurement truly is primitive, the question shifts to why this particular boundary exists and how it relates to consciousness, decoherence, or wavefunction ontology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_primitivity_vs_definition, conceptual, 'Whether measurement is foundational or whether treating it as foundational is a theoretical move that avoids answering deeper questions.').

omega_variable(
    empirical_distinguishability_of_interpretations,
    'Can current or near-future experiments distinguish between Copenhagen, many-worlds, and pilot-wave interpretations, or are they provably empirically equivalent for all physically measurable quantities?',
    'Systematic search for post-selection effects, quantum computing implementations, precision measurements at decoherence thresholds, or quantum gravity phenomenology that would produce different predictions. Alternatively: formal proof that the interpretations are empirically indistinguishable within the operational framework.',
    'If interpretations remain empirically indistinguishable, Copenhagen''s institutional dominance is purely a choice of framework (a legitimate choice, but a choice), and the constraint''s extraction becomes transparently institutional gatekeeping rather than empirical winnowing. If experiments distinguish, Copenhagen may face empirical displacement; if Copenhagen prevails empirically, its institutional dominance becomes justified and extractiveness drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_distinguishability_of_interpretations, empirical, 'Whether the constraint''s persistence is empirically grounded or purely institutional.').

omega_variable(
    observer_role_coherence_across_scales,
    'At what physical and organizational scales does the Copenhagen requirement for an ''observer'' or ''measurement apparatus'' remain coherent? Does the framework break down at quantum gravity scales, or at earlier stages?',
    'Rigorous analysis of decoherence thresholds, quantum gravity boundary conditions, and the emergence of classicality. Empirical investigation of whether macroscopic systems genuinely decohere to classical states or whether Copenhagen is incomplete at large scales.',
    'If Copenhagen''s observer requirement breaks down at scales where quantum gravity is relevant, the framework is foundationally incomplete and alternatives gain plausibility. If Copenhagen holds across all empirical scales, its claim to universality is strengthened. The constraint''s extractiveness depends partly on whether it appears as a local choice (valid for particle physics, breaks down at gravity) or a universal law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_role_coherence_across_scales, empirical, 'Whether the Copenhagen reading remains coherent across all physical scales or only in restricted domains.').

omega_variable(
    institutional_gatekeeping_as_enforcement_mechanism,
    'To what extent does the constraint''s persistence depend on institutional enforcement (journal gatekeeping, curriculum control, funding structures) rather than on the constraint''s inherent explanatory power?',
    'Historical analysis of textbook evolution, citation patterns in foundational vs. applied quantum mechanics, journal acceptance rates for alternative interpretations. Counterfactual: if physics departments adopted a ''teach all interpretations equally'' curriculum, would Copenhagen remain dominant, or would alternatives gain traction?',
    'If institutional enforcement is the primary mechanism (theater_ratio rising as extractiveness plateaus suggests this), then reducing gatekeeping would make the constraint transparent as a choice rather than a discovery. If Copenhagen''s explanatory power is the driver, removing gatekeeping would have little effect. This omega determines whether the constraint is fundamentally snare-like (extraction disguised as coordination) or genuinely tangled_rope (real coordination with institutional overlay).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_gatekeeping_as_enforcement_mechanism, empirical, 'Whether the constraint''s persistence is primarily institutional gatekeeping or primarily superior explanatory performance.').

omega_variable(
    committer_contest_structure,
    'Does each reading of quantum_formalism represent a genuinely different physical claim, or do they represent different choices about what to take as primitive (measurement vs. branches vs. particle positions)?',
    'Formal analysis of the relationship between interpretation choice and observable prediction. Philosophical analysis of whether these are competing metaphysical claims (different physics) or different descriptions of the same physics. Examination of whether one interpretation can be reformulated as the other by redefining observables.',
    'If they are genuinely different physical claims, the contest is real and one might be empirically displaced. If they are equivalent descriptions under different observable choices, the contest is purely conventional and the constraint''s institutional dominance is a choice of convention rather than a discovery of truth. This omega documents the most basic uncertainty about the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_contest_structure, conceptual, 'Whether the contest between quantum interpretations is about different physics or different descriptions of the same physics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t14, quantum_formalism__copenhagen_reading, theater_ratio, 14, 0.31).
narrative_ontology:measurement_basis(quan_tr_t14, observed).
narrative_ontology:measurement(quan_tr_t28, quantum_formalism__copenhagen_reading, theater_ratio, 28, 0.34).
narrative_ontology:measurement_basis(quan_tr_t28, observed).
narrative_ontology:measurement(quan_tr_t42, quantum_formalism__copenhagen_reading, theater_ratio, 42, 0.38).
narrative_ontology:measurement_basis(quan_tr_t42, observed).
narrative_ontology:measurement(quan_tr_t56, quantum_formalism__copenhagen_reading, theater_ratio, 56, 0.41).
narrative_ontology:measurement_basis(quan_tr_t56, observed).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__copenhagen_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement_basis(quan_tr_t70, observed).
narrative_ontology:measurement(quan_tr_t100, quantum_formalism__copenhagen_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(quan_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t14, quantum_formalism__copenhagen_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement_basis(quan_be_t14, observed).
narrative_ontology:measurement(quan_be_t28, quantum_formalism__copenhagen_reading, base_extractiveness, 28, 0.59).
narrative_ontology:measurement_basis(quan_be_t28, observed).
narrative_ontology:measurement(quan_be_t42, quantum_formalism__copenhagen_reading, base_extractiveness, 42, 0.64).
narrative_ontology:measurement_basis(quan_be_t42, observed).
narrative_ontology:measurement(quan_be_t56, quantum_formalism__copenhagen_reading, base_extractiveness, 56, 0.67).
narrative_ontology:measurement_basis(quan_be_t56, observed).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__copenhagen_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(quan_be_t70, observed).
narrative_ontology:measurement(quan_be_t100, quantum_formalism__copenhagen_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(quan_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t14, quantum_formalism__copenhagen_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement_basis(quan_su_t14, observed).
narrative_ontology:measurement(quan_su_t28, quantum_formalism__copenhagen_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement_basis(quan_su_t28, observed).
narrative_ontology:measurement(quan_su_t42, quantum_formalism__copenhagen_reading, suppression_requirement, 42, 0.68).
narrative_ontology:measurement_basis(quan_su_t42, observed).
narrative_ontology:measurement(quan_su_t56, quantum_formalism__copenhagen_reading, suppression_requirement, 56, 0.7).
narrative_ontology:measurement_basis(quan_su_t56, observed).
narrative_ontology:measurement(quan_su_t70, quantum_formalism__copenhagen_reading, suppression_requirement, 70, 0.71).
narrative_ontology:measurement_basis(quan_su_t70, observed).
narrative_ontology:measurement(quan_su_t100, quantum_formalism__copenhagen_reading, suppression_requirement, 100, 0.71).
narrative_ontology:measurement_basis(quan_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.12).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story instantiates the Copenhagen reading of the quantum_formalism kernel. Sibling stories (many_worlds_reading, pilot_wave_reading) are separate constraints with distinct ε values, beneficiary structures, and institutional positions. The three readings coexist as live alternatives but occupy different seats in physics departments and alternative communities. This story's ε (0.68) reflects the extractive institutional gatekeeping required to maintain Copenhagen's canonical status despite empirical equivalence with alternatives. Many-worlds_reading has lower institutional extraction (empirically equivalent, less gatekeeping required, lower suppression) but higher conceptual extraction (requires commitment to branch existence). Pilot_wave_reading has similar institutional extraction but different beneficiary structure (determinism-advocates benefit; Copenhagen researchers lose their irreducibilist claim). The readings are linked through network.affects_constraints so that coupling and contamination analysis can trace how institutional dominance in one reading cascades to suppress alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
