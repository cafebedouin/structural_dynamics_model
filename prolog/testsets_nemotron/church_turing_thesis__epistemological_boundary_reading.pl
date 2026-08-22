% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Formal Computability
 *   domain: philosophy/mathematics/computation
 *
 * SUMMARY:
 *   This constraint story instantiates the epistemological_boundary_reading
 *   of the Church-Turing thesis kernel: the thesis marks the boundary of
 *   formally knowable computation — functions we can prove computable are
 *   exactly Turing-computable, regardless of physical possibility. This
 *   reading treats the thesis as a methodological constraint on what counts
 *   as a valid computability proof within formal mathematics. It is NOT a
 *   claim about physics (that is the physical_claim_reading) nor a mere
 *   stipulative definition (that is the mathematical_definition_reading). The
 *   constraint operates by defining the standards of evidence for
 *   computability claims: non-constructive existence proofs, oracle-based
 *   models, and hypercomputation proposals fall outside the boundary of
 *   'formally knowable' computation. Beneficiaries include proof theorists
 *   and formal methods practitioners who gain a stable, well-defined
 *   framework. Victims are non-constructive computability claims and
 *   hypercomputation models that are methodologically excluded. The
 *   constraint claims mountain type (emerges_naturally=true) but declares
 *   beneficiaries, making it an FSM candidate — the omega variables document
 *   the natural-law vs. constructed ambiguity.
 *
 * KEY AGENTS:
 *   - proof_theorists: Primary beneficiary (powerful/analytical) — gain stable framework for computability proofs
 *   - formal_methods_practitioners: Secondary beneficiary (organized/biographical) — gain reliable foundations for verification
 *   - computer_science_foundations: Institutional beneficiary (institutional/generational) — discipline-defining boundary
 *   - non_constructive_computability_claims: Primary victim (analytical/analytical) — excluded by methodological fiat
 *   - hypercomputation_proposals: Victim (organized/constrained) — research programs excluded from mainstream computability
 *   - oracle_machine_models: Victim (analytical/analytical) — technically valid but epistemologically excluded
 *   - analytical_observer: Observer (analytical/analytical) — sees full structural landscape across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.28).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, mountain).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Formal Computability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy/mathematics/computation").

domain_priors:emerges_naturally(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '14de14e4-2f78-4995-b411-6b34ca3a2f7a').
narrative_ontology:cs_kernel_codification('14de14e4-2f78-4995-b411-6b34ca3a2f7a', formalized).
narrative_ontology:cs_authority_grounding('14de14e4-2f78-4995-b411-6b34ca3a2f7a', expertise).
narrative_ontology:cs_interpretation_layer_present('14de14e4-2f78-4995-b411-6b34ca3a2f7a').
narrative_ontology:cs_reading_relation('14de14e4-2f78-4995-b411-6b34ca3a2f7a', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('14de14e4-2f78-4995-b411-6b34ca3a2f7a', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('14de14e4-2f78-4995-b411-6b34ca3a2f7a', foundational, computability_requires_constructive_witness).
narrative_ontology:cs_axiom_status(computability_requires_constructive_witness, holdable).
narrative_ontology:cs_axiom_grounding('14de14e4-2f78-4995-b411-6b34ca3a2f7a', computability_requires_constructive_witness, conventional).
narrative_ontology:cs_axiom('14de14e4-2f78-4995-b411-6b34ca3a2f7a', foundational, formal_knowability_distinct_from_physical_possibility).
narrative_ontology:cs_axiom_status(formal_knowability_distinct_from_physical_possibility, holdable).
narrative_ontology:cs_axiom_grounding('14de14e4-2f78-4995-b411-6b34ca3a2f7a', formal_knowability_distinct_from_physical_possibility, deontological).
narrative_ontology:cs_reference_frame('14de14e4-2f78-4995-b411-6b34ca3a2f7a', turing_1936_analysis_of_effective_calculability).
narrative_ontology:cs_drift_state('14de14e4-2f78-4995-b411-6b34ca3a2f7a', contemporary_type_theory_and_formal_verification, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('14de14e4-2f78-4995-b411-6b34ca3a2f7a', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, formal_methods_practitioners).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_science_foundations).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_proposals).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, oracle_machine_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, formal_methods_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a stable, universally accepted boundary for computability proofs. The thesis provides the framework within which recursion theory, lambda calculus, and type theory interoperate. They can work in alternative frameworks (constructive math, synthetic computability) but the Turing boundary remains the interoperability standard. Exit is easy — they can adopt other frameworks — but the boundary's utility makes exit unnecessary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theorists, beneficiary,
    powerful, biographical, arbitrage, global).

% Rely on the thesis as the foundation for program verification, model checking, and compiler correctness. The boundary tells them what is provable and what is not. They pay a small cost: some true properties are unprovable within the framework (Gödelian limits). But the framework's stability and tool support outweigh this. They could use alternative foundations (e.g., Coq's constructive logic) but the Turing-equivalence standard enables cross-tool interoperability.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, formal_methods_practitioners, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, formal_methods_practitioners, payer).

% The discipline's curricula, conferences, journals, and funding structures are organized around the Turing boundary. It defines what counts as a 'computability result' versus 'hypercomputation speculation'. This is not a person but an institutional structure — the aggregate of departments, societies, and publication venues that enforce the boundary. It administers the constraint by setting standards of evidence.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_science_foundations, agenda_setter,
    institutional, generational, arbitrage, universal).

% Mathematical claims about computability that rely on non-constructive existence proofs (e.g., 'there exists a computable function with property X' proven by classical logic without providing an algorithm). These are not agents but a class of mathematical objects. They are excluded from 'formally knowable computation' by the thesis's constructive proof requirement. They cannot 'exit' — they are definitionally outside the boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims, payer,
    analytical, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims).

% Research programs proposing models beyond Turing computability: infinite-time Turing machines, oracle machines, analog recurrent neural networks, relativistic hypercomputers. They are methodologically excluded from mainstream computability theory — their papers go to specialized venues, their results are not taught in standard curricula, their funding is marginal. They could reframe as 'oracle-relative computability' (which is accepted) but that concedes the boundary. Exit means leaving the field or accepting the boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_proposals, payer,
    organized, biographical, constrained, global).

% Technically well-defined mathematical models (Turing machines with oracle tapes) that compute non-recursive functions. They are not 'wrong' — they are valid objects of study in relative computability. But they are excluded from 'absolute computability' by this reading. They are a class of mathematical objects, not agents. Their exclusion is definitional, not coercive.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, oracle_machine_models, payer,
    analytical, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(church_turing_thesis__epistemological_boundary_reading, oracle_machine_models).

% The indexical classification engine's analytical seat. Sees all three readings of the kernel simultaneously, tracks their structural relationships, and computes per-seat classifications. Does not participate in the constraint — observes its operation across the kernel's readings.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, interoperable standard for what counts as a valid computability proof across proof theory, type theory, lambda calculus, and formal verification. Enables mathematicians and computer scientists to communicate results without re-litigating the foundations of effective computability.
% TRANSFER_FUNCTION: Moves epistemic authority from non-constructive existence proofs and hypercomputation models to constructive, algorithmically witnessed proofs. The 'transfer' is methodological: claims that cannot provide explicit constructions are excluded from the domain of 'formally knowable computation'. No material resources transfer; the currency is epistemic legitimacy.
% ABSENT_VOICES: Physicists investigating whether physical processes can compute non-recursive functions (physical_claim_reading proponents) — they are not excluded from physics but their claims about 'computability' are not recognized in the formal framework. Constructive mathematicians who reject classical logic entirely — they operate in a different framework where the boundary is drawn differently. These voices are not silenced; they operate in adjacent frameworks.
% DISAPPEARANCE_RATIONALE: If the epistemological boundary vanished, proof theory would lose its shared standard for 'computable function'. Alternative frameworks (oracle-relative, hypercomputation, constructive) would compete without a common interoperability layer. The field would fragment — some adopting stricter constructive standards, others expanding to include oracle results. The discipline's coherence would degrade until a new boundary emerged.
% FOUNDING_PROBLEM: Hilbert's Entscheidungsproblem (1928): is there a definite mechanical procedure to decide the truth of any mathematical statement? The thesis was built to give a precise meaning to 'mechanical procedure' so the problem could be solved (negatively, by Turing and Church).
% FOUNDING_PROBLEM_CORROBORATION: The Entscheidungsproblem is solved — no such procedure exists for first-order logic. This is attested by the mathematical community broadly (not just beneficiaries), including logicians who work in non-classical frameworks. The original problem is dead. However, the constraint persists because it acquired new coordination functions: foundation for computer science, type theory, formal verification, programming language semantics. These are attested by practitioners outside the original beneficiary set (e.g., software engineers using verified compilers, cryptographers relying on computational hardness assumptions grounded in the Turing model).
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, ExtMetricName, E),
    domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(church_turing_thesis__epistemological_boundary_reading),
    narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28) because the constraint primarily excludes claims that cannot meet constructive proof standards within the framework — it does not extract resources from agents who participate in the framework. Suppression (0.42) is moderate: the constraint actively shapes what counts as a valid computability proof (journal standards, curriculum, funding), but this suppression is largely internal to the proof-theoretic framework rather than coercive externally. Theater ratio is low (0.15): the boundary is genuinely used for productive work (proof theory, formal verification, programming language semantics). Accessibility collapse is high (0.78): within the framework, alternatives (oracle machines, hypercomputation) are not viable as 'formally knowable computation' — they are definitionally excluded. Resistance is moderate (0.35): hypercomputation research persists at margins, and the physical claim reading contests the boundary's scope. The claimed type is mountain with emerges_naturally=true, but beneficiaries are declared, making this an FSM candidate — the omega variables capture the ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the proof-theorist seat (beneficiary), the constraint is a mountain: a natural boundary that organizes the field productively. From the hypercomputation researcher seat (victim), it is a snare/tangled rope: a methodological gate that excludes their research program without empirical refutation. From the analytical observer seat, the constraint appears as a mountain with declared beneficiaries — the FSM signature may reclassify it as tangled_rope (methodological coordination + asymmetric exclusion). The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (proof_theorists, formal_methods_practitioners, computer_science_foundations) gain a stable, well-defined framework — their directionality d is low (beneficiary end). Victims (non_constructive_computability_claims, hypercomputation_proposals, oracle_machine_models) are methodologically excluded — their directionality d is high (target end). The analytical observer has d=0.5 (symmetric). The constraint does not extract material resources; its extraction is epistemic — it defines what counts as knowledge in this domain. This epistemic extraction is real but low-magnitude compared to material extraction constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Hilbert's Entscheidungsproblem and the need for a precise notion of effective computability) is largely solved — the thesis provides the boundary. However, the constraint persists not as a zombie but as an active coordinating framework for proof theory, type theory, and formal verification. The mandatrophy is resolved in the sense that the original problem is dead, but the constraint has acquired new coordination functions (foundations for computer science, programming language semantics, constructive mathematics) that are live. This is not a piton (theatrical maintenance) — the constraint does active coordination work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the Church-Turing boundary a genuine natural law of formal reasoning, or a constructed methodological convention that benefits identifiable proof-theoretic communities?',
    'Track whether the boundary shifts when proof-theoretic frameworks expand (e.g., type theory, constructive mathematics, quantum computation models) — if the boundary is stable across framework expansions, it tracks natural law; if it shifts with framework, it is constructed.',
    'If constructed with beneficiaries, FSM triggers reclassification to tangled_rope (methodological exclusion with coordination function for proof theory). If natural law, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Natural law vs. constructed methodological boundary ambiguity — FSM candidate.').

omega_variable(
    kernel_reading_contestation,
    'Does this reading (epistemological boundary) foreclose, coexist with, or influence the mathematical definition reading and the physical claim reading?',
    'Analyze whether accepting the epistemological boundary reading logically commits one to rejecting the mathematical definition reading (forecloses), whether they are held by different communities as live positions (coexists_with), or whether the epistemological reading reshapes the legitimacy conditions for the other readings without eliminating them (influences).',
    'Determines cs_structure.reading_relations and whether the kernel has genuine foreclosure pairs or a coexistence structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural relationship between sibling readings of the Church-Turing thesis kernel.').

omega_variable(
    hypercomputation_boundary_contestation,
    'Are hypercomputation proposals (oracle machines, infinite-time Turing machines, analog computation models) genuine challenges to the boundary or category errors relative to this reading''s framework?',
    'Examine whether hypercomputation models satisfy the epistemological boundary reading''s criteria for ''formally knowable computation'' — if they require non-constructive existence proofs, they are excluded by definition, not refuted by evidence.',
    'If excluded by definition, the victim set is methodological, not empirical — affects whether suppression is structural (gatekeeping) or natural (proof-theoretic impossibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_boundary_contestation, conceptual, 'Whether hypercomputation models are genuine competitors or methodological outsiders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1970, 0.11).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(chur_tr_t2010, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(chur_tr_t2025, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.15).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(chur_be_t2025, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(chur_su_t2025, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.02).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, turing_equivalence_proofs).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, formal_verification_foundations).

% DUAL FORMULATION NOTE:
% Church-Turing thesis kernel decomposes into three constraint stories with distinct ε and victim sets: (1) epistemological_boundary_reading — methodological exclusion of non-constructive claims (this story, ε≈0.28, mountain/FSM candidate); (2) mathematical_definition_reading — stipulative convention (ε≈0.05, mountain); (3) physical_claim_reading — empirical claim about universe (ε≈0.65, tangled_rope/snare depending on physics). The epistemological reading influences both siblings by setting the methodological standards that the definition reading formalizes and the physical reading must engage with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
