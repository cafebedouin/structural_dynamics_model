% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-28
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Church-Turing Thesis as Physical Claim (Physical_claim_reading)
 *   domain: philosophy/computation/foundations
 *
 * SUMMARY:
 *   The Church-Turing thesis as a physical claim asserts that the universe
 *   itself respects the Turing limit: no physical process, however exotic,
 *   can compute a function that a Turing machine cannot. This reading emerged
 *   in the 1950s–70s as CS sought to ground its mathematical theory in
 *   physics. It functions as a coordination device across CS and physics but
 *   also suppresses research programs that explore super-Turing physics
 *   (hypercomputation, certain quantum gravity models, analog computation
 *   with infinite precision). The constraint is empirically contestable — its
 *   truth depends on unknown physics — making it a scaffold: if new physics
 *   violates it, the constraint sunsets and new computational physics opens;
 *   if it holds, it remains a coordination boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.45).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.38).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, scaffold).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Claim (Physical_claim_reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy/computation/foundations").

narrative_ontology:has_sunset_clause(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47').
narrative_ontology:cs_kernel_codification('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', distributed).
narrative_ontology:cs_authority_grounding('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', practice).
narrative_ontology:cs_interpretation_layer_present('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47').
narrative_ontology:cs_reading_relation('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', foundational, physics_adjudicates_computability).
narrative_ontology:cs_axiom_status(physics_adjudicates_computability, holdable).
narrative_ontology:cs_axiom_grounding('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', physics_adjudicates_computability, empirically_contingent).
narrative_ontology:cs_axiom('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', foundational, turing_limit_is_physical_law).
narrative_ontology:cs_axiom_status(turing_limit_is_physical_law, holdable).
narrative_ontology:cs_axiom_grounding('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', turing_limit_is_physical_law, empirically_contingent).
narrative_ontology:cs_reference_frame('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', turing_1936_physical_grounding).
narrative_ontology:cs_drift_state('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', contemporary_quantum_gravity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('68b9d4dc-48ea-4ddf-ada4-7f6a7eef6d47', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_cs_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, standard_physics_practitioners).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, turing_machine_equivalence_community).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, non_turing_physics_explorers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_machine_universality).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, physical_computational_equivalence).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, effective_computability_empirical_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate within the standard Turing-machine framework; the physical claim reading provides a stable empirical boundary that validates their theoretical work and curriculum. They benefit from the constraint's function as a coordination device for computational theory.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_cs_theorists, beneficiary,
    institutional, generational, arbitrage, global).

% Use computability assumptions in physical modeling (e.g., simulation limits, complexity bounds). The constraint coordinates their expectations about what physics can compute, simplifying theory-building.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, standard_physics_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Curate the definition of 'effective computation' across CS, physics, and philosophy; they administer the standard model that treats CT as physical law. Their authority derives from the constraint's acceptance.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, turing_machine_equivalence_community, agenda_setter,
    institutional, generational, arbitrage, global).

% Investigate models exceeding Turing computability (e.g., analog computation, infinite-time Turing machines, relativistic hypercomputation). The physical claim reading treats their research programs as empirically impossible, suppressing funding, publication venues, and academic legitimacy.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Claim quantum devices outperform classical computers on specific tasks. If any claim implied super-Turing power, the physical claim reading would delegitimize it. They benefit from the constraint's coordination of 'quantum advantage' within Turing bounds but pay if their results are mischaracterized as hypercomputation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, beneficiary).

% Explore physical theories where computability limits might be exceeded (e.g., Malament-Hogarth spacetimes, closed timelike curves, infinite precision measurements). The constraint treats their work as pseudoscience, making career advancement nearly impossible.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, non_turing_physics_explorers, payer,
    powerless, biographical, trapped, global).

% Analyze the thesis's status across readings; they track the empirical, mathematical, and epistemological framings without committing to one. Their seat computes the structural divergence between readings.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_of_computation_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared empirical boundary for computational physics and CS theory: 'what can be physically computed' is identified with 'what a Turing machine computes,' enabling cross-disciplinary work without re-litigating computability per project.
% TRANSFER_FUNCTION: Moves research legitimacy, funding access, and publication acceptance from hypercomputation and non-Turing physics programs to mainstream Turing-equivalence work. The constraint acts as a filter: proposals assuming super-Turing physics are rejected or marginalized.
% ABSENT_VOICES: Physicists working on speculative spacetimes (Malament-Hogarth, CTCs) and analog computation theorists who cannot publish in mainstream venues; early-career researchers who would pursue hypercomputation but are warned it is career suicide. They are structurally excluded by the constraint's gatekeeping function.
% DISAPPEARANCE_RATIONALE: If the physical claim reading vanished, hypercomputation research would become a legitimate (if speculative) physics program; quantum supremacy claims would be evaluated without a priori Turing-bound assumptions; new journals and funding lines for non-Turing physics would emerge; the CS/physics boundary would shift from a hard wall to a research frontier.
% FOUNDING_PROBLEM: Early computability theory needed an empirical anchor: without a physical claim, Turing's analysis of 'effective procedure' remained a mathematical idealization. The physical claim reading grounded the theory in the world, enabling CS to make predictions about physical computers.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream CS historians (e.g., Copeland, Davis) attest the physical reading was not Turing's original intent but a later interpretation (post-1950s). Quantum gravity theorists (e.g., Aaronson, Deutsch) attest the founding problem is live: we still lack a physics-grounded theory of computability. No corroboration exists from outside the Turing-equivalence community that the physical claim is empirically settled.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects moderate rent extraction: mainstream CS/physics gains coordination value while hypercomputation researchers lose legitimacy and resources. Suppression (0.38) is real but not total: hypercomputation work persists in philosophy and niche physics venues, but is excluded from mainstream CS/physics. Theater ratio (0.12) is low: the constraint's enforcement is mostly genuine gatekeeping (peer review, curriculum standards), not performative. Accessibility collapse (0.62) is moderate: alternatives (hypercomputation models) exist and are formally coherent, but are treated as physically impossible. Resistance (0.55) is significant: a persistent minority challenges the claim, and quantum computing advances keep the empirical question open.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, the constraint is a genuine coordination scaffold: it solved the founding problem (grounding computability in physics) and carries a sunset clause (if physics violates it, the constraint dissolves). From the payer seats, it operates as a snare: it suppresses legitimate physics exploration under the guise of empirical law, and its 'sunset' is not operationalized — no mechanism exists to detect or admit a violation. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda-setters (turing_machine_equivalence_community) and beneficiaries (mainstream_cs_theorists, standard_physics_practitioners) sit at low directionality (d ~ 0.1–0.2): the constraint subsidizes their work. Payers (hypercomputation_researchers, non_turing_physics_explorers) sit at high directionality (d ~ 0.8–0.9): they bear the extraction via exclusion. Quantum_supremacy_claimants are dual-positioned: they benefit from the constraint's coordination of 'quantum advantage within Turing bounds' (d ~ 0.3) but pay if their work is misread as hypercomputation (d ~ 0.7). Observers (philosophy_of_computation_scholars) sit at d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grounding computability in physics) is contested: mainstream CS treats it as solved, but quantum gravity and hypercomputation researchers treat it as open. The constraint persists because the benefiting community controls the definition of 'physical computation' and has no incentive to sunset it. Mandatrophy is unresolved: the arrangement has outlived its uncontested coordination function but persists through institutional inertia and gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_falsifiability_status,
    'Is the physical claim reading genuinely empirically falsifiable, or has it become a conventional framework that absorbs anomalies?',
    'A confirmed physical process computing a non-Turing function (e.g., a Malament-Hogarth spacetime computer, or an analog system with verified infinite precision) would falsify it. Absent such a process, the claim is protected by the ''no counterexample yet'' barrier.',
    'If genuinely falsifiable, the scaffold classification holds (sunset is live). If conventionalized, the constraint is a piton or false mountain: it persists regardless of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_falsifiability_status, empirical, 'Whether the physical claim reading retains its empirical risk or has become immune to counterexample.').

omega_variable(
    beneficiary_capture_of_physical_claim,
    'Does the mainstream CS/physics community benefit from treating CT as physical law in a way that incentivizes suppressing hypercomputation research?',
    'Trace funding, publication, and hiring patterns: if hypercomputation proposals are rejected on ''physical impossibility'' grounds without empirical engagement, capture is indicated.',
    'If capture exists, the constraint''s extraction is higher than its coordination value; the scaffold claim is a cover for a snare. If no capture, the constraint is a genuine coordination scaffold with moderate extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_physical_claim, conceptual, 'Whether the physical claim reading''s persistence is driven by beneficiary capture rather than empirical adequacy.').

omega_variable(
    kernel_framing_underdetermination,
    'Do the three sibling readings of the CT kernel represent genuinely distinct structural claims, or are they perspectival variants of one constraint?',
    'Check ε-invariance: if measuring the constraint via ''physical prediction'' yields different extraction/suppression than measuring via ''mathematical convention'' or ''epistemological boundary'', they are distinct constraints. The BGS decomposition standard applies.',
    'If distinct, each reading gets its own constraint story (as authored here). If variants, the kernel is one constraint with observer-dependent classification — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel decomposition into three readings satisfies the ε-invariance principle or masks a single constraint with measurement-dependent metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.02).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__physical_claim_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__physical_claim_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__physical_claim_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(chur_tr_t2005, church_turing_thesis__physical_claim_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(chur_tr_t2025, church_turing_thesis__physical_claim_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.15).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__physical_claim_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__physical_claim_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__physical_claim_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(chur_be_t2005, church_turing_thesis__physical_claim_reading, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement(chur_be_t2025, church_turing_thesis__physical_claim_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.1).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__physical_claim_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__physical_claim_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__physical_claim_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(chur_su_t2005, church_turing_thesis__physical_claim_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(chur_su_t2025, church_turing_thesis__physical_claim_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.08).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This reading treats CT as an empirical physical claim (falsifiable, physics-adjudicated). The mathematical_definition_reading treats it as a stipulative convention (true by definition, no empirical content). The epistemological_boundary_reading treats it as a limit of formal provability (metamathematical, not physical). All three share the kernel 'Church-Turing thesis' but instantiate different constraints with different ε, beneficiaries, victims, and types. This reading is a scaffold (sunset if physics violates it); the mathematical reading is a mountain (definition, zero extraction); the epistemological reading is a rope (coordination of proof-theoretic boundaries).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, moderate, 0.85).
constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
