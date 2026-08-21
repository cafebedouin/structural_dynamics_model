% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis: Epistemological Boundary Reading
 *   domain: philosophy_of_mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as an epistemological
 *   boundary: it defines what counts as 'formally knowable computation'
 *   within mathematics and computer science. Functions that are not
 *   Turing-computable are, by this reading, outside the scope of what can be
 *   formally proven or effectively realized. This reading is distinct from
 *   viewing the thesis as a purely mathematical definition or an empirical
 *   claim about physics. The constraint is claimed as a Rope due to its
 *   coordination function in defining a field, but its metrics reflect a
 *   low-to-moderate extractiveness and suppression due to its role in
 *   excluding alternative theoretical approaches.
 *
 * KEY AGENTS:
 *   - constructive_mathematicians: Primary beneficiary (institutional/mobile) — benefits from clear boundaries.
 *   - computer_scientists: Primary beneficiary (institutional/mobile) — benefits from a stable foundation for their field.
 *   - non_constructive_computability_theorists: Primary payer (moderate/constrained) — their work is marginalized by this boundary.
 *   - hypercomputation_theorists: Primary payer (moderate/constrained) — their work is defined as outside 'knowable computation'.
 *   - philosophers_of_mathematics: Analytical observer (analytical/analytical) — analyzes the implications of the thesis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.45).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis: Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '8ef8b7b0-c79d-47cb-a1e0-8626580709d2').
narrative_ontology:cs_kernel_codification('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', formalized).
narrative_ontology:cs_authority_grounding('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', expertise).
narrative_ontology:cs_interpretation_layer_present('8ef8b7b0-c79d-47cb-a1e0-8626580709d2').
narrative_ontology:cs_reading_relation('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', foundational, computability_is_formal_knowability).
narrative_ontology:cs_axiom_status(computability_is_formal_knowability, holdable).
narrative_ontology:cs_axiom_grounding('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', computability_is_formal_knowability, deontological).
narrative_ontology:cs_axiom('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', foundational, turing_machine_is_universal_formal_model).
narrative_ontology:cs_axiom_status(turing_machine_is_universal_formal_model, holdable).
narrative_ontology:cs_axiom_grounding('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', turing_machine_is_universal_formal_model, conventional).
narrative_ontology:cs_reference_frame('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', formal_proof_theoretic_framework).
narrative_ontology:cs_drift_state('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', contemporary_hypercomputation_debates, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8ef8b7b0-c79d-47cb-a1e0-8626580709d2', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_scientists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, formal boundary for what counts as a 'computable' function, aligning with their emphasis on explicit constructions and proofs. The thesis provides a stable foundation for their work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    institutional, generational, mobile, global).

% Utilize the thesis as a foundational principle for algorithm design, complexity theory, and the limits of what can be achieved with digital computers. It defines the scope of their discipline's theoretical capabilities.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, global).

% Are constrained by the thesis's definition of 'knowable computation,' which can exclude or marginalize theoretical approaches to computability that do not rely on explicit, constructive methods, even if mathematically sound.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists, payer,
    moderate, biographical, constrained, global).

% Propose models of computation that exceed Turing limits. This reading of the thesis frames their work as outside the boundary of 'formally knowable computation,' making it harder to gain mainstream acceptance and funding within established computer science departments.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_theorists, payer,
    moderate, biographical, constrained, global).

% Analyze the philosophical implications of the Church-Turing Thesis, including its status as a definition, an empirical claim, or an epistemological boundary. They observe the debates and the structural effects of each reading.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally accepted standard for what constitutes 'computable' in formal mathematical proofs and theoretical computer science, allowing researchers to coordinate on a shared understanding of computational limits.
% TRANSFER_FUNCTION: Transfers epistemic authority and definitional power to the Turing machine model, effectively excluding non-Turing-computable functions from the domain of 'formally knowable computation' and directing research efforts within these boundaries.
% ABSENT_VOICES: Theorists exploring non-constructive proofs of computability or hypercomputation models often find their work implicitly or explicitly excluded from the core discourse on 'computability' as defined by this reading, as their methods or subjects fall outside the established epistemological boundary.
% DISAPPEARANCE_RATIONALE: If this epistemological boundary vanished, the very definition of 'computable' in formal contexts would become ambiguous, leading to fragmentation in computability theory and a re-evaluation of what constitutes a valid proof or a solvable problem in computer science. The field would need to re-establish its foundational limits.
% FOUNDING_PROBLEM: To provide a rigorous, formal definition of 'effective calculability' or 'computability' that could be universally accepted across different mathematical and logical systems, resolving ambiguities and inconsistencies in informal notions of computation.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for a clear, formal boundary in computability theory is attested by the continued use of the thesis in textbooks and research. Philosophers of mathematics and computer scientists outside the direct beneficiaries corroborate its foundational role in defining the scope of their disciplines.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is low-to-moderate because while the thesis provides a valuable coordination function, this reading also implicitly excludes or devalues certain lines of theoretical inquiry (e.g., hypercomputation, non-constructive computability). Suppression (0.45) is present as the academic and publishing norms enforce this epistemological boundary, making it harder for 'excluded' theories to gain traction. Theater ratio is low (0.1) as the thesis is genuinely applied and foundational, not merely performative. Accessibility collapse (0.7) is high because once this epistemological boundary is accepted, alternatives for 'knowable computation' largely collapse within the formal disciplines. Resistance (0.2) is low, as the thesis is widely accepted, though some theorists do push against its boundaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of constructive mathematicians and computer scientists, the thesis is a beneficial, clarifying principle (a Rope). From the perspective of hypercomputation theorists, it acts as a barrier to their research, defining their work as outside the 'knowable' (a Snare-like effect on their specific seat). The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructive mathematicians and computer scientists are beneficiaries (d near 0.0) as the thesis provides a stable, agreed-upon framework for their work. Non-constructive computability theorists and hypercomputation theorists are payers (d near 1.0) as their research is implicitly or explicitly excluded from the 'formally knowable' domain, facing higher barriers to acceptance. Philosophers of mathematics are observers (d near 0.5) as they analyze the constraint without being directly subject to its enforcement or benefiting from its coordination in their own research.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of the Church-Turing Thesis has not suffered mandatrophy. Its mandate to define the boundary of formally knowable computation remains live and central to the fields of mathematics and computer science. The classification as a Rope (claimed) with low-to-moderate extraction (metrics) prevents mislabeling it as pure extraction, acknowledging its genuine coordination function while still capturing the costs borne by those whose work falls outside its defined boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemological_vs_mathematical_status,
    'Is the Church-Turing Thesis primarily an epistemological boundary for ''knowable computation'' or a purely mathematical definition of ''effective computability''?',
    'Analysis of how the thesis is invoked in foundational proofs vs. definitional statements in textbooks. If its force is primarily to delimit what can be proven, it''s epistemological; if it''s merely a synonym, it''s definitional.',
    'If purely definitional (mathematical_definition_reading), extractiveness and suppression would be lower, as it would be a convention rather than an exclusionary boundary. If epistemological, the current metrics are appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemological_vs_mathematical_status, conceptual, 'Ambiguity in the fundamental status of the Church-Turing Thesis.').

omega_variable(
    epistemological_vs_physical_status,
    'To what extent does this epistemological boundary implicitly rely on or influence the ''physical claim'' reading of the Church-Turing Thesis?',
    'Examination of arguments for the epistemological boundary: do they implicitly assume physical limitations, or are they purely formal? If physical limitations are invoked, the readings are more coupled than currently modeled.',
    'If strongly coupled to the physical claim, challenges to the physical claim (e.g., from quantum computation or hypercomputation) would more directly undermine the epistemological boundary, potentially increasing resistance and lowering accessibility collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemological_vs_physical_status, conceptual, 'Interdependence between epistemological and physical interpretations of the thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing Thesis. This 'epistemological boundary' reading defines what counts as 'formally knowable computation,' distinct from the 'mathematical definition' (a convention) and 'physical claim' (an empirical statement) readings. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
