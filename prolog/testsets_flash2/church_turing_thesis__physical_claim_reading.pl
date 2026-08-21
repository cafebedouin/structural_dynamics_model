% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis: Physical Computability Claim
 *   domain: philosophy_of_computation/foundations_of_physics
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis (CTT) as an empirical
 *   claim about the physical universe: that no physical process can compute
 *   functions beyond Turing-machine computability. This reading is distinct
 *   from the CTT as a mathematical definition or an epistemological boundary.
 *   It acts as a 'tangled rope' because it genuinely coordinates research
 *   efforts by providing a shared empirical limit, but it also extracts from
 *   and suppresses alternative research programs (hypercomputation, certain
 *   interpretations of quantum computation) that challenge this limit. Its
 *   persistence relies on active enforcement through academic gatekeeping and
 *   the absence of definitive counter-evidence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.65).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis: Physical Computability Claim").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/foundations_of_physics").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '4be3528f-4382-4a84-9a69-a5d569349eaf').
narrative_ontology:cs_kernel_codification('4be3528f-4382-4a84-9a69-a5d569349eaf', formalized).
narrative_ontology:cs_authority_grounding('4be3528f-4382-4a84-9a69-a5d569349eaf', expertise).
narrative_ontology:cs_interpretation_layer_present('4be3528f-4382-4a84-9a69-a5d569349eaf').
narrative_ontology:cs_reading_relation('4be3528f-4382-4a84-9a69-a5d569349eaf', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4be3528f-4382-4a84-9a69-a5d569349eaf', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('4be3528f-4382-4a84-9a69-a5d569349eaf', foundational, physical_computability_is_turing_computability).
narrative_ontology:cs_axiom_status(physical_computability_is_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('4be3528f-4382-4a84-9a69-a5d569349eaf', physical_computability_is_turing_computability, empirically_contingent).
narrative_ontology:cs_reference_frame('4be3528f-4382-4a84-9a69-a5d569349eaf', turing_machine_as_universal_physical_model).
narrative_ontology:cs_drift_state('4be3528f-4382-4a84-9a69-a5d569349eaf', contemporary_quantum_computing_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4be3528f-4382-4a84-9a69-a5d569349eaf', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computer_science).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, theoretical_physics).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a stable foundation for computability theory, which simplifies model building and curriculum design. This reading of CTT provides a strong empirical grounding for the limits of what can be built or simulated. They enforce this view through funding, publication, and peer review.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computer_science, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the CTT as a physical claim by having a clear boundary for what physical systems can compute. This simplifies the search for fundamental laws and prevents speculative 'hypercomputer' physics from dominating the field. They implicitly enforce this through the structure of accepted theories.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, theoretical_physics, beneficiary,
    institutional, generational, constrained, universal).

% Their research programs directly challenge the physical CTT, seeking to identify or construct physical systems that could compute beyond Turing limits. They face significant resistance in funding, publication, and academic legitimacy due to the prevailing acceptance of the physical CTT.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% While not directly claiming hypercomputation, their work on quantum computers achieving 'supremacy' (solving problems intractable for classical computers) pushes against the perceived empirical limits of the CTT. They face scrutiny regarding whether their claims truly exceed Turing computability or merely demonstrate practical speedups.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    powerful, immediate, constrained, global).

% Analyze the conceptual implications and empirical status of the CTT as a physical claim. They critically examine arguments for and against hypercomputation, and the philosophical consequences of either outcome. Their role is primarily interpretive and critical, not directly enforcing or paying.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared empirical understanding of the limits of physical computation, allowing researchers in computer science and physics to coordinate their efforts and build upon a common foundational assumption about what is physically possible to compute.
% TRANSFER_FUNCTION: Transfers academic legitimacy, funding, and research focus away from projects exploring non-Turing physical computation towards those operating within the Turing paradigm. It also transfers a sense of conceptual closure to mainstream fields.
% ABSENT_VOICES: Future researchers who might discover or invent physical processes that demonstrably exceed Turing computability are currently excluded from the mainstream discourse, their potential findings suppressed by the current empirical consensus. Their 'voice' is the future empirical data.
% DISAPPEARANCE_RATIONALE: If the physical CTT were definitively disproven overnight (e.g., by a robust experimental demonstration of hypercomputation), the foundations of computer science and theoretical physics would undergo a profound rearrangement. New computational paradigms would emerge, and the search for fundamental physical laws would be reoriented to accommodate non-Turing processes.
% FOUNDING_PROBLEM: To establish the empirical limits of what physical systems can compute, providing a scientific basis for the scope of computation in the natural world.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream computer scientists and physicists attest that the problem is still live, as no definitive physical hypercomputer has been demonstrated. Hypercomputation researchers and some philosophers contest this, arguing that the problem is framed too narrowly or that potential counter-evidence is prematurely dismissed. The corroboration is primarily the absence of counter-evidence, interpreted differently by different parties.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while it provides a useful framework, it imposes a cost on those whose research falls outside its bounds. Suppression is higher (0.65) due to the active gatekeeping in academia (funding, publication, peer review) against claims of physical hypercomputation. Theater ratio is low (0.1) because the claim is genuinely empirical and subject to falsification, so its maintenance is not primarily performative. The slight dip in extractiveness and suppression towards the end of the interval reflects increased interest and funding in quantum computing, which, while not hypercomputation, has opened some conceptual space for questioning computational limits.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream computer science and theoretical physics, the physical CTT is a robust empirical truth that guides productive research. From the perspective of hypercomputation researchers, it is a suppressive dogma that stifles innovation and prematurely closes off avenues of inquiry. The engine's classification as 'tangled_rope' reflects this dual nature of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computer science and theoretical physics are beneficiaries, as the physical CTT provides a stable, simplifying assumption for their work. Hypercomputation researchers and quantum supremacy claimants are payers/victims, as their work directly challenges or is constrained by this empirical claim. Philosophers of computation act as observers, analyzing the implications without directly enforcing or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_falsifiability,
    'Is the physical Church-Turing Thesis genuinely empirically falsifiable, or has it become a de facto methodological principle that resists empirical challenge?',
    'Analysis of responses to proposed hypercomputation models or quantum supremacy claims: if such claims are dismissed on a priori grounds rather than empirical ones, it suggests a shift towards a methodological principle.',
    'If it''s a methodological principle, its extractiveness and suppression are higher, as it functions more like a ''snare'' for non-conforming research. If genuinely falsifiable, it remains a ''tangled_rope'' or ''scaffold'' awaiting empirical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_falsifiability, conceptual, 'Ambiguity between empirical claim and methodological principle.').

omega_variable(
    hypercomputation_evidence_threshold,
    'What constitutes sufficient empirical evidence to falsify the physical Church-Turing Thesis, and is there a consensus on this threshold?',
    'A survey of leading researchers in theoretical computer science and physics regarding the criteria for accepting a physical hypercomputer, and analysis of historical responses to ''anomalous'' computational phenomena.',
    'A high, unachievable threshold implies higher suppression and extractiveness, as it effectively traps researchers. A clear, achievable threshold would make the constraint more like a ''rope'' or ''scaffold'' awaiting a specific discovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypercomputation_evidence_threshold, empirical, 'Lack of clear criteria for falsifying the physical CTT.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__physical_claim_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__physical_claim_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__physical_claim_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2010, church_turing_thesis__physical_claim_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__physical_claim_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.4).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__physical_claim_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__physical_claim_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__physical_claim_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__physical_claim_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__physical_claim_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__physical_claim_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__physical_claim_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__physical_claim_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__physical_claim_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__physical_claim_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing Thesis kernel. This reading (physical claim) influences the others by setting an empirical context for their conceptual and mathematical interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
