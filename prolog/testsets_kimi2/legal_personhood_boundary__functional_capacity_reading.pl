% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Functional Capacity Personhood Boundary
 *   domain: legal/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the functional_capacity_reading of the
 *   legal_personhood_boundary kernel: the legal principle that personhood â
 *   with its associated rights, standing, and protections â is granted
 *   solely upon demonstration of cognitive capacities such as rationality,
 *   sentience, or self-awareness, without regard to species membership. Under
 *   this arrangement, courts and legislatures administer capacity tests that
 *   determine which entities are rights-bearing persons and which remain
 *   legal property or objects of guardianship. The constraint coordinates
 *   rights allocation in plural legal orders but asymmetrically extracts from
 *   sentient beings that fail the threshold, including most non-human animals
 *   and humans with severe cognitive impairments. It is claimed as a rational
 *   coordination mechanism while structurally enforcing a species-blind but
 *   capacity-biased hierarchy.
 *
 * KEY AGENTS:
 *   - Judiciary (agenda_setter): institutional power, constrained exit â administers capacity tests and interprets personhood thresholds through precedent.
 *   - Cognitively capable humans (beneficiary): organized power, mobile exit â retain full legal personhood and dominate the legal and political process defining the threshold.
 *   - Non-human sentient beings (payer): powerless, trapped exit â possess demonstrated sentience but lack the specific cognitive capacities legally required for personhood; remain property without standing.
 *   - Humans with severe cognitive impairment (payer): powerless, trapped exit â their personhood becomes contingent on administrative findings of capacity, exposing them to guardianship and rights denial.
 *   - Animal cognition researchers (observer): institutional, analytical exit â produce empirical evidence of non-human capacities that is routinely subordinated to legal precedent.
 *   - Disability rights advocates (excluded): organized, constrained exit â argue for inherent dignity regardless of capacity but are structurally marginalized in constitutional personhood debates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.79).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Functional Capacity Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical/constitutional").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '55dafdb8-7f69-48c5-a82f-3123e4be4340').
narrative_ontology:cs_kernel_codification('55dafdb8-7f69-48c5-a82f-3123e4be4340', formalized).
narrative_ontology:cs_authority_grounding('55dafdb8-7f69-48c5-a82f-3123e4be4340', lineage).
narrative_ontology:cs_interpretation_layer_present('55dafdb8-7f69-48c5-a82f-3123e4be4340').
narrative_ontology:cs_reading_relation('55dafdb8-7f69-48c5-a82f-3123e4be4340', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('55dafdb8-7f69-48c5-a82f-3123e4be4340', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_axiom('55dafdb8-7f69-48c5-a82f-3123e4be4340', foundational, personhood_requires_demonstrated_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrated_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('55dafdb8-7f69-48c5-a82f-3123e4be4340', personhood_requires_demonstrated_cognitive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('55dafdb8-7f69-48c5-a82f-3123e4be4340', foundational, species_membership_irrelevant_to_personhood).
narrative_ontology:cs_axiom_status(species_membership_irrelevant_to_personhood, holdable).
narrative_ontology:cs_axiom_grounding('55dafdb8-7f69-48c5-a82f-3123e4be4340', species_membership_irrelevant_to_personhood, deontological).
narrative_ontology:cs_reference_frame('55dafdb8-7f69-48c5-a82f-3123e4be4340', capacity_based_legal_order).
narrative_ontology:cs_drift_state('55dafdb8-7f69-48c5-a82f-3123e4be4340', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55dafdb8-7f69-48c5-a82f-3123e4be4340', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, cognitively_capable_humans).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, nonhuman_sentient_beings).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, humans_with_severe_cognitive_impairment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers cognitive capacity tests in guardianship, criminal, and constitutional cases; interprets personhood through precedent and evidentiary standards that it sets and revises incrementally.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Hold and retain legal personhood automatically under prevailing interpretations of the capacity threshold; their interests dominate legislative and judicial agenda-setting; they benefit from legal protections and standing.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitively_capable_humans, beneficiary,
    organized, biographical, mobile, national).

% Great apes, cetaceans, elephants, and other animals with demonstrated sentience but uncertain legal standing under strict capacity tests; remain property under law and lack standing to challenge confinement or use.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, nonhuman_sentient_beings, payer,
    powerless, immediate, trapped, national).

% Humans in persistent vegetative states, with advanced dementia, or profound intellectual disability; their personhood becomes contingent on administrative capacity findings, exposing them to guardianship overrides and denial of autonomy.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, humans_with_severe_cognitive_impairment, payer,
    powerless, immediate, trapped, national).

% Produce empirical evidence of non-human cognitive capacities; their findings are admissible but rarely decisive in altering the legal threshold; they observe the gap between scientific and legal standards.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_cognition_researchers, observer,
    institutional, civilizational, analytical, global).

% Argue for inherent human dignity regardless of cognitive capacity; structurally marginalized in constitutional drafting and high-court adjudication that privileges capacity metrics.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves which entities hold legal rights, standing, and duties by tying personhood to demonstrable cognitive capacities, creating a predictable rights-allocation framework for courts and legislatures.
% TRANSFER_FUNCTION: Moves legal protections, autonomy, and immunity from use from entities that fail capacity demonstrations to the legal system and to entities recognized as persons, while leaving the excluded open to property status, experimentation, and termination.
% ABSENT_VOICES: Non-human animals and severely cognitively impaired humans cannot speak in the fora where the threshold is set; disability-rights advocates and animal ethicists are often formally present but structurally marginalized in constitutional drafting.
% DISAPPEARANCE_RATIONALE: If the capacity-based personhood standard vanished, the legal system would lose its current primary mechanism for distinguishing rights-bearers from property; property regimes, guardianship structures, and criminal prohibitions on harm would require re-foundation on a different criterion (inherent dignity, species membership, or relational ethics).
% FOUNDING_PROBLEM: The need to determine which entities can bear rights and duties in a plural legal order, and to prevent arbitrary exclusion while managing the practical consequences of granting standing to non-human or non-rational entities.
% FOUNDING_PROBLEM_CORROBORATION: Legal philosophers such as Peter Singer and Martha Nussbaum have attested from outside the beneficiary class that the problem of arbitrary species exclusion remains unsolved; disability-rights scholars like Eva Kittay contest that capacity is the right solution. No neutral consensus exists.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.79) is high because the constraint allocates the fundamental legal status of personhood versus property; those below the threshold lose all rights and standing. Suppression (0.72) reflects active legal enforcement of the boundary through courts, property law, and criminal statutes. Theater ratio (0.28) is moderate: capacity tests claim scientific objectivity but often rehearse anthropocentric assumptions in their design. Accessibility collapse (0.84) is high because once the capacity framework is adopted, alternatives like inherent dignity collapse in legal discourse. Resistance (0.68) reflects sustained opposition from animal ethics and disability rights movements. Temporal measurements show rising extraction and suppression as scientific evidence of non-human cognition accumulates but legal thresholds lag, widening the gap between empirical reality and legal classification.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and capable humans experience this constraint as a necessary coordination device that prevents legal chaos by limiting rights to demonstrable agents. Non-human animals and impaired humans experience it as total legal erasure. The engine will compute divergent seat classifications: low directionality for beneficiaries (the constraint subsidizes their legal status) and high directionality for payers (the constraint extracts their standing). Animal cognition researchers occupy an analytical seat with near-neutral directionality but their evidence is systematically absorbed without altering the threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cognitively capable humans) are structurally subsidized by the constraint: their legal personhood is preserved and reinforced by the capacity test they design and pass. Victims (non-human sentient beings, humans with severe cognitive impairment) are structural targets: the constraint directly extracts legal standing from them. The high suppression and active enforcement amplify effective extraction for the target seats. No directionality override is needed because beneficiary/victim declarations plus exit options (mobile vs trapped) correctly map the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination problem: legal systems require a boundary for rights and duties. However, the coordination function does not justify the specific threshold location or its administration by the beneficiary class. The Tangled Rope classification captures that the same structure both coordinates (creates legal predictability) and extracts (denies standing to the powerless). If the coordination story were pure cover, it would compute as a Snare; if extraction were negligible, it would compute as a Rope. The authored metrics describe the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_location,
    'Where is the cognitive capacity threshold administratively set, and does its location derive from empirical findings or from the political power of the testing institutions?',
    'Comparative analysis of capacity-test outcomes across jurisdictions with different cultural and political compositions.',
    'If the threshold is politically rather than empirically determined, the constraint''s extraction is higher than its coordination framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_location, empirical, 'Whether the personhood threshold tracks science or institutional bias.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the functional capacity reading logically foreclose the developmental potentiality and restrictive anthropocentric readings within a single constitutional framework, or can any of them coexist as interpretive options?',
    'Jurisprudential analysis of whether a single legal order can simultaneously protect zygotes as potential persons and deny personhood to entities lacking current capacity, or can limit personhood to born humans while remaining species-neutral.',
    'If foreclosure is genuine, the kernel is zero-sum at the constitutional level; if coexistence is possible, the constraint may function as a distributed commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between sibling readings of the personhood kernel.').

omega_variable(
    excluded_beings_sentience_suffering,
    'To what extent do excluded sentient beings experience the absence of legal personhood as harm distinct from their physical treatment?',
    'Ethological and welfare analysis comparing legal standing gaps to physical welfare outcomes for non-human animals and impaired humans.',
    'High harm would raise effective extraction; low harm would support the coordination framing that legal personhood is merely a procedural status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_beings_sentience_suffering, empirical, 'Whether legal non-personhood itself constitutes harm beyond physical conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 50, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
