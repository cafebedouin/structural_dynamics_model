% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological Record
 *   domain: epistemology/anthropology
 *
 * SUMMARY:
 *   This constraint story instantiates the creationist_reading of the
 *   anthropological_record kernel. The reading asserts that the fossil,
 *   genetic, and archaeological record reveals divine creation event(s)
 *   compatible with a scriptural timeline or designed complexity. It operates
 *   as a tangled_rope: it genuinely coordinates identity and communal
 *   continuity for creationist communities (coordination function) while
 *   simultaneously extracting epistemic authority, educational resources, and
 *   life chances from mainstream science, public school students, and
 *   excluded indigenous epistemologies (extraction function), and it requires
 *   active enforcement — legal, curricular, and social — to maintain its
 *   contested position against the naturalist_reading and
 *   indigenous_epistemology_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.72).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '575ca6c8-3c7b-4bc2-b222-843ef52175a2').
narrative_ontology:cs_kernel_codification('575ca6c8-3c7b-4bc2-b222-843ef52175a2', fixed_text).
narrative_ontology:cs_authority_grounding('575ca6c8-3c7b-4bc2-b222-843ef52175a2', lineage).
narrative_ontology:cs_interpretation_layer_present('575ca6c8-3c7b-4bc2-b222-843ef52175a2').
narrative_ontology:cs_reading_relation('575ca6c8-3c7b-4bc2-b222-843ef52175a2', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('575ca6c8-3c7b-4bc2-b222-843ef52175a2', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('575ca6c8-3c7b-4bc2-b222-843ef52175a2', foundational, scriptural_timeline_is_historically_accurate).
narrative_ontology:cs_axiom_status(scriptural_timeline_is_historically_accurate, holdable).
narrative_ontology:cs_axiom_grounding('575ca6c8-3c7b-4bc2-b222-843ef52175a2', scriptural_timeline_is_historically_accurate, theological).
narrative_ontology:cs_axiom('575ca6c8-3c7b-4bc2-b222-843ef52175a2', foundational, designed_complexity_requires_intelligent_causation).
narrative_ontology:cs_axiom_status(designed_complexity_requires_intelligent_causation, holdable).
narrative_ontology:cs_axiom_grounding('575ca6c8-3c7b-4bc2-b222-843ef52175a2', designed_complexity_requires_intelligent_causation, empirically_contingent).
narrative_ontology:cs_reference_frame('575ca6c8-3c7b-4bc2-b222-843ef52175a2', scriptural_creation_account_as_literal_history).
narrative_ontology:cs_drift_state('575ca6c8-3c7b-4bc2-b222-843ef52175a2', post_genomic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('575ca6c8-3c7b-4bc2-b222-843ef52175a2', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, faith_based_educational_institutions).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_apologetics_organizations).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, mainstream_scientific_community).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, public_school_students_in_affected_jurisdictions).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, indigenous_knowledge_holders_excluded_by_exclusivist_framing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the creationist reading as core to religious identity and communal coherence. The reading provides epistemic authorization for their worldview against a dominant secular-scientific framework. Exit would mean fracturing identity, community, and often family ties — the reading is fused with who they are.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Administer curricula, accreditation, and hiring that institutionalize the creationist reading. They set the agenda for what counts as legitimate knowledge within their spheres and lobby for legal protection of that agenda. Exit would mean losing institutional mission, funding base, and legal standing.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, faith_based_educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Produce and disseminate the arguments, materials, and expert witnesses that sustain the creationist reading in public contests. They benefit from donation streams, speaking fees, and organizational relevance tied to the reading's prominence. They can pivot to other culture-war fronts if this one collapses.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_apologetics_organizations, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, religious_apologetics_organizations, beneficiary).

% Bears the cost of contested epistemic authority: diverted resources to defense, erosion of public trust, legislative interference with research and education. Their exit is arbitrage-grade — the scientific method operates independently of this constraint's recognition, but the social cost of the contest is real and imposed.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, mainstream_scientific_community, payer,
    institutional, civilizational, arbitrage, universal).

% Receive science education diluted or distorted by creationist mandates. They have no meaningful exit — they cannot choose their school system, and the deficit compounds over a lifetime. The constraint extracts educational opportunity from them directly.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, public_school_students_in_affected_jurisdictions, payer,
    powerless, biographical, trapped, regional).

% Hold relational, place-based epistemologies of deep time that are neither materialist-evolutionary nor scriptural-creationist. The creationist reading's claim to exclusive divine authorship of the record renders their traditions invisible or heretical. They are not in the room when 'the record' is adjudicated.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, indigenous_knowledge_holders_excluded_by_exclusivist_framing, excluded,
    moderate, generational, identity_locked, global).

% Sees the full structural field: three readings of one kernel, each with its own beneficiary/payer/excluded structure, each claiming the same evidentiary base. No stake in any reading's victory.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal identity, moral orientation, and intergenerational transmission within creationist communities by anchoring them in a shared reading of deep history that resists secular assimilation.
% TRANSFER_FUNCTION: Moves epistemic authority and educational resources from the mainstream scientific establishment to faith-based institutions and apologetics organizations; moves educational opportunity from students in affected jurisdictions to the maintenance of the creationist framework.
% ABSENT_VOICES: Indigenous knowledge holders whose relational epistemologies of deep time are rendered invisible by the binary (creationist vs. naturalist) framing of the contest. Also: theistic evolutionists and religious scientists who hold both scientific method and divine causation but are claimed by neither institutional pole.
% DISAPPEARANCE_RATIONALE: If the creationist reading vanished overnight, faith-based educational institutions would lose their curricular anchor, apologetics organizations would lose their central product, creationist communities would face identity crisis, and public school curricula in affected jurisdictions would revert to mainstream scientific consensus — the social and educational landscape would reorganize substantially.
% FOUNDING_PROBLEM: The perceived threat that a purely materialist account of human origins would erode the moral and communal foundations of religious life, leaving believers epistemically colonized by a secular framework that denies divine agency in history.
% FOUNDING_PROBLEM_CORROBORATION: Creationist communities and their institutions attest the problem remains live — citing rising secularization and cultural marginalization. Mainstream scientific bodies and science education researchers attest the problem is substantially a reaction to scientific consensus rather than an independent epistemic crisis, and that the 'threat' is constructed by the reading's own apologetic machinery. Sociologists of religion outside both camps document the reading's emergence as a 20th-century response to evolutionary biology's institutional consolidation, not a continuous feature of religious tradition.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the reading's structural diversion of epistemic authority and educational resources from the scientific establishment and affected students to faith-based institutions. Suppression (0.72) is high because the reading's persistence depends on active exclusion of rival payment routing — here, rival epistemic routing: litigation to mandate equal time, textbook adoption battles, accreditation pressure, and social enforcement within communities. Theater ratio (0.25) is moderate: the coordination function (identity, community, moral orientation) is real and valued by beneficiaries, but a growing share of enforcement activity defends the reading's epistemic monopoly rather than its communal function. Accessibility collapse (0.60) is substantial: once the reading is accepted as the framework, alternative interpretations of the same evidence become cognitively difficult to sustain from within. Resistance (0.75) is high: the scientific establishment, courts, educators, and excluded epistemologies actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the creationist community seat, the constraint is a rope — genuine coordination against epistemic assimilation. From the public school student seat, it is a snare — pure extraction of educational opportunity. From the mainstream science seat, it is a tangled_rope — coordination for them, extraction from us, enforced by law. From the indigenous knowledge holder seat, it is a snare of a different kind — epistemic erasure disguised as divine revelation. The engine computes these per-seat classifications from the structural data; the authored claimed_type (tangled_rope) is the generating model's structural judgment from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Creationist communities are identity_locked beneficiaries (d near 0.0 — the reading subsidizes their identity and coherence). Faith-based institutions and apologetics organizations are agenda_setters with constrained to mobile exit (d ~0.2-0.3 — they administer and profit from the constraint). Mainstream science is an institutional payer with arbitrage exit (d ~0.7 — bears social costs but the method survives). Public school students are powerless, trapped payers (d ~0.95 — no exit, direct extraction). Indigenous knowledge holders are identity_locked excluded (d undefined — not in the coordination/extraction circuit but structurally erased by the binary). The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (secular epistemic colonization) was live in 1925 when the reading crystallized in response to evolutionary biology's institutional consolidation. By 2025, the problem's status is contested: creationist communities attest it is live (rising secularization); scientists and sociologists attest it is largely a reaction constructed by the reading's own machinery. The reading has not resolved its mandatrophy — it persists by expanding its enforcement apparatus (theater rising, suppression rising) rather than by solving its founding problem. The constraint now extracts more than it coordinates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the creationist_reading a distinct constraint with its own stable epsilon, or does its classification depend on which observable of the anthropological_record is centered (fossil gaps vs. genetic continuity vs. cultural transmission)?',
    'Apply the epsilon-invariance test: if measuring the constraint via fossil record yields low extraction but measuring via public education policy yields high extraction, the label ''creationist_reading'' covers multiple constraints. Decompose into separate stories per observable.',
    'If epsilon is observable-dependent, this story violates DP-001 and must be split. The current authored epsilon (0.68) reflects the policy/education observable; the scientific-evidence observable would yield a different value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the creationist_reading label is epsilon-invariant across observables of the kernel.').

omega_variable(
    coordination_extraction_boundary,
    'Is the communal identity coordination function structurally separable from the epistemic authority extraction function, or are they fused such that the coordination cannot survive without the extraction?',
    'Natural experiment: observe creationist communities in jurisdictions where the reading has no legal/educational enforcement power. If communal coherence persists without extraction, the functions are separable; if coherence collapses, the extraction is load-bearing for the coordination.',
    'If inseparable, the tangled_rope classification is structurally necessary — the extraction IS the price of the coordination. If separable, the extraction is a removable layer and the constraint could be a rope with a snare overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the reading''s coordination and extraction components are structurally fused or separable.').

omega_variable(
    indigenous_epistemology_erasure_mechanism,
    'Does the creationist_reading actively suppress the indigenous_epistemology_reading, or does the binary (creationist vs. naturalist) framing merely render it invisible as a side effect?',
    'Trace apologetics literature and legislative testimony: are indigenous epistemologies explicitly addressed and rejected, or are they absent from the discourse because the binary framework has no category for them?',
    'If active suppression, the indigenous_epistemology_reading is a victim of this constraint (add to victims array). If side-effect invisibility, the erasure is a property of the kernel''s contested structure, not this reading''s enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_epistemology_erasure_mechanism, empirical, 'Whether indigenous epistemology exclusion is active suppression or structural invisibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 1925, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1925, anthropological_record__creationist_reading, theater_ratio, 1925, 0.1).
narrative_ontology:measurement(anth_tr_t1950, anthropological_record__creationist_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(anth_tr_t1975, anthropological_record__creationist_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(anth_tr_t1990, anthropological_record__creationist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(anth_tr_t2005, anthropological_record__creationist_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(anth_tr_t2015, anthropological_record__creationist_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(anth_tr_t2025, anthropological_record__creationist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(anth_be_t1925, anthropological_record__creationist_reading, base_extractiveness, 1925, 0.35).
narrative_ontology:measurement(anth_be_t1950, anthropological_record__creationist_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(anth_be_t1975, anthropological_record__creationist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(anth_be_t1990, anthropological_record__creationist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(anth_be_t2005, anthropological_record__creationist_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(anth_be_t2015, anthropological_record__creationist_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(anth_be_t2025, anthropological_record__creationist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1925, anthropological_record__creationist_reading, suppression_requirement, 1925, 0.4).
narrative_ontology:measurement(anth_su_t1950, anthropological_record__creationist_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(anth_su_t1975, anthropological_record__creationist_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(anth_su_t1990, anthropological_record__creationist_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(anth_su_t2005, anthropological_record__creationist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(anth_su_t2015, anthropological_record__creationist_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(anth_su_t2025, anthropological_record__creationist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the anthropological_record constraint family (kernel_id: anthropological_record). The three readings instantiate distinct constraints with different epsilon values: naturalist_reading (epsilon ~0.15, mountain-like for scientific community, tangled_rope for creationist communities), indigenous_epistemology_reading (epsilon ~0.25, rope for holding communities, snare for excluded scientific interlocutors), and this creationist_reading (epsilon 0.68, tangled_rope). The epsilon divergence across readings of the same kernel is the structural signature of a contested kernel — the label 'the anthropological record' conceals three different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, organized, 0.15).
constraint_indexing:directionality_override(anthropological_record__creationist_reading, institutional, 0.7).
constraint_indexing:directionality_override(anthropological_record__creationist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
