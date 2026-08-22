% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Practical Survival Knowledge Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the reading of ritual as a system for encoding
 *   and transmitting practical survival knowledge — timing of migrations,
 *   resource management protocols, family reorganization strategies, and
 *   adaptation heuristics — rather than (or in addition to) its symbolic or
 *   identity-maintaining functions. The reading asserts that the constraint's
 *   structure is a genuine coordination mechanism (rope-like) that has
 *   accumulated extractive overhead (snare-like) as formalist communities
 *   maintain ritual form while losing practical content, creating a victim
 *   class that performs the rite without its survival function. Beneficiaries
 *   are diaspora communities and adaptive family networks that retain the
 *   practical decoding of ritual and gain measurable adaptive capacity. The
 *   kernel 'catastrophe_memory_survival' is contested: this reading
 *   (competence_transmission) competes with hybrid_encoding (dual register)
 *   and symbol_survival (symbolic continuity only). This JSON instantiates
 *   ONLY the competence_transmission_reading as a clean epsilon-invariant
 *   constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival Knowledge Transmission").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '139fdec0-b912-4a7b-9e01-c92ba92e27dc').
narrative_ontology:cs_kernel_codification('139fdec0-b912-4a7b-9e01-c92ba92e27dc', distributed).
narrative_ontology:cs_authority_grounding('139fdec0-b912-4a7b-9e01-c92ba92e27dc', practice).
narrative_ontology:cs_interpretation_layer_present('139fdec0-b912-4a7b-9e01-c92ba92e27dc').
narrative_ontology:cs_reading_relation('139fdec0-b912-4a7b-9e01-c92ba92e27dc', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('139fdec0-b912-4a7b-9e01-c92ba92e27dc', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_axiom('139fdec0-b912-4a7b-9e01-c92ba92e27dc', foundational, practical_knowledge_primary_ritual_function).
narrative_ontology:cs_axiom_status(practical_knowledge_primary_ritual_function, holdable).
narrative_ontology:cs_axiom_grounding('139fdec0-b912-4a7b-9e01-c92ba92e27dc', practical_knowledge_primary_ritual_function, empirically_contingent).
narrative_ontology:cs_axiom('139fdec0-b912-4a7b-9e01-c92ba92e27dc', secondary, form_without_content_is_extractive).
narrative_ontology:cs_axiom_status(form_without_content_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('139fdec0-b912-4a7b-9e01-c92ba92e27dc', form_without_content_is_extractive, deontological).
narrative_ontology:cs_reference_frame('139fdec0-b912-4a7b-9e01-c92ba92e27dc', oral_tradition_competence_transmission).
narrative_ontology:cs_drift_state('139fdec0-b912-4a7b-9e01-c92ba92e27dc', post_literacy_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('139fdec0-b912-4a7b-9e01-c92ba92e27dc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, adaptive_family_networks).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, formalist_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, displaced_traditional_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, ritual_authorities).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, practical_knowledge_embedding_in_ritual).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, survival_competence_transmission_through_rite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the ritual calendar, authorize transmitters, and police boundary between correct and incorrect performance. They derive legitimacy and institutional continuity from maintaining the form. Their exit would mean institutional dissolution — the ritual IS the institution. They also benefit from the practical knowledge when catastrophes occur, but their primary structural position is agenda-setting.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_authorities, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, ritual_authorities, beneficiary).

% Maintain ritual practice as a portable survival system: the rites encode migration timing, resource caching, family reorganization, and host-society navigation. They actively decode the practical layer and adapt it to new environments. Leaving the ritual means losing a tested adaptive infrastructure, but they can and do modify the form to fit new conditions — exit is constrained but possible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, biographical, constrained, global).

% Extended kin groups that use ritual moments (life-cycle rites, seasonal observances) to transmit practical protocols: land management, conflict resolution, marriage alliance maintenance, crisis decision-making. They treat ritual as a scheduled coordination point for updating shared practical knowledge. Exit is mobile — they can shift to non-ritual coordination (family councils, digital archives) but lose the catastrophe-resilient transmission channel.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, adaptive_family_networks, beneficiary,
    moderate, biographical, mobile, local).

% Communities that maintain full ritual observance but have lost the practical decoding — the rites are performed correctly but the survival knowledge they once carried is no longer taught or understood. They bear the time, resource, and opportunity costs of performance without the adaptive return. Exit is identity_locked: abandoning the ritual would dissolve the communal self-concept ('we are the people who keep these rites').
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, formalist_communities, payer,
    moderate, generational, identity_locked, regional).

% Groups forcibly separated from their ritual landscapes (sacred sites, seasonal rounds, material substrates) who attempt to maintain practice in exile. They pay the highest cost — performance without place, transmission without elders, adaptation without the encoded heuristics for their specific ecology. Exit is trapped: they cannot return to the landscape, and the ritual in exile extracts labor without delivering its designed survival value.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, displaced_traditional_groups, payer,
    powerless, immediate, trapped, local).

% External analysts documenting the constraint's operation across communities. They neither collect nor pay; they map the structural relationships between form, content, and adaptive outcomes. Their seat is the analytical reference frame from which the epsilon-invariance of this reading is assessed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologist_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual schedules and structures the transmission of practical survival knowledge across generations and catastrophe cycles — solving the coordination problem of 'how does a community ensure critical heuristics survive when the elders who hold them die, and the catastrophe that validates them is rare?'
% TRANSFER_FUNCTION: Moves practical survival competence (timing, resource management, family protocols, adaptation strategies) from ritual authorities and elder transmitters to performing communities; moves compliance labor and resource expenditure from performing communities to ritual authorities (in status, legitimacy, material support).
% ABSENT_VOICES: Communities that have already lost the practical decoding and would object to the ritual's continuation as pure form — they are not in the conversation because their objection would dissolve the very identity that gives them standing. Also absent: communities facing novel catastrophes (climate collapse, algorithmic governance) for which the ritual encodes no heuristics — they cannot consent to a transmission system that has no content for their survival.
% DISAPPEARANCE_RATIONALE: If the ritual-as-practical-transmission constraint vanished, diaspora communities would lose a portable, catastrophe-resilient knowledge infrastructure; adaptive family networks would lose their scheduled coordination point for updating shared protocols; formalist communities would lose their identity-constituting practice (but not their practical deficit, which predates the disappearance); displaced groups would lose even the attenuated connection to their traditional heuristics. The world rearranges because the coordination function is real and not fully substitutable by written or digital media under catastrophe conditions.
% FOUNDING_PROBLEM: How to transmit survival-critical practical knowledge (when to migrate, how to allocate scarce resources, how to reorganize family structures under stress, how to adapt to novel environments) across generations when the catastrophes that validate the knowledge are rare, the elders who hold it die, and the environments that require it change?
% FOUNDING_PROBLEM_CORROBORATION: Diaspora community elders and adaptive family network heads attest the problem is live — they actively use ritual to transmit protocols that have no written equivalent and have proven effective under displacement. Formalist community leaders attest the problem is dead — 'we have writing, institutions, and science now; the ritual is for identity, not survival.' Anthropological literature (outside both beneficiary sets) corroborates that ritual-encoded practical knowledge outperforms written transmission under conditions of societal collapse, displacement, and literacy loss (e.g., post-colonial Pacific navigation rites, Saami seasonal round encoding, Jewish diaspora calendar-as-migration-map).
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).
:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint extracts practical knowledge labor from performing communities while delivering adaptive value to those who decode it. The rise from 0.32 to 0.45 over the interval reflects secularization and institutional capture — formalist communities pay the cost of performance without the survival return. Suppression (0.35) is modest but rising: communities that attempt to strip ritual to practical core face boundary-policing from formalist institutions. Theater ratio (0.28) reflects that a growing share of ritual performance is performative maintenance of form without practical transmission. Accessibility collapse (0.52) and resistance (0.48) are mid-range: alternatives (written manuals, oral instruction outside rite) exist but are less robust under catastrophe conditions.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora beneficiary seat, the constraint appears as a rope — genuine coordination delivering survival value. From the formalist victim seat, it appears as a snare — extraction of labor and compliance without the promised return. From the ritual authority seat, it appears as a tangled rope — they genuinely coordinate transmission but also extract status and control from maintaining the form. The engine computes this divergence from the structural data; the authored claim (tangled_rope) represents the analytical observer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and adaptive family networks are structural beneficiaries (d near 0.15): they receive adaptive capacity from the constraint's operation. Formalist communities and displaced traditional groups are victims (d near 0.85): they bear the performance cost while the practical content atrophies. The agenda-setter seat (ritual authorities, lineage holders) sits near symmetric (d ~ 0.5) — they administer the transmission but also depend on it for legitimacy. Exit options differentiate: diaspora groups have constrained exit (can abandon ritual but lose adaptive infrastructure); formalist communities are identity_locked (ritual performance constitutes their communal self-concept).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting survival competence across catastrophe cycles — is contested (some communities face no active catastrophe; others face novel catastrophes the ritual doesn't encode). The arrangement persists partly because the form itself became the identity marker (mandatrophy: the mandate 'transmit survival knowledge' has atrophied into 'maintain ritual form'). The mandatrophy is not resolved: formalist communities maintain form without function; diaspora communities maintain function by adapting form. This tension is the engine of the constraint's current dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the competence_transmission_reading a distinct constraint from the hybrid_encoding_reading and symbol_survival_reading, or do they describe different observational facets of the same structural constraint?',
    'Compare epsilon values and victim/beneficiary structures across readings: if competence_transmission has moderate extraction targeting formalist communities while hybrid_encoding shows lower extraction with broader coordination, they are distinct constraints per epsilon-invariance.',
    'If they are one constraint, the kernel framework collapses and all three readings must merge into a single story; if distinct, the network of affects_constraints correctly models their structural influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s three readings instantiate three epsilon-invariant constraints or one constraint viewed from three angles.').

omega_variable(
    practical_knowledge_measurement,
    'How to distinguish practical survival knowledge embedded in ritual from symbolic boundary-maintenance when both appear in the same performance?',
    'Longitudinal study of communities that lost practical content but retained form versus those that retained both: measure adaptive capacity under stress (resource scarcity, displacement, regime change).',
    'If practical content is separable and its loss correlates with reduced adaptive capacity, the extraction metric (moderate ε) is justified; if inseparable, the constraint may be lower extraction than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practical_knowledge_measurement, empirical, 'Whether practical and symbolic registers in ritual are structurally separable for measurement purposes.').

omega_variable(
    diaspora_beneficiary_specificity,
    'Do all diaspora communities gain adaptive capacity from ritual-encoded practical knowledge, or only those maintaining specific transmission lineages?',
    'Compare adaptive outcomes across diaspora groups with high versus low ritual practice continuity, controlling for socioeconomic variables.',
    'If benefit is lineage-specific, the beneficiary declaration ''diaspora_communities'' is overbroad and directionality for some groups shifts toward payer; if universal, the beneficiary set is correct as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_beneficiary_specificity, empirical, 'Whether the beneficiary effect is universal across diaspora or conditional on transmission fidelity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).

% DUAL FORMULATION NOTE:
% Kernel 'catastrophe_memory_survival' decomposes into three readings with distinct epsilon values and victim/beneficiary structures. competence_transmission_reading (this story): moderate ε, victims = formalist communities, beneficiaries = diaspora. hybrid_encoding_reading: lower ε, coordination dominates, broader beneficiary set. symbol_survival_reading: near-zero ε, mountain-like, victims only if symbolic continuity is contested. The three are linked via affects_constraints; each instantiates a different constraint from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, institutional, 0.48).
constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, organized, 0.2).
constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
