% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the domain_partition_reading of the
 *   shinbutsu ontological substrate kernel: the claim that kami and buddhas
 *   govern separate ritual domains (this-world versus afterlife), making
 *   their historical coexistence in Japan functional and pragmatic rather
 *   than ontologically unified. This reading was historically
 *   institutionalized during the Meiji period's shinbutsu bunri (separation
 *   of kami and buddhas) policies, though it presents itself as merely
 *   describing an indigenous functional coordination. The reading is
 *   contested by the syncretic_fusion reading (honji suijaku as metaphysical
 *   truth) and the incoherent_bundle reading (no coherent kernel, only
 *   accumulated drift under state pressure).
 *
 * KEY AGENTS:
 *   - State religious administrators: Primary agenda-setter (institutional/arbitrage) â enforce and maintain the domain classification.
 *   - Shrine priesthood: Primary beneficiary (organized/constrained) â gains exclusive mandate over this-world rituals.
 *   - Buddhist clergy: Primary payer (organized/constrained) â loses this-world ritual authority to the partition.
 *   - Syncretic priesthood: Secondary payer (moderate/trapped) â combinatory practice is delegitimized.
 *   - Lay practitioners: Mixed beneficiary (moderate/mobile) â gain functional clarity at the cost of combinatory richness.
 *   - Religious studies scholars: Analytical observer (analytical/analytical) â tracks the divergence between official partition and folk practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.45).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.5).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '7918c4c3-38cb-4e30-8835-4699d9b05916').
narrative_ontology:cs_kernel_codification('7918c4c3-38cb-4e30-8835-4699d9b05916', implicit).
narrative_ontology:cs_authority_grounding('7918c4c3-38cb-4e30-8835-4699d9b05916', practice).
narrative_ontology:cs_interpretation_layer_present('7918c4c3-38cb-4e30-8835-4699d9b05916').
narrative_ontology:cs_reading_relation('7918c4c3-38cb-4e30-8835-4699d9b05916', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('7918c4c3-38cb-4e30-8835-4699d9b05916', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('7918c4c3-38cb-4e30-8835-4699d9b05916', foundational, functional_domain_separation).
narrative_ontology:cs_axiom_status(functional_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('7918c4c3-38cb-4e30-8835-4699d9b05916', functional_domain_separation, conventional).
narrative_ontology:cs_axiom('7918c4c3-38cb-4e30-8835-4699d9b05916', secondary, pragmatic_coexistence_without_ontological_unification).
narrative_ontology:cs_axiom_status(pragmatic_coexistence_without_ontological_unification, holdable).
narrative_ontology:cs_axiom_grounding('7918c4c3-38cb-4e30-8835-4699d9b05916', pragmatic_coexistence_without_ontological_unification, conventional).
narrative_ontology:cs_reference_frame('7918c4c3-38cb-4e30-8835-4699d9b05916', pragmatic_religious_dualism).
narrative_ontology:cs_drift_state('7918c4c3-38cb-4e30-8835-4699d9b05916', post_meiji_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7918c4c3-38cb-4e30-8835-4699d9b05916', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, state_religious_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the legal and bureaucratic classification of religious institutions, enforcing the separation of shrine and temple functions. They derive administrative control and political legitimacy from maintaining a clear domain partition between kami and buddha rituals.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_religious_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold an exclusive institutional mandate for this-world rituals, births, and community festivals under the domain partition. Their authority is bolstered by the state's recognition of shrines as distinct from temples, though this dependence on state classification limits their autonomy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthood, beneficiary,
    organized, biographical, constrained, national).

% Historically performed both this-world and afterlife rituals; under the domain partition they lose the this-world ritual monopoly and are confined to funerary and memorial domains. They retain temple assets and doctrinal authority but must accept a narrowed institutional role.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_clergy, payer,
    organized, biographical, constrained, national).

% Navigate religious needs by routing this-world concerns to shrines and afterlife concerns to temples. They gain functional clarity but must operate within a framework that discourages combinatory practice and doubles the institutional points of contact.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Maintained combinatory shrines and temples that honored both kami and buddhas under a unified ritual framework. The domain partition delegitimizes their practice, forcing them to choose a single institutional identity or operate in marginal spaces.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_priesthood, payer,
    moderate, biographical, trapped, regional).

% Analyze the domain partition as one contested reading of Japanese religious history, comparing it against syncretic fusion and incoherent bundle interpretations. They observe the divergence between official classification and folk practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a functional schema for managing the coexistence of kami worship and Buddhism by assigning distinct ritual domains â this-world concerns to shrines, afterlife concerns to temples â reducing doctrinal competition and giving practitioners a clear routing mechanism.
% TRANSFER_FUNCTION: Moves ritual authority and institutional legitimacy from syncretic and Buddhist complexes toward distinct shrine and state-administered spheres, while transferring the cognitive labor of religious navigation to lay practitioners.
% ABSENT_VOICES: Syncretic priests who practiced combinatory honji suijaku rituals are structurally excluded from the official framework; their claim that kami and buddhas form a unified ontological and ritual field is rendered illegitimate by the partition.
% DISAPPEARANCE_RATIONALE: If the domain partition commitment disappeared, the boundary between shrine and temple functions would blur, combinatory practices would resurface institutionally, and state administration of religion would lose its primary classificatory grid for managing the shinbutsu field.
% FOUNDING_PROBLEM: How to coordinate two major religious systems â kami worship and Buddhism â within a single society without institutional chaos or theological conflict.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians corroborate that the coordination problem existed but attest that Edo-period syncretism had already solved it; Buddhist clergy attest the partition created new problems rather than solving an old one. Corroboration from outside the state and shrine beneficiary sets supports the already-solved reading.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the domain partition allocates ritual authority asymmetrically: shrine institutions gain a state-backed monopoly on this-world concerns while Buddhist institutions are confined to funerary domains. Suppression is moderate (0.50) â the partition does not eliminate syncretic practice entirely but requires active administrative enforcement to maintain institutional separation, especially during the Meiji-Taisho period. Theater ratio (0.35) reflects the gap between the official narrative of clean functional separation and the continued reality of combinatory folk practice. Accessibility collapse is moderate (0.40): the syncretic fusion alternative was marginalized but not erased. Resistance (0.45) captures Buddhist institutional pushback and folk persistence. The temporal series shows a peak in extraction and suppression during the Meiji enforcement period (T20-T40), with partial relaxation but structural persistence into the contemporary era.
 *
 * PERSPECTIVAL GAP:
 *   From the state and shrine seats, the domain partition is necessary administrative clarity that prevents institutional chaos and gives practitioners clear routing. From the Buddhist and syncretic seats, the same arrangement is an artificial construct that destroys a historically integrated ritual economy and extracts authority from Buddhist institutions. The engine computes this divergence from the structural data: agenda-setters with arbitrage exit experience low directionality, while trapped syncretic priests experience high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   State religious administrators are structural beneficiaries with arbitrage-grade exit (they can change policy) and sit near the full-beneficiary end. Shrine priesthood are beneficiaries but with constrained exit (dependent on state recognition), placing them slightly above pure beneficiary. Buddhist clergy and syncretic priesthood are payers; the clergy have constrained exit (must comply to survive institutionally) while syncretic priests are trapped (their practice is delegitimized entirely), giving the latter a higher directionality. Lay practitioners are near-symmetric: they gain functional clarity but bear the cost of navigating a partitioned ritual landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â coordinating kami worship and Buddhism â was arguably solved by Edo-period syncretic fusion before the Meiji state manufactured the domain partition as a new 'solution.' The partition's persistence after its state-enforcement rationale waned suggests partial mandatrophy: the arrangement continues to extract institutional authority for shrine priesthood and administrative control for the state, even though its original coordination function (clarity under bureaucratic centralization) is no longer urgent. The founding_problem_status is contested because the state and shrine beneficiaries claim the problem remains live, while external scholars and Buddhist clergy claim it was already solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_construction_vs_spontaneous_emergence,
    'Does the domain partition between kami and buddhas reflect a spontaneous functional coordination in Japanese religious practice, or is it primarily a construct of the Meiji state''s administrative enforcement?',
    'Historical archival analysis of pre-Meiji ritual records and Meiji policy documents; ethnographic comparison of folk practice against official classification.',
    'If purely state-constructed, the constraint reclassifies toward snare; if spontaneous folk coordination, toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_construction_vs_spontaneous_emergence, empirical, 'Whether the domain partition is natural practice or state construction').

omega_variable(
    syncretism_suppression_mechanism,
    'Is the suppression of syncretic practice under the domain partition structural (legal and administrative barriers) or internalized (practitioners adopting separate Shinto or Buddhist identities)?',
    'Post-exit trajectory analysis: do combinatory practices resume when administrative enforcement relaxes, or do separate identities persist?',
    'If internalized, effective suppression exceeds structural measures; if structural, the constraint depends on active enforcement for persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretism_suppression_mechanism, empirical, 'Structural versus internalized suppression of syncretism').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the domain_partition_reading of the shinbutsu ontological substrate kernel. How would classification change if the syncretic_fusion reading or incoherent_bundle reading were adopted as the operative framework?',
    'Comparative structural analysis across the constraint family of kernel readings.',
    'The syncretic_fusion reading would likely classify as rope or mountain (theological coordination), while the incoherent_bundle reading would dissolve the constraint entirely by denying any coherent kernel exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural uncertainty from kernel reading multiplicity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
