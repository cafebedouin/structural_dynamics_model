% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition (Life/Harvest vs Death/Salvation)
 *   domain: religious_studies/historical
 *
 * SUMMARY:
 *   The domain partition reading holds that Japanese religious life from the
 *   Heian through early Edo periods operated under a stable functional
 *   division: kami presided over this-worldly concerns (agriculture, purity,
 *   procreation), while Buddhist deities managed other-worldly salvation.
 *   This was not a theological synthesis but a practical jurisdictional
 *   settlement, sustained by court ritual, village custom, and institutional
 *   boundary maintenance. The reading treats the arrangement as a genuine
 *   coordination device that nonetheless imposed duplicate institutional
 *   costs on the populace and suppressed unified theological alternatives.
 *
 * KEY AGENTS:
 *   - Buddhist temple networks: Primary beneficiary (institutional/constrained) â controls funeral and salvation rites.
 *   - Shinto shrine networks: Primary beneficiary (institutional/constrained) â controls harvest and purity rites.
 *   - Imperial court: Agenda-setter (institutional/mobile) â administers ritual jurisdiction codes.
 *   - Village communities: Primary payer (powerless/trapped) â must sustain and pay both institutional circuits.
 *   - Syncretic theologians: Payer (moderate/identity_locked) â advocates of ontological unification marginalized by the partition.
 *   - Modern scholars: Observer (analytical) â adjudicate between competing historical readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.45).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Domain Partition (Life/Harvest vs Death/Salvation)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '13be415c-4ab0-43ab-9528-5de5be7f2014').
narrative_ontology:cs_kernel_codification('13be415c-4ab0-43ab-9528-5de5be7f2014', implicit).
narrative_ontology:cs_authority_grounding('13be415c-4ab0-43ab-9528-5de5be7f2014', practice).
narrative_ontology:cs_interpretation_layer_present('13be415c-4ab0-43ab-9528-5de5be7f2014').
narrative_ontology:cs_reading_relation('13be415c-4ab0-43ab-9528-5de5be7f2014', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('13be415c-4ab0-43ab-9528-5de5be7f2014', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('13be415c-4ab0-43ab-9528-5de5be7f2014', foundational, domain_specificity_of_divine_agency).
narrative_ontology:cs_axiom_status(domain_specificity_of_divine_agency, holdable).
narrative_ontology:cs_axiom_grounding('13be415c-4ab0-43ab-9528-5de5be7f2014', domain_specificity_of_divine_agency, conventional).
narrative_ontology:cs_axiom('13be415c-4ab0-43ab-9528-5de5be7f2014', foundational, ritual_jurisdiction_over_theological_unity).
narrative_ontology:cs_axiom_status(ritual_jurisdiction_over_theological_unity, holdable).
narrative_ontology:cs_axiom_grounding('13be415c-4ab0-43ab-9528-5de5be7f2014', ritual_jurisdiction_over_theological_unity, conventional).
narrative_ontology:cs_reference_frame('13be415c-4ab0-43ab-9528-5de5be7f2014', classical_domain_partition).
narrative_ontology:cs_drift_state('13be415c-4ab0-43ab-9528-5de5be7f2014', medieval_syncretic_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13be415c-4ab0-43ab-9528-5de5be7f2014', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, village_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manage funeral rites, memorial services, and other-worldly salvation. They receive patronage, land, and labor from communities in exchange for ritual services. Their institutional survival depends on maintaining exclusive jurisdiction over death and the afterlife within the partitioned order.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_networks, beneficiary,
    institutional, generational, constrained, national).

% Oversee harvest festivals, purity rituals, and this-worldly blessings. They receive offerings and community support. Their authority rests on control of kami rites for life, birth, and agriculture, protected from absorption by Buddhist eschatological frameworks.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks, beneficiary,
    institutional, generational, constrained, national).

% Administers the ritual calendar, ranks shrines and temples, and adjudicates jurisdictional disputes between religious institutions. It benefits from social stability and reduced sectarian conflict but must expend political capital to maintain the boundary and manage competing patronage claims.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court, agenda_setter,
    institutional, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court, beneficiary).

% Participate in both shrine festivals and Buddhist funeral rites. They provide material support, labor, and fees to both temple and shrine. They cannot opt out of either circuit without losing essential social and religious services, effectively sustaining two parallel institutional tax bases.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_communities, payer,
    powerless, biographical, trapped, local).

% Develop theological frameworks that unify kami and buddhas into a single ontological hierarchy. Their proposals are marginalized by the institutional partition, which treats theological unification as a threat to jurisdictional stability and the established ritual order.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, syncretic_theologians, payer,
    moderate, biographical, identity_locked, national).

% Analyze the historical arrangement from outside the system, debating whether the partition was a genuine functional equilibrium, a mask for structural subordination, or an incoherent bundle of local practices held together by inertia.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, modern_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates ritual jurisdiction between Buddhist and Shinto institutions, preventing territorial and theological conflict by assigning clear functional domains: kami for this-worldly life concerns, buddhas for other-worldly salvation.
% TRANSFER_FUNCTION: Moves material support and labor from village communities to both temple and shrine networks simultaneously; moves cognitive authority away from unified theology toward a functional partition maintained by practice.
% ABSENT_VOICES: Peasant communities who might prefer a single institutional locus for all rites had no voice in theological boundary-setting; syncretic theologians advocating ontological unification were structurally excluded from legitimacy.
% DISAPPEARANCE_RATIONALE: If the partition vanished, either sectarian conflict over ritual jurisdiction would erupt, or a unified syncretic framework would absorb one side into the other; village practice, state ritual, and institutional finance would all reorganize.
% FOUNDING_PROBLEM: How to integrate the continental Buddhist church and indigenous kami worship into a single society without constant institutional warfare, doctrinal schism, or competitive violence over ritual turf.
% FOUNDING_PROBLEM_CORROBORATION: Court diaries such as KujÅ Kanezane's GyokuyÅ and legal codifications like the Engishiki attest to state management of shrine-temple relations. Modern historians including Kuroda Toshio and Mark Teeuwen corroborate the conflict-management reading from outside the benefiting religious institutions.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the partition, while preventing conflict, required communities to maintain dual patronage obligations. Suppression (0.40) reflects the active exclusion of honji-suijaku unification and pure-Shinto revivalism as legitimate frameworks within the partition. Theater ratio (0.25) is low: the functional division was materially real in ritual practice, not merely rhetorical. Accessibility collapse (0.50) is moderate because once the partition was naturalized, syncretic alternatives became cognitively distant. Resistance (0.30) is low-moderate: syncretic movements existed but were largely absorbed or marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The temple and shrine networks experienced the constraint as protective turf; village communities experienced it as a mandatory double subscription; syncretic theologians experienced it as a cap on theological innovation. The engine should compute low directionality for institutions and high directionality for payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist and Shinto institutions are beneficiaries (low d) because the partition secures their ritual jurisdictions and revenue streams. Village communities and syncretic theologians are payers (high d) because they bear the costs of dual patronage and suppressed alternatives. The imperial court sits nearer symmetric: it administers and benefits from stability but bears the political cost of managing boundary disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â integrating two religious systems â was live in the early classical period. By the medieval era, the arrangement had become a self-sustaining equilibrium. The Meiji restoration's forced separation (shinbutsu bunri) demonstrates that the pre-modern partition had not fully eliminated underlying tension, suggesting the coordination had accumulated mild inertial extraction, though not enough to qualify as a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_practice_vs_elite_enforcement,
    'Was the domain partition maintained primarily by popular practice and mutual institutional interest, or by active state enforcement and ritual jurisprudence?',
    'Archival analysis of court edicts, temple-shrine contracts, and village records to determine the ratio of bottom-up custom to top-down regulation in boundary maintenance.',
    'If enforcement was elite-driven, the constraint''s suppression and active-enforcement scores should be higher; if practice-driven, the constraint reads closer to a rope or emergent norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_practice_vs_elite_enforcement, empirical, 'Ambiguity between popular practice and state enforcement as the source of boundary maintenance').

omega_variable(
    institutional_symmetry,
    'Was the partition symmetric, or did Buddhist eschatological superiority make Shinto structurally subordinate despite the parallel domain mapping?',
    'Comparative analysis of patronage flows, land grants, and doctrinal rhetoric to assess whether extraction ran primarily toward Buddhist institutions.',
    'If asymmetric, one beneficiary seat captures most gains and the constraint tilts toward snare; if symmetric, extraction is more diffuse and the tangled-rope framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_symmetry, empirical, 'Whether the partition hid an asymmetric extraction toward Buddhist institutions').

omega_variable(
    kernel_reading_position,
    'How does this domain-partition reading relate to its sibling readings structurally?',
    'Cross-reading comparison of beneficiary/victim structures: syncretic_fusion_reading would subordinate kami to buddhas, reversing shrine-network directionality; incoherent_bundle_reading would deny all coherent structural relationships.',
    'This reading is one of three epsilon-invariant constraints derived from the same kernel. Its classification as tangled_rope depends on accepting the partition as a real, enforced arrangement rather than a fusionist hierarchy or an incoherent bundle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega documenting this reading''s position within the shinbutsu kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t160, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 160, 0.13).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t320, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 320, 0.17).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t480, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 480, 0.2).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t640, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 640, 0.23).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.25).

% Extraction over time
narrative_ontology:measurement(shinbutsu_domain_partition_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t160, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 160, 0.34).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t320, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 320, 0.38).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t480, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 480, 0.4).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t640, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 640, 0.43).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_domain_partition_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t160, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 160, 0.28).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t320, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 320, 0.32).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t480, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 480, 0.35).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t640, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 640, 0.38).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 800, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the shinbutsu_coexistence_commitment family. It shares the historical referent with syncretic_fusion_reading and incoherent_bundle_reading but carries a distinct epsilon and stakeholder structure due to its domain-partition interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
