% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Simultaneous Veneration (Kami: This-Worldly, Buddhas: Afterlife)
 *   domain: religious_studies/japanese_history/comparative_religion
 *
 * SUMMARY:
 *   This constraint story models the domain_partition_reading of the
 *   simultaneous_veneration kernel in pre-Meiji Japan. The reading asserts
 *   that kami and buddhas were functionally distinct: kami governed
 *   this-worldly prosperity (harvest, health, clan fortune) while buddhas
 *   governed afterlife salvation (rebirth, enlightenment, ancestral peace).
 *   Simultaneous veneration was not syncretic confusion but
 *   domain-appropriate specialization — practitioners petitioned kami for
 *   life-domain needs and buddhas for death-domain needs. The coordination
 *   function is a cognitive and ritual division of labor that reduces
 *   decision costs for practitioners and allocates ritual authority between
 *   shrine and temple institutions. No extraction is claimed: the arrangement
 *   persists because it works, not because alternatives are suppressed. The
 *   Meiji separation (1868) forcibly collapsed this coordination, which the
 *   reading treats as exogenous disruption, not internal failure.
 *
 * KEY AGENTS:
 *   - common_practitioners: Primary beneficiary (powerless/constrained) — gains cognitive economy and ritual clarity from domain partition
 *   - shrine_priests: Agenda setter (organized/local) — maintains kami domain, collects offerings for life-domain rituals
 *   - buddhist_clergy: Agenda setter (organized/local) — maintains buddha domain, collects offerings for death-domain rituals
 *   - meiji_reformers: Excluded (powerful/national) — imposed separation, dismantled the coordination
 *   - modern_scholars: Observer (analytical/universal) — debates which reading matches historical reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.15).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Simultaneous Veneration (Kami: This-Worldly, Buddhas: Afterlife)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/japanese_history/comparative_religion").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '1324f24a-8ec8-42e2-a95c-6892298dc13c').
narrative_ontology:cs_kernel_codification('1324f24a-8ec8-42e2-a95c-6892298dc13c', distributed).
narrative_ontology:cs_authority_grounding('1324f24a-8ec8-42e2-a95c-6892298dc13c', practice).
narrative_ontology:cs_interpretation_layer_present('1324f24a-8ec8-42e2-a95c-6892298dc13c').
narrative_ontology:cs_reading_relation('1324f24a-8ec8-42e2-a95c-6892298dc13c', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1324f24a-8ec8-42e2-a95c-6892298dc13c', simultaneous_veneration__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('1324f24a-8ec8-42e2-a95c-6892298dc13c', foundational, domain_appropriate_specialization).
narrative_ontology:cs_axiom_status(domain_appropriate_specialization, holdable).
narrative_ontology:cs_axiom_grounding('1324f24a-8ec8-42e2-a95c-6892298dc13c', domain_appropriate_specialization, conventional).
narrative_ontology:cs_axiom('1324f24a-8ec8-42e2-a95c-6892298dc13c', foundational, functional_distinction_over_ontological_identity).
narrative_ontology:cs_axiom_status(functional_distinction_over_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('1324f24a-8ec8-42e2-a95c-6892298dc13c', functional_distinction_over_ontological_identity, conventional).
narrative_ontology:cs_reference_frame('1324f24a-8ec8-42e2-a95c-6892298dc13c', pre_meiji_honji_suijaku_practice).
narrative_ontology:cs_drift_state('1324f24a-8ec8-42e2-a95c-6892298dc13c', meiji_separation_edicts, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('1324f24a-8ec8-42e2-a95c-6892298dc13c', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, common_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, buddhist_clergy).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_specialization_of_divine_powers).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, domain_appropriate_petitioning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary people navigating life-cycle needs: visit shrines for harvest prayers, healing, business success, childbirth; visit temples for funerals, ancestral memorials, afterlife assurance. They gain cognitive economy — no need to reconcile ontologies, just follow domain-appropriate practice. Exit is constrained: social expectation and family tradition guide domain-appropriate visits, but no penalty for crossing domains. Offerings are voluntary and small.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, common_practitioners, beneficiary,
    powerless, biographical, constrained, local).

% Hereditary priestly families maintaining shrine rituals for kami. They administer the life-domain jurisdiction: agricultural rites, clan festivals, personal petitions (kitō). They receive offerings and ritual fees. Their identity is fused with the shrine institution — exit means abandoning hereditary vocation and communal role. They benefit from the domain partition (protected jurisdiction) but also maintain it (agenda-setting).
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_priests, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, shrine_priests, beneficiary).

% Monastic and hereditary temple clergy maintaining Buddhist rituals for buddhas. They administer the death-domain jurisdiction: funerals, memorial services (kuyō), ancestral rites, salvation assurance. They receive offerings, funeral fees, parishioner support (danka system). Like shrine priests, their identity is fused with the temple institution. They benefit from protected death-domain jurisdiction but also maintain the boundary.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_clergy, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, buddhist_clergy, beneficiary).

% State officials and nativist scholars (kokugakusha) who engineered shinbutsu bunri (separation of kami and buddhas) from 1868. They were not subject to the domain partition constraint — they operated from state power to dismantle it. Their goal: consolidate Shinto as state ideology, strip Buddhist institutions of land/authority. They had arbitrage-grade exit (state power) and imposed a new constraint (State Shinto) by force.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, meiji_reformers, excluded,
    powerful, biographical, arbitrage, national).

% Historians of religion, anthropologists, and Japanese studies scholars analyzing the simultaneous_veneration kernel. They hold no stake in the domain partition's operation but produce the readings that become constraint stories. Their analytical seat sees all three readings as live interpretive options. They do not collect from or pay into the historical constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the divine petitioning coordination problem: practitioners face diverse needs (health, harvest, business, death, ancestors, salvation) and diverse divine powers (kami, buddhas). The domain partition assigns each need-type to a specialist divine power, reducing search costs and ritual error. Institutions (shrines/temples) get protected jurisdictions, reducing inter-institutional conflict.
% TRANSFER_FUNCTION: Moves voluntary offerings and ritual fees from practitioners to shrine priests (for life-domain rituals) and Buddhist clergy (for death-domain rituals). No mandatory extraction; payments are gift-exchange for perceived ritual efficacy. The transfer is bidirectional: practitioners give offerings, receive ritual assurance; clergy give ritual service, receive material support.
% ABSENT_VOICES: Marginalized groups whose needs didn't fit the binary: women in childbirth (sometimes served by both, sometimes excluded from shrine precincts), outcaste communities (burakumin) served by separate Buddhist lineages, regional folk practitioners maintaining non-standard kami/buddha mappings. These voices were not in the room when the domain partition was formalized by priestly elites.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, practitioners would lose the cognitive map for which ritual to perform where. Shrine and temple jurisdictions would overlap, triggering turf conflicts. The Meiji separation demonstrated this: forced separation caused ritual confusion, loss of ancestral rites continuity, and violent conflict (haibutsu kishaku). The world rearranged violently when the constraint was removed.
% FOUNDING_PROBLEM: Pre-Heian Japan had imported Buddhism alongside native kami worship with no clear jurisdictional boundary. Practitioners faced ontological confusion: are kami and buddhas rivals, aspects of one reality, or separate? Institutions competed for the same ritual markets. The domain partition emerged (Heian-Kamakura) as a practical settlement: kami take this world, buddhas take the next.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) Contemporary practice — modern Japanese still use shrines for life events (weddings, births, new year) and temples for death events (funerals, memorials), independent of doctrinal commitment. (2) Folklorists (Yanagita Kunio, Ōrikuchi Shinobu) documented the life/death domain split as persistent folk taxonomy. (3) The Meiji state's need to violently enforce separation proves the partition was functional, not moribund — if incoherent, it would have collapsed without force.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the domain partition solves a genuine coordination problem (which divine power for which need) without transferring resources from practitioners to institutions beyond voluntary offerings. Suppression is low (0.15) — no enforcement machinery prevents practitioners from petitioning kami for afterlife or buddhas for harvest; the partition is maintained by shared cultural schema, not coercion. Theater ratio is minimal (0.08) — rituals have functional intent, not performative maintenance. Accessibility collapse is moderate (0.35) — alternatives exist (exclusive Shinto, exclusive Buddhism, folk syncretism) but the partition is the dominant low-friction path. Resistance is near-zero (0.10) — the system meets little opposition because it works for practitioners. The claimed type is rope (pure coordination). Metrics are authored independently of claim; if engine computes differently, that divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the constraint is invisible coordination — they simply know which shrine/temple to visit for which need. From the priestly seats, it is a jurisdictional settlement that prevents turf wars between shrines and temples. From the Meiji reformer seat (excluded), it is an obstacle to state Shinto consolidation. The engine will compute per-seat types from these structural positions; the domain_participant seat should compute rope, the priestly seats may compute rope or tangled_rope depending on whether offering flows are seen as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Common practitioners are beneficiaries (d ≈ 0.15): they receive coordination value (cognitive economy, ritual clarity) with minimal cost (voluntary offerings). Shrine priests and Buddhist clergy are agenda_setters with symmetric position (d ≈ 0.5): they maintain the domain boundary and receive offerings, but their authority depends on delivering perceived ritual efficacy — if practitioners stop seeing results, the priests lose standing. Meiji reformers are excluded (d not computed): they were not subject to the constraint but dismantled it from outside. Modern scholars are observers (d = 0.5 analytical). The derivation chain: beneficiaries declared → low d; agenda_setters with voluntary exchange → symmetric d; no victims declared → no high-d seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cognitive/ritual division of labor for divine petitioning) remains live — modern Japanese still visit shrines for life events (shichi-go-san, hatsumōde) and temples for funerals/ancestral rites. The domain partition persists informally despite Meiji separation. The arrangement was not built to solve a problem that disappeared; it solved a perennial coordination need. Mandatrophy is not resolved — the coordination function survives the formal institution's destruction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the domain_partition_reading a distinct constraint from the ontological_fusion_reading and pragmatic_incoherence_reading of the simultaneous_veneration kernel, or are they measurement variants of one constraint?',
    'Apply ε-invariance test: if measuring the constraint via domain-partition observables (petition success rates, domain-specific ritual uptake) yields ε ≈ 0.12 while measuring via ontological-coherence observables (doctrinal consistency, theological dispute intensity) yields ε ≈ 0.6+, they are distinct constraints.',
    'If distinct, each reading gets its own constraint story with independent ε, stakeholders, and classification. The domain_partition_reading classifies as rope; ontological_fusion_reading likely tangled_rope; pragmatic_incoherence_reading likely piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel decomposition: simultaneous_veneration kernel → three distinct constraint stories').

omega_variable(
    extraction_absence_verification,
    'Does the domain partition coordination genuinely operate without extraction, or does the priestly class extract rents (offerings, fees, status) from maintaining the domain boundary?',
    'Historical economic analysis of shrine/temple revenue streams: if domain-specific rituals (kitō for kami, kuyō for buddhas) generated substantial mandatory payments beyond voluntary offerings, extraction is non-zero.',
    'Non-zero priestly rent extraction would raise ε and potentially reclassify from rope to tangled_rope (coordination + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_absence_verification, empirical, 'Whether priestly maintenance of domain boundaries constitutes hidden extraction').

omega_variable(
    meiji_separation_causality,
    'Did the Meiji separation edicts (shinbutsu bunri) destroy a functioning coordination system (domain partition) or expose an already-incoherent practice?',
    'Comparative analysis of pre-Meiji practitioner behavior: if domain-appropriate petitioning was stable and low-conflict, separation destroyed coordination; if practitioners already confused domains or double-petitioned indiscriminately, separation exposed incoherence.',
    'Determines whether the domain_partition_reading''s reference frame (pre-Meiji coordinated practice) was genuine or projected backward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_causality, empirical, 'Causal status of Meiji separation relative to domain partition coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sv_dp_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.05).
narrative_ontology:measurement(sv_dp_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.06).
narrative_ontology:measurement(sv_dp_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.07).
narrative_ontology:measurement(sv_dp_tr_t1400, simultaneous_veneration__domain_partition_reading, theater_ratio, 1400, 0.08).
narrative_ontology:measurement(sv_dp_tr_t1600, simultaneous_veneration__domain_partition_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(sv_dp_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.12).

% Extraction over time
narrative_ontology:measurement(sv_dp_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(sv_dp_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(sv_dp_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.11).
narrative_ontology:measurement(sv_dp_be_t1400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1400, 0.12).
narrative_ontology:measurement(sv_dp_be_t1600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1600, 0.12).
narrative_ontology:measurement(sv_dp_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: simultaneous_veneration kernel decomposed into three readings. domain_partition_reading (this story) = rope, ε≈0.12. ontological_fusion_reading = likely tangled_rope (doctrinal enforcement extracts compliance), ε≈0.5+. pragmatic_incoherence_reading = likely piton (coordination atrophied, maintained by social inertia), ε≈0.3 with high theater. All three share stakeholders but differ in claimed_type, ε, and suppression. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__domain_partition_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
