% ============================================================================
% CONSTRAINT STORY: syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_syncretic_fusion_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: syncretic_fusion_reading
 *   human_readable: Syncretic Fusion Reading: Kami as Buddha-Nature Manifestations
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The honji-suijaku (original ground - manifest trace) doctrine emerged in
 *   Heian Japan (794-1185) as a syncretic framework unifying kami worship and
 *   Buddhism. The doctrine asserts that kami are local manifestations
 *   (suijaku) of universal buddha-nature (honji): kami are not independent
 *   deities but phenomenal expressions of the same enlightened reality that
 *   buddhas embody. This ontological subordination solved a coordination
 *   problem (how to integrate indigenous kami cults into the Buddhist
 *   institutional order without violent suppression) but also created an
 *   extraction mechanism (Buddhist institutions gained doctrinal authority
 *   over kami worship, concentrating religious legitimacy and resources). The
 *   constraint intensified through the Kamakura period (1185-1333) as
 *   temple-shrine complexes (jingūji) formalized the administrative
 *   integration, peaked in the Muromachi period (1336-1573), and began to
 *   erode in the Edo period (1603-1868) as nativist movements (kokugaku)
 *   challenged Buddhist ontological supremacy. The Meiji shinbutsu bunri
 *   (kami-buddha separation) edict of 1868 formally dismantled the syncretic
 *   fusion, but whether this constituted a genuine sunset or merely a surface
 *   rearrangement remains contested (omega variable). The constraint is ONE
 *   READING of a contested kernel: the domain partition reading and the
 *   pragmatic incoherence reading are sibling alternatives with different
 *   beneficiary structures and different ε values.
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Authority: Primary beneficiary (institutional/arbitrage) — gains doctrinal authority over kami cults, integrates indigenous worship into Buddhist cosmology and temple infrastructure
 *   - Syncretic Temple Complexes (jingūji): Secondary beneficiary (institutional/mobile) — coordinate kami-buddha ritual calendars, pool resources, attract pilgrimage traffic
 *   - Indigenous Kami Cult Practitioners: Primary victim (powerless/identity_locked) — kami reduced to derivative manifestations of buddha-nature, ontological autonomy of ancestral cults erased
 *   - Local Shrine Autonomy: Secondary victim (moderate/constrained) — administrative subordination to Buddhist temple networks, loss of doctrinal independence
 *   - Syncretic Reform Movement: Organized agents (organized/mobile) — Tendai and Shingon syncretists, Ryōbu Shintō theorists who see honji-suijaku as transitional pedagogical device with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (religious pluralism without violent suppression) alongside extraction mechanism (ontological subordination, institutional concentration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(syncretic_fusion_reading, 0.48).
domain_priors:suppression_score(syncretic_fusion_reading, 0.62).
domain_priors:theater_ratio(syncretic_fusion_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(syncretic_fusion_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(syncretic_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(syncretic_fusion_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(syncretic_fusion_reading, "Syncretic Fusion Reading: Kami as Buddha-Nature Manifestations").
narrative_ontology:topic_domain(syncretic_fusion_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(syncretic_fusion_reading, 'ca0f1e01-993b-4918-92dd-f571053b1e42').
narrative_ontology:cs_kernel_codification('ca0f1e01-993b-4918-92dd-f571053b1e42', formalized).
narrative_ontology:cs_authority_grounding('ca0f1e01-993b-4918-92dd-f571053b1e42', lineage).
narrative_ontology:cs_interpretation_layer_present('ca0f1e01-993b-4918-92dd-f571053b1e42').
narrative_ontology:cs_reading_relation('ca0f1e01-993b-4918-92dd-f571053b1e42', syncretic_fusion_reading__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca0f1e01-993b-4918-92dd-f571053b1e42', syncretic_fusion_reading__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('ca0f1e01-993b-4918-92dd-f571053b1e42', foundational, ontological_monism_buddha_nature).
narrative_ontology:cs_axiom_status(ontological_monism_buddha_nature, holdable).
narrative_ontology:cs_axiom_grounding('ca0f1e01-993b-4918-92dd-f571053b1e42', ontological_monism_buddha_nature, deontological).
narrative_ontology:cs_axiom('ca0f1e01-993b-4918-92dd-f571053b1e42', secondary, kami_phenomenal_derivative_status).
narrative_ontology:cs_axiom_status(kami_phenomenal_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('ca0f1e01-993b-4918-92dd-f571053b1e42', kami_phenomenal_derivative_status, deontological).
narrative_ontology:cs_reference_frame('ca0f1e01-993b-4918-92dd-f571053b1e42', heian_syncretic_cosmology).
narrative_ontology:cs_drift_state('ca0f1e01-993b-4918-92dd-f571053b1e42', edo_nativist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca0f1e01-993b-4918-92dd-f571053b1e42', '').
narrative_ontology:cs_kernel_id(syncretic_fusion_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, syncretic_temple_complexes).
narrative_ontology:constraint_victim(syncretic_fusion_reading, indigenous_kami_cult_practitioners).
narrative_ontology:constraint_victim(syncretic_fusion_reading, local_shrine_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS KAMI CULT PRACTITIONER (SNARE) — Identity-locked within local kami worship tradition; the syncretic fusion subordinates kami to buddha-nature, erasing ontological autonomy of indigenous practice. Cannot exit without abandoning ancestral cult identity. Experiences maximum extraction: local kami reduced to derivative manifestations of foreign cosmology.
constraint_indexing:constraint_classification(syncretic_fusion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL SHRINE ADMINISTRATOR (TANGLED ROPE) — Constrained by institutional pressure from Buddhist temple networks but benefits from syncretic temple-shrine complexes (jingūji) that provide resources and pilgrimage traffic. Mixed experience: coordination function (shared ritual calendar, resource pooling) alongside extraction (subordination of kami to Buddhist ontology, loss of doctrinal autonomy).
constraint_indexing:constraint_classification(syncretic_fusion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — Primary beneficiary. The honji-suijaku framework solves the coordination problem of integrating indigenous kami worship into Buddhist cosmology without violent suppression. Arbitrage-level exit: can adopt or abandon syncretic fusion based on political utility. Experiences the constraint as pure coordination: kami cults become Buddhist institutional infrastructure.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SYNCRETIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (Tendai and Shingon syncretists, Ryōbu Shintō theorists) see the fusion as a transitional framework: the honji-suijaku doctrine is a pedagogical device for converting kami-worshippers, not a permanent ontological settlement. Sunset logic: once Buddhist practice is established, the scaffolding can be revised or discarded. Mobile exit: can shift to alternative syncretic formulations.
constraint_indexing:constraint_classification(syncretic_fusion_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The syncretic fusion is a genuine coordination mechanism (solves the problem of religious pluralism without violent suppression) AND an extraction mechanism (subordinates indigenous ontology to Buddhist metaphysics, concentrates doctrinal authority in Buddhist institutions). The constraint requires active enforcement (temple-shrine administrative integration, ritual calendar coordination, doctrinal policing) and produces asymmetric benefits: Buddhist institutions gain legitimacy and infrastructure; kami cults lose ontological autonomy.
constraint_indexing:constraint_classification(syncretic_fusion_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(syncretic_fusion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(syncretic_fusion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(syncretic_fusion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The honji-suijaku framework subordinates kami to buddha-nature, concentrating doctrinal authority in Buddhist institutions and erasing the ontological autonomy of indigenous kami cults. The extraction is substantial but not maximal: kami worship is not eliminated, merely reframed within Buddhist cosmology. The trajectory shows extraction rising from early Heian (0.30) through Kamakura (0.48) and Muromachi (0.52), then declining in Edo (0.45) as nativist resistance grows, and collapsing at Meiji (0.15) with formal separation. Suppression (0.62): Moderate-high. The constraint requires active enforcement through temple-shrine administrative integration, ritual calendar coordination, and doctrinal policing. Suppression rises from early Heian (0.40) to peak in Muromachi (0.68) as jingūji complexes formalize the hierarchy, then declines in Edo (0.60) and collapses at Meiji (0.25). Theater ratio (0.35): Moderate-low. The honji-suijaku doctrine is not primarily performative — it is a genuine ontological claim with real institutional consequences. Theater increases over time (0.20 → 0.65) as the doctrine becomes ritualized and the original coordination function atrophies, but remains below the piton threshold during the constraint's active period.
 *
 * PERSPECTIVAL GAP:
 *   The syncretic fusion reading produces a wide perspectival gap. Buddhist institutional authority sees pure coordination (rope): the honji-suijaku framework solves the problem of integrating indigenous worship without violent suppression. Indigenous kami cult practitioners see pure extraction (snare): their deities are reduced to derivative manifestations of foreign cosmology, and they cannot exit without abandoning ancestral identity. Local shrine administrators see mixed coordination and extraction (tangled rope): they benefit from syncretic temple-shrine complexes but bear the cost of doctrinal subordination. Syncretic reform movements see a transitional framework (scaffold): the doctrine is a pedagogical device that can be revised or discarded once Buddhist practice is established. The analytical observer sees genuine coordination function alongside extraction mechanism (tangled rope): the constraint solves religious pluralism without violence but subordinates indigenous ontology to Buddhist metaphysics. The gap reveals that the 'same' syncretic fusion is structurally different constraints from different seats: coordination for beneficiaries, extraction for victims, transition for reformers.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional authority is the primary beneficiary: the honji-suijaku framework integrates kami cults into Buddhist cosmology and temple infrastructure, concentrating religious legitimacy and resources. The engine derives low d (beneficiary status + arbitrage exit) → low or negative χ. Indigenous kami cult practitioners are the primary victims: their ancestral deities are ontologically subordinated to buddha-nature, erasing the autonomy of local kami worship. The engine derives high d (victim status + identity_locked exit) → high χ. Local shrine administrators experience mixed coordination and extraction (tangled rope): they benefit from syncretic temple-shrine complexes (resource pooling, pilgrimage traffic) but bear the cost of doctrinal subordination. The engine derives moderate d (victim status + constrained exit, modulated by beneficiary participation) → moderate χ. Syncretic reform movements see the constraint as transitional (scaffold): the honji-suijaku doctrine is a pedagogical device with sunset logic, not a permanent ontological settlement. The engine derives low-moderate d (organized power + mobile exit) → low-moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic fusion reading resolves mandatrophy by distinguishing genuine coordination function (integrating kami worship into Buddhist institutional order without violent suppression) from extraction mechanism (ontological subordination of kami to buddha-nature, concentration of doctrinal authority in Buddhist institutions). The constraint is a tangled rope: it BOTH coordinates (solves religious pluralism problem) AND extracts (subordinates indigenous cosmology). The perspectival gap is not a measurement error but the structural reality: beneficiaries experience coordination, victims experience extraction, and both are correct from their respective seats. The Meiji shinbutsu bunri edict tests the scaffold hypothesis: if the syncretic fusion was genuinely transitional, the separation should have eliminated the extraction mechanism; if it was surface rearrangement, the underlying structure should have persisted under new labels (omega variable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_subordination,
    'Does the honji-suijaku framework assert genuine ontological subordination (kami are metaphysically derivative of buddha-nature) or merely institutional subordination (kami cults are administratively integrated into Buddhist temple networks)?',
    'Textual analysis of honji-suijaku treatises (Tendai and Shingon doctrinal texts, Ryōbu Shintō commentaries) distinguishing metaphysical claims from institutional arrangements; ethnographic evidence of practitioner understanding (do worshippers experience kami as ontologically subordinate or as administratively coordinated?)',
    'If ontological: the constraint is more extractive (indigenous cosmology is erased, not merely coordinated). If institutional: the constraint is more coordinative (administrative integration without metaphysical subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_institutional_subordination, conceptual, 'Whether honji-suijaku asserts ontological or institutional subordination').

omega_variable(
    committer_frame_kernel_ambiguity,
    'Is the syncretic fusion reading the only coherent interpretation of the kami-buddha relationship in Heian-Kamakura Japan, or is it one reading of a contested kernel with sibling alternatives (domain partition, pragmatic incoherence)?',
    'Historical analysis of alternative frameworks: domain partition reading (kami govern worldly affairs, buddhas govern salvation — separate jurisdictions, not hierarchical subordination); pragmatic incoherence reading (practitioners hold both kami and buddha commitments without resolving ontological relationship). Evidence: ritual practice that violates honji-suijaku hierarchy, doctrinal texts that resist subordination, regional variation in syncretic formulations.',
    'If syncretic fusion is the only reading: the constraint is a single historical fact. If it is one reading of a contested kernel: the constraint is a committer-axis variable, and the sibling readings (domain_partition_reading, pragmatic_incoherence_reading) are structurally distinct constraints with different beneficiary sets and different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_ambiguity, conceptual, 'Whether syncretic fusion is the only reading or one of multiple contested readings').

omega_variable(
    meiji_shinbutsu_bunri_sunset,
    'Did the Meiji shinbutsu bunri (kami-buddha separation) edict of 1868 constitute a genuine sunset of the syncretic fusion constraint, or merely a surface rearrangement that left the underlying coordination-extraction structure intact?',
    'Post-Meiji ethnographic and institutional analysis: did shrine-temple separation eliminate Buddhist ontological authority over kami, or did it merely formalize separate administrative domains while preserving Buddhist metaphysical influence? Evidence: persistence of syncretic practice at the popular level, continued Buddhist institutional influence over shrine doctrine, State Shinto''s adoption of Buddhist organizational models.',
    'If genuine sunset: the scaffold perspective is vindicated — the syncretic fusion was a transitional framework that was successfully dismantled. If surface rearrangement: the scaffold perspective is aspirational rather than structural — the extraction mechanism persisted under new labels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_shinbutsu_bunri_sunset, empirical, 'Whether Meiji shinbutsu bunri constituted a genuine sunset or surface rearrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(syncretic_fusion_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(syncfus_tr_t0, syncretic_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(syncfus_tr_t2, syncretic_fusion_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(syncfus_tr_t4, syncretic_fusion_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(syncfus_tr_t6, syncretic_fusion_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(syncfus_tr_t8, syncretic_fusion_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(syncfus_tr_t10, syncretic_fusion_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(syncfus_extract_heian_early, syncretic_fusion_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(syncfus_extract_heian_mid, syncretic_fusion_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(syncfus_extract_kamakura, syncretic_fusion_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(syncfus_extract_muromachi, syncretic_fusion_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(syncfus_extract_edo, syncretic_fusion_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(syncfus_extract_meiji, syncretic_fusion_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(syncfus_su_t0, syncretic_fusion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(syncfus_su_t2, syncretic_fusion_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(syncfus_su_t4, syncretic_fusion_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(syncfus_su_t6, syncretic_fusion_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(syncfus_su_t8, syncretic_fusion_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(syncfus_su_t10, syncretic_fusion_reading, suppression_requirement, 10, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(syncretic_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The syncretic fusion reading is one of three structurally distinct constraints in the kami-buddha ontology family. The domain partition reading has lower extractiveness (separate jurisdictions rather than hierarchical subordination) and different beneficiaries (both Buddhist and Shinto institutions maintain autonomy). The pragmatic incoherence reading has minimal extractiveness (no unified cosmology to enforce) and different victims (no ontological subordination, so no erasure of indigenous autonomy). The three readings are linked by network.affects_constraints because they compete for the same institutional and doctrinal space: adopting one reading creates structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
