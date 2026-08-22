% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading
 *   domain: religious_studies/japanese_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The domain-partition reading of shinbutsu coexistence treats the
 *   kami/Buddha boundary as a functional jurisdictional settlement: kami
 *   govern life, purity, and harvest; Buddhas govern death, salvation, and
 *   afterlife. This reading emphasizes that the arrangement operated for over
 *   a millennium (c. 700–1868) as a coordination mechanism — it allocated
 *   ritual jurisdictions, resource flows, and parishioner obligations between
 *   two parallel priestly hierarchies without requiring them to agree on
 *   ontology. The constraint's 'law' is the boundary itself, maintained by
 *   village elders and institutional complexes, not a unified doctrine.
 *   Popular practice (not elite theology) is the effective authority:
 *   villagers move between shrine and temple as life demands, and the
 *   institutions accommodate this because their revenue depends on it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/japanese_history/philosophy_of_religion").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '25a1a59f-f252-4862-90fc-0c94a9f7b5ae').
narrative_ontology:cs_kernel_codification('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', implicit).
narrative_ontology:cs_authority_grounding('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', practice).
narrative_ontology:cs_interpretation_layer_present('25a1a59f-f252-4862-90fc-0c94a9f7b5ae').
narrative_ontology:cs_reading_relation('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', foundational, ritual_jurisdiction_partition_is_functional).
narrative_ontology:cs_axiom_status(ritual_jurisdiction_partition_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', ritual_jurisdiction_partition_is_functional, conventional).
narrative_ontology:cs_axiom('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', foundational, popular_practice_is_effective_authority).
narrative_ontology:cs_axiom_status(popular_practice_is_effective_authority, holdable).
narrative_ontology:cs_axiom_grounding('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', popular_practice_is_effective_authority, conventional).
narrative_ontology:cs_reference_frame('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', pre_meiji_ritual_jurisdiction_settlement).
narrative_ontology:cs_drift_state('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', late_edo_terauke_system_mature, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25a1a59f-f252-4862-90fc-0c94a9f7b5ae', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, village_elders).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, local_landholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, ordinary_villagers).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, itinerant_mountain_ascetics).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, ordinary_villagers).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, domain_partition_coexistence).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, popular_practice_as_authority).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_coexistence_without_theological_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mediate the boundary between kami rites (agricultural calendar, purification, festivals) and Buddhist rites (funerals, memorial services, ancestor veneration). Their authority derives from maintaining the functional partition that organizes community life. They collect social legitimacy and resource flows (labor, offerings) by managing the interface.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_elders, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, village_elders, beneficiary).

% Operate as integrated religious-economic units (jingū-ji). Buddhist temples manage death rites and parishioner registers; Shinto shrines manage life rites and land-ritual calendars. The partition allows each to claim a protected domain without doctrinal reconciliation. They collect land revenue, corvée exemptions, and pilgrim offerings from their respective jurisdictions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, temple_shrine_complexes, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, temple_shrine_complexes, agenda_setter).

% Depend on the ritual calendar to structure agricultural labor, tax cycles, and dispute resolution. The partition gives them predictable access to both kami and Buddhist ritual services without having to choose or reconcile doctrines. They contribute labor and produce to both institutions as customary obligation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, local_landholders, beneficiary,
    moderate, biographical, constrained, local).

% Navigate the boundary professionally (shugendō practitioners). They must perform competence in both systems to serve communities, but hold no protected institutional position. Their livelihood depends on the partition's permeability — they are coordinated by it but also bear the cost of maintaining fluency in two ritual grammars without institutional backing.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, itinerant_mountain_ascetics, payer,
    powerless, biographical, mobile, regional).

% Participate in both ritual streams as a condition of community membership: birth/purification/harvest at the shrine, death/memorial at the temple. They bear the cost of dual obligation (time, offerings, labor) but receive the coordination benefit of a complete ritual coverage for life-cycle events. Exit means social ostracism or relocation.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, ordinary_villagers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, ordinary_villagers, beneficiary).

% Maintain the formal court calendar (kami-centered) while patronizing Buddhist institutions for state protection rites. They observe the partition from above, using it to balance competing priestly factions. Their interest is stability of the ritual-political order, not doctrinal coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court_ritualists, observer,
    institutional, generational, analytical, national).

% Would later (1868+) violently impose shinbutsu bunri (separation of kami and Buddhas) by destroying the partition this reading describes. They are excluded from the constraint's operation during its active period but their future action is the historical terminus that reveals the partition's contingency.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_restoration_ideologues, excluded,
    institutional, civilizational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides complete ritual coverage for the human life-cycle and agricultural year by assigning kami the domain of life/purity/harvest and Buddhas the domain of death/salvation/afterlife, without requiring theological unification. The partition solves the problem of how two imported/competing ritual systems can coexist in one community by giving each a protected, non-overlapping jurisdiction.
% TRANSFER_FUNCTION: Moves ritual obligation and resource flows (offerings, labor, land revenue) from villagers and landholders to two parallel institutional hierarchies (shrine and temple networks) via the mediation of village elders. The transfer is bidirectional: life-cycle events flow to shrines, death-cycle events flow to temples.
% ABSENT_VOICES: Women's ritual knowledge (miko, lay nun communities) operated within and across the partition but left minimal textual record. Their experience of the boundary — whether it empowered or constrained their practice — is not represented in the priestly/elite sources that document the arrangement. Also absent: the voices of those who converted to Christianity (kirishitan) and experienced the partition as exclusionary closure.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, village ritual life would lose its coordinating structure: no clear assignment of which institution handles which life-cycle event, no protected jurisdiction for either priesthood, immediate competition for the same ritual fees and parishioner loyalty. The Meiji shinbutsu bunri edicts (1868) are the historical proof — their enforcement caused precisely this rearrangement: shrine priests expelled Buddhist icons, temple lands confiscated, parishioner registers restructured, and a decade of violent conflict over ritual jurisdiction.
% FOUNDING_PROBLEM: How to integrate Buddhism (imported 6th century, centered on salvation, monasticism, and universal doctrine) with the indigenous kami cult (centered on land, lineage, purity, and this-worldly benefit) without destroying either system's legitimacy or triggering endless doctrinal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary scholarship (Kuroda Toshio, Allan Grapard, Mark Teeuwen) corroborates that the partition emerged as a pragmatic settlement, not a theological synthesis. The benefiting parties (temple-shrine complexes, court ritualists) produced origin myths (honji suijaku, later inverted as shinpon butsujaku) that retroactively sacralized the arrangement. No independent corroboration exists that the partition 'solved' the problem in any final sense — it managed it.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.22) reflects that the constraint primarily coordinates rather than extracts: both institutions receive customary offerings, but the rate is not monopolistic and alternatives (folk ritual, mountain ascetics) persist at the margins. Low suppression (0.15) because the partition is maintained by mutual institutional interest and customary expectation, not coercion — villagers could (and did) ignore boundaries in private practice. Low theater (0.18) because the coordination function is real and the doctrinal ambiguity is acknowledged, not performed as unity. Accessibility collapse (0.45) is moderate: the partition structures the ritual field but does not eliminate alternative imaginaries (Pure Land exclusivism, nativist Shinto, kirishitan). Resistance (0.25) is low during the constraint's active period — the Meiji rupture comes from outside the system.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different seat types: from the village elder seat, the constraint is a rope (genuine coordination, low extraction). From the itinerant ascetic seat, it leans tangled rope (coordination function exists but asymmetric cost of boundary fluency). From the ordinary villager seat, it may compute as scaffold (transitional — the partition worked until state power imposed unification). The claimed type (rope) represents the author's structural judgment from the analytical seat; divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Village elders and temple-shrine complexes are structural beneficiaries (d ~ 0.2): they collect legitimacy and revenue from managing the boundary. Landholders are symmetric beneficiaries (d ~ 0.5): they gain ritual coverage and pay customary dues. Itinerant ascetics are payers (d ~ 0.7): they must master both systems without institutional protection. Ordinary villagers are payers-beneficiaries (d ~ 0.55): dual obligation is costly but complete coverage is valuable. Court ritualists are analytical observers (d ~ 0.0): they use the partition politically. Meiji ideologues are excluded — their future action terminates the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating two ritual systems) was never fully 'solved' — the partition managed it. By the late Edo period, the constraint had become a piton candidate: the original coordination need (preventing doctrinal war) had attenuated as both systems accommodated each other, but the institutional machinery (jingū-ji complexes, parishioner registers) persisted through inertia and vested interest. The Meiji Restoration did not 'resolve' mandatrophy — it violently terminated the constraint, replacing it with a state-enforced separation (shinbutsu bunri) that was itself a new extraction mechanism (State Shinto).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_functional_primacy,
    'Was the domain partition primarily a theological accommodation (honji suijaku as lived doctrine) or a functional jurisdictional settlement that theological narratives later rationalized?',
    'Comparative analysis of village-level dispute records (mura-sō) vs. elite doctrinal treatises: if disputes were resolved by jurisdictional precedent rather than doctrinal authority, the functional reading is primary.',
    'If functional primacy holds, the constraint is a genuine rope (coordination without extraction). If theological primacy holds, the partition is a tangled rope (coordination serving as cover for institutional extraction via doctrinal monopoly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_functional_primacy, conceptual, 'Whether the constraint''s coordination function is structurally primary or epiphenomenal to doctrinal power.').

omega_variable(
    extraction_accumulation_late_edo,
    'Did the constraint''s extractiveness increase in the late Edo period (1700–1868) as temple-shrine complexes leveraged parishioner registers (terauke/seidan) for state surveillance and tax collection?',
    'Quantitative analysis of terauke certificate fees, shrine offering records, and corvée exemption patterns over time. Correlation with domain-level fiscal pressure.',
    'If extraction accumulated, the constraint drifted from rope toward tangled_rope or piton in its final centuries — the coordination function became a vehicle for state-extraction collaboration. This would validate the Meiji reformers'' claim that the system had become corrupt (even if their remedy was worse).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_accumulation_late_edo, empirical, 'Whether the constraint accumulated extractive layers in its terminal phase.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the structural disagreement between this reading and the syncretic_fusion_reading lie — in the ontological claim (are kami manifestations of Buddhas?), the authority claim (is popular practice or elite doctrine sovereign?), or the historical claim (was the partition stable or contested)?',
    'Formal decomposition of each reading''s axioms into the cs_structure schema: if the axioms are mutually exclusive (one asserts ontological identity, the other asserts jurisdictional separation), the relation is ''forecloses''. If both can be held by different parties simultaneously (different villages, different periods), the relation is ''coexists_with''.',
    'Determines the reading_relations entry for syncretic_fusion_reading. If ''forecloses'', the kernel contains a genuine logical contradiction. If ''coexists_with'', the kernel hosts a stable pluralism that state power (Meiji) forcibly resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Structural location of the disagreement between domain-partition and syncretic-fusion readings of the shinbutsu kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1300, 0.15).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.18).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 700, 0.12).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1300, 0.2).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1700, 0.22).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 700, 0.08).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1100, 0.12).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1300, 0.14).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, state_shinto_establishment).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel decomposes into three readings: this domain_partition_reading (rope — functional coordination), syncretic_fusion_reading (tangled_rope — doctrinal unification serving institutional extraction), and incoherent_bundle_reading (snare — ambiguity maintained by power). The three stories form a constraint family linked by affects_constraints. This reading's ε (0.22) is substantially lower than the syncretic_fusion_reading's expected ε (0.45+) because the latter's doctrinal monopoly enables extraction; the incoherent_bundle_reading's ε is highest (0.6+) as it describes the constraint from the perspective of those excluded by the ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__domain_partition_reading, organized, 0.25).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__domain_partition_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
