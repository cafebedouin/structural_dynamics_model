% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Functional Coexistence Reading)
 *   domain: religious/historical/commitment_system
 *
 * SUMMARY:
 *   The domain_partition_reading interprets pre-Meiji Japanese shinbutsu
 *   shūgō (kami-buddha syncretism) as a functional division of labor: kami
 *   govern this-worldly affairs (agriculture, health, clan continuity) while
 *   buddhas govern afterlife matters (funerals, ancestors, salvation).
 *   Coexistence is pragmatic, not ontological — honji suijaku (original
 *   ground / manifest trace) is an institutional arrangement, not
 *   metaphysical truth. The Tokugawa terauke system institutionalized this
 *   partition, using Buddhist temples as state registration agents while
 *   Shinto shrines received patronage for this-worldly rites. The reading
 *   claims low institutional entanglement and easy separation (realized at
 *   Meiji), positioning the constraint as coordination (rope) rather than
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.35).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.45).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Coexistence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious/historical/commitment_system").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'e77ef32f-7630-46c7-ba0b-86d3106ce160').
narrative_ontology:cs_kernel_codification('e77ef32f-7630-46c7-ba0b-86d3106ce160', distributed).
narrative_ontology:cs_authority_grounding('e77ef32f-7630-46c7-ba0b-86d3106ce160', practice).
narrative_ontology:cs_interpretation_layer_present('e77ef32f-7630-46c7-ba0b-86d3106ce160').
narrative_ontology:cs_reading_relation('e77ef32f-7630-46c7-ba0b-86d3106ce160', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('e77ef32f-7630-46c7-ba0b-86d3106ce160', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('e77ef32f-7630-46c7-ba0b-86d3106ce160', foundational, kami_buddha_domain_separation).
narrative_ontology:cs_axiom_status(kami_buddha_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('e77ef32f-7630-46c7-ba0b-86d3106ce160', kami_buddha_domain_separation, empirically_contingent).
narrative_ontology:cs_axiom('e77ef32f-7630-46c7-ba0b-86d3106ce160', secondary, honji_suijaku_as_pragmatic_arrangement).
narrative_ontology:cs_axiom_status(honji_suijaku_as_pragmatic_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('e77ef32f-7630-46c7-ba0b-86d3106ce160', honji_suijaku_as_pragmatic_arrangement, conventional).
narrative_ontology:cs_reference_frame('e77ef32f-7630-46c7-ba0b-86d3106ce160', medieval_functional_coexistence).
narrative_ontology:cs_drift_state('e77ef32f-7630-46c7-ba0b-86d3106ce160', tokugawa_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e77ef32f-7630-46c7-ba0b-86d3106ce160', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_state).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, commoner_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, minority_religious_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temples).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_domain_partition_thesis).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, honji_suijaku_as_institutional_pragmatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established and enforced the temple certification system (terauke) requiring every household to register with a Buddhist temple. Used religious registration as population control and anti-Christian measure. Collected no direct revenue but gained administrative control.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained guaranteed parishioners (danka) and exclusive rights to perform funerals and memorial services, generating stable revenue. In return, temples acted as state agents for population registration and ideological monitoring. Could not easily refuse state directives without losing privileges.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temples, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temples, payer).

% Received state patronage and official recognition within the domain partition framework (kami govern this-world). Shrine priests performed state rites and maintained local registries. Depended on domain/state funding; autonomous doctrinal authority limited by state oversight.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrines, beneficiary,
    organized, biographical, constrained, national).

% Required to register with a Buddhist temple for life events (birth, marriage, death) and pay mandatory fees for funerals and memorial services. No legal exit; non-compliance meant suspicion of Christianity and severe punishment. Bore the financial and ideological costs of the system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, commoner_households, payer,
    powerless, biographical, trapped, local).

% Groups like hidden Christians (kakure kirishitan), Fujufuse Nichiren sect, and mountain ascetics (shugendo) operated outside or against the domain partition. Their practices were criminalized; they survived through secrecy or faced persecution. The partition framework structurally excluded their legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, minority_religious_groups, excluded,
    powerless, biographical, trapped, local).

% Analyze the shinbutsu relationship through historical, anthropological, and theological lenses. The domain_partition_reading is one scholarly position among others. No material stake in the historical arrangement; professional reputation may align with specific interpretations.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable framework for dual religious affiliation: kami addressed this-worldly needs (agriculture, health, clan prosperity) while buddhas addressed afterlife salvation (funerals, ancestors, rebirth). Households could participate in both without doctrinal conflict, and institutions divided ritual labor.
% TRANSFER_FUNCTION: Moved labor, resources, and ideological compliance from commoner households to Buddhist temples (funeral fees, temple support) and the Tokugawa state (population registration, anti-Christian surveillance). Shinto shrines received state patronage in exchange for ritual service.
% ABSENT_VOICES: Commoner households had no formal voice in the arrangement; their compliance was coerced. Minority religious groups (hidden Christians, Fujufuse sect) were structurally excluded and criminalized. Women's specific ritual burdens (e.g., managing household Buddhist altars) are rarely documented in institutional records.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight in the Tokugawa period, the terauke system would collapse, Buddhist temples would lose guaranteed parishioners and revenue, state population control would lose its primary mechanism, and commoners would face religious vacuum — the social order would reorganize around new religious-administrative structures (as occurred at Meiji).
% FOUNDING_PROBLEM: Medieval Japan faced fragmented religious authority: kami cults were local and clan-based, Buddhism was imported and monastic. Neither provided comprehensive coverage of life-cycle rituals and cosmic order. The domain partition solved this by assigning complementary jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (comprehensive ritual coverage in a dual-tradition society) is attested as resolved by the Meiji separation itself — the state created new institutions (State Shinto, sectarian Buddhism) that replaced the partition. Non-beneficiary corroboration: Tokugawa administrative records show the system functioning as population control, not ritual provision; hidden Christian communities demonstrate the system's failure to provide genuine spiritual coverage for all.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35 avg) because the arrangement did transfer resources from commoners to temples/state, but the reading assesses this as the cost of coordination, not rent. Suppression rises from 0.2 to 0.55 as the terauke system hardens (1600s), then falls at Meiji — the reading sees enforcement as incidental to coordination. Theater remains low (0.15-0.28): the ritual division was genuinely practiced. Accessibility collapse at 0.5 reflects that alternatives (Christianity, Fujufuse) existed but were suppressed. Resistance at 0.4 captures hidden Christians and sectarian defiance. The claimed type (rope) diverges from what metrics might compute for payer seats — the engine will measure that gap.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state) and beneficiary (temples/shrines) seats compute as rope/scaffold — the system coordinates and persists by design. The payer (commoners) and excluded (minorities) seats compute as snare/tangled_rope — the same structure extracts and suppresses. The domain_partition_reading narrates from the beneficiary/agenda-setter perspective; the engine's per-seat computation will reveal the divergence this reading minimizes.
 *
 * DIRECTIONALITY LOGIC:
 *   Tokugawa state is structural beneficiary (d near 0): gains administrative control without direct cost. Buddhist temples are dual: beneficiaries of guaranteed revenue (d ~ 0.2) but payers of state compliance (d ~ 0.6). Shinto shrines are beneficiaries of patronage (d ~ 0.15). Commoners are full targets (d ~ 0.9): trapped, no exit, bear fees and ideological compliance. Minority groups are excluded targets (d ~ 1.0): actively suppressed. The reading's claim of 'functional coexistence' reflects the beneficiary seats' experience; the payer seats experience extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (comprehensive ritual coverage) was live in the medieval period but dead by Tokugawa — the system persisted as population control. The reading's claim that separation was 'easy' is belied by Meiji's violence (haibutsu kishaku). Mandatrophy is resolved: the constraint's mandate (ritual coordination) atrophied, replaced by state extraction. The reading itself is a retrospective coordination story that obscures the extraction phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_belief_vs_institutional_cover,
    'Did medieval/early modern Japanese genuinely believe in the domain partition, or was it an institutional cover for Buddhist temple dominance?',
    'Comparative analysis of vernacular religious texts, temple records, and folk practices vs. elite doctrinal treatises. If daily practice matches the partition, belief is genuine; if practice ignores it, cover.',
    'If genuine belief, the constraint is rope (coordination via shared cosmology). If cover, it is tangled_rope (coordination story masking temple extraction). Changes ε assessment for the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_belief_vs_institutional_cover, empirical, 'Whether the domain partition reflects lived cosmology or institutional fiction.').

omega_variable(
    separation_ease_vs_meiji_violence,
    'Was the Meiji separation (shinbutsu bunri) truly ''easy'' as the reading claims, or did it require state violence because entanglement was deep?',
    'Quantify temple/shrine property transfers, clergy defrocking, iconoclasm incidents, and resistance duration during 1868-1875. Compare to other institutional separations.',
    'If separation was violent/protracted, the reading''s ''low entanglement'' claim fails; the constraint was more extractive/inertial (piton/tangled_rope). If peaceful, rope/scaffold holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separation_ease_vs_meiji_violence, empirical, 'Whether low institutional entanglement is historically accurate or a retrospective simplification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was suppression of minority groups (Christians, Fujufuse) structural (state policing) or internalized (commoners self-policing via temple registration)?',
    'Analyze terauke compliance records: voluntary vs. coerced registration rates, informant networks, and post-Meiji religious switching speed.',
    'If internalized, suppression is higher than structural measures suggest — the constraint''s effective suppression persists after state enforcement ends. Affects classification for payer/excluded seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the terauke system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 1200, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1350, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1350, 0.18).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1600, 0.22).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1700, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(shinbutsu_domain_partition_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.25).

% Extraction over time
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.2).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1350, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1350, 0.25).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.3).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1700, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1700, 0.38).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(shinbutsu_domain_partition_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1350, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1350, 0.25).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.3).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1600, 0.45).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1700, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(shinbutsu_domain_partition_su_t1868, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1868, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, meiji_state_shinto_formation).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_sectarian_institutionalization).

% DUAL FORMULATION NOTE:
% The shinbutsu_ontological_substrate kernel decomposes into three readings with distinct ε and beneficiary structures. This reading (domain_partition) claims rope with low ε; syncretic_fusion claims mountain/rope with ontological unity; incoherent_bundle claims snare/piton with state extraction. All three share the historical referent but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__domain_partition_reading, organized, 0.2).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__domain_partition_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
