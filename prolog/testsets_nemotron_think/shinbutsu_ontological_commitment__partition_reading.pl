% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Domain Partition (Life-cycle vs Afterlife)
 *   domain: religious/historical/japanese
 *
 * SUMMARY:
 *   The partition reading describes the Edo-period (1600-1868) functional
 *   separation where Shinto shrines handled life-cycle rituals (birth,
 *   coming-of-age, marriage, agriculture) and Buddhist temples handled
 *   afterlife rituals (funerals, memorials, ancestral care). This was not a
 *   doctrinal synthesis but a pragmatic division of ritual labor that gave
 *   practitioners autonomy to participate in both without declaring exclusive
 *   allegiance. The arrangement emerged from centuries of shinbutsu-shūgō
 *   (syncretic mixing) but stabilized into a low-integration coordination
 *   mechanism. No single institution or group captured the arrangement —
 *   temples and shrines each held recognized domains, practitioners moved
 *   freely between them, and state authorities used the partition for
 *   population registration without enforcing theological purity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.12).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Domain Partition (Life-cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/historical/japanese").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '94eecd81-58b7-446c-928c-3528ab8d6d68').
narrative_ontology:cs_kernel_codification('94eecd81-58b7-446c-928c-3528ab8d6d68', distributed).
narrative_ontology:cs_authority_grounding('94eecd81-58b7-446c-928c-3528ab8d6d68', practice).
narrative_ontology:cs_interpretation_layer_present('94eecd81-58b7-446c-928c-3528ab8d6d68').
narrative_ontology:cs_reading_relation('94eecd81-58b7-446c-928c-3528ab8d6d68', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('94eecd81-58b7-446c-928c-3528ab8d6d68', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('94eecd81-58b7-446c-928c-3528ab8d6d68', foundational, domain_separation_principle).
narrative_ontology:cs_axiom_status(domain_separation_principle, holdable).
narrative_ontology:cs_axiom_grounding('94eecd81-58b7-446c-928c-3528ab8d6d68', domain_separation_principle, conventional).
narrative_ontology:cs_axiom('94eecd81-58b7-446c-928c-3528ab8d6d68', secondary, practitioner_autonomy_preserved).
narrative_ontology:cs_axiom_status(practitioner_autonomy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('94eecd81-58b7-446c-928c-3528ab8d6d68', practitioner_autonomy_preserved, conventional).
narrative_ontology:cs_reference_frame('94eecd81-58b7-446c-928c-3528ab8d6d68', functional_domain_partition).
narrative_ontology:cs_drift_state('94eecd81-58b7-446c-928c-3528ab8d6d68', meiji_restoration, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('94eecd81-58b7-446c-928c-3528ab8d6d68', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_domain_separation).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, ritual_autonomy_of_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control funerary and afterlife rituals (funerals, memorial services, ancestral rites). Receive donations and land endowments for these services. Maintain priestly lineages and doctrinal training. Their domain is recognized but not exclusive — some folk practices bypass them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, buddhist_temples, beneficiary).

% Control life-cycle rituals (birth ceremonies, coming-of-age, weddings, agricultural festivals). Receive offerings and patronage. Maintain hereditary priesthoods and local festival calendars. Their domain is recognized but not exclusive — Buddhist rites sometimes intrude on life-cycle events.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrines, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, shinto_shrines, beneficiary).

% Navigate both domains freely — visit shrines for life events, temples for funerals. No doctrinal loyalty test; participation is situational and pragmatic. Can choose purely Buddhist or purely Shinto practice if they wish, but most follow the partitioned pattern without coercion.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, mobile, local).

% Tokugawa bakufu and domain lords regulated temple-shrine registration (terauke/danka system) for population control. Used the partition administratively but did not enforce doctrinal purity. Benefited from stable ritual infrastructure for governance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, state_authorities_edo, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyze the historical partition as either functional adaptation, colonial imposition, or indigenous coherence. Their interpretations shape contemporary heritage policy and religious identity debates but do not constrain historical practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, modern_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clear, uncontested ritual domains for life-cycle events (Shinto) and afterlife/funerary matters (Buddhism), eliminating competition over the same ritual occasions and giving practitioners predictable pathways without doctrinal commitment.
% TRANSFER_FUNCTION: Moves ritual patronage and donations from lay practitioners to the respective institutions — shrines for life-cycle rites, temples for funerary rites — with minimal cross-subsidy or extraction between domains.
% ABSENT_VOICES: Kakure kirishitan (hidden Christians) and other suppressed minorities who could not access either domain freely; also early Meiji reformers who wanted a unified State Shinto and saw the partition as backward.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, ritual markets would become contested — temples and shrines would compete for the same ceremonies, practitioners would face doctrinal pressure to choose one tradition exclusively, and the low-friction pragmatic navigation would collapse into explicit affiliation demands.
% FOUNDING_PROBLEM: Heian-to-Kamakura period need to accommodate both indigenous kami veneration and imported Buddhist soteriology without forcing doctrinal synthesis or violent conflict, while providing ritual coverage for all life stages.
% FOUNDING_PROBLEM_CORROBORATION: Medieval temple-shrine complexes (jingū-ji) document the practical arrangement; Edo-period parishioner registers (shūmon aratame) show functional partition in practice; modern folklorists (Yanagita Kunio, Ōrikuchi Shinobu) attest the partition as lived reality. No single beneficiary group controls the narrative — Buddhist and Shinto institutions both claim continuity, scholars debate origins.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the arrangement primarily coordinates ritual domains rather than extracting rents — practitioners pay for specific services in each domain, and the fees track service provision. Suppression is minimal (0.12) because alternatives existed (pure Buddhism, pure Shinto, folk practice) and were not actively suppressed during the Edo period. Theater ratio is very low (0.08) — the coordination function is genuine and the institutions perform their ritual roles substantively. Accessibility collapse is low (0.25) because practitioners could and did choose alternative ritual pathways. Resistance is low (0.18) because the arrangement reduced conflict rather than generating it. The claimed type 'rope' reflects genuine coordination with minimal coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the partition is invisible infrastructure — it just works. From the institutional seats (temples/shrines), it is a stable jurisdictional agreement that prevents ruinous competition. From the state seat, it is an administrative tool. From the modern scholar seat, it is a historical puzzle (was it organic? imposed? strategic?). The engine computes these as different type experiences: rope for practitioners and institutions, possibly scaffold for state (transitional administrative tool), mountain for scholars who reify it as 'Japanese religion.' The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temples and Shinto shrines are both agenda-setters and beneficiaries within their recognized domains — they set ritual standards and receive patronage, but neither dominates the other's domain. Lay practitioners are beneficiaries with mobile exit — they gain ritual clarity without coercion. Edo state authorities are agenda-setters who administratively recognized the partition for governance but did not extract ritual rents. Modern scholars are analytical observers. Directionality derivation: temples/shrines have constrained exit (institutional continuity) but low d because they benefit; practitioners have mobile exit and low d; state has arbitrage exit. The engine will compute low effective extraction for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition solved a genuine coordination problem (ritual domain clarity without doctrinal war) and persisted because it worked, not because any party could enforce it against the others. When Meiji state forcibly separated Shinto and Buddhism (shinbutsu bunri, 1868), it broke a functioning rope and replaced it with a state-enforced scaffold (State Shinto) that extracted heavily. The original partition shows no mandatrophy — its function remained live until external force dismantled it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretism_boundary,
    'Where does the partition reading end and the syncretic reading begin? Did practitioners experience a clean domain separation or a fluid honji-suijaku unity that looked like partition to administrators?',
    'Comparative analysis of temple-shrine complex records (jingū-ji), parishioner registers, and folk practice documentation across regions and periods. Look for doctrinal statements vs. lived practice.',
    'If the boundary is porous, the partition reading may be an administrative artifact (Edo-state projection) rather than a lived coordination structure. This would shift classification toward scaffold (state-imposed) or tangled_rope (if extraction accompanied administration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretism_boundary, empirical, 'Whether domain separation was practitioner-lived or administrator-projected.').

omega_variable(
    state_role_in_partition_stability,
    'Did the Tokugawa state''s terauke/danka registration system create the partition''s stability, or merely recognize and codify an existing folk arrangement?',
    'Pre-Edo temple-shrine records, medieval parishioner lists, and archaeological evidence of ritual practice before state registration systems.',
    'If state-created, the partition is a scaffold with state as agenda-setter extracting administrative control. If state-recognized, it is a genuine rope. The extractiveness measurement (0.15) assumes the latter; state-creation would imply hidden extraction via administrative monopoly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_role_in_partition_stability, conceptual, 'Origin of partition stability: state imposition vs. folk recognition.').

omega_variable(
    kernel_reading_relations,
    'How do the three readings of shinbutsu_ontological_commitment structurally relate? Does partition_reading foreclose syncretic_reading, or do they coexist as period-specific descriptions?',
    'Structural analysis of each reading''s axioms and drift states. If partition_reading''s domain_separation_principle logically contradicts syncretic_reading''s honji-suijaku_unity, forecloses applies. If both describe different historical phases without contradiction, coexists_with applies.',
    'Determines whether the kernel contains genuine ontological contestation (forecloses) or period-specific functional descriptions (coexists_with). Affects how the engine models commitment-system drift across the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between partition, syncretic, and incoherence readings of the shinbutsu kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t1600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(shinbutsu_partition_tr_t1650, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1650, 0.06).
narrative_ontology:measurement(shinbutsu_partition_tr_t1700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1700, 0.07).
narrative_ontology:measurement(shinbutsu_partition_tr_t1750, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1750, 0.07).
narrative_ontology:measurement(shinbutsu_partition_tr_t1800, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(shinbutsu_partition_tr_t1868, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1868, 0.08).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t1600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1600, 0.1).
narrative_ontology:measurement(shinbutsu_partition_be_t1650, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1650, 0.12).
narrative_ontology:measurement(shinbutsu_partition_be_t1700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1700, 0.13).
narrative_ontology:measurement(shinbutsu_partition_be_t1750, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1750, 0.14).
narrative_ontology:measurement(shinbutsu_partition_be_t1800, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(shinbutsu_partition_be_t1868, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1868, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t1600, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1600, 0.08).
narrative_ontology:measurement(shinbutsu_partition_su_t1650, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1650, 0.1).
narrative_ontology:measurement(shinbutsu_partition_su_t1700, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1700, 0.11).
narrative_ontology:measurement(shinbutsu_partition_su_t1750, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1750, 0.11).
narrative_ontology:measurement(shinbutsu_partition_su_t1800, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(shinbutsu_partition_su_t1868, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1868, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% Partition reading is one of three constraint stories decomposing the shinbutsu_ontological_commitment kernel. Syncretic_reading (honji-suijaku unity) dominated medieval period; partition_reading describes Edo-period functional separation; incoherence_reading is modern scholarly view of persistent ambiguity. All three linked via network.affects_constraints. Partition_reading's axioms (domain_separation_principle, practitioner_autonomy_preserved) are distinct from syncretic_reading's honji_suijaku_unity and incoherence_reading's no_stable_commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
