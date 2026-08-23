% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration as Pragmatic Incoherence (Pre-Meiji)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint story models the pre-Meiji Japanese practice of
 *   simultaneous veneration (shinbutsu-shūgō) from the pragmatic incoherence
 *   reading: the system was never doctrinally or experientially coherent;
 *   practitioners held contradictory beliefs (Buddhist soteriology vs. Shinto
 *   this-worldly efficacy) simultaneously without resolution. The appearance
 *   of harmony was sustained by lack of enforcement pressure — no authority
 *   demanded doctrinal consistency — until the Meiji state imposed
 *   shinbutsu-bunri (separation of kami and buddhas), revealing the latent
 *   incoherence through violent institutional rupture. The constraint is a
 *   snare: the coordination story (integrated ritual coverage) was cover for
 *   extraction of labor, resources, and compliance from communities by
 *   institutions and the state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.72).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.68).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration as Pragmatic Incoherence (Pre-Meiji)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '64e87210-d3b3-4655-8aea-24bdbfe32289').
narrative_ontology:cs_kernel_codification('64e87210-d3b3-4655-8aea-24bdbfe32289', distributed).
narrative_ontology:cs_authority_grounding('64e87210-d3b3-4655-8aea-24bdbfe32289', practice).
narrative_ontology:cs_interpretation_layer_present('64e87210-d3b3-4655-8aea-24bdbfe32289').
narrative_ontology:cs_reading_relation('64e87210-d3b3-4655-8aea-24bdbfe32289', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('64e87210-d3b3-4655-8aea-24bdbfe32289', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('64e87210-d3b3-4655-8aea-24bdbfe32289', foundational, simultaneous_veneration_was_never_coherent).
narrative_ontology:cs_axiom_status(simultaneous_veneration_was_never_coherent, holdable).
narrative_ontology:cs_axiom_grounding('64e87210-d3b3-4655-8aea-24bdbfe32289', simultaneous_veneration_was_never_coherent, empirically_contingent).
narrative_ontology:cs_axiom('64e87210-d3b3-4655-8aea-24bdbfe32289', foundational, meiji_separation_revealed_latent_incoherence).
narrative_ontology:cs_axiom_status(meiji_separation_revealed_latent_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('64e87210-d3b3-4655-8aea-24bdbfe32289', meiji_separation_revealed_latent_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('64e87210-d3b3-4655-8aea-24bdbfe32289', contradictory_practice_without_resolution).
narrative_ontology:cs_drift_state('64e87210-d3b3-4655-8aea-24bdbfe32289', meiji_restoration_1868, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('64e87210-d3b3-4655-8aea-24bdbfe32289', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, pre_meiji_state_authorities).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, local_communities).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, religious_harmony_as_social_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controlled temple networks and doctrinal interpretation; benefited from landholdings, patronage, and state protection under the syncretic system. Maintained honji-suijaku theory as intellectual cover while practice remained contradictory. Exit would mean losing institutional privilege and land base.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Managed shrine networks and local ritual life; benefited from Buddhist institutional support and state recognition within the syncretic framework. Depended on temple-shrine complexes (jingū-ji) for resources and legitimacy. Exit would mean losing material support and ritual infrastructure.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Used the syncretic religious system for population registration (terauke), social control, and legitimacy. The contradiction-free appearance of simultaneous veneration made it a stable governance tool. Could shift policy when convenient (as Meiji did).
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, pre_meiji_state_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Required to participate in both Buddhist and Shinto rituals for funerals, ancestor rites, and agricultural cycles. Bore the cognitive and material cost of maintaining contradictory practices (e.g., Buddhist funerals + Shinto purification) without doctrinal resolution. Identity fused to community ritual participation; exit meant social ostracism.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Maintained temple-shrine complexes (jingū-ji) and funded both Buddhist and Shinto clergy through village resources. The syncretic system extracted labor and resources for dual ritual calendars. Community cohesion depended on participation; exit was collective-action impossible.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, local_communities, payer,
    organized, generational, constrained, local).

% Excluded from the syncretic system's power structure; viewed simultaneous veneration as superstition obstructing modernization. Their voice was suppressed until the Meiji Restoration enabled shinbutsu-bunri as state policy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_reformers, excluded,
    powerful, biographical, mobile, national).

% Analyze the historical record from outside the constraint. Competing readings (ontological fusion, domain partition, pragmatic incoherence) reflect different methodological commitments. No material stake in the historical arrangement.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, modern_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single integrated ritual calendar covering life-cycle events (birth, marriage, death, agriculture) that no single tradition fully addressed, allowing communities to avoid choosing between traditions.
% TRANSFER_FUNCTION: Moved labor, resources, and cognitive compliance from common practitioners and local communities to Buddhist and Shinto institutions and the state, in exchange for ritual coverage and social legitimacy.
% ABSENT_VOICES: Meiji reformers and nativist scholars (kokugakusha) who argued for Shinto purity were structurally excluded from institutional power until 1868. Their critique that the system was incoherent was suppressed by the very institutions benefiting from it.
% DISAPPEARANCE_RATIONALE: When the constraint disappeared (Meiji shinbutsu-bunri), the institutional landscape violently reorganized: temple-shrine complexes were dismantled, Buddhist property confiscated, Shinto established as state cult, and communities forced to choose single affiliations. The rearrangement cost was enormous (haibutsu kishaku).
% FOUNDING_PROBLEM: Pre-modern Japanese communities needed ritual coverage for both this-worldly concerns (agriculture, disease, clan prosperity) and other-worldly concerns (afterlife, ancestor salvation), but no single tradition claimed competence over both domains.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by Meiji reformers' own documents (Daijō-kan edicts) and by the fact that modern Japanese religion operates with separated Shinto/Buddhist institutions. Buddhist scholars (e.g., Tamamuro Fumio) acknowledge the syncretic system solved a historical problem that no longer exists in the same form. No contemporary party claims the pre-Meiji problem persists unchanged.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because institutions and state extracted substantial material and cognitive compliance while providing ritual services that could have been delivered without doctrinal contradiction. Suppression (0.68) is substantial because alternatives (pure Buddhist or pure Shinto practice) were structurally unavailable — the terauke system mandated temple affiliation, and village ritual life required both. Theater ratio (0.55) is moderate-high: honji-suijaku theory and temple-shrine complexes performed coherence while practice remained contradictory. Accessibility collapse (0.45) is moderate because the contradiction was live but unenforced — practitioners could (and did) privately negotiate it. Resistance (0.35) is low because the constraint's beneficiaries controlled the interpretive apparatus and state registration.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seats (Buddhist/Shinto), the constraint appears as genuine coordination (rope-like) — they provided real ritual services and maintained social order. From the payer seats (practitioners, communities), it operates as extraction (snare) — they bear the cost of contradictory demands with no doctrinal resolution. The Meiji reformers' seat (excluded) reads it as incoherent superstition. The engine computes this divergence from the structural data: same constraint, three different experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist and Shinto institutions are agenda_setters (institutional power, constrained exit — they administer the system but depend on it). The pre-Meiji state is a beneficiary (institutional power, arbitrage exit — it used the system but could abandon it). Common practitioners are payers (powerless, identity_locked — ritual participation is identity-constitutive; exit means social death). Local communities are payers (organized, constrained — collective action problem prevents exit). Meiji reformers are excluded (powerful, mobile — they had voice but no institutional access until 1868). Modern scholars are observers (analytical, analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dual-domain ritual coverage) was real but dead by 1868 — modern Japan has separated institutions and no longer requires a single integrated ritual calendar. The constraint persisted ~1200 years past its founding problem's relevance because institutions captured the extraction stream. The mandatrophy is resolved in the sense that the arrangement's original justification is gone, but the resolution came through violent state imposition (shinbutsu-bunri), not organic sunset. This is a snare whose extraction was revealed by rupture, not a scaffold that transitioned gracefully.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_fusion_vs_pragmatic_incoherence,
    'Did pre-Meiji practitioners genuinely believe in ontological identity (honji-suijaku) or did they hold contradictory beliefs without resolution?',
    'Comparative analysis of vernacular texts, miracle tales (setsuwa), and lay practice records vs. elite doctrinal treatises. If lay sources show unresolved contradiction while elite sources assert identity, the pragmatic incoherence reading is supported.',
    'If ontological fusion was genuinely believed, the constraint was a genuine coordination mechanism (rope/tangled_rope). If contradiction persisted without resolution, the constraint was a snare extracting compliance through suppressed incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_fusion_vs_pragmatic_incoherence, empirical, 'Whether the syncretic system''s coherence was real (believed) or performed (enforced).').

omega_variable(
    domain_partition_resolution,
    'Did functional domain partition (this-worldly vs. other-worldly) actually resolve the contradiction for practitioners, or was it an elite rationalization?',
    'Analysis of petition records, votive offerings, and ritual calendars: if practitioners consistently invoked kami for prosperity and buddhas for salvation without mixing, domain partition has descriptive force. If they invoked both for the same ends, partition fails.',
    'If domain partition resolved the contradiction, the constraint was a rope (genuine coordination). If it was a post-hoc rationalization, the constraint remains a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_partition_resolution, empirical, 'Whether functional specialization genuinely resolved practitioners'' cognitive load or was an institutional cover story.').

omega_variable(
    meiji_rupture_vs_revelation,
    'Was Meiji shinbutsu-bunri an imposed rupture that created incoherence, or a revelation of pre-existing latent incoherence?',
    'Trace pre-Meiji nativist critique (kokugaku), temple-shrine conflict records, and popular resistance to separation. If incoherence was already articulated and resisted, revelation. If separation generated the conflict, rupture.',
    'If revelation, the snare''s extraction was structural and pre-Meiji. If rupture, the extraction was largely Meiji-era; the pre-Meiji system was more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_vs_revelation, conceptual, 'Whether the Meiji separation exposed or created the constraint''s extractive nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(simu_tr_t300, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 300, 0.4).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 600, 0.45).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 900, 0.5).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1200, 0.52).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1500, 0.54).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1868, 0.55).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(simu_be_t300, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 900, 0.62).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1868, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(simu_su_t300, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 300, 0.48).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 900, 0.6).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1200, 0.63).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1500, 0.66).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1868, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__pragmatic_incoherence_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, state_shinto_formation).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, modern_japanese_religious_freedom).

% DUAL FORMULATION NOTE:
% This reading (pragmatic_incoherence_reading) decomposes the simultaneous_veneration kernel alongside ontological_fusion_reading and domain_partition_reading. The epsilon values differ sharply: ontological_fusion_reading authors low ε (genuine coordination), domain_partition_reading authors moderate ε (functional coordination with some extraction), pragmatic_incoherence_reading authors high ε (suppressed contradiction as extraction). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, institutional, 0.15).
constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
