% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment Requires Physical Execution; Suspended Without Temple
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The performance_only reading of the sacrifice commandment holds that the
 *   biblical obligation to offer sacrifices requires physical execution in
 *   the Jerusalem Temple by kohanim according to precise specifications.
 *   Without the Temple, the commandment is inoperative — suspended, not
 *   fulfilled through study, prayer, or symbolic acts. This reading has
 *   dominated mainstream Orthodox halakha for 1,900 years, structuring
 *   yeshiva curricula (Kodashim as core tractates), rabbinic career paths,
 *   and institutional funding toward Temple restoration. The constraint
 *   extracts massive scholarly labor: generations of scholars master laws
 *   that cannot be performed, while living law domains (agunot, medical
 *   ethics, financial regulation) remain under-developed. The suppression is
 *   active: curricula mandate Kodashim; ordination requires mastery;
 *   alternative readings (study_as_performance, archive_maintenance) are
 *   marginalized in mainstream institutions. The theater_ratio reflects
 *   genuine coordination (preserving textual integrity) increasingly mixed
 *   with performative maintenance (detailed vessel production, kohen
 *   training) that serves institutional identity more than divine obligation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment Requires Physical Execution; Suspended Without Temple").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, 'b894d9bb-5fae-469c-9112-ed157f014a2e').
narrative_ontology:cs_kernel_codification('b894d9bb-5fae-469c-9112-ed157f014a2e', fixed_text).
narrative_ontology:cs_authority_grounding('b894d9bb-5fae-469c-9112-ed157f014a2e', lineage).
narrative_ontology:cs_interpretation_layer_present('b894d9bb-5fae-469c-9112-ed157f014a2e').
narrative_ontology:cs_reading_relation('b894d9bb-5fae-469c-9112-ed157f014a2e', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('b894d9bb-5fae-469c-9112-ed157f014a2e', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('b894d9bb-5fae-469c-9112-ed157f014a2e', foundational, physical_execution_required_for_sacrifice).
narrative_ontology:cs_axiom_status(physical_execution_required_for_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('b894d9bb-5fae-469c-9112-ed157f014a2e', physical_execution_required_for_sacrifice, deontological).
narrative_ontology:cs_axiom('b894d9bb-5fae-469c-9112-ed157f014a2e', foundational, study_cannot_substitute_for_embodied_performance).
narrative_ontology:cs_axiom_status(study_cannot_substitute_for_embodied_performance, holdable).
narrative_ontology:cs_axiom_grounding('b894d9bb-5fae-469c-9112-ed157f014a2e', study_cannot_substitute_for_embodied_performance, deontological).
narrative_ontology:cs_axiom('b894d9bb-5fae-469c-9112-ed157f014a2e', secondary, commandment_suspended_not_fulfilled_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_not_fulfilled_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('b894d9bb-5fae-469c-9112-ed157f014a2e', commandment_suspended_not_fulfilled_without_temple, deontological).
narrative_ontology:cs_reference_frame('b894d9bb-5fae-469c-9112-ed157f014a2e', tannaitic_sacrificial_obligation).
narrative_ontology:cs_drift_state('b894d9bb-5fae-469c-9112-ed157f014a2e', contemporary_restoration_activism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b894d9bb-5fae-469c-9112-ed157f014a2e', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, priestly_scholarly_establishment).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_restoration_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, living_law_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, divine_command_requires_embodied_performance).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, halakhic_integrity_preserved_through_suspension_not_substitution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the authoritative interpretation of sacrifice law through yeshiva curricula, rabbinic ordination, and Temple Institute activities. Benefits from the constraint by maintaining priestly prestige, institutional funding streams tied to Temple restoration, and scholarly authority over a domain that cannot be empirically falsified. Their exit is arbitrage-grade: they can pivot to other halakhic domains or institutional roles without losing status.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, priestly_scholarly_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, priestly_scholarly_establishment, beneficiary).

% Devote substantial career capital to mastering sacrifice law (Kodashim tractates, Temple architecture, ritual purity details) knowing physical performance is impossible. Their professional identity is fused with this mastery — leaving the field means abandoning the expertise that constitutes their scholarly self-concept. The constraint extracts their finite attention from living law (monetary, family, civil law) where halakhic decisions affect daily life.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_scholars, payer,
    organized, biographical, identity_locked, global).

% Communities and poskim (decisors) who need halakhic guidance on contemporary issues (medical ethics, technology, economics, agunot) but find scholarly resources disproportionately allocated to theoretical Temple service. Their exit is constrained: they depend on the same scholarly infrastructure but cannot redirect its priorities. They bear the opportunity cost of a scholarly ecosystem oriented toward suspended commandments.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, living_law_practitioners, payer,
    moderate, biographical, constrained, regional).

% Organizations (Temple Institute, Temple Mount movements) that fundraise, produce vessels, train kohanim, and advocate politically for Temple restoration. They benefit directly from the performance_only reading because it validates their mission: if study fulfilled the commandment, restoration would be religiously optional. Their exit is mobile — they could pivot to other nationalist or religious causes.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_institutions, beneficiary,
    organized, generational, mobile, national).

% Groups who believe the commandment will only be relevant in a messianic age initiated divinely, not humanly. They are excluded from the halakhic conversation because their position renders both study and restoration activity religiously premature. They have no institutional voice in curriculum-setting or funding decisions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_quietists, excluded,
    powerless, civilizational, trapped, global).

% Scholars of religion, law, and anthropology who analyze the constraint from outside the commitment system. They see the full structure: how a suspended commandment organizes scholarly labor, institutional funding, and identity formation. They neither pay nor collect within the system.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, academic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the ontological integrity of the sacrifice commandment as a divine obligation requiring physical embodiment — preventing the commandment from being spiritualized into irrelevance or replaced by humanly accessible substitutes.
% TRANSFER_FUNCTION: Moves scholarly labor (attention, career capital, curriculum space, publishing resources) from living halakhic domains (civil, family, medical, commercial law) to the theoretical study of unperformable Temple rites, and moves institutional funding toward Temple-restoration organizations.
% ABSENT_VOICES: Messianic quietists who reject humanly-driven restoration; reformist halakhists who would redirect Kodashim study toward living law analogies; lay communities bearing the cost of under-developed contemporary halakha. They are structurally excluded by the curriculum gatekeeping of the priestly-scholarly establishment.
% DISAPPEARANCE_RATIONALE: If the performance_only reading vanished overnight, yeshiva curricula would shift Kodashim study from central to elective, scholarly careers would reorient toward living law, Temple restoration organizations would lose their halakhic mandate, and halakhic discourse would reorganize around commandments with present operational relevance.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), how to maintain the sacrifice commandment's status as binding divine law without a Temple — without either spiritualizing it into metaphor or abandoning it as obsolete.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmudic sages themselves (Yoma 5b, Menachot 110a) who explicitly debated whether study substitutes for performance. Josephus (Antiquities) and early Christian sources (Hebrews 10) independently corroborate that the suspension of sacrifice created a theological crisis. Modern historians (Schäfer, Boyarin) confirm the rabbinic response was contested from the start — the study-as-performance reading emerged precisely as a rival solution to the same founding problem.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint directs ~1,900 years of elite scholarly attention toward unperformable acts — a massive opportunity cost measured in foregone living law development. Suppression is high (0.78) because the halakhic system actively enforces the reading through curriculum mandates, ordination requirements, and institutional gatekeeping; alternative readings are not merely discouraged but structurally excluded from authority. Theater ratio (0.45) captures the dual character: the textual preservation function is real (coordination), but the expanding Temple-restoration infrastructure (vessels, training, political advocacy) increasingly performs a restoration narrative that the performance_only reading officially treats as premature. Accessibility collapse (0.68) is substantial but not total: living law alternatives exist and are practiced, but the gravitational pull of the suspended commandment distorts the field. Resistance (0.35) is moderate: internal critique exists (R. Kook's limited endorsement of study_as_performance, academic scholarship, some Modern Orthodox voices) but has not shifted institutional consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (priestly_scholarly_establishment), the constraint is a rope: it coordinates textual fidelity across millennia, preserves the commandment's integrity, and provides a coherent framework for messianic hope. From the payer seats (halakhic_scholars, living_law_practitioners), it is a snare: the coordination story is cover for extracting scholarly labor that could serve present human needs, and the suppression of alternatives is real. The engine computes this divergence from the structural data — the claimed_type (snare) reflects the payer-seat reality, while the beneficiaries' lived experience of genuine coordination is not erased but structurally subordinated.
 *
 * DIRECTIONALITY LOGIC:
 *   The priestly_scholarly_establishment and temple_restoration_institutions are structural beneficiaries (d near 0.0): they collect prestige, funding, and institutional rationale from the constraint. Halakhic_scholars are identity_locked payers (d near 1.0): their professional self-concept is fused with mastery of the suspended domain, making exit psychologically prohibitive despite the extraction. Living_law_practitioners are constrained payers (d ~ 0.7): they bear opportunity costs but have some mobility within the halakhic system. Messianic_quietists are trapped excluded (d ~ 0.9): they cannot participate in the conversation that defines their religious reality. Academic_observers are analytical (d = 0.5): they experience neither extraction nor subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining commandment integrity post-destruction) was live for centuries. By the early modern period, the problem was substantially solved: the textual tradition was secured, the laws were codified (Rambam's Kodashim), and the commandment's suspended status was stable. The constraint persists because the priestly_scholarly_establishment extracts benefit from its maintenance (institutional identity, restoration funding, curricular centrality) and because halakhic_scholars are identity_locked into the mastery it demands. The mandate has atrophied into a self-justifying structure — the study of suspension has become the institution's purpose. This is not a piton (which would imply theatrical maintenance of a dead function); the coordination function (textual integrity) remains real, but the extraction layered atop it is the dominant structural feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_mechanism_identity_vs_structure,
    'Is the scholarly labor extraction primarily structural (curriculum mandates, funding flows) or internalized (scholarly identity fused with Kodashim mastery)?',
    'Compare exit trajectories: scholars who leave yeshiva for academic Jewish studies or living-law pulpit roles — do they report liberation or identity fracture? Longitudinal career data from rabbinic placement offices.',
    'If internalized dominates, the constraint''s effective suppression is higher than structural measures suggest — scholars carry the suppression with them. If structural dominates, policy interventions (curriculum reform, funding redirects) could reduce extraction without identity collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_identity_vs_structure, empirical, 'Structural vs. internalized suppression mechanism for identity_locked scholars.').

omega_variable(
    living_law_opportunity_cost_magnitude,
    'What is the actual magnitude of living law under-development attributable to Kodashim centrality, versus other factors (complexity of modern issues, lack of precedent, political controversy)?',
    'Comparative analysis: map halakhic responsa volume by topic over time (1800-2025) against yeshiva curriculum hours. Control for topic difficulty and social urgency. Counterfactual: topics with similar difficulty but no Kodashim competition (e.g., Shabbat technology).',
    'If Kodashim centrality explains >30% of living law lag, the extraction claim is empirically grounded. If <10%, the extraction narrative overstates the constraint''s causal role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_law_opportunity_cost_magnitude, empirical, 'Quantifying the opportunity cost of suspended-commandment study on living law development.').

omega_variable(
    performance_only_vs_archive_maintenance_boundary,
    'Does the performance_only reading genuinely foreclose archive_maintenance, or do they functionally converge in institutional practice (both direct resources to Temple-restoration preparation)?',
    'Institutional ethnography: trace funding flows, curriculum time, and public messaging of major yeshivas and Temple organizations. Do performance_only institutions (mainstream yeshivas) materially support archive_maintenance activities (Temple Institute, vessel production)?',
    'If they converge in practice, the structural delta between readings is smaller than the ideological delta — the extraction benefits the same institutional complex regardless of reading. The engine would detect a single constraint family with multiple framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_vs_archive_maintenance_boundary, conceptual, 'Whether ideological distinction between performance_only and archive_maintenance maps to materially distinct resource flows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__performance_only, theater_ratio, 70, 0.15).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.22).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_commandment__performance_only, theater_ratio, 1800, 0.42).
narrative_ontology:measurement(sacr_tr_t1948, sacrifice_commandment__performance_only, theater_ratio, 1948, 0.44).
narrative_ontology:measurement(sacr_tr_t1967, sacrifice_commandment__performance_only, theater_ratio, 1967, 0.45).
narrative_ontology:measurement(sacr_tr_t2025, sacrifice_commandment__performance_only, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__performance_only, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.45).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_commandment__performance_only, base_extractiveness, 1800, 0.75).
narrative_ontology:measurement(sacr_be_t1948, sacrifice_commandment__performance_only, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(sacr_be_t1967, sacrifice_commandment__performance_only, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(sacr_be_t2025, sacrifice_commandment__performance_only, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_commandment__performance_only, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_commandment__performance_only, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(sacr_su_t1948, sacrifice_commandment__performance_only, suppression_requirement, 1948, 0.77).
narrative_ontology:measurement(sacr_su_t1967, sacrifice_commandment__performance_only, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(sacr_su_t2025, sacrifice_commandment__performance_only, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, halakhic_curriculum_allocation).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, temple_restoration_funding).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, living_law_development_pace).

% DUAL FORMULATION NOTE:
% This constraint is the performance_only reading of the sacrifice_commandment kernel. It forecloses the study_as_performance reading (logically contradictory on whether study fulfills the obligation) and coexists tensely with archive_maintenance (both deny present fulfillment but archive_maintenance assigns study a restorative teleology). The kernel decomposes into three constraint stories linked by network.affects_constraints: performance_only (this), study_as_performance, archive_maintenance. The performance_only reading has the highest extractiveness because it denies any present-value to sacrifice study while mandating its centrality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, organized, 0.15).
constraint_indexing:directionality_override(sacrifice_commandment__performance_only, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
