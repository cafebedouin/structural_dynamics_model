% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment — Archive Maintenance Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The archive_maintenance reading of the sacrifice commandment holds that
 *   studying sacrificial laws preserves technical knowledge for a future
 *   Temple restoration, rather than fulfilling the commandment in the
 *   present. The constraint is the obligation to maintain this body of
 *   knowledge across the exile period. Present practitioners invest study
 *   effort whose utility is realized only by a future generation that may or
 *   may not exist and whose circumstances are unknown. The coordination
 *   function is genuine: a complex technical tradition (Temple service
 *   protocols, purity laws, sacrificial taxonomy) would be lost without
 *   continuous transmission, and the future restoration scenario requires
 *   that knowledge. The extraction function is asymmetric: present
 *   practitioners bear the full cost of maintaining a demanding study regime
 *   whose present devotional value is contested (this reading explicitly
 *   denies present worship value), while the beneficiary is a non-extant
 *   future generation. Halakhic institutions act as agenda_setters
 *   maintaining the curriculum and authorizing the study obligation.
 *
 * KEY AGENTS:
 *   - future_restoration_generation: Primary beneficiary (generational/universal) — receives preserved technical knowledge for Temple restoration
 *   - present_study_practitioners: Primary payer (organized/biographical) — bears study burden with uncertain present value
 *   - halakhic_institutions: Agenda setter (institutional/generational) — sets curriculum, authorizes obligation, maintains transmission chain
 *   - analytical_observer: Observer (analytical/civilizational/universal) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.31).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment — Archive Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'ce708b47-7e09-441c-a449-fab1793f99d2').
narrative_ontology:cs_kernel_codification('ce708b47-7e09-441c-a449-fab1793f99d2', fixed_text).
narrative_ontology:cs_authority_grounding('ce708b47-7e09-441c-a449-fab1793f99d2', lineage).
narrative_ontology:cs_interpretation_layer_present('ce708b47-7e09-441c-a449-fab1793f99d2').
narrative_ontology:cs_reading_relation('ce708b47-7e09-441c-a449-fab1793f99d2', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('ce708b47-7e09-441c-a449-fab1793f99d2', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('ce708b47-7e09-441c-a449-fab1793f99d2', foundational, study_preserves_for_future_restoration).
narrative_ontology:cs_axiom_status(study_preserves_for_future_restoration, holdable).
narrative_ontology:cs_axiom_grounding('ce708b47-7e09-441c-a449-fab1793f99d2', study_preserves_for_future_restoration, deontological).
narrative_ontology:cs_axiom('ce708b47-7e09-441c-a449-fab1793f99d2', foundational, no_present_devotional_value_in_sacrificial_study).
narrative_ontology:cs_axiom_status(no_present_devotional_value_in_sacrificial_study, holdable).
narrative_ontology:cs_axiom_grounding('ce708b47-7e09-441c-a449-fab1793f99d2', no_present_devotional_value_in_sacrificial_study, deontological).
narrative_ontology:cs_reference_frame('ce708b47-7e09-441c-a449-fab1793f99d2', post_churban_rabbinic_preservation).
narrative_ontology:cs_drift_state('ce708b47-7e09-441c-a449-fab1793f99d2', contemporary_halakhic_world, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce708b47-7e09-441c-a449-fab1793f99d2', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_restoration_generation).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, torah_eternal_relevance).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, messianic_preparation_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A generation that does not yet exist, which would receive the preserved technical knowledge of Temple service if restoration occurs. They bear no cost of maintenance, cannot consent to the arrangement, cannot resist it, and have no exit — they are the ultimate beneficiary of a constraint they did not choose and cannot influence. Their 'power' is rated moderate because if they exist and restoration occurs, they inherit a functioning system; if they never exist or restoration never occurs, the constraint's entire extraction was for nothing.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_restoration_generation, beneficiary,
    moderate, generational, trapped, universal).

% Halakhically observant individuals (primarily men in traditional communities) who devote significant daily study time to sacrificial law tractates (Kodashim, Taharot) and related technical literature. Under this reading, this study has no present devotional value — it is purely archive maintenance. The cost is biographical (years of study), cognitive (mastering complex technical material), and opportunity (foregone alternative pursuits). Exit is constrained: leaving the study track means leaving the halakhic community framework that structures their identity, marriage prospects, social standing, and communal support. They are 'organized' because they operate within institutionalized yeshiva/kollel systems that enforce the curriculum.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_study_practitioners, payer,
    organized, biographical, constrained, global).

% Yeshivas, kollels, rabbinic courts, and poskim (decisors) that set the curriculum, authorize the study obligation, certify competence, and maintain the transmission chain. They could change the curriculum (reduce sacrificial study, emphasize other areas) but are invested in the tradition's continuity and their own authority as its guardians. They benefit from the constraint's persistence (institutional relevance, authority, resource flows) but also bear administrative costs. Their exit is 'arbitrage' — they define the framework and can shift emphasis within it without losing their position.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% The analytical seat that sees the full structure across all three readings of the sacrifice_commandment kernel. Bears no cost, collects no benefit, has full exit. Provides the classification that the engine computes.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical knowledge corpus for Temple service (sacrificial taxonomy, purity protocols, altar dimensions, priestly procedures) across the exile period so that a future restoration generation can rebuild and operate the Temple without reinventing the tradition from fragments.
% TRANSFER_FUNCTION: Moves study effort, cognitive resources, and biographical time from present practitioners (organized halakhic communities) to a future restoration generation (non-extant, universal) via the halakhic institutions that maintain the transmission chain. The transfer is intergenerational and one-way.
% ABSENT_VOICES: The future restoration generation itself — they would object if the knowledge were lost (they would inherit nothing) but also might object to the present extraction if they could see its cost. They are structurally excluded because they do not exist. Also absent: halakhic practitioners who would prefer study_as_performance framing (present devotional value) but are constrained by institutional curriculum; secular or liberal Jewish voices who reject the entire restoration framework.
% DISAPPEARANCE_RATIONALE: If the archive_maintenance obligation vanished overnight, the technical knowledge of Temple service would attenuate rapidly within 1-2 generations. Halakhic institutions would shift curriculum emphasis to areas with present devotional or practical value. The restoration scenario would become practically impossible without reinventing the tradition from textual fragments. The halakhic world would reorganize around present-valued study and practice.
% FOUNDING_PROBLEM: After the Second Temple destruction (70 CE), the sacrificial system — the central worship modality of biblical religion — became inoperable. The founding problem was how to prevent the total loss of the technical knowledge required to restore that system if/when the Temple is rebuilt, given that the knowledge was complex, oral/practical, and dependent on continuous practice.
% FOUNDING_PROBLEM_CORROBORATION: The halakhic tradition (Talmud, Maimonides, later codes) attests the founding problem is live — exile continues, Temple not rebuilt, knowledge would be lost without maintenance. Maimonides (Hilkhot Beit HaBechirah) and the Talmudic tractates themselves (preserving details despite inoperability) corroborate from within the tradition. Outside the benefiting parties: academic scholars of ancient Judaism (e.g., Jacob Neusner, Lawrence Schiffman) document the historical reality of knowledge preservation as a conscious rabbinic project; historians of religion note similar preservation dynamics in other displaced cultic traditions. No corroboration exists for the restoration scenario itself — that remains a theological claim.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects moderate but real extraction: present practitioners devote significant cognitive and temporal resources to a study regime whose present payoff this reading explicitly denies, justified only by future utility. The constraint requires active enforcement (curriculum mandates, institutional authority, social pressure to maintain the study track) — without it, the technical knowledge would atrophy. Theater ratio (0.28) captures that some study activity is performative (maintaining the appearance of readiness for a restoration that may never come) while the core technical preservation is genuine. Accessibility collapse (0.45) is moderate: alternative framings (study as present worship, study as intellectual discipline) exist but are suppressed by this reading's authority. Resistance (0.38) reflects historical and contemporary pushback: rationalist critiques (Maimonidean), mystical alternatives (kabbalistic), and modern secular disengagement. The claimed type is tangled_rope because both coordination (technical preservation across exile) and asymmetric extraction (present pays, future benefits) are structurally present.
 *
 * PERSPECTIVAL GAP:
 *   From the present practitioner seat, the constraint feels like a demanding obligation with deferred and uncertain payoff — extraction-heavy. From the halakhic institution seat, it is a necessary coordination function maintaining a tradition that cannot be allowed to lapse — coordination-heavy. From the analytical observer seat, the structural asymmetry is clear: a non-extant beneficiary cannot consent, resist, or exit, while present practitioners are organized/biographical with constrained exit (leaving the halakhic framework entirely). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Future_restoration_generation is the structural beneficiary — receives the preserved knowledge corpus without bearing its maintenance cost. Directionality derivation places them at the beneficiary end (d near 0.0), but they have no exit options (non-extant) — the engine's derivation chain must handle non-extant agents. Present_study_practitioners are the structural payers — bear the full maintenance cost (study time, cognitive load, opportunity cost) with no present devotional return under this reading. Their power is 'organized' (embedded in halakhic communities) and exit is 'constrained' (leaving requires abandoning the communal framework). Halakhic_institutions are agenda_setters — they administer the constraint, set the curriculum, and could change it but are invested in its continuity. Their power is 'institutional', time_horizon 'generational', exit 'arbitrage' (they define the framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving Temple technical knowledge across exile) remains live in the sense that the exile continues and the knowledge would be lost without maintenance. But the restoration scenario that justifies the maintenance is contested in timing, form, and necessity. The arrangement persists with moderate extraction and rising theater — a classic mandatrophy signal where the coordination function (preservation) has been partially displaced by the extraction function (present obligation for future benefit). The mandate has not been formally resolved; the sunset condition (Temple restoration) is indefinitely deferred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'Is this constraint one reading of the sacrifice_commandment kernel, and does the kernel admit multiple structurally distinct readings?',
    'Cross-reading comparison of structural metrics and beneficiary/victim sets across sibling readings (study_as_performance, performance_only) to confirm they instantiate different constraints with different ε values.',
    'If multiple readings have divergent structural profiles, the kernel is confirmed as a committer frame requiring decomposition; if all readings converge, the decomposition was unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Kernel vs. reading decomposition for sacrifice commandment').

omega_variable(
    future_generation_beneficiary_ontology,
    'Can a future generation that does not yet exist be a structural beneficiary of a present constraint, and how is directionality derived for non-extant agents?',
    'Engine directionality derivation for beneficiaries with no present exit options; compare with present practitioners'' directionality to assess structural asymmetry.',
    'If future generation is the primary beneficiary, the constraint extracts from present practitioners for a beneficiary that cannot consent, resist, or exit — a structural extraction pattern distinct from present-beneficiary constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_beneficiary_ontology, conceptual, 'Beneficiary ontology for non-extant agents').

omega_variable(
    archive_vs_performance_boundary,
    'Where does the boundary lie between study-as-archive-maintenance (this reading) and study-as-performance (sibling reading), and do practitioners experience them as distinct?',
    'Ethnographic and textual analysis of how halakhic authorities and study practitioners categorize their own activity — whether the intentional frame (preparation vs. fulfillment) changes the structural experience of the constraint.',
    'If the boundary is porous in practice, the two readings may be a single constraint with observer-dependent framing; if sharp, they are structurally distinct constraints as the ε-invariance principle requires.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_vs_performance_boundary, empirical, 'Boundary between archive_maintenance and study_as_performance readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.2).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.23).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__archive_maintenance, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(sacr_tr_t2500, sacrifice_commandment__archive_maintenance, theater_ratio, 2500, 0.28).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__archive_maintenance, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(sacr_be_t2500, sacrifice_commandment__archive_maintenance, base_extractiveness, 2500, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.25).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.28).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__archive_maintenance, suppression_requirement, 1500, 0.3).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement(sacr_su_t2500, sacrifice_commandment__archive_maintenance, suppression_requirement, 2500, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three constraint stories: archive_maintenance (this story, moderate extraction, future beneficiary), study_as_performance (lower extraction, present devotional beneficiary), and performance_only (near-zero extraction, no present obligation). They form a constraint family linked by affects_constraints. The ε values differ because each reading structures the obligation differently: archive_maintenance extracts from present for future; study_as_performance coordinates present devotional practice; performance_only suspends the obligation entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, organized, 0.75).
constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
