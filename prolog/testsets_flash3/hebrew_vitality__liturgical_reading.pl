% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality: Liturgical Preservation Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical reading' of Hebrew vitality,
 *   asserting that unbroken use in religious ritual is sufficient for the
 *   language to be considered 'vital'. It is presented as a natural,
 *   self-evident truth within certain religious frameworks. The constraint's
 *   low extractiveness and suppression reflect its status as a deeply
 *   internalized cultural and religious norm rather than an actively
 *   enforced, costly mechanism. It benefits rabbinic authorities and
 *   religious communities by validating their traditions and roles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.05).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '1c757b7d-9181-4086-91a3-dec8164c9f0b').
narrative_ontology:cs_kernel_codification('1c757b7d-9181-4086-91a3-dec8164c9f0b', formalized).
narrative_ontology:cs_authority_grounding('1c757b7d-9181-4086-91a3-dec8164c9f0b', lineage).
narrative_ontology:cs_interpretation_layer_present('1c757b7d-9181-4086-91a3-dec8164c9f0b').
narrative_ontology:cs_reading_relation('1c757b7d-9181-4086-91a3-dec8164c9f0b', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c757b7d-9181-4086-91a3-dec8164c9f0b', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('1c757b7d-9181-4086-91a3-dec8164c9f0b', foundational, liturgical_continuity_is_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('1c757b7d-9181-4086-91a3-dec8164c9f0b', liturgical_continuity_is_vitality, deontological).
narrative_ontology:cs_reference_frame('1c757b7d-9181-4086-91a3-dec8164c9f0b', unbroken_sacred_tradition).
narrative_ontology:cs_drift_state('1c757b7d-9181-4086-91a3-dec8164c9f0b', post_modern_hebrew_revival, gap(stable, minor, false)).
narrative_ontology:cs_created_at('1c757b7d-9181-4086-91a3-dec8164c9f0b', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, religious_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, sacred_language_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their authority and the continuity of their tradition are affirmed by the unbroken liturgical use of Hebrew. They benefit from the constraint's persistence as it validates their role as custodians of the sacred language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% Their spiritual and cultural identity is deeply intertwined with the use of Hebrew in prayer and study. The constraint provides a sense of continuity and connection to their heritage, reinforcing community bonds.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, religious_communities, beneficiary,
    organized, generational, identity_locked, global).

% Analyze language vitality from a purely descriptive, non-normative perspective, often focusing on native speakers and daily use. They observe the liturgical use but may not consider it sufficient for 'vitality' in their framework.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_linguists, observer,
    analytical, generational, analytical, global).

% Advocate for Hebrew as a spoken, modern language, not just a liturgical one. While they respect the ritual use, they would argue it's insufficient for true vitality and that a broader, vernacular use is essential. Their perspective is often marginalized in discussions dominated by religious authorities.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, hebrew_revivalists, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous use of Hebrew in religious rituals across diverse Jewish communities globally, ensuring a shared sacred linguistic heritage.
% TRANSFER_FUNCTION: Transfers a sense of historical continuity and religious legitimacy to rabbinic authorities and religious communities, reinforcing their identity and traditional structures.
% ABSENT_VOICES: Secular linguists and Hebrew revivalists, who would argue that liturgical use alone does not constitute full language vitality, are often excluded from the normative definition of 'vitality' within this framework.
% DISAPPEARANCE_RATIONALE: If the belief that liturgical preservation constitutes Hebrew's vitality vanished, it would fundamentally alter the self-understanding and practices of many Jewish communities and rabbinic institutions, forcing a re-evaluation of language status and cultural continuity.
% FOUNDING_PROBLEM: The existential threat of Hebrew's complete disappearance as a living language after the destruction of the Second Temple and subsequent diasporas.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities attest the problem is live, emphasizing the ongoing need for sacred language preservation. However, secular linguists and historians, from outside the benefiting parties, corroborate that Hebrew's 'death' as a vernacular was largely averted by the modern revival, rendering the original problem of total disappearance largely 'dead' in its original form, though the *value* of liturgical continuity remains.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_vitality__liturgical_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.1) reflect that, within this reading, liturgical use is a self-sustaining practice with minimal coercive overhead. It's a 'mountain' because its 'vitality' is seen as an intrinsic property of its sacred function, not dependent on external factors like native speakers. The high accessibility collapse (0.9) means that for those who accept this reading, alternatives to liturgical use for 'vitality' are largely irrelevant. Resistance is minimal (0.02) because the claim is deeply embedded in religious identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authorities and religious communities, this constraint is a self-evident truth, a 'mountain' that ensures the continuity of a sacred language. From the perspective of secular linguists or Hebrew revivalists (excluded stakeholders), this reading might be seen as a 'snare' or 'tangled rope' that suppresses broader vernacular use by defining vitality too narrowly, but this story only captures the liturgical reading's internal logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and religious communities are beneficiaries (d near 0.0) as their identity and authority are affirmed by this definition of vitality. There are no direct 'victims' within this reading, as the preservation imposes no direct cost on participants; rather, it is a source of cultural and spiritual gain. Secular linguists are observers, and Hebrew revivalists are excluded, bearing no direct costs or benefits from this specific definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving Hebrew's vitality) is considered 'dead' by external observers (secular linguists, revivalists) due to the success of modern Hebrew revival. However, within the liturgical reading, the mandate remains 'live' as the continuous sacred use is seen as an ongoing, essential act of preservation. The classification as a 'mountain' within this reading prevents mislabeling it as a 'snare' or 'piton' by acknowledging its deeply internalized, non-extractive nature from the perspective of its adherents, even if its founding problem is externally contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_ambiguity,
    'Is ''language vitality'' fundamentally constituted by liturgical preservation, or does it require native speakers and daily vernacular use?',
    'Conceptual clarification and agreement on a shared definition of ''vitality'' across religious and secular linguistic frameworks, or empirical observation of language use patterns and speaker demographics.',
    'If vitality is defined by vernacular use, this constraint would be reclassified from a ''mountain'' to a ''snare'' or ''tangled rope'' from the perspective of those seeking broader use, as it would be seen as suppressing alternative forms of vitality. If liturgical preservation is accepted as a sufficient condition, the ''mountain'' classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''language vitality'' itself.').

omega_variable(
    false_summit_potential,
    'Is this constraint a genuine natural law of sacred language continuity, or a constructed claim that primarily benefits rabbinic authorities by validating their traditional roles?',
    'Historical analysis of the evolution of rabbinic authority and its relationship to language preservation, alongside a sociological study of the perceived ''naturalness'' of this claim within religious communities versus its functional benefits to institutional power.',
    'If found to be primarily a constructed claim for institutional benefit, the constraint would be reclassified as a ''tangled_rope'' or ''snare'', reflecting its extractive function despite its ''mountain'' presentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_potential, empirical, 'Whether the ''naturalness'' of liturgical vitality is a genuine feature or a constructed claim benefiting specific agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t500, hebrew_vitality__liturgical_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_vitality__liturgical_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_vitality__liturgical_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__liturgical_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t500, hebrew_vitality__liturgical_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(hebr_be_t1000, hebrew_vitality__liturgical_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(hebr_be_t1500, hebrew_vitality__liturgical_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__liturgical_reading, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t500, hebrew_vitality__liturgical_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(hebr_su_t1000, hebrew_vitality__liturgical_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(hebr_su_t1500, hebrew_vitality__liturgical_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__liturgical_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_vitality' kernel. This 'liturgical_reading' defines vitality by ritual preservation, influencing and coexisting with other readings that emphasize vernacular use or hybrid approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
