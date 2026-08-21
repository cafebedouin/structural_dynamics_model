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
 *   human_readable: Hebrew Vitality: Liturgical Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story models the 'liturgical reading' of Hebrew vitality,
 *   which asserts that unbroken ritual use is sufficient for the language to
 *   be considered 'vital.' This reading is distinct from those emphasizing
 *   native daily speech or a hybrid approach. It is presented as a Mountain
 *   due to its deep entrenchment in religious tradition and the low perceived
 *   cost or extraction associated with its maintenance within this specific
 *   framework. The beneficiaries are those whose authority and identity are
 *   tied to this definition of vitality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.05).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.02).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality: Liturgical Reading").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '1862d0b6-e882-4797-a3b3-c448254fba0f').
narrative_ontology:cs_kernel_codification('1862d0b6-e882-4797-a3b3-c448254fba0f', implicit).
narrative_ontology:cs_authority_grounding('1862d0b6-e882-4797-a3b3-c448254fba0f', lineage).
narrative_ontology:cs_interpretation_layer_present('1862d0b6-e882-4797-a3b3-c448254fba0f').
narrative_ontology:cs_reading_relation('1862d0b6-e882-4797-a3b3-c448254fba0f', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('1862d0b6-e882-4797-a3b3-c448254fba0f', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('1862d0b6-e882-4797-a3b3-c448254fba0f', foundational, liturgical_continuity_is_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('1862d0b6-e882-4797-a3b3-c448254fba0f', liturgical_continuity_is_vitality, deontological).
narrative_ontology:cs_reference_frame('1862d0b6-e882-4797-a3b3-c448254fba0f', unbroken_sacred_transmission).
narrative_ontology:cs_drift_state('1862d0b6-e882-4797-a3b3-c448254fba0f', contemporary_sociolinguistic_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('1862d0b6-e882-4797-a3b3-c448254fba0f', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, traditional_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities define and uphold the liturgical use of Hebrew, viewing its unbroken ritual transmission as the core of its vitality. Their authority is partly constituted by this claim.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% For these communities, Hebrew's vitality is intrinsically linked to its sacred, liturgical function. They experience the language as alive through prayer and study, not necessarily through daily vernacular use.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, traditional_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% These groups prioritize Hebrew's use as a modern, spoken language, often viewing liturgical use as insufficient for true vitality. They are excluded from the definitional authority of the liturgical reading.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_hebrew_revivalists, excluded,
    moderate, biographical, mobile, national).

% Academically analyze the historical and contemporary status of Hebrew, often distinguishing between liturgical preservation and vernacular revitalization. They observe the debate without being bound by its internal claims.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of Hebrew's status and role within traditional Jewish religious practice, ensuring continuity of sacred texts and rituals across generations.
% TRANSFER_FUNCTION: Transfers the concept of 'vitality' from a purely vernacular, spoken status to one encompassing ritual and sacred use, thereby preserving the authority of traditional religious institutions.
% ABSENT_VOICES: Secular Hebrew revivalists and some linguistic scholars would argue that liturgical use alone does not constitute full vitality, emphasizing the need for native speakers and daily vernacular use. Their perspective is not central to this reading's definition of vitality.
% DISAPPEARANCE_RATIONALE: If the claim that 'ritual preservation constitutes vitality' vanished, traditional communities would likely continue liturgical use, but the *justification* for its vitality would shift, potentially opening space for other definitions. The practice itself is deeply ingrained.
% FOUNDING_PROBLEM: The problem of maintaining the sacred status and continuity of Hebrew as a language of religious practice and identity, particularly during periods when it was not a spoken vernacular.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and traditional Jewish communities universally attest that the problem of maintaining sacred language continuity is live. Linguistic scholars corroborate the historical challenge of language maintenance but may dispute the sufficiency of liturgical use for 'vitality' in a modern sense.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because this reading primarily defines a state of affairs rather than imposing significant costs or transfers. Suppression is minimal (0.02) as adherence is largely voluntary and identity-driven within traditional communities, not coercively enforced. Theater ratio is negligible (0.01) as the liturgical practices are genuine and central to the communities' self-conception. Accessibility collapse is high (0.95) because, within this framework, the alternative definitions of vitality (e.g., requiring native speakers) are largely irrelevant or dismissed. Resistance is low (0.01) because those who disagree with this definition typically operate in different linguistic or cultural frameworks rather than actively resisting this specific claim.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authorities, this is a self-evident truth, a 'mountain' of cultural and religious continuity. From the perspective of secular revivalists, it might be seen as a 'snare' that obscures the true state of the language, but that is a different reading of the kernel, not a different perspective on this specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and traditional Jewish communities are beneficiaries (d near 0.0) as their cultural and religious continuity is affirmed by this definition of vitality. There are no direct 'victims' in this reading, as the constraint primarily defines a state rather than extracting from specific parties. Secular Hebrew revivalists are 'excluded' from the definitional authority, but not directly harmed by the liturgical reading itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''vitality'' for a language fundamentally defined by unbroken liturgical use, or by the presence of native, daily speakers?',
    'Conceptual clarification and agreement on the scope of ''vitality'' in sociolinguistics, or a shift in community consensus regarding the primary marker of a language''s aliveness.',
    'If vitality is redefined to require native speakers, this constraint would be reclassified from a Mountain to a Snare (as it would then be seen as obscuring a lack of true vitality), with significant implications for language revitalization efforts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the core definition of ''language vitality'' itself.').

omega_variable(
    natural_vs_constructed_mountain,
    'Is the claim that ''ritual preservation constitutes vitality'' a natural, self-evident truth within its cultural context, or a constructed claim that benefits identifiable agents (rabbinic authorities)?',
    'Historical and sociological analysis of the claim''s emergence and its functional role in maintaining institutional authority, particularly in comparison to alternative definitions of vitality.',
    'If found to be a constructed claim primarily benefiting rabbinic authorities, the constraint would be reclassified as a Tangled Rope (false summit mountain), as it would then possess both a coordination function (cultural continuity) and asymmetric extraction (legitimacy for authorities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_mountain, empirical, 'Whether the ''mountain'' status is genuinely natural or a constructed claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(hebr_tr_t25, hebrew_vitality__liturgical_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(hebr_tr_t50, hebrew_vitality__liturgical_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(hebr_tr_t75, hebrew_vitality__liturgical_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t25, hebrew_vitality__liturgical_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(hebr_be_t50, hebrew_vitality__liturgical_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(hebr_be_t75, hebrew_vitality__liturgical_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(hebr_su_t25, hebrew_vitality__liturgical_reading, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(hebr_su_t50, hebrew_vitality__liturgical_reading, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(hebr_su_t75, hebrew_vitality__liturgical_reading, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
