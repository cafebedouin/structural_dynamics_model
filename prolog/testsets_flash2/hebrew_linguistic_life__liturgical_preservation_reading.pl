% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical preservation' reading of
 *   Hebrew's linguistic life. It asserts that Hebrew has always been a living
 *   language due to its continuous use in sacred contexts, regardless of its
 *   status as a vernacular. From this perspective, the secular revival of
 *   Hebrew (e.g., by Ben-Yehuda) is not a 'resurrection' but a desecration,
 *   as it removes Hebrew from its sacred context. The constraint is claimed
 *   as a Mountain because its proponents view this definition of linguistic
 *   life as an immutable truth of their tradition, not a human construct. The
 *   metrics reflect this: low extractiveness (it's about preservation, not
 *   material gain), low suppression (it's an internal definition, not
 *   externally enforced), and low theater (the practices are genuine, not
 *   performative).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'c5aa82bf-13d6-4e63-b1e2-720a89818f73').
narrative_ontology:cs_kernel_codification('c5aa82bf-13d6-4e63-b1e2-720a89818f73', fixed_text).
narrative_ontology:cs_authority_grounding('c5aa82bf-13d6-4e63-b1e2-720a89818f73', lineage).
narrative_ontology:cs_interpretation_layer_present('c5aa82bf-13d6-4e63-b1e2-720a89818f73').
narrative_ontology:cs_reading_relation('c5aa82bf-13d6-4e63-b1e2-720a89818f73', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('c5aa82bf-13d6-4e63-b1e2-720a89818f73', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('c5aa82bf-13d6-4e63-b1e2-720a89818f73', foundational, hebrew_is_lashon_hakodesh).
narrative_ontology:cs_axiom_status(hebrew_is_lashon_hakodesh, holdable).
narrative_ontology:cs_axiom_grounding('c5aa82bf-13d6-4e63-b1e2-720a89818f73', hebrew_is_lashon_hakodesh, theological).
narrative_ontology:cs_axiom('c5aa82bf-13d6-4e63-b1e2-720a89818f73', foundational, linguistic_continuity_through_sacred_use).
narrative_ontology:cs_axiom_status(linguistic_continuity_through_sacred_use, holdable).
narrative_ontology:cs_axiom_grounding('c5aa82bf-13d6-4e63-b1e2-720a89818f73', linguistic_continuity_through_sacred_use, conventional).
narrative_ontology:cs_reference_frame('c5aa82bf-13d6-4e63-b1e2-720a89818f73', unbroken_sacred_chain).
narrative_ontology:cs_drift_state('c5aa82bf-13d6-4e63-b1e2-720a89818f73', contemporary_secular_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c5aa82bf-13d6-4e63-b1e2-720a89818f73', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_rabbinic_tradition).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, sacred_texts_themselves).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_revivalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the continuous chain of recitation, study, and transmission of Hebrew sacred texts. For this tradition, Hebrew's 'life' is defined by this unbroken liturgical use, independent of vernacular status. They see Ben-Yehuda's secular revival as a desecration, not a resurrection.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_rabbinic_tradition, agenda_setter,
    institutional, generational, identity_locked, global).

% The texts are 'beneficiaries' in that their continued relevance and sanctity are preserved by this reading. Their 'life' is sustained by continuous engagement, regardless of external linguistic shifts. They are not an agent but are central to the constraint's definition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_texts_themselves, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_texts_themselves).

% These are the proponents of modern, vernacular Hebrew. From the liturgical preservation reading's perspective, their efforts are a 'cost' or 'victimization' to the sacred tradition, as they dilute the sanctity and unique role of Hebrew by secularizing it. They are 'victims' of the constraint's definition of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_revivalists, payer,
    organized, biographical, constrained, national).

% Study the historical and contemporary status of Hebrew, often engaging with both liturgical and vernacular uses. They analyze the claims of linguistic continuity and revival without necessarily endorsing one definition of 'life' over another.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the continuous, consistent transmission and interpretation of sacred texts across generations and geographies, maintaining a shared spiritual and intellectual heritage.
% TRANSFER_FUNCTION: Transfers the responsibility and authority for defining Hebrew's 'life' from secular, vernacular usage to the continuous, unbroken chain of liturgical and scholarly engagement with sacred texts.
% ABSENT_VOICES: Secular linguists who define language life purely by native speakers and daily use are implicitly excluded from this reading's definition, as their criteria are deemed irrelevant or even antithetical to Hebrew's true 'life'.
% DISAPPEARANCE_RATIONALE: If this definition of Hebrew's linguistic life disappeared, the orthodox tradition would continue its practices, as its understanding of Hebrew's sanctity is foundational. The 'world' of liturgical practice would remain unchanged, as it operates on an internal logic independent of external linguistic definitions.
% FOUNDING_PROBLEM: The perceived threat of Hebrew becoming a 'dead language' in the sense of losing its sacred function and continuous engagement, particularly in the face of diaspora and the rise of vernaculars.
% FOUNDING_PROBLEM_CORROBORATION: The orthodox rabbinic tradition itself attests to the ongoing live status of this problem, viewing any attempt to secularize or redefine Hebrew's 'life' as a continuous threat to its sacred purpose. No external corroboration is sought or accepted, as the definition is internal to the tradition.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that this reading is primarily about preserving a sacred tradition, not about material extraction. The 'victims' (secular revivalists) are 'victimized' by the definition itself, which invalidates their project, rather than by direct material extraction. Suppression is low (0.05) because the constraint is maintained by internal adherence to tradition, not by active coercion against external definitions. Accessibility collapse is high (0.9) because, within this framework, no alternative definition of Hebrew's 'life' is considered valid. Resistance is low (0.02) because the tradition largely ignores or dismisses external challenges to its definition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the orthodox tradition, this is a self-evident truth (a Mountain). From the perspective of secular revivalists, it is a conceptual Snare that denies the validity of their efforts and the 'rebirth' of Hebrew. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The orthodox rabbinic tradition is the agenda-setter and primary beneficiary, as this reading validates and perpetuates its core mission. The sacred texts themselves are also beneficiaries, as their continued sanctity is ensured. Secular Hebrew revivalists are 'victims' because their efforts are implicitly delegitimized by this definition of linguistic life. Linguistic scholars are observers, analyzing the phenomenon without being structurally bound by this specific definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_linguistic_life,
    'Is ''linguistic life'' an empirically verifiable state (e.g., number of native speakers, domains of use) or a culturally/theologically defined concept?',
    'Conceptual analysis of the term ''language life'' across different linguistic and cultural traditions, and empirical study of how different communities define and maintain their languages.',
    'If ''linguistic life'' is primarily an empirical concept, this constraint''s claim of ''emerges_naturally'' would be challenged, potentially reclassifying it as a conceptual Snare or Tangled Rope. If it is primarily a culturally/theologically defined concept, the Mountain classification would be reinforced within its own framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'Ambiguity in the definition of ''linguistic life'' itself.').

omega_variable(
    ben_yehuda_desecration_or_resurrection,
    'Is Eliezer Ben-Yehuda''s project of reviving Hebrew as a vernacular a desecration of a sacred language or a legitimate act of linguistic resurrection?',
    'Historical and sociological analysis of the impact of the revival on both sacred and secular uses of Hebrew, and theological debate within different Jewish traditions.',
    'If viewed as a legitimate resurrection, the ''victim'' status of secular revivalists would be inverted, and the constraint''s ''emerges_naturally'' claim would be undermined, as it would imply a ''death'' from which revival was necessary. If viewed as desecration, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ben_yehuda_desecration_or_resurrection, preference, 'Contested interpretation of the modern Hebrew revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t25, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(hebr_tr_t50, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(hebr_tr_t75, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t25, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(hebr_be_t50, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(hebr_be_t75, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t25, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(hebr_su_t50, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(hebr_su_t75, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
