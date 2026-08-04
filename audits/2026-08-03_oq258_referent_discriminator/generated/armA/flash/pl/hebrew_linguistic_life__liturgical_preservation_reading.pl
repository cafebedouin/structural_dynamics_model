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
    narrative_ontology:constraint_vindicates/2,
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
 *   Hebrew linguistic life, which asserts that a language is alive as long as
 *   its sacred texts are continuously recited, studied, and transmitted,
 *   irrespective of its use in daily vernacular. From this perspective,
 *   Hebrew never 'died' and therefore did not require 'revival' by figures
 *   like Eliezer Ben-Yehuda; such efforts are seen as irrelevant or even a
 *   desecration of its sacred status. This reading is claimed as a Mountain
 *   due to its perceived naturalness within the tradition, with negligible
 *   extraction and suppression, as its persistence is seen as inherent to the
 *   religious practice itself.
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
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '622010c2-43a0-4095-9174-841e2953c936').
narrative_ontology:cs_kernel_codification('622010c2-43a0-4095-9174-841e2953c936', fixed_text).
narrative_ontology:cs_authority_grounding('622010c2-43a0-4095-9174-841e2953c936', lineage).
narrative_ontology:cs_interpretation_layer_present('622010c2-43a0-4095-9174-841e2953c936').
narrative_ontology:cs_reading_relation('622010c2-43a0-4095-9174-841e2953c936', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('622010c2-43a0-4095-9174-841e2953c936', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('622010c2-43a0-4095-9174-841e2953c936', foundational, sacred_use_defines_linguistic_life).
narrative_ontology:cs_axiom_status(sacred_use_defines_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('622010c2-43a0-4095-9174-841e2953c936', sacred_use_defines_linguistic_life, theological).
narrative_ontology:cs_axiom('622010c2-43a0-4095-9174-841e2953c936', foundational, hebrew_never_died).
narrative_ontology:cs_axiom_status(hebrew_never_died, holdable).
narrative_ontology:cs_axiom_grounding('622010c2-43a0-4095-9174-841e2953c936', hebrew_never_died, conventional).
narrative_ontology:cs_reference_frame('622010c2-43a0-4095-9174-841e2953c936', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('622010c2-43a0-4095-9174-841e2953c936', contemporary_secular_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('622010c2-43a0-4095-9174-841e2953c936', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_never_died_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_language_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities maintain the continuous recitation, study, and transmission of Hebrew sacred texts. For them, this practice is the very definition of the language's life, and they benefit from the continuity of their religious and cultural identity. Exit would mean abandoning their core religious practice and identity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% From this reading's perspective, the sacred tradition of Hebrew benefits from its unbroken liturgical use, as its meaning and sanctity are preserved through continuous engagement with its original linguistic form. It is a non-agent entity that is conceptually sustained by the constraint.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).

% These are speakers of modern, vernacular Hebrew who do not necessarily engage with the language through sacred texts. From the liturgical preservation reading, their use is irrelevant to the 'life' of the language, and their perspective on linguistic vitality is excluded from this definition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Advocates for the modern revival of Hebrew as a spoken vernacular. This reading implicitly rejects their premise that Hebrew 'died' and needed revival, thus excluding their efforts as either unnecessary or even a desecration of the sacred language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_revivalists, excluded,
    organized, generational, constrained, national).

% Academics who study the historical evolution and usage of Hebrew. They can analyze the claims of this reading against empirical evidence of language use and transmission, but their analysis does not directly alter the constraint's operation within the communities that adhere to it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous transmission of sacred knowledge and cultural identity across generations within religious communities by defining the 'life' of Hebrew through its liturgical use, ensuring a shared linguistic and textual heritage.
% TRANSFER_FUNCTION: Transfers the responsibility and privilege of preserving sacred Hebrew texts and their associated meaning to those who engage in continuous recitation and study, from past generations to future ones.
% ABSENT_VOICES: Secular Hebrew speakers and linguistic revivalists are absent from this definition of linguistic life; they would argue that a language's vitality is measured by its use in daily life and as a mother tongue, not solely by sacred practice. Their perspectives are deemed irrelevant or even antithetical to this reading.
% DISAPPEARANCE_RATIONALE: If this constraint (the belief that liturgical use defines Hebrew's life) disappeared, the self-perception and practices of Orthodox Jewish communities would fundamentally alter. The concept of Hebrew's unbroken continuity would be challenged, potentially leading to a re-evaluation of modern Hebrew's status and the historical narrative of the language.
% FOUNDING_PROBLEM: The perceived threat of linguistic and cultural assimilation, and the need to maintain the sanctity and continuity of Hebrew as a sacred language for religious practice and identity across the diaspora.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Jewish religious authorities and scholars consistently attest to the ongoing need for this preservation, citing the importance of maintaining tradition against modernizing influences. This corroboration comes from within the benefiting communities, as the premise is foundational to their identity.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects that this definition of linguistic life is primarily a self-sustaining cultural and religious practice, not a mechanism for material gain. Suppression (0.05) is minimal because adherence is voluntary, driven by identity and belief rather than coercion. Theater ratio (0.05) is low as the practice is genuinely functional for its adherents. Accessibility collapse is high (0.9) because, within this framework, alternatives to liturgical preservation for defining 'life' are almost entirely collapsed. Resistance is near zero (0.02) because those who adhere to this reading do not perceive it as a constraint to be resisted, but rather as a foundational truth.
 *
 * PERSPECTIVAL GAP:
 *   For adherents of this reading, the constraint is a self-evident truth (Mountain). For those who define linguistic life by vernacular use, this reading is a conceptual barrier that dismisses their efforts and perspectives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox Jewish communities are beneficiaries, as this reading validates their continuous practice and identity. The 'sacred tradition itself' is also a conceptual beneficiary, as its continuity is affirmed. Secular Hebrew speakers and linguistic revivalists are excluded, as their definitions of linguistic life are deemed irrelevant or contradictory to this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_belief,
    'Is the definition of Hebrew''s linguistic life through liturgical preservation a natural, inherent truth, or a constructed belief system maintained by specific communities?',
    'Comparative anthropological and sociolinguistic studies of other ''sacred languages'' and their communities, examining whether similar definitions of ''life'' emerge universally or are culturally specific.',
    'If constructed, the constraint''s ''emerges_naturally'' claim would be challenged, potentially reclassifying it from Mountain to a more constructed type (e.g., Identity Coordination Rope or Snare, depending on the degree of internal enforcement and external exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_belief, conceptual, 'Ambiguity between inherent linguistic truth and culturally constructed definition of language vitality.').

omega_variable(
    victim_set_ambiguity,
    'Does this reading''s implicit rejection of modern Hebrew''s ''revival'' constitute a ''victimization'' of the modern Hebrew language or its speakers, or is it merely a definitional difference?',
    'Analysis of the material consequences (e.g., funding, social status, educational curricula) for modern Hebrew and its speakers within communities that strictly adhere to the liturgical preservation reading. If material harm or suppression of modern use is demonstrated, the victim set would expand.',
    'If modern Hebrew or its speakers are identified as victims, the constraint''s extractiveness and suppression metrics would increase, and its classification would shift away from Mountain, likely towards a Snare or Tangled Rope, as it would be actively extracting from or suppressing an alternative form of linguistic life.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_ambiguity, empirical, 'Whether definitional exclusion translates into active victimization or merely a difference in perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebr_tr_t1850, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hebr_be_t1850, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(hebr_su_t1850, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
