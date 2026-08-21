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
 *   Hebrew's linguistic life, which asserts that the language has remained
 *   continuously 'alive' through its unbroken use in sacred texts,
 *   recitation, and study, irrespective of its status as a vernacular. From
 *   this perspective, the modern revival of Hebrew for daily speech is not a
 *   'resurrection' but a separate phenomenon, or even a desecration of its
 *   sacred status. This reading claims the constraint is a Mountain,
 *   reflecting its proponents' view of it as an unchangeable truth about the
 *   language's inherent nature and its relationship to tradition.
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
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'a44eb84c-0c7c-4918-97cb-2938c51e7361').
narrative_ontology:cs_kernel_codification('a44eb84c-0c7c-4918-97cb-2938c51e7361', fixed_text).
narrative_ontology:cs_authority_grounding('a44eb84c-0c7c-4918-97cb-2938c51e7361', lineage).
narrative_ontology:cs_interpretation_layer_present('a44eb84c-0c7c-4918-97cb-2938c51e7361').
narrative_ontology:cs_reading_relation('a44eb84c-0c7c-4918-97cb-2938c51e7361', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('a44eb84c-0c7c-4918-97cb-2938c51e7361', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('a44eb84c-0c7c-4918-97cb-2938c51e7361', foundational, linguistic_life_is_liturgical_continuity).
narrative_ontology:cs_axiom_status(linguistic_life_is_liturgical_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a44eb84c-0c7c-4918-97cb-2938c51e7361', linguistic_life_is_liturgical_continuity, theological).
narrative_ontology:cs_axiom('a44eb84c-0c7c-4918-97cb-2938c51e7361', secondary, vernacular_use_is_irrelevant_to_sacred_life).
narrative_ontology:cs_axiom_status(vernacular_use_is_irrelevant_to_sacred_life, holdable).
narrative_ontology:cs_axiom_grounding('a44eb84c-0c7c-4918-97cb-2938c51e7361', vernacular_use_is_irrelevant_to_sacred_life, deontological).
narrative_ontology:cs_reference_frame('a44eb84c-0c7c-4918-97cb-2938c51e7361', unbroken_sacred_chain).
narrative_ontology:cs_drift_state('a44eb84c-0c7c-4918-97cb-2938c51e7361', contemporary_linguistic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a44eb84c-0c7c-4918-97cb-2938c51e7361', '').
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

% These communities maintain the continuous recitation, study, and transmission of Hebrew sacred texts, viewing this practice as the true measure of the language's life. They benefit from the continuity of their religious and cultural identity, which is deeply intertwined with this linguistic understanding.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% From this reading's perspective, the sacred tradition of Hebrew benefits from its unbroken liturgical use, as its integrity and meaning are preserved through continuous engagement with its original linguistic form. It is a non-agent entity that is conceptually sustained by the constraint.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).

% These are individuals who use modern Hebrew as a vernacular language, often without deep engagement with its sacred texts. Their understanding of 'linguistic life' (based on daily use) is excluded from this reading's definition, though they are not directly harmed by it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Advocates for the modern revival of Hebrew (e.g., Ben-Yehuda's project) are excluded from this reading's definition of linguistic life. Their efforts are seen as creating a new language or desecrating the sacred one, rather than continuing an unbroken chain.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_revivalists, excluded,
    organized, generational, constrained, national).

% Academics who study the historical evolution of Hebrew and its various forms, including both liturgical and vernacular uses. They analyze the claims of different readings without necessarily endorsing one over another, focusing on empirical evidence of language use and change.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the continuous and consistent transmission of sacred texts and the associated religious and cultural identity across generations within Orthodox Jewish communities, by defining 'linguistic life' as liturgical continuity.
% TRANSFER_FUNCTION: Transfers the authority and legitimacy of defining 'linguistic life' from vernacular use or native speakers to the unbroken chain of sacred recitation and study, from the sacred tradition to its adherents.
% ABSENT_VOICES: Secular Hebrew speakers and linguistic revivalists are absent from this definition of linguistic life; they would argue that a language's vitality is measured by its daily, vernacular use and native acquisition, not solely by liturgical practice.
% DISAPPEARANCE_RATIONALE: If this specific reading of Hebrew's linguistic life disappeared, the actual practices of recitation, study, and transmission within Orthodox communities would likely continue, as they are deeply ingrained. What would change is the *interpretation* of those practices and the associated claims about Hebrew's historical status (e.g., whether it 'died' or not). The physical reality of the language's use would remain, but the conceptual framework for understanding it would shift.
% FOUNDING_PROBLEM: The problem of maintaining the sacred status and continuity of Hebrew as a holy language, distinct from its potential secularization or 'death' as a spoken tongue, particularly in diaspora contexts.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Jewish communities universally attest that the problem of preserving sacred linguistic continuity is live and central to their faith. Historical linguists, while not endorsing the theological claim, corroborate the historical fact of continuous liturgical use, distinguishing it from vernacular decline.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The metrics reflect the 'Mountain' claim: extractiveness is low (0.15) because the constraint primarily defines a state of affairs rather than extracting resources; suppression is low (0.05) as it's maintained by voluntary adherence to tradition, not coercion. Theater ratio is low (0.05) as the practices are genuinely functional for the communities. Accessibility collapse is high (0.9) because, from this reading's perspective, there are no 'alternatives' to this definition of linguistic life; it simply *is* the truth. Resistance is negligible (0.02) because those who adhere to this reading do not resist it, and those who disagree are simply operating under a different definition, not actively fighting this one.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between adherents of this reading and those who define linguistic life by vernacular use. For the former, Hebrew never died; for the latter, it was revived. This constraint models the former perspective, where the 'Mountain' classification reflects the perceived naturalness and inevitability of liturgical continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox Jewish communities are beneficiaries (d near 0.0) as their identity and tradition are affirmed and preserved by this reading. The 'sacred tradition itself' is also a conceptual beneficiary. There are no direct 'victims' in this reading, as it doesn't impose costs but rather defines a state. Secular Hebrew speakers and linguistic revivalists are 'excluded' from this definition, but not directly extracted from by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the conventional sense, as its 'mandate' is to define a continuous state of being rather than to solve a problem that could become obsolete. The persistence is inherent to the claim of unbroken tradition. The classification prevents mislabeling a deeply held, self-sustaining cultural/religious definition as an extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_definition,
    'Is this definition of ''linguistic life'' a genuine natural law of language and tradition, or a constructed definition that benefits identifiable religious communities by affirming their historical narrative?',
    'Comparative analysis of other ''sacred languages'' and their communities: if similar definitions consistently emerge across diverse traditions, it supports a more ''natural'' interpretation; if it''s unique to this context, it suggests a constructed aspect.',
    'If primarily constructed, the constraint might be reclassified from Mountain to a more coordination-oriented type (e.g., Rope or Tangled Rope), reflecting the active maintenance of a beneficial interpretive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_definition, conceptual, 'Ambiguity between inherent linguistic truth and a culturally beneficial interpretive framework.').

omega_variable(
    impact_on_secular_hebrew_identity,
    'Does this reading, by denying the ''death'' and ''revival'' of Hebrew, implicitly devalue the cultural and national identity of secular Hebrew speakers who identify with the modern vernacular?',
    'Sociological studies on identity formation among secular Hebrew speakers and their perception of historical narratives. Analysis of public discourse and educational curricula.',
    'If it significantly devalues secular identity, it introduces an unacknowledged ''victim'' group (secular_hebrew_speakers) and a subtle form of extraction (of cultural legitimacy), potentially shifting the classification towards a Snare or Tangled Rope from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_secular_hebrew_identity, preference, 'Unacknowledged impact on the identity and legitimacy of secular Hebrew culture.').


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
