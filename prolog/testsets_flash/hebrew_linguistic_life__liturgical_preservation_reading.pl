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
 *   Hebrew linguistic life, which asserts that the language's vitality is
 *   maintained through the unbroken chain of sacred recitation, study, and
 *   transmission, irrespective of its use in daily vernacular. From this
 *   perspective, Hebrew never 'died' and therefore did not require 'revival'
 *   by figures like Eliezer Ben-Yehuda; such efforts are seen as a
 *   secularization or even desecration of a sacred tradition. The constraint
 *   is claimed as a Mountain due to its perceived natural and immutable
 *   status within this theological framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'a5f52abd-b877-463d-ae9e-9ff7a38a9d1f').
narrative_ontology:cs_kernel_codification('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', fixed_text).
narrative_ontology:cs_authority_grounding('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', lineage).
narrative_ontology:cs_interpretation_layer_present('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f').
narrative_ontology:cs_reading_relation('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', foundational, hebrew_never_died).
narrative_ontology:cs_axiom_status(hebrew_never_died, holdable).
narrative_ontology:cs_axiom_grounding('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', hebrew_never_died, deontological).
narrative_ontology:cs_axiom('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', foundational, sacred_use_is_true_life).
narrative_ontology:cs_axiom_status(sacred_use_is_true_life, holdable).
narrative_ontology:cs_axiom_grounding('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', sacred_use_is_true_life, theological).
narrative_ontology:cs_reference_frame('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', unbroken_sacred_chain).
narrative_ontology:cs_drift_state('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', contemporary_secular_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a5f52abd-b877-463d-ae9e-9ff7a38a9d1f', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholars).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_tradition_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_language_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities maintain the continuous recitation, study, and transmission of Hebrew sacred texts as a core religious practice. For them, this practice is the very definition of Hebrew's 'life,' and its cessation would mean a profound loss of identity and tradition. They benefit from the perceived unbroken chain of transmission.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, orthodox_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% As custodians and interpreters of sacred texts, their professional and spiritual lives are entirely bound to the continuous study and transmission of Hebrew. They are the primary agents of this preservation and derive authority and meaning from it. They are the intellectual beneficiaries of this reading.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% These individuals use modern Hebrew as a vernacular language for daily life, often without deep engagement with sacred texts. From the perspective of this reading, their use of Hebrew, while widespread, does not constitute 'linguistic life' in the sacred sense, and their claims of 'revival' are seen as irrelevant or even disrespectful to the unbroken tradition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% The sacred tradition, as an abstract entity, 'pays' by being rigidly defined and protected from external interpretations that might dilute its meaning. It is a 'victim' of any attempt to redefine Hebrew's life outside of its liturgical context, as such attempts are seen as a desecration of its inherent sanctity and continuity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).

% Academically study the historical evolution and use of Hebrew, including its liturgical and vernacular forms. They analyze the claims of continuous transmission versus revival without necessarily endorsing one over the other, focusing on empirical evidence.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous practice of religious observance and scholarly transmission across generations and geographies, ensuring the integrity and accessibility of sacred texts.
% TRANSFER_FUNCTION: Transfers the responsibility and privilege of maintaining the sacred chain of Hebrew from one generation of scholars and communities to the next, ensuring the preservation of religious identity and knowledge.
% ABSENT_VOICES: Secular linguists and proponents of modern Hebrew revival are excluded; they would argue that a language's life is defined by its use in daily vernacular, not solely by sacred recitation. Their perspective is deemed irrelevant by this reading's framework.
% DISAPPEARANCE_RATIONALE: If the continuous liturgical use and transmission of Hebrew ceased, it would signify a catastrophic break in a millennia-old religious and cultural tradition. The identity of Orthodox Jewish communities and the entire framework of rabbinic scholarship would fundamentally collapse, necessitating a complete reorganization of religious life and self-understanding.
% FOUNDING_PROBLEM: The problem of ensuring the eternal preservation of the Hebrew language as the sacred vehicle for divine revelation and the unbroken chain of Jewish tradition, preventing its 'death' through disuse or secularization.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Jewish communities and rabbinic authorities universally attest that the problem of preserving Hebrew's sacred life is perpetually live, requiring constant vigilance against assimilation and secular redefinition. This is corroborated by centuries of continuous practice and theological texts from within the tradition, though secular scholars might frame it differently.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.1) and suppression (0.05) reflect the internal coherence and self-sustaining nature of this tradition. It is not seen as extracting from its participants but rather as providing a framework for spiritual and cultural continuity. Resistance is minimal (0.02) because within this framework, the definition is largely uncontested. Accessibility collapse is high (0.9) because alternatives to this definition of 'linguistic life' are simply not recognized as valid. The theater ratio is 0.0 as the practices are seen as genuinely functional for religious life, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, there is no significant perspectival gap among its adherents; the definition of Hebrew's life is a foundational truth. However, other readings (e.g., native generational, marketplace pidgin) would experience this constraint as highly suppressive and extractive, as it invalidates their claims to Hebrew's vitality.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox Jewish communities and rabbinic scholars are beneficiaries, as their identity and authority are deeply intertwined with this continuous tradition. The 'sacred tradition itself' is listed as a 'payer' (non-agent) because it bears the 'cost' of being rigidly defined and protected from external, secular interpretations that might dilute its meaning. Secular Hebrew speakers are 'excluded' as their definition of linguistic life is outside this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy from its own internal logic, as its mandate (preserving sacred tradition) is considered eternal and unchanging. The 'problem' it solves is perpetually live within its framework. Mandatrophy would only be perceived from an external, secular perspective that views the liturgical function as obsolete for 'linguistic life' defined by vernacular use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_tradition,
    'Is the continuous liturgical preservation of Hebrew a ''natural law'' of its linguistic existence, or a ''constructed tradition'' maintained by specific communities for theological reasons?',
    'Comparative historical linguistics and sociological analysis of language death/revival, examining whether other ''sacred'' languages have persisted solely through liturgical use without vernacular forms.',
    'If a constructed tradition, the constraint''s ''emerges_naturally'' claim would be reclassified as false, potentially shifting its type from Mountain to a more constructed form (e.g., Rope or Tangled Rope) for external observers, though not for its internal adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_tradition, conceptual, 'Ambiguity between inherent linguistic property and community-maintained practice.').

omega_variable(
    definition_of_linguistic_life,
    'Is ''linguistic life'' fundamentally defined by sacred transmission, or by vernacular use and native acquisition?',
    'This is a conceptual/preference question, not empirically resolvable. Resolution depends on which definition of ''life'' one adopts for a language.',
    'If an alternative definition of ''linguistic life'' (e.g., vernacular use) is adopted, this constraint would be seen as highly suppressive and extractive, as it actively denies the ''life'' of modern Hebrew, reclassifying it as a Snare or Tangled Rope from that perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'The core definitional dispute underlying the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(hebr_tr_t500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(hebr_be_t1000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(hebr_be_t1500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(hebr_su_t1000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(hebr_su_t1500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
