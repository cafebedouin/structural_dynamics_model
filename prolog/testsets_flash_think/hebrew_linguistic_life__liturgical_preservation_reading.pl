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
    narrative_ontology:coordination_type/2,
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
 *   This constraint defines the 'life' of the Hebrew language through the
 *   continuous, unbroken chain of recitation, study, and transmission of its
 *   sacred texts, irrespective of its use in daily vernacular speech. From
 *   this perspective, Hebrew never 'died' and therefore did not require
 *   'revival' by modern secular movements. The constraint describes a state
 *   of affairs maintained by religious communities, which, from their
 *   internal perspective, is a natural and continuous manifestation of the
 *   language's inherent vitality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'a5bed63a-8263-419a-9d9e-e68615a83ee2').
narrative_ontology:cs_kernel_codification('a5bed63a-8263-419a-9d9e-e68615a83ee2', fixed_text).
narrative_ontology:cs_authority_grounding('a5bed63a-8263-419a-9d9e-e68615a83ee2', lineage).
narrative_ontology:cs_interpretation_layer_present('a5bed63a-8263-419a-9d9e-e68615a83ee2').
narrative_ontology:cs_reading_relation('a5bed63a-8263-419a-9d9e-e68615a83ee2', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('a5bed63a-8263-419a-9d9e-e68615a83ee2', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('a5bed63a-8263-419a-9d9e-e68615a83ee2', foundational, linguistic_life_is_textual_transmission).
narrative_ontology:cs_axiom_status(linguistic_life_is_textual_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a5bed63a-8263-419a-9d9e-e68615a83ee2', linguistic_life_is_textual_transmission, theological).
narrative_ontology:cs_axiom('a5bed63a-8263-419a-9d9e-e68615a83ee2', secondary, vernacular_use_is_irrelevant_to_life).
narrative_ontology:cs_axiom_status(vernacular_use_is_irrelevant_to_life, holdable).
narrative_ontology:cs_axiom_grounding('a5bed63a-8263-419a-9d9e-e68615a83ee2', vernacular_use_is_irrelevant_to_life, conventional).
narrative_ontology:cs_reference_frame('a5bed63a-8263-419a-9d9e-e68615a83ee2', unbroken_sacred_chain_continuity).
narrative_ontology:cs_drift_state('a5bed63a-8263-419a-9d9e-e68615a83ee2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5bed63a-8263-419a-9d9e-e68615a83ee2', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, the_sacred_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary custodians and interpreters of the sacred texts. Their professional and spiritual identity is fused with the continuous study and transmission of Hebrew in its liturgical form. They define and enforce the standards of preservation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Communities whose religious and cultural identity is deeply intertwined with the continuous recitation and study of Hebrew sacred texts. They benefit from the spiritual continuity and shared identity, and bear the costs of maintaining the tradition through active participation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities, beneficiary,
    organized, generational, identity_locked, local).

% The abstract entity of the unbroken chain of transmission itself. It 'pays' the cost of potential breakage or degradation if the communities fail to uphold it, losing its 'life' by this definition. Its existence is entirely dependent on the continuous actions of its custodians.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, the_sacred_tradition, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, the_sacred_tradition).

% Advocates for modern, vernacular Hebrew as the primary marker of linguistic life. From the perspective of this constraint, their efforts are irrelevant or even a desecration, as they do not contribute to the 'unbroken chain' of sacred transmission. They are excluded from this definition of linguistic vitality.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_revivalists, excluded,
    organized, biographical, constrained, national).

% Academic researchers who study language vitality based on empirical metrics like native speakers, daily use, and intergenerational transmission. They observe the claims of this constraint but operate from a different definitional framework.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous, intergenerational transmission, study, and recitation of Hebrew sacred texts, ensuring the preservation of a specific form of linguistic and spiritual life.
% TRANSFER_FUNCTION: Transfers spiritual merit, cultural identity, and textual knowledge across generations within liturgical communities, from elders and scholars to students and congregants.
% ABSENT_VOICES: Secular Hebrew revivalists and many sociolinguists are absent from this definition of linguistic life. They would argue that language vitality is primarily about vernacular use and native speakers, not solely sacred transmission, and that Hebrew 'died' before its modern revival.
% DISAPPEARANCE_RATIONALE: If the continuous chain of recitation, study, and transmission of sacred texts in Hebrew were to break, the religious and cultural identity of the communities that uphold it would be fundamentally altered. Their spiritual connection to the past and their collective self-understanding would be profoundly disrupted, leading to a significant reorganization of their social and religious structures.
% FOUNDING_PROBLEM: The perceived threat of linguistic and cultural assimilation, the loss of sacred knowledge, and the spiritual discontinuity that would arise from the cessation of the unbroken chain of Hebrew sacred text transmission.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and community elders within the liturgical communities consistently attest to the ongoing nature of this threat, emphasizing the constant vigilance required to maintain the tradition. While external sociolinguists might not share the same definition of 'linguistic life,' they would acknowledge the *community's perception* of the problem as a live concern for cultural and religious continuity.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.15) reflects that, from this reading's perspective, the constraint is a description of an existing, self-sustaining cultural and spiritual fact, rather than an extractive mechanism. The effort involved in maintaining the tradition is seen as inherent to its 'life,' not a cost imposed by an external structure. Suppression (0.1) is minimal, as adherence is driven by internal commitment and identity, not coercion. Theater ratio (0.05) is low because the acts of recitation and study are genuine expressions of faith and tradition. Accessibility collapse (0.9) is high because, by this definition, there are few, if any, alternative paths to 'linguistic life' for Hebrew. Resistance (0.1) is low from within the communities that uphold this view.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious scholars and liturgical communities, this constraint describes a fundamental truth about Hebrew's enduring life. From the perspective of secular sociolinguists or revivalists, this definition is narrow and ignores the empirical realities of language use, potentially mislabeling a dormant language as 'alive' or a revived language as 'dead.' The engine's FSM will detect the presence of beneficiaries on a claimed 'mountain' and flag it for re-evaluation, highlighting this perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and liturgical communities are beneficiaries, as their spiritual and cultural identity is sustained by this continuous practice. The 'sacred tradition' itself is listed as a victim (payer role as a non-agent stakeholder) because its 'life' is at stake if the chain breaks. Secular Hebrew revivalists are excluded, as their definition of linguistic life is orthogonal to, and often seen as undermining, this sacred continuity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    descriptive_vs_normative_claim,
    'Is this constraint a descriptive claim about the inherent nature of Hebrew''s linguistic life, or a normative claim about how Hebrew *should* be kept alive?',
    'Analysis of theological and philosophical texts from within the tradition: if the claim is presented as an immutable truth about reality, it leans descriptive; if it''s a prescriptive command, it leans normative.',
    'If purely descriptive, the ''mountain'' classification is stronger. If normative, the presence of beneficiaries and the active maintenance suggests a ''tangled_rope'' or ''rope'' classification, as it requires human coordination to uphold a prescribed state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_vs_normative_claim, conceptual, 'Ambiguity between a descriptive truth-claim and a normative prescription for linguistic life.').

omega_variable(
    sacred_tradition_as_victim_validity,
    'Is ''the_sacred_tradition'' a valid ''victim'' in the sense of bearing costs, or is this a metaphorical framing for the costs borne by its human custodians?',
    'Clarification of the ontological status of ''tradition'' within the relevant theological framework: does it have an independent ''life'' that can be harmed, or is its ''harm'' reducible to the harm of its human adherents?',
    'If purely metaphorical, the ''victim'' status should be re-assigned to the human custodians, potentially altering the directionality and extraction profile for those agents. If ontologically distinct, the current framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_tradition_as_victim_validity, conceptual, 'Validity of an abstract entity as a ''victim'' bearing costs.').

omega_variable(
    hebrew_death_and_revival_status,
    'Did Hebrew truly remain ''alive'' through liturgical preservation, or did it ''die'' as a vernacular language and subsequently undergo a ''revival''?',
    'Empirical sociolinguistic studies on vernacular use and intergenerational transmission prior to the modern revival, combined with historical analysis of the ''unbroken chain'' claim.',
    'If Hebrew ''died'' by empirical metrics, this reading''s claim of continuous life is challenged, potentially shifting its classification from ''mountain'' to a ''snare'' (if the claim is used to suppress alternatives) or a ''piton'' (if maintained theatrically despite empirical evidence). If the chain is demonstrably unbroken and sufficient for ''life'' by some metric, the ''mountain'' claim is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hebrew_death_and_revival_status, empirical, 'The core empirical/conceptual contest over Hebrew''s linguistic vitality history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement(hebr_be_t1000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(hebr_be_t1500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(hebr_su_t1000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(hebr_su_t1500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_linguistic_life' kernel, focusing on liturgical preservation. It stands in direct opposition to readings centered on vernacular use or marketplace function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
