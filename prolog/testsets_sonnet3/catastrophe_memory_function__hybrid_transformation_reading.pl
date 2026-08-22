% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover as Hybrid Mourning-and-Survival Ritual (Bitter Herbs + Seder Performance)
 *   domain: religious/anthropological
 *
 * SUMMARY:
 *   This story instantiates the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel: the Passover seder is read as ONE
 *   ritual structure that simultaneously performs mourning-practice (bitter
 *   herbs, unleavened bread, naming of affliction — D1/D4 content) and
 *   rehearses survival-competence (the fixed, teachable, temple-independent
 *   liturgical sequence transmissible at household scale — D5 content). The
 *   claim is that these are not two separable functions bundled by accident
 *   but one integrated structure whose dual-encoding is itself the adaptive
 *   achievement: a population without guaranteed institutional continuity
 *   needed both affect-processing and portable procedural competence, and got
 *   both from the same yearly performance. This is deliberately NOT the
 *   mourning_practice_reading (which would treat the survival-competence
 *   content as incidental to identity-maintenance) and NOT the
 *   survival_competence_reading (which would treat the mourning content as
 *   incidental to transformation-training) — each of those is a distinct
 *   sibling constraint with its own ε and its own file.
 *
 * KEY AGENTS:
 *   - diaspora_community_members: primary participants and beneficiaries of both encoded functions
 *   - household_ritual_leaders: agenda-setters who administer the fixed liturgical structure at household scale
 *   - rabbinic_interpretive_authorities: institutional beneficiaries who codify and interpret the balance of mourning and survival content across eras
 *   - children_and_new_generations: structurally centered addressees (Four Questions) who receive the transmission without having chosen it
 *   - comparative_ritual_scholars: analytical observers testing whether the dual-function reading is well-supported or over-fit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover as Hybrid Mourning-and-Survival Ritual (Bitter Herbs + Seder Performance)").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/anthropological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'e443e23d-df0c-41db-926e-542e736298ea').
narrative_ontology:cs_kernel_codification('e443e23d-df0c-41db-926e-542e736298ea', fixed_text).
narrative_ontology:cs_authority_grounding('e443e23d-df0c-41db-926e-542e736298ea', lineage).
narrative_ontology:cs_interpretation_layer_present('e443e23d-df0c-41db-926e-542e736298ea').
narrative_ontology:cs_reading_relation('e443e23d-df0c-41db-926e-542e736298ea', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('e443e23d-df0c-41db-926e-542e736298ea', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('e443e23d-df0c-41db-926e-542e736298ea', foundational, dual_function_non_separability).
narrative_ontology:cs_axiom_status(dual_function_non_separability, holdable).
narrative_ontology:cs_axiom_grounding('e443e23d-df0c-41db-926e-542e736298ea', dual_function_non_separability, empirically_contingent).
narrative_ontology:cs_axiom('e443e23d-df0c-41db-926e-542e736298ea', secondary, integrated_transmission_superior_to_isolated_function).
narrative_ontology:cs_axiom_status(integrated_transmission_superior_to_isolated_function, holdable).
narrative_ontology:cs_axiom_grounding('e443e23d-df0c-41db-926e-542e736298ea', integrated_transmission_superior_to_isolated_function, instrumental).
narrative_ontology:cs_reference_frame('e443e23d-df0c-41db-926e-542e736298ea', dual_encoded_haggadah_transmission).
narrative_ontology:cs_drift_state('e443e23d-df0c-41db-926e-542e736298ea', contemporary_diaspora_stability, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e443e23d-df0c-41db-926e-542e736298ea', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, children_and_new_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_dual_encoding_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the seder as both mourners recalling bondage (bitter herbs, unleavened bread as haste-and-privation markers) and as rehearsers of a transmissible survival script (the ordered retelling, the structured questions, the adaptable household-scale performance that requires no central temple or priesthood). They receive both an affect-processing function and a portable competence that has let the practice persist across expulsions and migrations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_community_members, beneficiary,
    moderate, generational, constrained, global).

% Conduct the seder in the home rather than a central institution, adapting the fixed liturgical skeleton to local circumstance. They administer both the mourning content (naming the affliction, eating the bitter herb) and the competence content (leading the structured, teachable sequence that non-specialists can reproduce). Their authority is real but bounded by the fixed text they transmit rather than author.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders, agenda_setter,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, household_ritual_leaders, beneficiary).

% Codify and interpret the Haggadah text across centuries, adjudicating how much emphasis mourning-elements versus survival-elements receive in a given era. They benefit from the ritual's continued authority and from being positioned as its legitimate interpreters, but do not directly administer individual households' observance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_authorities, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_interpretive_authorities, observer).

% Are structurally positioned as the ritual's primary addressees (the Four Questions are asked BY the youngest present) — they receive both the transmitted memory of catastrophe and the rehearsed competence of decentralized, replicable practice, without having chosen to be enrolled in either function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, children_and_new_generations, beneficiary,
    powerless, biographical, trapped, local).

% Would question whether the dual-function reading over-integrates two things that could be separated — mourning without the survival-training apparatus, or vice versa — but their skepticism about the hybrid frame is rarely voiced inside the ritual space itself; they are more likely to simply not attend.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, assimilationist_or_secular_descendants, excluded,
    moderate, biographical, mobile, national).

% Study the Passover seder as a test case for whether commemorative ritual can simultaneously encode grief-processing and adaptive-capacity transmission within one structure, or whether such readings over-fit a single tradition to a general theory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves two coordination problems at once with one shared structure: it gives a dispersed population a repeatable, low-infrastructure way to process collective catastrophic memory (the bitter herb, the naming of affliction) AND a repeatable, low-infrastructure way to train each new household in the adaptive competence of decentralized, portable practice (the ordered seder liturgy, teachable without a temple or centralized clergy) — a competence that historically mattered because the population could not assume continuity of any fixed institutional seat.
% TRANSFER_FUNCTION: Moves affective and procedural inheritance across generations: mourning-content moves from the historical catastrophe into present affect (guilt, grief, gratitude); survival-content moves procedural competence (how to reconstitute communal practice with minimal institutional scaffolding) from elders to the youngest present, structurally centered via the Four Questions.
% ABSENT_VOICES: Assimilationist or secular descendants who might argue the coupling of mourning and survival-training over-determines a single reading of the ritual's function are rarely present to register the objection inside the ritual space; their dissent shows up as non-participation rather than argument.
% DISAPPEARANCE_RATIONALE: Practitioners and rabbinic authorities would say the world rearranges substantially — both the affective processing of collective memory and the transmissible template for decentralized continuity would need to be reconstructed from scratch, likely imperfectly. Secular or purely-analytical observers would say a functionally equivalent memorial or civic-education practice could emerge to fill either function separately, so the world would adapt rather than rupture. The dispute is genuine and unresolved between these seats.
% FOUNDING_PROBLEM: A population needed both to process an experienced catastrophe (enslavement, dispossession, forced departure) without institutional continuity guaranteed, and to build in advance a repeatable competence for reconstituting communal life and practice under conditions of future dispersal, exile, or institutional loss.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish diaspora communities (writing from outside rabbinic institutional interest) corroborate that the seder's household-portable, temple-independent structure functioned as genuine adaptive infrastructure during periods of exile and institutional destruction — this is attested in comparative religious-history scholarship, not only by rabbinic sources with an interest in the ritual's continued centrality.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.28) because the ritual's costs are largely the ordinary costs of any sustained cultural practice (time, dietary constraint, generational obligation) rather than transfer to an extractive party — no beneficiary group here structurally profits at another's expense; the coordination is real on both the mourning-side and the competence-side. Suppression is low (0.22): non-participation carries social cost but not coercive enforcement. Theater ratio is moderate and rising over the measured interval (0.20 to 0.40) reflecting an honest observation that as institutional Judaism stabilized in many diaspora contexts, some of the original survival-competence urgency (rehearsing decentralized continuity under active threat of institutional destruction) has partially given way to more performative repetition of the same forms — the mourning content remains vivid, but the survival-training content is exercised less as a live necessity and more as inherited form in stable contexts. This is descriptively distinct from the claimed_type (rope): the ritual is claimed as functioning coordination, and the metrics support that claim while flagging a modest, honestly-authored theater drift.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authorities and elder practitioners experience the ritual as a coherent, continuously vital dual-function structure; comparative scholars and secular descendants are more likely to see the survival-competence layer as historically real but currently vestigial in stable diaspora contexts, with the mourning-layer doing most of the present-day structural work. This gap is exactly what the theater_ratio drift is tracking.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder here is structurally positioned as a target extracting a transfer from another — beneficiaries include the practicing community, its household leaders, and its interpretive authorities, with no declared victim group, consistent with the low extractiveness score. Children are listed as beneficiaries despite lacking exit options because the transmission functions (both mourning and competence) are the thing being conferred on them, not a cost extracted from them; their trapped exit_options reflects the involuntariness of enrollment in a communal practice, not exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists the mandatrophy trap in a specific way: if only the mourning function were named, one could argue the ritual's founding problem is dead in contexts of political safety (mislabeling ongoing survival-training as inert commemoration). If only the survival-competence function were named, one could argue the mourning content is decorative residue on a training exercise. Naming both prevents either half from being discarded as obsolete while the other persists unexamined — the founding_problem_status is authored as contested precisely because different seats corroborate different halves of the founding problem as still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_decomposable_function,
    'Is the dual mourning-and-survival encoding a genuinely integrated structural achievement (removing either component would degrade the other), or are the two functions merely co-located in one performance and separable without loss?',
    'Comparative ritual analysis: identify communities or historical periods where one function (mourning-only or competence-only Passover variants) was practiced in isolation, and assess whether the isolated version preserved comparable continuity and affect-processing outcomes to the hybrid version.',
    'If separable without loss, this hybrid_transformation_reading overclaims integration and should be treated as a weaker, more contestable reading than either sibling; if genuinely non-separable, the hybrid reading captures something the two sibling readings each individually miss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_decomposable_function, conceptual, 'Whether the mourning and survival-competence functions are structurally integrated or merely co-located.').

omega_variable(
    theater_drift_locus,
    'Where specifically is the rising theater_ratio located — in the mourning-content (grief performance becoming rote) or in the survival-competence content (the decentralized-continuity training becoming vestigial because institutional stability removed its practical urgency), or both equally?',
    'Ethnographic comparison of seder practice in communities under active institutional threat versus communities in long-term political stability, tracking which ritual elements retain functional urgency versus become purely repeated form in each context.',
    'If theater drift concentrates in the survival-competence layer specifically, this reading''s claim that BOTH functions remain live is weakened in stable contexts, pushing the constraint toward the mourning_practice_reading''s profile in those contexts specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_drift_locus, empirical, 'Locating which encoded function is driving the observed theater-ratio increase.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to read Passover as ONE hybrid-function ritual (rather than as two ritual layers historically merged, or as the mourning_practice_reading''s or survival_competence_reading''s single-function accounts) itself defensible, or does it reflect a scholarly preference for integrative narratives over decomposition?',
    'None fully available; this is a framing-level ambiguity rather than an empirically resolvable one. Textual-historical layering analysis of the Haggadah''s redaction history could partially inform which elements were added for which purpose and when, but would not settle whether the resulting whole should be read as integrated or composite.',
    'If the composite framing is more defensible, the hybrid_transformation_reading and its siblings are better understood as describing sequential historical layers rather than three live simultaneous readings of one unified structure — this would not eliminate the kernel but would change how its readings relate (more like `influences` chains than `coexists_with` triads).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the hybrid framing is a defensible integrative reading or reflects a preference for narrative integration over historical decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__hybrid_transformation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'what the Passover ritual preserves' per the epsilon-invariance principle. hybrid_transformation_reading (this file) claims the mourning and survival-competence functions are structurally integrated in one ritual form. mourning_practice_reading claims the ritual's primary function is D1/D4 memorial-identity maintenance. survival_competence_reading claims the ritual's primary function is D5 adaptive-capacity transmission. Each carries its own epsilon and stakeholder structure; none is a measurement-basis variant of the others — they are three distinct structural claims about the same underlying practice, linked here via network.affects_constraints per the kernel-reading protocol.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
