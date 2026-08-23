% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Liturgical Preservation Constitutes Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the liturgical_reading of the
 *   hebrew_vitality kernel. The reading asserts that unbroken liturgical use
 *   of Hebrew — sustained through rabbinic transmission, communal recitation,
 *   and textual fidelity — is not merely preservation but constitutes
 *   language vitality itself. No vernacular generation is required; the
 *   ritual register carries the full weight of continuity. The reading
 *   identifies rabbinic authorities as beneficiaries (their custodial
 *   authority is validated) and declares no victim set (the arrangement
 *   imposes no cost, only coordinates). Metrics reflect the reading's
 *   self-presentation: low extractiveness, low suppression, high
 *   accessibility collapse (alternatives are conceptually unavailable within
 *   the framework), low resistance. The claimed type is mountain — the
 *   reading presents this as a natural law of language vitality for a
 *   covenantal people.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Preservation Constitutes Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'a844d281-0bd1-448a-8c0c-0853f3e688bf').
narrative_ontology:cs_kernel_codification('a844d281-0bd1-448a-8c0c-0853f3e688bf', formalized).
narrative_ontology:cs_authority_grounding('a844d281-0bd1-448a-8c0c-0853f3e688bf', lineage).
narrative_ontology:cs_interpretation_layer_present('a844d281-0bd1-448a-8c0c-0853f3e688bf').
narrative_ontology:cs_reading_relation('a844d281-0bd1-448a-8c0c-0853f3e688bf', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('a844d281-0bd1-448a-8c0c-0853f3e688bf', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a844d281-0bd1-448a-8c0c-0853f3e688bf', foundational, liturgical_continuity_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a844d281-0bd1-448a-8c0c-0853f3e688bf', liturgical_continuity_constitutes_vitality, theological).
narrative_ontology:cs_axiom('a844d281-0bd1-448a-8c0c-0853f3e688bf', secondary, rabbinic_stewardship_authenticates_transmission).
narrative_ontology:cs_axiom_status(rabbinic_stewardship_authenticates_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a844d281-0bd1-448a-8c0c-0853f3e688bf', rabbinic_stewardship_authenticates_transmission, theological).
narrative_ontology:cs_reference_frame('a844d281-0bd1-448a-8c0c-0853f3e688bf', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('a844d281-0bd1-448a-8c0c-0853f3e688bf', contemporary_vernacular_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a844d281-0bd1-448a-8c0c-0853f3e688bf', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_congregants).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, liturgical_continuity_equals_vitality).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, unbroken_ritual_use_sustains_language).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, vernacular_generation_not_required_for_vitality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain liturgical authority through unbroken chain of ritual transmission; their interpretive monopoly over Hebrew's sacred register is validated by the claim that liturgical continuity itself constitutes language vitality. Exit would mean relinquishing the defining ground of their authority.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% Participate in ritual Hebrew as primary site of communal belonging; the liturgy provides accessible, structured engagement with the language without requiring vernacular fluency. Exit options limited by communal embeddedness and identity fusion with ritual practice.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_congregants, beneficiary,
    organized, biographical, constrained, global).

% Argue that vitality requires native intergenerational transmission in daily life; they view liturgical Hebrew as preservation, not life. Their voice is excluded from the liturgical reading's framework, which defines vitality so as to render vernacular revival unnecessary.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, native_daily_advocates, excluded,
    organized, biographical, mobile, national).

% Hold that liturgical substrate was necessary but insufficient without deliberate reconstruction; they are excluded because the liturgical reading treats the substrate as the whole of vitality, leaving no structural space for the reconstruction phase as a distinct, necessary contribution.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, hybrid_continuity_scholars, excluded,
    moderate, biographical, mobile, global).

% Analyze the constraint from outside the commitment framework; they document the empirical outcomes of each reading's operationalization without adopting any reading's internal criteria for vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish peoplehood across diaspora and time through a shared, stable liturgical register that requires no vernacular fluency to participate; solves the problem of maintaining a unified textual and ritual tradition without a shared spoken language.
% TRANSFER_FUNCTION: Moves interpretive authority and authenticity from the historical textual tradition to the present community via rabbinic stewardship; the community receives validated access to the tradition, rabbinic authorities receive legitimated custodianship.
% ABSENT_VOICES: Native daily use advocates (native_daily_reading) and hybrid continuity scholars (hybrid_continuity_reading) are structurally excluded; the liturgical reading's definition of vitality as ritual continuity renders their criteria (vernacular generation, reconstruction) irrelevant by definition, not by engagement.
% DISAPPEARANCE_RATIONALE: If the conviction that liturgical continuity constitutes vitality vanished, the primary legitimation for maintaining Hebrew as a solely ritual language would collapse; communal resources would shift toward vernacular acquisition or the tradition would fragment into disconnected local practices.
% FOUNDING_PROBLEM: Preserving Jewish textual continuity, ritual unity, and peoplehood across two millennia of exile, dispersion, and absence of a territorial center where Hebrew could be spoken natively.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by independent historical scholarship on Jewish survival (e.g., Simon Rawidowicz, Salo Baron) and by the documentary record of liturgical standardization (e.g., the Geonic and Rishonic periods) — sources outside the rabbinic beneficiary set confirm the historical exigency of a portable, text-anchored unity.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint operates only in the ritual domain and the reading claims no transfer of resources from non-participants; suppression is low (0.10) because the reading presents participation as voluntary communal embrace, not coercion; theater_ratio is low (0.12) because the ritual function is genuine and continuous, not performative. Accessibility_collapse is high (0.75) because within the reading's framework, the concept of vitality without liturgical continuity is unintelligible — alternatives collapse at the definitional level. Resistance is low (0.15) because the reading's internal logic treats dissent as category error, not opposition.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences this as mountain (natural law of covenantal continuity); the excluded seats experience it as snare (a definition that erases their criteria for vitality). The engine computes this divergence from the structural data — the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities sit at the beneficiary pole (d near 0): they collect authority, legitimacy, and institutional continuity from the constraint. Liturgical congregants are near-symmetric (d ~ 0.5): they receive identity and participation, bear minimal cost. Native_daily_advocates and hybrid_continuity_scholars are excluded (not assigned directionality) — they are not seats within this reading's framework. The engine will compute per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (portable unity without territory) remains live; the constraint has not outlived its function. Mandatrophy is not resolved — the arrangement continues to solve the problem it was built for, per this reading's lights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_vitality,
    'Is the equation of liturgical continuity with language vitality a natural law of covenantal peoples, or a constructed claim that benefits rabbinic authority by defining vitality so as to require their custodianship?',
    'Comparative analysis of other diasporic traditions with liturgical-but-not-vernacular languages (e.g., Sanskrit, Classical Arabic, Ge''ez) to test whether ritual continuity alone sustains communal vitality across millennia without vernacular transmission.',
    'If constructed, the mountain claim is a false summit (FSM) — the constraint would reclassify as tangled_rope (coordination + asymmetric extraction benefiting rabbinic authorities). If natural, the mountain classification holds and the beneficiary declaration reflects a real structural alignment, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_vitality, conceptual, 'Whether the mountain claim survives ε-invariance or is a false summit benefiting rabbinic authorities.').

omega_variable(
    committer_structure_liturgical_reading,
    'How does this reading''s structural profile (low ε, beneficiary=rabbinic_authorities, no victims) differ from its siblings, and where is the disagreement located?',
    'Structural comparison of all three readings'' beneficiary/victim declarations, ε values, and claimed types; the disagreement is located in the definition of vitality itself (what counts as life vs. preservation) and the consequent victim set.',
    'Clarifies that the kernel hebrew_vitality is not a single constraint but a family of three structurally distinct constraints linked by network.affects_constraints. Prevents averaging ε across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_liturgical_reading, conceptual, 'Commitment-system framing: this reading instantiates one constraint from a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression measured here structural (genuinely voluntary participation) or internalized (communal identity fused with ritual practice such that exit is unthinkable)?',
    'Post-exit trajectory study: track individuals who leave liturgical communities — does the sense of vitality loss persist after structural barriers are removed, indicating internalized suppression?',
    'If internalized, effective suppression is higher than the structural measure; the constraint operates as identity_locked coordination with covert extraction of autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-coordination constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t500, hebrew_vitality__liturgical_reading, theater_ratio, 500, 0.06).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t1000, hebrew_vitality__liturgical_reading, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t1500, hebrew_vitality__liturgical_reading, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t2000, hebrew_vitality__liturgical_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t3000, hebrew_vitality__liturgical_reading, theater_ratio, 3000, 0.12).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t500, hebrew_vitality__liturgical_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t1000, hebrew_vitality__liturgical_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t1500, hebrew_vitality__liturgical_reading, base_extractiveness, 1500, 0.13).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t2000, hebrew_vitality__liturgical_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t3000, hebrew_vitality__liturgical_reading, base_extractiveness, 3000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t500, hebrew_vitality__liturgical_reading, suppression_requirement, 500, 0.06).
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t1000, hebrew_vitality__liturgical_reading, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t1500, hebrew_vitality__liturgical_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t2000, hebrew_vitality__liturgical_reading, suppression_requirement, 2000, 0.09).
narrative_ontology:measurement(hebrew_vitality_liturgical_su_t3000, hebrew_vitality__liturgical_reading, suppression_requirement, 3000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the hebrew_vitality constraint family (kernel_id: hebrew_vitality). The three readings decompose the colloquial label 'Hebrew vitality' into structurally distinct claims: (1) liturgical_reading — ritual continuity constitutes vitality (mountain, low ε, beneficiary=rabbinic_authorities, no victims); (2) native_daily_reading — only native generation constitutes vitality (tangled_rope or snare, higher ε, victims=non-native communities); (3) hybrid_continuity_reading — liturgical substrate necessary but insufficient (tangled_rope, moderate ε, beneficiaries=rabbinic_authorities as substrate-providers, victims=generations awaiting reconstruction). ε values differ because each reading defines the referent arrangement differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
