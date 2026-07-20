% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Mourning Obligation â Commemorative Reading (D1/D4)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   Tisha B'Av is a day of intensive mourning and restriction commemorating
 *   the destruction of the Temple. Under the mourning_practice_reading of the
 *   catastrophe_memory_function kernel, the ritual is understood as pure
 *   boundary-maintenance (D1/D4): its function is to preserve group identity
 *   and communal boundaries through memorial obligation, with no transmission
 *   of survival competence (D5). Structurally, the obligation coordinates
 *   collective memory across a diasporic community, but it also extracts
 *   observant labor and emotional conformity from members who participate
 *   under identity-lock or social pressure rather than conviction. The
 *   halakhic authority and traditionalist guardians benefit from the
 *   reinforced boundaries; obligated members bear the costs. The constraint
 *   is claimed as tangled_rope â genuine coordination function coupled with
 *   asymmetric extraction â and the metrics are authored independently of
 *   that claim.
 *
 * KEY AGENTS:
 *   - rabbinic_authority (institutional/constrained): Agenda-setter and primary beneficiary â administers the ritual norms and collects authority from their enforcement
 *   - traditionalist_guardians (organized/constrained): Beneficiary â their social world of clear boundaries depends on the ritual's performance
 *   - obligated_members (moderate/identity_locked): Payer and victim â bear the physical and emotional costs of the fast and restrictions under communal pressure
 *   - secular_assimilated (moderate/mobile): Excluded â their non-observance marks the boundary they are outside
 *   - ritual_theorist (analytical/analytical): Observer â sees the full structural picture without being subject to the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.62).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Mourning Obligation â Commemorative Reading (D1/D4)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '9a4279ea-20d9-4c02-b111-6e755baa45ef').
narrative_ontology:cs_kernel_codification('9a4279ea-20d9-4c02-b111-6e755baa45ef', fixed_text).
narrative_ontology:cs_authority_grounding('9a4279ea-20d9-4c02-b111-6e755baa45ef', lineage).
narrative_ontology:cs_interpretation_layer_present('9a4279ea-20d9-4c02-b111-6e755baa45ef').
narrative_ontology:cs_reading_relation('9a4279ea-20d9-4c02-b111-6e755baa45ef', catastrophe_memory_function__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('9a4279ea-20d9-4c02-b111-6e755baa45ef', catastrophe_memory_function__hybrid_transformation_reading, forecloses).
narrative_ontology:cs_axiom('9a4279ea-20d9-4c02-b111-6e755baa45ef', foundational, mourning_is_constitutive_not_instrumental).
narrative_ontology:cs_axiom_status(mourning_is_constitutive_not_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('9a4279ea-20d9-4c02-b111-6e755baa45ef', mourning_is_constitutive_not_instrumental, deontological).
narrative_ontology:cs_axiom('9a4279ea-20d9-4c02-b111-6e755baa45ef', foundational, catastrophe_boundaries_are_eternal).
narrative_ontology:cs_axiom_status(catastrophe_boundaries_are_eternal, holdable).
narrative_ontology:cs_axiom_grounding('9a4279ea-20d9-4c02-b111-6e755baa45ef', catastrophe_boundaries_are_eternal, theological).
narrative_ontology:cs_reference_frame('9a4279ea-20d9-4c02-b111-6e755baa45ef', classical_mourning_obligation).
narrative_ontology:cs_drift_state('9a4279ea-20d9-4c02-b111-6e755baa45ef', contemporary_diaspora_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a4279ea-20d9-4c02-b111-6e755baa45ef', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, traditionalist_guardians).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, obligated_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the halakhic norms of Tisha B'Av observance, including the fast, liturgical restrictions, and behavioral prohibitions. Their authority as legitimate interpreters of catastrophe meaning is reinforced when the community collectively observes the ritual they prescribe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Members most invested in strict boundary maintenance between Jewish and non-Jewish practice, and between traditional and assimilated Jews. They experience the ritual as producing the clear distinctions their social world depends on, and they enforce normative compliance within their networks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, traditionalist_guardians, beneficiary,
    organized, generational, constrained, global).

% Members who observe Tisha B'Av due to family pressure, marriage considerations, employment in communal institutions, or identity fusion rather than personal theological conviction. They bear the physical costs of the fast, the emotional labor of mourning, and the foregone opportunities of the restricted day, while receiving limited subjective benefit from the boundary maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, obligated_members, payer,
    moderate, biographical, identity_locked, global).

% Jews who do not observe Tisha B'Av and have largely integrated into non-Jewish or secular social frameworks. Their non-observance is used by the traditionalist boundary system to mark the limits of the communal 'we', and they are not present in halakhic deliberations about the ritual's form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_assimilated, excluded,
    moderate, biographical, mobile, global).

% Academic scholars of religion and anthropologists who study Tisha B'Av as a case of catastrophe commemoration. They observe the ritual's structural role in maintaining collective memory and group boundaries without themselves being subject to its obligations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_theorist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes collective memory of the Temple destruction across a diasporic community that lacks territorial or political unity, providing a shared temporal rhythm and behavioral repertoire that makes group identity perceptible to its members.
% TRANSFER_FUNCTION: Moves observant labor (fasting, prayer, restricted activity), emotional conformity (mourning affect), and social compliance from individual members to the communal boundary-maintenance apparatus; moves authority and social capital from the ritual performance to rabbinic interpreters and traditionalist enforcers.
% ABSENT_VOICES: Secular Jewish historians, Reform movement voices, and assimilated family members who would argue that the ritual has become an engine of exclusion rather than memory, or that catastrophe memory should be channeled through civic or national frameworks rather than halakhic obligation. They are absent from the halakhic discourse that shapes the ritual's form.
% DISAPPEARANCE_RATIONALE: If the obligation to observe Tisha B'Av disappeared, rabbinic authority over the communal calendar would weaken, the distinction between strict observance and assimilation would blur, and the primary annual occasion for collective confrontation with statelessness and loss would vanish. Collective memory would shift to Yom HaShoah, Israeli national memorial days, or secular historiography, rearranging the architecture of Jewish identity.
% FOUNDING_PROBLEM: Maintaining collective identity and cohesion after the destruction of the Second Temple and the loss of political sovereignty, when the prior mechanisms of boundary maintenance (territory, Temple cult, political autonomy) were gone.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists outside the halakhic system attest that the founding problem (preserving peoplehood in catastrophic loss) was historically urgent. However, Zionist historians and secular Jewish scholars contest that the rabbinic ritual form was the only or still-necessary solution, pointing to Israeli statehood and modern civic identity as alternative resolutions to the founding problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the ritual imposes significant physical and emotional costs (25-hour fast, work restrictions, mourning comportment) on participants, and these costs fall most heavily on identity-locked members who would prefer reduced obligation. Suppression (0.58) is moderate-to-high because compliance depends on a mix of internalized identity fusion and structural communal sanctions; non-observance within traditionalist contexts carries marriageability, employment, and social consequences. Theater_ratio (0.48) reflects the growing historical distance from the Temple destruction, which increases the performative dimension of mourning for participants who have no direct connection to the lost Temple. Accessibility_collapse (0.60) is moderate: alternatives (secularism, Reform Judaism, Israeli civic identity) are visible but psychologically foreclosed for identity-locked members. Resistance (0.42) is moderate: secularization, assimilation, and Reform movements constitute ongoing resistance, but they are largely excluded from the halakhic discourse rather than engaged as interlocutors.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic_authority seat computes the constraint as rope or scaffold â a necessary coordination mechanism preserving peoplehood across catastrophe. The obligated_members seat computes it as snare â an enforced extraction of their bodies and emotions to maintain boundaries they did not choose. The traditionalist_guardians and secular_assimilated seats produce divergent types because the former's identity is constituted by the boundary the latter refuses. The engine derives this divergence from the structural data: identical spatial scope but opposite directionality, with exit_options ranging from constrained (rabbinic authority) to identity_locked (obligated members).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic_authority sits near the beneficiary pole: they subsidize the constraint with administrative labor but capture authority and interpretive control. Traditionalist_guardians are net beneficiaries: the ritual produces the distinctions their social status requires. Obligated_members are net targets: they pay the extractive transfer of observant labor and emotional conformity without receiving boundary-maintenance benefits they value. Secular_assimilated have exited entirely and are structurally excluded rather than targeted. The directionality derivation reflects the beneficiary/victim declarations modulated by exit: identity_locked exit amplifies extraction for obligated_members, while constrained but institutional rabbinic authority sits nearer the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents the error of treating the ritual as pure coordination (rope) â which would ignore the coerced participation of identity-locked members â and also prevents treating it as pure extraction (snare) â which would deny the genuine collective-action problem of maintaining diasporic memory without territory or state. The classification captures that both dynamics operate through the same structure: the fast and restrictions simultaneously coordinate collective memory and extract from compelled participants. Were the founding problem (preserving peoplehood after Temple destruction) definitively solved by modern political arrangements, the constraint would drift toward piton; the contested status of the founding problem keeps the coordination function alive, preventing mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_vs_survival_function,
    'Does pure commemorative ritual maintain group boundaries without transmitting any survival competence, or is boundary maintenance itself a form of survival competence for a diasporic minority?',
    'Comparative historical analysis: if communities with strong boundary rituals but weak survival-competence transmission show higher continuity than those with weak boundary rituals, boundary maintenance itself may constitute survival competence.',
    'If boundary maintenance is survival competence, the mourning_practice_reading''s foreclosure of D5 fails and the constraint collapses toward the hybrid_transformation_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_vs_survival_function, conceptual, 'Whether D1/D4 boundary maintenance is separable from D5 survival competence').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the compliance pressure on obligated members structural (communal sanctions, exclusion from marriage networks) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: if members who leave traditionalist communities continue to feel guilt or compulsion to observe, suppression is partially internalized.',
    'If internalized, effective extraction exceeds the structural measure because members carry the constraint with them after apparent exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    theater_authenticity_drift,
    'As historical distance from the Temple destruction increases, does the ritual''s mourning function become predominantly performative rather than experiential?',
    'Ethnographic study of mourning affect during Tisha B''Av: measure the ratio of reported genuine grief to normative performance pressure across age cohorts and observance levels.',
    'High theater ratio would support piton reclassification; sustained authentic grief would support continued tangled_rope or rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_authenticity_drift, empirical, 'Whether ritual mourning is becoming performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three structurally distinct readings: mourning_practice_reading (pure D1/D4, denies D5), survival_competence_reading (pure D5), and hybrid_transformation_reading (D1/D4 + D5). Each reading has a different epsilon and different beneficiary/victim structure because the presence or absence of the survival-competence function changes whether the ritual extracts for adaptive transmission or for boundary maintenance alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
