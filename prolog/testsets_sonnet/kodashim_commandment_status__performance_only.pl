% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Sacrifice Law as Suspended Commandment Contingent on Temple/Altar Existence (Performance-Only Reading)
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the 'performance_only' reading of the
 *   kodashim_commandment_status kernel: sacrifice laws are contingent on the
 *   physical existence of the Temple and its altar. Without an altar, the
 *   commandment is not fulfilled through any substitute activity — it is
 *   suspended, a husk awaiting restoration. This is structurally distinct
 *   from the sibling reading in which study itself constitutes performance
 *   (study_as_performance), and from the reading in which the suspension is
 *   explicitly a temporally bounded deferral oriented toward messianic
 *   restoration with study as readiness-maintenance (messianic_deferral).
 *   Under performance_only, no current activity — including study —
 *   discharges the commandment; the commandment simply has no operative
 *   object and its extensive continued scholarly elaboration is not
 *   commandment-fulfillment but tradition-preservation, which is a different
 *   and more contestable claim on resources.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.66).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.42).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.66).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Sacrifice Law as Suspended Commandment Contingent on Temple/Altar Existence (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '32d1c7eb-8133-4d29-a732-154a59ec75cf').
narrative_ontology:cs_kernel_codification('32d1c7eb-8133-4d29-a732-154a59ec75cf', fixed_text).
narrative_ontology:cs_authority_grounding('32d1c7eb-8133-4d29-a732-154a59ec75cf', lineage).
narrative_ontology:cs_interpretation_layer_present('32d1c7eb-8133-4d29-a732-154a59ec75cf').
narrative_ontology:cs_reading_relation('32d1c7eb-8133-4d29-a732-154a59ec75cf', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('32d1c7eb-8133-4d29-a732-154a59ec75cf', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('32d1c7eb-8133-4d29-a732-154a59ec75cf', foundational, performance_requires_physical_altar).
narrative_ontology:cs_axiom_status(performance_requires_physical_altar, holdable).
narrative_ontology:cs_axiom_grounding('32d1c7eb-8133-4d29-a732-154a59ec75cf', performance_requires_physical_altar, conventional).
narrative_ontology:cs_axiom('32d1c7eb-8133-4d29-a732-154a59ec75cf', foundational, study_does_not_discharge_commandment).
narrative_ontology:cs_axiom_status(study_does_not_discharge_commandment, holdable).
narrative_ontology:cs_axiom_grounding('32d1c7eb-8133-4d29-a732-154a59ec75cf', study_does_not_discharge_commandment, conventional).
narrative_ontology:cs_reference_frame('32d1c7eb-8133-4d29-a732-154a59ec75cf', temple_era_sacrificial_practice).
narrative_ontology:cs_drift_state('32d1c7eb-8133-4d29-a732-154a59ec75cf', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('32d1c7eb-8133-4d29-a732-154a59ec75cf', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, kodashim_specialist_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_institutions_offering_kodashim_tracks).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, students_diverted_from_applicable_law_study).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communities_underserved_by_scholarly_labor_allocation).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, performative_conditionality_of_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have built scholarly careers, reputational standing, and teaching positions around mastery of sacrificial law under the premise that its performative force is suspended pending the altar's reconstruction. Their expertise commands respect within the tradition precisely because the material is difficult and, on this reading, currently inapplicable rather than dead. Exit from this specialization would mean abandoning years of accumulated interpretive capital with no equivalent standing elsewhere in the curriculum.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, kodashim_specialist_scholars, beneficiary,
    organized, generational, identity_locked, national).

% Set curricular emphasis and allocate teaching hours toward Kodashim study as part of the full corpus of Talmudic tractates, administering the pedagogical structure that treats sacrifice law as suspended-but-intact rather than obsolete. They collect tuition, donor support, and institutional prestige tied to comprehensive coverage of the tradition, including the temple-contingent commandments.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_institutions_offering_kodashim_tracks, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, yeshiva_institutions_offering_kodashim_tracks, beneficiary).

% Spend substantial study hours mastering sacrificial procedure that cannot currently be performed by anyone, under a curriculum that treats this as core rather than optional. Time and cognitive investment here is time not spent on directly applicable law (civil, family, dietary, ethical). Exit is constrained by institutional expectation and the social cost of appearing to slight a traditional subject.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, students_diverted_from_applicable_law_study, payer,
    moderate, biographical, constrained, local).

% Have communal and pastoral needs — practical halakhic guidance, communal governance, social welfare adjudication — that could draw on the same scholarly labor currently allocated to sacrificial law mastery. They have no direct voice in curricular allocation decisions made by yeshiva leadership and specialist faculty.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communities_underserved_by_scholarly_labor_allocation, payer,
    powerless, biographical, trapped, local).

% Hold that sacrifice law is not merely suspended but has been superseded by ethical and prayer-based worship; they are structurally outside the traditional commitment system that treats the commandment as merely dormant, and their view is not represented in the halakhic adjudication process that governs this reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, reform_and_reconstructionist_movements, excluded,
    organized, generational, mobile, national).

% Study the history and function of the performance-only doctrine comparatively — tracking how the 'husk' framing has been used across centuries to preserve textual continuity without claiming present performative obligation, and how resource allocation around it has shifted with institutional incentives.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the full textual and interpretive corpus of sacrificial law intact and transmissible, so that if the Temple were rebuilt, the tradition would not need to be reconstructed from fragments — coordinating collective memory of a suspended practice against future contingency.
% TRANSFER_FUNCTION: Moves scholarly labor-hours, institutional teaching budgets, and student cognitive investment away from currently applicable areas of law and communal need, toward mastery of a commandment whose performative object (the altar) does not exist and is not being built.
% ABSENT_VOICES: Reform and Reconstructionist voices who hold sacrifice law fully superseded rather than merely dormant are not part of the traditional adjudicating community and have no standing to redirect the resource allocation; communities with unmet practical halakhic needs have no formal channel to contest curricular emphasis set by yeshiva leadership.
% DISAPPEARANCE_RATIONALE: If the performance-only framing were abandoned in favor of treating sacrifice law as fully obsolete husk with no live commandment status at all, specialist scholars would lose the doctrinal basis for the discipline's centrality, yeshiva curricula would face pressure to reallocate hours toward applicable law, and the elaborate interpretive apparatus sustaining Kodashim's parity with practiced tractates would lose its structural justification — a genuine reorganization of scholarly labor and institutional prestige, not a cosmetic change.
% FOUNDING_PROBLEM: The Temple's destruction created a rupture: commandments that had structured Jewish worship for centuries could no longer be performed. The performance-only doctrine was built to answer a narrow legal question — does an unperformable commandment retain any halakhic status — by holding that it is suspended (contingent on the altar) rather than annulled, preserving the possibility of future restoration without claiming present obligation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish law and comparative religion scholars outside the yeshiva system attest that the doctrinal question of suspension-versus-obsolescence was live and contested for centuries and remains formally unresolved by any single authoritative body; some contemporary halakhic authorities within the tradition itself argue the doctrine has calcified into institutional self-perpetuation rather than active legal reasoning, while others outside the beneficiary set (academic Talmudists, non-Orthodox halakhists) regard the continued study investment as disproportionate to any live practical stakes.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high (0.66) and rising over the interval because continued institutional investment in Kodashim mastery, under a doctrine that denies any performative discharge is occurring, increasingly resembles resource allocation to a non-functional practice rather than active commandment-fulfillment — the coordination function (preserving the corpus) persists but is decreasingly proportionate to what it is preserving against, as messianic restoration recedes as a practical near-term expectation for most participants. Theater ratio is high and rising (0.71 at T=100) because the elaborate pilpulistic apparatus around inapplicable law increasingly serves institutional and reputational functions — credentialing, prestige hierarchies, curricular completeness signaling — rather than functional legal reasoning toward an achievable end. Suppression is moderate (0.42): there is no coercive enforcement mechanism, but institutional and social expectation constrains curricular exit for students and reputational path-dependence locks specialist scholars in.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting yeshiva institution's seat, this is a rope or scaffold — genuine preservation of a suspended-but-real obligation, transitional pending restoration. From the payer seats (diverted students, underserved communities), the same structure computes closer to a piton: a degraded function maintained by institutional inertia and theatrical intellectual maintenance, where no single party benefits enough to justify the diffuse cost, yet no party is positioned to reallocate it. The engine should register this seat divergence directly from the structural data rather than from any narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialist scholars and the yeshiva institutions are structural beneficiaries: they derive career standing, prestige, and institutional completeness from the corpus's continued centrality, and the performance_only doctrine (denying study discharges the commandment) paradoxically increases the perceived intellectual rigor demanded to engage with 'merely preservative' material, deepening the specialization's mystique. Students and underserved communities are payers: the former in diverted study-hours, the latter in foregone scholarly labor that could address live communal needs. Reform and Reconstructionist voices are excluded outright from the adjudicating community and carry no directionality within this commitment system at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what legal status does an unperformable commandment retain — was live and urgent immediately after the Temple's destruction, when restoration was plausible within a generation or two. Under the performance_only reading specifically, that founding problem has arguably gone dormant twice over: not only has the altar not been rebuilt, but this reading also denies that study itself renews any performative link to the commandment, making the doctrine's practical stakes almost entirely genealogical and institutional. Classifying this as piton rather than snare avoids treating specialist scholars as if they were extracting rents through coercive enforcement (there is none) while still registering that the arrangement persists through inertia and theatrical maintenance rather than a currently-functioning coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_versus_performance_kernel_disagreement,
    'Does intellectual engagement with sacrificial law constitute performance of the commandment (as the study_as_performance reading holds), or does the commandment remain strictly unperformed and unperformable absent the physical altar (as this performance_only reading holds)?',
    'This is not empirically resolvable — it is a live doctrinal dispute within halakhic authority itself, adjudicated by competing lines of rabbinic reasoning (e.g., readings of Talmudic statements that ''the study of the laws of sacrifice is accounted as if one had offered them'') rather than by external evidence. Different halakhic authorities and communities hold different readings simultaneously.',
    'If the study_as_performance reading is adopted instead, the entire resource-allocation critique in this story dissolves: study would BE the commandment''s discharge, not a preservative substitute for it, collapsing the coordination/extraction gap this story identifies. The two readings produce structurally different constraints with different ε profiles from the same underlying kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_versus_performance_kernel_disagreement, conceptual, 'Core kernel disagreement between performance_only and study_as_performance readings over whether study discharges the commandment.').

omega_variable(
    restoration_temporal_horizon,
    'Is the temple''s non-existence a permanent structural fact for this reading, or an indefinite-but-still-anticipated deferral, as the messianic_deferral reading holds?',
    'Would require either a communal-consensus theological event (which has not occurred) or explicit doctrinal statements distinguishing ''permanently husk until restoration, which is not the operative frame'' from ''actively deferred pending restoration, which is the operative frame'' — largely a matter of which authorities and communities one surveys.',
    'If treated as active deferral with restoration as a near-horizon expectation (messianic_deferral''s framing), the theater_ratio and extractiveness trajectory would likely be authored lower, since the preservation activity would read as readiness-maintenance for an anticipated near-term event rather than indefinite institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_temporal_horizon, conceptual, 'Whether the temporal horizon for restoration is treated as indefinite/theoretical (this reading) or actively anticipated (sibling reading).').

omega_variable(
    resource_reallocation_counterfactual,
    'If Kodashim study hours were reduced, would the freed scholarly labor actually redirect to underserved communal needs, or would it simply redistribute to other areas of the existing curriculum with no net benefit to the identified victim groups?',
    'Comparative study of yeshiva curricula that have de-emphasized Kodashim (some Modern Orthodox and academic Talmud programs) versus those that maintain full emphasis, tracking where freed instructional capacity actually goes.',
    'If reallocation is largely intra-curricular rather than toward genuinely underserved communities, the victim harm attributed to ''communities_underserved_by_scholarly_labor_allocation'' is weaker than claimed, and the constraint may be better characterized as diffuse inefficiency than targeted extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_reallocation_counterfactual, empirical, 'Whether reduced Kodashim emphasis would actually benefit the named victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.48).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.55).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__performance_only, theater_ratio, 60, 0.62).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__performance_only, theater_ratio, 80, 0.67).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__performance_only, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__performance_only, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.66).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_commandment_status kernel, decomposed per the ε-invariance principle: performance_only (this story, moderate-high and rising extractiveness, piton-leaning), study_as_performance (lower extractiveness — study itself discharges the obligation, so the resource investment is not diverted from commandment-fulfillment but IS the fulfillment), and messianic_deferral (intermediate — active readiness-maintenance for anticipated restoration tempers the theater_ratio relative to this reading). The three stories share the same underlying textual kernel (Temple-era sacrificial law) but diverge sharply in claimed coordination function and therefore in authored ε. Do not average across them; each is a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
