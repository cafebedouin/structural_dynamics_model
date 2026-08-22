% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status — Messianic Deferral Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The messianic deferral reading of kodashim commandment status treats the
 *   sacrificial system as temporarily suspended but permanently binding.
 *   Study of kodashim (Talmudic orders, Maimonidean codes, Temple Institute
 *   activity) is framed as 'readiness maintenance' — keeping the commandment
 *   alive so it can be instantly reactivated upon Temple restoration. This
 *   reading became dominant in Religious Zionist halakha after 1967 and has
 *   structured significant resource flows: yeshiva curricula, Temple
 *   Institute operations, land-use policy, and tzedakah allocation. The
 *   constraint claims to be a scaffold (temporary support until restoration),
 *   but the sunset clause (messianic arrival) has no knowable deadline, and
 *   the extractiveness has risen steadily as institutional interests
 *   crystallized around the deferral narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.31).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status — Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '0fa1b03e-55f7-45ce-a237-26fdc7bf6c87').
narrative_ontology:cs_kernel_codification('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', formalized).
narrative_ontology:cs_authority_grounding('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', extraction).
narrative_ontology:cs_interpretation_layer_present('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87').
narrative_ontology:cs_reading_relation('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', kodashim_commandment_status__performance_only, influences).
narrative_ontology:cs_axiom('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', foundational, commandment_suspended_not_obsolete).
narrative_ontology:cs_axiom_status(commandment_suspended_not_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', commandment_suspended_not_obsolete, conventional).
narrative_ontology:cs_axiom('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', foundational, study_as_preparatory_obligation).
narrative_ontology:cs_axiom_status(study_as_preparatory_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', study_as_preparatory_obligation, conventional).
narrative_ontology:cs_axiom('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', secondary, messianic_restoration_justifies_present_resource_claim).
narrative_ontology:cs_axiom_status(messianic_restoration_justifies_present_resource_claim, holdable).
narrative_ontology:cs_axiom_grounding('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', messianic_restoration_justifies_present_resource_claim, instrumental).
narrative_ontology:cs_reference_frame('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', exilic_kodashim_preservation).
narrative_ontology:cs_drift_state('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', post_1967_sovereignty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0fa1b03e-55f7-45ce-a237-26fdc7bf6c87', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, halakhic_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_poor).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_orphans_widows).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, agricultural_workers_displaced_by_temple_priority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, temple_institute_activists).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_restoration_certainty).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, study_as_preparatory_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the interpretive framework that treats kodashim commandments as suspended-but-intact. Their authority derives from maintaining the transmission chain that links present study to future restoration. They set curricula, allocate communal resources to sacrificial study, and adjudicate disputes about what constitutes adequate preparation. Their position is fused with the tradition's self-conception — exit would mean abandoning the role that constitutes their authority.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive communal funding, prestige, and student enrollment for maintaining kodashim study programs. The messianic deferral reading justifies dedicated departments, specialized faculty, and capital campaigns for 'Temple preparation.' They shape curriculum priorities and influence halakhic discourse. Exit would mean restructuring institutional identity and losing a distinctive funding rationale, but they could pivot to other areas of Torah study.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, agenda_setter).

% Bear the opportunity cost of communal resources directed to sacrificial study and Temple-preparation infrastructure rather than immediate poverty relief, medical care, or housing. Tzedakah funds are partially allocated to yeshiva kodashim programs and Temple Institute activities. They have no voice in allocation decisions and no exit from the communal structure that determines priorities. Their needs are subordinated to a future contingency they may not live to see.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_poor, payer,
    powerless, immediate, trapped, local).

% Halakhic priority categories (orphans, widows, strangers) receive diminished communal support because resources flow to messianic-readiness institutions. The deferral reading treats present suffering as provisional — to be resolved in the messianic era — while the commandment to support the vulnerable is treated as fully operative now. They are structurally excluded from the decision calculus that ranks future preparation above present obligation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_orphans_widows, payer,
    powerless, immediate, trapped, local).

% Land-use priorities in Israel are influenced by Temple Mount advocacy and priestly-portion allocations (terumah/maaser) that presuppose restoration. Agricultural planning, water rights, and zoning reflect a future Temple order. Farmers and workers bear compliance costs and opportunity costs for a system that does not yet exist. They can sell land or change crops but cannot exit the regulatory framework that privileges Temple-orientation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, agricultural_workers_displaced_by_temple_priority, payer,
    moderate, biographical, constrained, regional).

% Operate the Temple Institute, manufacture vessels, train kohanim, and lobby for Temple Mount access. Their entire organizational mission and funding model depend on the messianic deferral reading being authoritative. They are the most visible beneficiaries of the 'readiness' narrative — it converts theological hope into concrete institutional activity. Exit would dissolve their life's work and organizational identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, temple_institute_activists, beneficiary,
    organized, generational, identity_locked, national).

% Bear the civic costs of Temple-focused policy (security arrangements, access restrictions, archaeological preservation, education funding) without sharing the theological framework. They would object to public resources serving a messianic agenda but are excluded from the halakhic conversation that legitimates it. Their exit is political (voting, advocacy, emigration) but the constraint operates in a domain that claims authority over the shared public sphere.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, secular_israeli_citizens, excluded,
    organized, biographical, mobile, national).

% Study the historical development of kodashim halakha, the rhetoric of deferral, and the sociology of Temple movements. They analyze the constraint from outside the commitment system, documenting how the reading functions to maintain authority, allocate resources, and structure communal imagination. They neither pay nor collect within the system.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, academic_talmud_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal orientation toward a future restoration by maintaining a living tradition of sacrificial law that would otherwise atrophy. Provides a shared telos that structures curriculum, resource allocation, and identity across generations. Solves the problem of how to keep a commandment alive when its material conditions are absent for millennia.
% TRANSFER_FUNCTION: Moves communal resources (funding, land, educational slots, political attention, halakhic creativity) from present-generation welfare and immediate mitzvot to messianic-preparation institutions and Temple-restoration advocacy. The transfer is justified as investment in the ultimate redemption rather than extraction from the present.
% ABSENT_VOICES: The present-generation poor, orphans, and widows who would object to resource diversion if they were halakhically empowered to weigh present obligation against future contingency. Also absent: the potential converts and marginalized Jews who experience the deferral reading as a barrier to inclusion (the messianic frame centers a particular priestly/restorationist imagination). They are structurally excluded because the halakhic conversation only recognizes voices that accept the kernel's authority.
% DISAPPEARANCE_RATIONALE: If the messianic deferral reading vanished overnight, yeshiva kodashim departments would lose their distinctive justification, Temple Institute funding would collapse, land-use priorities in Israel would shift to present-need criteria, and halakhic authorities would face a legitimacy crisis about the status of suspended commandments. The communal imagination oriented around 'readiness' would need a new organizing narrative. The world rearranges because the constraint currently structures real resource flows and institutional identities.
% FOUNDING_PROBLEM: How to maintain the integrity and transmissibility of the sacrificial commandments (kodashim) during the prolonged absence of the Temple, altar, and priestly service — without declaring them obsolete, which would fracture the Torah's unity and the covenant's permanence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the halakhic tradition itself (Rambam Hilkhot Beit HaBechirah, Ra'avad, Ritva) as a genuine dilemma of exile. However, the status is contested: the Temple Institute and messianic authorities attest the problem remains live (Temple not yet rebuilt). Critics from within the tradition (Netziv on the danger of 'study without action,' contemporary voices like R. Shagar on the ethics of deferral) and outside (academic scholars of religion, secular Israeli civil society) attest the problem has shifted — the constraint now primarily serves to legitimize resource capture and political claims rather than preserve a threatened tradition. No neutral arbiter exists; the corroboration split maps exactly onto the kernel's reading divide.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate but rising — the opportunity cost of present needs subordinated to future contingency has grown as Temple-adjacent institutions expanded. Suppression (0.31) is moderate: the constraint does not violently coerce but structurally excludes dissenting allocations through halakhic authority and communal pressure. Theater ratio (0.48) is near the pivot point: the study function is real (scholars genuinely master complex material), but a growing share of activity performs 'readiness' rather than achieving it (vessels manufactured for a Temple that cannot be built, kohanim trained for service that cannot occur). The measurement series shows extraction and theater rising together since 1967 — institutional capture of the deferral narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (halakhic authorities), the constraint is a genuine scaffold: they believe the Temple will be rebuilt and study preserves the chain. From the payer seats (present poor, orphans, displaced workers), it operates as a snare: extraction justified by an untestable future promise. The engine computes this divergence from the structural data — the claimed type (scaffold) reflects the authorities' self-understanding, while the metrics describe the payers' lived reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities and Temple Institute activists are structural beneficiaries (d ~ 0.15-0.25): they collect authority, funding, and identity from the constraint. Yeshiva institutions are mixed beneficiaries/payers (d ~ 0.4): they gain resources but bear the cost of maintaining the performance. Present-generation poor, orphans, widows, and displaced agricultural workers are targets (d ~ 0.75-0.9): they bear opportunity costs with no exit. Secular citizens are excluded (d ~ 0.6): they pay civic costs without representation. The identity_locked exit for authorities and activists reflects professional/theological fusion — their self-concept is constituted by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving kodashim integrity during exile) was live for ~1900 years. The messianic deferral reading solved it by converting performance into study. But after 1967, with Jewish sovereignty on the Temple Mount, the problem shifted: the constraint now primarily legitimates resource capture (Temple Institute, yeshiva budgets, land claims) and political maximalism. The mandatrophy is unresolved — the arrangement persists because the authority structure extracts benefit from preventing kernel revision (authority_grounding: extraction). The sunset clause (messianic arrival) functions as a perpetual deferral mechanism, not a genuine transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_falsifiability,
    'Is the messianic restoration timeline genuinely open-ended (making the scaffold''s sunset clause functionally infinite) or does the tradition contain internal criteria for recognizing restoration that could falsify the deferral?',
    'Analyze the halakhic criteria for messianic verification (Rambam Hilkhot Melakhim 11:4, Ra''avad, contemporary poskim). If criteria are specific and observationally testable, the sunset clause has teeth; if they are theological/eschatological, the scaffold is effectively permanent.',
    'If the sunset clause is functionally infinite, the constraint reclassifies from scaffold toward piton or snare — temporary support that never transitions. If falsifiable, the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timeline_falsifiability, conceptual, 'Whether the scaffold''s sunset clause is genuine or performative.').

omega_variable(
    resource_diversion_causality,
    'Do communal resources actually flow to kodashim institutions BECAUSE of the messianic deferral reading, or would they flow there anyway under other readings (e.g., study_as_performance)?',
    'Counterfactual comparison: examine communities that hold study_as_performance but not messianic_deferral (e.g., certain Haredi sectors, academic yeshivot). Measure their kodashim resource allocation. If similar, the deferral reading is not the causal driver of extraction.',
    'If extraction is reading-invariant, the constraint''s extractiveness is a property of the kernel itself (commitment-system maintenance cost), not this reading. If reading-dependent, the messianic frame specifically enables the resource capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_diversion_causality, empirical, 'Whether the measured extraction is caused by this reading or by the kernel''s structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (halakhic authority, communal enforcement, state policy) or internalized (theological self-concept that makes dissent unthinkable for payers)?',
    'Interview payers (poor, orphans, displaced workers) about their experience of the resource allocation. If they articulate the deferral rationale as legitimate, suppression is partly internalized. If they experience it as imposed but inescapable, suppression is structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the suppression after exit. This would increase computed χ for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for payer seats.').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three readings of kodashim_commandment_status disagree structurally?',
    'Map each reading''s ε, beneficiary set, victim set, and sunset condition. The disagreement is located in: (1) whether study counts as fulfillment (study_as_performance) vs. preparation (messianic_deferral) vs. nothing (performance_only); (2) whether present resource claims are justified by future contingency; (3) whether the commandment''s status changes the kernel''s authority structure.',
    'If the disagreement is only about fulfillment semantics (1), the readings may be co-instantiable. If it includes resource claims (2) and authority (3), they are structurally competing constraints over the same material referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural location of disagreement among kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1948, kodashim_commandment_status__messianic_deferral, theater_ratio, 1948, 0.22).
narrative_ontology:measurement(koda_tr_t1967, kodashim_commandment_status__messianic_deferral, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(koda_tr_t1978, kodashim_commandment_status__messianic_deferral, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(koda_tr_t1987, kodashim_commandment_status__messianic_deferral, theater_ratio, 1987, 0.41).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(koda_tr_t2010, kodashim_commandment_status__messianic_deferral, theater_ratio, 2010, 0.46).
narrative_ontology:measurement(koda_tr_t2024, kodashim_commandment_status__messianic_deferral, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(koda_be_t1948, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1948, 0.18).
narrative_ontology:measurement(koda_be_t1967, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1967, 0.25).
narrative_ontology:measurement(koda_be_t1978, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(koda_be_t1987, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1987, 0.32).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(koda_be_t2010, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement(koda_be_t2024, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1948, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(koda_su_t1967, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1967, 0.22).
narrative_ontology:measurement(koda_su_t1978, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(koda_su_t1987, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1987, 0.28).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(koda_su_t2010, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(koda_su_t2024, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2024, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, temple_mount_access_policy).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, israeli_land_use_priority_terumah_maaser).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three readings with distinct ε values and victim sets. messianic_deferral (this story) has moderate extractiveness from opportunity cost and present-generation victims. study_as_performance has low extractiveness (study fulfills, no future contingency) and minimal victims. performance_only has near-zero extractiveness (commandment inert) but creates a different exclusion dynamic (priestly identity without function). All three linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, institutional, 0.18).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, organized, 0.35).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, powerless, 0.85).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, moderate, 0.65).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
