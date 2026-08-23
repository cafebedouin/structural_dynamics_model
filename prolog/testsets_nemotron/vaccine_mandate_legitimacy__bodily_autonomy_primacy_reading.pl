% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy: Absolute Medical Self-Sovereignty Against State Coercion
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primacy_reading of
 *   the vaccine_mandate_legitimacy kernel. The reading holds that medical
 *   self-sovereignty is absolute: the state may not coerce medical
 *   intervention regardless of epidemiological outcome. The constraint is
 *   claimed as a Mountain (natural law, emerges naturally, near-zero
 *   extraction). However, the reading declares identifiable beneficiaries
 *   (liberty advocacy movements) and victims (immunocompromised individuals
 *   who bear elevated exposure risk when mandates are prohibited). This
 *   beneficiary/victim structure on a claimed Mountain triggers False Summit
 *   Mountain (FSM) evaluation. The measurement series spans Jacobson v.
 *   Massachusetts (1905) through the COVID-19 mandate era, showing stable
 *   near-zero extractiveness but rising theater_ratio as the absolute framing
 *   becomes a performative liberty signal.
 *
 * KEY AGENTS:
 *   - liberty_advocacy_movements: Primary beneficiary (institutional/organized) — organizes around absolute autonomy framing, gains membership and funding
 *   - civil_liberties_organizations: Beneficiary (institutional) — litigates and lobbies on absolute autonomy grounds
 *   - bodily_autonomy_advocates: Beneficiary (organized) — grassroots mobilization around the absolute principle
 *   - immunocompromised_individuals: Primary victim (powerless/moderate) — bear involuntary infection risk when mandates are blocked; identity_locked exit (cannot exit immunocompromised status)
 *   - high_exposure_vulnerable_populations: Victim (powerless) — elderly, congregate-living, occupational exposure groups who cannot avoid risk when community protection is foregone; constrained exit
 *   - public_health_authorities: Excluded (institutional) — would implement mandates for collective protection but are constrained by this reading's dominance in courts/legislatures
 *   - analytical_observer: Observer (analytical) — sees full structure including kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.05).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.1).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy: Absolute Medical Self-Sovereignty Against State Coercion").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e').
narrative_ontology:cs_kernel_codification('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', formalized).
narrative_ontology:cs_authority_grounding('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', lineage).
narrative_ontology:cs_interpretation_layer_present('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e').
narrative_ontology:cs_reading_relation('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', foundational, bodily_integrity_absolute_against_state).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute_against_state, holdable).
narrative_ontology:cs_axiom_grounding('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', bodily_integrity_absolute_against_state, deontological).
narrative_ontology:cs_axiom('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', pre_jacobson_state_violence).
narrative_ontology:cs_drift_state('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', post_covid_mandate_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('ec957bd3-9f63-4b5b-bed1-9ec52b83fc6e', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, civil_liberties_organizations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_autonomy_advocates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_exposure_vulnerable_populations).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_self_sovereignty_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, anti_coercion_absolute_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National and state-level organizations that litigate, lobby, and mobilize around absolute bodily autonomy. They gain members, donations, and political influence from the absolute framing. If the constraint changed (mandates permitted), they would pivot to other liberty issues — their exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, arbitrage, national).

% Established institutional actors (ACLU, state affiliates) that bring constitutional challenges to mandates on bodily integrity grounds. They benefit from the absolute framing as a clear bright-line rule that simplifies litigation. Their organizational mission ensures continued relevance across issues — high exit optionality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, civil_liberties_organizations, beneficiary,
    institutional, generational, arbitrage, national).

% Grassroots activists and organizations centered on medical freedom, parental rights, and anti-mandate mobilization. They gain community, identity, and political voice from the absolute principle. Some are identity_locked (the cause constitutes their self-concept), but the movement as a whole has mobile exit — participants can leave without losing their livelihood or identity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_autonomy_advocates, beneficiary,
    organized, biographical, mobile, national).

% People with compromised immune systems (transplant recipients, chemotherapy patients, primary immunodeficiencies) who cannot mount vaccine protection and depend on community immunity. When absolute autonomy blocks mandates, their exposure risk rises involuntarily. They cannot exit their medical condition — identity_locked exit. They bear the epidemiologic cost of the liberty coordination.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, biographical, identity_locked, national).

% Elderly in congregate care, essential workers in high-density settings, immunocompromised-adjacent household members. They bear elevated risk when community protection drops. Some exit options exist (job change, relocation) but at high personal cost — constrained exit. They are not organized as a political bloc and lack institutional voice in the mandate debate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_exposure_vulnerable_populations, payer,
    powerless, biographical, constrained, national).

% State and local health departments, CDC, WHO — the institutions that would implement mandates if legally permitted. They are structurally excluded from the constraint's operation: the absolute autonomy reading removes their mandate authority. They analyze from outside the constraint's coordination function, advocating for tools the constraint forbids.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, excluded,
    institutional, generational, analytical, national).

% The indexical classification seat: sees the kernel contest, the three readings' different victim/beneficiary structures, and the FSM tension in the absolute autonomy reading. Does not collect from or pay into any reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a liberty advocacy coalition around a bright-line absolute principle: medical decisions are categorically beyond state reach. This solves the movement's coordination problem by providing a non-negotiable anchor that prevents fracturing into risk-stratified or proportionality-based positions.
% TRANSFER_FUNCTION: Transfers epidemiologic risk from the collective (which would bear it under mandate regimes) to the immunocompromised and high-exposure vulnerable (who bear it when mandates are prohibited). Transfers political capital, litigation wins, and membership revenue to liberty advocacy movements.
% ABSENT_VOICES: Immunocompromised individuals and high-exposure vulnerable populations are structurally excluded from the constraint's authorizing coalition. They would object to bearing involuntary risk for a liberty principle they do not share, but they lack organized representation in the courts and legislatures where the absolute autonomy reading is adjudicated. Public health authorities are also excluded — they would implement mandates but are blocked by the constraint.
% DISAPPEARANCE_RATIONALE: If the absolute autonomy constraint vanished overnight (mandates became legally permissible under Jacobson necessity/proportionality), states would implement targeted mandates for high-transmission settings, community protection would rise, immunocompromised risk would fall measurably, and liberty advocacy movements would lose their bright-line anchor and fracture into proportionality debates — the world rearranges.
% FOUNDING_PROBLEM: Pre-1905: state power to forcibly vaccinate with unsafe early vaccines, no due process, no medical exemption, enforced by police power during smallpox outbreaks. The absolute autonomy principle was built to prevent state violence against bodies under the guise of public health.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Gostin, Jacobson scholars) attest the founding problem is dead: modern vaccines are safe, medical exemptions are standard, due process is required, and Jacobson already incorporates necessity/proportionality. Liberty advocacy movements attest the problem is live (citing COVID mandates as evidence of state overreach). The corroboration from outside the beneficiary set (legal historians, public health scholars) supports 'dead'; the beneficiary set's self-attestation supports 'live'. The mismatch (status=dead, verdict=world_rearranges) is a mandatrophy signal.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is near-zero (0.05) because the constraint, on its own terms, refrains from action — it prohibits state coercion rather than extracting resources. Suppression is low (0.1) because the constraint operates by removing state power, not by suppressing alternatives. Theater_ratio is low but rising (0.08 at 2024) because the absolute framing increasingly functions as a political identity marker rather than a settled legal principle. Accessibility_collapse is high (0.9) — if bodily autonomy is a natural law, alternatives (mandates) are not merely dispreferred but categorically impermissible. Resistance is near-zero (0.05) — the constraint meets little active resistance because it is a negative right (freedom from interference). The FSM-relevant tension: beneficiaries are identifiable organized movements; victims are identifiable vulnerable populations. The constraint coordinates liberty advocacy while the immunocompromised bear the epidemiologic cost of that coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the liberty advocacy seat (beneficiary, organized, arbitrage exit), the constraint is a Mountain — a natural law protecting the inviolable body. From the immunocompromised seat (victim, powerless, identity_locked exit), the same constraint operates as a Snare — it extracts their safety for a coordination function (liberty signaling) they do not benefit from and cannot exit. The engine computes this seat divergence from the structural data: same constraint, opposite effective extraction signs.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements and civil liberties organizations are declared beneficiaries — they collect political capital, litigation victories, and membership growth from the absolute autonomy framing. Their exit is arbitrage (can shift to other issues). Immunocompromised individuals and high-exposure vulnerable populations are declared victims — they bear involuntary infection risk that would be reduced if mandates were permitted. Their exit is identity_locked (immunocompromised) or constrained (occupational/congregate). Public health authorities are excluded — they would act differently but are structurally blocked. The analytical observer sees the full kernel contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-1905): state power to forcibly vaccinate during smallpox outbreaks with unsafe vaccines and no due process. That problem is substantially dead (vaccines are safe, due process exists, Jacobson standard permits mandates only with necessity/proportionality). Yet the absolute autonomy reading persists and has intensified post-2020. This is mandatrophy: the arrangement (absolute prohibition) has outlived its founding justification. The reading's persistence is not coordination for a live problem but extraction of political identity from a settled danger. The FSM mechanism captures this: a Mountain claim with beneficiaries and victims signals a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is absolute bodily autonomy a genuine natural law constraint (Mountain) or a constructed right that benefits identifiable advocacy movements (false summit)?',
    'Cross-cultural and historical survey of whether any society has ever sustained absolute medical self-sovereignty without exception; analysis of whether the ''absolute'' framing emerges only when specific political movements organize around it.',
    'If constructed, the constraint is a false summit Mountain (FSM) that reclassifies as Tangled Rope: it coordinates liberty advocacy (beneficiary) while extracting risk-bearing from immunocompromised (victim). If genuine natural law, Mountain certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Whether absolute bodily autonomy is a natural-law Mountain or a constructed constraint with identifiable beneficiaries and victims').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of vaccine_mandate_legitimacy kernel structurally disagree — on the axiom, the victim set, the beneficiary structure, or the coordination function?',
    'Structural decomposition of each reading''s constraint story: compare ε referents, beneficiary/victim declarations, and coordination/transfer function statements across bodily_autonomy_primacy, public_health_primacy, and risk_stratification readings.',
    'Clarifies whether the kernel contest is a genuine multi-constraint family (different ε, different victims) or a single constraint with observer-relative classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement across the three kernel readings').

omega_variable(
    immunocompromised_victim_status_under_absolute_autonomy,
    'Are immunocompromised individuals genuine victims of the absolute autonomy constraint (bearing involuntary risk), or does the constraint''s Mountain status mean no one is a victim because the constraint is not ''doing'' anything — it merely refrains?',
    'Counterfactual: if the absolute autonomy constraint were removed (mandates permitted), would immunocompromised risk decrease measurably? If yes, the constraint''s operation causally contributes to their risk — victim status follows. If no, victim declaration may be analytical projection.',
    'Determines whether victim declarations on a Mountain are analytically valid or category errors; affects FSM evaluation and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_status_under_absolute_autonomy, empirical, 'Whether immunocompromised bear extractive cost from the absolute autonomy constraint or whether victim declaration misapplies extraction logic to a non-extractive Mountain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t1905, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 1905, 0.02).
narrative_ontology:measurement(vacc_tr_t1970, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(vacc_tr_t2000, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2020, 0.07).
narrative_ontology:measurement(vacc_tr_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(vacc_be_t1905, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 1905, 0.03).
narrative_ontology:measurement(vacc_be_t1970, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 1970, 0.04).
narrative_ontology:measurement(vacc_be_t2000, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2000, 0.04).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2020, 0.05).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t1905, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 1905, 0.05).
narrative_ontology:measurement(vacc_su_t1970, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(vacc_su_t2000, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2000, 0.06).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2020, 0.08).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.08).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% vaccine_mandate_legitimacy kernel decomposes into three constraint stories with different ε values and victim/beneficiary structures. bodily_autonomy_primacy: ε≈0.05 (Mountain claimed, FSM candidate), beneficiaries=liberty_advocacy, victims=immunocompromised. public_health_primacy: ε≈0.35 (Tangled Rope), beneficiaries=collective_public_health, victims=unvaccinated_free_riders. risk_stratification: ε≈0.15 (Scaffold/Rope), beneficiaries=vulnerable_at_threshold, victims=those_above_threshold_subject_to_mandate. The ε-invariance principle requires separate stories: each reading instantiates a different constraint with different structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, powerless, 0.95).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, organized, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
