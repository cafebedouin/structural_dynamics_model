% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation in Messianic Suspension: Study as Readiness Maintenance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, the biblical commandment
 *   of animal sacrifice could no longer be performed at its ordained site.
 *   The messianic_suspension reading holds that the obligation is neither
 *   fulfilled nor violated but remains in a state of suspension, with textual
 *   study serving as a maintenance protocol that preserves communal readiness
 *   for reactivation upon messianic restoration. This constraint story models
 *   that reading as a coordination mechanism whose cost is a moderate
 *   readiness burden borne by diaspora communities, extracting no current
 *   victims but generating institutional authority and identity continuity.
 *
 * KEY AGENTS:
 *   - rabbinic_authority (agenda_setter/institutional/identity_locked) â administers the suspension framework and derives legitimacy from it
 *   - diaspora_communities (beneficiary/organized/identity_locked) â bear the study burden and receive continuity
 *   - temple_restoration_advocates (excluded/moderate/constrained) â reject deferral, seek immediate performance
 *   - academic_textual_critics (observer/analytical) â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.22).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation in Messianic Suspension: Study as Readiness Maintenance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'ceba80e0-00d1-4326-a2e8-fec0e4c75cb2').
narrative_ontology:cs_kernel_codification('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', fixed_text).
narrative_ontology:cs_authority_grounding('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', lineage).
narrative_ontology:cs_interpretation_layer_present('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2').
narrative_ontology:cs_reading_relation('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', sacrifice_obligation_continuity__study_as_performance, influences).
narrative_ontology:cs_axiom('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', foundational, obligation_suspended_pending_messiah).
narrative_ontology:cs_axiom_status(obligation_suspended_pending_messiah, holdable).
narrative_ontology:cs_axiom_grounding('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', obligation_suspended_pending_messiah, theological).
narrative_ontology:cs_axiom('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', foundational, textual_study_preserves_ritual_capacity).
narrative_ontology:cs_axiom_status(textual_study_preserves_ritual_capacity, holdable).
narrative_ontology:cs_axiom_grounding('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', textual_study_preserves_ritual_capacity, deontological).
narrative_ontology:cs_reference_frame('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', temple_centric_performance).
narrative_ontology:cs_drift_state('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ceba80e0-00d1-4326-a2e8-fec0e4c75cb2', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, diaspora_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_deferral_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, textual_substitution_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic framework holding that biblical sacrifice is suspendedânot abrogatedâpending messianic restoration, and that sustained study of sacrificial law preserves communal readiness and normative competence. Derives institutional legitimacy, role, and continuity from this interpretive stance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the ongoing burden of textual study, curricular attention to sacrificial law, and ritual readiness norms in the absence of a Temple. Receive communal continuity, identity cohesion, and a preserved covenantal framework in return. Physical exit is possible but carries heavy identity and social costs; assimilationist alternatives are structurally available yet rarely taken.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% Advocate for immediate physical restoration of sacrifice at the Temple site and reject messianic deferral. Their position is structurally excluded from the dominant normative framework, which treats active sacrifice as premature or forbidden under current conditions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, temple_restoration_advocates, excluded,
    moderate, biographical, constrained, national).

% Study the history of sacrificial law and its interpretive transformations from outside the normative commitment. They observe the shift from Temple performance to textual study as a historical and political contingency but do not participate in the readiness burden.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, academic_textual_critics, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves ritual-legal competence and communal identity for a dispersed people during the indefinite absence of a central cultic site, by substituting textual study for physical performance while keeping the normative framework intact for future reactivation.
% TRANSFER_FUNCTION: Moves cognitive labor, educational time, and interpretive attention from dispersed communities toward the study of sacrificial law, generating rabbinic authority, textual continuity, and communal cohesion in return.
% ABSENT_VOICES: Temple restoration movements that reject messianic deferral and demand immediate performance; assimilationist or secular voices that regard the sacrificial framework as obsolete; internal dissenters who view the textual turn as an evasion rather than a legitimate suspension.
% DISAPPEARANCE_RATIONALE: If the suspended obligation and its study protocol vanished, diaspora communities would lose the primary mechanism linking them to the Temple-centered past. The normative vacuum would force a choice among restorationist performance, archival museumification, or complete abandonment, and rabbinic authority's legitimating narrative would reconfigure.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the loss of the sole legitimate venue for biblical sacrificial worship, which created an existential crisis for a covenantal community whose obligations were tied to that physical site.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature (Mishnah, Talmud) attests the crisis and the textual-substitution response from within the agenda-setter seat. External historians and archaeologists corroborate the Temple destruction as historical fact but do not attest the theological necessity of messianic suspension; some internal modern voices (secular historians, Reform and secular Zionist movements) contest whether the founding problem remains live or whether the textual framework is a permanent evolutionary adaptation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the readiness burdenâsustained curricular attention to defunct ritual lawâis real and asymmetrically concentrated in communities that preserve it, yet it carries no guilt penalty or active enforcement. Suppression is low (0.22): the constraint persists through identity lock and communal norm rather than through active coercion against alternatives. Theater_ratio rises over the interval (0.38 at endpoint) because as the messianic horizon recedes, the functional distinction between readiness maintenance and performative continuity weakens. Accessibility_collapse is elevated (0.65) because once the suspension framework is adopted, internal alternatives (restoration, abandonment) collapse within the tradition; resistance is low (0.12) because the arrangement is largely accepted by the communities that inhabit it.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic_authority seat experiences the constraint as a rope: it solves the coordination problem of preserving a people without a Temple, and the agenda-setter is a net beneficiary. The diaspora_communities seat also computes as rope (net beneficiary) because the identity continuity they receive outweighs the study burden, though their effective extraction is higher than the agenda-setter's. The excluded restorationist seat would compute the same structure as a snare or tangled rope if they were inside the framework, because from their perspective the suppression of immediate performance is an active, enforced exclusion. The divergence is structural: directionality is low for the beneficiaries and high for the excluded, but the excluded are not victims of active extractionâmerely locked out of a normative space they reject.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_authority and diaspora_communities are declared beneficiaries because the constraint subsidizes their continuity and identity maintenance; their directionality sits near the beneficiary end. There is no declared victim set because the obligation is suspendedâno agent is currently being extracted from via an active duty they cannot perform. The temple_restoration_advocates are excluded, not victims: they are locked out of the normative conversation, but the constraint does not extract from them. Because the only directional stakes are beneficiary (low d) and excluded (no d assignment for non-parties), effective extraction is damped for the inside seats and undefined for the excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than piton or snare prevents mislabeling the readiness burden as pure extraction. The constraint has a live coordination function (preserving textual competence and communal identity across two millennia of dispersion), lacks a concentrated capturer of rents, and lacks active enforcement against a victim class. If the messianic horizon were revealed as permanently deferred and the study became purely performative, the metrics would drift toward piton; the temporal measurements show theater_ratio rising but not yet above the functional threshold, and extractiveness is stable rather than accumulating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the messianic_suspension reading of the sacrifice_obligation_continuity kernel. How would its classification change if the archival_preservation reading (study as cultural memory without normative force) were adopted?',
    'Adopt the archival_preservation framing and re-evaluate beneficiary/victim structure: if the obligation is no longer binding, the readiness burden disappears, extractiveness drops to near zero, and the type shifts toward rope or archival mountain.',
    'Would reclassify the constraint''s type and eliminate the current moderate extractiveness by removing the normative burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Locates this constraint within its kernel family and identifies the structural delta of sibling readings.').

omega_variable(
    messianic_indefiniteness,
    'Does the messianic horizon function as a genuine temporal limit or as an indefinitely deferred endpoint that converts the suspension into a permanent structure?',
    'Observe whether messianic restoration movements that gain normative traction are resisted by the rabbinic authority as threats to the current framework; if the framework resists reactivation, the suspension is functionally permanent.',
    'If the suspension is functionally permanent and maintained by inertia rather than genuine readiness, the constraint would reclassify from rope toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_indefiniteness, empirical, 'Tests whether the messianic horizon is a real sunset or a performative deferral.').

omega_variable(
    readiness_burden_asymmetry,
    'Does the readiness burden of sacrificial study fall evenly across the diaspora, or does it asymmetrically concentrate on specific subgroups?',
    'Sociological mapping of time allocation, educational curriculum, and leadership expectations across community segments.',
    'If the burden concentrates asymmetrically on kohanim, scholars, or specific geographies, the rope claim conceals tangled-rope dynamics within the community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_burden_asymmetry, empirical, 'Checks for hidden asymmetric extraction within the coordinated group.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.18).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.32).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.38).

% Extraction over time
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(sacrifice_obligation_continuity__messianic_suspension_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_continuity kernel. The kernel decomposes into four structurally distinct constraints because the natural-language phrase 'continuity of sacrifice obligation' conflates claims with different epsilon values, beneficiary structures, and normative statuses. This reading (messianic_suspension) treats the obligation as binding but suspended; siblings treat it as abrogated (archival_preservation), preparation-only (performance_only), or fulfilled-through-study (study_as_performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
