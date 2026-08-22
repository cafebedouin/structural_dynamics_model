% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Mourning-Practice as Persecution-Survival Training (Survival-Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the survival-competence reading of the
 *   catastrophe memory kernel: a persecuted or historically persecuted
 *   community's mourning-practice calendar is read as an operational
 *   transmission mechanism for crisis-response competence rather than
 *   primarily as boundary policing, trauma warning, or symbolic continuity
 *   (those are sibling readings, not this constraint). Under this reading,
 *   the ritual rehearsal of loss, dispersal, and reconstitution functions
 *   analogously to fire drills — repeated enactment that keeps a
 *   low-probability, high-stakes response pattern available. The rising
 *   theater_ratio and suppression_requirement over the interval track a
 *   documented pattern: as lived memory of the founding persecution episodes
 *   recedes and actual crisis frequency drops, the community increasingly
 *   relies on formal enforcement (social sanction against non-observance)
 *   rather than experiential necessity to keep the transmission mechanism
 *   active, and a growing share of practice drifts toward performance of
 *   continuity rather than functional rehearsal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Mourning-Practice as Persecution-Survival Training (Survival-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '6bf59188-9129-415a-ab2e-376fd3534006').
narrative_ontology:cs_kernel_codification('6bf59188-9129-415a-ab2e-376fd3534006', distributed).
narrative_ontology:cs_authority_grounding('6bf59188-9129-415a-ab2e-376fd3534006', practice).
narrative_ontology:cs_interpretation_layer_present('6bf59188-9129-415a-ab2e-376fd3534006').
narrative_ontology:cs_reading_relation('6bf59188-9129-415a-ab2e-376fd3534006', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bf59188-9129-415a-ab2e-376fd3534006', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_reading_relation('6bf59188-9129-415a-ab2e-376fd3534006', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('6bf59188-9129-415a-ab2e-376fd3534006', foundational, ritual_rehearsal_transmits_operational_competence).
narrative_ontology:cs_axiom_status(ritual_rehearsal_transmits_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('6bf59188-9129-415a-ab2e-376fd3534006', ritual_rehearsal_transmits_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('6bf59188-9129-415a-ab2e-376fd3534006', secondary, reduced_observance_increases_collective_vulnerability).
narrative_ontology:cs_axiom_status(reduced_observance_increases_collective_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('6bf59188-9129-415a-ab2e-376fd3534006', reduced_observance_increases_collective_vulnerability, instrumental).
narrative_ontology:cs_reference_frame('6bf59188-9129-415a-ab2e-376fd3534006', founding_persecution_era_competence_necessity).
narrative_ontology:cs_drift_state('6bf59188-9129-415a-ab2e-376fd3534006', contemporary_low_incidence_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6bf59188-9129-415a-ab2e-376fd3534006', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_community_as_collective).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_specialists).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_seeking_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, exogamous_and_intermarried_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, younger_generation_diaspora).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, younger_generation_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the mourning-rites calendar and the specific choreography of lament, fasting, and communal gathering that the tradition holds encodes what to do when persecution recurs: whom to trust, how to disperse, what to hide, how to reconstitute community after loss. They set which practices are obligatory and adjudicate deviations. Their own status and meaning derive entirely from the transmission role, so they have no exit from the system they administer.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_specialists, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_specialists, beneficiary).

% The dispersed community as a whole, whose historical experience includes repeated episodes of persecution. Through repeated ritual rehearsal of catastrophe-response patterns embedded in mourning practice, the community retains operational competence — evacuation logic, mutual-aid protocols, information-compartmentalization habits — that has demonstrably aided survival in prior crises. Benefits diffusely and collectively; no single member can extract the competence outside participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_community_as_collective, beneficiary,
    organized, civilizational, constrained, regional).

% Individuals who want to reduce visible difference from the surrounding majority — through intermarriage, relaxed observance, or geographic dispersal into non-community neighborhoods — bear social costs for non-participation in mourning rites: exclusion from mutual-aid networks, family estrangement, loss of standing. The ritual's persecution-survival logic treats reduced observance as increased collective vulnerability, so pressure to conform falls specifically on those most inclined to blend in.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_seeking_members, payer,
    moderate, biographical, constrained, local).

% Families formed across community boundaries face the sharpest enforcement of mourning-practice obligations, since intermarriage is read by the ritual's own logic as a primary erosion vector for the transmitted competence. They pay through social friction, contested inheritance of ritual obligation for mixed-heritage children, and repeated renegotiation of belonging at each life-cycle mourning event.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, exogamous_and_intermarried_families, payer,
    moderate, biographical, constrained, local).

% Younger members raised in relative safety, often geographically dispersed and digitally connected, receive the transmitted competence as inherited obligation rather than lived necessity. They benefit from the resilience knowledge in the abstract but bear the cost of maintaining practices whose operational logic no longer matches their daily risk environment; some have genuine exit via migration and assimilation, which the elders read as attrition of collective capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, younger_generation_diaspora, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, younger_generation_diaspora, beneficiary).

% The surrounding majority institutions — courts, schools, employers — are not party to the internal ritual calendar but are the historical and sometimes present source of the persecution risk the ritual encodes against. They have no voice in how the community narrates the threat, and their own record of past persecution episodes is rarely consulted as external corroboration of the founding problem.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, host_society_institutions, excluded,
    institutional, generational, analytical, national).

% Study the ritual calendar across communities and historical periods, comparing claimed function (survival competence transmission) against documented outcomes in actual persecution episodes, and against the competing readings (boundary maintenance, trauma encoding, symbolic continuity) that the same practices also plausibly serve.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mourning-practice rehearsal transmits operational patterns — dispersal logic, mutual-aid activation, information discipline, in-group trust calibration — that have functioned as genuine adaptive competence during historical persecution episodes, solving the real problem of preserving crisis-response knowledge across generations who have not personally lived through catastrophe.
% TRANSFER_FUNCTION: Moves social standing, belonging, and access to mutual-aid networks from those who reduce ritual observance to those who maintain it; moves interpretive authority over what counts as adequate transmission to the elder ritual specialists who administer the calendar.
% ABSENT_VOICES: Host-society institutions, whose historical conduct is the referent the survival competence responds to, have no say in whether the threat model is current, exaggerated, or accurately calibrated to present conditions; younger diaspora members who have exited are structurally removed from the conversation that judges their exit as capacity erosion.
% DISAPPEARANCE_RATIONALE: The community itself is divided: elders and those who credit the tradition with historical survival hold that losing the ritual calendar would leave the community materially less prepared for renewed persecution; assimilation-inclined members and much of the diaspora hold that the operational knowledge has already migrated to secular institutions (legal advocacy groups, diaspora networks, digital archives) and that ritual disappearance would mainly relieve social pressure without functional loss.
% FOUNDING_PROBLEM: Repeated historical episodes of persecution required the community to preserve and transmit crisis-response competence — how to disperse, hide, mutual-aid, and reconstitute — across generations who had not directly experienced the catastrophe, using a medium durable enough to survive displacement and loss of written records.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of persecuted diaspora communities attest, from outside the tradition's own beneficiary structure, that ritual mourning calendars in several documented cases preceded and appear to have shaped effective crisis responses (dispersal timing, resource caching, information compartmentalization) in subsequent persecution episodes — but the same scholars note the evidence is mixed and confounded with the boundary-maintenance and symbol-continuity functions the same rituals simultaneously serve; no fully independent replication isolates survival-competence transmission as the operative mechanism.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) because the survival-competence function is at least partly real and independently corroborated by comparative-religion scholarship, not a pure cover story — this distinguishes the reading from a pure snare. But it is not zero: enforcement against assimilation-seeking members and intermarried families imposes real costs that the coordination function does not require in full — a lighter-touch transmission mechanism could plausibly preserve most of the competence without the social sanction currently attached to non-observance. Suppression (0.38) and resistance (0.45) are both mid-range, reflecting genuine but contested internal dissent rather than either free coordination or coercive lockdown. Accessibility collapse (0.40) is moderate — alternative transmission channels (secular archives, diaspora advocacy networks) exist and are used, but the ritual retains privileged status as the primary sanctioned mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The threatened community as a collective and the elder specialists who administer the calendar sit toward the beneficiary end: the collective gains real operational resilience, and the specialists derive their entire social role and status from being the transmission's stewards. Assimilation-seeking members and intermarried families sit toward the target end: the same mechanism that builds collective competence extracts conformity costs from exactly those most likely to reduce observance, since the survival-competence logic treats reduced observance as increased collective vulnerability, converting individual choice into a collective-risk problem the ritual apparatus is entitled to police. The younger diaspora generation is split — genuine partial beneficiary of inherited knowledge, genuine partial payer of obligations calibrated to a risk environment they may not currently inhabit, with real exit via geographic and social mobility that older, more embedded members lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting crisis-response competence across generations without lived experience of catastrophe) is authored as contested rather than flatly dead: comparative evidence suggests the competence transmission mechanism has partially worked historically, but present persecution risk in many diaspora contexts is lower than in the founding periods, while the enforcement apparatus (suppression_requirement rising to 0.38) has not correspondingly relaxed. This is the classic mandatrophy signature — a coordination function whose founding necessity has partially receded while the enforcement machinery that once served it continues to intensify, now serving relationship-maintenance and status functions for the elder specialists as much as, or more than, its original survival function. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating it as a pure extraction racket (which would ignore the corroborated historical competence-transmission function) and treating it as natural or inevitable (which would ignore the real, unevenly distributed costs falling on assimilation-seeking and intermarried members).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_transmission_efficacy,
    'Does the ritual mourning calendar actually transmit operationally useful persecution-response competence, or does it transmit a narrative of competence whose real content has been superseded by secular institutions (legal aid networks, diaspora advocacy organizations, digital archives)?',
    'Comparative case studies of communities that maintained versus abandoned the ritual calendar, measured against documented outcomes in subsequent actual persecution episodes, controlling for secular institutional support available in each case.',
    'If the ritual transmits negligible unique operational value beyond what secular institutions now provide, the constraint''s coordination function has substantially atrophied and the classification should drift toward piton or snare (enforcement without corresponding function); if genuine unique transmission is confirmed, the tangled_rope classification with real coordination value is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_transmission_efficacy, empirical, 'Whether the claimed survival-competence transmission function is empirically real or a legitimating narrative for a function now performed elsewhere.').

omega_variable(
    reading_decomposition_boundary,
    'Is the survival-competence function of this ritual practice structurally separable from its boundary-maintenance, trauma-encoding, and symbol-continuity functions, or are these four readings describing inseparable aspects of a single unified practice that cannot be decomposed without distortion?',
    'Ethnographic and historical analysis of whether communities have ever maintained one function (e.g., competence transmission) while explicitly relaxing another (e.g., boundary enforcement against intermarriage) — such natural experiments would indicate separability.',
    'If the functions are genuinely inseparable in practice, the four-way kernel decomposition, while analytically useful, may understate how tightly the extraction (boundary policing) is coupled to the coordination (competence transmission) — meaning efforts to preserve the coordination function without the extraction cost may not be achievable in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_decomposition_boundary, conceptual, 'Whether the kernel''s four readings pick out genuinely separable structural functions or are analytically distinct facets of one fused practice.').

omega_variable(
    host_society_corroboration_gap,
    'How reliable is the community''s internal threat model of ongoing or recurring persecution risk, given that the primary external party (host society institutions) is structurally excluded from the conversation and has no incentive to either confirm or deny the threat assessment?',
    'Independent historical and sociological assessment of actual persecution incidence and risk trends in the relevant host societies over the measurement interval, compared against the community''s internally maintained threat narrative.',
    'A significant gap between actual documented risk trends and the internally maintained threat level would support the mandatrophy reading (enforcement outlasting founding necessity); close alignment would support the founding problem being genuinely live and the enforcement calibration being reasonable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_society_corroboration_gap, empirical, 'Whether the community''s threat model justifying continued ritual enforcement is externally corroborated or internally self-reinforcing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the catastrophe_memory_kernel, each authored as a structurally distinct constraint per the ε-invariance principle (DP-001): the same observable ritual practices (mourning rites, fasting, commemorative gatherings) support four different structural claims about what the practice IS doing. survival_competence_reading (this file) authors moderate extractiveness (0.42) reflecting a partly-corroborated coordination function; boundary_maintenance_reading would author higher suppression and a more purely extractive profile toward exogamous/intermarried members; trauma_encoding_reading would author the practice as a warning-transmission system with different beneficiary logic (protective information transfer rather than operational competence); symbol_continuity_reading would author the lowest extractiveness of the four, closest to a genuine rope, since identity/continuity preservation with minimal enforcement cost is the least extraction-coupled reading. All four share the same underlying observable ritual calendar but diverge in claimed function, beneficiary/victim structure, and ε — hence four files, linked via affects_constraints, not one file with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
