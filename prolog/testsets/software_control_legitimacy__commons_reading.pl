% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Commons Governance (Negotiated Collective Management Reading)
 *   domain: technological/political-economy
 *
 * SUMMARY:
 *   The commons reading of software control legitimacy asserts that neither
 *   absolute property rights nor absolute freedom provides legitimate
 *   governance of shared digital infrastructure. Instead, control should be
 *   negotiated collectively among stakeholders — developers, users,
 *   maintainers, and communities — through transparent, revisable governance
 *   processes. This reading emerged from open-source practice (GPL-style
 *   licensing, foundation governance, contributor agreements) and is now
 *   formalized in organizational structures like the Linux Foundation, Apache
 *   Foundation, and countless project communities. The constraint is CLAIMED
 *   as tangled_rope because it genuinely solves a coordination problem
 *   (distributed contribution with maintained coherence) AND creates
 *   asymmetric extraction (restricts freedom absolutists and property
 *   absolutists from unilateral control, requires governance participation
 *   from ecosystem participants). The reading coexists with three sibling
 *   readings: the freedom-imperative reading (which denies property
 *   legitimacy entirely), the property-rights reading (which denies freedom
 *   to modify), and the pragmatic-openness reading (which sees commons
 *   governance as one legitimate choice among others but not the only one).
 *
 * KEY AGENTS:
 *   - Infrastructure maintainers (Linux Foundation, Apache Foundation, core teams): Set governance agendas, define contribution rules, enforce collective decisions
 *   - Ecosystem participants (developers, users, contributors): Participate in governance, benefit from coordination, bear costs of compliance with collective rules
 *   - Freedom absolutists (GPL ideologues, free software advocates): Denied unilateral control, must negotiate with property-claim holders
 *   - Property-rights absolutists (commercial software creators, IP-maximalist organizations): Denied unilateral control, must negotiate with freedom advocates and communities
 *   - Excluded voices (resource-poor developers, regions without digital infrastructure, future users): Structurally absent from governance tables due to participation barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.38).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance (Negotiated Collective Management Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "technological/political-economy").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018').
narrative_ontology:cs_kernel_codification('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', distributed).
narrative_ontology:cs_authority_grounding('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', practice).
narrative_ontology:cs_interpretation_layer_present('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018').
narrative_ontology:cs_reading_relation('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_axiom('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', foundational, multi_stakeholder_legitimacy_principle).
narrative_ontology:cs_axiom_status(multi_stakeholder_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', multi_stakeholder_legitimacy_principle, deontological).
narrative_ontology:cs_axiom('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', foundational, negotiated_governance_necessity).
narrative_ontology:cs_axiom_status(negotiated_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', negotiated_governance_necessity, instrumental).
narrative_ontology:cs_reference_frame('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', distributed_contribution_coordination).
narrative_ontology:cs_drift_state('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', contemporary_institutionalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5cf4dfbe-438e-41a9-89d2-eb2d1a5dc018', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, ecosystem_participants).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, freedom_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, property_rights_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, excluded_development_voices).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.48 (moderate) because the commons reading genuinely solves a coordination problem (shared infrastructure, distributed modification) but also restricts both freedom absolutists and property absolutists from unilateral control. The measurement series show modest growth from 0.38 to 0.48 over 25 time units, reflecting increasing formalization of governance structures (foundations, conflict-resolution bodies, license standardization) — as commons governance becomes institutionalized, it develops more rules restricting freedom/property absolutism. Theater ratio (0.22) is low because the constraint's functioning governance bodies are genuinely active in resolving disputes and setting policy, not primarily performative; the ratio increases modestly as governance becomes more established but plateaus when the institutional form stabilizes. Suppression (0.38) reflects the active enforcement required to keep freedom and property absolutists within negotiated governance rather than allowing them to split off into pure open or pure proprietary ecosystems. Suppression increases as commons-governance legitimacy grows because more actors must be constrained to maintain the reading's coherence. Accessibility collapse (0.62) is moderate-high: once a developer or organization enters a commons-governed project, the alternatives (pure open or pure proprietary) are accessible in principle but carry switching costs in terms of community relationships, reputation, and infrastructure; the commons reading narrows viable exit paths but does not eliminate them. Resistance (0.71) is substantial: both freedom and property absolutists actively resist commons-governance claims; intellectual property advocates argue it violates property rights; free software advocates argue it compromises freedom; pragmatists argue it is unnecessary. This resistance reflects the reading's contested status — it is not a settled natural law but an actively defended position.
 *
 * PERSPECTIVAL GAP:
 *   The stakeholder-community and ecosystem-participant seats experience commons governance as beneficial coordination with manageable constraints. The infrastructure-maintainer seat experiences it as legitimate authority over collective resources. The freedom-absolutist seat experiences it as constraints on non-negotiable rights. The property-rights-absolutist seat experiences it as restrictions on legitimate ownership. The excluded-voices seat experiences it as inaccessible governance claiming to represent them without their input. The engine should compute different types per seat because the structural relationship to the constraint differs fundamentally: beneficiaries experience coordination; absolutists experience suppression; maintainers experience authority; excluded voices experience powerlessness. The commons reading claims all these experiences are legitimate simultaneously — none can be dismissed as mere ideology — and governance must navigate among them. This claim distinguishes the commons reading from the sibling readings, each of which dismisses some experiences as illegitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and ecosystem participants are the structural beneficiaries: they gain voice in governance and access to maintained infrastructure without surrendering all property or freedom claims. Their directionality is low (d ~0.3–0.4: they pay compliance costs but collect genuine benefits). Freedom absolutists are structurally targeted by the requirement to negotiate property claims; their directionality is high (d ~0.7–0.8: they bear costs of constrained freedom in service of collective governance). Property-rights absolutists are also targeted, required to negotiate with freedom advocates and communities; their directionality is similarly high (d ~0.7–0.8). Infrastructure maintainers sit at symmetric-to-high directionality (d ~0.5–0.6): they have legitimate authority but bear significant maintenance burden; the constraint enables their role but also locks them into identity-fused commitment. Excluded voices cannot be modeled in the standard directionality framework because they are not seated — their absence is structural. No directionality override is needed; the asymmetry emerges naturally from beneficiary/victim declarations and exit-option analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The commons reading avoids the false-satisfaction problem by requiring active conflict resolution and governance participation, not by claiming the underlying disputes have dissolved. Unlike a mountain (which claims the constraint is natural and universal), or a rope (which claims participants prefer it), the commons reading is explicitly a working compromise. The founding problem (coordination without chaos or isolation) remains live because it is re-encountered in every new project and every new stakeholder admission. The claim and metrics align: extractiveness is moderate because the constraint both solves and imposes; suppression is needed because absolutists of both kinds would exit; theater is low because governance bodies are actively functioning, not ceremonial. The measurement trajectory shows stabilization rather than drift, indicating the constraint has matured into a stable institutional form. Mandatrophy would occur if the founding problem — distributed modification with maintained coherence — were solved by some other mechanism (e.g., AI-assisted code reconciliation, automated testing, or a breakthrough in property-law flexibility), rendering commons governance redundant. Current evidence shows no such displacement; the founding problem remains live. The constraint should remain classified as tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_boundary_commons_vs_pragmatism,
    'Is the commons reading a structurally distinct legitimacy claim (shared authority grounded in stakeholder participation) or is it a pragmatic choice-point within a larger property-flexible framework?',
    'Close reading of founding documents, governance-body decision minutes, and dispute-resolution rationales. If commons bodies consistently appeal to stakeholder participation AS THE GROUND of legitimacy (not as a means to efficient code), it is distinct. If they appeal to pragmatic benefits (better code, faster iteration), it collapses toward the pragmatic-openness reading.',
    'If the commons reading is distinct, it has independent ε and can be modeled as a tangled rope (coordination + restricted absolutism). If it collapses into pragmatism, it becomes a rope with much lower extractiveness and should reclassify. The distinction matters for how conflict-resolution bodies defend their authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_boundary_commons_vs_pragmatism, conceptual, 'Whether commons governance is a distinct legitimacy claim or a pragmatic choice.').

omega_variable(
    power_asymmetry_in_commons_governance,
    'Do commons-governance bodies genuinely distribute authority among stakeholders, or do they concentrate power in maintainer hands under the cover of collective participation?',
    'Comparative study of decision authority in governance bodies (Linux kernel, Python, Apache projects): who has veto power, who can override community decisions, who controls agenda-setting. Measurement of stakeholder influence on actual policy changes over a decade-long period.',
    'If authority is genuinely distributed, the extractiveness (0.48) is accurate and the constraint is tangled rope. If maintainers hold veto despite ''community governance,'' extractiveness is understated and the constraint approaches snare; suppression would be understated as well. The commons reading would then be a legitimacy narrative covering concentration, not a description of actual governance structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_in_commons_governance, empirical, 'Whether commons governance actually distributes authority or concentrates it in maintainer hands.').

omega_variable(
    structural_exclusion_persistence,
    'Is the exclusion of resource-poor developers and regional communities from governance tables a resolvable participation barrier or a structural feature of commons governance?',
    'Track whether excluded voices gain access as governance infrastructure matures (documentation translation, time-zone accommodations, funding for participation). If access grows, exclusion is resolvable. If access remains stable despite infrastructure investment, it is structural.',
    'If structural, the commons reading victimizes excluded voices as a persistent feature, not an accidental side effect. The victim group in base_properties should grow to include structural-exclusion targets, and suppression should be higher to maintain the exclusion. The constraint would show signs of piton-hood (maintained for other reasons but not genuinely beneficial) at the excluded-voice seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_exclusion_persistence, empirical, 'Whether structural exclusion from governance is a resolvable barrier or a persistent feature.').

omega_variable(
    freedom_absolutism_foreclosure_test,
    'Does the commons reading logically foreclose the freedom-imperative reading, or do both readings coexist without either ruling out the other?',
    'Test the logical structure: a freedom absolutist claims ''all proprietary software is ethically illegitimate.'' A commons advocate claims ''property claims require negotiation with users and community.'' Can both claims be held simultaneously without contradiction? If a single actor can hold ''I believe freedom is a right AND I accept negotiated limits on that right for this project,'' they coexist. If no coherent actor can hold both, the commons reading forecloses the freedom reading.',
    'If coexists_with: the readings are genuinely distinct positions held by different parties; the commons reading does not claim to have resolved the underlying dispute about freedom, only to have created a forum for negotiation. If forecloses: the commons reading asserts that freedom-absolutism is logically indefensible in the presence of community and property stakeholders; the constraint is more deeply exclusionary than the metrics suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_absolutism_foreclosure_test, conceptual, 'Logical relationship between commons and freedom-absolutist readings.').

omega_variable(
    maintenance_identity_lock_mechanism,
    'Why is the infrastructure-maintainer exit classified as identity_locked rather than mobile or arbitrage? What specific identity-fusion mechanism binds maintainers?',
    'Ethnographic study of maintainer interviews, documented burn-out, and attempted exits. Identify whether exit barrier is professional identity (career path built on the role), relational identity (community recognizes them through this role), ideological identity (worldview constituted through commitment to the project), or institutional identity (the project has become ''who they are'').',
    'If identity-lock is strong (high relational + ideological components), maintainers are partially coerced into continued governance participation and the constraint''s suppression is understated. If exit is merely constrained (career costs, but psychologically exitable), the constraint is less extractive than the identity-lock framing suggests. Identity-lock mechanism matters for whether the commons reading is sustainable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_identity_lock_mechanism, empirical, 'Mechanism of identity-lock in maintainer participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__commons_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__commons_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__commons_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, open_source_license_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_contribution_attribution).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, fork_governance_rights).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the software_control_legitimacy kernel. The commons reading instantiates a specific structural claim: that legitimate authority over software control is neither unilateral property ownership nor unilateral user freedom, but negotiated governance among stakeholders. This differs fundamentally from the freedom-imperative reading (which denies property legitimacy entirely), the property-rights reading (which denies unilateral user modification rights), and the pragmatic-openness reading (which treats all three governance models as equally legitimate alternatives). Each reading has different ε, different beneficiary/victim structures, and different classification. The ε-invariance principle requires separate stories for each reading because the structural form of legitimacy differs: what counts as extraction, coordination, and suppression depends on which authority framework is being instantiated. All four readings should be modeled in the corpus to enable comparison of how different legitimacy claims reshape stakeholder relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
