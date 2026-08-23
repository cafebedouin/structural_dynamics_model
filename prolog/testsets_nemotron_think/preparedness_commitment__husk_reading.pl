% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The preparedness commitment kernel—a stabilized institutional promise
 *   that 'we will be ready'—admits multiple readings. The husk_reading
 *   instantiates the constraint as memorial performance: the drills, plans,
 *   and certifications persist as ritualized forms that feel like retention
 *   but have lost operational competence. High form-compliance coexists with
 *   low adaptive capacity; the D5 break (novel stress exceeding rehearsed
 *   scenarios) manifests as competence collapse. This reading is not a mere
 *   critique—it is a structural description of how the constraint operates
 *   for the seats that bear its costs. The claimed_type is tangled_rope
 *   because the constraint retains a genuine coordination function (baseline
 *   interoperability for routine incidents) while simultaneously extracting
 *   legitimacy and resources from the gap between ritual and reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.72).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.68).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '28fb8d38-38ae-46a4-be8f-deb0ec494980').
narrative_ontology:cs_kernel_codification('28fb8d38-38ae-46a4-be8f-deb0ec494980', formalized).
narrative_ontology:cs_authority_grounding('28fb8d38-38ae-46a4-be8f-deb0ec494980', lineage).
narrative_ontology:cs_interpretation_layer_present('28fb8d38-38ae-46a4-be8f-deb0ec494980').
narrative_ontology:cs_reading_relation('28fb8d38-38ae-46a4-be8f-deb0ec494980', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('28fb8d38-38ae-46a4-be8f-deb0ec494980', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('28fb8d38-38ae-46a4-be8f-deb0ec494980', foundational, form_compliance_suffices_for_preparedness).
narrative_ontology:cs_axiom_status(form_compliance_suffices_for_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('28fb8d38-38ae-46a4-be8f-deb0ec494980', form_compliance_suffices_for_preparedness, conventional).
narrative_ontology:cs_axiom('28fb8d38-38ae-46a4-be8f-deb0ec494980', secondary, ritual_sustains_institutional_legitimacy).
narrative_ontology:cs_axiom_status(ritual_sustains_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('28fb8d38-38ae-46a4-be8f-deb0ec494980', ritual_sustains_institutional_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('28fb8d38-38ae-46a4-be8f-deb0ec494980', formal_compliance_framework).
narrative_ontology:cs_drift_state('28fb8d38-38ae-46a4-be8f-deb0ec494980', contemporary_complex_threat_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('28fb8d38-38ae-46a4-be8f-deb0ec494980', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, emergency_management_bureaucracy).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, affected_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, formal_compliance_equals_readiness).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, ritual_sustains_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the preparedness compliance regime: writes the standards, mandates the drills, audits the checkboxes. They control the definition of what counts as preparedness and the consequences of non-compliance. Their authority derives from the regime's formal codification.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, emergency_management_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).

% Receives political credit, budget protection, and liability shielding from the appearance of preparedness. They authorize funding for the compliance apparatus but not for the adaptive capacity it displaces. They can exit the constraint by leaving office; the institution remains bound.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_leadership, beneficiary,
    powerful, biographical, arbitrage, national).

% Execute the drills, file the reports, maintain the certifications. They know the rituals lack operational realism but face disciplinary and career consequences for non-compliance. They bear the physical risk when novel events expose the competence gap. Some residual benefit: the protocols provide baseline coordination for routine incidents.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, frontline_responders, beneficiary).

% Experience the consequences when memorial performance meets novel stress: delayed response, misallocated resources, command paralysis. They cannot exit the jurisdiction of the preparedness regime. Their only leverage is post-disaster political pressure, which the compliance regime is designed to absorb.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, affected_communities, payer,
    powerless, generational, trapped, local).

% Audit form-compliance metrics: drill frequency, plan currency, certification rates. Their mandate is to verify adherence to the formal standard, not to test adaptive capacity. They legitimize the theater by certifying it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, oversight_auditors, observer,
    institutional, generational, analytical, national).

% Advocate for adaptive exercises, red-teaming, and competence-based metrics. They are structurally excluded from the compliance rulemaking process because their proposals threaten the regime's legitimacy and the beneficiaries' cover. Some achieve marginal pilot programs that are not scaled.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, reform_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a baseline of shared procedures, communication protocols, resource inventories, and inter-agency contact structures that enable coordination during routine and previously-experienced incident types.
% TRANSFER_FUNCTION: Moves legitimacy, budget authority, political cover, and liability protection from the public and frontline responders to institutional leadership and emergency management bureaucracy, in exchange for form-compliance rituals that simulate preparedness without maintaining adaptive capacity.
% ABSENT_VOICES: Communities that have experienced competence collapse during novel disasters (wildfire-urban interface, pandemic, compound events); frontline responders who have refused to participate in theater drills and faced retaliation; independent preparedness experts who advocate adaptive capacity over form compliance; municipal officials in jurisdictions that have rejected the compliance regime in favor of community-based resilience.
% DISAPPEARANCE_RATIONALE: If the memorial performance disappeared overnight, the gap between claimed and actual preparedness would be exposed. Institutions would face immediate pressure to either invest genuinely in adaptive capacity (red-teaming, stress-testing, community-integrated exercises) or publicly acknowledge unpreparedness. The compliance regime's enforcement machinery (audits, certifications, funding conditionalities) would lose its object, forcing a reorganization of the preparedness field around demonstrable competence or honest vulnerability.
% FOUNDING_PROBLEM: After a series of major disasters in the 1970s-1990s revealed catastrophic coordination failures between agencies, jurisdictions, and sectors, institutions established standardized preparedness rituals—drills, plans, certifications, interoperable communications—to ensure baseline readiness and interoperability for known threat profiles.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from Hurricane Katrina (2005), the 2009 H1N1 pandemic, Superstorm Sandy (2012), and the COVID-19 pandemic (2020) document that form-compliance metrics (plan currency, drill completion, certification rates) were met while adaptive capacity failed catastrophically for novel threat profiles. The 2006 Post-Katrina Emergency Management Reform Act and multiple GAO reports confirm the founding problem (basic interoperability for known hazards) was substantially addressed by the early 2000s. Independent commissions (e.g., the 9/11 Commission, the COVID Crisis Group) attest that the current regime persists not because the founding problem remains, because the rituals have become the source of institutional legitimacy and budget authority.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the regime captures budget, political capital, and liability protection while delivering diminishing adaptive returns. Suppression (0.68) is substantial because alternative preparedness models (community-based, adaptive, red-teamed) are structurally excluded from funding and legitimacy by the compliance regime's monopoly on certification. Theater_ratio (0.82) is very high—the drills are increasingly performative, optimized for auditability rather than stress. Accessibility_collapse (0.62) reflects that once an agency enters the compliance regime, exiting to a competence-based model requires rebuilding legitimacy from scratch. Resistance (0.55) is moderate: frontline responders resist quietly (workarounds, cynicism) but lack collective exit options.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the constraint is a Rope: it coordinates interoperability, standardizes language, ensures baseline readiness. From the payer seats (frontline_responders, affected_communities), it is a Snare: the coordination story is cover for a regime that extracts their safety to fund institutional legitimacy. The engine computes this divergence from the structural data—this commentary only describes it.
 *
 * DIRECTIONALITY LOGIC:
 *   The emergency_management_bureaucracy (agenda_setter) sits near the beneficiary end (d ~ 0.2): they control the regime and extract institutional survival from it. Institutional_leadership (beneficiary) sits at d ~ 0.15: they capture the political returns with minimal personal cost. Frontline_responders (payer) sit near the target end (d ~ 0.85): they pay in time, credibility, and physical risk with constrained exit. Affected_communities (payer) are at d ~ 0.95: trapped, powerless, bearing the full cost of competence collapse. Oversight_auditors (observer) are at d ~ 0.5 (analytical seat). Reform_advocates (excluded) would be targets if included (d ~ 0.9) but are kept out of the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (basic interoperability for known hazards) is dead—solved by the early 2000s. The constraint persists because the rituals became the source of institutional legitimacy and budget authority (mandatrophy). The classification prevents mislabeling this as pure coordination (Rope) by documenting the asymmetric extraction: beneficiaries capture the gains of the preparedness claim while payers bear the costs of its hollowness. It also prevents mislabeling as pure extraction (Snare) because the baseline coordination for routine incidents is real and valued by frontline_responders (secondary_role: beneficiary). The tangled_rope classification captures the hybrid: coordination on form, extraction on substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_coordination_value,
    'Do the form-compliance rituals provide any residual coordination value during novel crises, or is the coordination function entirely displaced by theater?',
    'Comparative analysis of response effectiveness in jurisdictions with high vs. low form-compliance but similar threat profiles, controlling for resource levels. Natural experiments from agencies that lost certification but retained experienced personnel.',
    'If residual value is substantial, the tangled_rope classification is reinforced (genuine coordination + extraction). If near zero, the constraint reclassifies toward snare (coordination story is pure cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_value, empirical, 'Whether the rituals retain any functional coordination capacity under novel stress.').

omega_variable(
    conscious_design_vs_emergent_theater,
    'Is the theater consciously maintained by institutional leadership as a strategy, or is it an emergent property of bureaucratic incentives (Goodhart''s law on compliance metrics)?',
    'Analysis of internal communications, budget deliberations, and reform proposals: do leaders explicitly trade adaptive capacity for compliance metrics, or do they genuinely believe the metrics proxy for readiness?',
    'If conscious design, the extraction is intentional (snare-adjacent). If emergent, the constraint is a piton-in-formation where the coordination function atrophied without deliberate replacement—affects mandatrophy trajectory and fixability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conscious_design_vs_emergent_theater, conceptual, 'Intentionality of the theater: strategy vs. structural drift.').

omega_variable(
    community_exit_potential,
    'Could affected communities develop alternative preparedness models (mutual aid, community emergency response teams, participatory scenario planning) that reduce their trapped status, or is their entrapment structural (geographic, political, legal)?',
    'Case studies of communities that have built parallel preparedness structures (e.g., Cuban hurricane preparedness, Japanese community disaster management, U.S. CERT programs) and their interaction with the formal regime.',
    'If exit is possible, the trapped classification for affected_communities may overstate their constraint-binding; if structural, the snare/tangled_rope extraction on this seat is more severe than metrics alone indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_exit_potential, empirical, 'Whether community-level exit from the memorial performance regime is feasible.').

omega_variable(
    reading_framing_underdetermination,
    'Does the husk_reading accurately capture the kernel''s operation, or does it overstate theater by conflating ''form-compliance for known hazards'' with ''total absence of adaptive capacity''?',
    'Cross-kernel comparison: apply the same reading-decomposition method to other commitment kernels (e.g., nuclear safety culture, aviation safety, cybersecurity compliance). If the husk pattern recurs systematically, it is a general feature of formalized commitment systems; if unique to preparedness, the framing may be overfit.',
    'If the husk pattern is general, the constraint family (preparedness_commitment) has a structural tendency toward memorial performance that the competence_reading and hybrid_reading must explain. If idiosyncratic, the reading may be a partisan framing rather than a structural description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the husk_reading''s framing is a general feature of formalized commitment systems or specific to preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__husk_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__husk_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.67).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__husk_reading, theater_ratio, 32, 0.76).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__husk_reading, theater_ratio, 40, 0.82).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__husk_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__husk_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__husk_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__husk_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__husk_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__husk_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__husk_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__husk_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__husk_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three readings: competence_reading (live exercised knowledge, low extraction, rope/mountain), husk_reading (memorial performance, high extraction, tangled_rope), and hybrid_reading (layered system, mixed). The husk_reading influences the hybrid_reading by creating pressure to legitimize the memorial layer; it coexists_with the competence_reading as different institutional units and actors instantiate different readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__husk_reading, institutional, 0.2).
constraint_indexing:directionality_override(preparedness_commitment__husk_reading, powerful, 0.15).
constraint_indexing:directionality_override(preparedness_commitment__husk_reading, organized, 0.85).
constraint_indexing:directionality_override(preparedness_commitment__husk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
