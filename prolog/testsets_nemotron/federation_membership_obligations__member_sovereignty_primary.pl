% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Sovereignty Reading of Federation Membership Obligations
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'member_sovereignty_primary' reading
 *   of the federation_membership_obligations kernel. The reading asserts that
 *   national welfare closure authority is the governing principle: free
 *   movement is real but conditional, and the condition is protection of
 *   domestic labor markets and welfare system sustainability. Mobile workers
 *   and economically inactive migrants are structurally excluded from full
 *   welfare beneficiary status; member state legislatures retain effective
 *   veto over access rules. The constraint operates as a tangled rope: it
 *   genuinely coordinates fiscal sustainability across diverse national
 *   systems (coordination function) while extracting from mobile populations
 *   who pay contributions but face restricted benefits (asymmetric
 *   extraction), and it requires active enforcement through residence tests,
 *   contribution thresholds, and categorical exclusions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.65).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Sovereignty Reading of Federation Membership Obligations").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6').
narrative_ontology:cs_kernel_codification('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', formalized).
narrative_ontology:cs_authority_grounding('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', lineage).
narrative_ontology:cs_interpretation_layer_present('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6').
narrative_ontology:cs_reading_relation('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', foundational, national_welfare_closure_authority_supreme).
narrative_ontology:cs_axiom_status(national_welfare_closure_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', national_welfare_closure_authority_supreme, conventional).
narrative_ontology:cs_axiom('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', foundational, free_movement_conditional_on_sustainability).
narrative_ontology:cs_axiom_status(free_movement_conditional_on_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', free_movement_conditional_on_sustainability, instrumental).
narrative_ontology:cs_reference_frame('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', rome_treaty_compromise).
narrative_ontology:cs_drift_state('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', post_eastern_enlargement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9d7ec77c-0e13-43b7-8c72-3f7f616b7fc6', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_commuters).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, national_welfare_closure_authority).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, labor_market_protection_principle).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_system_sustainability_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain legislative veto over welfare access rules for non-citizen residents. Set contribution thresholds, waiting periods, and categorical exclusions. Can invoke 'welfare system sustainability' to justify restrictions. Face electoral pressure from domestic labor constituencies to maintain closure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer the actual exclusion mechanisms: residence tests, contribution histories, categorical eligibility rules. Benefit from preserved fiscal autonomy and administrative control. Their institutional coherence depends on maintaining boundaries.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, national_welfare_administrations, beneficiary).

% Domestic workers and unions in receiving states who gain wage-floor protection and reduced competition from mobile workers willing to accept lower standards. Their political organization makes them effective veto players at national level. Exit means accepting labor market competition they organized to avoid.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Workers who move across federation borders for employment but face restricted welfare access: waiting periods for unemployment benefits, exclusion from housing assistance, family benefit restrictions. Pay taxes and contributions but cannot access full beneficiary set. Exit options limited by career investment, family ties, and lack of portability.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Retirees, students, caregivers, job-seekers who move under free movement rights but fall outside 'worker' status. Face near-total welfare exclusion: no access to minimum income schemes, healthcare only via emergency or private insurance, housing exclusion. Cannot easily return (sunk migration costs) nor fully integrate (structural barriers).
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, economically_inactive_migrants, payer,
    powerless, biographical, trapped, continental).

% Workers living in one member state, employed in another. Caught between two welfare systems: contribute to host state system but reside in home state system. Face double-exclusion risk — neither system treats them as full beneficiaries. More mobile than settled migrants but structurally vulnerable to coordination failures.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_commuters, payer,
    moderate, immediate, mobile, regional).

% Commission, Court, Parliament that formally uphold free movement as fundamental right but practically accommodate member state welfare autonomy through directives, derogations, and case law. Their authority is measured by how much integration they can actually advance against member state resistance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, federation_institutions, observer,
    institutional, generational, analytical, continental).

% Civil society, legal scholars, mobile worker organizations arguing for full portability and non-discrimination. Structurally excluded from welfare rule-making; their victories are judicial (individual cases) not legislative (systemic change). Exit means accepting the sovereignty reading as settled.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, integration_advocates, excluded,
    moderate, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates fiscal sustainability of diverse national welfare systems within a federation that formally guarantees free movement. Solves the problem of how to prevent welfare tourism and adverse selection without formally abandoning mobility rights.
% TRANSFER_FUNCTION: Transfers fiscal risk from national welfare systems to mobile populations: mobile workers and economically inactive migrants bear the cost of exclusion (reduced benefits, insurance gaps, administrative burden) so that national systems avoid cross-border fiscal exposure.
% ABSENT_VOICES: Third-country nationals legally resident in member states who face even stricter exclusion but have no free movement rights to invoke. Future generations of mobile workers whose rights are being negotiated now without their representation. Small member states that depend on out-migration and remittances but lack veto power.
% DISAPPEARANCE_RATIONALE: If member state welfare closure authority vanished overnight, national systems would face immediate cross-border fiscal pressure. Some would restrict eligibility universally (harming domestic poor), others would seek federal fiscal transfers, and free movement would become genuinely portable — triggering political crisis in high-welfare states. The federation's current equilibrium depends on this constraint.
% FOUNDING_PROBLEM: Post-war European integration needed to reconcile free movement (economic integration) with nationally diverse welfare systems (social legitimacy). The founding compromise: mobility rights exist but welfare boundaries remain national. The 'worker' status became the gateway — those who work get mobility; those who don't work get national determination.
% FOUNDING_PROBLEM_CORROBORATION: Original treaty negotiations (Messina 1955, Rome 1957) record explicit member state insistence on welfare sovereignty. Contemporary corroboration: Commission's own 2020 report on free movement acknowledges 'persistent barriers' to welfare portability. European Court of Justice case law (Dano, Alimanovic) confirms member state discretion. Integration advocates dispute whether the founding problem still exists given economic convergence.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that mobile populations contribute fiscally but access reduced benefits — the gap between contribution and access is the extraction. Suppression (0.65) is substantial: the constraint persists through active legal and administrative barriers (waiting periods, residence tests, 'genuine prospect of work' tests) that would collapse without enforcement. Theater ratio (0.35) captures that the coordination function (fiscal sustainability, labor market protection) is real but increasingly overshadowed by exclusionary drift. Accessibility collapse (0.45) is moderate: alternatives exist (private insurance, return migration, federal reform) but are costly. Resistance (0.4) reflects ongoing judicial challenges and political contestation but limited structural change.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (member state legislatures), this is a necessary coordination mechanism preserving welfare diversity within a federation. From the payer seats (mobile workers, economically inactive migrants), it is enforced extraction justified by a coordination story that increasingly serves domestic political interests. The engine computes this divergence from the structural data — the declared beneficiaries and victims, their power levels, and their exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures and welfare administrations are structural beneficiaries (d ~ 0.15-0.25): they retain authority, control fiscal boundaries, and face minimal exit pressure. Receiving state labor forces are beneficiaries (d ~ 0.3): they gain protection but face demographic pressures that make closure increasingly costly. Mobile workers are targets (d ~ 0.7): they pay into systems they cannot fully access, with constrained exit due to career/family investment. Economically inactive migrants are full targets (d ~ 0.9): near-total exclusion, trapped by sunk costs. Cross-border commuters sit nearer symmetric (d ~ 0.5): dual exclusion but higher mobility. Federation institutions are analytical observers (d ~ 0.5). Integration advocates are excluded (d ~ 0.8): they bear advocacy costs with minimal structural influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling mobility with welfare diversity) was live in 1957. Today it is contested: economic convergence has reduced but not eliminated welfare differentials; demographic aging makes mobile workers fiscally net-contributors in many receiving states; yet the closure architecture has expanded rather than contracted. The constraint now extracts from populations that the founding compromise did not anticipate (retirees, cross-border commuters, third-country nationals). Mandatrophy is unresolved — the arrangement persists because dismantling it requires federal fiscal capacity that does not exist, and member states veto its creation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_closure,
    'Is national welfare closure authority a genuine structural necessity of federalism (mountain-like) or a constructed political choice that benefits identifiable agents (member state legislatures, domestic labor)?',
    'Counterfactual analysis: if federation had built portable welfare from inception (as some 1950s proposals suggested), would free movement have failed? Historical institutional analysis of path dependence vs. structural necessity.',
    'If genuine necessity, the constraint is closer to rope (coordination with unavoidable exclusion). If constructed choice, it is tangled_rope or snare — extraction dressed as coordination. FSM detection for false mountain claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_closure, conceptual, 'Whether welfare closure is structurally necessary or politically constructed').

omega_variable(
    fiscal_net_contribution_reality,
    'Are mobile workers in receiving states net fiscal contributors or net beneficiaries under current rules?',
    'Longitudinal fiscal incidence studies tracking lifetime contributions vs. benefits for mobile cohorts, disaggregated by age, skill, duration of stay.',
    'If mobile workers are net contributors, the ''welfare tourism'' justification collapses and extraction is pure. If net beneficiaries, the closure has some actuarial basis (though distributional questions remain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_net_contribution_reality, empirical, 'Actual fiscal balance of mobile populations vs. closure justification').

omega_variable(
    reading_relations_stability,
    'Do the three kernel readings (member_sovereignty_primary, integration_primary, selective_solidarity) represent stable equilibrium positions, or is one reading displacing the others?',
    'Track European Court of Justice doctrine evolution, legislative proposals, and public opinion trends over 5-10 years. Measure whether case law is converging toward one reading.',
    'If integration_primary is displacing member_sovereignty_primary, this constraint''s extractiveness is declining. If selective_solidarity is emerging as synthesis, extraction shifts from citizenship-status to contribution-history basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_stability, empirical, 'Dynamic stability of the kernel''s reading structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_ms_tr_t1957, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(fmo_ms_tr_t1970, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(fmo_ms_tr_t1985, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(fmo_ms_tr_t1992, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(fmo_ms_tr_t2004, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(fmo_ms_tr_t2010, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(fmo_ms_tr_t2015, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(fmo_ms_tr_t2020, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2020, 0.34).
narrative_ontology:measurement(fmo_ms_tr_t2024, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(fmo_ms_be_t1957, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1957, 0.3).
narrative_ontology:measurement(fmo_ms_be_t1970, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(fmo_ms_be_t1985, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(fmo_ms_be_t1992, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(fmo_ms_be_t2004, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(fmo_ms_be_t2010, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(fmo_ms_be_t2015, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(fmo_ms_be_t2020, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(fmo_ms_be_t2024, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fmo_ms_su_t1957, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1957, 0.4).
narrative_ontology:measurement(fmo_ms_su_t1970, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(fmo_ms_su_t1985, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(fmo_ms_su_t1992, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(fmo_ms_su_t2004, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(fmo_ms_su_t2010, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(fmo_ms_su_t2015, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(fmo_ms_su_t2020, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement(fmo_ms_su_t2024, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, eu_citizenship_rights_portability).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, national_welfare_system_sustainability).

% DUAL FORMULATION NOTE:
% This constraint family (federation_membership_obligations) decomposes the single kernel into three readings with distinct ε values and beneficiary/victim structures. member_sovereignty_primary has the highest extractiveness (0.58) because it excludes the broadest population; integration_primary would have lower extractiveness (near 0.2) but higher suppression (federal override of national welfare); selective_solidarity sits between (~0.4) with contribution-based tiering. The ε-invariance principle requires separate stories because the referent (the standing arrangement) is assessed differently by each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, institutional, 0.2).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, organized, 0.3).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, moderate, 0.7).
constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
