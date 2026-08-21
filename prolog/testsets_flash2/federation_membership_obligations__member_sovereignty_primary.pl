% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Sovereignty Primary in EU Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty primary' reading of EU
 *   federation membership obligations. It asserts that national welfare
 *   states retain the authority to control access to their social security
 *   systems, making free movement conditional on protecting national labor
 *   markets and ensuring welfare system sustainability. This reading
 *   prioritizes national fiscal and social cohesion over deeper EU
 *   integration in social policy, leading to policies that restrict welfare
 *   access for mobile EU citizens and non-contributory migrants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.45).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.6).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Sovereignty Primary in EU Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '67b2c76d-3c4d-434c-b548-9331b7fb76e4').
narrative_ontology:cs_kernel_codification('67b2c76d-3c4d-434c-b548-9331b7fb76e4', formalized).
narrative_ontology:cs_authority_grounding('67b2c76d-3c4d-434c-b548-9331b7fb76e4', lineage).
narrative_ontology:cs_interpretation_layer_present('67b2c76d-3c4d-434c-b548-9331b7fb76e4').
narrative_ontology:cs_reading_relation('67b2c76d-3c4d-434c-b548-9331b7fb76e4', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('67b2c76d-3c4d-434c-b548-9331b7fb76e4', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('67b2c76d-3c4d-434c-b548-9331b7fb76e4', foundational, national_welfare_sovereignty).
narrative_ontology:cs_axiom_status(national_welfare_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('67b2c76d-3c4d-434c-b548-9331b7fb76e4', national_welfare_sovereignty, conventional).
narrative_ontology:cs_axiom('67b2c76d-3c4d-434c-b548-9331b7fb76e4', foundational, labor_market_protection_priority).
narrative_ontology:cs_axiom_status(labor_market_protection_priority, holdable).
narrative_ontology:cs_axiom_grounding('67b2c76d-3c4d-434c-b548-9331b7fb76e4', labor_market_protection_priority, instrumental).
narrative_ontology:cs_reference_frame('67b2c76d-3c4d-434c-b548-9331b7fb76e4', westphalian_welfare_state_model).
narrative_ontology:cs_drift_state('67b2c76d-3c4d-434c-b548-9331b7fb76e4', contemporary_eu_integration_pressure, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('67b2c76d-3c4d-434c-b548-9331b7fb76e4', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_labor_forces).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_citizens_seeking_welfare).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, non_contributory_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These governments prioritize the fiscal sustainability of their national welfare systems and the protection of their domestic labor markets. They actively enforce policies that restrict welfare access for mobile EU citizens who have not made sufficient contributions or are not economically active, viewing this as a core aspect of national sovereignty within the federation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from policies that limit competition from mobile workers in certain sectors and protect the integrity of national social security systems. They support their governments' efforts to ensure that free movement does not undermine national employment standards or welfare provisions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% These individuals face restrictions on accessing welfare benefits in host member states, often requiring proof of economic activity or sufficient resources. They bear the direct cost of these limitations, which can lead to precarity and exclusion, despite their EU citizenship status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_citizens_seeking_welfare, payer,
    powerless, immediate, constrained, regional).

% These migrants, often without a history of contributions to the host state's welfare system, are explicitly targeted by restrictive policies. Their access to social support is severely limited, making them highly vulnerable and effectively trapped in a state of exclusion if they cannot secure employment.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, non_contributory_migrants, payer,
    powerless, immediate, trapped, local).

% The Commission generally advocates for deeper integration and the full realization of free movement rights, including non-discriminatory access to welfare. However, under this reading, its authority to challenge national restrictions is constrained by member states' assertions of sovereignty over welfare policy, often leading to protracted legal disputes.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_commission, excluded,
    institutional, generational, constrained, continental).

% Interprets EU law regarding free movement and welfare access. While it has historically expanded rights, this reading emphasizes the limits of its jurisdiction when national welfare sustainability is invoked, leading to complex and often ambiguous rulings that reflect the ongoing tension between national sovereignty and EU integration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to coordinate their national welfare policies with the principle of free movement, ensuring that the latter does not destabilize the former, thereby maintaining political support for the overall federal project.
% TRANSFER_FUNCTION: Limits the transfer of welfare benefits from host member states to mobile EU citizens and non-contributory migrants, effectively retaining resources within national systems for national populations.
% ABSENT_VOICES: Advocacy groups for migrant rights and proponents of a more integrated European social union are often marginalized in national policy debates, where the focus remains on national interests and fiscal prudence. They would argue for universal access to welfare based on residency, not nationality or contribution history.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, member states would face immediate and significant pressure on their welfare systems, potentially leading to fiscal crises and political backlash. The balance of power within the EU regarding social policy would shift dramatically towards integration, and national governments would need to fundamentally rethink their social contracts.
% FOUNDING_PROBLEM: The original problem was how to reconcile the economic benefits of free movement with the political and fiscal realities of national welfare states, preventing 'welfare tourism' and ensuring the sustainability of social security systems.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and national electorates consistently attest that the problem of welfare sustainability under free movement is live and pressing. Independent economic analyses and public opinion surveys from outside the direct beneficiaries also corroborate the ongoing political salience and perceived fiscal challenges, even if the severity is debated.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).
:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the interests of member states in maintaining their welfare systems while simultaneously extracting from mobile citizens who are denied full access. Extractiveness (0.45) reflects the significant costs borne by those excluded from benefits. Suppression (0.6) is moderate, as member states actively enforce these restrictions through legal and administrative means, but there is also ongoing legal challenge and political debate. Theater ratio is low (0.2), indicating that the stated justification (welfare sustainability) is largely genuine, though it also serves to legitimize the extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member state governments, this constraint is a necessary Rope, ensuring the stability of their welfare systems. From the perspective of mobile citizens, it operates as a Snare, denying them benefits despite their EU citizenship. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and national labor forces are beneficiaries, as the constraint protects their interests and resources (low directionality). Mobile EU citizens and non-contributory migrants are targets, bearing the costs of restricted access (high directionality). The European Commission and Court of Justice, while institutional actors, experience this constraint as a limitation on their integrationist agenda, making them observers or excluded parties in this specific framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_threshold,
    'What is the empirically verifiable threshold at which mobile worker welfare access genuinely threatens national welfare state fiscal sustainability?',
    'Independent, cross-national econometric studies modeling the long-term fiscal impact of different welfare access regimes for mobile EU citizens, controlling for economic cycles and demographic shifts.',
    'If the current restrictions are found to be far below the actual fiscal threat threshold, the ''sustainability'' justification would be weakened, potentially reclassifying the constraint as more extractive. If the threat is real and imminent, it would strengthen the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_threshold, empirical, 'Empirical basis for welfare sustainability claims.').

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the assertion of national sovereignty over welfare policy a fundamental, irreducible principle, or a political choice that could be re-negotiated in favor of deeper EU social integration?',
    'A conceptual analysis of federalism and sovereignty in multi-level governance, examining whether the ''member sovereignty primary'' reading is a logical necessity or a contingent political preference. This would involve comparing constitutional theories and historical precedents.',
    'If framed as an irreducible principle, the constraint''s ''naturalness'' (in a political sense) would be higher, making it harder to challenge. If framed as a contingent choice, it opens pathways for political contestation and re-negotiation, potentially lowering its perceived legitimacy and increasing resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'Conceptual framing of national sovereignty in EU welfare policy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, administrative hurdles) or internalized (mobile citizens'' self-exclusion due to perceived stigma or complexity)?',
    'Post-exit suppression trajectory: if mobile citizens continue to avoid seeking welfare even after legal barriers are reduced, reclassify as partially internalized. Surveys on perceived barriers vs. actual legal restrictions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — mobile citizens carry the suppression with them, making exit harder even if formal rules change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mobile citizens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.19).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership_obligations' kernel. It represents the 'member sovereignty primary' perspective, which prioritizes national control over welfare and labor markets within the EU framework. Other readings (integration_primary, selective_solidarity) offer alternative interpretations of the same core obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
