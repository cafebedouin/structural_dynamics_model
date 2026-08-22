% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement Supremacy Over Welfare Closure (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the integration_primary reading of the
 *   federation_membership_obligations kernel. Under this reading, free
 *   movement of workers and citizens is constitutive of EU citizenship and
 *   the single market; member state welfare boundaries are legally
 *   subordinate to mobility rights. The ECJ has progressively expanded this
 *   doctrine (Van Gend en Loos, Baumbast, Grzelczyk, Brey), shifting the
 *   arrangement from pure labor-market coordination toward federal
 *   citizenship entitlements. Mobile EU workers enter the full welfare
 *   beneficiary set in host states; receiving member states lose fiscal
 *   closure authority; and local labor in high-mobility regions bears
 *   adjustment costs in wages, housing, and public service access. The
 *   constraint coordinates a continental labor market but asymmetrically
 *   extracts fiscal and social adjustment costs from host populations and
 *   their representative states.
 *
 * KEY AGENTS:
 *   - mobile_eu_workers: Primary beneficiary (moderate/mobile) â gains cross-border welfare and labor access
 *   - displaced_local_labor: Primary target (powerless/constrained) â bears wage and service competition
 *   - receiving_member_states: Institutional target (institutional/constrained) â loses welfare closure authority
 *   - ecj: Agenda setter and secondary beneficiary (institutional/analytical) â expands authority via case law
 *   - cross_border_employers: Secondary beneficiary (powerful/arbitrage) â gains labor supply flexibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.7).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement Supremacy Over Welfare Closure (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '4bf4e076-91ed-4b0f-822e-81b662e657cf').
narrative_ontology:cs_kernel_codification('4bf4e076-91ed-4b0f-822e-81b662e657cf', formalized).
narrative_ontology:cs_authority_grounding('4bf4e076-91ed-4b0f-822e-81b662e657cf', lineage).
narrative_ontology:cs_interpretation_layer_present('4bf4e076-91ed-4b0f-822e-81b662e657cf').
narrative_ontology:cs_reading_relation('4bf4e076-91ed-4b0f-822e-81b662e657cf', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('4bf4e076-91ed-4b0f-822e-81b662e657cf', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('4bf4e076-91ed-4b0f-822e-81b662e657cf', foundational, free_movement_constitutive_of_union_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_union_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('4bf4e076-91ed-4b0f-822e-81b662e657cf', free_movement_constitutive_of_union_citizenship, conventional).
narrative_ontology:cs_axiom('4bf4e076-91ed-4b0f-822e-81b662e657cf', foundational, market_integration_requires_welfare_non_discrimination).
narrative_ontology:cs_axiom_status(market_integration_requires_welfare_non_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('4bf4e076-91ed-4b0f-822e-81b662e657cf', market_integration_requires_welfare_non_discrimination, instrumental).
narrative_ontology:cs_reference_frame('4bf4e076-91ed-4b0f-822e-81b662e657cf', treaty_based_market_integration).
narrative_ontology:cs_drift_state('4bf4e076-91ed-4b0f-822e-81b662e657cf', post_enlargement_citizenship_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4bf4e076-91ed-4b0f-822e-81b662e657cf', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, ecj).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise free movement rights to seek employment and residence across member states; upon lawful residence gain access to welfare benefits, housing support, and social advantages on equal terms with host-state nationals in receiving jurisdictions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Access an expanded continental labor pool without navigating separate national work permits or visa regimes; benefit from wage competition and labor supply flexibility in receiving states.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Interprets the EU Treaties to progressively expand the personal and material scope of free movement and non-discrimination; through preliminary rulings and infringement judgments it narrows member state welfare closure options and accumulates institutional authority over social policy.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, ecj, beneficiary).

% Faces intensified labor-market competition and wage pressure from incoming mobile workers in sectors with high labor mobility; bears adjustment costs in employment security, housing access, and public service quality without targeted compensation mechanisms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, constrained, national).

% Must open welfare systems and labor markets to mobile EU citizens; lose legal autonomy to impose residence requirements, nationality conditions, or priority clauses for domestic workers; face political backlash and fiscal costs while treaty withdrawal remains economically and politically prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_member_states, payer,
    institutional, generational, constrained, national).

% Monitors member state compliance with free movement and non-discrimination rules; initiates infringement proceedings against states that restrict welfare access or impose disproportionate residence requirements on mobile citizens.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Political and civil-society formations that argue for national welfare closure and labor-market priority for domestic residents; structurally excluded from the EU legal framework which treats mobility as a fundamental freedom overriding such closure claims.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, welfare_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a continental labor market by removing national barriers to worker mobility and preventing welfare-state closure from undermining market integration; creates a unified citizenship space where individuals can change member state without losing social protections.
% TRANSFER_FUNCTION: Transfers welfare access rights and social equality claims from closed national systems to mobile citizens; transfers labor-market adjustment costs and fiscal burdens from mobile workers and sending states to host-state labor markets and public budgets.
% ABSENT_VOICES: Welfare sovereignty advocates and local labor-protection movements are structurally excluded from the EU legal framework; their claims are treated as protectionist restrictions on fundamental freedoms rather than legitimate social-policy positions. Member state constitutional courts that resist ECJ supremacy are marginalized in the preliminary reference system.
% DISAPPEARANCE_RATIONALE: If free movement supremacy over welfare boundaries vanished overnight, member states would reimpose residence tests and nationality requirements within weeks, labor markets would renationalize, the ECJ would lose its primary engine of federal authority expansion, and the single market in labor would fragment into twenty-seven separate regimes.
% FOUNDING_PROBLEM: Post-war Western European economic fragmentation and low labor mobility; need to build an integrated market by preventing member states from protecting domestic labor and closing welfare systems to workers from partner states.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of the Treaties of Rome (e.g., Weiler, Davies) corroborate the economic-integration origin from outside the benefiting parties. Displaced local labor and receiving member states attest that the citizenship-based welfare expansion was not the original problem being solved. ECJ jurists and the Commission assert continuity with an evolving telos; independent historical scholarship provides the extra-beneficiary corroboration.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored as substantial because the constraint systematically transfers welfare costs and labor-market competition risks to host-state populations without commensurate federal fiscal compensation. Suppression (0.70) is high because the arrangement requires ongoing ECJ enforcement and treaty supremacy to override member-state resistance and legal closure attempts. Theater ratio (0.25) is moderate: EU citizenship rhetoric carries performative federalist elements, but the underlying legal machinery produces real coordination effects. Accessibility collapse (0.60) reflects that lawful alternatives (welfare residence tests, labor-market priority clauses) have been progressively barred by ECJ doctrine, though political resistance keeps alternatives rhetorically alive. Resistance (0.55) captures sustained member-state pushback, safeguard clauses, and Brexit as a high-cost exit event. The measurement series trace rising extraction and enforcement from 1957 to 2024, reflecting the shift from economic-market coordination to citizenship-based entitlement.
 *
 * PERSPECTIVAL GAP:
 *   From the ECJ seat, the constraint is the natural evolution of an autonomous legal order securing fundamental freedoms. From the mobile worker seat, it is a rights-bearing entitlement. From the receiving member state and local labor seats, it is an externally imposed fiscal and labor-market shock. The engine computes these divergences from the same structural data: identical legal rules produce opposed experiential signatures depending on power, exit options, and beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and cross-border employers are structural beneficiaries: the constraint subsidizes their mobility and labor access, placing their directionality near the beneficiary pole (low d). The ECJ, as the enforcing and interpreting authority, also occupies the beneficiary side through institutional authority accumulation. Receiving member states and displaced local labor are the structural targets: they bear the fiscal and labor-market costs of openness without commensurate offset. Their directionality sits near the target pole (high d). The member states' institutional power moderates their d slightly relative to local labor, but their exit is constrained by treaty commitments and the economic cost of withdrawal.
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination function â a continental labor market without fragmented national barriers â prevents classifying this as a snare. The welfare-access and non-discrimination rules do solve a real collective-action problem (race-to-the-bottom in labor standards, welfare-state closure undermining mobility). However, the asymmetric extraction prevents classifying it as a rope: the costs of adjustment are not pooled or compensated at the federal level, but concentrated on host-state labor and budgets. The Tangled Rope classification captures this hybridity. If the federal level introduced full fiscal equalization or mobility adjustment funds, the extraction would damp toward Rope; if the coordination story were entirely cover for dismantling welfare states, it would shift toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a reading of federal integration (integration_primary), intergovernmental coordination (member_sovereignty_primary), or contributory tiering (selective_solidarity)?',
    'Comparative legal analysis of ECJ case law trajectory versus member state constitutional court reservations (e.g., German Bundesverfassungsgericht, Danish Supreme Court) and treaty revision history.',
    'If member_sovereignty_primary or selective_solidarity were adopted as the operative reading, the beneficiary/victim structure would invert or tier, and the directionality of receiving member states would shift toward beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the federation membership obligations kernel is structurally operative').

omega_variable(
    welfare_portability_scope,
    'Does EU citizenship require full equal treatment in all welfare benefits, or only in benefits linked to economic activity and employment?',
    'ECJ ruling cascade analysis: a narrowing trend in post-Brey jurisprudence would indicate partial scope; continued expansion into non-contributory benefits would confirm full portability.',
    'If scope is limited to employment-linked benefits, extractiveness falls and the constraint moves toward a purer coordination mechanism; if scope includes all social assistance, extraction from host state budgets rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_portability_scope, empirical, 'Scope of welfare portability under free movement').

omega_variable(
    displacement_mechanism,
    'Is local labor displacement caused by free movement itself, or by separable failures in housing and industrial policy in receiving regions?',
    'Regional econometric analysis comparing high-mobility regions with comparable low-mobility regions, controlling for housing supply and sectoral composition.',
    'If displacement is separable from mobility, part of the measured extraction is attributable to domestic policy failure rather than the constraint itself, lowering epsilon for the mobility rule pure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_mechanism, empirical, 'Attribution of local labor displacement to free movement vs domestic policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 67).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmi_ip_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fmi_ip_tr_t10, federation_membership_obligations__integration_primary, theater_ratio, 10, 0.07).
narrative_ontology:measurement(fmi_ip_tr_t20, federation_membership_obligations__integration_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fmi_ip_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.13).
narrative_ontology:measurement(fmi_ip_tr_t40, federation_membership_obligations__integration_primary, theater_ratio, 40, 0.16).
narrative_ontology:measurement(fmi_ip_tr_t50, federation_membership_obligations__integration_primary, theater_ratio, 50, 0.2).
narrative_ontology:measurement(fmi_ip_tr_t60, federation_membership_obligations__integration_primary, theater_ratio, 60, 0.23).
narrative_ontology:measurement(fmi_ip_tr_t67, federation_membership_obligations__integration_primary, theater_ratio, 67, 0.25).

% Extraction over time
narrative_ontology:measurement(fmi_ip_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fmi_ip_be_t10, federation_membership_obligations__integration_primary, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(fmi_ip_be_t20, federation_membership_obligations__integration_primary, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(fmi_ip_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(fmi_ip_be_t40, federation_membership_obligations__integration_primary, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(fmi_ip_be_t50, federation_membership_obligations__integration_primary, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(fmi_ip_be_t60, federation_membership_obligations__integration_primary, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(fmi_ip_be_t67, federation_membership_obligations__integration_primary, base_extractiveness, 67, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fmi_ip_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fmi_ip_su_t10, federation_membership_obligations__integration_primary, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(fmi_ip_su_t20, federation_membership_obligations__integration_primary, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(fmi_ip_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(fmi_ip_su_t40, federation_membership_obligations__integration_primary, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(fmi_ip_su_t50, federation_membership_obligations__integration_primary, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(fmi_ip_su_t60, federation_membership_obligations__integration_primary, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(fmi_ip_su_t67, federation_membership_obligations__integration_primary, suppression_requirement, 67, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% The federation_membership_obligations kernel decomposes into three structurally distinct readings: integration_primary (this file), member_sovereignty_primary, and selective_solidarity. Each reading carries a different epsilon, beneficiary/victim structure, and directionality profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
