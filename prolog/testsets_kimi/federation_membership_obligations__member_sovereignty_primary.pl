% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Closure with Conditional Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the member_sovereignty_primary reading
 *   of the federation_membership_obligations kernel within federal political
 *   economy. It captures the structural arrangement whereby national welfare
 *   states retain authority to close or condition welfare access for mobile
 *   workers from other federation members, justified by the need to protect
 *   domestic labour markets and ensure welfare system sustainability. The
 *   constraint operates as a tangled rope: it genuinely coordinates the
 *   federal compact by preventing welfare tourism anxieties from collapsing
 *   political support for free movement, while asymmetrically extracting from
 *   mobile workers who are partially excluded from the social protections
 *   their tax contributions help fund. Member state legislatures retain veto
 *   authority over harmonisation, receiving-state labour forces enjoy
 *   protected market position, and mobile workers bear the cost of fragmented
 *   social citizenship.
 *
 * KEY AGENTS:
 *   - member_state_legislatures: Agenda setter (institutional/constrained) â retains veto authority, sets closure conditions
 *   - receiving_state_labor_forces: Primary beneficiary (organized/constrained) â protected from labour market competition
 *   - mobile_workers: Primary target (powerless/constrained) â excluded from full welfare despite contributions
 *   - eu_commission: Observer (institutional/analytical) â pushes integration, faces vetoes
 *   - mobile_worker_advocacy_groups: Excluded voice (moderate/constrained) â advocates for portability but shut out of veto process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.65).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Closure with Conditional Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, 'b2d2b900-04f6-49d8-abd3-51249995be0c').
narrative_ontology:cs_kernel_codification('b2d2b900-04f6-49d8-abd3-51249995be0c', formalized).
narrative_ontology:cs_authority_grounding('b2d2b900-04f6-49d8-abd3-51249995be0c', lineage).
narrative_ontology:cs_interpretation_layer_present('b2d2b900-04f6-49d8-abd3-51249995be0c').
narrative_ontology:cs_reading_relation('b2d2b900-04f6-49d8-abd3-51249995be0c', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('b2d2b900-04f6-49d8-abd3-51249995be0c', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('b2d2b900-04f6-49d8-abd3-51249995be0c', foundational, member_state_social_sovereignty).
narrative_ontology:cs_axiom_status(member_state_social_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b2d2b900-04f6-49d8-abd3-51249995be0c', member_state_social_sovereignty, conventional).
narrative_ontology:cs_axiom('b2d2b900-04f6-49d8-abd3-51249995be0c', foundational, federal_stability_requires_welfare_closure).
narrative_ontology:cs_axiom_status(federal_stability_requires_welfare_closure, holdable).
narrative_ontology:cs_axiom_grounding('b2d2b900-04f6-49d8-abd3-51249995be0c', federal_stability_requires_welfare_closure, instrumental).
narrative_ontology:cs_reference_frame('b2d2b900-04f6-49d8-abd3-51249995be0c', member_state_social_sovereignty).
narrative_ontology:cs_drift_state('b2d2b900-04f6-49d8-abd3-51249995be0c', post_enlargement_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2d2b900-04f6-49d8-abd3-51249995be0c', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, social_dumping_prevention).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_state_sustainability).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, member_state_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain veto authority over EU welfare harmonisation legislation and set residence or contribution conditions for welfare access by mobile workers, asserting closure authority to protect domestic labour markets and welfare system fiscal sustainability.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, constrained, continental).

% Domestic workers whose employment security and social wage levels are protected by restricting immediate full welfare access to intra-federation mobile workers, reducing downward competitive pressure on labour standards and maintaining insider-outsider labour market segmentation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Workers exercising intra-federation free movement who are partially or fully excluded from receiving-state welfare benefits despite residence and employment, bearing social risks while contributing to the receiving state's tax and social security base.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, national).

% Advances free movement and non-discrimination as fundamental federation principles; challenges member state closure measures through infringement procedures, but faces member state veto authority and political blocking in social policy harmonisation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_commission, observer,
    institutional, generational, analytical, continental).

% Advocate for full portability of social rights and non-discrimination for mobile workers, but are structurally excluded from the member state legislative veto processes that determine welfare closure and from the inter-governmental bargaining that sets federation membership obligations.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_worker_advocacy_groups, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents welfare system destabilisation and protects receiving-state labour market standards by allowing member states to condition welfare access on contribution history or residence requirements, thereby maintaining political support for the federal free movement compact among domestic electorates and preventing anti-federation backlash.
% TRANSFER_FUNCTION: Moves welfare protection and labour market security from mobile workers to receiving-state resident labour forces, while preserving fiscal and legislative autonomy for member state governments at the expense of portable social citizenship.
% ABSENT_VOICES: Mobile workers who have not yet exercised free movement but might, sending-state governments representing their fiscal and political interests, and EU federalist advocates arguing for full portability of social rights are structurally under-weighted in member state legislative veto and inter-governmental bargaining processes.
% DISAPPEARANCE_RATIONALE: If member states lost welfare closure authority overnight, receiving states would face immediate fiscal and political pressure to restrict migration or harmonise welfare upward; mobile workers would gain full access but labour markets and wage structures in wealthy receiving states would face competitive shocks; the federation's political compact would require renegotiation and likely see sovereigntist exit pressures surge.
% FOUNDING_PROBLEM: How to sustain a federal free movement area when member states have divergent welfare levels and labour markets, without triggering welfare tourism fears that would destabilise national social contracts and fuel anti-federation political backlash.
% FOUNDING_PROBLEM_CORROBORATION: Receiving-state governments and domestic labour unions attest the problem is live, citing fiscal sustainability and social dumping risks. The EU Commission and mobile worker advocates attest the problem is overstated and instrumentalised to legitimate exclusionary closure; independent fiscal studies from cross-national research bodies show modest welfare tourism effects, corroborating the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because mobile workers face systematic exclusion from means-tested benefits and restricted access to non-contributory welfare despite residence and employment. Suppression (0.65) reflects the active enforcement required: administrative residence tests, legislative vetoes against EU harmonisation proposals, and legal battles to maintain closure. Theater ratio (0.28) is moderate-low: while 'welfare tourism' rhetoric contains performative elements, the fiscal and labour-market closure mechanisms are functionally real. Accessibility collapse (0.45) is moderate: alternatives such as full federal welfare harmonisation exist in theory and are advocated by integration_primary readings, but are politically blocked by member state unanimity requirements. Resistance (0.55) is significant, coming from EU institutions, mobile worker advocates, and sending states. The measurements trace the post-enlargement intensification of closure (2004-2014) and subsequent partial normalisation.
 *
 * PERSPECTIVAL GAP:
 *   The member_state_legislature seat experiences the constraint as a necessary defence of democratic welfare state sovereignty and labour market stability â a coordination mechanism without which the federal free movement area would lose political legitimacy. The mobile_worker seat experiences the same structure as extraction: they contribute taxes and social security in the receiving state yet are denied full membership in its welfare community, facing greater social risk than stationary workers. The engine computes this divergence from the structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state labour forces are declared beneficiaries (low d) because the constraint subsidises their labour market position by reducing competition from mobile workers who, if fully insured, might accept riskier employment terms. Member state legislatures are agenda_setters with low-to-moderate d because the constraint amplifies their policy authority and fiscal autonomy, though they do not personally collect the extraction. Mobile workers are declared victims (high d) because the constraint specifically targets them for welfare exclusion when they exercise free movement rights; their constrained exit options (returning home or accepting precarity) place them near the full-target end. The EU Commission sits at analytical remove.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare is warranted because the constraint solves a genuine coordination problem: without some closure authority, asymmetric welfare levels across a federation with free movement create credible fiscal and political destabilisation risks. A snare classification would require the coordination story to be mere cover; here, the receiving-state labour protection and welfare sustainability concerns are structurally real. However, the asymmetric extraction from mobile workers is not merely incidental cost â it is a transfer function built into the arrangement. The piton classification is rejected because the constraint is actively maintained by concentrated beneficiaries (domestic labour forces, member state governments) who would lose political and economic advantages if it dissolved, and victims (mobile workers) who are increasingly organised in resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the member_sovereignty_primary reading remain a distinct structural position under sustained EU legal pressure, or does it collapse into selective_solidarity as a pragmatic fallback?',
    'Comparative case-law tracking: if member state closure authority is progressively narrowed to contributory thresholds only, the reading is collapsing into selective_solidarity; if veto authority over non-contributory benefits is defended as a core sovereignty line, it remains distinct.',
    'If collapsing, the constraint''s type may shift toward rope (less asymmetric extraction, more genuine coordination through tiered inclusion); if distinct, the tangled_rope classification holds with member state veto as the enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this kernel reading remains structurally distinct from selective_solidarity under legal evolution.').

omega_variable(
    welfare_tourism_empirical_basis,
    'Is welfare tourism a significant empirical phenomenon that threatens welfare state sustainability, or a constructed threat used to legitimate closure?',
    'Cross-national fiscal incidence studies comparing mobile worker welfare claims against their tax and social security contributions in receiving states; measurement of net fiscal impact per mobile worker cohort.',
    'If the net fiscal impact is negligible, the coordination justification weakens and the constraint shifts toward snare; if significant, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_tourism_empirical_basis, empirical, 'Whether the claimed fiscal threat is empirically substantial or rhetorically constructed.').

omega_variable(
    closure_as_coordination_price,
    'Is the extraction from mobile workers the necessary price of maintaining member state consent to the federal free movement area, or could the same political stability be achieved through federal transfer mechanisms?',
    'Natural experiment or comparative analysis of federations with and without welfare closure authority, controlling for inter-regional transfer magnitude and mobility levels.',
    'If federal transfers can substitute for closure, the constraint''s coordination function is separable from its extraction and the asymmetric transfer is pure rent; if not, the coordination and extraction are structurally entangled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(closure_as_coordination_price, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 8, 0.22).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 16, 0.35).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(fede_tr_t32, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 32, 0.32).
narrative_ontology:measurement(fede_tr_t40, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(fede_be_t32, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(fede_be_t40, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(fede_su_t32, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(fede_su_t40, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, selective_solidarity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_obligations kernel, instantiating the member_sovereignty_primary position against integration_primary and selective_solidarity siblings. The member sovereignty reading creates structural pressure that influences selective_solidarity tiering as a fallback, while coexisting with integration_primary as a live antagonist in EU political and legal discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
