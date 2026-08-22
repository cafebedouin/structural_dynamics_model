% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership: Member State Sovereignty Over Labor Market Protection
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the federation-membership
 *   treaty: the sovereignty-primary reading, in which member states retain
 *   authority to restrict labor mobility and protect national labor markets
 *   and welfare systems. This is distinct from the integration-primary
 *   reading (which treats free movement as constitutive and presumes
 *   restrictions illegitimate) and the subsidiarity-balance reading (which
 *   permits state restrictions only when proportional to legitimate
 *   interests). Under the sovereignty-primary reading, the default is state
 *   consent; workers and service providers must justify access, not states
 *   must justify restriction. The beneficiary set includes incumbent workers
 *   in high-wage states and welfare-system administrators; the victim set
 *   includes mobile workers from lower-wage regions facing conditional
 *   access. Suppression is high (0.72) because the constraint's persistence
 *   depends actively on enforcing work-permit systems, quota mechanisms, and
 *   residency requirements—not merely on neutral market forces. Theater ratio
 *   (0.41) reflects a mix: genuine welfare-protection function, combined with
 *   incumbent-worker protection that may be more expensive to achieve than
 *   necessary and increasingly justified post-hoc.
 *
 * KEY AGENTS:
 *   - incumbent_national_labor_forces: Primary beneficiary; protected from wage competition; power concentrated in wealthy-state unions
 *   - welfare_system_funding_states: Agenda-setter; controls the sovereignty gate; institutional power to set labor-access terms
 *   - mobile_workers_from_lower_wage_regions: Primary victim; powerless, identity-locked to origin region; face conditional access
 *   - national_regulatory_authorities: Enforce the gate; retain primary authority over labor-market regulation
 *   - federation_central_authority: Observer; lacks unilateral override on state consent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.72).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership: Member State Sovereignty Over Labor Market Protection").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '5b7e9ca6-786e-44b8-8050-5d463b0ca354').
narrative_ontology:cs_kernel_codification('5b7e9ca6-786e-44b8-8050-5d463b0ca354', formalized).
narrative_ontology:cs_authority_grounding('5b7e9ca6-786e-44b8-8050-5d463b0ca354', lineage).
narrative_ontology:cs_interpretation_layer_present('5b7e9ca6-786e-44b8-8050-5d463b0ca354').
narrative_ontology:cs_reading_relation('5b7e9ca6-786e-44b8-8050-5d463b0ca354', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('5b7e9ca6-786e-44b8-8050-5d463b0ca354', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('5b7e9ca6-786e-44b8-8050-5d463b0ca354', foundational, member_state_regulatory_authority_primary).
narrative_ontology:cs_axiom_status(member_state_regulatory_authority_primary, holdable).
narrative_ontology:cs_axiom_grounding('5b7e9ca6-786e-44b8-8050-5d463b0ca354', member_state_regulatory_authority_primary, deontological).
narrative_ontology:cs_axiom('5b7e9ca6-786e-44b8-8050-5d463b0ca354', foundational, labor_market_protection_justifies_mobility_gates).
narrative_ontology:cs_axiom_status(labor_market_protection_justifies_mobility_gates, holdable).
narrative_ontology:cs_axiom_grounding('5b7e9ca6-786e-44b8-8050-5d463b0ca354', labor_market_protection_justifies_mobility_gates, empirically_contingent).
narrative_ontology:cs_reference_frame('5b7e9ca6-786e-44b8-8050-5d463b0ca354', sovereign_state_labor_market_authority).
narrative_ontology:cs_drift_state('5b7e9ca6-786e-44b8-8050-5d463b0ca354', contemporary_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b7e9ca6-786e-44b8-8050-5d463b0ca354', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, incumbent_national_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_funding_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_regulatory_authorities).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers_from_lower_wage_regions).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_service_providers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, high_skill_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, high_skill_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Local workers in higher-wage member states whose employment and wage levels are protected by restrictions on intra-federation labor mobility. They benefit from reduced wage competition and preserved bargaining power. Exit for them means relocating to lower-wage regions or accepting labor market restructuring — both costly.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, incumbent_national_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% Member states with generous welfare systems and aging populations that retain authority to limit access by migrant workers, thereby protecting per-capita welfare expenditure and reducing benefit-cost ratios. They administer the sovereignty-based gate.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, welfare_system_funding_states, beneficiary,
    institutional, generational, constrained, national).

% National governments that set membership conditions and enforce labor market protections via work permits, residency requirements, and social-benefit eligibility thresholds. They retain the primary authority to define access and set the terms of federation participation for their markets.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_regulatory_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Workers from lower-wage federation members seeking employment in higher-wage states face approval gates, quota systems, and welfare-access restrictions. Their exit is constrained by limited domestic opportunity; identity is locked to regional origin, which gates their access. They bear the cost of labor market segmentation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers_from_lower_wage_regions, payer,
    powerless, biographical, identity_locked, continental).

% Self-employed professionals and business operators whose service delivery across borders is conditional on member state consent. They navigate inconsistent regulatory recognition, work-permit requirements, and nationality-based restrictions on scope of practice.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_service_providers, payer,
    moderate, biographical, constrained, continental).

% Highly credentialed workers (physicians, engineers, researchers) who often secure work before mobility, giving them negotiating advantage and exit options. They may extract favorable terms from employers desperate for their skills, but remain conditionally dependent on state approval and face uncertainty about long-term residency and career portability.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, high_skill_migrants, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, high_skill_migrants, beneficiary).

% The federation's coordinating institutions (commission, court, parliament if applicable) observe and adjudicate disputes over the balance between free movement and state sovereignty, but lack unilateral power to override member state consent on labor market regulation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_central_authority, observer,
    institutional, generational, analytical, global).

% Multinational corporations, construction firms, and knowledge-intensive service sectors whose operational models depend on cross-border talent mobility. They are structurally excluded from the sovereignty gate-setting but bear the cost of restricted access to labor pools.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, business_sectors_requiring_mobility, excluded,
    powerful, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, incumbent_national_labor_forces).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federation membership around a shared labor market framework while preserving state authority to protect domestic labor-market stability, pension-fund sustainability, and welfare-system fiscal health. Solves the problem: how can federation members gain economic integration benefits while maintaining democratic control over local employment conditions.
% TRANSFER_FUNCTION: Transfers regulatory authority over labor access from a federation-wide mobility presumption to member states. Moves labor-allocation decisions from markets toward national governments. In practice: surplus rents from employment access flow to incumbent workers and welfare-state administrators; costs accrue to would-be migrants and service providers facing access restrictions.
% ABSENT_VOICES: Mobile workers from lower-wage regions have no formal seat in the sovereignty gate-setting (no vote on permit systems, quota levels, or welfare-eligibility thresholds). Business sectors requiring cross-border talent are similarly excluded from the rule-making. Their objections are registered only through lobbying and litigation, not participation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished (free movement became presumptively unrestricted), labor would reallocate toward higher-wage regions within months, incumbent workers in wealthy states would face wage pressure, welfare-system per-capita costs would shift, and national governments would lose primary regulatory authority over employment access. Member states would renegotiate federation terms immediately.
% FOUNDING_PROBLEM: After federation formation, wealthy member states faced the prospect of labor inflows that would compete with incumbent workers and strain welfare systems funded on the assumption of closed labor markets. The sovereignty clause was negotiated to protect national labor-market policy autonomy while federation benefits remained available.
% FOUNDING_PROBLEM_CORROBORATION: National governments, labor unions, and welfare-system administrators in wealthy states attest the problem remains live: pressure from lower-wage region workers seeking access, tension between federation economic benefits and national labor-market protection. Independent economists and policy analysts from outside the benefiting states acknowledge the trade-off is real but contest whether the current restriction level is necessary to achieve the protection goal.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers surplus from mobile workers and service providers to incumbent workers and welfare administrations; the transfer is not matched by marginal service cost or genuine collective-action problem requiring restriction at this intensity. Suppression (0.72) is high because active enforcement machinery (work permits, quota systems, residency tracking, welfare-eligibility verification) must be continuously operated to maintain the gate; without enforcement, labor would reallocate toward higher-wage regions. Theater ratio (0.41) indicates substantial performative maintenance: the constraint is justified as welfare-system protection and labor-market stability, and these functions are real, but the degree of restriction often exceeds what strict welfare-protection logic would require—political protection of incumbent workers is bundled with system-protection narratives. Accessibility collapse (0.58, moderate) reflects that alternatives exist but are costly: lower-wage workers could relocate within their regions, businesses could invest in automation or relocate operations, but these alternatives are constrained and expensive. Resistance (0.71, high) reflects substantial pushback from mobile workers, business sectors dependent on cross-border talent, and lower-wage member states whose workers face barriers.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (incumbent workers, welfare administrators, national regulatory authorities) experience this as legitimate sovereign authority exercising justified protection of shared public goods. The victim seats (mobile workers, service providers) experience this as exclusionary restriction of movement and opportunity. The federation central authority observes that the sovereignty-primary reading privileges state-level preferences over individual mobility, which differs from the integration-primary reading that would privilege mobility. This divergence is structural, not merely evaluative: from the sovereign-state seat, the constraint is the baseline arrangement protecting legitimate interests; from the mobile-worker seat, the constraint is an imposed barrier requiring justification for access. The engine computes this seat divergence from the structural data—the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent workers and welfare administrators are structural beneficiaries (d toward 0.0): the constraint protects their interests, they did not initiate exit, they have moderate-to-strong power to defend the status quo. Mobile workers from lower-wage regions are structural targets (d toward 1.0): the constraint extracts from their opportunity set, they face identity-locked exit (origin-region identity gates their access), they are powerless as individuals and lack representation in the gate-setting. High-skill migrants sit at an intermediate position (d~0.4): they benefit from scarcity-driven wage premiums (fewer competitors), but face conditional access and long-term insecurity, so they bear extraction costs despite skill advantage. Business sectors are trapped outside the arrangement—they cannot participate in gate-setting but bear compliance costs and labor-access constraints (d effectively 1.0 at the margin, but excluded entirely from seat count). The directionality derivation from beneficiary/victim + exit confirms this alignment.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is CONTESTED in this case. The sovereignty-primary reading asserts the mandate is live: member states genuinely need labor-market protection to sustain welfare systems and democratic control. The integration-primary reading asserts the mandate is dead: modern federation markets self-regulate wages without requiring state gates, and the real function is incumbent-worker protection, not welfare sustainability. The subsidiarity-balance reading occupies the middle ground: the mandate is partially live but overstated—proportionality tests would permit some national restrictions (for cyclical unemployment or rapid migration surges) but not the standing gate. This constraint's classification as Tangled Rope (coordination + asymmetric extraction + active enforcement) does NOT resolve the mandatrophy question—it describes the structure, not adjudicate whether the function the structure purports to serve is still necessary. An omega variable captures this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_welfare_protection_necessity,
    'Is active labor-market gating genuinely necessary to protect welfare-system fiscal health and labor-market stability, or does the modern federation labor market self-regulate sufficiently through wage-and-employment adjustment mechanisms?',
    'Natural experiments from regional labor markets with lower mobility barriers (e.g., intra-national migration in federal systems): measure welfare-system sustainability and employment volatility. Compare federation regions with different restriction intensities. Econometric analysis of wage-depression and benefit-cost ratios under different mobility regimes.',
    'If welfare systems are self-sustaining without labor gating, the mandate is dead and the constraint shifts toward snare (pure extraction). If welfare-system sustainability genuinely depends on restricted mobility, the mandate remains live and the constraint is authentic Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_welfare_protection_necessity, empirical, 'Whether the labor-market gate is functionally necessary or economically optional.').

omega_variable(
    identity_lock_mechanism_internalization,
    'For mobile workers from lower-wage regions, is the suppression measured in the constraint primarily structural (legal barriers, economic exclusion) or internalized (workers internalize the origin-region identity as determining their access, persist in that identity after the legal barrier is removed)?',
    'Post-liberalization trajectory analysis: if restrictions were lifted in a test region, do workers from lower-wage regions rapidly fill available positions? Or do they persist in lower-wage regions despite legal barriers falling, indicating internalized identity-lock?',
    'If suppression is primarily structural, lifting barriers would enable rapid reallocation and the constraint''s effective suppression would fall sharply. If suppression is internalized, the constraint''s functional suppression persists even after legal barriers fall, indicating the identity-fusion mechanism is self-sustaining.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether identity-lock suppression is structural (removable by policy) or internalized (persisting after removal).').

omega_variable(
    sovereignty_reading_vs_integration_reading_foreclosure,
    'Does the sovereignty-primary reading logically foreclose the integration-primary reading, or do they remain coexistent positions held by different institutional coalitions?',
    'Examine whether a single member state could hold both readings simultaneously without internal contradiction (e.g., could a state assert both ''we have primary authority'' AND ''free movement is constitutive'')? If yes, they coexist. If no, foreclosure is present.',
    'If foreclosure obtains, one reading must eventually give way and the other will crystallize as institutional doctrine. If coexistence obtains, the readings will persist in ongoing tension and the federation will oscillate between interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_reading_vs_integration_reading_foreclosure, conceptual, 'Whether the sovereignty-primary and integration-primary readings are logically exclusive or merely institutionally opposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmts_sov_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fmts_sov_tr_t7, federation_membership_treaty__sovereignty_primary, theater_ratio, 7, 0.32).
narrative_ontology:measurement(fmts_sov_tr_t14, federation_membership_treaty__sovereignty_primary, theater_ratio, 14, 0.36).
narrative_ontology:measurement(fmts_sov_tr_t21, federation_membership_treaty__sovereignty_primary, theater_ratio, 21, 0.39).
narrative_ontology:measurement(fmts_sov_tr_t28, federation_membership_treaty__sovereignty_primary, theater_ratio, 28, 0.4).
narrative_ontology:measurement(fmts_sov_tr_t35, federation_membership_treaty__sovereignty_primary, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(fmts_sov_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(fmts_sov_be_t7, federation_membership_treaty__sovereignty_primary, base_extractiveness, 7, 0.56).
narrative_ontology:measurement(fmts_sov_be_t14, federation_membership_treaty__sovereignty_primary, base_extractiveness, 14, 0.61).
narrative_ontology:measurement(fmts_sov_be_t21, federation_membership_treaty__sovereignty_primary, base_extractiveness, 21, 0.65).
narrative_ontology:measurement(fmts_sov_be_t28, federation_membership_treaty__sovereignty_primary, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(fmts_sov_be_t35, federation_membership_treaty__sovereignty_primary, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmts_sov_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fmts_sov_su_t7, federation_membership_treaty__sovereignty_primary, suppression_requirement, 7, 0.63).
narrative_ontology:measurement(fmts_sov_su_t14, federation_membership_treaty__sovereignty_primary, suppression_requirement, 14, 0.67).
narrative_ontology:measurement(fmts_sov_su_t21, federation_membership_treaty__sovereignty_primary, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(fmts_sov_su_t28, federation_membership_treaty__sovereignty_primary, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(fmts_sov_su_t35, federation_membership_treaty__sovereignty_primary, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is part of the federation-membership-treaty family. All three readings (sovereignty_primary, integration_primary, subsidiarity_balance) decompose a single formalized kernel (the treaty's free-movement commitment) into three structurally distinct constraints with different beneficiary sets, extraction profiles, and ε values. The sovereignty_primary reading treats state regulatory authority as the primary value; integration_primary treats worker mobility as primary; subsidiarity_balance treats proportionality as primary. Each reading instantiates a different constraint with different structural properties. The family is linked via affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
