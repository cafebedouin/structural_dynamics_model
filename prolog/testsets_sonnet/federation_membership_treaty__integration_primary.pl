% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of the Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the federation_membership_treaty
 *   kernel: the integration-primary reading, under which free movement is
 *   treated as constitutive of the single market itself rather than as a
 *   conditional grant from member states. Under this reading, national
 *   restrictions on labor mobility, residency-based welfare conditions, and
 *   local labor-market protections are presumptively illegitimate and survive
 *   judicial review only if narrowly tailored to a compelling justification
 *   unrelated to protecting the local market from competition. This produces
 *   a structurally distinct constraint from the sovereignty_primary reading
 *   (where restriction is the default and free movement is the exception
 *   requiring justification) and the subsidiarity_balance reading (where
 *   proportionality genuinely cuts both ways). The three readings are not
 *   measurement perspectives on one constraint; they are three different
 *   constraints with different beneficiary/victim structures and different ε
 *   values, linked as a kernel family.
 *
 * KEY AGENTS:
 *   - federal_court: Primary agenda-setter — administers and enforces the constitutive doctrine (institutional/analytical)
 *   - mobile_workers: Primary beneficiary — gains guaranteed continent-wide access (moderate/mobile)
 *   - cross_border_employers: Secondary beneficiary — gains frictionless labor sourcing (powerful/arbitrage)
 *   - local_labor_markets and low_wage_incumbent_workers: Primary victims — absorb wage compression and displacement (moderate-powerless/trapped)
 *   - national_welfare_systems: Institutional victim — bears fiscal strain from non-discrimination mandates (institutional/constrained)
 *   - member_state_governments: Constrained intermediary — cannot enact restrictive measures that would survive review (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.61).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.61).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of the Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '562abe0e-0faf-4a97-994b-a710c2464f2e').
narrative_ontology:cs_kernel_codification('562abe0e-0faf-4a97-994b-a710c2464f2e', formalized).
narrative_ontology:cs_authority_grounding('562abe0e-0faf-4a97-994b-a710c2464f2e', lineage).
narrative_ontology:cs_interpretation_layer_present('562abe0e-0faf-4a97-994b-a710c2464f2e').
narrative_ontology:cs_reading_relation('562abe0e-0faf-4a97-994b-a710c2464f2e', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('562abe0e-0faf-4a97-994b-a710c2464f2e', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('562abe0e-0faf-4a97-994b-a710c2464f2e', foundational, movement_default_is_permission_not_restriction).
narrative_ontology:cs_axiom_status(movement_default_is_permission_not_restriction, holdable).
narrative_ontology:cs_axiom_grounding('562abe0e-0faf-4a97-994b-a710c2464f2e', movement_default_is_permission_not_restriction, conventional).
narrative_ontology:cs_axiom('562abe0e-0faf-4a97-994b-a710c2464f2e', secondary, market_completion_requires_labor_mobility_parity_with_capital_and_goods).
narrative_ontology:cs_axiom_status(market_completion_requires_labor_mobility_parity_with_capital_and_goods, holdable).
narrative_ontology:cs_axiom_grounding('562abe0e-0faf-4a97-994b-a710c2464f2e', market_completion_requires_labor_mobility_parity_with_capital_and_goods, instrumental).
narrative_ontology:cs_reference_frame('562abe0e-0faf-4a97-994b-a710c2464f2e', constitutive_market_completion_doctrine).
narrative_ontology:cs_drift_state('562abe0e-0faf-4a97-994b-a710c2464f2e', post_enlargement_fiscal_strain_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('562abe0e-0faf-4a97-994b-a710c2464f2e', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, single_market_integration_project).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, low_wage_incumbent_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates disputes between member states and mobile citizens, applying a doctrine that treats free movement as constitutive of the market itself. Reviews national restrictions under strict proportionality and narrow-tailoring standards, striking down measures that fail. Its own institutional standing and the coherence of the treaty order both depend on maintaining this doctrine's primacy over competing readings.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_court, agenda_setter,
    institutional, generational, analytical, continental).

% Cross borders to work, study, or settle, relying on treaty guarantees that member states cannot easily condition or restrict their access to labor markets, housing, or services. Directly gain from every ruling that narrows the space for national gatekeeping.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without needing work-permit sponsorship or local hiring quotas, lowering recruitment costs and giving them leverage to source labor from wherever wages are lowest within the federation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% The treaty order's foundational commitment to an internal market without borders. Every ruling that treats free movement as constitutive rather than conditional deepens this project's legal entrenchment and forecloses future retreat.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, single_market_integration_project, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__integration_primary, single_market_integration_project).

% Absorb sustained wage compression and displacement in sectors exposed to inbound labor supply. Cannot vote, litigate, or bargain collectively against inflows framed as a constitutive treaty right rather than a policy choice open to local negotiation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    moderate, biographical, trapped, regional).

% Must extend benefits, healthcare access, and social insurance to incoming citizens under non-discrimination requirements, straining systems designed and funded around a national contribution base. Attempts to condition access on residency duration or contribution history face strict judicial scrutiny as presumptively illegitimate restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Compete directly with newly arrived mobile workers for entry-level and low-skill positions, bearing the sharpest wage and employment effects. Have no seat in treaty negotiation or judicial interpretation and limited capacity to relocate themselves in response.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, low_wage_incumbent_workers, payer,
    powerless, biographical, trapped, local).

% Face electorates demanding restriction of inbound migration and welfare access, but any national measure must survive the federal court's narrow-tailoring test, which most restrictive measures fail. Their democratic mandate to regulate movement is subordinated to the treaty's constitutive reading; exit from this constraint requires exit from the federation itself.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, excluded).

% Argue that free movement should be conditional on member-state consent and that states retain authority over labor markets and welfare eligibility. Their reading loses in the courts and in treaty interpretation, but persists as a live political and legal minority position that resurfaces at every renegotiation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, sovereignty_primary_reading_advocates, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable labor and residency market across member states, eliminating duplicated permitting regimes and enabling firms and workers to allocate labor to where it is most valued without per-border friction.
% TRANSFER_FUNCTION: Moves labor-market access and welfare-system entitlement from nationally bounded, locally negotiated allocations toward a continent-wide entitlement that mobile workers and their employers can invoke against local restriction; the corresponding wage-compression and fiscal costs move from being distributed by national democratic process to being absorbed by whichever local labor markets and welfare systems happen to receive inflows.
% ABSENT_VOICES: Local incumbent workers and national electorates who would prefer conditional or negotiated movement have no direct standing before the federal court; their preferences enter only indirectly through member-state governments whose restrictive measures are themselves subject to strict judicial review and routinely struck down.
% DISAPPEARANCE_RATIONALE: If the constitutive reading were displaced, member states would reintroduce labor-market tests, residency-based welfare conditions, and permit regimes; cross-border employers would lose frictionless labor sourcing; mobile workers would face renewed barriers; the single market's legal architecture would require substantial renegotiation.
% FOUNDING_PROBLEM: Post-war economic integration required dismantling protectionist barriers between states whose prior competitive devaluations and trade restrictions had deepened economic conflict; free movement of labor was designed to complete the internal market alongside free movement of goods, services, and capital.
% FOUNDING_PROBLEM_CORROBORATION: The federal court and integration-project institutions attest the founding problem remains live — market completion is ongoing and reversible without constitutive protection. Independent labor economists and several member-state parliamentary inquiries, sitting outside the beneficiary set, attest that the marginal integration gains from further movement liberalization have declined while distributive costs to local labor markets and welfare systems have become the dominant observed effect, suggesting the founding coordination problem is substantially solved and the constitutive doctrine now functions primarily as a rent-shielding and entrenchment mechanism for the reading itself.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects the real but asymmetric transfer: aggregate continental welfare gains are plausible, but the costs concentrate on specific local labor markets and low-wage incumbents while gains diffuse to mobile workers and employers broadly. Suppression is high (0.78) because the reading's defining feature is that it forecloses the normal democratic remedy — national restriction — by subjecting it to a legal test most restrictive measures cannot pass; this is suppression of a policy alternative, not merely friction. Theater is low (0.22) because the coordination function (a genuine frictionless internal market) is substantially real, not merely performed. Accessibility collapse (0.58) is moderate: national governments retain narrow avenues (public policy, public security, public health derogations) but these are narrowly construed, so most alternatives are foreclosed in practice. Resistance (0.71) is high, tracking the persistent political mobilization by sovereignty_primary advocates and periodic treaty renegotiation pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and cross-border employers sit near the beneficiary end: the constitutive reading directly expands their entitlements and lowers their transaction costs. Local labor markets, incumbent low-wage workers, and national welfare systems sit near the target end: they bear concentrated, often localized costs from a rule they had no direct voice in setting and cannot exit without exiting the federation itself. Member-state governments occupy an intermediate but constrained position — nominally sovereign, but their restrictive instruments are structurally disabled by the same doctrine, making their exit options effectively 'constrained' rather than 'mobile' despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dismantling protectionist barriers that had fueled inter-state economic conflict — was substantially solved decades into the treaty's operation. The constitutive doctrine's continued expansion (rising suppression_requirement over the measured interval) despite declining marginal integration gains is consistent with mandatrophy: a coordination mechanism whose original justification has weakened while its enforcement apparatus (judicial doctrine, narrow-tailoring review) has hardened. Classifying this as tangled_rope rather than snare or mountain matters here: there IS a genuine coordination function (a working continental labor market), which prevents mislabeling the whole arrangement as pure extraction; but the asymmetric victim set and active enforcement (judicial strike-down of restrictive national measures) prevent mislabeling it as pure Rope. The tangled_rope classification is the one that holds both truths simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_integration_primary,
    'This constraint instantiates the integration_primary reading of the federation_membership_treaty kernel — is this reading, rather than sovereignty_primary or subsidiarity_balance, the one that actually governs current treaty practice?',
    'Track the pattern of federal court rulings over time: a rising rate of struck-down national restrictions and narrowing derogation categories corroborates integration_primary as operative; a rising rate of upheld restrictions or expanding derogations would corroborate sovereignty_primary or subsidiarity_balance instead.',
    'If the operative reading shifts toward sovereignty_primary, the beneficiary/victim structure inverts — member states and local labor markets move toward beneficiary status, mobile workers and cross-border employers toward payer status — and this story''s ε, suppression, and classification would no longer apply; a new story would need to be authored for the operative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_integration_primary, conceptual, 'Whether integration_primary is the reading actually instantiated in current treaty practice, versus its two siblings.').

omega_variable(
    constitutive_vs_conditional_naturalness,
    'Is treating free movement as ''constitutive'' of the single market a discovery about what an internal market inherently requires, or a constructed doctrinal choice that could equally have been decided the other way at founding?',
    'Comparative federalism analysis: do other functioning internal markets (federal unions, customs unions) achieve comparable integration without treating labor mobility as constitutionally foreclosed to restriction? If yes, constitutive status is a choice, not a discovery.',
    'If constitutive status is shown to be a contingent doctrinal choice rather than a functional necessity, the presumptive illegitimacy of restriction loses its naturalized justification and the suppression score would be read as a policy choice rather than a structural requirement of market integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_conditional_naturalness, conceptual, 'Whether the constitutive framing is a functional necessity or a contestable interpretive choice among the kernel''s readings.').

omega_variable(
    diffuse_vs_concentrated_benefit_distribution,
    'Are the aggregate welfare gains from free movement large enough and broadly enough distributed to offset the concentrated costs borne by specific local labor markets, or does the aggregate case rest on averaging over communities that never see the offsetting gain?',
    'Disaggregated regional economic analysis comparing labor-market outcomes in high-inflow versus low-inflow regions over multiple business cycles, controlling for other regional shocks.',
    'If gains are genuinely diffuse and large, the tangled_rope classification''s coordination component is well-supported; if gains are concentrated among employers and mobile workers while costs concentrate on distinct incumbent populations, the extraction component dominates and a snare reading becomes more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_vs_concentrated_benefit_distribution, empirical, 'Whether the claimed aggregate welfare gain from free movement is real and broadly shared or a statistical artifact of averaging over non-overlapping winners and losers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__integration_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__integration_primary, theater_ratio, 16, 0.16).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__integration_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__integration_primary, theater_ratio, 32, 0.2).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__integration_primary, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__integration_primary, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__integration_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__integration_primary, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__integration_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__integration_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__integration_primary, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__integration_primary, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the federation_membership_treaty kernel. integration_primary (this story) treats free movement as constitutive with restriction presumptively illegitimate; sovereignty_primary treats free movement as conditional on state consent with restriction as the retained default; subsidiarity_balance treats both mobility and restriction as subject to a genuine two-way proportionality test. Each reading has its own ε, beneficiary/victim structure, and classification — they are not the same constraint measured three ways. The three form a constraint family; each links to the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
