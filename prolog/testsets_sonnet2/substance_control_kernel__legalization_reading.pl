% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Control Regime — Liberty/Externality (Legalization) Reading
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the substance control
 *   kernel: substance use itself is reframed as an individual liberty matter,
 *   and the state's legitimate role narrows to preventing and pricing
 *   third-party harm (impaired driving, secondhand exposure, underage sales)
 *   rather than punishing use or possession as such. Under this reading, the
 *   personal-use victim class of the prohibition reading is eliminated
 *   entirely — users become beneficiaries of a legal, taxed channel — while a
 *   new, narrower victim class emerges: the third parties who bear
 *   externality costs that the legal/tax apparatus only partially
 *   internalizes, plus informal-market participants displaced by licensing
 *   capital requirements. A legal industry and the state's tax apparatus
 *   emerge as concentrated beneficiaries not present, or present in very
 *   different form, in the other readings.
 *
 * KEY AGENTS:
 *   - recreational_users: beneficiary (moderate/mobile) — exits criminal victim class entirely
 *   - licensed_cannabis_alcohol_industry: agenda_setter/beneficiary (organized/arbitrage) — captures legal market rents
 *   - state_tax_revenue_agencies: beneficiary/agenda_setter (institutional/analytical) — collects and spends externality-enforcement revenue
 *   - dui_crash_victims and secondhand_exposure_bystanders: payer (powerless/trapped-constrained) — bear uncompensated externality costs
 *   - small_scale_informal_growers_priced_out: payer (powerless/trapped) — displaced by licensing capital barriers
 *   - communities_with_residual_black_market_violence: payer (powerless/trapped) — bear costs of persistent gray-market activity where tax/licensing barriers remain high
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Control Regime — Liberty/Externality (Legalization) Reading").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '46347038-483a-494a-a4e9-2057dd32d0ef').
narrative_ontology:cs_kernel_codification('46347038-483a-494a-a4e9-2057dd32d0ef', distributed).
narrative_ontology:cs_authority_grounding('46347038-483a-494a-a4e9-2057dd32d0ef', distributed).
narrative_ontology:cs_reading_relation('46347038-483a-494a-a4e9-2057dd32d0ef', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('46347038-483a-494a-a4e9-2057dd32d0ef', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('46347038-483a-494a-a4e9-2057dd32d0ef', foundational, harm_principle_bounds_state_coercion).
narrative_ontology:cs_axiom_status(harm_principle_bounds_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('46347038-483a-494a-a4e9-2057dd32d0ef', harm_principle_bounds_state_coercion, deontological).
narrative_ontology:cs_axiom('46347038-483a-494a-a4e9-2057dd32d0ef', foundational, externality_costs_are_the_only_legitimate_intervention_trigger).
narrative_ontology:cs_axiom_status(externality_costs_are_the_only_legitimate_intervention_trigger, holdable).
narrative_ontology:cs_axiom_grounding('46347038-483a-494a-a4e9-2057dd32d0ef', externality_costs_are_the_only_legitimate_intervention_trigger, instrumental).
narrative_ontology:cs_reference_frame('46347038-483a-494a-a4e9-2057dd32d0ef', harm_principle_liberty_baseline).
narrative_ontology:cs_drift_state('46347038-483a-494a-a4e9-2057dd32d0ef', post_legalization_wave_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('46347038-483a-494a-a4e9-2057dd32d0ef', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_revenue_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, recreational_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, regulated_retailers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, dui_crash_victims).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, small_scale_informal_growers_priced_out).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, communities_with_residual_black_market_violence).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, harm_principle_of_liberty).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, externality_internalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase substances through a regulated legal channel, pay embedded taxes, and are no longer criminally targeted for consumption or simple possession. Their exposure to arrest and record has collapsed; their remaining cost is the tax-inflated price and compliance with public-use rules.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, recreational_users, beneficiary,
    moderate, biographical, mobile, regional).

% Operates licensed cultivation, production, and retail under the legalization framework, lobbies for favorable licensing rules and tax structure, and captures the bulk of legal-market revenue. Shapes enforcement priorities toward protecting its licensed monopoly against unlicensed competition.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry, beneficiary).

% Collects excise and sales tax on legal substance transactions, funds enforcement of the externality boundary (DUI checkpoints, licensing inspections) partly from that revenue, and sets the tax rate that determines how much informal-market pressure remains.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_revenue_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_tax_revenue_agencies, agenda_setter).

% Bear the direct physical, financial, and legal costs of impaired-driving incidents involving legally purchased substances. They did not consent to the risk and have no exit from being on the same roads as impaired users; their only recourse is after-the-fact civil or criminal claims against the impaired party.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, dui_crash_victims, payer,
    powerless, immediate, trapped, local).

% Live in multi-unit housing, work in venues, or share public space where legal use produces secondhand smoke or vapor exposure. Zoning and use-location rules only partially separate their space from users; where enforcement is thin, their exposure is a direct uncompensated cost of the liberty framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders, payer,
    powerless, immediate, constrained, local).

% Operated in the pre-legalization informal economy and lack the capital, real estate, or clean legal record to obtain a license under the new regulated regime. Legalization did not amnesty or capitalize them; they are displaced by licensed operators while remaining exposed to residual enforcement if they continue selling unlicensed.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, small_scale_informal_growers_priced_out, payer,
    powerless, biographical, trapped, regional).

% Live where legal-market taxes and licensing barriers are high enough that an untaxed informal market persists alongside the legal one, along with its enforcement and violence dynamics. Legalization redirected but did not eliminate the illicit supply chain in their neighborhoods.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, communities_with_residual_black_market_violence, payer,
    powerless, generational, trapped, local).

% Operate storefronts under license, compete on price and service within the legal channel, and benefit from consumer trust and legal protection that unlicensed sellers lack. Face compliance costs but also legal certainty absent under prohibition.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, regulated_retailers, beneficiary,
    moderate, biographical, mobile, regional).

% Previously staffed and funded around prohibition enforcement; under the legalization reading their mandate narrows to externality policing (DUI, illegal sales, underage access). Their institutional preference for a broader enforcement mandate is not represented in the legalization reading's design and their objections are treated as legacy friction rather than a design input.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, prohibition_law_enforcement_agencies, excluded,
    institutional, biographical, constrained, national).

% Study use rates, externality incidence, and market structure following legalization, feeding data back into tax-rate and regulation debates without holding enforcement or revenue power themselves.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legal, regulated channel for substance production, sale, and consumption so that the state's coercive apparatus is redirected from punishing personal use toward the narrower, genuinely collective problem of externalities imposed on non-users (impaired driving, secondhand exposure, underage access).
% TRANSFER_FUNCTION: Moves consumption from the untaxed informal economy into a taxed and licensed legal economy: tax revenue flows from users and industry to the state; profit flows from consumers to licensed producers/retailers; uncompensated externality costs continue to flow from users to third parties who neither consented nor share in the industry's or state's gains.
% ABSENT_VOICES: Displaced informal-market growers and sellers who lack capital or clean records to obtain licenses are not represented in the legalization design process, which is typically driven by industry investors, tax planners, and liberty-framed advocacy groups. Communities bearing residual black-market violence are rarely consulted on tax-rate design, even though tax rate directly determines how much illicit supply persists.
% DISAPPEARANCE_RATIONALE: If the legalization reading's arrangement disappeared and prohibition-style controls returned, the licensed industry would be dismantled or driven underground, tax revenue streams would vanish, users would re-enter criminal exposure, and enforcement resources would have to be rebuilt for the broader prohibition mandate — a substantial institutional and economic rearrangement in either direction.
% FOUNDING_PROBLEM: Mass criminalization of personal substance use was producing enormous incarceration costs, racially disparate enforcement, and a large untaxed illicit market, while doing little to address the genuine third-party harms (impaired driving, underage access) that justify state intervention under a liberty framework.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminal-justice researchers and public-health epidemiologists outside the cannabis/alcohol industry corroborate that criminalization-driven incarceration and racial disparity were real and substantial; they diverge from industry and tax-agency messaging in reporting that current tax rates and licensing barriers in several jurisdictions are high enough to sustain a persistent illicit market, meaning the founding problem of an untaxed informal economy is only partially resolved rather than closed.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate rather than low — because although users exit the victim set, the tax-and-license structure creates a new concentrated beneficiary (the licensed industry) that captures rents disproportionate to the externality-management function it nominally funds, and because uncompensated third-party costs (DUI, secondhand exposure) persist without full internalization. Suppression is authored lower (0.35) than a prohibition reading would carry, reflecting that the coercive apparatus is now aimed narrowly at externality enforcement (DUI laws, underage sales, unlicensed sales) rather than at use itself — but it is not zero, because unlicensed competitors and informal-market participants are still actively suppressed to protect the licensed channel. Theater ratio (0.28) reflects that some 'public health' framing around tax revenue allocation is performative relative to the genuine externality-prevention function. All three time series share one grid (T=0 to T=20, six points) tracking the transition from residual prohibition-era enforcement intensity down to the mature legalization steady state.
 *
 * DIRECTIONALITY LOGIC:
 *   Recreational users, the licensed industry, and the state's tax agencies sit near the beneficiary end of directionality: users no longer bear criminal risk, industry captures profit, and the state captures revenue with reduced enforcement cost per capita. DUI crash victims and secondhand exposure bystanders sit near the full-target end: they are powerless, often trapped by locality, and bear costs they did not choose and are not compensated for through the tax structure in most jurisdictions. Informal-market growers and gray-market communities sit at a distinct target position — not victims of the constraint's coordination function per se, but casualties of its licensing/capital-barrier design, which the legalization reading does not treat as a first-order harm to be remedied.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading resolves one mandatrophy risk directly: continuing to punish personal use after evidence undermined the deterrence rationale would be a classic zombie mandate (dead founding problem, arrangement persists). By narrowing state intervention to externality prevention, this reading retires that zombie function. But it risks generating a new one: if tax/licensing rates are set to protect industry margins rather than to fund externality mitigation, the 'externality capture' justification can persist as cover for what is functionally industry rent protection — the omega on tax-rate capture below addresses this directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_internalization_completeness,
    'Does the tax and regulatory apparatus under legalization actually capture the full externality cost imposed by substance use (DUI, secondhand exposure, healthcare costs), or does it fall structurally short, leaving third parties as uncompensated residual victims?',
    'Compare aggregate excise tax and licensing revenue allocated to externality-mitigation programs (DUI enforcement, exposure remediation, health system costs) against independently estimated externality cost totals in mature legalization jurisdictions.',
    'If revenue substantially undershoots externality costs, the tangled_rope classification is reinforced — coordination function is real but asymmetric extraction persists via uncompensated third parties. If revenue matches or exceeds costs, the reading moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_internalization_completeness, empirical, 'Whether tax/licensing revenue actually internalizes externality costs or merely funds industry-protective enforcement.').

omega_variable(
    licensing_capture_vs_public_interest,
    'Is the licensing and tax-rate structure calibrated to public-interest goals (minimizing externalities, minimizing illicit market persistence) or captured by incumbent licensed operators seeking to raise barriers against new entrants and suppress price competition?',
    'Track lobbying activity and rate-setting outcomes over time; compare jurisdictions with industry-influenced rate-setting bodies against those with independent public-health-driven rate-setting.',
    'Capture would indicate the licensed industry''s beneficiary role is not incidental to coordination but actively shapes the constraint''s design toward extraction — pushing the classification toward a harder-edged tangled_rope or even snare-adjacent for displaced informal operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_capture_vs_public_interest, empirical, 'Whether rate/licensing design serves public interest or incumbent industry capture.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the liberty/externality boundary sit — i.e., which harms count as sufficiently ''third-party'' to justify state intervention under this reading, versus which are dismissed as merely the state moralizing (prohibition-adjacent) or under-addressing (harm-reduction-adjacent)?',
    'This is a conceptual boundary internal to the legalization reading''s own framework, not resolvable by data alone; it would require a stable philosophical or legal consensus on what counts as a cognizable externality versus a paternalistic justification.',
    'A narrower externality boundary shrinks the victim set and pushes the constraint toward a cleaner rope; a broader boundary (including e.g. diffuse social costs, family/community effects) reintroduces harm-reduction-style or even prohibition-adjacent justifications and blurs this reading''s distinctiveness from its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Contested boundary of what counts as a third-party externality under the liberty framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the substance_control_kernel. prohibition_reading treats use as moral transgression (different victim set: users themselves, informal-market participants under criminal exposure; different ε reflecting punitive extraction). harm_reduction_reading treats use as a health condition (different beneficiary structure: public health services, not licensed industry; different coordination function: harm minimization independent of cessation or market legality). All three share the underlying contested question of what state intervention into substance use is FOR, but each instantiates a structurally distinct constraint with its own ε, beneficiaries, victims, and enforcement logic — they are linked here via affects_constraints rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
