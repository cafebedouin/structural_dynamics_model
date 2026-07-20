% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Legalization with Externality Taxation
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the legalization reading of the
 *   substance_control_kernel: substance use is framed as an individual
 *   liberty issue, and the state intervenes only to prevent third-party harm
 *   and to capture externality costs through taxation. The structural delta
 *   from sibling readings is sharp: users exit the victim set entirely, while
 *   third parties enter it via uncompensated externalities such as impaired
 *   driving and secondhand exposure. A legal industry emerges as a
 *   concentrated beneficiary, and the state becomes a direct revenue
 *   collector. The framework is actively enforced through licensing,
 *   taxation, and suppression of unlicensed supply. It is claimed as a
 *   tangled rope because it carries a genuine coordination function â
 *   organizing a safer market and funding public budgets â while
 *   asymmetrically extracting from diffuse third-party harm bearers who lack
 *   exit.
 *
 * KEY AGENTS:
 *   - substance_users: Primary beneficiaries (organized/mobile) â decriminalized, pay taxes, consume regulated products.
 *   - legal_substance_industry: Secondary beneficiaries (powerful/constrained) â licensed operators profiting from regulatory barriers.
 *   - state_revenue_apparatus: Agenda-setter and fiscal beneficiary (institutional/arbitrage) â sets rules, collects revenue, enforces compliance.
 *   - third_party_harm_bearers: Primary payers (powerless/trapped) â bear uncompensated externality costs in shared public spaces.
 *   - black_market_operators: Excluded actors (moderate/trapped) â displaced suppliers subject to enforcement protecting tax revenue.
 *   - prohibition_advocates: Excluded voices (organized/constrained) â morally opposed but structurally sidelined.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.58).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.48).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Legalization with Externality Taxation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, 'f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef').
narrative_ontology:cs_kernel_codification('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', formalized).
narrative_ontology:cs_authority_grounding('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', extraction).
narrative_ontology:cs_interpretation_layer_present('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef').
narrative_ontology:cs_reading_relation('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', foundational, substance_use_as_liberty_right).
narrative_ontology:cs_axiom_status(substance_use_as_liberty_right, holdable).
narrative_ontology:cs_axiom_grounding('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', substance_use_as_liberty_right, deontological).
narrative_ontology:cs_axiom('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', foundational, taxation_as_externality_capture).
narrative_ontology:cs_axiom_status(taxation_as_externality_capture, holdable).
narrative_ontology:cs_axiom_grounding('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', taxation_as_externality_capture, instrumental).
narrative_ontology:cs_reference_frame('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', legalized_commercial_market).
narrative_ontology:cs_drift_state('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', contemporary_regulatory_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9e1dc0c-86a6-4f3d-9cd8-0442e25a56ef', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue_apparatus).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_harm_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decriminalized for use and possession within regulated limits. Pay excise taxes on legal products and face quality controls. No longer subject to criminal sanction for personal use, shifting from offenders to consumers. Can exit to abstinence or unregulated sources, but legal market offers convenience and safety testing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    organized, biographical, mobile, national).

% Licensed cultivators, distributors, and retailers operating within a tax-and-regulate framework. Profit from legal market access and from regulatory barriers that limit competition. Invest heavily in compliance infrastructure and political lobbying. Cannot exit to illegality without forfeiting licenses and sunk capital.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    powerful, biographical, constrained, national).

% Sets tax rates, product standards, licensing criteria, and advertising rules. Collects substantial excise and sales tax revenue from the legal market. Enforces compliance through licensing boards, tax audits, and targeted policing of unlicensed sellers. Benefits from reduced criminal enforcement costs and a stable taxable commodity base.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_revenue_apparatus, beneficiary).

% Diffuse public and private actors who bear uncompensated costs from substance-related externalities, including impaired driving incidents, secondhand exposure, and public nuisance. Have no direct contractual relationship with users or sellers. Cannot practically opt out of shared roads, airspace, or public domains where harms manifest. Lack organizational capacity to demand full compensation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_harm_bearers, payer,
    powerless, immediate, trapped, local).

% Unlicensed suppliers displaced by the legal regime but still serving market segments priced out by taxation or excluded by regulation. Subject to enforcement actions justified as protecting tax revenue and licensed market integrity. Structurally excluded from policy deliberations and treated as criminal rather than competitors.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    moderate, immediate, trapped, regional).

% Moral and public-health advocates who maintain that substance use should remain fully criminalized. Their normative framing is formally rejected by the legalization kernel. Retain cultural influence and lobbying capacity but are structurally sidelined in the dominant policy consensus.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_revenue_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a legal, taxable market for adult substance use while channeling revenue toward public budgets and attempting to internalize social costs that were previously borne by the general public under prohibition.
% TRANSFER_FUNCTION: Moves excise tax revenue from substance users and licensed sellers to the state, and shifts externality costs such as impaired-driving risk and secondhand exposure from users and industry to third parties and the public.
% ABSENT_VOICES: Black market operators are structurally excluded as criminal elements; prohibition advocates are sidelined by the liberty framing; future potential victims of industry marketing or product design are not yet organized in the deliberative space.
% DISAPPEARANCE_RATIONALE: If the legalization framework disappeared overnight, the licensed industry would collapse, state tax revenues would evaporate, criminal sanctions would return for users, and black-market supply chains would rapidly reconstitute â the social and institutional arrangements would reorganize around prohibition or harm reduction.
% FOUNDING_PROBLEM: Prohibition of substance use produced violent black markets, mass incarceration, corrupted enforcement, and failed to reduce consumption while imposing enormous fiscal and social costs.
% FOUNDING_PROBLEM_CORROBORATION: Criminal justice reformers and economists outside the legal industry attest to prohibition's costs. Prohibition advocates contest that the problem was prohibition itself rather than use; public health researchers debate whether legalization has solved the founding problem or merely transformed its distribution.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the tax burden and externality shift represent a real transfer from users and third parties to the state and industry. Suppression (0.48) is moderate: the constraint depends on active enforcement against unlicensed sellers and DUI, but is less coercive than prohibition. Theater ratio (0.28) reflects some performative enforcement (tax-compliance theater, highway checkpoints) alongside functional regulation. Accessibility collapse (0.40) indicates that while alternatives to the legal market exist, they are penalized or stigmatized. Resistance (0.55) captures ongoing opposition from prohibition advocates and black-market persistence. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The state and licensed industry experience the constraint as successful coordination: stable revenue, regulated markets, and reduced criminal overhead. Substance users experience it as a liberty gain with a tax surcharge. Third-party harm bearers experience the same structure as cost-shifting: they receive neither the liberty benefit nor the revenue, but bear the residual risks. The engine should compute these seats differently â low directionality for the state and industry, moderate for users, and high directionality for trapped third parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map cleanly: substance_users, legal_substance_industry, and state_revenue_apparatus are declared beneficiaries, positioning them toward the low-d (subsidy) end. Third_party_harm_bearers are declared victims, positioning them toward high-d (target) end. The state is an agenda-setter with arbitrage exit, further damping its computed extraction. Third parties are powerless with trapped exit, amplifying theirs. Black-market operators are excluded but not declared victims; their extraction is through enforcement, captured via suppression metrics rather than directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing the coordination layer (licensed supply, quality control, tax-funded public services) from the extraction layer (regulatory capture, excise taxes exceeding externality costs, uncompensated third-party harms). Without the victim declaration for third_party_harm_bearers, the framework might read as a rope; without the beneficiary declarations, it might read as a snare. Both are structurally required to capture the tangled-rope reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_boundary_ambiguity,
    'Where does the boundary between user autonomy and compensable third-party harm lie, and who defines it?',
    'Comparative case law analysis and epidemiological attribution studies quantifying the fraction of DUI incidents, secondhand exposures, and public health costs attributable to legal versus unregulated use.',
    'If the boundary expands, more user behavior is regulated and the constraint slides toward a public-health snare; if it contracts, third parties bear more uncompensated costs and extraction increases for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_boundary_ambiguity, conceptual, 'Ambiguity in defining compensable third-party harm under a liberty framework').

omega_variable(
    industry_regulatory_capture,
    'Has the legal substance industry captured the regulatory framework, inflating extractiveness beyond the externality-capture justification?',
    'Lobbying-disclosure analysis paired with tax-rate and licensing-barrier time series: capture would show rising industry concentration, regulatory moats, and tax rates decoupled from social-cost estimates.',
    'Confirmed capture would reclassify the constraint toward snare for consumers and third parties; absence would support the tangled-rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_regulatory_capture, empirical, 'Whether industry capture has distorted the legalization framework').

omega_variable(
    cs_authority_framing,
    'Is the authority of this constraint best framed as lineage (constitutional liberty tradition) or extraction (state fiscal dependence on excise revenue)?',
    'Fiscal-reliance analysis: if the state''s budget becomes materially dependent on substance taxes, extraction framing dominates; if taxes remain marginal and the framework is defended primarily on rights grounds, lineage framing dominates.',
    'Extraction framing raises the theater_ratio and predicts harder reform; lineage framing predicts more principled flexibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_authority_framing, conceptual, 'Alternative commitment-system framing of state authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__legalization_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__legalization_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__legalization_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__legalization_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__legalization_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__legalization_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__legalization_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__legalization_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__legalization_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_kernel. It shares the empirical domain of substance use with its siblings but instantiates a distinct normative commitment. Its Îµ is modulated by the shift of users from victims to beneficiaries and the emergence of third-party externalities as the victim set. Sibling constraints should be consulted for the prohibition and harm-reduction readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
