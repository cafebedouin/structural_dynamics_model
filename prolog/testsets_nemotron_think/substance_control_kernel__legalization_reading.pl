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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalization with Externality Capture Regime
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the legalization reading of the substance
 *   control kernel: substance use is framed as an individual liberty issue
 *   where the state's sole legitimate role is preventing third-party harm
 *   (DUI, secondhand exposure) and capturing externality costs through
 *   Pigouvian taxation. The regime replaces prohibition's criminalization of
 *   users with a regulated market. Users exit the victim set entirely. A
 *   legal industry emerges as a concentrated beneficiary. The state becomes a
 *   revenue collector. Third parties enter the victim set via residual
 *   externalities that taxation does not fully internalize. Black markets
 *   persist in gray areas (untaxed sales, underage access, potency caps). The
 *   constraint is claimed as tangled_rope — genuine coordination (market
 *   replaces black market, reduces violence) with asymmetric extraction
 *   (third parties bear uncompensated externalities, industry captures
 *   regulatory surplus).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization with Externality Capture Regime").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '388b3bee-a7e4-4cc1-852b-d651a7d82c4e').
narrative_ontology:cs_kernel_codification('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', formalized).
narrative_ontology:cs_authority_grounding('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', lineage).
narrative_ontology:cs_interpretation_layer_present('388b3bee-a7e4-4cc1-852b-d651a7d82c4e').
narrative_ontology:cs_reading_relation('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', foundational, substance_use_is_liberty_right).
narrative_ontology:cs_axiom_status(substance_use_is_liberty_right, holdable).
narrative_ontology:cs_axiom_grounding('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', substance_use_is_liberty_right, deontological).
narrative_ontology:cs_axiom('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', foundational, state_intervention_only_for_externalities).
narrative_ontology:cs_axiom_status(state_intervention_only_for_externalities, holdable).
narrative_ontology:cs_axiom_grounding('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', state_intervention_only_for_externalities, deontological).
narrative_ontology:cs_axiom('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', secondary, taxation_internalizes_externalities).
narrative_ontology:cs_axiom_status(taxation_internalizes_externalities, holdable).
narrative_ontology:cs_axiom_grounding('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', taxation_internalizes_externalities, instrumental).
narrative_ontology:cs_reference_frame('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', harm_principle_legalization_framework).
narrative_ontology:cs_drift_state('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', post_legalization_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('388b3bee-a7e4-4cc1-852b-d651a7d82c4e', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_authority).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, harm_principle).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, individual_autonomy_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, pigouvian_taxation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates licensed cultivation, processing, and retail; pays taxes and compliance costs but captures market share from black market; lobbies for favorable tax rates and regulatory frameworks; profits scale with consumption volume.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    powerful, biographical, mobile, national).

% Sets licensing rules, tax rates, product standards, and consumption restrictions; collects excise and sales tax revenue from legal substance sales; uses revenue for general fund or dedicated programs; enforcement apparatus polices unlicensed market and DUI.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_tax_authority, beneficiary).

% Gain legal access to regulated products without criminal penalty; pay retail prices inclusive of taxes; can choose legal market or gray/black market alternatives; advocacy groups influence policy but individual users have high exit mobility.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, substance_users, payer).

% Bear residual externality costs not fully captured by taxation: DUI crash victims, children exposed to secondhand smoke/vapor, communities with normalized public consumption; no direct representation in regulatory process; exit requires geographic relocation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities, payer,
    moderate, biographical, constrained, regional).

% Monitor use prevalence, youth initiation, treatment demand, and health outcomes; advise on tax rates, potency caps, and marketing restrictions; their recommendations compete with industry lobbying for regulator attention.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% Displaced by legal market but persist in gray areas: untaxed sales, underage sales, high-potency products exceeding legal caps, jurisdictions without legal markets; would oppose legalization but have no legitimate voice in policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    organized, biographical, trapped, national).

% Evaluates the regime's net social welfare: tax revenue vs. externality costs, liberty gains vs. public health harms, displacement of black market vs. persistence of gray market; no material stake in outcome.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces prohibition's black market violence and user criminalization with a regulated market that internalizes externalities through taxation and prevents third-party harm through DUI laws, age restrictions, and consumption regulations.
% TRANSFER_FUNCTION: Moves tax revenue from legal substance sales to state treasury; moves profit to legal industry; moves residual externality costs (DUI injuries, secondhand exposure, normalization effects) to third parties; moves compliance costs to industry.
% ABSENT_VOICES: Third parties who bear externality costs (DUI victims, children exposed to secondhand smoke, communities with normalized consumption) are diffuse and underrepresented in regulatory capture by industry; future generations affected by normalization of use have no voice; black market operators excluded but would argue prohibition was preferable for their interests.
% DISAPPEARANCE_RATIONALE: If the legalization regime vanished overnight, the legal market would collapse, black market would likely resurge with associated violence, users would face recriminalization, state would lose billions in tax revenue, and third-party harms would shift from regulated externalities (DUI, secondhand) to prohibition-era harms (black market violence, unregulated product toxicity, mass incarceration).
% FOUNDING_PROBLEM: Prohibition's failure: black market violence, mass incarceration of users (with severe racial disparities), failure to control product quality/safety, foregone tax revenue, erosion of civil liberties, and diversion of law enforcement resources from violent crime.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminology studies (Cato Institute, Brookings, RAND) document prohibition's failures; law enforcement testimony is divided (some support legalization for resource reallocation, others oppose); public health data shows mixed post-legalization outcomes — youth use stable or down in some jurisdictions, up in others; treatment demand increased; no disinterested consensus on net welfare.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects that the constraint extracts from third parties via uncompensated externalities and from consumers via taxes above marginal externality cost, but users gain liberty (negative extraction for them). Suppression (0.35) is moderate — enforcement targets unlicensed market and DUI, not users per se. Theater ratio (0.25) is low-moderate — regulatory apparatus is largely functional but industry capture of rulemaking grows over time. Accessibility collapse (0.3) is low — gray/black market alternatives persist. Resistance (0.4) is moderate — industry resists tax increases; public health advocates push tighter regulation; prohibitionists seek recriminalization. The measurement series shows extraction rising then plateauing as industry matures and regulatory capture deepens; theater rising as compliance becomes performative; suppression declining as legal market stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the state/industry seat, the regime is successful coordination: black market shrunk, revenue generated, users free. From third-party seat, it is extraction: they bear DUI and secondhand harms while industry profits and state collects taxes but underfunds mitigation. From user seat, it is liberty with a price tag. The engine computes this divergence; the claimed type (tangled_rope) captures the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal industry and state tax authority are structural beneficiaries (d near 0.0) — they collect rents/revenue from the constraint. Substance users are near-symmetric beneficiaries (d ~ 0.3) — gain liberty, pay taxes. Third parties exposed to externalities are targets (d ~ 0.8) — bear uncompensated costs with constrained exit. Black market operators are excluded (trapped, no voice). Public health authorities and analytical observers are analytical seats. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's harms) is substantially solved but contested — new harms emerge (commercialization, normalization, potency escalation). The arrangement persists not because the founding problem remains acute, but because beneficiaries (industry, state) are concentrated and organized, while victims (third parties) are diffuse. This is not pure mandatrophy (the coordination function remains live) but shows mandatrophic drift: the original justification (harm reduction via regulation) is increasingly displaced by revenue maximization and industry protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the substance_control_kernel, or does it blur into the harm_reduction_reading in practice?',
    'Compare statutory frameworks: if the law''s stated purpose is liberty/externalities (not health), and health interventions are subordinate to market regulation, the readings are distinct. If health agencies co-lead regulation, the boundary blurs.',
    'If readings blur, the constraint family decomposition fails — we would have one constraint with internal tension, not two structurally distinct constraints. This would change network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether legalization and harm reduction are structurally distinct constraint readings or a single hybrid regime.').

omega_variable(
    externality_capture_sufficiency,
    'Can taxation fully capture the externalities of legal substance use, or do third parties systematically bear residual costs?',
    'Econometric estimation of total externality costs (DUI, secondhand, productivity, healthcare) vs. tax revenue collected; longitudinal studies of jurisdictions with varying tax rates.',
    'If capture is insufficient, the constraint is structurally tangled_rope (coordination + extraction). If capture is sufficient, it approaches rope. If capture is negative (taxes < externalities), it trends toward snare for third parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_capture_sufficiency, empirical, 'Whether the Pigouvian logic of the legalization reading holds empirically or whether third parties are net victims.').

omega_variable(
    industry_regulatory_capture,
    'Does the legal industry become the de facto agenda setter, shaping regulations to maximize profit rather than minimize harm?',
    'Track lobbying expenditure, regulatory docket comments, revolving door personnel, and policy outcomes (tax rates, potency caps, marketing rules, license caps) over the interval.',
    'If capture is established, the state_tax_authority''s role shifts from agenda_setter to captured_regulator; the constraint''s coordination function degrades; theater_ratio rises; extraction from third parties increases as industry externalizes more costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_regulatory_capture, empirical, 'Whether the beneficiary structure shifts from state-led to industry-led over time.').

omega_variable(
    black_market_persistence,
    'Does the black market collapse or persist in gray areas, and does its persistence constitute a failure of the coordination function?',
    'Seizure data, price gap analysis (legal vs. illegal), survey data on source of last purchase, youth access studies.',
    'If black market persists significantly, the coordination function (displacing illicit market) is incomplete; the constraint may require higher suppression (enforcement against illicit market) than authored, increasing extraction on residual illicit actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Whether the legalization regime achieves its primary coordination goal of market displacement.').

omega_variable(
    harm_reduction_displacement,
    'Does legalization crowd out harm reduction funding and infrastructure, or does tax revenue expand it?',
    'Budget analysis: track dedicated harm reduction funding pre/post legalization; compare jurisdictions with/without earmarked tax revenue for treatment.',
    'If displacement occurs, the harm_reduction_reading''s coordination function is degraded by this reading''s influence (supports cs_structure.reading_relations: influences). If expansion occurs, the readings are complementary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_reduction_displacement, empirical, 'Whether the legalization reading''s resource allocation undermines or supports the harm reduction reading''s coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scklr_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(scklr_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(scklr_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(scklr_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(scklr_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(scklr_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(scklr_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(scklr_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(scklr_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(scklr_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(scklr_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(scklr_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(scklr_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(scklr_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(scklr_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(scklr_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(scklr_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(scklr_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legalization_reading of the substance_control_kernel. It forecloses the prohibition_reading (liberty vs. moral transgression are mutually exclusive governing premises) and coexists with the harm_reduction_reading (both can operate simultaneously). It influences harm_reduction by redirecting regulatory focus and funding. The ε values differ: prohibition_reading has high extraction on users (ε ~ 0.8); harm_reduction_reading has moderate extraction on users via paternalism (ε ~ 0.4); this reading has low extraction on users but moderate on third parties (ε ~ 0.45).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
