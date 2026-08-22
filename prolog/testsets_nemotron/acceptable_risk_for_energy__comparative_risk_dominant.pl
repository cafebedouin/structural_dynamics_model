% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Nuclear Acceptability Contingent on Fossil Fuel Alternative
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'comparative risk dominant' reading
 *   of the contested kernel 'acceptable_risk_for_energy.' It instantiates the
 *   position that nuclear risk has no absolute threshold — acceptability is
 *   determined solely by comparison to the risks of available alternatives,
 *   primarily coal emissions and climate catastrophe. The reading treats
 *   temporal urgency (near-term climate tipping points) as structurally
 *   overriding intergenerational waste burdens and low-probability
 *   catastrophic tails. This is one of three live readings; the siblings are
 *   'catastrophic_tail_dominant' (irreversibility and intergenerational
 *   burden dominate) and 'expected_value_dominant' (probability-weighted
 *   annual calculus governs). The constraint operates as a tangled rope: it
 *   genuinely coordinates the energy transition by legitimating nuclear as a
 *   bridge, but simultaneously extracts intergenerational waste stewardship
 *   and localized catastrophic risk from populations who did not consent to
 *   the tradeoff.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.38).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Nuclear Acceptability Contingent on Fossil Fuel Alternative").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '5f111a28-4294-42c3-bc03-82224ec6bcf2').
narrative_ontology:cs_kernel_codification('5f111a28-4294-42c3-bc03-82224ec6bcf2', distributed).
narrative_ontology:cs_authority_grounding('5f111a28-4294-42c3-bc03-82224ec6bcf2', distributed).
narrative_ontology:cs_reading_relation('5f111a28-4294-42c3-bc03-82224ec6bcf2', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('5f111a28-4294-42c3-bc03-82224ec6bcf2', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('5f111a28-4294-42c3-bc03-82224ec6bcf2', foundational, no_absolute_risk_threshold).
narrative_ontology:cs_axiom_status(no_absolute_risk_threshold, holdable).
narrative_ontology:cs_axiom_grounding('5f111a28-4294-42c3-bc03-82224ec6bcf2', no_absolute_risk_threshold, instrumental).
narrative_ontology:cs_axiom('5f111a28-4294-42c3-bc03-82224ec6bcf2', foundational, temporal_urgency_overrides_intergenerational_burden).
narrative_ontology:cs_axiom_status(temporal_urgency_overrides_intergenerational_burden, holdable).
narrative_ontology:cs_axiom_grounding('5f111a28-4294-42c3-bc03-82224ec6bcf2', temporal_urgency_overrides_intergenerational_burden, instrumental).
narrative_ontology:cs_axiom('5f111a28-4294-42c3-bc03-82224ec6bcf2', secondary, substitution_necessity_justifies_transfer).
narrative_ontology:cs_axiom_status(substitution_necessity_justifies_transfer, holdable).
narrative_ontology:cs_axiom_grounding('5f111a28-4294-42c3-bc03-82224ec6bcf2', substitution_necessity_justifies_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('5f111a28-4294-42c3-bc03-82224ec6bcf2', post_oil_crisis_energy_security_frame).
narrative_ontology:cs_drift_state('5f111a28-4294-42c3-bc03-82224ec6bcf2', contemporary_renewables_penetration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f111a28-4294-42c3-bc03-82224ec6bcf2', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, current_energy_consumers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_workers_in_transition).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, energy_security_advocates).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_waste_bearers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_adjacent_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_exposed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_workers_in_transition).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, comparative_risk_framing).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, temporal_urgency_overrides_intergenerational_burden).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, energy_substitution_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reliable, affordable electricity enabled by nuclear baseload that displaces coal. Their energy costs and grid stability depend on the comparative risk framework continuing to justify nuclear operation. Exit means accepting higher prices or reduced reliability from intermittent alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, current_energy_consumers, beneficiary,
    organized, biographical, constrained, national).

% Face immediate climate catastrophe from continued fossil fuel use. The comparative risk framework validates nuclear as a necessary bridge, directly affecting their survival prospects. No meaningful exit — they cannot relocate from climate impacts or individually change energy systems.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Gain from nuclear providing stable baseload that enables renewable integration while preserving some energy-sector employment. Also bear transition costs — skill obsolescence, community disruption, and health impacts from both fossil and nuclear facilities. Exit options limited by geographic and skill lock-in.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_workers_in_transition, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_workers_in_transition, payer).

% Set the risk assessment frameworks that treat nuclear as strategically necessary for energy independence and grid reliability. They administer the comparative risk calculus through regulatory agencies and energy ministries. Can pivot to other security frameworks if political winds shift.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_security_advocates, agenda_setter,
    institutional, generational, arbitrage, national).

% Inherit nuclear waste management burdens spanning millennia without having consented to the risk calculus. The comparative risk framework discounts their burden against near-term climate benefits. No exit — they cannot refuse inheritance of waste stewardship obligations.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_waste_bearers, payer,
    powerless, civilizational, trapped, global).

% Host nuclear facilities and bear routine emissions, accident risk, and economic dependency. The comparative risk framework justifies their exposure as necessary for broader climate benefit. Exit means abandoning homes, communities, and livelihoods — possible but costly.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_adjacent_communities, payer,
    moderate, generational, constrained, local).

% Live in zones where a low-probability, high-consequence nuclear event would cause irreversible harm (contamination, displacement, generational health impacts). Their risk is discounted by probability-weighting in the comparative framework. Identity-locked because their communities, cultures, and ancestral lands are inseparable from the exposed territory.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_exposed_populations, payer,
    powerless, generational, identity_locked, continental).

% Study risk assessment methodologies and challenge the comparative framework's structural assumptions. They document how temporal discounting, probability weighting, and victim selection shape the acceptability calculus. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, independent_risk_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global energy transition by establishing nuclear as a necessary bridge technology — solving the intermittency-reliability gap that would otherwise force continued fossil fuel dependence.
% TRANSFER_FUNCTION: Transfers intergenerational waste stewardship burden and localized catastrophic risk from current energy consumers and climate-vulnerable populations to future generations and nuclear-adjacent communities, in exchange for near-term emissions reduction and energy security.
% ABSENT_VOICES: Future generations who inherit waste burdens without representation; indigenous communities whose consent was never sought for nuclear facility siting; small island states facing existential climate risk who may prefer different risk tradeoffs. They are absent because the comparative risk framework structurally excludes non-present, non-voting, and geographically dispersed parties.
% DISAPPEARANCE_RATIONALE: If the comparative risk framework vanished, nuclear projects would face absolute safety thresholds that most current designs cannot meet. Coal and gas would fill the baseload gap, accelerating climate impacts. The energy transition timeline would extend by decades. The world rearranges because the constraint currently enables a specific technological pathway that has no ready substitute at scale.
% FOUNDING_PROBLEM: Post-1970s energy crises created a perceived choice: nuclear expansion or fossil fuel lock-in. The comparative risk framework was built to legitimate nuclear as the lesser evil when renewables were not yet viable at grid scale.
% FOUNDING_PROBLEM_CORROBORATION: Energy historians document the 1970s framing (outside beneficiaries). Nuclear advocates attest the problem persists (storage, intermittency). Climate scientists attest renewables+storage now change the calculus. No consensus — the founding problem's status is genuinely disputed across epistemic communities.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).
:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint transfers real, long-lived burdens (waste, contamination risk) to non-consenting parties, but the transfer is not purely extractive — the coordination function (enabling nuclear as climate bridge) is real and acknowledged by beneficiaries. Suppression (0.42) is moderate: the framework operates through regulatory licensing, cost-benefit mandates, and exclusion of absolute-threshold alternatives from policy discourse, but does not require total silencing of dissent. Theater ratio (0.28) captures that safety theater (exercises, redundant systems) is genuine but a growing share of regulatory activity performs 'due diligence' without changing outcomes. Accessibility collapse (0.45) is partial: absolute-threshold frameworks exist and are advocated but are structurally excluded from binding policy. Resistance (0.55) is significant: waste communities, indigenous opponents, and catastrophic-tail advocates actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (energy security advocates), the constraint appears as necessary coordination — the only viable path to decarbonization at speed. From the intergenerational waste bearer seat, it appears as a snare — a framework designed by the present to externalize its waste onto the future. From the catastrophic-tail-exposed seat, it appears as a snare with existential stakes — the probability discount is a structural erasure of their risk. The engine computes this divergence from the declared power/exit/beneficiary structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Current energy consumers and climate-vulnerable populations are structural beneficiaries (d ~0.2-0.3) — they receive the climate and reliability benefits. Energy security advocates as agenda-setters have d ~0.15 (they administer the framework and could pivot). Intergenerational waste bearers and catastrophic tail exposed are full targets (d ~0.85-0.95) — they bear the transferred burdens with minimal exit. Nuclear-adjacent communities sit at d ~0.6 (constrained exit, some compensation). The identity-locked exit for catastrophic-tail-exposed populations reflects that their communities, cultures, and ancestral lands cannot be relocated — exit means cultural extinction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s energy crisis: nuclear vs. fossil lock-in) is contested — renewables+storage now change the substitution calculus. The constraint persists because the comparative framework was institutionalized in licensing regimes, liability caps, and waste policies that create path dependency. Mandatrophy is unresolved: the coordination function (bridge to renewables) may be live, but the extraction (intergenerational waste transfer) continues regardless. The constraint is a tangled rope because both functions are real and the extraction is not incidental — the waste transfer is structurally necessary to make nuclear economics work at current scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the ''acceptable_risk_for_energy'' kernel admit one coherent risk framework, or are the three readings (comparative, catastrophic-tail, expected-value) structurally incommensurable frames that cannot be reconciled within a single assessment methodology?',
    'Meta-analysis of whether regulatory decisions actually integrate across frameworks or tacitly select one. If integration fails systematically, the kernel is a site of frame contestation, not a shared referent.',
    'If incommensurable, each reading instantiates a genuinely different constraint with different ε, different victim sets, different classification. The kernel is a false unity — the label ''acceptable risk'' covers three distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel is a single contested commitment or a label for three distinct constraints.').

omega_variable(
    temporal_discounting_legitimacy,
    'Is the comparative risk framework''s implicit temporal discounting (near-term climate urgency overrides millennial waste burdens) a legitimate ethical choice or a structural bias that privileges present power?',
    'Intergenerational ethics analysis: test whether the discount rate implied by the framework can be justified by any coherent population ethics, or whether it simply reflects the political non-representation of future generations.',
    'If structural bias, the constraint''s extraction from intergenerational_waste_bearers is illegitimate by the framework''s own coordination logic — the coordination function (climate bridge) does not require the specific waste transfer terms imposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_discounting_legitimacy, preference, 'Whether temporal discounting in comparative risk is ethical choice or power artifact.').

omega_variable(
    substitution_timeline_uncertainty,
    'When will renewables+storage reliably displace nuclear''s baseload function at global scale? The comparative risk framework''s coordination claim depends on nuclear being a *temporary* bridge — if the bridge duration extends indefinitely, the extraction becomes permanent.',
    'Technology deployment trajectory modeling with credible ranges for storage cost, grid integration, mineral supply chains, and land-use constraints. Track whether ''bridge'' timelines systematically extend.',
    'If bridge duration is unbounded, the constraint shifts from scaffold-like (temporary coordination) to snare-like (permanent extraction masked as temporary). The theater_ratio trajectory would rise as the ''temporary'' justification becomes ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_timeline_uncertainty, empirical, 'Whether the comparative risk framework''s bridge narrative has a credible endpoint.').

omega_variable(
    victim_set_boundary,
    'Does the comparative risk framework''s victim set structurally exclude climate-vulnerable populations who would prefer absolute safety thresholds (e.g., small island states facing existential sea-level rise who oppose nuclear on catastrophic-tail grounds)?',
    'Document positions of climate-vulnerable nations in IAEA, UNFCCC, and regional forums. Test whether their risk preferences are incorporated or overridden by the comparative framework''s major-power consensus.',
    'If climate-vulnerable populations are on both sides (some beneficiaries, some victims), the ''climate_vulnerable_populations'' beneficiary declaration is internally fractured — the constraint coordinates *some* vulnerable groups at the expense of *others*.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Whether climate-vulnerable populations are a coherent beneficiary class or fractured across the kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1973, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arfe_crd_tr_t1973, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(arfe_crd_tr_t1986, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1986, 0.28).
narrative_ontology:measurement(arfe_crd_tr_t1997, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(arfe_crd_tr_t2011, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(arfe_crd_tr_t2020, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(arfe_crd_tr_t2035, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2035, 0.33).
narrative_ontology:measurement(arfe_crd_tr_t2050, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2050, 0.41).

% Extraction over time
narrative_ontology:measurement(arfe_crd_be_t1973, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1973, 0.22).
narrative_ontology:measurement(arfe_crd_be_t1986, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(arfe_crd_be_t1997, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1997, 0.31).
narrative_ontology:measurement(arfe_crd_be_t2011, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2011, 0.42).
narrative_ontology:measurement(arfe_crd_be_t2020, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(arfe_crd_be_t2035, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2035, 0.45).
narrative_ontology:measurement(arfe_crd_be_t2050, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2050, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(arfe_crd_su_t1973, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1973, 0.3).
narrative_ontology:measurement(arfe_crd_su_t1986, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(arfe_crd_su_t1997, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1997, 0.4).
narrative_ontology:measurement(arfe_crd_su_t2011, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2011, 0.6).
narrative_ontology:measurement(arfe_crd_su_t2020, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(arfe_crd_su_t2035, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2035, 0.48).
narrative_ontology:measurement(arfe_crd_su_t2050, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2050, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_governance).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, climate_migration_policy).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, energy_transition_justice).

% DUAL FORMULATION NOTE:
% Part of the acceptable_risk_for_energy kernel family (3 readings). This reading (comparative_risk_dominant) treats nuclear as contingent bridge; catastrophic_tail_dominant treats nuclear as intolerable tail risk; expected_value_dominant treats nuclear as probability-weighted portfolio option. ε differs substantially: comparative reading has moderate extractiveness (transfer to future); catastrophic-tail reading has near-zero extractiveness (nuclear prohibited); expected-value reading has variable extractiveness depending on probability assignments. The readings coexist as live policy positions but foreclose each other in any single licensing decision — a reactor cannot be simultaneously approved under comparative and rejected under catastrophic-tail logic within the same regulatory framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, powerless, 0.92).
constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, moderate, 0.58).
constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
