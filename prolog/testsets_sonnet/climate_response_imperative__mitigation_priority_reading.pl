% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'climate response
 *   imperative' kernel: the mitigation-priority reading, in which emissions
 *   reduction via technological innovation and market mechanisms (carbon
 *   pricing, clean-tech subsidy, offset markets, negative-emissions R&D) is
 *   treated as the primary climate response, with adaptation and
 *   loss-and-damage funding structured as residual. This reading has been
 *   institutionally dominant since the 1992 UNFCCC framing and through the
 *   Kyoto and Paris architecture, increasingly channeling multilateral
 *   finance and Global North industrial policy toward mitigation technology.
 *   It is one of three siblings sharing the same kernel: the
 *   adaptation-priority reading (resilience and damage reduction as primary,
 *   mitigation as aspirational) and the degrowth reading (structural economic
 *   transformation as the precondition for both). Each reading names a
 *   different beneficiary/victim structure and a different ε; they are linked
 *   here via network.affects_constraints rather than merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.52).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, 'bde95313-55e6-4a48-9934-3ecc3c068f06').
narrative_ontology:cs_kernel_codification('bde95313-55e6-4a48-9934-3ecc3c068f06', distributed).
narrative_ontology:cs_authority_grounding('bde95313-55e6-4a48-9934-3ecc3c068f06', distributed).
narrative_ontology:cs_reading_relation('bde95313-55e6-4a48-9934-3ecc3c068f06', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('bde95313-55e6-4a48-9934-3ecc3c068f06', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('bde95313-55e6-4a48-9934-3ecc3c068f06', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('bde95313-55e6-4a48-9934-3ecc3c068f06', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('bde95313-55e6-4a48-9934-3ecc3c068f06', foundational, market_price_signals_are_the_efficient_allocator_of_mitigation_effort).
narrative_ontology:cs_axiom_status(market_price_signals_are_the_efficient_allocator_of_mitigation_effort, holdable).
narrative_ontology:cs_axiom_grounding('bde95313-55e6-4a48-9934-3ecc3c068f06', market_price_signals_are_the_efficient_allocator_of_mitigation_effort, instrumental).
narrative_ontology:cs_reference_frame('bde95313-55e6-4a48-9934-3ecc3c068f06', unfccc_common_but_differentiated_responsibilities_framework).
narrative_ontology:cs_drift_state('bde95313-55e6-4a48-9934-3ecc3c068f06', post_paris_agreement_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bde95313-55e6-4a48-9934-3ecc3c068f06', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_clean_tech_sector).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, incumbent_fossil_utilities_via_offset_credits).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, multilateral_finance_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, small_island_and_coastal_states).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, smallholder_farmers_in_climate_exposed_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, urban_poor_in_heat_and_flood_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_dioxide_removal_technology_developers).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, innovation_led_decarbonization_feasibility).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, carbon_pricing_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives subsidies, tax credits, carbon-market revenue, and preferential procurement built around the premise that emissions reduction via innovation is the primary climate response. Patents, supply chains, and market position accrue disproportionately here; can pivot capital across jurisdictions as incentive structures shift.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_clean_tech_sector, beneficiary,
    organized, generational, arbitrage, global).

% Design, verify, and trade offset and removal credits that let mitigation obligations be satisfied on paper without proportional emissions cuts. Collect fees on every transaction; have direct influence over methodology standards that determine what counts as a credible offset.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, agenda_setter).

% Structure climate finance instruments, set eligibility criteria for mitigation versus adaptation funding, and administer the treaties and frameworks that codify mitigation as the default priority. Can revise the allocation formula but currently benefits from the technocratic legitimacy that a mitigation-first framing confers.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, multilateral_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Use offset purchases and pledged future removal technology to continue current emissions while claiming net-zero trajectories; the mitigation-priority framing legitimizes continued operation contingent on future technological delivery rather than present reduction.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, incumbent_fossil_utilities_via_offset_credits, beneficiary,
    powerful, biographical, arbitrage, national).

% Inherit whatever residual warming, locked-in damages, and adaptation deficits result if promised carbon dioxide removal and innovation-led decarbonization underdeliver relative to schedule. Have no seat in current allocation decisions and no capacity to renegotiate the bet being made on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Face existential sea-level and storm exposure now, while adaptation and loss-and-damage funding remains structurally residual to mitigation funding in treaty architecture and multilateral budgets. Cannot exit their geography and have limited leverage to reorder global funding priorities despite decades of advocacy.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, small_island_and_coastal_states, payer,
    powerless, generational, trapped, regional).

% Absorb drought, flood, and yield-collapse costs directly while global climate finance concentrates on mitigation technology deployed largely in industrialized economies. Adaptation support arrives late, underfunded, and structured as emergency relief rather than planned investment.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, smallholder_farmers_in_climate_exposed_regions, payer,
    powerless, biographical, trapped, regional).

% Live in informal or under-resourced settlements exposed to heat waves and flooding intensified by delayed mitigation; municipal adaptation infrastructure competes for funds against nationally prioritized mitigation and innovation commitments and typically loses.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, urban_poor_in_heat_and_flood_zones, payer,
    powerless, biographical, trapped, local).

% Receive research funding and forward-purchase commitments premised on the assumption that unproven or unscaled carbon removal will close the gap between insufficient near-term cuts and stated climate targets; the mitigation-priority reading structurally depends on their eventual delivery.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_dioxide_removal_technology_developers, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, carbon_dioxide_removal_technology_developers, agenda_setter).

% Represent exposed regions and communities in international negotiations but hold structurally weaker standing than mitigation-finance and technology blocs; consistently argue for parity in funding allocation but face procedural and voting-weight disadvantages in the bodies that set priorities.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_advocacy_coalitions, excluded,
    moderate, generational, constrained, global).

% Produce the physical and economic modeling that both mitigation-priority and adaptation-priority readings cite; document the growing gap between pledged mitigation trajectories and actual emissions, and increasingly flag the risk premium embedded in reliance on unscaled negative-emissions technology.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_science_and_ipcc_assessment_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global emissions-reduction effort by channeling capital, research, and policy attention toward technologies and market mechanisms (carbon pricing, clean-tech deployment, credit trading) that could, if they scale as projected, reduce aggregate emissions more cheaply than either large-scale behavioral/consumption change or immediate adaptation investment.
% TRANSFER_FUNCTION: Moves near-term climate finance, R&D subsidy, and institutional attention away from adaptation and loss-and-damage support for currently exposed populations, and toward innovation subsidy and offset infrastructure that principally benefits Global North technology and finance sectors; simultaneously transfers physical and financial risk forward onto future generations, whose exposure depends on technologies not yet proven at scale.
% ABSENT_VOICES: Vulnerable coastal and agrarian populations bearing present climate damage, and future generations who will inherit any shortfall between promised removal capacity and delivered removal capacity, are structurally underrepresented in the treaty bodies and finance institutions that set the mitigation/adaptation funding split; adaptation advocacy coalitions raise this consistently but hold weaker procedural standing than mitigation-finance blocs.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing were abandoned overnight in favor of parity or adaptation-priority funding, multilateral climate finance allocation, clean-tech subsidy structures, carbon market architecture, and national NDC design would all have to be renegotiated; entire industries (offset verification, CDR startups, green-tech manufacturing subsidy chains) built on the current allocation logic would lose their primary funding rationale.
% FOUNDING_PROBLEM: Rising atmospheric greenhouse gas concentrations threaten catastrophic and potentially irreversible climate disruption; the founding problem this reading was built to solve is preventing that disruption by reducing the flow of new emissions before damage becomes locked in.
% FOUNDING_PROBLEM_CORROBORATION: Global North governments, clean-tech industry associations, and multilateral finance institutions attest the mitigation-first problem framing remains live and urgent. Independent corroboration from outside the primary beneficiary set is mixed: IPCC assessment reports themselves increasingly note that committed warming and locked-in impacts mean adaptation is no longer residual but concurrent and urgent — a reading that sits closer to the sibling adaptation-priority framing than to this one, which is itself a signal that the founding problem, as this reading defines its scope, is only partially corroborated by the science it invokes.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and theater ratio (0.44) are authored as substantial and rising because an increasing share of mitigation-priority activity consists of paper compliance — offset credits of uncertain additionality, forward-purchased carbon removal not yet delivered at scale — layered onto a genuine underlying coordination function (a real global emissions-reduction problem exists and channeling capital toward decarbonization technology is a real coordination act). Suppression (0.52) reflects the structural lock-in of treaty architecture and finance-institution eligibility criteria that make it institutionally difficult for exposed regions to redirect funding toward adaptation, though this is enforcement of an allocation formula rather than coercive suppression of physical exit. Accessibility collapse (0.5) and resistance (0.58) are set at moderate-to-substantial levels reflecting that alternative framings (adaptation-priority, degrowth) remain live, contested, and actively argued by advocacy coalitions — this is not a settled natural fact but a policy choice under sustained challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats (finance institutions, clean-tech sector), the arrangement reads as prudent, technocratically optimal climate coordination. From the payer seats (exposed populations, future generations), the identical structure reads as risk transfer: near-term certainty of finance directed elsewhere in exchange for a promise that projected technology will arrive in time. The engine computes this divergence from the structural power/exit data; the claimed_type does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North clean-tech firms, carbon market intermediaries, CDR developers, and (via offset purchase) incumbent fossil utilities sit near the beneficiary end: the mitigation-priority framing directs subsidy, procurement preference, and market-making activity toward them. Multilateral finance institutions are agenda-setters who both administer and derive institutional legitimacy from the framing. Future generations, small island states, smallholder farmers, and the urban poor sit near the full-target end: they bear the deferred cost if the technology bet underdelivers, have no meaningful exit (geography, poverty, or non-existence-yet in the case of future generations foreclose mobility), and had no vote in the allocation decision made on their behalf.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare preserves the fact that a genuine coordination problem (reducing global emissions) is being addressed by real institutional machinery, some of which functions as intended (renewable deployment has scaled, costs have fallen). Classifying it as pure extraction would mislabel decades of functioning coordination; classifying it as pure rope would erase the asymmetric cost transfer onto populations and future generations who did not choose the innovation-led bet and cannot exit its consequences. The tangled_rope reading holds both facts simultaneously and requires active enforcement (treaty eligibility rules, finance-institution allocation formulas) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_delivery_uncertainty,
    'Will carbon dioxide removal and other negative-emissions technologies scale to the volumes assumed in mitigation-priority pathway modeling, on the timeline assumed?',
    'Track actual deployed CDR capacity against IPCC pathway assumptions at 5-year checkpoints; a persistent and widening gap between pledged and delivered removal capacity would indicate the reading''s central technological bet is failing.',
    'If CDR substantially underdelivers, the deferred adaptation and loss-and-damage costs currently borne by future generations and exposed regions become larger and more certain, strengthening the case that this reading structurally transfers risk rather than resolves it; if CDR delivers close to assumption, the coordination function is more substantially vindicated and less extractive than currently measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_delivery_uncertainty, empirical, 'Whether the reading''s core technological premise (scalable CDR) will actually materialize.').

omega_variable(
    kernel_reading_selection_authority,
    'Who has the legitimate authority to determine which reading of the climate response imperative (mitigation-priority, adaptation-priority, degrowth) governs global finance allocation, and on what basis was mitigation-priority selected as institutionally dominant?',
    'Historical and institutional analysis of UNFCCC/Kyoto/Paris negotiating records to trace whether mitigation-priority became dominant through superior evidentiary support, superior negotiating power of Global North parties, or path dependency from early framework design.',
    'If selection tracked negotiating power rather than evidentiary merit, this strengthens the false-summit reading of the mitigation-priority framing as a constructed rather than naturally optimal response; if selection tracked genuine cost-effectiveness analysis available at the time, the coordination function is more substantially grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Whether the dominance of this particular kernel reading reflects merit or power asymmetry among negotiating parties.').

omega_variable(
    adaptation_underfunding_causal_attribution,
    'Is adaptation funding residual because mitigation is genuinely more cost-effective at the margin, or because mitigation investment is more attractive to Global North capital (patentable, exportable, growth-generating) regardless of relative cost-effectiveness?',
    'Compare marginal cost-effectiveness (damage avoided per dollar) of mitigation versus adaptation investment across a range of exposed-region case studies, controlling for who captures the investment return.',
    'If mitigation investment is favored primarily because returns accrue to investing-country industry rather than because it is more cost-effective at preventing harm, the tangled_rope classification''s extraction component is strengthened; if genuinely more cost-effective, the coordination component is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_underfunding_causal_attribution, empirical, 'Whether the mitigation/adaptation funding split tracks cost-effectiveness or investor-capture incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.24).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(clim_tr_t2012, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(clim_tr_t2019, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(clim_tr_t2024, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(clim_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.4).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(clim_be_t2012, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(clim_be_t2019, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(clim_be_t2024, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(clim_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.34).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(clim_su_t2012, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(clim_su_t2019, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement(clim_su_t2024, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, carbon_offset_market_integrity).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, loss_and_damage_finance_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_imperative kernel. adaptation_priority_reading treats resilience and damage reduction as primary and would name a substantially different beneficiary set (adaptation engineering/insurance sectors, exposed-region governments) and a different victim set (populations if mitigation is neglected long-term). degrowth_reading treats structural economic transformation as the precondition for both mitigation and adaptation and would name Global North consumption-dependent industries as the primary payer class. Each reading carries its own ε and classification; none averages or supersedes the others. This story additionally influences downstream constraints governing offset-market integrity and loss-and-damage finance mechanisms, since the funding-allocation logic it establishes structurally shapes both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
