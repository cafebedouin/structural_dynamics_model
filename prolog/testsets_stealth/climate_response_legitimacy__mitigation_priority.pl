% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy Norm
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   The dominant legitimacy norm in climate governance holds that a
 *   legitimate response reduces emissions through technological innovation
 *   and carbon pricing while preserving economic growth, decoupling output
 *   from emissions. The norm performs real coordination — it kept every major
 *   emitter inside one regime where earlier architectures collapsed — and
 *   carries real asymmetric cost: present growth is protected while residual
 *   warming risk and dependence on undemonstrated carbon removal accumulate
 *   on parties with no seat. Interval 0-30 maps approximately to 1995-2025,
 *   from the post-Kyoto consolidation of the frame through the Paris era to
 *   the first global stocktake. This story instantiates ONE reading —
 *   mitigation_priority — of the contested kernel
 *   climate_response_legitimacy. The sibling readings (adaptation_priority,
 *   degrowth_transformation) are separate constraints with their own epsilon
 *   values, victim sets, and classifications; they are linked via
 *   network.affects_constraints, not folded into this file. The epsilon here
 *   refers to the mitigation-priority arrangement itself as the standing
 *   arrangement under contest.
 *
 * KEY AGENTS:
 *   - - national_governments: agenda_setter (institutional/constrained) — wrote and administers the legitimacy criteria; benefits from growth-compatible climate action
 *   - - incumbent_fossil_energy_producers: primary beneficiary (powerful/arbitrage) — protected from forced stranding by the growth-preservation premise
 *   - - green_technology_industries: beneficiary (organized/mobile) — receives subsidy and mandate flows under mitigation-first allocation
 *   - - carbon_market_financial_intermediaries: beneficiary (organized/arbitrage) — collects fees on the pricing architecture
 *   - - current_generation_wealthy_consumers: beneficiary/secondary payer (organized/constrained) — keeps growth and consumption; pays prices at the margin
 *   - - low_income_households: payer (moderate/trapped) — bears regressive price incidence without compensation-design authority
 *   - - fossil_fuel_dependent_communities: payer (moderate/trapped) — absorbs concentrated closure costs on nationally set timelines
 *   - - future_generations: payer (powerless/trapped) — inherit residual warming and CDR dependency if decoupling fails; no seat, no exit
 *   - - climate_vulnerable_populations: payer/secondary excluded (powerless/trapped) — adaptation subordinated while impacts arrive now
 *   - - degrowth_and_postgrowth_movements: excluded (moderate/constrained) — ruled illegitimate ex ante by the frame
 *   - - integrated_assessment_modeling_community: observer/secondary agenda_setter (institutional/analytical) — supplies the cost-benefit architecture that adjudicates the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.6).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy Norm").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886').
narrative_ontology:cs_kernel_codification('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', formalized).
narrative_ontology:cs_authority_grounding('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', expertise).
narrative_ontology:cs_interpretation_layer_present('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886').
narrative_ontology:cs_reading_relation('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', foundational, growth_compatible_decoupling_sufficiency).
narrative_ontology:cs_axiom_status(growth_compatible_decoupling_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', growth_compatible_decoupling_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', foundational, carbon_pricing_internalization_primacy).
narrative_ontology:cs_axiom_status(carbon_pricing_internalization_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', carbon_pricing_internalization_primacy, instrumental).
narrative_ontology:cs_reference_frame('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', growth_preserving_priced_mitigation).
narrative_ontology:cs_drift_state('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', post_paris_global_stocktake, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0d4ac2a9-ffd2-487b-b6e4-69cc4e50e886', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_energy_producers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, green_technology_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_financial_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_generation_wealthy_consumers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, national_governments).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_fuel_dependent_communities).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, current_generation_wealthy_consumers).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_decoupling_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, carbon_pricing_efficiency_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, technological_substitutability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate carbon prices, fund innovation portfolios, and negotiate internationally under a frame that keeps climate action compatible with growth-dependent tax bases and electoral cycles. They wrote the legitimacy criteria and administer compliance with them. Treaty commitments and domestic constituencies on both sides of the pricing question limit how far they can move in any direction.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, national_governments, agenda_setter,
    institutional, biographical, constrained, continental).

% Operate under carbon prices cushioned by free allowances, offset credits, and multi-decade phase-down schedules. The growth-preservation premise protects their core reserves and infrastructure from forced early retirement. They can shift portfolios between jurisdictions, earn offset revenue, and commission favorable economic modeling.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_fossil_energy_producers, beneficiary,
    powerful, biographical, arbitrage, global).

% Renewables, storage, electric vehicle, and hydrogen manufacturers receive subsidies, purchase mandates, and guaranteed demand expansion under the frame. Revenue scales with the mitigation-first allocation of public climate finance. Production can relocate to whichever jurisdiction subsidizes most generously.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, green_technology_industries, beneficiary,
    organized, biographical, mobile, global).

% Exchanges, offset verifiers, project developers, and ESG product managers collect fees on every tonne traded or credited. Income depends on the pricing architecture persisting and expanding into new sectors. Any single market can be exited instantly; the fee-taking position itself is portable.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_financial_intermediaries, beneficiary,
    organized, immediate, arbitrage, global).

% Retain the growth-linked employment, asset values, and consumption patterns the frame promises to protect. They pay carbon prices at the margin and indirectly through product prices, and their voting behavior caps how far pricing can rise. Their stake is split: present comfort now, atmospheric liability later.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, current_generation_wealthy_consumers, beneficiary,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, current_generation_wealthy_consumers, payer).

% Energy and transport are non-discretionary spending, so carbon price incidence falls hardest here. Compensation design is decided in forums they do not staff. There is no exit from the priced economy; the only lever is political voice, which the frame's technocratic administration tends to bypass.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_households, payer,
    moderate, immediate, trapped, national).

% Coal and oil regions absorb plant closures and job losses on transition timelines set nationally. Promised reinvestment arrives slowly and often after the losses. Geographic anchoring and skill specificity block relocation; the gains from the transition accrue elsewhere while the closure costs concentrate here.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_fuel_dependent_communities, payer,
    moderate, biographical, trapped, regional).

% Inherit whatever atmospheric stock and technological dependencies the present arrangement leaves behind. If decoupling proceeds fast enough they receive a stabilized climate paid for fairly; if it does not, they receive residual warming plus a debt of undemonstrated removal capacity that present accounting treats as an asset. They hold no seat in any negotiating forum and cannot exit the inherited climate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Already experiencing intensifying floods, heat, and storm damage while the frame channels scarce climate finance toward long-horizon mitigation and innovation. Adaptation needs are subordinated in allocation decisions made elsewhere. Representation in the relevant forums is thin relative to exposure, and migration is bounded by border regimes.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations, excluded).

% Propose structural transformation of wealthy-nation economies — universal basic services, working-time reduction, democratic firm ownership — as the climate response. The prevailing legitimacy criteria rule their proposals out of fundable policy space before evaluation. They operate through protest and marginal academic channels rather than negotiation tables.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_and_postgrowth_movements, excluded,
    moderate, generational, constrained, global).

% Supplies the cost-benefit architecture — discount rates, damage functions, social cost of carbon estimates, net-zero pathway feasibility — through which competing climate responses are compared and ranked. Observes the frame and shapes it simultaneously; the modeling choices function as the frame's interpretive layer.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, integrated_assessment_modeling_community, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, integrated_assessment_modeling_community, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, incumbent_fossil_energy_producers).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the core collective-action problem of climate change: emissions are an unpriced externality, so the arrangement coordinates abatement across sovereign economies by pricing carbon, pooling innovation investment, and offering every major emitter a growth-compatible path to participation — keeping all large economies inside one regime instead of free-riding.
% TRANSFER_FUNCTION: Moves resources from current-period taxpayers and consumers (carbon prices, subsidy finance) into low-carbon technology sectors, carbon-market fee streams, and preserved incumbent operations; moves deferred costs — residual warming risk and dependence on undemonstrated removal capacity — onto future generations; moves political feasibility to sitting governments.
% ABSENT_VOICES: Climate-vulnerable populations already living with impacts would object that adaptation is subordinated; future generations have no seat anywhere; degrowth and post-growth thinkers are ruled illegitimate ex ante rather than argued with; uncompensated low-income households face pricing designed without them. Each sits outside the rooms where the legitimacy criteria are maintained.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority legitimacy norm vanished overnight, climate finance would reroute toward adaptation and resilience infrastructure, carbon markets would lose their legitimating frame and contract sharply, incumbent transition schedules would lose protection and face stranding pressure, and the degrowth coalition would move from excluded to contending. The entire architecture of climate diplomacy, green industrial policy, and sustainable finance reorganizes around whichever legitimacy criterion replaces it.
% FOUNDING_PROBLEM: In the late 1990s and 2000s, binding emission cuts were politically blocked wherever they threatened growth: developing nations refused development caps, wealthy electorates refused consumption limits, and the Kyoto-era architecture stalled under exactly that tension. The arrangement was built to make climate action feasible by guaranteeing that legitimacy requires nothing incompatible with continued growth — decouple, do not de-grow.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: IPCC synthesis reports document the persistent gap between pledged mitigation and required trajectories under growth-compatible assumptions; vulnerable-nation negotiating blocs and climate justice organizations attest that the frame underdelivers for those already impacted; independent empirical literatures on absolute decoupling rates dispute whether the founding bargain remains executable on its own terms. Corroboration does not come from the frame's own beneficiary set alone.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.60 at interval end) because the arrangement's costs and benefits are systematically misaligned in time and place: present consumers and capital keep growth, while residual warming risk and removal-capacity debt accumulate on future generations and on populations already impacted. Suppression (0.52) is primarily discursive and institutional rather than coercive — alternative framings are not banned but are defunded, unmodeled, and ruled out of fundable policy space; the rising suppression_requirement series tracks the maturation of that gating machinery (modeler consensus, funding criteria, investor disclosure frameworks, central bank scenario practice). Theater ratio (0.40) reflects a real and growing performative layer: net-zero pledges dated past officeholders' tenures, offset quality scandals, and innovation rhetoric substituting for measured decarbonization, alongside genuine deployment of renewables and functioning pricing schemes. Accessibility collapse is low (0.35) because the sibling alternatives remain visible and live — they have not collapsed, they are actively contested. Resistance (0.60) is real on two flanks: pricing payers (fuel-tax protests) and frame dissenters (justice and post-growth movements). All three tracked series run on one shared time grid (t=0,5,10,15,20,25,30) with endpoint values matching the base_properties scalars. Claim and metrics are independent: tangled_rope is claimed on structural grounds — a genuine coordination function AND an identifiable victim set held together by active enforcement — while the metrics above describe observed operation without tuning toward any predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda_setter seats should compute very different types from identical structural data. From the incumbent producer's or intermediary's position the arrangement is a stable, profitable coordination order they helped design; from the low-income household's or fossil-community's position it is a pricing regime whose costs concentrate locally while gains accrue elsewhere; from the future-generations seat — computed structurally despite physical absence — the same arrangement is deferred-cost accumulation with no recourse. Coalition analysis notes an asymmetry: powerless present-day payers (vulnerable populations, price-exposed households) can in principle coordinate, but the largest victim seat (future generations) cannot coalition at all, which is precisely why its extraction computes at the full-target end. The engine computes this per-seat divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: incumbents (arbitrage-grade exit, protected assets) sit nearest the beneficiary end; green technology and market intermediaries collect direct flows; governments derive low d as declared beneficiaries though they also bear fiscal and electoral costs, pulling them slightly toward symmetric; wealthy consumers carry dual roles and land near the middle. Declared victims derive high directionality: future generations (powerless, trapped, no exit from the inherited atmosphere) sit nearest the full-target end; low-income households and fossil-dependent communities are trapped payers; climate-vulnerable populations combine payer exposure with exclusion from the conversation. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct structural relationships, and the schema's power-atom-keyed override mechanism would be too blunt to improve on the derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making climate action politically feasible under growth constraints — is contested rather than dead: the feasibility problem persists, but whether THIS solution remains executable is disputed by the decoupling-rate evidence. The R5 mismatch consumer therefore reads status=contested x verdict=world_rearranges: no zombie flag fires, correctly, because the arrangement is still actively maintained and contested rather than inertially administered. The classification prevents two opposite mislabels. Calling this a rope ignores the victim set: the growth-preservation clause is not costless coordination, it is a distributional choice whose deferred costs land on seatless parties. Calling it a snare erases the genuine coordination achievement — every prior architecture that demanded growth sacrifice collapsed, and this one keeps all major emitters inside one regime. The tangled_rope claim holds both facts: coordination function real, extraction asymmetric, enforcement (pricing legislation, market regulation, discursive gating) required throughout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading (mitigation_priority) of the kernel climate_response_legitimacy; how would the classification change under the sibling readings?',
    'Comparative classification across the three reading-stories: hold the structural data constant, swap the legitimacy criterion, and observe which seats migrate between beneficiary and victim.',
    'Under adaptation_priority, climate_vulnerable_populations move from excluded/payer to primary beneficiary and future_generations recede in the victim set; under degrowth_transformation, current_generation_wealthy_consumers and incumbent producers become primary targets. The victim set — and therefore effective extraction for every seat — is reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: one-of-three readings; the location of the disagreement is the growth-compatibility axiom, and the victim set is reading-indexed.').

omega_variable(
    decoupling_sufficiency_uncertainty,
    'Will absolute decoupling of GDP from territorial and consumption-based emissions proceed fast enough to meet temperature goals without large residual burdens falling on future generations?',
    'Carbon-budget arithmetic against observed decoupling rates: compare required annual reduction percentages with achieved rates across major economies over successive five-year windows.',
    'If decoupling is insufficient, future_generations convert from conditional to realized victims, base extractiveness trends upward, and the classification drifts from tangled_rope toward snare; if sufficient, the coordination reading strengthens and extraction compresses toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_sufficiency_uncertainty, empirical, 'Whether the frame''s core empirical premise holds at the required scale.').

omega_variable(
    cdr_scaleup_dependency_risk,
    'Do net-zero architectures built on this frame depend on carbon dioxide removal at scales never demonstrated, and what happens to the deferred cost burden if CDR underdelivers?',
    'Track commissioned versus delivered CDR capacity against the removal quantities embedded in national net-zero plans; audit the delivery gap at each global stocktake.',
    'A widening delivery gap converts the frame''s technological optimism into intergenerational cost-shifting: present consumption was extracted against a promise future generations must service. Reclassification pressure toward snare intensifies with the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scaleup_dependency_risk, empirical, 'Technological dependency risk embedded in the frame''s accounting of future removal.').

omega_variable(
    discursive_suppression_mechanism,
    'Is the measured suppression of alternative climate framings structural (funding gates, mandate design, modeler gatekeeping) or internalized (researchers and officials self-limiting to fundable framings)?',
    'Post-frame-change funding and career trajectories: if research and policy entrepreneurship diversifies rapidly wherever the legitimacy monopoly breaks, the suppression was structural; if self-limitation persists after the gates open, it is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlives the arrangement itself — the target carries the frame''s boundaries with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_suppression_mechanism, empirical, 'Structural versus internalized mechanism behind the suppression of sibling framings.').

omega_variable(
    authority_framing_underdetermination,
    'Is the frame''s authority genuinely expertise-grounded (voluntary deference to demonstrated competence in cost-benefit analysis), or extraction-grounded (an institutional-financial apparatus whose authority depends on preventing revision of the frame)?',
    'Test interpretive responsiveness: whether the IAM/social-cost-of-carbon layer updates under adverse evidence (discount-rate revisions, damage-function updates) or absorbs drift without surfacing revision; trace who funds the modeling infrastructure and who collects from its outputs.',
    'If extraction-grounded, the CS classification shifts authority_grounding from expertise to extraction and the interpretation layer reads as a drift-denial buffer; the constraint''s legitimacy claim weakens and its enforcement machinery reads as self-protection rather than adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'CS-framing under-determination: expertise versus extraction as the true ground of the frame''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t0, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t5, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t10, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t15, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t20, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t25, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t0, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t5, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t10, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t15, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t20, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t25, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t0, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t5, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.43).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t10, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t15, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t20, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t25, observed).
narrative_ontology:measurement(clim_legit_mitigation_priority_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(clim_legit_mitigation_priority_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'legitimate climate response' is a colloquial label covering three structurally distinct arrangements with different epsilon values, beneficiary/victim structures, and failure modes. This member is the upstream, most institutionally entrenched reading: it influences the adaptation sibling's resourcing and legitimacy conditions (mitigation-first allocation starves adaptation finance) and forecloses the degrowth sibling within any single legitimacy framework (the growth-preservation premise contradicts the dismantle-the-imperative premise). Each sibling is authored as its own story; no observable-switching parameter spans them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
