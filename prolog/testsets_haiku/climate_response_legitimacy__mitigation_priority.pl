% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy (Decoupling via Technology and Carbon Pricing)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate-response legitimacy holds that
 *   atmospheric emissions can be decoupled from economic growth through
 *   technological innovation (renewable energy, efficiency, carbon capture),
 *   carbon pricing (internalizing the externality), and market-driven
 *   deployment. The frame is endorsed by IPCC modeling, mainstream climate
 *   policy institutions, renewable-energy investors, and wealthy-nation
 *   governments. It positions climate response as compatible with growth:
 *   nations can reduce emissions while preserving or increasing GDP and
 *   consumption. This reading contests two alternatives: the
 *   adaptation-priority reading (focus on resilience to unavoidable warming)
 *   and the degrowth-transformation reading (require structural economic
 *   change). This constraint story examines the mitigation-priority reading
 *   as a contested kernel instantiation, not as settled truth. The framing
 *   carries benefits for incumbent industries (which can participate in
 *   transition without transformation), technology vendors (who capture
 *   demand from renewable scale-up), wealthy-nation consumers (who avoid
 *   downshift in living standards), and financial intermediaries (who manage
 *   carbon markets). It carries costs for future generations (if decoupling
 *   fails), developing economies (who pay adaptation costs while wealthy
 *   nations transition), workers in fossil industries (stranded assets,
 *   constrained retraining), and excluded advocacy groups (adaptation and
 *   degrowth advocates).
 *
 * KEY AGENTS:
 *   - incumbent_carbon_intensive_industries: institutional power, arbitrage exit — participate in transition via carbon credits and renewable investment without business-model transformation
 *   - wealthy_nations_near_term_consumption: institutional power, mobile exit — maintain consumption and growth via decoupled emissions, shifting costs to developing nations and future eras
 *   - technology_vendors_renewable_cdr: powerful institutions, arbitrage exit — primary winners, capture rents from renewable scale-up and carbon-removal deployment
 *   - climate_modelers_mitigation_focused_research: institutional power, identity-locked exit — set intellectual agenda through IPCC modeling, dependent on mitigation-first framing for authority and funding
 *   - future_generations_climate_impacts: powerless, trapped exit — structural victims if decoupling fails; inherit both warming and institutional lock-in to a technological fix that may not materialize
 *   - developing_economies_adaptation_costs: moderate power, constrained exit — bear adaptation funding gaps and technology costs while mitigation benefits flow to wealthy nations
 *   - workers_stranded_carbon_assets: powerless, identity-locked exit — face immediate unemployment and community disruption from asset stranding; retraining and just-transition promises are underfunded
 *   - adaptation_priority_advocates: organized, constrained exit — excluded from main policy coalition despite expertise in resilience and adaptive capacity
 *   - degrowth_transformation_advocates: organized, constrained exit — excluded from main policy coalition because proposals challenge growth itself, not just energy systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy (Decoupling via Technology and Carbon Pricing)").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'd0cc1b63-266c-415e-ba1f-f425fb4526e3').
narrative_ontology:cs_kernel_codification('d0cc1b63-266c-415e-ba1f-f425fb4526e3', distributed).
narrative_ontology:cs_authority_grounding('d0cc1b63-266c-415e-ba1f-f425fb4526e3', distributed).
narrative_ontology:cs_reading_relation('d0cc1b63-266c-415e-ba1f-f425fb4526e3', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('d0cc1b63-266c-415e-ba1f-f425fb4526e3', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('d0cc1b63-266c-415e-ba1f-f425fb4526e3', foundational, decoupling_growth_from_emissions_achievable).
narrative_ontology:cs_axiom_status(decoupling_growth_from_emissions_achievable, holdable).
narrative_ontology:cs_axiom_grounding('d0cc1b63-266c-415e-ba1f-f425fb4526e3', decoupling_growth_from_emissions_achievable, empirically_contingent).
narrative_ontology:cs_axiom('d0cc1b63-266c-415e-ba1f-f425fb4526e3', foundational, technology_deployment_solves_externality).
narrative_ontology:cs_axiom_status(technology_deployment_solves_externality, holdable).
narrative_ontology:cs_axiom_grounding('d0cc1b63-266c-415e-ba1f-f425fb4526e3', technology_deployment_solves_externality, instrumental).
narrative_ontology:cs_axiom('d0cc1b63-266c-415e-ba1f-f425fb4526e3', secondary, carbon_pricing_internalizes_global_cost).
narrative_ontology:cs_axiom_status(carbon_pricing_internalizes_global_cost, holdable).
narrative_ontology:cs_axiom_grounding('d0cc1b63-266c-415e-ba1f-f425fb4526e3', carbon_pricing_internalizes_global_cost, conventional).
narrative_ontology:cs_reference_frame('d0cc1b63-266c-415e-ba1f-f425fb4526e3', growth_compatible_emissions_reduction).
narrative_ontology:cs_drift_state('d0cc1b63-266c-415e-ba1f-f425fb4526e3', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0cc1b63-266c-415e-ba1f-f425fb4526e3', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, wealthy_nations_near_term_consumption).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, technology_vendors_renewable_cdr).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, financial_intermediaries_carbon_markets).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations_climate_impacts).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, developing_economies_adaptation_costs).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, workers_stranded_carbon_assets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, climate_modelers_mitigation_focused_research).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, incumbent_carbon_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fossil fuel producers, high-emission manufacturing, and incumbent energy infrastructure. The mitigation-priority frame allows them to participate in transition (purchasing offsets, investing in renewables as secondary revenue, lobbying for favorable carbon pricing rules) without structural transformation of their core business model. They bear near-term costs from carbon pricing but avoid the radical restructuring demanded by degrowth or the full adaptation-cost burden of high-warming scenarios. Their exit is arbitrage: they can move capital to renewable ventures, acquire carbon credits, or lobby for carbon-border adjustments.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_carbon_intensive_industries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_carbon_intensive_industries, payer).

% Citizens and businesses in high-income nations (EU, North America, developed Asia-Pacific). The mitigation-priority frame promises they can maintain consumption trajectories and growth trajectories during the transition: emissions fall via efficiency and switching to renewables, but GDP and material living standards are decoupled and preserved. They benefit from the temporal bargain: transition costs are managed as carbon pricing (passed partly to developing nations and future consumers), not as downshift in material throughput.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, wealthy_nations_near_term_consumption, beneficiary,
    institutional, biographical, mobile, global).

% Solar/wind manufacturers, carbon capture and removal (CDR) startups, battery and electric-vehicle makers, smart-grid vendors. The mitigation-priority frame creates demand for their products as the primary mechanism of response. They capture rents from scale-up of renewables, carbon-removal infrastructure, grid modernization, and electrification. Their exit is strong: they can pivot between renewable energy, CDR, and grid-tech markets as policy shifts.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, technology_vendors_renewable_cdr, beneficiary,
    powerful, biographical, arbitrage, global).

% Investment funds, carbon credit traders, green bonds underwriters, ESG rating agencies. The mitigation-priority frame operationalizes through carbon pricing and market mechanisms, which require financial infrastructure to monetize, trade, and validate credits. They capture spreads, management fees, and trading profits as the carbon market scales. Their exit is strong: capital flows to wherever carbon pricing and renewable investment is highest.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, financial_intermediaries_carbon_markets, beneficiary,
    institutional, biographical, arbitrage, global).

% Scientists and research institutions whose authority and funding depend on the mitigation-first framing (IPCC carbon-budget paradigm, integrated assessment models built on mitigation-cost assumptions, energy-systems modeling). They set the intellectual agenda through climate science communication, policy briefings, and model outputs that privilege mitigation scenarios. Their identity is locked: accepting that the mitigation framework is contested rather than settled would destabilize career investment and institutional authority.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_modelers_mitigation_focused_research, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, climate_modelers_mitigation_focused_research, beneficiary).

% Political actors and policy advocates who have built careers and constituencies around carbon pricing and renewable energy deployment (carbon tax advocates, cap-and-trade designers, climate-focused NGOs in wealthy nations). They set policy direction through regulatory design, legislation, and international negotiations. Their exit is constrained: the mitigation frame is the foundation of their political base, and moving to degrowth or high-adaptation frames would require rebuilding credibility and coalition support.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, policy_entrepreneurs_carbon_pricing, agenda_setter,
    organized, biographical, constrained, national).

% People born after 2050 who will inherit the physical climate outcome. They are the structural victims because the mitigation-priority frame's viability depends on the success of decoupling, carbon capture, and technological transitions that are not yet proven at scale. If decoupling fails, future generations inherit cumulative warming and institutional lock-in to a technological fix that never worked. Their exit is trapped: they cannot opt out of climate impacts or choose a different institutional response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations_climate_impacts, payer,
    powerless, civilizational, trapped, global).

% Nations with lower per-capita emissions but high climate vulnerability (small-island states, African nations, South Asia). The mitigation-priority frame privatizes adaptation costs to them (they must fund resilience infrastructure while wealthy nations reduce emissions through technology) and transfers technology costs via carbon-border adjustments and trade rules. They bear the gap between mitigation ambition and actual warming.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, developing_economies_adaptation_costs, payer,
    moderate, biographical, constrained, national).

% Coal miners, oil rig workers, petroleum refinery operators, and their dependent communities. The mitigation-priority frame requires phasing out carbon-intensive production, but the transition promises (retraining, green jobs, just transition) are underfunded relative to the pace of asset stranding. Their identity is locked to their work and place: leaving coal mining means leaving their community, retraining for renewable-energy jobs that may not exist locally or pay at equivalent levels.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, workers_stranded_carbon_assets, payer,
    powerless, biographical, identity_locked, regional).

% Researchers, policymakers, and advocates centered on climate adaptation and resilience (adaptive capacity building, water security, agricultural transformation, ecosystem restoration). The mitigation-priority frame marginalizes them by positioning adaptation as a secondary concern (residual to mitigation success) rather than a co-equal response. They would argue that the mitigation frame's dependence on unproven technologies crowds out adaptation funding.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_priority_advocates, excluded,
    organized, generational, constrained, global).

% Researchers, policymakers, and advocates proposing structural economic transformation (universal basic services, working time reduction, democratic firm ownership, circular economies). They argue the mitigation frame's assumption that decoupling is achievable without demand reduction is empirically unfounded and that the frame is a cover story for preserving incumbent power. They are excluded from the main climate policy table because degrowth frames challenge growth itself.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_transformation_advocates, excluded,
    organized, generational, constrained, global).

% The IPCC synthesizes climate science and reports to governments. It observes the mitigation-priority frame from an analytical seat: its assessments are cited to justify the frame, but the IPCC itself does not advocate for a particular policy reading. However, the modeling assumptions and scenario architecture it uses privilege mitigation-first futures, creating path-dependence in policy legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, intergovernmental_panel_climate_change, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, technology_vendors_renewable_cdr).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a global accounting system for emissions reductions, price carbon at the source or point of consumption, and create investment signals for renewable energy and efficiency technology. The coordination problem is: how do independent emitters internalize a global externality without binding agreement? Carbon pricing and accounting rules solve the collective-action problem of verification, attribution, and enforcement.
% TRANSFER_FUNCTION: Moves resources from high-emission economic activities to low-emission alternatives (via carbon pricing, technology subsidies, and finance flows) and from current consumption to future-generation climate resilience (in theory). In practice, it transfers: carbon-pricing revenue from developing and transitional economies to wealthy nations (via trade effects and carbon leakage); adaptation funding gaps from wealthy to vulnerable nations (underfunding); technology rents to renewable and CDR vendors; and stranding costs to fossil-fuel workers and carbon-intensive communities.
% ABSENT_VOICES: Future generations (who cannot negotiate or vote); developing nations with limited capital for rapid renewable transitions (voice in UNFCCC is weak); workers and communities dependent on carbon-intensive industries (excluded from policy design); ecological systems and non-human species (no representation). Adaptation-priority advocates and degrowth advocates are present in discourse but structurally marginalized in agenda-setting: their proposed alternatives are framed as secondary or infeasible, not as co-equal policy options.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority frame and its enforcement mechanisms (carbon markets, renewable subsidies, technology mandates) disappeared overnight, energy systems would not reorganize around efficiency and renewable deployment at the same pace. Emissions would remain coupled to GDP growth (absent the carbon-pricing signal). Technology vendors would face weaker demand signals. The political consensus on climate action would fragment, likely stalling bilateral agreements and capital flows. Wealthy nations would revert to lower climate policy ambition. Developing nations would pursue energy security independently, likely via incumbent fossil fuels.
% FOUNDING_PROBLEM: Atmospheric CO2 accumulation creates warming risk; current energy systems lock in carbon-intensive infrastructure; individual and firm incentives do not account for climate externality; technological substitution pathways exist but lack deployment scale. The founding problem, as mitigation-priority reads it: market failure in emissions pricing and coordination failure in global technology deployment. Solved by: internalizing the externality (carbon pricing) and coordinating technology scale-up (subsidies, standards, finance) while preserving growth (decoupling via efficiency and substitution).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (coupling of growth to emissions, market-failure framing) is attested by climate scientists, economists, and multilateral institutions (World Bank, IMF, IPCC). However, whether the founding problem is SOLVED by mitigation-priority framing is contested outside the beneficiary coalition. Adaptation-priority advocates attest the decoupling assumption is insufficiently vindicated and the frame leaves adaptation gaps. Degrowth advocates attest that the problem is not market failure but growth imperative itself, and that decoupling is empirically implausible at required scale. Workers and developing-economy representatives attest that the mitigation frame solves the founding problem for wealthy nations at the cost of extracting from vulnerable populations. These are testimonies from outside the mitigation-beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness reaches 0.68 at interval end because the constraint transfers resources from developing economies and future generations to incumbent industries and technology vendors. The decoupling assumption — that growth can be preserved while emissions fall — is empirically unproven at required scale, making the frame's core justification contingent on unvalidated technology and policy choices. Suppression rises to 0.62 because the frame requires active enforcement: carbon markets must be maintained against free-riding; adaptation-priority and degrowth alternatives must be marginalized in policy discourse; stranded-asset workers must be prevented from blocking transitions; developing-nation alternatives (independent energy security) must be constrained by carbon-border rules and finance conditionality. Theater rises to 0.44 because performative elements accumulate: renewable-energy announcements without accounting for rebound effects; carbon-offset claims that obscure permanence and additionality uncertainties; 'just transition' language without adequate retraining funding; and net-zero pledges that rely heavily on speculative carbon-removal technologies. The measurement series shows rising extraction and suppression over time (0–30 observed, 30–50 projected): as the mitigation frame solidifies in policy, the costs of maintaining the decoupling narrative (suppressing alternatives, defending carbon markets against criticism) increase. Accessibility collapse (0.51) is moderate because exit options persist for technology vendors and some wealthy-nation stakeholders (they can shift to adaptation or degrowth frames if politically advantageous), but are closing for workers and developing nations (constrained by finance and technology flows tied to mitigation commitments). Resistance (0.58) is moderate-to-high: developing nations challenge the mitigation frame in UNFCCC negotiations; youth climate activists push for more aggressive action; workers organize against asset stranding; and adaptation/degrowth researchers publish critiques, but policy-agenda control remains with mitigation-priority coalition.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is fundamental: The mitigation-priority frame is stable and beneficial when viewed from the technology-vendor seat (growth of their market, rents from deployment) and the wealthy-nation consumption seat (growth preserved, costs outsourced to developing nations). From the stranded-worker seat and future-generation seat, the frame appears as enforced extraction under a growth-preservation cover story — the supposed coordination around emissions reduction is actually coordination around maintaining incumbent wealth and consumption patterns. From the developing-economy seat, the frame extracts through dual channels: carbon-border rules that disadvantage their exports, and adaptation-cost transfer (wealthy nations reduce their emissions via technology, leaving developing nations to fund resilience infrastructure for warming caused largely by historical wealthy-nation emissions). From the adaptation-priority and degrowth-advocacy seats, the frame is a false solution that crowds out more robust responses by pre-empting policy space.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain reveals how different agents experience the same constraint through asymmetric extraction. Incumbent industries benefit (d ≈ 0.2, low end) from a transition that can be profitable and non-disruptive; technology vendors are strong beneficiaries (d ≈ 0.15, beneficiary end) because their growth depends on the frame. Wealthy-nation consumers benefit indirectly (d ≈ 0.35, beneficiary-leaning) by outsourcing transition costs. Climate modelers benefit (d ≈ 0.25, beneficiary end) via funded research and institutional authority. In contrast, future generations are pure targets (d ≈ 0.95, target end) because they inherit both warming (if decoupling fails) and institutional lock-in to a failed frame; they have trapped exit, no negotiating power, and accumulating costs. Developing economies are constrained targets (d ≈ 0.78, high target end): they bear adaptation costs, must accept carbon-border rules, and have limited exit (constrained by development dependency and finance flows). Stranded workers are targets (d ≈ 0.88, high target end) because asset stranding costs are immediate and localized, while retraining is underfunded and identity-locked exit is barred. Excluded advocates have moderate directional pressure (d ≈ 0.45, symmetric) because they are marginalized from extraction but also benefit (if more inclusive climate policy emerges); their position is unstable. The directionality derivation flows from beneficiary/victim declarations and exit-option asymmetry; no overrides are necessary because the structural data capture the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries a live mandatrophy tension. The founding problem (coupling of growth to emissions; market failure in pricing externality) is attested by mainstream institutions and scientists. The founding-problem status is contested: mitigation-priority advocates assert the problem is being solved through market mechanisms and technology; adaptation and degrowth advocates assert the founding problem persists and the mitigation frame is a cover story for growth preservation. The disappearance verdict is world_rearranges: if the mitigation frame and its enforcement mechanisms (carbon pricing, technology subsidies, renewable mandates) disappeared, energy systems would revert to more carbon-intensive paths, and climate ambition would decline. This verdict is consistent with the constraint being functional (not mandatrophic). However, the theater_ratio rising to 0.44 and the measurement of suppression increasing over time signal that performative maintenance is accumulating: renewables are deployed but emissions-reduction targets slip due to rebound effects; carbon offsets are traded but quality questions grow; just-transition promises are made but funding remains sparse. If theater continues rising and the core decoupling empirically fails (emissions rebound faster than efficiency gains), the constraint could drift into mandatrophy: it continues to be enforced and defended despite its founding-problem-solving function being obsolete, replaced by a pure wealth-transfer operation. The current reading holds the constraint as a tangled rope (genuine coordination function around externality pricing + asymmetric extraction through decoupling-assumption risk), not as a piton; but the trajectory (rising theater, rising suppression, projected stalled decoupling) is consistent with piton-drift if the technology and policy bets fail over 30–50 year horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_feasibility,
    'Can economic growth be decoupled from emissions at required scale and speed without demand reduction?',
    'Empirical data on relative decoupling (intensity improvements) vs. absolute decoupling (actual emissions reductions) over the 2020–2050 period, accounting for consumption-based emissions, embodied carbon in imports, and rebound effects. Natural experiments from carbon-pricing jurisdictions comparing predicted vs. actual decoupling outcomes.',
    'If decoupling fails (rebound effects or trade leakage exceed efficiency gains), the mitigation frame''s core legitimacy collapses and future generations enter the victim set even more severely (they inherit both warming and the institutional assumption that growth is compatible with climate safety, which is falsified). This would trigger reclassification from tangled rope toward snare (extraction under a failed cover story) and piton-drift (institutional inertia maintaining a defunct frame).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_empirical_feasibility, empirical, 'Whether the decoupling assumption holds under observed conditions and policy implementation').

omega_variable(
    carbon_removal_scale_risk,
    'Can carbon capture and removal (CDR) and direct air capture (DAC) be deployed at the scale and cost the mitigation frame assumes, or will technological and resource constraints force reliance on nature-based solutions that compete with adaptation?',
    'Technical feasibility studies, cost trajectories, energy requirements, land-use competition analysis, and pilot deployment results from 2025 onwards. Assessment of whether CDR scales faster than renewable energy or lags critically.',
    'If CDR remains expensive and slow-scaling (current trend), the mitigation frame''s reliance on late-cycle carbon removal to offset early continued emissions makes future generations structural victims of deferred action. The constraint''s extraction from the future becomes more severe. The technology-vendor seat becomes parasitic rather than genuinely beneficial if CDR promises drive investment while actual deployment stalls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_removal_scale_risk, empirical, 'Whether carbon-removal technology can achieve required scale within the mitigation frame''s assumptions').

omega_variable(
    adaptation_funding_convergence,
    'Will developing nations'' adaptation costs converge with or diverge from available finance under mitigation-priority frame? Is the gap sustainable politically?',
    'Tracking of adaptation finance commitments vs. actual disbursements, cost estimates for resilience infrastructure in vulnerable nations, and political economy of climate finance negotiations. Assessment of whether adaptation costs exceed mitigation benefits for vulnerable nations.',
    'If adaptation-funding gap widens (trend to date), developing economies'' extraction increases and their exit options further constrain. The constraint drifts toward snare classification in the developing-economy seat. This creates instability: the UNFCCC consensus on mitigation-priority depends on developing-nation participation; if the frame is experienced as pure extraction by that coalition, the legitimacy and enforceability of carbon pricing and technology mandates erodes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_funding_convergence, empirical, 'Whether mitigation-priority frame adequately funds adaptation in vulnerable regions or transfers costs to developing economies').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Are the mitigation-priority, adaptation-priority, and degrowth-transformation readings logically foreclosed from each other, or can they coexist in a single policy framework at different scales or sequentially?',
    'Examination of whether a nation can adopt mitigation in energy systems, adaptation in infrastructure, and degrowth in consumption simultaneously without internal contradiction. Case studies from jurisdictions attempting multi-reading policy mixes (e.g., EU mitigation targets + adaptation frameworks + circular-economy regulations).',
    'If the readings genuinely foreclose each other (only one can be adopted), the engine will reclassify reading_relations from coexists_with to forecloses, and the constraint''s stability will depend on maintaining institutional dominance of the mitigation reading. If they coexist, the constraint is more stable but becomes a coalition problem (multiple readings held simultaneously by different constituencies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Whether the three contested readings of climate-response legitimacy are logically independent or mutually exclusive').

omega_variable(
    justice_distribution_extractiveness_perception,
    'Is the measured extractiveness (0.68) experienced equally across seats, or is it perceived as more extractive by developing-economy and stranded-worker seats due to asymmetric cost distribution?',
    'Qualitative research on how different stakeholder groups perceive the fairness and legitimacy of mitigation costs and benefits; polling on acceptance of carbon pricing in wealthy vs. developing nations; analysis of political resistance and coalition stability across seats.',
    'If extractiveness is perceived as higher (>0.75) in developing and worker seats, suppression requirements will need to increase to maintain the constraint, driving piton-drift and mandatrophy risk. Alternatively, the constraint could fracture: developing nations and worker coalitions could defect to degrowth or adaptation-priority frames if mitigation-priority is experienced as unjust extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justice_distribution_extractiveness_perception, empirical, 'Whether extractiveness from mitigation-priority frame is perceived as legitimate or as unjust extraction across stakeholder seats').

omega_variable(
    technological_solutionism_identity_lock,
    'Does the mitigation-priority frame''s dependence on unproven technology (carbon capture, renewable scaling, grid transformation) constitute an identity lock for climate modelers and research institutions, preventing them from entertaining degrowth or adaptation-priority frames as equally viable?',
    'Bibliometric analysis of citation patterns and institutional funding flows in climate research; interviews with researchers about career incentives and institutional pressure; examination of alternative scenarios (degrowth, high-adaptation) in IPCC and academic modeling vs. mainline mitigation scenarios.',
    'If identity lock is confirmed, the research consensus (which carries authority in policy) is partially fabricated by institutional incentives rather than scientific evidence, reducing the frame''s legitimacy. This increases omega uncertainty and mandatrophy risk if the frame''s scientific foundation erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_solutionism_identity_lock, conceptual, 'Whether mitigation-priority dominance in climate research reflects scientific consensus or institutional identity lock to a particular frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__mitigation_priority, theater_ratio, 6, 0.32).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__mitigation_priority, theater_ratio, 12, 0.37).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__mitigation_priority, theater_ratio, 18, 0.4).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__mitigation_priority, theater_ratio, 24, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.44).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__mitigation_priority, theater_ratio, 40, 0.45).
narrative_ontology:measurement(clim_tr_t50, climate_response_legitimacy__mitigation_priority, theater_ratio, 50, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__mitigation_priority, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__mitigation_priority, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__mitigation_priority, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__mitigation_priority, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__mitigation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(clim_be_t50, climate_response_legitimacy__mitigation_priority, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__mitigation_priority, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__mitigation_priority, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__mitigation_priority, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__mitigation_priority, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__mitigation_priority, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(clim_su_t50, climate_response_legitimacy__mitigation_priority, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.22).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, carbon_pricing_enforcement_regime).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, renewable_energy_technology_deployment).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, developing_economy_adaptation_finance).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the climate_response_legitimacy kernel. Each reading has its own beneficiary/victim structure, ε value, and classification type. They are linked via network.affects_constraints to show the constraint family. The decomposition is necessary because a single averaged constraint across all three readings would obscure the directional asymmetries and seat divergence. Each reading should be authored and classified independently; the engine computes the family relationships through the network links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
