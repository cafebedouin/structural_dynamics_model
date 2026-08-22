% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Growth-Compatible Emissions Reduction Framework
 *   domain: climate_policy/political_economy/intergenerational
 *
 * SUMMARY:
 *   The mitigation_priority reading of the climate harm prevention kernel
 *   asserts that legitimate climate response prioritizes emissions reduction
 *   (preventing future warming damages) via technological transition within a
 *   growth-compatible economic framework. This reading structures the UNFCCC,
 *   Paris Agreement, and mainstream climate policy: the core promise is that
 *   decarbonization can be achieved while maintaining or accelerating growth
 *   in high-income nations through renewable energy deployment, efficiency
 *   improvements, and market mechanisms (carbon pricing). The constraint's
 *   enforcement machinery includes regulatory phase-outs of fossil fuel
 *   infrastructure, renewable energy mandates, carbon pricing, and technology
 *   subsidies. The reading's primary beneficiary is future generations (who
 *   avoid severe warming damages); secondary beneficiaries are
 *   renewable-technology sectors and climate-vulnerable nations. Primary
 *   cost-bearers are fossil fuel industries (stranded assets) and
 *   carbon-intensive workers (displacement); secondary cost-bearers are
 *   present-generation populations whose consumption is constrained by carbon
 *   limits. The constraint is claimed as rope (genuine coordination around a
 *   feasible solution) while the metrics describe moderately extractive
 *   operation with active enforcement — the gap is deliberate and
 *   engine-computed, not authored reconciliation.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary; receive damage prevention but bear no agency in the constraint choice
 *   - fossil_fuel_industries: Institutional payer; face stranded assets and regulatory phase-out
 *   - carbon_intensive_workers: Powerless payer with identity-locked exit; occupational displacement driven by constraint
 *   - renewable_technology_sectors: Institutional beneficiary; capture growth trajectory from constraint-driven investment
 *   - international_climate_regime: Agenda-setter; establishes enforcement machinery and targets
 *   - climate_vulnerable_nations: Organized beneficiary coalition; organized to advocate for mitigation stringency
 *   - adaptation_advocates: Excluded; argue mitigation alone insufficient and politically infeasible
 *   - degrowth_advocates: Excluded; argue growth-compatibility assumption is physically impossible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Growth-Compatible Emissions Reduction Framework").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'ddae99d3-1dc8-45ef-af1b-6f99384d011b').
narrative_ontology:cs_kernel_codification('ddae99d3-1dc8-45ef-af1b-6f99384d011b', fixed_text).
narrative_ontology:cs_authority_grounding('ddae99d3-1dc8-45ef-af1b-6f99384d011b', extraction).
narrative_ontology:cs_interpretation_layer_present('ddae99d3-1dc8-45ef-af1b-6f99384d011b').
narrative_ontology:cs_reading_relation('ddae99d3-1dc8-45ef-af1b-6f99384d011b', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ddae99d3-1dc8-45ef-af1b-6f99384d011b', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('ddae99d3-1dc8-45ef-af1b-6f99384d011b', foundational, growth_compatible_decarbonization_feasible).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_feasible, holdable).
narrative_ontology:cs_axiom_grounding('ddae99d3-1dc8-45ef-af1b-6f99384d011b', growth_compatible_decarbonization_feasible, empirically_contingent).
narrative_ontology:cs_axiom('ddae99d3-1dc8-45ef-af1b-6f99384d011b', foundational, future_harm_prevention_primary_mandate).
narrative_ontology:cs_axiom_status(future_harm_prevention_primary_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ddae99d3-1dc8-45ef-af1b-6f99384d011b', future_harm_prevention_primary_mandate, deontological).
narrative_ontology:cs_reference_frame('ddae99d3-1dc8-45ef-af1b-6f99384d011b', paris_agreement_mitigation_centrality).
narrative_ontology:cs_drift_state('ddae99d3-1dc8-45ef-af1b-6f99384d011b', contemporary_emissions_trajectory_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddae99d3-1dc8-45ef-af1b-6f99384d011b', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_technology_sectors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_consumption_deferring_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the physical consequences of present-day atmospheric GHG concentrations. Their welfare is the stated justification for the constraint — preventing warming damages they cannot avoid. They have no seat at present policy negotiation and cannot exit the constraint's scope.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face stranded asset risk, regulatory phase-out timelines, and financing withdrawal as the constraint's enforcement machinery (carbon pricing, emissions standards, renewable mandates) compresses the economic space for new coal/gas infrastructure. Their exit option is institutional transformation (diversification into renewable energy or utilities), which is constrained by capital lock-in and incumbent business models.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_industries, payer,
    institutional, generational, constrained, global).

% Communities dependent on coal mining, oil extraction, gas refining, and heavy manufacturing face employment displacement as the constraint drives transition. Their exit is identity-locked: occupational identity, regional economics, and social capital are fused to the carbon-intensive sectors. Retraining promises are authored into policy but unevenly delivered; their options are constrained to geographic relocation, downward mobility, or resistance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_workers, payer,
    powerless, biographical, identity_locked, regional).

% Solar, wind, battery, and grid-modernization industries capture the growth trajectory the constraint creates: subsidies, procurement mandates, R&D funding, and market-expansion rules send capital flows to them. They benefit from the constraint's enforcement without running it (institutional agenda-setters do that); their exit option is to arbitrage across jurisdictions with different carbon prices or transition speeds.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_technology_sectors, beneficiary,
    institutional, generational, arbitrage, global).

% Small island states and low-lying delta regions face existential risk from warming; emissions reduction directly reduces their damages. They are organized as a coalition (Alliance of Small Island States, least-developed-country bloc) and advocate loudly for mitigation stringency. They have constrained exit: they cannot unilaterally reduce warming and are dependent on Global North cooperation for financing and technology transfer.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_vulnerable_nations, beneficiary,
    organized, generational, constrained, global).

% Populations in high-income nations whose energy and material consumption are held to lower growth rates by carbon constraints (implicit carbon budgets, energy efficiency mandates, fuel prices). They are not destitute but face deferred consumption choices — higher energy costs, fewer air miles, less meat consumption — driven by the constraint's enforcement. Exit is constrained: they inhabit jurisdictions with binding climate policy; leaving jurisdiction is possible but costly.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_consumption_deferring_populations, payer,
    moderate, biographical, constrained, regional).

% The UNFCCC, national climate authorities, and the broader legitimacy apparatus establish emissions targets, carbon accounting rules, renewable procurement mandates, and phase-out timelines. They interpret the constraint as the embodiment of climate science (future damage prevention) translated into policy. They set the enforcement machinery and adjudicate compliance. Their exit option is analytical: they can be studied but do not exit the arrangement.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, international_climate_regime, agenda_setter,
    institutional, generational, analytical, global).

% The epistemic authority that legitimates the constraint: the IPCC consensus that anthropogenic GHG drives warming and that deep emissions reductions are necessary to prevent severe damage. It is not an agent collecting rents but a vindicated proposition — the constraint's legitimacy depends on the consensus holding and being treated as decisive for policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_science_consensus, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__mitigation_priority, climate_science_consensus).

% Argue that mitigation alone is insufficient or politically infeasible, and that climate policy should prioritize near-term adaptation (flood defenses, drought-resistant agriculture, managed retreat). They are organized at policy conferences and in academic debate but are systematically deprioritized in funding and mandate architecture; their objections are heard but not centered.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_advocates, excluded,
    organized, generational, constrained, global).

% Argue that growth-compatible decarbonization is physically impossible and that climate policy must mandate economic contraction in high-income nations. They are organized in activist networks and academic work but are structurally excluded from mainstream climate negotiations — the growth-compatibility assumption is baked into the constraint's definition and cannot be negotiated within this reading.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, international_climate_regime).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns atmospheric science (warming consequences are real and severe) with a decarbonization strategy that does not require dismantling the growth economy, solving the apparent coordination problem: how do high-income nations reduce emissions without sacrificing growth? The answer is technological transition and efficiency gains substituting for production reduction.
% TRANSFER_FUNCTION: Moves the cost of emissions reduction (capital investment in renewable infrastructure, energy efficiency retrofit, grid modernization) and the cost of workforce transition (income loss in carbon-intensive sectors, geographic displacement, deferred consumption) from future generations (who would bear warming damages) to present generations and carbon-intensive economic sectors (who bear transition costs now).
% ABSENT_VOICES: Degrowth advocates and many climate-vulnerable populations argue the growth-compatibility assumption is the constraint itself — that the real trade-off is between growth and climate safety, and that prioritizing growth guarantees insufficient emissions reduction. They are excluded from mainstream climate negotiations by the constraint's definition (growth is not on the table for debate). Adaptation-focused voices are heard but deprioritized in funding allocation and political narrative, with little power to shift mandate emphasis.
% DISAPPEARANCE_RATIONALE: If the constraint (growth-compatible mitigation as the dominant framework) vanished overnight, climate policy would bifurcate: some jurisdictions would accelerate adaptation spending, others would experiment with degrowth policies, and emissions reduction would proceed at variable speeds with no coordinated decarbonization pathway. The fossil fuel industries would see regulatory pressure ease; renewable sectors would lose growth-narrative support; and present-day consumption patterns would not face carbon-driven constraint. The global coordination around a specific mitigation strategy would dissolve.
% FOUNDING_PROBLEM: Anthropogenic climate change poses catastrophic risk to future welfare; without deep emissions reductions, warming will exceed 2–3°C and cause severe damages (sea-level rise, agricultural disruption, ecosystem collapse). The constraint was built to solve the alignment problem: how to drive emissions reductions fast enough to prevent dangerous warming without requiring the Global North to abandon growth.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC consensus, national climate science agencies, and the 2015 Paris Agreement architecture all attest that the founding problem is live — warming is happening and emissions must be cut deeply and quickly. Climate-vulnerable nations and island-state coalitions corroborate the threat severity. Degrowth and adaptation advocates dispute whether growth-compatible mitigation is adequate (they argue the founding problem is in fact unsolvable within the stated framework), but they do not dispute that the founding problem itself is real.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.15 at 1990 → 0.68 at 2025 → 0.68 at 2050) reflects two dynamics: first, the constraint's enforcement machinery has strengthened dramatically as carbon regulatory infrastructure matured and carbon pricing widened (1990–2025); second, the measured extractiveness plateaus in projection (2025–2050) because by 2050 fossil fuel industries will have already borne most stranded-asset loss and carbon-intensive sectors will have largely transitioned, so the extraction per unit of remaining economic activity stabilizes. Suppression (0.52 at interval end) is moderate-to-high because the constraint depends on regulatory enforcement (carbon phase-outs, renewable mandates) and financial disincentives (carbon pricing) to overcome present-generation preferences for carbon-intensive consumption; it is lower than extractive snares (0.7+) because the constraint also carries real damage-prevention benefits that make participation rational for some stakeholders. Theater ratio (0.41) reflects growing gap between stated mitigation goals and actual emissions reduction: policy frameworks announce net-zero targets but continue fossil fuel subsidies and underinvest in grid infrastructure, so a substantial fraction of enforcement activity is performative commitment-signaling rather than functional emissions reduction. Accessibility_collapse (0.45) is moderate because alternatives persist: nations, corporations, and communities can still pursue adaptation-heavy or degrowth strategies, and technological pathways outside the constraint exist (though regulatory pressure constrains them). Resistance (0.58) is substantial because fossil fuel industries, carbon-intensive workers, and sovereignty-minded nations all mount organized resistance to the constraint's enforcement; this is not a natural law but an enforced arrangement meeting real opposition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (fossil fuel industries, carbon-intensive workers, present-generation consumers) and the agenda-setter seat (international climate regime, wealthy-nation governments) will compute dramatically different types. From the payers' position, the constraint appears as an enforced extraction justified by abstract future benefits; they bear concentrated costs today for diffuse benefits to people not yet born. From the agenda-setter position, the constraint is genuine coordination: aligning climate science with policy to prevent catastrophic damages is a coordination success that rational actors should endorse. The engine computes both: the payer seat likely classifies as snare or tangled_rope (concentrated extraction, active enforcement, excluded alternatives); the agenda-setter seat likely classifies as rope (coordination around a legitimate scientific-policy alignment). This divergence is not a flaw but the measurement the corpus exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations occupy the full-beneficiary end (d → 0.0): they receive damage prevention with no agency or resistance cost imposed on them directly — the constraint is structured to benefit them. Fossil fuel industries occupy the full-target end (d → 1.0): they bear concentrated stranded-asset losses and regulatory exclusion; their exit is constrained by capital lock-in and no alternative remains within their current business model. Carbon-intensive workers occupy the high-target end (d → 0.85–0.9): they bear employment displacement and must exit their communities or accept downward mobility; their exit is identity-locked, making effective d very high. Renewable-technology sectors sit near the beneficiary end (d → 0.2–0.3): they capture investment flows and growth markets but do not run the constraint and can arbitrage across jurisdictions. Climate-vulnerable nations sit near symmetric (d → 0.4–0.6): they are primary advocates for the constraint (beneficiary alignment) but also bear some costs (energy transition investment, adaptation spending); their power is organized but not institutional, moderating their effective benefit. Present-generation consumers in high-income nations sit near the target end (d → 0.7–0.8): consumption is constrained by carbon limits, and they bear deferred gratification; their power is diffuse but moderate, and exit is constrained by geography.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (anthropogenic climate change is real and requires deep emissions reductions) is live and corroborated outside the benefiting parties — climate science consensus, island-state coalitions, and scientific institutions all attest it. The constraint's mandate is NOT suffering mandatrophy in the traditional sense (purpose becoming obsolete). Instead, the omega-class tension is whether the growth-compatibility assumption — the constraint's core differentiator from adaptation_priority and degrowth_reading — remains credible. If growth-compatible decarbonization proves technologically or economically infeasible, the constraint's internal logic breaks (the founding problem persists but the mandated solution is impossible), which is a different failure mode than mandatrophy: the mandate persists but becomes operationally incoherent. This is captured in omega_1 (growth_compatibility_feasibility) and is the key test for whether this reading remains structurally viable or devolves into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_feasibility,
    'Is growth-compatible deep decarbonization physically and economically achievable, or does the constraint''s core promise require a contradiction between thermodynamic/resource limits and perpetual growth?',
    'Empirical tracking: does global CO2 intensity per unit GDP fall fast enough to reach net-zero by 2050 while GDP grows 2%+ annually? Does energy demand decouple from GDP growth in historical data? What is the empirical decoupling trajectory needed vs. achieved?',
    'If growth-compatible decarbonization proves impossible (decoupling is too slow or reverses), the constraint''s classification shifts from rope (genuine coordination around a feasible solution) toward snare (the growth-compatibility promise covers an infeasible mandate). This is the central omega for this reading''s viability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_compatibility_feasibility, empirical, 'Whether growth-compatible emissions reduction is structurally achievable.').

omega_variable(
    future_generation_consent,
    'Future generations are the primary beneficiaries (damage prevention) but the primary uncompensated cost-bearers in one respect: present-generation consumption is curtailed to their benefit. Is this an intergenerational-equity coordination or an extraction from the future via a choice made for them?',
    'Normative/philosophical: does the constraint''s framing of future benefit as justifying present costs answer the consent question, or does it beg it? Different ethical frameworks (utilitarian damage minimization vs. rights-based future autonomy) resolve this differently.',
    'If the constraint is reframed as extracting from future generations (present decisions made without their consent, for their purported benefit), the victim set changes and the classification could shift. This is structurally a conceptual omega about intergenerational agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_consent, conceptual, 'Whether future generations are beneficiaries or uncompensated cost-bearers.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) primarily structural (regulatory barriers, financial disincentives) or internalized (populations accept the constraint as legitimate and self-restrict consumption)?',
    'Post-enforcement dynamics: if carbon constraints are relaxed or removed, do populations'' consumption patterns revert immediately (structural suppression), or do they persist (internalized norm acceptance)? Survey data on constraint legitimacy and compliance motivation.',
    'If suppression is internalized, the constraint''s stability is higher than the structural measure suggests, and it is more rope-like (participants coordinate on energy reduction as a norm). If suppression is structural, it approaches snare-like (persistence depends on active enforcement, not consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized norm.').

omega_variable(
    kernel_reading_stability,
    'What empirical facts would make the ''adaptation_priority'' or ''degrowth_reading'' framings inescapable instead of contestable alternatives?',
    'Empirical: if cost curves for adaptation prove far cheaper than mitigation (shifting cost-benefit calculus), mitigation_priority loses urgency. If emissions reduction lags target trajectories, degrowth_reading gains structural plausibility. If climate damages accelerate faster than prevented-damage benefits materialize, the foundational problem shifts.',
    'This reading''s viability depends on empirical facts staying within its framing boundaries. If those facts shift (adaptation becomes more cost-effective, decarbonization stalls, damages accelerate), the kernel contest becomes a different engineering problem and this reading''s classification could flip from rope/tangled_rope toward piton (inert, persisted by narrative only) or toward being dominated by alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, empirical, 'Boundary conditions for reading viability within the kernel contest.').

omega_variable(
    technological_transition_pace_credibility,
    'Does the constraint assume renewable energy and efficiency transition speeds that historical technology adoption rates can sustain, or does it depend on a discontinuous acceleration?',
    'Empirical comparison: solar/wind deployment rates vs. historical S-curve patterns for technology adoption; grid infrastructure retrofitting timelines; battery chemistry breakthroughs required vs. demonstrated. Cross-check against International Energy Agency Net Zero Roadmap assumptions and actual deployment gaps.',
    'If transition speeds are infeasible within the assumed growth framework, the constraint''s core logic (growth + decarbonization) becomes engineering-impossible, not just politically hard. This would flip the classification toward snare (the promise becomes a cover story) and strengthen the degrowth_reading alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_transition_pace_credibility, empirical, 'Credibility of assumed technological transition pace.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.08).
narrative_ontology:measurement_basis(clim_tr_t1990, observed).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(clim_tr_t2005, observed).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__mitigation_priority, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__mitigation_priority, theater_ratio, 2035, 0.43).
narrative_ontology:measurement_basis(clim_tr_t2035, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.41).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(clim_be_t1990, observed).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement_basis(clim_be_t2005, observed).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__mitigation_priority, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(clim_be_t2025, observed).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__mitigation_priority, base_extractiveness, 2035, 0.72).
narrative_ontology:measurement_basis(clim_be_t2035, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.68).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement_basis(clim_su_t1990, observed).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement_basis(clim_su_t2005, observed).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__mitigation_priority, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement_basis(clim_su_t2025, observed).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__mitigation_priority, suppression_requirement, 2035, 0.58).
narrative_ontology:measurement_basis(clim_su_t2035, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.52).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.22).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% climate_harm_prevention is a contested kernel with three structural readings. The mitigation_priority reading (this file) assumes growth-compatible decarbonization is feasible and prioritizes emissions reduction. The adaptation_priority reading (sibling) assumes mitigation is politically infeasible and prioritizes near-term resilience. The degrowth_reading (sibling) argues growth-compatible mitigation is physically impossible and mandates economic contraction. Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different feasibility axioms. They coexist as live policy positions held by different parties; no single framework forecloses the others, though each influences the others' structural conditions. Network links enable cross-reading analysis of how empirical drift in one reading's assumptions would shift the kernel contest toward another reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
