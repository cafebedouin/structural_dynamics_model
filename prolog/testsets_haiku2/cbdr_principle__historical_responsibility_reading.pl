% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Reading: Binding Emissions Reductions + Loss/Damage Finance
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The Common But Differentiated Responsibilities (CBDR) principle, as
 *   instantiated through the historical-responsibility reading, allocates
 *   binding emissions-reduction obligations to developed nations proportional
 *   to their cumulative historical greenhouse-gas emissions since
 *   industrialization, plus mandatory loss-and-damage financing for
 *   vulnerable nations. This reading treats atmospheric carbon as a stock
 *   externality: developed nations industrialized via fossil-fuel-driven
 *   accumulation; their wealth is in part a function of that externality; and
 *   their obligation to reduce emissions and finance adaptation flows from
 *   that historical responsibility. This is ONE reading of the CBDR kernel;
 *   the voluntary-commitment reading interprets CBDR differently, emphasizing
 *   nationally-determined contributions and technology transfer over binding
 *   historical allocations. The historical-responsibility reading is
 *   increasingly dominant in vulnerable-nation advocacy and UNFCCC COP
 *   discourse, though developed nations resist the normative implication of
 *   retrospective liability.
 *
 * KEY AGENTS:
 *   - developed_nations: payer (institutional power, generational time horizon, constrained exit) — bear binding reduction and finance obligations
 *   - least_developed_countries + vulnerable island states: primary beneficiaries (powerless, trapped exit) — receive binding commitments and loss/damage financing
 *   - fossil_fuel_economies: secondary payers (organized power, biographical horizon) — carry reduction obligations and transition costs
 *   - UNFCCC_secretariat: agenda-setter (institutional power) — administers the reading, calculates responsibility metrics, monitors compliance
 *   - transition_finance_intermediaries: agenda-setter + beneficiary (institutional power, arbitrage exit) — mediate transfers, extract administrative rents
 *   - domestic_climate_constituencies: excluded (moderate power, constrained exit) — bear concentrated decarbonization costs but are absent from CBDR negotiation
 *   - climate_research_community: observer (analytical power) — provides epistemic infrastructure for emissions accounting and attribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.52).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Reading: Binding Emissions Reductions + Loss/Damage Finance").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'c83a4f9e-45db-4c6c-b590-f9d1750e63d7').
narrative_ontology:cs_kernel_codification('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', formalized).
narrative_ontology:cs_authority_grounding('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', lineage).
narrative_ontology:cs_interpretation_layer_present('c83a4f9e-45db-4c6c-b590-f9d1750e63d7').
narrative_ontology:cs_reading_relation('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', foundational, cumulative_emissions_impute_remedial_obligation).
narrative_ontology:cs_axiom_status(cumulative_emissions_impute_remedial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', cumulative_emissions_impute_remedial_obligation, deontological).
narrative_ontology:cs_axiom('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', foundational, binding_reduction_commitments_required).
narrative_ontology:cs_axiom_status(binding_reduction_commitments_required, holdable).
narrative_ontology:cs_axiom_grounding('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', binding_reduction_commitments_required, instrumental).
narrative_ontology:cs_reference_frame('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', atmospheric_commons_shared_responsibility).
narrative_ontology:cs_drift_state('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', contemporary_cop28_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c83a4f9e-45db-4c6c-b590-f9d1750e63d7', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, vulnerable_island_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_displaced_populations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, transition_finance_intermediaries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, future_generations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, vulnerable_island_states).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, carbon_border_adjustment_economies).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, historical_responsibility_doctrine).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, common_but_differentiated_responsibilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, developed nations bear binding, proportional emissions reductions indexed to their cumulative historical emissions since industrialization, plus mandatory loss/damage financing for vulnerable nations. They pay through domestic decarbonization costs, technology transfer, and direct adaptation transfers. Exit options are constrained by treaty obligation and reputational cost; sovereignty remains but delegation to UNFCCC accountability mechanisms limits policy discretion. The reading imputes responsibility for externalities generated across 150+ years of industrial activity.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations, payer,
    institutional, generational, constrained, global).

% Receive binding commitments for emissions reductions from high-emitters plus dedicated loss/damage financing to cover adaptation costs and climate-induced displacement. They are the primary beneficiary of the reading's transfer mechanism. They also bear diffuse costs: adaptation financing is structured as a finite pool relative to estimated damages, and the reading does not eliminate their own climate vulnerability or development tradeoffs. Their exit is trapped—they cannot renegotiate the basic physics of climate change or their geographic exposure.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, least_developed_countries, payer).

% Face existential threat from sea-level rise driven by accumulated developed-nation emissions. The reading grants them standing to claim loss/damage financing and binding reduction commitments. They remain trapped in geographic exposure and dependent on developed-nation compliance; they also face the secondary burden of adaptation costs that outpace available financing even under this reading's commitments.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, vulnerable_island_states, beneficiary,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, vulnerable_island_states, payer).

% Developed or middle-income economies whose comparative advantage and revenue base depend on fossil fuel extraction and export. Under this reading they are structured as payers via binding reduction commitments and finance obligations. Their exit options are constrained by treaty, market pressure for decarbonization, and the finality of historical responsibility allocations. They carry both the transition cost and the assignment of historical liability.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_economies, payer,
    organized, biographical, constrained, regional).

% Populations displaced by climate impacts (sea-level rise, desertification, flooding) driven by developed-nation emissions. The reading allocates them standing to receive loss/damage finance and relocation support. They are trapped by geography and economic constraints; they have no exit but benefit from the reading's allocation of responsibility and transfer mechanism, though the actual funding is typically insufficient.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_displaced_populations, beneficiary,
    powerless, immediate, trapped, global).

% Administers the CBDR principle, negotiates and monitors compliance, adjudicates disputes about historical responsibility calculations and transfer adequacy. Sets the agenda for interpreting the reading through COPs and subsidiary bodies. Maintains the technical infrastructure for emissions accounting and finance tracking.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Development banks, climate funds, and bilateral aid agencies that mediate the transfer. They benefit from being the conduit (institutional legitimacy, operational budgets, technical control), set terms for recipient access, and hold agency in how abstract financial commitments translate to actual deployment. They extract administrative rents and shape implementation narratives.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, transition_finance_intermediaries, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, transition_finance_intermediaries, beneficiary).

% Working-class and middle-income communities in developed nations who bear concentrated costs of rapid decarbonization (job loss in fossil fuel sectors, energy price inflation, industrial transition disruption) but are excluded from the primary CBDR negotiation. Their absence from the decision table allows developed-nation elites to commit to transfers without domestic political friction being surfaced in the treaty.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, domestic_climate_constituencies, excluded,
    moderate, biographical, constrained, national).

% Inherit either the climate-stabilized world that binding reductions enable, or the cascading damages from continued high-emission trajectories. They are voiceless in current negotiations and structurally trapped. The reading allocates them benefit (stabilization via binding reductions) but provides no direct agency or voice.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Developed economies pursuing unilateral carbon border adjustment mechanisms (CBAM) as enforcement proxy. They carry the cost of implementing carbon pricing at borders, face retaliation risk, and claim to be payers under CBDR via these mechanisms. Their exit is relatively mobile (they can weaken CBAM enforcement); they function as enforcement agents for the reading's regime but extract rents through border mechanisms and face political pressure from export-oriented industries.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, carbon_border_adjustment_economies, payer,
    powerful, biographical, mobile, regional).

% Produces the emissions accounting, climate impact models, and responsibility metrics that make the reading's historical-responsibility calculations possible. They are analytical observers, neither collecting from nor paying into the constraint directly, but providing the epistemic infrastructure that grounds the reading's truth claims about cumulative emissions and attribution.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, transition_finance_intermediaries).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for global emissions reduction and climate adaptation financing based on historical contribution to atmospheric carbon stock. Solves the collective-action problem of how to assign burdens when the harms are global, cumulative, and lagged, and when development capacity is unequally distributed. The reading specifically coordinates around the principle that nations responsible for the atmospheric excess should bear the cost of restoring the stable state.
% TRANSFER_FUNCTION: Transfers financial resources and technology from developed nations (high cumulative emissions) to vulnerable nations (climate-impact victims with low historical responsibility). Transfers domestic policy constraint (binding emissions reductions) from developed nations to developing nations via treaty obligation. Transfers adaptive capacity by allocating loss/damage financing pools. The net flow is North-to-South in finance; South-to-North in terms of the externalities already embedded in developed-nation wealth stocks.
% ABSENT_VOICES: Domestic working-class constituencies in developed nations (who bear concentrated transition costs) are excluded from CBDR negotiation; future generations of all nations are voiceless; subnational climate-vulnerable populations within middle-income emitters (who face reduction obligations on behalf of their nation's historical emissions) are diffusely represented. Their absence allows developed-nation elites and developing-nation diplomatic representatives to settle allocations without surfacing the domestic political conflicts decarbonization triggers.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—if the binding historical-responsibility allocation vanished and CBDR reverted to purely voluntary commitments—developed nations would externalize adaptation costs, atmospheric carbon would accumulate faster (no binding reduction teeth), and vulnerable nations would face unmitigated climate impacts without compensatory transfers. The global development trajectory and climate outcome would reorganize sharply: inequality would widen, adaptation financing would collapse, and emissions-reduction incentives would weaken.
% FOUNDING_PROBLEM: The atmospheric commons was degraded by a century of industrial emissions concentrated in the Global North. Developing nations face climate damages they did not cause. No mechanism existed to allocate responsibility for the stock problem or to finance adaptation in low-emission, high-vulnerability economies. The founding problem is the absence of a principle connecting historical emissions to present obligations.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and IPCC reports corroborate that atmospheric carbon is stock-accumulated and that the Global North accounts for ~80% of cumulative emissions. Small island states and least-developed-country negotiators corroborate that they face climate damages disproportionate to their historical emissions. Developed-nation negotiators and fossil-fuel-export economies contest whether historical responsibility should override current capacity and national sovereignty; they argue that forward-looking commitments (not retrospective liability) should govern. The founding problem is universally acknowledged; its normative weight and policy implication are contested.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the reading allocates binding, long-term financial and emissions-reduction obligations to developed nations without corresponding compensation, indexed to historical externalities they did not formally consent to remediate. The series shows extraction rising as the reading moves from rhetorical principle (1992, Rio) through implementation architecture (2015, Paris) to concrete fund operationalization (2022, COP27 loss/damage fund). Theater is moderate and rising (0.41 at interval end): early CBDR was mostly aspirational rhetoric; as mechanisms hardened, performative elements emerged — developed nations negotiate carbon-border mechanisms as enforcement proxies while maintaining policy discretion over domestic reduction pathways; intermediaries stage elaborate climate-finance conferences while actual disbursement lags commitments. Suppression is measured as the degree to which exit alternatives are eliminated: developed nations face reputational and legal pressure to comply, but sovereignty formally remains; they can (and do) weaken implementation through voluntary NDC revision and finance underfunding. The coercion grid shows that suppression operates primarily at the organizational and individual levels (domestic constituencies and subnational actors in developed nations bear the costs without formal voice); structural-level suppression is lighter (national governments retain policy discretion). Resistance is substantial and rising (0.71 at interval end): fossil-fuel economies, developed-nation industry associations, and excluded working-class constituencies actively resist the reading through lobbying, CBAM countermeasures, and domestic political mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The developed-nation seat perceives retrospective liability and externalized obligation; the vulnerable-nation seat perceives corrective justice and overdue recompense; the intermediary seat perceives operational opportunity; the excluded domestic seat in developed nations perceives imposed sacrifice without voice. From the developed-nation negotiating perspective, the binding allocation of historical responsibility for centuries of industrial activity violates national sovereignty and retroactively condemns prior legal commercial activity; from the vulnerable-nation perspective, sovereignty claims are cover for climate-imperialism — the wealthy nations industrialized on the back of atmospheric dumping and now claim sovereign right to escape the cost. These are not bridgeable from a single normative seat; they are different constraint-classifications of the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations carry high directionality toward the constraint (d ~0.85): they are structural targets of an allocation mechanism they did not negotiate freely. Their exit is constrained (treaty obligation, reputational cost, market pressure); they cannot unilaterally exit CBDR without defecting from the climate treaty regime. Least-developed nations carry low directionality (d ~0.15): they are beneficiaries of the reading's allocation; the constraint subsidizes them (though insufficiently). Vulnerable island states carry very low d (~0.10) on the allocation dimension but moderate d (~0.45) when accounting for the inadequacy of actual transfers relative to claimed benefit — they are nominally beneficiaries but structurally underfunded, so the effective directionality is higher than the role would suggest. Fossil-fuel economies carry moderate-to-high d (~0.65): they are formally payers but retain policy discretion over how quickly to transition. The coercion grid reveals that suppression (the elimination of exit) is highest at the individual and class levels (domestic constituencies in developed nations cannot exit decarbonization costs) and lowest at the structural level (national governments retain formal policy autonomy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atmospheric commons degradation, uncompensated climate damages in low-emission economies) remains live and pressing — there is no sense in which the CBDR reading has solved its mandate and outlived it. However, there is a significant mandatrophy signal: the reading's coordination function (allocating global emissions-reduction burden fairly) has become entangled with an extraction function (developed nations pay transfers without corresponding benefit or renegotiation). The rising theater_ratio (0.05 in 1992 → 0.41 in 2022) indicates that performative maintenance is replacing actual mechanism: developed nations negotiate carbon-border adjustments, stage climate conferences, and announce pledges while maintaining low implementation rates; vulnerable nations perform acceptance of inadequate financing while facing real adaptation gaps. The measurement series suggests the reading has drifted from a coordination mechanism (allocation of fair burden) toward a Tangled Rope (real coordination side-by-side with extraction side, sustained by enforcement pressure). The divergence between disappearance_verdict (world_rearranges) and founding_problem_status (contested) is itself mandatrophy signal: if the problem is contested, why does the world rearrange if the solution disappears? Answer: because the reading has become institutionally entrenched and politically salient independent of consensus on its problem definition — removing it would force renegotiation of global emissions architecture, not because there is agreement that it solves the problem, but because powerful institutional and vulnerable-nation coalitions are invested in maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cumulative_responsibility_attribution,
    'What is the correct attribution of historical emissions responsibility across developed nations? Should responsibility be indexed to cumulative emissions since 1750 (pre-industrial period) or since 1990 (treaty baseline)? Should it include only direct national emissions or also embodied emissions in traded goods?',
    'IPCC Working Group III emissions-inventory methodology; post-hoc attribution analysis from high-resolution historical climate-model runs; accounting-standards negotiations within UNFCCC.',
    'Different attribution baselines shift which developed nations carry the highest responsibility (US vs. UK vs. Germany), how much total transfer is owed, and whether nations that industrialized recently (China, India if counted as developed) can claim lower responsibility. A narrow baseline favors recent developers; a wide baseline distributes burden toward historical leaders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_responsibility_attribution, empirical, 'The temporal and scope boundary for cumulative emissions attribution').

omega_variable(
    historical_responsibility_foreclosure,
    'Does the historical-responsibility reading logically foreclose the voluntary-commitment reading within a single nation''s decision framework, or can both readings coexist as competing strategic positions?',
    'Normative analysis of the foundational axioms: if historical responsibility is held as deontological (a duty grounded in past wrongs), does it logically rule out a purely forward-looking voluntary framework? If both are held as instrumental (both are means to emissions reduction), can they coexist?',
    'If the readings foreclose each other, one must eventually dominate UNFCCC consensus; if they coexist, CBDR remains permanently contested and implementation remains incoherent. The mismatch between the two readings is currently a feature of North-South negotiation friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_responsibility_foreclosure, conceptual, 'Whether the historical-responsibility and voluntary-commitment readings are logically incompatible or merely strategically opposed').

omega_variable(
    loss_and_damage_financing_adequacy,
    'What is the adequate level of loss-and-damage financing? Vulnerable nations claim $100B+/year; developed nations have committed ~$10B/year (as of 2023). Is the gap a shortfall in implementation or a disagreement on what constitutes ''adequate''?',
    'Cost-of-climate-adaptation studies from IPCC and UNCTAD; post-hoc measurement of actual adaptation outcomes in recipient nations; explicit renegotiation of financing targets.',
    'If the gap is a shortfall, the reading is being underfunded and vulnerable nations are bearing residual climate risk despite the reading''s allocation mechanism. If the gap reflects disagreement on adequacy, the reading itself is contested on what it requires.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_and_damage_financing_adequacy, empirical, 'Whether loss-and-damage financing commitments meet the adaptation needs of vulnerable nations').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of exit alternatives structural (treaty obligation, legal penalties, trade pressure) or internalized (developing nations believe they deserve the allocation, developed nations believe they bear responsibility)? If internalized, does removing the formal mechanism change behavior?',
    'Post-exit-mechanism experiments: if developed nations formally withdrew from CBDR and removed legal penalties, would they maintain emissions reductions and financing? Polling of developed-nation elites on whether they believe historical responsibility is normatively binding.',
    'If suppression is primarily structural, the reading is brittle — removing formal mechanisms allows exit. If internalized, the reading has captured normative commitment and persists even without enforcement infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the constraint''s enforcement is structural coercion or internalized normative commitment').

omega_variable(
    excluded_domestic_constituencies_latent_veto,
    'Can developed nations maintain compliance with the historical-responsibility reading if domestic working-class constituencies (coal miners, auto workers, fossil-dependent communities) mount sufficient political resistance? Or is their exclusion stable because their interests are structurally submerged in national-diplomatic processes?',
    'Historical analysis of coal-industry lobby power, election outcomes in fossil-dependent regions, success rate of working-class coalition building against climate policy; counterfactual: what if excluded constituencies were given voice in CBDR negotiations?',
    'If excluded constituencies can mount a latent veto (e.g., through electoral pressure), the reading''s stability is conditional on remaining exclusion. If their interests are permanently submerged, the reading is stable. The mechanism of their exclusion (diplomatic institutions, national sovereignty claims, elite coalition capture) is itself the subject.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_domestic_constituencies_latent_veto, empirical, 'Whether domestically-excluded constituencies retain latent power to veto the reading''s implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hist_resp_theater_1992_rio, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement_basis(cbdr_hist_resp_theater_1992_rio, observed).
narrative_ontology:measurement(cbdr_hist_resp_theater_2005_kyoto, cbdr_principle__historical_responsibility_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(cbdr_hist_resp_theater_2005_kyoto, observed).
narrative_ontology:measurement(cbdr_hist_resp_theater_2015_paris_ndc, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(cbdr_hist_resp_theater_2015_paris_ndc, observed).
narrative_ontology:measurement(cbdr_hist_resp_theater_2022_loss_damage, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.41).
narrative_ontology:measurement_basis(cbdr_hist_resp_theater_2022_loss_damage, observed).
narrative_ontology:measurement(cbdr_hist_resp_theater_2030_cop28, cbdr_principle__historical_responsibility_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement_basis(cbdr_hist_resp_theater_2030_cop28, projected).

% Extraction over time
narrative_ontology:measurement(cbdr_hist_resp_extractiveness_1992_rio, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement_basis(cbdr_hist_resp_extractiveness_1992_rio, observed).
narrative_ontology:measurement(cbdr_hist_resp_extractiveness_2005_kyoto_post, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement_basis(cbdr_hist_resp_extractiveness_2005_kyoto_post, observed).
narrative_ontology:measurement(cbdr_hist_resp_extractiveness_2015_paris, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(cbdr_hist_resp_extractiveness_2015_paris, observed).
narrative_ontology:measurement(cbdr_hist_resp_extractiveness_2022_loss_damage_fund, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement_basis(cbdr_hist_resp_extractiveness_2022_loss_damage_fund, observed).
narrative_ontology:measurement(cbdr_hist_resp_extractiveness_2030_cop28_trajectory, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement_basis(cbdr_hist_resp_extractiveness_2030_cop28_trajectory, projected).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hist_resp_suppression_1992_rio, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement_basis(cbdr_hist_resp_suppression_1992_rio, observed).
narrative_ontology:measurement(cbdr_hist_resp_suppression_2005_kyoto_enforcement, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement_basis(cbdr_hist_resp_suppression_2005_kyoto_enforcement, observed).
narrative_ontology:measurement(cbdr_hist_resp_suppression_2015_paris_paris, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(cbdr_hist_resp_suppression_2015_paris_paris, observed).
narrative_ontology:measurement(cbdr_hist_resp_suppression_2022_loss_damage_negotiations, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(cbdr_hist_resp_suppression_2022_loss_damage_negotiations, observed).
narrative_ontology:measurement(cbdr_hist_resp_suppression_2030_cop28_trajectory, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2030, 0.52).
narrative_ontology:measurement_basis(cbdr_hist_resp_suppression_2030_cop28_trajectory, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1992, tn=2030
narrative_ontology:measurement(cbdr_hist_resp_grid_01, cbdr_principle__historical_responsibility_reading, accessibility_collapse(class), 1992, 0.15).
narrative_ontology:measurement(cbdr_hist_resp_grid_02, cbdr_principle__historical_responsibility_reading, accessibility_collapse(class), 2030, 0.55).
narrative_ontology:measurement(cbdr_hist_resp_grid_03, cbdr_principle__historical_responsibility_reading, accessibility_collapse(individual), 1992, 0.1).
narrative_ontology:measurement(cbdr_hist_resp_grid_04, cbdr_principle__historical_responsibility_reading, accessibility_collapse(individual), 2030, 0.48).
narrative_ontology:measurement(cbdr_hist_resp_grid_05, cbdr_principle__historical_responsibility_reading, accessibility_collapse(organizational), 1992, 0.25).
narrative_ontology:measurement(cbdr_hist_resp_grid_06, cbdr_principle__historical_responsibility_reading, accessibility_collapse(organizational), 2030, 0.58).
narrative_ontology:measurement(cbdr_hist_resp_grid_07, cbdr_principle__historical_responsibility_reading, accessibility_collapse(structural), 1992, 0.35).
narrative_ontology:measurement(cbdr_hist_resp_grid_08, cbdr_principle__historical_responsibility_reading, accessibility_collapse(structural), 2030, 0.62).
narrative_ontology:measurement(cbdr_hist_resp_grid_09, cbdr_principle__historical_responsibility_reading, resistance(class), 1992, 0.48).
narrative_ontology:measurement(cbdr_hist_resp_grid_10, cbdr_principle__historical_responsibility_reading, resistance(class), 2030, 0.74).
narrative_ontology:measurement(cbdr_hist_resp_grid_11, cbdr_principle__historical_responsibility_reading, resistance(individual), 1992, 0.52).
narrative_ontology:measurement(cbdr_hist_resp_grid_12, cbdr_principle__historical_responsibility_reading, resistance(individual), 2030, 0.75).
narrative_ontology:measurement(cbdr_hist_resp_grid_13, cbdr_principle__historical_responsibility_reading, resistance(organizational), 1992, 0.42).
narrative_ontology:measurement(cbdr_hist_resp_grid_14, cbdr_principle__historical_responsibility_reading, resistance(organizational), 2030, 0.71).
narrative_ontology:measurement(cbdr_hist_resp_grid_15, cbdr_principle__historical_responsibility_reading, resistance(structural), 1992, 0.32).
narrative_ontology:measurement(cbdr_hist_resp_grid_16, cbdr_principle__historical_responsibility_reading, resistance(structural), 2030, 0.65).
narrative_ontology:measurement(cbdr_hist_resp_grid_17, cbdr_principle__historical_responsibility_reading, stakes_inflation(class), 1992, 0.12).
narrative_ontology:measurement(cbdr_hist_resp_grid_18, cbdr_principle__historical_responsibility_reading, stakes_inflation(class), 2030, 0.65).
narrative_ontology:measurement(cbdr_hist_resp_grid_19, cbdr_principle__historical_responsibility_reading, stakes_inflation(individual), 1992, 0.08).
narrative_ontology:measurement(cbdr_hist_resp_grid_20, cbdr_principle__historical_responsibility_reading, stakes_inflation(individual), 2030, 0.58).
narrative_ontology:measurement(cbdr_hist_resp_grid_21, cbdr_principle__historical_responsibility_reading, stakes_inflation(organizational), 1992, 0.15).
narrative_ontology:measurement(cbdr_hist_resp_grid_22, cbdr_principle__historical_responsibility_reading, stakes_inflation(organizational), 2030, 0.68).
narrative_ontology:measurement(cbdr_hist_resp_grid_23, cbdr_principle__historical_responsibility_reading, stakes_inflation(structural), 1992, 0.2).
narrative_ontology:measurement(cbdr_hist_resp_grid_24, cbdr_principle__historical_responsibility_reading, stakes_inflation(structural), 2030, 0.71).
narrative_ontology:measurement(cbdr_hist_resp_grid_25, cbdr_principle__historical_responsibility_reading, suppression(class), 1992, 0.22).
narrative_ontology:measurement(cbdr_hist_resp_grid_26, cbdr_principle__historical_responsibility_reading, suppression(class), 2030, 0.55).
narrative_ontology:measurement(cbdr_hist_resp_grid_27, cbdr_principle__historical_responsibility_reading, suppression(individual), 1992, 0.28).
narrative_ontology:measurement(cbdr_hist_resp_grid_28, cbdr_principle__historical_responsibility_reading, suppression(individual), 2030, 0.58).
narrative_ontology:measurement(cbdr_hist_resp_grid_29, cbdr_principle__historical_responsibility_reading, suppression(organizational), 1992, 0.18).
narrative_ontology:measurement(cbdr_hist_resp_grid_30, cbdr_principle__historical_responsibility_reading, suppression(organizational), 2030, 0.52).
narrative_ontology:measurement(cbdr_hist_resp_grid_31, cbdr_principle__historical_responsibility_reading, suppression(structural), 1992, 0.15).
narrative_ontology:measurement(cbdr_hist_resp_grid_32, cbdr_principle__historical_responsibility_reading, suppression(structural), 2030, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.22).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_framework).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, carbon_border_adjustment_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_operationalization).

% DUAL FORMULATION NOTE:
% The CBDR principle instantiates two structurally distinct constraints depending on which reading is adopted: (1) historical_responsibility_reading — binding, retroactively-indexed emissions reductions and loss/damage financing (this file); (2) voluntary_commitment_reading — voluntary nationally-determined contributions with technology transfer as primary developed-nation obligation. These readings have different ε values, different beneficiary/victim structures, and different sustainability dynamics. They coexist in UNFCCC discourse and represent competing interpretations of the same kernel commitment. The historical-responsibility reading creates structural pressure on the voluntary-commitment reading by raising normative stakes and enabling vulnerable-nation coalition building; the voluntary-commitment reading creates counter-pressure by allowing policy discretion and reducing developed-nation exit costs. Both readings remain live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
