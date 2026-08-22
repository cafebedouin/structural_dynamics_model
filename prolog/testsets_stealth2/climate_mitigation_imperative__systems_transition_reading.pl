% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Systems-Transition Gate on Climate Mitigation (Democratic-Decentralization Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   A contested governance constraint inside climate politics: the
 *   requirement that legitimate mitigation run through transformation of
 *   energy systems toward decentralized, democratically controlled ownership,
 *   with nuclear power classified as perpetuating extractive centralization
 *   and therefore excluded from the legitimate mitigation set. This file
 *   instantiates ONE reading (systems_transition_reading) of the kernel
 *   climate_mitigation_imperative. The ε referent is fixed to the operative
 *   governance structure this reading maintains — the
 *   discursive-institutional apparatus that allocates legitimacy, funding,
 *   and platform space among mitigation pathways — not to the incumbent
 *   centralized order the reading condemns (a separate constraint with its
 *   own story) and not to the decentralized commons the reading endorses. The
 *   claim/metric gap is deliberate: the reading CLAIMS liberation while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation — the engine measures that divergence; the claim is not
 *   reconciled to the metrics. KEY AGENTS (by structural relationship): -
 *   climate_philanthropy_and_ngo_funders: Agenda setter
 *   (institutional/arbitrage) — administers the legitimacy frame via grant
 *   criteria and 'real solutions' lists -
 *   distributed_renewable_energy_sector: Primary material beneficiary
 *   (organized/mobile) — captures redirected capital and a moral franchise -
 *   energy_democracy_movement_orgs: Identity beneficiary
 *   (moderate/identity_locked) — the frame constitutes their funding and
 *   self-concept - community_energy_cooperatives: Localized beneficiary
 *   (moderate/constrained) — genuine local value under the policy envelope -
 *   nuclear_industry_and_workforce: Primary payer (organized/constrained) —
 *   taxonomy exclusion, financing penalty, stalled life-extension -
 *   nuclear_research_community: Secondary payer (moderate/identity_locked) —
 *   contracting grant lines and venue legitimacy - electricity_ratepayers:
 *   Diffuse payer (powerless/trapped) — bears system-cost tail risk with no
 *   seat - technology_inclusive_advocates: Excluded voice (moderate/mobile) —
 *   objects from outside the governed room -
 *   ipcc_scenario_modeling_community: Analytical observer
 *   (institutional/analytical) — documents cost penalties of exclusion -
 *   transmission_grid_operators: Operational observer
 *   (institutional/constrained) — absorbs reliability consequences
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.66).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Systems-Transition Gate on Climate Mitigation (Democratic-Decentralization Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '398440a6-902f-4f84-b912-24ac896968f9').
narrative_ontology:cs_kernel_codification('398440a6-902f-4f84-b912-24ac896968f9', distributed).
narrative_ontology:cs_authority_grounding('398440a6-902f-4f84-b912-24ac896968f9', practice).
narrative_ontology:cs_interpretation_layer_present('398440a6-902f-4f84-b912-24ac896968f9').
narrative_ontology:cs_reading_relation('398440a6-902f-4f84-b912-24ac896968f9', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('398440a6-902f-4f84-b912-24ac896968f9', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('398440a6-902f-4f84-b912-24ac896968f9', foundational, legitimate_mitigation_requires_democratized_control).
narrative_ontology:cs_axiom_status(legitimate_mitigation_requires_democratized_control, holdable).
narrative_ontology:cs_axiom_grounding('398440a6-902f-4f84-b912-24ac896968f9', legitimate_mitigation_requires_democratized_control, deontological).
narrative_ontology:cs_axiom('398440a6-902f-4f84-b912-24ac896968f9', foundational, nuclear_structurally_perpetuates_extractive_centralization).
narrative_ontology:cs_axiom_status(nuclear_structurally_perpetuates_extractive_centralization, holdable).
narrative_ontology:cs_axiom_grounding('398440a6-902f-4f84-b912-24ac896968f9', nuclear_structurally_perpetuates_extractive_centralization, empirically_contingent).
narrative_ontology:cs_reference_frame('398440a6-902f-4f84-b912-24ac896968f9', democratic_decentralized_energy_commons).
narrative_ontology:cs_drift_state('398440a6-902f-4f84-b912-24ac896968f9', contemporary_net_zero_buildout_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('398440a6-902f-4f84-b912-24ac896968f9', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movement_orgs).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_and_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_research_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, energy_democracy_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, systemic_change_over_consumer_choice_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major climate foundations and NGO confederations. They write the grant criteria, publish the 'real solutions' lists, and convene the coalitions through which the frame is administered. Widening the frame to technology-inclusive portfolios is within their unilateral power, but would rupture donor bases and member coalitions. Their convening power and institutional relevance grow as the frame spreads.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_philanthropy_and_ngo_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% Solar, wind, and storage manufacturers, developers, and installers. When mitigation legitimacy runs exclusively through decentralized build-out, they capture procurement pipelines, subsidy streams, and a moral premium competitors cannot claim. They sell into whatever demand environment exists and can reprice or redirect across markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Climate justice networks, anti-nuclear legacy organizations, and movement intellectual circles. Their funding, membership rolls, and self-concept are constituted by the decentralization-plus-democratic-control frame; admitting nuclear would dissolve the coalition's distinguishing glue. Leaving the frame would mean organizational dissolution, not repositioning.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movement_orgs, beneficiary,
    moderate, generational, identity_locked, global).

% Village- and city-scale energy cooperatives producing local renewable power under feed-in tariffs and community-energy mandates. They capture genuine local value and member participation, and depend on the policy envelope the frame legitimizes. Exit means selling assets at a discount.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, generational, constrained, local).

% Reactor operators, suppliers, and skilled trades. Where the frame governs, they are excluded from clean-energy taxonomies and 'real solutions' lists; financing costs rise, life-extension and uprate projects stall, and workforce pipelines thin. Partial exit remains into defense, medical isotopes, and export markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry_and_workforce, payer,
    organized, biographical, constrained, global).

% Reactor physicists, fuels and materials researchers, and national-laboratory fission programs. Grant lines and journal and conference legitimacy contract when advanced fission is framed as perpetuating centralized power. Careers are specialized to fission; exit paths into fusion, defense, or employment abroad are costly and partial.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_research_community, payer,
    moderate, generational, identity_locked, global).

% Households and businesses connected to regional grids. Distributed build-out lowers marginal generation costs, but retirement of firm capacity without replacement can raise total system costs and price volatility; these costs arrive as bills with no attributable source. They cannot exit the grid and hold no seat in the planning bodies or coalitions that set the frame.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, electricity_ratepayers, beneficiary).

% Ecomodernist analysts, some engineers, and energy-trade unions who argue for technology-inclusive decarbonization portfolios. They publish, testify, and organize in the wider discourse, but are barred from funded coalitions and movement platforms by 'false solution' boundary lists. Their objection is registered in venues the frame does not govern.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, technology_inclusive_advocates, excluded,
    moderate, biographical, mobile, global).

% Integrated assessment and energy-system modeling groups whose published scenarios repeatedly attach cost and feasibility penalties to excluding firm low-carbon generators. They document the gap between the governance frame and model evidence in assessment reports; they set no agendas and collect no proceeds.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ipcc_scenario_modeling_community, observer,
    institutional, civilizational, analytical, global).

% Regional transmission and system operators tasked with reliability as variable generation grows. They absorb the operational consequences of firm-capacity foreclosure — ramping, reserve scarcity, curtailment management — and report constraints factually in planning filings. They have no authority over which technologies count as legitimate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, transmission_grid_operators, observer,
    institutional, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_energy_sector).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: climate mitigation requires a unifying strategic frame connecting individual action, investment, and policy to systemic change rather than consumer choice or technology-neutral tweaks. The imperative coordinates dispersed actors around one theory of change, aligns decarbonization with democratic and justice commitments, and builds coalitions durable enough to survive electoral turnover.
% TRANSFER_FUNCTION: Moves legitimacy, grant funding, media platform, and investment capital away from centralized and firm generation — above all nuclear — toward distributed renewable deployment and the movement organizations that define the frame. Aspirationally it moves decision rights from utility shareholders toward communities, though the measured transfer of actual control is smaller than the rhetoric.
% ABSENT_VOICES: Technology-inclusive climate advocates, nuclear engineers, and energy-trade unions object from outside the governed room: they are audible in general discourse but excluded from funded coalitions, 'real solutions' lists, and movement platforms. Grid-modeling expertise is consulted late or rhetorically. Electricity ratepayers — the diffuse payers of system-cost consequences — have no seat anywhere in the governance structure.
% DISAPPEARANCE_RATIONALE: If the imperative vanished overnight, climate philanthropy would re-bucket portfolios within a funding cycle, renewables sectors would compete on cost alone and lose the moral franchise, nuclear would regain taxonomy access and cheaper financing, and the movement coalition would fragment and re-form around a different frame — the governance landscape of mitigation visibly rearranges.
% FOUNDING_PROBLEM: Early climate politics treated emissions as a technical externality correctable without disturbing who owns and controls energy; fossil-centralized incumbents captured policy while proposed 'solutions' left power structures intact. The imperative was built to fuse decarbonization with democratization so mitigation could not be captured by the same concentrated interests that produced the crisis.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the political-science record of fossil-incumbent lobbying, compiled independently of the movement, corroborates that the founding capture problem was and is real; IPCC working-group assessments corroborate that institutional and distributional barriers constrain mitigation. Against the live-status reading, integrated-assessment and energy-economics literature from outside the movement attests that the nuclear-exclusion extension lacks a mitigation-cost basis and now functions as boundary maintenance — hence contested rather than live.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66) but short of pure-extraction range because the coordination content is real: the imperative solved a genuine collective-action problem — building durable, justice-aligned climate coalitions that survive electoral cycles — and its beneficiaries include actors with no extractive intent. Suppression (0.58) is a raw structural property, unscaled by power or scope: it reflects movement boundary-policing ('false solutions' doctrine), funder litmus tests, and platform gatekeeping, not physical coercion; only extractiveness is scaled, by the engine, through directionality and scope. Theater (0.31) tracks the spread of 'energy democracy' branding beyond actual devolved control — functioning cooperatives coexist with corporately owned 'community' projects. Accessibility collapse is moderate (0.45): technology-inclusive portfolios remain visible and argued in engineering and economics venues; collapse is deepest inside movement institutions. Resistance (0.55) is sustained and uncrushed. The three temporal series share one seven-point grid (T=0..24) so every metric is authored at every examined time point; trajectories are monotonic consolidation, not cyclical — the constraint ratchets as it migrates from activist fringe to funder gatekeeping, with no oscillation phase to model.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the constraint is discretionary portfolio design — a frame the setter could widen tomorrow at will. From the identity-locked beneficiary seat it is constitutive selfhood: the frame is who the organization is, not a tool it holds. From the payer seats it is enforced foreclosure experienced as bad faith — a climate movement excluding its largest proven low-carbon asset on grounds unrelated to carbon. From the trapped ratepayer seat it is an unattributable line on a bill. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the beneficiary pole: movement organizations sit nearest it (identity lock deepens their subsidized position), the renewables sector slightly less (mobile exit lets it reprice across markets), cooperatives mid-low. Victim declarations drive d toward the target pole: the research community sits nearer the full-target end than the industry (identity lock versus partial exit into defense, medicine, and export), and trapped ratepayers carry high d despite diffuse, hard-to-attribute harm. The agenda-setter seat derives ambiguously — it appears in neither the beneficiary nor the victim arrays — and no directionality override is authored: overrides key on the power atom, and the institutional atom is shared with two observer seats an override would mislabel. The ambiguity is carried here and in the omega set instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification preserves both halves of the structure. A pure-snare reading would erase the real coordination achievement — mobilizing durable climate coalitions and aligning mitigation with justice commitments — and misread net-benefiting participants as marks. A pure-rope reading would erase the enforced foreclosure of a zero-carbon option and its identifiable payers. The R5 interview arms the obsolescence tripwire honestly: founding_problem_status is 'contested', so the dead-times-world_rearranges mismatch flag will not fire spuriously today, but if the anti-capture problem dies while the frame persists, the flag routes the story to zombie detection against the computed theater path. mandatrophy_resolved is left undeclared: the mandate has not plainly outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_referent_fixation,
    'Is the ε referent correctly fixed to the operative governance structure this reading maintains, rather than to the incumbent centralized energy order the reading condemns?',
    'Cross-reading comparison: compile the sibling readings of the kernel and compare computed types; if this story''s effective-extraction profile tracks the incumbent-order story instead of the governance-structure story, the referent was mis-fixed and the file should be re-authored against the other arrangement.',
    'Mis-fixation would relocate the victim set (ratepayer and taxpayer seats instead of nuclear seats) and flip the classification toward the incumbent order''s profile; correct fixation keeps nuclear seats as targets and renewables seats as beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_fixation, conceptual, 'Fixes what this reading''s ε is about; guards against averaging across sibling readings of the shared kernel.').

omega_variable(
    discursive_recursion_of_centralization,
    'Does the imperative reproduce the centralization it condemns at the discursive level — movement gatekeepers replacing utility gatekeepers — or is its devolutionary practice different in kind?',
    'Compare actual decision-rights distribution in funded ''energy democracy'' projects against the frame''s claims; audit who sits on grant panels, coalition boards, and platform editorial positions.',
    'If the recursion is real, extraction is understated and the type drifts toward snare; if the practice is genuinely different in kind, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_recursion_of_centralization, conceptual, 'Self-application test of the reading''s own core critique of concentrated gatekeeping.').

omega_variable(
    firm_capacity_system_cost_attribution,
    'How much of the harm attributed to nuclear exclusion is real imposed system cost on ratepayers versus a modeling artifact?',
    'Jurisdictional natural experiments: grids that retired nuclear with and without firm replacement, holding renewables build-out roughly comparable, with published system-cost decompositions.',
    'If artifact-dominated, the ratepayer seat''s directionality falls and overall ε revises down; if the cost is real, the powerless trapped payer seat strengthens the extraction case considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_system_cost_attribution, empirical, 'Empirical weight of the diffuse-payer channel in the victim structure.').

omega_variable(
    movement_suppression_internalization,
    'Is the movement''s boundary-policing suppression structural (funder conditions, platform access) or internalized (self-censorship persisting after formal barriers lift)?',
    'Post-defection trajectories of organizations and researchers that broke ranks publicly: track whether professional and funding costs persisted after the formal barriers were removed.',
    'If internalized, effective suppression exceeds the structural measure, exit options are worse than authored for the identity-locked seats, and reclassification pressure toward snare increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_suppression_internalization, empirical, 'Structural versus internalized suppression split for the identity-locked seats.').

omega_variable(
    stigma_versus_economics_confound,
    'Are nuclear''s contracting fortunes caused by this constraint, or by independent cost and schedule economics that would bind regardless of the frame?',
    'Difference-in-differences across jurisdictions with varying movement influence over taxonomy and funding rules, controlling for overnight capital cost and construction timelines.',
    'If economics dominate, this constraint''s ε revises sharply downward and the payer seats are better explained by a separate economic constraint; if stigma bites independently of economics, the authored extraction stands as written.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stigma_versus_economics_confound, empirical, 'Causal attribution for the payer seats'' decline; separates the frame''s effect from market fundamentals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(clim_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(clim_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(clim_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, identity_coordination).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the climate mitigation imperative' conflates three structurally distinct claims with different ε values and different victim sets (ε-invariance decomposition into a constraint family): this story (governance-structure claim; nuclear seats in the victim set; distributed renewables as beneficiaries), climate_mitigation_imperative__portfolio_optimization_reading (inclusiveness claim; no nuclear victim; nuclear a beneficiary), and climate_mitigation_imperative__opportunity_cost_reading (cost-effectiveness claim; ratepayers and taxpayers as diffuse payers). Family members link via affects_constraints. The modeling-backed siblings function as upstream evidence sources this reading cites while overriding their conclusions; contamination propagates from this story toward the siblings when the legitimacy frame shifts, since the frame changes the reception conditions of their recommendations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
