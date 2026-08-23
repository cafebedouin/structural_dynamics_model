% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation Legitimacy Requirement
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   A normative-political standard asserting that a climate response counts
 *   as legitimate only if it dismantles the growth imperative in wealthy
 *   nations through structural economic transformation: universal basic
 *   services, working-time reduction, and democratic firm ownership. The
 *   standard operates on wealthy-nation polities — defining which policies
 *   may claim legitimacy, which coalitions cohere, and which commissions
 *   convene. Its referent arrangement is the standing growth-dependent
 *   regime, which this reading assesses as transferring atmospheric space and
 *   climate stability from future generations and exposed populations abroad
 *   to present wealthy-nation consumption. The standard's own operation, were
 *   it enforced, reverses that flow: current wealthy-nation households and
 *   carbon asset holders enter the cost-bearer set through income and
 *   consumption reduction, while future generations receive reduced warming
 *   without technological dependency. Enforcement is the binding weakness:
 *   the standard commands majorities in the abstract and loses them when
 *   costs are itemized, so its machinery remains largely discursive with
 *   institutional footholds at the margins. KEY AGENTS (by structural
 *   relationship): - future_generations_globally: primary beneficiary seat
 *   (powerless/trapped) — receives reduced warming without consenting or
 *   paying in any present forum - climate_vulnerable_global_south: secondary
 *   beneficiary (organized/constrained) — gains from wealthy-nation
 *   contraction; holds diplomatic but not enforcement leverage -
 *   developing_economy_states: swing beneficiary (organized/mobile) —
 *   endorses contraction applied to others while defending its own
 *   development space - affluent_high_consumption_households: primary payer
 *   (organized/constrained) — bears the largest itemized consumption and tax
 *   costs - carbon_intensive_sector_workers: concentrated payer
 *   (organized/trapped) — bears localized job destruction; sharpest
 *   resistance node - carbon_capital_owners: payer with arbitrage exit
 *   (powerful/arbitrage) — faces stranded stocks; funds opposition -
 *   low_income_wealthy_nation_households: net beneficiary with incidental
 *   costs (powerless/constrained) — receives decommodified services; pivotal
 *   electoral weight - degrowth_movement_intelligentsia: agenda setter with
 *   reputational collection (moderate/identity_locked) — authors the
 *   standard, enforces it discursively - wellbeing_governance_bodies:
 *   institutional agenda setter (institutional/mobile) — pilots fragments
 *   where electoral cost is low - intergovernmental_assessment_bodies:
 *   analytical observer (institutional/analytical) — documents the delivery
 *   gap every camp cites
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.78).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation Legitimacy Requirement").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, 'b659422f-f31c-4c09-8f25-ddb22855aa54').
narrative_ontology:cs_kernel_codification('b659422f-f31c-4c09-8f25-ddb22855aa54', distributed).
narrative_ontology:cs_authority_grounding('b659422f-f31c-4c09-8f25-ddb22855aa54', expertise).
narrative_ontology:cs_interpretation_layer_present('b659422f-f31c-4c09-8f25-ddb22855aa54').
narrative_ontology:cs_reading_relation('b659422f-f31c-4c09-8f25-ddb22855aa54', climate_response_legitimacy__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('b659422f-f31c-4c09-8f25-ddb22855aa54', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_axiom('b659422f-f31c-4c09-8f25-ddb22855aa54', foundational, growth_preserving_response_insufficient).
narrative_ontology:cs_axiom_status(growth_preserving_response_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('b659422f-f31c-4c09-8f25-ddb22855aa54', growth_preserving_response_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('b659422f-f31c-4c09-8f25-ddb22855aa54', foundational, throughput_reduction_legitimacy_criterion).
narrative_ontology:cs_axiom_status(throughput_reduction_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b659422f-f31c-4c09-8f25-ddb22855aa54', throughput_reduction_legitimacy_criterion, deontological).
narrative_ontology:cs_axiom('b659422f-f31c-4c09-8f25-ddb22855aa54', secondary, democratic_ownership_component).
narrative_ontology:cs_axiom_status(democratic_ownership_component, holdable).
narrative_ontology:cs_axiom_grounding('b659422f-f31c-4c09-8f25-ddb22855aa54', democratic_ownership_component, instrumental).
narrative_ontology:cs_reference_frame('b659422f-f31c-4c09-8f25-ddb22855aa54', fair_share_throughput_contraction_framework).
narrative_ontology:cs_drift_state('b659422f-f31c-4c09-8f25-ddb22855aa54', contemporary_post_ar6, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b659422f-f31c-4c09-8f25-ddb22855aa54', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations_globally).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_global_south).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, developing_economy_states).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, low_income_wealthy_nation_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, affluent_high_consumption_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, carbon_intensive_sector_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, carbon_capital_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, degrowth_movement_intelligentsia).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, low_income_wealthy_nation_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People not yet born who will live under whatever atmospheric concentration the present decades lock in. They receive the benefit of any genuine absolute emission reduction and bear the cost of any shortfall, without voting, paying, or negotiating in any present forum; their interests enter only through proxy advocates, constitutional clauses, and long-horizon institutions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations_globally, beneficiary,
    powerless, civilizational, trapped, global).

% Populations in regions with high physical exposure to warming and low cumulative emissions responsibility — coastal South Asia, the Sahel, small island states. They gain from any real reduction in wealthy-nation throughput and organize diplomatically as negotiating blocs demanding finance and loss-and-damage recognition, but hold no lever over wealthy-nation domestic policy choices; migration options are bounded by border regimes.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_global_south, beneficiary,
    organized, generational, constrained, continental).

% Governments of middle- and low-income economies that endorse the claim that wealthy nations must contract first, on historical-responsibility grounds, while insisting on their own development space. They gain diplomatic leverage and climate space from the standard's adoption and can move between policy framings in negotiation, backing whichever one shifts finance and effort toward rich countries.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, developing_economy_states, beneficiary,
    organized, generational, mobile, continental).

% The top-consuming deciles within wealthy economies, whose footprints dominate national emission profiles. Under the transformation program they bear the largest reductions: caps on high-carbon consumption such as frequent flying and large vehicles, higher progressive taxation funding universal services, and wealth levies. They cannot exit the national polity but can shelter consumption through offsets and cross-border spending; their defection registers visibly in lifestyle politics and tax revolts.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, affluent_high_consumption_households, payer,
    organized, biographical, constrained, national).

% Lower-income households in wealthy economies, whose emissions are modest and whose access to energy, transport, housing, and care is precarious. Universal basic services transfers the largest material gains to them — decommodified transit, housing retrofits, energy allowances — even as macroeconomic contraction touches them too; most program costings put their net position positive, which makes their electoral weight pivotal to the standard's fate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, low_income_wealthy_nation_households, beneficiary,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, low_income_wealthy_nation_households, payer).

% Workers in fossil fuel extraction, refining, aviation, cement, combustion-vehicle assembly, and energy-intensive manufacturing, concentrated in specific regions. Structural change eliminates or transforms their jobs faster than replacement employment appears locally; skills and mortgages tie them to place. Transition guarantees are promised in program documents, but historical retraining schemes under-delivered, so they treat new promises accordingly and resist through unions and regional politics.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, carbon_intensive_sector_workers, payer,
    organized, immediate, trapped, regional).

% Owners and shareholders of carbon-intensive assets — reserves, pipelines, airlines, combustion supply chains — whose valuations assume continued throughput growth. The transformation strands a large share of these assets. Capital mobility lets them shift portfolios and production jurisdictions ahead of policy, and they fund political opposition, media framing, and investment strikes; mobility protects their income flows, not their stock values.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, carbon_capital_owners, payer,
    powerful, biographical, arbitrage, global).

% Ecological economists, post-growth think tanks, and activist-scholars who produce the legitimacy argument, cost the universal-services programs, draft working-time legislation, and staff the commissions that adopt the framing. Their careers, networks, and institutional positions are built on the thesis; they collect reputational and positional returns when governments convene beyond-growth processes, and they police the standard discursively by rating growth-preserving proposals as inadequate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_movement_intelligentsia, agenda_setter,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, degrowth_movement_intelligentsia, beneficiary).

% State and supranational units experimenting with the program's fragments — future-generations commissioners, wellbeing budget frameworks, beyond-growth parliamentary committees, citizen assemblies on climate. They adopt pieces of the agenda where electoral cost is low, pilot working-time reductions and service expansions, and measure progress in wellbeing indicators rather than GDP; their mandates expand or shrink with each electoral cycle.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, wellbeing_governance_bodies, agenda_setter,
    institutional, generational, mobile, national).

% Scientific assessment bodies compiling physical-science projections, emissions-gap accounting, and scenario libraries, including low-energy-demand pathways. They evaluate proposals against temperature outcomes without endorsing any particular legitimacy criterion; their published gaps between pledges and pathways are cited by every camp for its own purposes.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, intergovernmental_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, low_income_wealthy_nation_households).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the burden-allocation side of the climate collective-action problem: where efficiency and pricing instruments demonstrably under-deliver absolute reductions, the standard supplies a shared criterion specifying who must reduce what — absolute material throughput in wealthy nations, allocated progressively through decommodified services — coordinating expectations among movements, states, and assessment bodies about what a fair contribution consists of.
% TRANSFER_FUNCTION: Moves consumption capacity, income, and workplace control from affluent households and carbon-intensive asset holders in wealthy economies into decommodified public services and lower-income households now, and moves usable atmospheric carbon space from present wealthy-nation users to future generations and exposed populations abroad.
% ABSENT_VOICES: Future generations are absent by construction and speak only through proxy commissioners and long-horizon clauses; exposed global-south populations sit outside the wealthy-nation polities whose votes decide enactment; carbon-intensive workers are addressed late, after program parameters are set; carbon capital participates only as a target of the standard, never as a drafter of its legitimacy terms — its objections register as opposition, not as voice.
% DISAPPEARANCE_RATIONALE: Movement coalitions, wellbeing-governance experiments, and academic programs organized around the standard would lose their unifying demand; the policy field would revert to pricing-and-innovation framings; the specific claim that wealthy-nation sufficiency is a legitimacy precondition would vanish from commission agendas and citizen-assembly charters. Material emissions trajectories would barely move in the short run — the standard's enforcement is too weak — but the institutional arrangements built on it would dismantle within a few electoral cycles.
% FOUNDING_PROBLEM: The delivery gap: wealthy economies pledged reductions that growth-preserving instruments — efficiency, pricing, substitution — have not produced at the required rate or scale, with apparent domestic decoupling partly outsourced through trade and partly banked on undeployed removal technology. The standard was built to close that gap by making absolute throughput contraction, rather than intensity improvement, the test of a legitimate response.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: intergovernmental assessment bodies (emissions-gap reporting) and national audit offices document the pledge-delivery gap; mitigation-economy economists who reject the degrowth remedy nonetheless attest that the gap exists — they dispute the cure, not the diagnosis. The problem statement therefore does not rest on the movement's own testimony; the contested element is the remedy, not the gap.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78 is authored for the standing growth-dependent arrangement as this reading assesses it — the intergenerational transfer of climate stability to present consumption, and impact-shifting onto populations with negligible cumulative emissions — not for the transformation program itself, which is the proposed corrective. Suppression 0.62 reflects the coercive content of enforcement-as-designed: overriding itemized-cost majorities, capping high-carbon consumption, and discursively ruling growth-preserving packages out of the legitimate set; the current machinery is weak, but suppression is authored as a property of the constraint's design, not its present enforcement strength. Theater 0.35: a large share of observable activity is declarative (manifestos, conferences, commission testimony) relative to enacted measures, though functional uptake — wellbeing budgets, working-time trials, service pilots — is growing, which the falling theater series tracks. Accessibility_collapse 0.25: understanding the standard does not close competing policy frames; rival legitimacy criteria remain fully articulable and institutionally sponsored. Resistance 0.72: fuel-tax and cost-of-living backlashes, electoral reversals of green governing coalitions, and sectoral opposition are the documented pattern whenever costs are itemized. Claim/metric independence: tangled_rope is claimed from structure — a genuine collective-action function (burden allocation for a real commons problem) bound to asymmetric cross-generational and cross-class incidence, held together by enforcement the standard does not yet possess; the metrics describe observed operation independently of that claim. All three measurement series share one eight-point grid; the suppression series dips at t8 (the post-backlash enforcement retreat) before rebuilding institutionally — a proposal/backlash/retreat/consolidation dynamic, not noise.
 *
 * PERSPECTIVAL GAP:
 *   From the affluent-household and carbon-capital seats the standard reads as confiscatory: it prices their lifestyles and strands their assets while promising benefits that arrive after their horizons. From the future-generations and exposed-population seats it reads as the minimum arithmetic of justice — the only offered standard that names them as recipients at all. From the intelligentsia seat it is an emancipatory program whose main obstacle is imagination. Sector workers occupy a distinct intermediate position: they accept the diagnosis but distrust the transition promises, having watched retraining schemes under-deliver. The engine computes these divergent per-seat classifications from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (future generations, exposed populations abroad, developing-economy states, low-income service recipients) drive those seats toward the subsidized end; victim declarations (affluent households, sector workers, carbon capital) drive them toward the target end, amplified by trapped exits for regionally locked workers. Two overrides correct derivations the structural arrays cannot see. First, carbon_capital_owners holds arbitrage-grade exit, which the exit term alone would read as a near-beneficiary position, but capital mobility protects income flows while leaving stock values stranded — overridden to d=0.85. Second, degrowth_movement_intelligentsia holds no beneficiary declaration yet collects reputational and positional returns when institutions adopt the framing — overridden to d=0.30 to record that mild subsidy. Note the referent split: epsilon is authored for the standing arrangement the reading contests, while the beneficiary/victim arrays describe the standard's own would-be operation. The two point in opposite directions by design — the standard is a corrective aimed at reversing the standing arrangement's flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the standard as pure coordination would erase its defining asymmetry: participants are not net beneficiaries — the paying generation buys stability it will partially enjoy and largely bequeath, so the coordination story and the cost story must both be kept. Reading it as pure extraction would erase the genuine commons problem the burden-allocation function addresses; the coordination function is not cover. The tangled-rope classification holds both halves. The scaffold temptation is real — transformation rhetoric implies a steady-state end state at which the corrective machinery retires — but no sunset clause is declared anywhere in the program literature, and declaring one would fabricate a transition promise the program does not make. Mandatrophy is unresolved: the founding delivery gap is live and independently corroborated, so no resolved-mandate declaration is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the degrowth_transformation reading of the climate_response_legitimacy kernel; which structural elements would differ under the sibling readings?',
    'Comparative classification of the sibling stories (mitigation_priority, adaptation_priority): diff the victim sets, cost-bearer entry points, directionality profiles, and enforcement requirements across the three files.',
    'Under mitigation_priority the cost-bearer set shifts from current wealthy-nation households (income and consumption reduction) to diffuse carbon-price incidence and fiscal balance-sheet risk, and future generations'' benefit becomes contingent on technology deployment rather than structural change; under adaptation_priority present-day exposed populations become the primary beneficiary seat and wealthy-nation cost-bearing shrinks to resilience spending.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings; sibling deltas live in the sibling files, not here.').

omega_variable(
    sufficiency_premise_contest,
    'Where exactly is the kernel disagreement located — in the empirical sufficiency premise (can growth-preserving instruments deliver required absolute reductions) or in the normative weighting of present versus future welfare?',
    'Decompose each sibling''s foundational axioms: if the mitigation reading''s core premise asserts decoupling sufficiency as an empirically demonstrated fact, the dispute is empirical and resolvable by observed absolute-decoupling rates; if it asserts present-welfare priority as a value, the dispute is normative and persists under any evidence.',
    'An empirical location would allow evidence to move seats between readings over time and eventually collapse the contest; a normative location fixes the contest as permanent pluralism with no evidentiary resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_premise_contest, conceptual, 'Locates the kernel disagreement: empirical sufficiency versus normative time-preference.').

omega_variable(
    resistance_composition,
    'Is wealthy-nation resistance to the standard driven by material interest (income and consumption stakes) or by internalized growth-consumption identity (rising-throughput prosperity as constitutive of the good life)?',
    'Disaggregate referendum and survey resistance among households whose net fiscal position under universal basic services would be positive; track post-adoption satisfaction trajectories in working-time reduction trials where material stakes were compensated.',
    'If resistance persists among net beneficiaries, a substantial share of suppression is internalized and compensation engineering cannot dissolve the enforcement burden; if resistance tracks net material position, the barrier is structural and compensable by program design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_composition, empirical, 'Structural versus internalized composition of the political feasibility barrier.').

omega_variable(
    crisis_window_dependency,
    'Does the standard''s enforcement capacity advance mainly through crisis windows (energy shocks, disaster seasons, inflation episodes) rather than through steady democratic consent?',
    'Time-series correlation between enforcement milestones (legislation, budget adoptions, commission mandates) and crisis events across the interval; comparative timing analysis of sufficiency-policy adoption across jurisdictions.',
    'If crisis-dependent, the persistence profile is punctuated rather than cumulative — enforcement ratchets during shocks and decays between them — and the suppression series should be read as window-driven rather than trend-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_window_dependency, empirical, 'Whether enforcement advances ride crisis windows.').

omega_variable(
    realized_climate_dividend,
    'Do future generations actually receive the climatic benefit that justifies the standard, or is the dividend eroded by rebound consumption, offset accounting, trade leakage, and implementation shortfall?',
    'Attribution analysis linking enacted transformation measures to verified absolute emission reductions net of leakage and rebound, tracked against the counterfactual growth-preserving pathway.',
    'A materially eroded dividend would convert the cross-generational transfer into intra-generational redistribution with climatic cover, weakening the future-generations beneficiary directionality and shifting computed classifications toward the extraction-heavy end for every paying seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(realized_climate_dividend, empirical, 'Whether the promised intergenerational transfer actually lands.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as the legitimacy question itself (who holds authority to certify a climate response as legitimate) or as the policy-content axis (which instrument portfolio constitutes an adequate response)?',
    'Test both framings against the sibling set: under the authority framing the three readings compete as rival certification regimes; under the content framing they compose into mixed portfolios, changing whether rivalry or complementarity is the correct structural description.',
    'The authority framing yields winner-take-most certification dynamics among incompatible standards; the content framing yields combinable instruments in which the readings are partial complements — changing downstream foreclosure and coexistence computations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: authority-question versus policy-content framing of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crl_degrowth_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.6).
narrative_ontology:measurement(crl_degrowth_tr_t2, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2, 0.56).
narrative_ontology:measurement(crl_degrowth_tr_t4, climate_response_legitimacy__degrowth_transformation, theater_ratio, 4, 0.52).
narrative_ontology:measurement(crl_degrowth_tr_t6, climate_response_legitimacy__degrowth_transformation, theater_ratio, 6, 0.47).
narrative_ontology:measurement(crl_degrowth_tr_t8, climate_response_legitimacy__degrowth_transformation, theater_ratio, 8, 0.44).
narrative_ontology:measurement(crl_degrowth_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.41).
narrative_ontology:measurement(crl_degrowth_tr_t12, climate_response_legitimacy__degrowth_transformation, theater_ratio, 12, 0.38).
narrative_ontology:measurement(crl_degrowth_tr_t15, climate_response_legitimacy__degrowth_transformation, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(crl_degrowth_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(crl_degrowth_be_t2, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2, 0.69).
narrative_ontology:measurement(crl_degrowth_be_t4, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 4, 0.71).
narrative_ontology:measurement(crl_degrowth_be_t6, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(crl_degrowth_be_t8, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 8, 0.73).
narrative_ontology:measurement(crl_degrowth_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(crl_degrowth_be_t12, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(crl_degrowth_be_t15, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(crl_degrowth_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(crl_degrowth_su_t2, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2, 0.32).
narrative_ontology:measurement(crl_degrowth_su_t4, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(crl_degrowth_su_t6, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(crl_degrowth_su_t8, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(crl_degrowth_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(crl_degrowth_su_t12, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(crl_degrowth_su_t15, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate climate response' covers at least three structurally distinct claims with different victim sets, cost-bearer entry points, and epsilon values: growth-preserving decoupling (mitigation_priority), trajectory acceptance with protection (adaptation_priority), and wealthy-nation throughput contraction (this file). Per the epsilon-invariance principle these are separate constraints, not one constraint with a measurement parameter; each carries its own beneficiaries, victims, and enforcement profile. This file instantiates the degrowth reading. Its foundational insufficiency axiom contradicts the mitigation reading's compatibility premise (forecloses); its historical-responsibility framing exerts downstream pressure on adaptation finance politics without eliminating the protection premise (influences). The assessment-body evidence base is common to all three readings; the readings diverge at the normative criterion layer, not the evidence layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.85).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
