% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Beneficiary-Maintained Closure
 *   domain: political economy / ideology / economic history
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested kernel
 *   market_as_natural_default: the beneficiary_maintained_reading, on which
 *   the presentation of markets as the natural, inevitable default of
 *   economic life is not a residue of forgotten alternatives but an actively
 *   defended settlement. On this reading, identifiable beneficiaries —
 *   finance above all, plus the corporate policy network and commercially
 *   aligned media — continuously fund the production and circulation of
 *   naturalness: think tanks, op-ed pipelines, endowed chairs,
 *   advertising-dependent framing, and revolving-door placement. Alternatives
 *   (cooperative ownership, public enterprise, strong labor codetermination,
 *   capital management) are suppressed rather than forgotten: they remain
 *   discoverable but are denied finance, legality at scale, and legitimacy.
 *   The epsilon referent is the standing naturalization arrangement itself,
 *   assessed by this reading's own lights as engineered closure — not the
 *   market-allocation substrate beneath it, and not any endorsed alternative.
 *   Claimed type (tangled_rope) and metrics are authored independently: I
 *   believe a genuine settlement function survives (a shared answer to 'what
 *   kind of economy is possible' stabilizes plans for everyone), that
 *   extraction rides asymmetrically on top of it, and that active enforcement
 *   holds the closure. The engine computes per-seat classifications from the
 *   structural data; divergence between my claim and any computed seat is the
 *   measurement the corpus takes. Sibling readings
 *   (lapsed_alternative_reading, hybrid_amnesia_reading) are separate files
 *   linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - financial_sector_incumbents — principal collector (institutional/arbitrage): rents depend on the menu of economic arrangements staying closed; funds the defense network
 *   - corporate_policy_network — agenda-setter (institutional/mobile): manufactures and circulates the naturalness frame across research institutes, media, and academe
 *   - commercial_media_owners — secondary collector (institutional/arbitrage): monetizes frame-conforming coverage and denies airtime to menu-opening proposals
 *   - organized_labor_movements — cost-bearer (organized/constrained): bears the distributional terms of the locked settlement; proposals relabeled as unnatural interference
 *   - cooperative_public_enterprise_advocates — cost-bearer (moderate/constrained): the suppressed portion of the menu; denied scale finance, legal parity, and legitimacy
 *   - heterodox_economic_scholars — cost-bearer (moderate/identity_locked): careers penalized for dissent; professional identity fused with the dissenting program
 *   - general_citizenry — dual-positioned (moderate/trapped): consumes real coordination benefits of functioning markets while absorbing foreclosed-alternative costs and a saturated persuasion environment
 *   - global_south_policy_communities — cost-bearer barred from the room (powerless/trapped): development paths pre-screened through market-naturalist conditionality
 *   - economic_historians_of_capitalism — analytical observer (analytical/analytical): documents how the frame was built and financed, with no lever on the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.68).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Beneficiary-Maintained Closure").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political economy / ideology / economic history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '47d1d1ed-3a60-42cd-ba6f-4a832e922219').
narrative_ontology:cs_kernel_codification('47d1d1ed-3a60-42cd-ba6f-4a832e922219', distributed).
narrative_ontology:cs_authority_grounding('47d1d1ed-3a60-42cd-ba6f-4a832e922219', expertise).
narrative_ontology:cs_interpretation_layer_present('47d1d1ed-3a60-42cd-ba6f-4a832e922219').
narrative_ontology:cs_reading_relation('47d1d1ed-3a60-42cd-ba6f-4a832e922219', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('47d1d1ed-3a60-42cd-ba6f-4a832e922219', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('47d1d1ed-3a60-42cd-ba6f-4a832e922219', foundational, incumbent_apparatus_defends_market_naturalization).
narrative_ontology:cs_axiom_status(incumbent_apparatus_defends_market_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('47d1d1ed-3a60-42cd-ba6f-4a832e922219', incumbent_apparatus_defends_market_naturalization, empirically_contingent).
narrative_ontology:cs_axiom('47d1d1ed-3a60-42cd-ba6f-4a832e922219', foundational, captured_foreclosure_denies_collective_economic_choice).
narrative_ontology:cs_axiom_status(captured_foreclosure_denies_collective_economic_choice, holdable).
narrative_ontology:cs_axiom_grounding('47d1d1ed-3a60-42cd-ba6f-4a832e922219', captured_foreclosure_denies_collective_economic_choice, deontological).
narrative_ontology:cs_reference_frame('47d1d1ed-3a60-42cd-ba6f-4a832e922219', defended_market_naturalism).
narrative_ontology:cs_drift_state('47d1d1ed-3a60-42cd-ba6f-4a832e922219', post_2008_legitimacy_crack, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('47d1d1ed-3a60-42cd-ba6f-4a832e922219', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_policy_network).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, commercial_media_owners).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, organized_labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, cooperative_public_enterprise_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, heterodox_economic_scholars).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, general_citizenry).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, global_south_policy_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, general_citizenry).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, tina_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, homo_economicus_model).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, spontaneous_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects fee, spread, and asset-price income whose level depends on the menu of permissible economic arrangements staying closed — alternative ownership forms, capital-management regimes, and public credit channels would compress it. Funds research institutes, campaigns, and candidacies that keep the menu closed, and can relocate capital across jurisdictions within days if any government reopens it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents, agenda_setter).

% Operates the production line for naturalness: think tanks, industry associations, endowed university chairs, op-ed syndication, and executive placement into ministries and central banks. Its staff and fellows circulate between network nodes and government. Its budget is renewed annually by the firms whose positions it defends, so its output must keep justifying the renewal.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_policy_network, agenda_setter,
    institutional, generational, mobile, global).

% Owns outlets whose revenue concentrates in advertisers and elite access, both of which reward frame-conforming economics coverage. Menu-opening proposals are covered as curiosities or threats; dissenting economists appear mainly as designated opposition. Ownership concentration makes exit from this posture expensive across the portfolio.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, commercial_media_owners, beneficiary,
    institutional, biographical, arbitrage, global).

% Bears the distributional terms of the locked settlement — weakened bargaining norms, fissured workplaces, and austerity cycles — and finds its proposals relabeled as interference with natural economic order. Cannot exit the economy it contests; organizes politically at costs its opponents offset from vastly larger war chests.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, organized_labor_movements, payer,
    organized, generational, constrained, national).

% Builds the suppressed portion of the menu — worker cooperatives, municipal enterprise, public options, participatory budgeting. Each pilot confronts denials of scale finance, legal parity with incorporated capital, and media legitimacy; success stories are absorbed as exceptions that prove the frame rather than openings in it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, cooperative_public_enterprise_advocates, payer,
    moderate, generational, constrained, national).

% Researches the foreclosed alternatives and the financing of the frame itself. Publishes in marginal journals, teaches in marginal departments, and watches hiring and grant committees route around the program. Leaving the dissent would mean abandoning the accumulated work of a career; remaining means absorbing the professional penalty indefinitely.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economic_scholars, payer,
    moderate, biographical, identity_locked, national).

% Consumes the real products of functioning markets — goods, employment, pension exposure — while paying twice for the frame: once in foreclosed alternatives that might have delivered different distributions, and once in attention spent inside a saturated persuasion environment. Individual voice is negligible; episodic collective surges meet rapid reframing and demobilization.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, general_citizenry, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, general_citizenry, beneficiary).

% Has development paths pre-screened through market-naturalist conditionality attached to credit and trade access. Domestic designs — food sovereignty, capital controls, state-led industrialization — are priced as heresy by capital flight and ratings downgrades. Effectively absent from the rooms where the frame is written, though its territories are where the frame's terms bind hardest.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, global_south_policy_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, global_south_policy_communities, excluded).

% Reconstructs from archives how the naturalization campaign was financed, staffed, and timed — the memoranda, the donor rolls, the placement records. Sees both the genuine coordination substrate beneath the frame and the engineered closure above it, and publishes findings that carry no operational lever over the arrangement they describe.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians_of_capitalism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_sector_incumbents).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, once and centrally, the constitutional question of economic life — what kinds of economic arrangement are possible — giving households, firms, and governments a shared baseline for planning, contracting, and policy expectation.
% TRANSFER_FUNCTION: Moves agenda control over economic arrangements from open democratic deliberation to incumbent capital holders; moves research funding, media coverage, and policy attention toward market-conforming framings and away from alternative designs; moves the distributional terms of the settlement onto labor, alternative-sector builders, and conditional borrowers abroad.
% ABSENT_VOICES: Global South policy communities subject to conditionality, grassroots cooperative builders outside formal advocacy organizations, and working-class constituencies beyond union structures are absent from the rooms where the frame is produced. Heterodox economists are nominally present but confined to sanctioned margins — present enough to be cited as proof of debate, absent from agenda-setting weight.
% DISAPPEARANCE_RATIONALE: If the naturalization settlement and its defense apparatus vanished overnight, the menu of economic arrangements would become openly contestable: cooperative and public-enterprise designs would seek scale finance and legal parity, capital-management and ownership questions would return to legislatures as ordinary politics rather than heresy, and the rents currently protected by the closed menu would face repricing. Investment horizons, media formats, and academic hierarchies would reorganize around a deliberated rather than decreed settlement.
% FOUNDING_PROBLEM: The 1970s twin crisis: a profitability squeeze across advanced capitalist economies coinciding with a legitimacy crisis — stagflation discrediting the postwar settlement, labor militancy, and visible competing models abroad. The naturalization campaign was assembled to restore investor confidence, break the inflationary wage-price dynamic, and foreclose redistributive politics by recasting market arrangements as nature rather than choice.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting set, economic-history scholarship on the 1970s (stagflation and profitability-crisis literatures) and archival documentation of the period's strategy memoranda corroborate that the founding crisis was real and that the response was deliberately organized. However, no party outside the beneficiary network attests that the founding problem remains live today — outside scholars date the original emergency to the 1970s-80s, while the network's own publications perpetually rediscover emergency conditions. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.48 sits in the manifest band (0.35–0.55): rents protected by foreclosed menus are substantial — fee, spread, and asset-price income that alternative regulatory and ownership designs would compress — but the underlying market substrate delivers real allocation services, capping the score below snare territory. Suppression 0.68 is authored RAW and UNSCALED: the frame is held up by funding asymmetry, journal and hiring gates, advertising-dependent coverage, and conditionality abroad; the engine scales only extractiveness (by directionality and scope), never suppression. Theater 0.35: the efficiency case was once substantive, but a growing share of output is ritual defense — annual 'in defense of markets' cycles responding to crises the frame cannot absorb — tracked by the rising theater series. Accessibility_collapse 0.60: understanding the frame does not erase alternatives (heterodox academies, cooperative sectors, and historical memory of mid-century arrangements survive), so collapse is heavy but incomplete — unlike a natural law, the menu is hidden, not annihilated. Resistance 0.58: labor campaigns, post-crisis movements, antitrust revival, degrowth discourse, and South-South heterodoxy constitute persistent, outgunned opposition. The temporal series run on one shared grid (t=0..50 step 10) with all three metrics authored at every point. Suppression_requirement is authored deliberately: enforcement CAPACITY materially changed — a build-up phase through the deregulatory battles (t=0..30) followed by a shift from overt confrontation to normalization and social self-enforcement, flattening the tail. The t=40 dip in extractiveness records the post-crisis mini-cycle: legitimacy crack, concessional reform, then reconsolidation — a partial cycle in which the concession phase functions as release-valve reinforcement (concessions defuse resistance before rents regroup), documented here rather than treated as noise. Coordination type is declared identity_coordination: the frame's dominant function is boundary maintenance — policing who counts as a serious economist or feasible policymaker. Flagged per the FNL gaming warning: 'this is simply how economies work' is precisely the identity cover story this reading alleges; the conservative floor applies and asymmetric extraction is not excused by the complexity offset.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structure. From the agenda-setter and collector seats, the arrangement is experienced as public education defending a hard-won discovery — coordination-dominant, low personal extraction (arbitrage-grade exit further dampens their effective burden). From the payer seats the same structure is experienced as a closed menu enforced at their expense — extraction-dominant. The heterodox scholar seat is the sharpest divergence: identity_lock fuses career and conviction, so the frame's costs are borne as self-definition, placing that seat near the full-target end regardless of modest nominal stakes. The citizenry seat sits near symmetric: genuine consumption benefits from functioning markets against diffuse costs of foreclosed alternatives and persuasion saturation. The historian observes the whole without a position in the flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation and no overrides are needed. Financial incumbents (declared beneficiary, arbitrage exit, institutional power) derive near the full-beneficiary end — the frame subsidizes them directly. The corporate policy network derives low despite running the apparatus, because its funding and relevance flow from the arrangement it administers. Commercial media derive low-moderate: beneficiary by advertising alignment and access, with some exposure to circulation risk when coverage tilts too visibly. Organized labor and cooperative advocates (declared victims, constrained exit) derive high — they bear the settlement's distributional terms and cannot exit the economy they contest. Heterodox scholars derive highest among domestic seats: identity_locked exit places them near the full-target end even at moderate power. General citizenry derive near symmetric from the dual payer/beneficiary declaration. Global South policy communities derive nearest the target end of all: trapped, powerless, and bearing conditionality's costs with no seat where the frame is authored. Coalition potential is real despite powerlessness: South-South blocs and transnational labor coordination have repeatedly forced menu items (capital management, food sovereignty pilots) back into view — the derivation prices current weakness, not organizing capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 1970s twin crisis of profitability and governing legitimacy that the naturalization campaign was assembled to answer — is, on the historical record, an emergency of a specific decade, corroborated by economic-history scholarship from outside the benefiting set. Yet the arrangement persists with a rising theater ratio: the defense apparatus now spends much of its output answering crises of its own making rather than the original one. That is the mandate-outlived-function signature, and declaring it matters in both directions. Reading the structure as pure extraction (snare) would erase the genuine settlement function — a shared, stable answer to the constitutional question of economic life has real value for every planner and worker, and the underlying market substrate coordinates allocation. Reading it as pure coordination (rope) would erase the capture: the settlement was manufactured rather than consented, its menu is closed by purchase rather than agreement, and its beneficiaries are identifiable and concentrated. Tangled rope holds both truths: coordination residue plus enforced asymmetric extraction, requiring the continuous enforcement spend the suppression series documents. The classification therefore blocks two symmetrical errors — mistaking a defended settlement for either a law of nature or a bare protection racket.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the beneficiary_maintained_reading of kernel market_as_natural_default; how would the sibling readings (lapsed_alternative_reading, hybrid_amnesia_reading) change this constraint''s structure?',
    'Generate and compare the sibling stories. The lapsed reading predicts absent active enforcement and beneficiaries concentrated at formation-time only, yielding a lower-extraction, inertia-dominated profile. The hybrid reading splits formation (lapse) from maintenance (capture) and assigns different extraction levels to each stage.',
    'If the lapsed account dominates, this story''s enforcement and beneficiary declarations are mis-specified and extraction falls toward the low band; if the hybrid account dominates, this story covers only the maintenance sub-stage and should be re-scoped accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed structure: sibling readings of the same kernel instantiate different constraints with different beneficiary sets and extraction levels.').

omega_variable(
    defense_causality_vs_selection,
    'Is the observed defense apparatus causally responsible for the persistence of market naturalization, or would the frame persist anyway through selection effects and institutional momentum?',
    'Natural experiments where defense funding or network capacity contracted (donor realignments, scandal-driven defunding of specific institutes) paired with longitudinal tracking of frame vitality in media, curricula, and policy agendas.',
    'If the frame persists undiminished without active defense, the reading collapses toward the lapsed-alternative structure: enforcement flags drop, extraction falls, and the constraint migrates toward an inertia-maintained classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_causality_vs_selection, empirical, 'Whether maintenance spending is load-bearing or decorative for the frame''s persistence.').

omega_variable(
    epsilon_referent_separability,
    'Does this story''s extraction measure bundle rents from maintaining the naturalization frame together with ordinary rents from market operation itself?',
    'Decompose by observable: measure the premium attributable to closed policy menus (counterfactual pricing of foregone public options and cooperative scaling) separately from returns to standard intermediation. If the two observables yield materially different extraction values, write two stories per the epsilon-invariance rule and link them.',
    'If separable, the frame-maintenance component alone carries this story''s extraction score and the market-operation remainder becomes a distinct, lower-extraction constraint in the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_separability, conceptual, 'Referent hygiene: whether one epsilon legitimately spans frame-maintenance and market-operation rents.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of alternatives primarily structural (funding gates, media access, career sanction) or internalized (widespread acceptance that no alternatives exist, sustained after exposure to pluralist argument)?',
    'Post-exposure trajectory: track whether publics and professionals who encounter credible alternative designs sustain demand for them once structural gates are bypassed (independent media episodes, cooperative successes at scale). Persistence after exposure indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts any single gatekeeper; dismantling the apparatus would not promptly reopen the menu, raising the cost class of repair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism at civilizational scale.').

omega_variable(
    profession_capture_depth,
    'How much of the economics profession''s naturalist consensus reflects independent judgment converging on the evidence, versus funded capture of journals, departments, and prize visibility?',
    'Funding-provenance audits of editorial boards, department endowments, and conference sponsorship, benchmarked against citation and hiring patterns for heterodox candidates holding comparable publication records.',
    'High capture depth supports the expertise-grounding reading of the commitment structure being a front for extraction-grounded authority; low capture depth strengthens the genuine-expertise reading and lowers the extraction attributable to the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(profession_capture_depth, empirical, 'Depth of beneficiary capture inside the profession that adjudicates the frame''s credibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(mark_tr_t40, observed).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement_basis(mark_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement_basis(mark_be_t40, observed).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(mark_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(mark_su_t10, observed).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(mark_su_t40, observed).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(mark_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'markets are the natural default' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story family: lapsed_alternative_reading (forgetting, low enforcement, inertia-leaning profile), hybrid_amnesia_reading (lapse-then-capture two-stage formation), and this file, beneficiary_maintained_reading (continuous funded defense, moderate-high extraction band 0.35-0.55). The upstream/downstream ordering runs from this file to the hybrid: the hybrid's maintenance stage cites this reading's evidence base, while the lapsed reading stands as the null hypothesis this reading must defeat empirically (see omega defense_causality_vs_selection). All three files carry mutual links in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
