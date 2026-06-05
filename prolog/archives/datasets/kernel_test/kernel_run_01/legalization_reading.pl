% ============================================================================
% CONSTRAINT STORY: legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legalization_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legalization_reading
 *   human_readable: Legalization Framework: Regulation as Harm Reduction vs. Market Extraction
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading instantiates one interpretation of substance
 *   control authority: the claim that regulated legal markets reduce harms
 *   (overdose deaths, product quality risks, market violence) compared to
 *   prohibition, and that state regulation is superior to enforcement as a
 *   harm minimization strategy. This reading is one of three competing
 *   readings of the same contested kernel (substance_control_authority). The
 *   legalization reading's core structural premise is that markets +
 *   regulation can solve information asymmetries and eliminate cartel
 *   violence, creating a coordination function alongside regulatory
 *   extraction. However, this reading generates three victim populations:
 *   communities experiencing enforcement violence (police implement
 *   legalization via enforcement against informal markets and non-compliant
 *   neighborhoods), informal market participants (street dealers, illicit
 *   manufacturers displaced by legal vendor monopolies), and incarcerated
 *   populations (prior enforcement actions continue under the carceral
 *   system). The constraint exhibits genuine tangled rope structure: real
 *   coordination benefits (overdose reduction via product quality, some
 *   violence reduction via market formalization) coexist with persistent
 *   asymmetric extraction (enforcement violence, market access barriers,
 *   vendor regulatory capture). The theater ratio (0.48) reflects that the
 *   legalization framework performs genuine public health functions
 *   (treatment access, harm reduction services) alongside regulatory
 *   performance (licensing, compliance), creating a less theatrically heavy
 *   structure than prohibition enforcement. However, the rising
 *   extractiveness trajectory (0.45 → 0.58 over 10 years) indicates
 *   regulatory capture: as vendors establish political dominance, licensing
 *   costs rise, market access barriers deepen, and enforcement violence
 *   persists or relocates. The legalization reading is read as the
 *   empirically progressive response to prohibition, but the structural data
 *   shows that legalization solves some harms while creating or displacing
 *   others.
 *
 * KEY AGENTS:
 *   - Communities experiencing enforcement violence (powerless/trapped) — neighborhoods where police implement legalization through aggressive enforcement against informal markets, maintaining racialized incarceration patterns
 *   - Users accessing regulated markets (moderate/constrained) — benefit from product quality and arrest avoidance, but constrained by legal pricing, licensing monopolies, and access barriers
 *   - Informal market participants (powerless/trapped) — street dealers, illicit manufacturers, underground supply networks eliminated or criminalized by legal vendor monopolies; no transition support
 *   - Licensed vendors and state (institutional/arbitrage) — primary beneficiaries capturing market value and tax revenue; zero experienced extraction
 *   - Public health infrastructure (organized/constrained) — harm reduction agencies with agency and sunset logic; see legalization as temporary coordination problem with performance metrics
 *   - International prohibition regime (institutional/arbitrage) — maintains global inertial norm that legalization violates; contains theater without generating compliance
 *   - Analytical observer (analytical/analytical) — assesses whether legalization genuinely reduces net harms or displaces extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legalization_reading, 0.58).
domain_priors:suppression_score(legalization_reading, 0.65).
domain_priors:theater_ratio(legalization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legalization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legalization_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legalization_reading, tangled_rope).
narrative_ontology:human_readable(legalization_reading, "Legalization Framework: Regulation as Harm Reduction vs. Market Extraction").
narrative_ontology:topic_domain(legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legalization_reading, 'ed725d89-738d-48fb-815c-8f0ea097b458').
narrative_ontology:cs_created_at('ed725d89-738d-48fb-815c-8f0ea097b458', '').
narrative_ontology:cs_kernel_codification('ed725d89-738d-48fb-815c-8f0ea097b458', formalized).
narrative_ontology:cs_authority_grounding('ed725d89-738d-48fb-815c-8f0ea097b458', expertise).
narrative_ontology:cs_kernel_id(legalization_reading, substance_control_authority).
narrative_ontology:cs_reading_relation('ed725d89-738d-48fb-815c-8f0ea097b458', prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('ed725d89-738d-48fb-815c-8f0ea097b458', harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('ed725d89-738d-48fb-815c-8f0ea097b458', foundational, regulation_superior_to_prohibition).
narrative_ontology:cs_axiom_status(regulation_superior_to_prohibition, holdable).
narrative_ontology:cs_axiom('ed725d89-738d-48fb-815c-8f0ea097b458', secondary, market_monopoly_justifiable).
narrative_ontology:cs_axiom_status(market_monopoly_justifiable, holdable).
narrative_ontology:cs_reference_frame('ed725d89-738d-48fb-815c-8f0ea097b458', regulated_market_harm_minimization).
narrative_ontology:cs_drift_state('ed725d89-738d-48fb-815c-8f0ea097b458', post_implementation_regulatory_capture, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legalization_reading, users_accessing_regulated_markets).
narrative_ontology:constraint_beneficiary(legalization_reading, state_tax_revenue).
narrative_ontology:constraint_beneficiary(legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(legalization_reading, communities_experiencing_enforcement_violence).
narrative_ontology:constraint_victim(legalization_reading, informal_market_participants).
narrative_ontology:constraint_victim(legalization_reading, incarcerated_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITIES EXPERIENCING ENFORCEMENT VIOLENCE (SNARE) — Trapped by racialized enforcement practices, aggressive order-maintenance policing, and incarceration pipeline targeting their neighborhoods. The legalization reading paradoxically maintains extraction through enforcement while claiming to reduce harm. Communities cannot exit enforcement jurisdiction; they experience maximum suppression (arrest, incarceration, asset seizure, family separation). The state frames enforcement as necessary regulation, but from this perspective it is pure extraction: communities bear costs, receive no market access benefits, and internalize the criminalization frame that justifies enforcement.
constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: USERS ACCESSING REGULATED LEGAL MARKETS (TANGLED ROPE) — Benefit from access to tested products, quality assurance, and freedom from arrest (post-legalization). But also experience regulatory extraction: taxation, licensing fees, mandatory packaging, dosing restrictions, and prohibition of certain product forms. Exit options are constrained by legal monopolies, high prices relative to informal markets, geographic access barriers, and income eligibility. Genuine coordination function (regulated quality reduces overdose deaths, product safety testing), but substantial asymmetric extraction (revenue and control concentrated with licensed vendors and state).
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMAL MARKET PARTICIPANTS (SNARE) — Legalization forecloses their livelihood. Street-level dealers, illicit manufacturers, and informal supply networks face criminalization or market elimination as legal vendors capture supply. No transition support, no path to formal licensing (capital barriers, criminal record prohibition, regulatory complexity). Trapped in informal economy or incarceration. Legalization reading presents this as harm reduction, but from this perspective it is market foreclosure via state monopoly creation — maximum extraction with no exit.
constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: LICENSED VENDORS AND STATE (ROPE) — Primary beneficiaries. Licensed vendors capture market value that was previously distributed across informal networks; state captures tax revenue. The legalization reading describes this as legitimate market coordination and public health funding. From this perspective, the constraint is functional coordination: creating a regulated market solves the information asymmetry problem (product safety), reduces violence (eliminates turf wars between cartels), and funds public health. Exit options for these agents are maximally flexible — they can arbitrage market conditions, adjust pricing, reallocate capital across jurisdictions. Zero experienced extraction; net benefit.
constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH INFRASTRUCTURE (SCAFFOLD) — Organized agents (public health agencies, overdose prevention programs, harm reduction coalitions) see legalization as partial coordination with a sunset: the legalization framework promises that regulation-funded treatment access, naloxone distribution, and harm reduction services will eventually exceed the direct harm of market violence. Theater ratio is moderate (0.48) — the state performs legitimate public health functions (addiction treatment, overdose response) alongside regulatory performance (licensing administration). Constrained but with agency and visible exit path: if treatment access targets are not met, if enforcement violence persists, if market prices remain prohibitive, organized agents can pivot to alternative structures (decriminalization without legalization, universal treatment access, harm reduction prioritization). Sunset is conditional on performance metrics.
constraint_indexing:constraint_classification(legalization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL DRUG POLICY REGIME (PITON) — UN Single Convention on Narcotic Drugs, international scheduling, and treaty obligations create a backdrop of inertial international prohibition. Individual nations' legalization frameworks are treated as exceptions or violations of the global prohibition norm. The international regime persists through institutional habit despite increasing evidence that it generates harms (mass incarceration, cartel violence, overdose deaths). From this perspective, national legalization is framed as temporary deviation from the normal rule, maintained not because it works but because local political pressure forces it. The international architecture contains theater (annual Commission on Narcotic Drugs meetings, supply control ceremonies) that maintains the prohibition frame without generating compliance.
constraint_indexing:constraint_classification(legalization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, the legalization reading shows genuine coordination function (regulated markets reduce certain harms: overdose via product quality, some violence via eliminating turf wars) alongside persistent or displaced extraction (enforcement violence, informal market criminalization, regulatory capture by licensed vendors, tax burden on users). Effective extraction χ at global scope is amplified by scope multiplier (σ(global)=1.2). The core tension: legalization solves some coordination problems (product quality, information symmetry) while creating others (market access inequality, incarceration of informal vendors, racialized enforcement). The classification is tangled rope because both functions (coordination toward harm reduction AND asymmetric extraction via enforcement and market control) are structurally present and mutually dependent — the regulation requires enforcement to maintain vendor monopoly and prevent undercutting by informal markets.
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legalization_reading, TR),
    TR >= 0.70.

:- end_tests(legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. The legalization reading generates extraction through multiple mechanisms: (1) enforcement violence targeting informal markets and enforcement-resistant communities (suppression = 0.65), (2) regulatory extraction from users (taxation, licensing fees, mandatory costs), (3) market access barriers excluding low-income and criminalized populations. The rising trajectory (0.45 → 0.58) reflects regulatory capture velocity — vendors establish political dominance, licensing costs increase, enforcement becomes more selective, and public health provisions are underfunded. Suppression (0.65): Moderate-high and non-declining. The legalization framework maintains high suppression through enforcement violence, incarceration of informal vendors, and the threat of criminal sanctions for non-compliance. Suppression is not reduced by legalization; it is relocated and relabeled as regulatory enforcement. Theater ratio (0.48, declining): The legalization framework performs genuine harm reduction functions (overdose prevention, treatment access, product safety testing) alongside regulatory performance (licensing administration, compliance inspection), reducing the pure theater ratio from 0.62 (prohibition-era performance) to 0.48 (legalization-era mixed function). The declining theater ratio reflects real institutional shift toward functional harm reduction, not pure performance. However, the rising extractiveness despite declining theater suggests that the real institutional gains are being offset by regulatory capture and vendor dominance.
 *
 * PERSPECTIVAL GAP:
 *   The legalization reading's perspectives split sharply across the beneficiary/victim divide. Licensed vendors and the state (perspective 4, rope) experience zero extraction and pure coordination function — the legalization framework is genuinely beneficial for market expansion and revenue generation. Organized public health infrastructure (perspective 5, scaffold) sees mixed coordination with sunset logic — the framework has potential but requires enforcement of performance metrics. Communities experiencing enforcement violence (perspective 1, snare) see pure extraction: legalization maintains police authority and incarceration infrastructure while providing them no benefits. Informal market participants (perspective 3, snare) experience market foreclosure — legalization eliminates their livelihood with no transition path. Users (perspective 2, tangled rope) occupy the middle ground: real benefits (arrest avoidance, product quality) alongside extraction (pricing, access barriers, restricted product forms). The analytical observer (perspective 7, tangled rope) sees both coordination and extraction simultaneously. This perspectival gap reflects the kernel-level ambiguity: legalization is simultaneously a genuine harm reduction strategy (from the licensed vendor and public health perspectives) and a mechanism for displacing prohibition violence into enforcement violence (from the community and informal market perspectives). The same framework is rope for the beneficiary, snare for the victim, scaffold for organized agencies, and piton for the international regime — all classifications valid from their respective structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from structural position relative to the extraction flow. Licensed vendors and state (beneficiaries, institutional power, arbitrage exit) derive d ≈ 0.05 (full beneficiary): experience minimal f(d) and negative or zero χ. Users (mixed beneficiary/victim status, moderate power, constrained exit) derive d ≈ 0.55 (slight victim): experience moderate f(d) and positive χ reflecting extraction through access barriers and pricing. Communities and informal vendors (victims, powerless, trapped exit) derive d ≈ 0.95 (full target): experience maximum f(d) ≈ 1.42 and high χ reflecting enforcement violence suppression. The analytical observer (neutral, analytical power, analytical exit) derives d ≈ 0.72 (balanced): sees extraction mechanisms operating alongside coordination functions. No directionality overrides are required — the canonical derivation chain produces appropriate differentiation across beneficiaries and victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_violence_displacement,
    'Does legalization reduce total enforcement violence, or merely relocate it from street dealers to informal market participants and communities resisting implementation?',
    'Longitudinal comparison of enforcement arrest and incarceration rates pre/post-legalization, disaggregated by enforcement target (street-level dealers vs. organized crime vs. new charges like unlicensed manufacturing). Spatial analysis of enforcement intensity across neighborhoods.',
    'If violence reduced: legalization framework is genuine harm reduction. If displaced: extraction persists through enforcement violence targeting informal markets, making victims permanent rather than transitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_violence_displacement, empirical, 'Whether enforcement violence is reduced or displaced under legalization').

omega_variable(
    market_access_equity,
    'Do users with lower incomes, criminal records, and geographic isolation actually access legalized regulated markets, or does legalization create a two-tier system (legal for affluent users, criminal for poor users)?',
    'Comparative analysis of legal market access by income quintile, criminal record status, and geographic distance to licensed vendors. Market price tracking vs. informal market prices. Usage surveys tracking which populations use legal vs. illicit sources post-legalization.',
    'If equitable access: legalization achieves stated harm reduction goal. If two-tier: legalization is tangled rope with persistent extraction through access barriers, and the ''universal'' regulation narrative masks class-stratified outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_access_equity, empirical, 'Whether legalization produces equitable market access across income and criminal record status').

omega_variable(
    informal_market_persistence,
    'After legalization, does the informal market contract to residual users (those unable to access legal markets), or does it persist at pre-legalization scale by undercutting legal prices and avoiding regulatory costs?',
    'Seizure data, arrest data, price comparison studies, ethnographic studies of user sourcing behavior. Estimation of informal market share pre/post-legalization.',
    'If informal market contracts: legalization has displaced informal supply, victims shift from users (to regulated market) to informal vendors (criminalization). If informal market persists: both legal and illegal extraction mechanisms operate simultaneously, and communities continue bearing enforcement violence while legal vendors capture only partial market share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_market_persistence, empirical, 'Whether informal markets contract or persist after legalization').

omega_variable(
    regulatory_capture_velocity,
    'What timeline does regulatory capture follow: do licensed vendors establish political dominance over regulators before public health agencies secure enforcement of harm reduction obligations?',
    'Analysis of regulatory agency budgets, staffing ratios (compliance inspectors vs. public health staff), enforcement priorities, and political lobbying expenditures by licensed vendors. Comparative case studies of jurisdictions with strong vs. weak public health provisions in legalization frameworks.',
    'If public health agencies dominate: legalization can sustain tangled rope with genuine coordination function. If vendors dominate within 5-10 years: constraint becomes snare with legalization narrative masking vendor extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_velocity, empirical, 'Speed of regulatory capture by licensed vendors vs. public health prioritization').

omega_variable(
    reading_foreclosure_claim,
    'Can the legalization reading and prohibition reading coexist within a single jurisdiction''s framework, or does the core normative commitment of legalization (markets + regulation as superior harm strategy to prohibition enforcement) logically rule out the prohibition reading''s core commitment (interdiction as primary harm strategy)?',
    'Structural analysis of competing premises: legalization assumes markets generate better information and reduce violence; prohibition assumes markets generate cartels and violence, and enforcement reduces harms. Both cannot be held simultaneously within the same jurisdiction''s policy framework without internal contradiction at the level of empirical claim about causal effects of enforcement.',
    'If forecloses: the legalization and prohibition readings are not compatible alternatives within one framework; supporting legalization means rejecting prohibition''s empirical premises. If coexists: both are live positions held by different factions. Current evidence suggests that legalization jurisdictions explicitly reject the prohibition reading''s causal claims, supporting foreclosure classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_claim, conceptual, 'Whether legalization reading''s causal premises logically foreclose the prohibition reading').

omega_variable(
    foundational_axiom_robustness,
    'Is the axiom ''regulation_superior_to_prohibition'' robust to evidence that regulation produces new extraction mechanisms (regulatory capture, market access barriers, incarceration of informal vendors)? Or does the legalization reading require updating its core premise?',
    'Logical analysis: can the legalization reading acknowledge and accommodate the new extraction mechanisms (via scope limitations or transition provisions) while maintaining the foundational claim that regulation reduces net harms? Or does evidence of persistent/displaced extraction undermine the axiom itself?',
    'If robust: legalization can be upgraded from tangled rope to qualify for scaffold (by adding temporal sunset provisions and performance metrics). If fragile: the legalization reading''s foundational axiom may require overriding, suggesting the reading should shift status from holdable to overridden in light of empirical failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_axiom_robustness, empirical, 'Robustness of the foundational axiom to evidence of persistent extraction mechanisms').

omega_variable(
    kernel_ambiguity_substance_control,
    'Is the contested kernel ''substance_control_authority'' fundamentally about the empirical question (what strategy reduces harms?), or about the normative question (who decides what counts as a harm)?',
    'Examination of actual dispute structure: do prohibition and legalization readings disagree primarily on causal claims (enforcement reduces violence vs. markets reduce violence) or on value frameworks (whose safety counts, what level of individual autonomy is acceptable)?',
    'If empirical: readings can potentially be resolved by evidence gathering. If normative: the dispute is fundamentally about framework choice, and readings coexist rather than foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_substance_control, conceptual, 'Whether substance control authority kernel is empirical or normative in nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legal_theater_t0, legalization_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(legal_theater_t5, legalization_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(legal_theater_t10, legalization_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(legal_extract_t0, legalization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legal_extract_t5, legalization_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(legal_extract_t10, legalization_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(legalization_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(legalization_reading, enforcement_violence_accumulation).
narrative_ontology:affects_constraint(legalization_reading, regulatory_capture_dynamics).

% DUAL FORMULATION NOTE:
% The legalization_reading constraint represents one reading of the substance_control_authority kernel. The prohibition_reading and harm_reduction_reading represent competing readings with different ε values, victim sets, and beneficiary structures. All three readings share the kernel but produce different constraints due to different empirical premises about causal effects of control strategies. The legalization_reading is downstream of international drug policy regime (perspective 6, piton) but upstream of jurisdictional-level capture dynamics. Network links connect readings that share the kernel and constraints that are causally influenced by legalization policy choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
