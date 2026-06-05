% ============================================================================
% CONSTRAINT STORY: engineered_closure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_engineered_closure_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: engineered_closure_reading
 *   human_readable: Market Naturalization as Engineered Institutional Closure
 *   domain: political_economy/institutional_theory
 *
 * SUMMARY:
 *   This constraint models market naturalization as active institutional work
 *   rather than as spontaneous emergence or as a neutral analytical truth.
 *   The engineered_closure reading specifically asserts that maintaining
 *   market-as-inevitable requires continuous state redesign,
 *   beneficiary-visible policy advocacy, and suppression of alternative
 *   coordination framings. The constraint distinguishes itself from sibling
 *   readings by foregrounding institutional labor: the state does not
 *   passively accommodate markets, nor do markets operate through dual logic
 *   (simultaneously market and non-market). Instead, beneficiaries—capital
 *   accumulation agents, rent-extraction beneficiaries—continuously redesign
 *   state structures, intellectual production, and regulatory frameworks to
 *   defend the market-naturalization premise. This is not a natural law or an
 *   inevitable equilibrium. It is contested institutional work with visible
 *   costs: closure of alternative coordination possibilities, atrophy of
 *   public institutional capacity, and cognitive capture of the powerless
 *   into identity-locked market frames. The extractiveness trajectory (0.32 →
 *   0.58 over the interval) reflects intensifying institutional maintenance
 *   as alternatives become more visible; theater ratio drift (0.48 → 0.62)
 *   reflects increasing gap between the functional necessity of market
 *   coordination and the expansionist political program that uses market
 *   naturalization to justify privatization.
 *
 * KEY AGENTS:
 *   - Capital Accumulation Agents: Primary beneficiaries (institutional/arbitrage) — benefit from state redesign enabling rent extraction, accumulation through debt-financed growth, and privatization of public goods
 *   - Rent-Extraction Beneficiaries: Secondary beneficiary group (institutional/arbitrage) — financial sector, IP monopolists, platform gatekeepers who extract surplus through information asymmetry and structural position
 *   - Alternative Coordination Possibilities: Primary victim (powerless/identity_locked) — public coordination, commons management, mutual aid, cooperative platforms appear unthinkable within naturalized market frame
 *   - Public Institutional Capacity: Secondary victim (moderate/constrained) — state capacity for alternative coordination (public banking, enterprise ownership, infrastructure investment) atrophies through decades of market-naturalizing discourse and policy
 *   - Reform-Seeking State Actors: Ambiguous position (moderate/constrained) — want alternative policies but face capital mobility threat and epistemic capture by market-naturalizing orthodoxy
 *   - Institutional Innovation Coalition: Organized challengers (organized/constrained) — platform cooperatives, municipal public banking, degrowth advocates who demonstrate alternative viability but face institutional marginalization
 *   - Economics Profession: Defensive authority (institutional/arbitrage) — reproduces market naturalization through textbooks, methodology, hiring practices; sees continuous intellectual work as neutral knowledge production
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating market naturalization as discovered truth (Mountain) rather than recognized institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(engineered_closure_reading, 0.58).
domain_priors:suppression_score(engineered_closure_reading, 0.68).
domain_priors:theater_ratio(engineered_closure_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(engineered_closure_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(engineered_closure_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(engineered_closure_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(engineered_closure_reading, tangled_rope).
narrative_ontology:human_readable(engineered_closure_reading, "Market Naturalization as Engineered Institutional Closure").
narrative_ontology:topic_domain(engineered_closure_reading, "political_economy/institutional_theory").

domain_priors:requires_active_enforcement(engineered_closure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(engineered_closure_reading, formalized).
narrative_ontology:cs_authority_grounding(engineered_closure_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(engineered_closure_reading).
narrative_ontology:cs_kernel_id(engineered_closure_reading, market_as_natural_default).
narrative_ontology:cs_reading_relation(engineered_closure_reading, lapsed_closure_reading, influences).
narrative_ontology:cs_reading_relation(engineered_closure_reading, dual_operation_reading, coexists_with).
narrative_ontology:cs_axiom(engineered_closure_reading, foundational, continuous_institutional_work_requirement).
narrative_ontology:cs_axiom_status(continuous_institutional_work_requirement, holdable).
narrative_ontology:cs_axiom(engineered_closure_reading, foundational, beneficiary_visibility_in_policy_defense).
narrative_ontology:cs_axiom_status(beneficiary_visibility_in_policy_defense, holdable).
narrative_ontology:cs_reference_frame(engineered_closure_reading, market_efficiency_necessity).
narrative_ontology:cs_drift_state(engineered_closure_reading, neoliberal_maturation_and_crisis, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(engineered_closure_reading, capital_accumulation_agents).
narrative_ontology:constraint_beneficiary(engineered_closure_reading, rent_extraction_beneficiaries).
narrative_ontology:constraint_victim(engineered_closure_reading, alternative_coordination_possibilities).
narrative_ontology:constraint_victim(engineered_closure_reading, public_institutional_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENCLOSED COMMONS AGENT (SNARE) — Structurally mobile but identity-locked into market-frame assumptions. Believes alternatives are unthinkable ('there is no alternative'). Bears extraction through privatized coordination mechanisms while experiencing the constraint as natural inevitability. Maximum perceived extraction with internalized suppression.
constraint_indexing:constraint_classification(engineered_closure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORM-SEEKING STATE ACTOR (TANGLED ROPE) — Constrained by institutional path-dependency and capital mobility threats ('capital strike'). Experiences genuine coordination function (market prices do transmit information) alongside asymmetric extraction (capital retains veto over public investment). High suppression through threat capacity rather than pure coercion. Sees alternative public coordination models as feasible but costly.
constraint_indexing:constraint_classification(engineered_closure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL DEFENDER (ROPE) — Benefits from market-naturalization frame. Experiences the constraint as pure coordination: price signals enable efficient resource allocation. Active defenders (economists, policymakers, corporate leadership) see their own continuous institutional work (state redesign, regulatory capture, intellectual production) as necessary maintenance of a natural order, not as extraction mechanism.
constraint_indexing:constraint_classification(engineered_closure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL INNOVATION COALITION (SCAFFOLD) — Organized alternatives (cooperative platforms, municipal public banking, degrowth movements, solidarity economies) represent sunset pathway. Sees market naturalization as a temporary institutional arrangement with plausible alternatives. Significant suppression from capital mobility threat, but organized agency and visible exit pathways create lower-extraction experience. Theater ratio is moderate because alternatives have partial institutional legitimacy.
constraint_indexing:constraint_classification(engineered_closure_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NEOLIBERAL CONSENSUS AUTHORITY (PITON) — The institutional apparatus that naturalizes markets (central banks, economics profession, development institutions, corporate boards) increasingly operates as theater. Core coordination function (price signals aggregating dispersed information) is real, but the expansionist logic (privatizing all social coordination) persists through inertia and institutional capture despite mounting evidence of dysfunction. Degraded piton because the primary function has ossified into dogma.
constraint_indexing:constraint_classification(engineered_closure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EFFICIENCY NATURALIST (MOUNTAIN) — From an optimized economic first-principles view, market mechanisms represent the highest-efficiency coordination possible given information dispersal and incentive alignment constraints. No superior alternative exists in principle (Kenneth Arrow, general equilibrium theorem). This perspective sees market naturalization as analytical necessity, not institutional work. FALSE SUMMIT CANDIDATE: beneficiaries visible, institutional maintenance continuous, yet classified as immutable natural law.
constraint_indexing:constraint_classification(engineered_closure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(engineered_closure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(engineered_closure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(engineered_closure_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(engineered_closure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(engineered_closure_reading, TR),
    TR >= 0.70.

:- end_tests(engineered_closure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Market naturalization extracts value through (a) enabling rent extraction via information asymmetry and monopoly pricing, (b) privatizing public goods and services, (c) capturing state capacity for redistribution/public investment. But extraction is not maximal because price signals do provide genuine coordination benefit—some of the extraction appears as improved resource allocation efficiency. The 0.58 value reflects genuine mixed coordination-extraction rather than pure extraction. Suppression (0.68): Moderate-high. Suppression operates through (a) epistemic closure (alternatives labeled impossible/inefficient), (b) material dependence (income security tied to market participation), (c) identity fusion (selfhood constituted through market roles and property claims), (d) capital mobility threat (states that resist market naturalization face investment withdrawal). The 0.68 reflects multiple suppression pathways, not all equally coercive. Theater ratio (0.62): Moderate-high. The performative content comes from (a) economists' insistence that market naturalization is analytical necessity despite contingent institutional history, (b) state redesign framed as 'reform' or 'efficiency' rather than as class-interest protection, (c) alternative coordination possibilities dismissed as naive without institutional examination, (d) continuous policy churn (deregulation cycles, re-regulation, financialization waves) presented as inevitable rather than as beneficiary-driven agenda.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. The capital defender sees pure coordination (Rope)—market prices solve the knowledge problem efficiently; institutional maintenance is merely implementing discovered truth. The reform-seeking state actor sees mixed extraction-coordination (Tangled Rope)—prices do transmit useful information, but capital retains veto power and rents accumulate. The institutional innovation coalition sees a degrading institutional form with viable alternatives (Scaffold)—market naturalization was credible in the 1990s, but platform cooperatives, municipal banking, and climate-transition planning demonstrate alternatives. The powerless agent locked into market frames sees pure extraction (Snare)—their only option is participation in market roles; alternatives appear impossible. The neoliberal consensus authority increasingly operates as theater (Piton)—the core function (price discovery) works, but the expansionist agenda (privatize everything) persists despite dysfunction. The analytical efficiency naturalist risks treating market naturalization as a law of information economics (Mountain)—no superior coordination method exists given asymmetric information and dispersed knowledge. The false summit candidate: beneficiaries are visible (capital accumulation agents), institutional maintenance is continuous and costly, yet the efficiency naturalist perspective classifies as immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim structural positions. Capital accumulation agents hold beneficiary status + arbitrage exit (can relocate if states resist), producing low d (~0.15) → low f(d) → beneficial experience. Reform-seeking state actors are constrained victims (no true exit without sovereignty loss) + moderate power, producing d ~0.60 → moderate f(d) → significant but not maximum perceived extraction. Powerless agents locked into market frames are trapped by identity rather than material barriers (they have structural mobility but cannot see it), producing d ~0.85 → high f(d) → high perceived extraction. The gap between structural and perceived mobility (trapped vs identity_locked exit options) is the diagnostic signal: the constraint is changeable, but identity lock prevents enclosure victims from recognizing changeability. Institutional defenders (economists, central bankers) experience this as pure coordination (Rope) because their professional identity is constituted through market-naturalizing frames; identity_lock runs through them as beneficiaries rather than making them powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The engineered_closure reading resolves mandatrophy by mapping the constraint type to the visibility of institutional work. If market naturalization required NO active institutional maintenance, it would be Mountain or Rope (natural or self-sustaining). But the empirical record shows continuous state redesign—financial deregulation cycles, intellectual property expansion, privatization waves, anti-labor law, tax code restructuring, central bank mandate shifts. This institutional labor is visible to beneficiaries (who conduct it), intelligible to analysts who examine state policy history, and suppressed in the consciousness of enclosure victims who experience markets as natural. The Tangled Rope classification reflects that genuine coordination function exists (markets do solve information aggregation) alongside organized institutional work that captures public goods and suppresses alternatives. The constraint is not a discovered truth; it is an arrangement that requires defending. The mandate—market naturalization as inevitable—would fail without continuous work. Once the work is visible, the classification becomes determinate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point does market-price coordination become indistinguishable from extraction through information asymmetry and power concentration?',
    'Comparative institutional analysis: measure extraction via (a) price-cost margins in competitive vs monopolistic sectors, (b) asymmetric information rents in financial markets, (c) transaction cost ratios for coordinated vs market-mediated functions',
    'If extraction is minimal: market naturalization is near-pure coordination (Rope from many perspectives). If extraction dominates: market naturalization is predominantly extraction mechanism (Snare/Tangled Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary between coordination function and extraction through information asymmetry').

omega_variable(
    state_redesign_necessity,
    'Is continuous state redesign a functional requirement for market coordination, or an institutional work program serving beneficiary interests?',
    'Historical counterfactuals: identify moments of reduced state redesign activity and measure market function degradation vs beneficiary power loss. Examine how market-supporting states (China, South Korea) redesign differently than market-naturalizing states (US, UK) and compare efficiency/stability outcomes.',
    'If state redesign is necessary: the constraint is genuine coordination (Rope) requiring continuous work. If state redesign serves beneficiaries: the constraint is extraction masquerading as coordination maintenance (Tangled Rope/Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_redesign_necessity, empirical, 'Whether state redesign is functional necessity or beneficiary service work').

omega_variable(
    alternative_coordination_viability,
    'Do institutionally mature alternatives (platform cooperatives, municipal public banking, solidarity economies) demonstrate coordination capacity comparable to market mechanisms for complex resource allocation?',
    'Comparative institutional performance analysis: examine scaling limits, information-aggregation capacity, and resilience of cooperative vs market platforms. Test whether ''market naturalization'' reflects genuine coordination superiority or entrenched institutional path-dependency.',
    'If alternatives are viable: market naturalization is contingent institutional closure (Tangled Rope/Piton). If alternatives fail at scale: market naturalization is structural necessity (Rope/Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Viability of institutionally mature non-market coordination alternatives').

omega_variable(
    identity_lock_mechanism_in_powerless,
    'Is the ''TINA'' (There Is No Alternative) frame internalized through epistemic closure, identity fusion with market roles, or material dependence on market-coordinated income?',
    'Ethnographic/cognitive analysis: measure whether enclosure victims can articulate alternatives (epistemic closure), whether stating alternatives triggers identity threat (identity fusion), or whether income dependence creates material constraint. Compare groups with different levels of alternative-awareness.',
    'If epistemic: intervention pathway is consciousness-raising (Scaffold potential). If identity-fused: intervention requires identity reconstruction (deeper Snare). If material: constraint is genuinely trapped rather than identity_locked (classification downward to trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_powerless, empirical, 'Root mechanism of identity_locked TINA frame').

omega_variable(
    reading_contest_locus,
    'Which sibling reading (lapsed_closure vs dual_operation) is this engineered_closure reading actually contesting, and what structural data would resolve the contest?',
    'Examine historical periods where market naturalization appeared to relax (''lapsed'' periods like 1930s-1970s Keynesianism, or current ''dual operation'' periods like state capitalism). For each period, measure: (a) state redesign activity levels, (b) beneficiary visibility in policy debates, (c) institutional maintenance costs, (d) alternative-coordination institutional maturity.',
    'The engineered_closure reading depends on high, continuous, visible institutional work. If institutional work was low during prior periods (supporting lapsed reading), then engineered_closure is a post-1980s phenomenon specific to neoliberal state form. If institutional work has always been high but concealed (supporting engineered_closure as universal), then the reading differs from both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_locus, empirical, 'Locus of contest between sibling readings and historical periodization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(engineered_closure_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(engclos_tr_t0, engineered_closure_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(engclos_tr_t2, engineered_closure_reading, theater_ratio, 2, 0.54).
narrative_ontology:measurement(engclos_tr_t4, engineered_closure_reading, theater_ratio, 4, 0.6).
narrative_ontology:measurement(engclos_tr_t6, engineered_closure_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(engclos_be_t0, engineered_closure_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(engclos_be_t2, engineered_closure_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(engclos_be_t4, engineered_closure_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(engclos_be_t6, engineered_closure_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(engineered_closure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(engineered_closure_reading, 0.18).
narrative_ontology:affects_constraint(engineered_closure_reading, lapsed_closure_reading).
narrative_ontology:affects_constraint(engineered_closure_reading, dual_operation_reading).
narrative_ontology:affects_constraint(engineered_closure_reading, capital_strike_threat).
narrative_ontology:affects_constraint(engineered_closure_reading, epistemic_closure_market_naturalization).

% DUAL FORMULATION NOTE:
% The market_as_natural_default kernel decomposes into three structurally distinct constraint stories, each modeling a different claim about institutional work. The engineered_closure reading models high-visibility, beneficiary-led institutional maintenance (ε=0.58). The lapsed_closure reading models neoliberal-era emergence followed by post-2008 institutional relaxation (different ε trajectory). The dual_operation reading models simultaneous market and non-market logics (different ε basis). Network edges indicate that if engineered_closure is correct (visible institutional work), then the effectiveness of reform movements depends on disrupting that work (affects capital_strike_threat). If lapsed_closure is correct (contingent 1980-2008 imposition), then recovery to mixed-economy models has higher probability. If dual_operation is correct (always-already hybrid), then all three readings partially misframe the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(engineered_closure_reading, institutional, 0.25).
constraint_indexing:directionality_override(engineered_closure_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
