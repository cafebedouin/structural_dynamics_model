% ============================================================================
% CONSTRAINT STORY: indo_german_defense_pact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_german_defense_pact, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indo_german_defense_pact
 *   human_readable: India-Germany Defense Industrial Partnership
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The India-Germany Defense Industrial Partnership represents a strategic
 *   deepening of bilateral defense cooperation through co-development,
 *   co-production, and technology transfer agreements. India seeks rapid
 *   military modernization to address regional security challenges (Pakistan,
 *   China border tensions) and achieve 'Atmanirbhar Bharat' (self-reliant
 *   India) in defense manufacturing. Germany aims to expand defense market
 *   access, deepen strategic influence in Indo-Pacific, and counter Chinese
 *   defense industry dominance. The partnership creates a structural tension:
 *   genuine coordination benefit (rapid military modernization) coexists with
 *   asymmetric extraction (technology dependency, domestic industry
 *   suppression). German firms retain IP control and supply-chain dominance
 *   while Indian manufacturers are locked into co-production terms that
 *   preserve component-level dependency. The constraint exhibits
 *   characteristics of a Tangled Rope: both actors have incentives to
 *   coordinate (mutual security benefit), but enforcement mechanisms
 *   systematically advantage German firms over Indian domestic industry and
 *   long-term sovereignty. The theater ratio (0.64) reflects gap between
 *   official 'Make in India' self-reliance narratives and actual technology
 *   absorption rates — procurement committees and joint oversight mechanisms
 *   perform strategic partnership without delivering promised capability
 *   transfer.
 *
 * KEY AGENTS:
 *   - German Defense Manufacturers (Siemens, Rheinmetall, MTU, Diehl): Primary beneficiary (institutional/arbitrage) — gain market access, maintain IP control, capture high-margin licensing revenue, have costless exit option
 *   - Indian Military & Defense Ministry (Institutional/constrained): Dual role — benefits from rapid modernization but constrained by path-dependency and technology decisions; bears extraction cost of sovereignty loss
 *   - Indian Domestic Defense Manufacturers (HAL, L&T, BEL, Tata): Primary victim (powerless/trapped) — locked out of high-value co-production components, face suppression of independent R&D through market crowding-out, cannot exit due to government procurement preferences
 *   - Indian Technology Sovereignty (Civilizational/trapped): Abstract victim — represents long-term national capacity for independent defense innovation; locked into import-dependent logistics and supply chains
 *   - German Government / Strategic Actor (Powerful/mobile): Organizes extraction while maintaining coordination function; can rebalance toward other partners if India relationship deteriorates
 *   - Indian Domestic R&D Ecosystem (Organized/constrained): Scaffold perspective — technology transfer potentially enables sunrise of independent capability over 10-15 years if enforcement includes mandatory IP sharing and local firm participation
 *   - Defense Procurement Bureaucracy (Institutional/arbitrage): Piton perspective — formal governance structures (joint committees, charters) create institutional theater masking actual decision concentration in German firms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_german_defense_pact, 0.52).
domain_priors:suppression_score(indo_german_defense_pact, 0.58).
domain_priors:theater_ratio(indo_german_defense_pact, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_german_defense_pact, extractiveness, 0.52).
narrative_ontology:constraint_metric(indo_german_defense_pact, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(indo_german_defense_pact, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_german_defense_pact, tangled_rope).
narrative_ontology:human_readable(indo_german_defense_pact, "India-Germany Defense Industrial Partnership").
narrative_ontology:topic_domain(indo_german_defense_pact, "geopolitical/economic").

domain_priors:requires_active_enforcement(indo_german_defense_pact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, german_defense_manufacturers).
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, indian_military_modernization_agenda).
narrative_ontology:constraint_victim(indo_german_defense_pact, indian_domestic_defense_industry).
narrative_ontology:constraint_victim(indo_german_defense_pact, technology_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN DOMESTIC DEFENSE MANUFACTURERS (SNARE) — Small-to-mid-tier Indian defense firms face technology lock-in and market access barriers. Co-production agreements allocate high-value components to German firms while restricting Indian manufacturers to assembly and subsystems. Exit is blocked by: military strategic focus on German technology, political pressure from bilateral agreements, and lack of capital for independent R&D. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(indo_german_defense_pact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GERMAN DEFENSE MANUFACTURERS (ROPE) — Access to Indian market (1.4B population, rising military expenditure, geographic leverage against China). Co-production agreements position German firms as technology leaders. Technology transfer clauses protect IP while enabling high-margin licensing. Exit is costless: if partnership fails, German firms retain proprietary advantage and can pivot to other markets. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.05. Net coordination benefit — solves market access problem for Germany.
constraint_indexing:constraint_classification(indo_german_defense_pact, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIAN MILITARY & DEFENSE MINISTRY (TANGLED ROPE) — Experiences genuine coordination benefit: rapid access to proven German technology accelerates military modernization against regional threats (Pakistan, China border). But also bears extraction cost: strategic technology decisions become path-dependent on German supply chains, reducing long-term sovereignty over defense R&D. Constrained exit due to geopolitical commitments and sunk military modernization plans. d≈0.58, f(d)≈0.74, σ=1.0 → χ≈0.38. Mixed structure: coordination (military readiness) + asymmetric extraction (technology dependency).
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GERMAN GOVERNMENT / STRATEGIC ACTOR (TANGLED ROPE) — Experiences coordination: deepens EU influence in Indo-Pacific, counters Chinese defense industry dominance, strengthens strategic alliance with India. But enforces asymmetric extraction on Indian side through: IP protection clauses, technology transfer restrictions, co-production terms favoring German lead firms. Mobile exit due to multiple strategic partners; can rebalance toward France, UK, or US partnerships. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.24. Lower effective extraction than manufacturers due to mobility and strategic flexibility.
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIAN TECHNOLOGY SOVEREIGNTY (SNARE) — Abstract institutional actor representing long-term national capacity for independent defense R&D. Co-production agreements lock in dependency on German components, suppliers, and technical standards. This creates path dependency: once Indian military logistics adopt German systems, pivoting to domestic alternatives becomes expensive. Theater performance increases (strategic announcements of 'self-reliance' mask continued import dependency). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. Pure extraction against long-term technological autonomy.
constraint_indexing:constraint_classification(indo_german_defense_pact, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: INDIAN DOMESTIC R&D ECOSYSTEM (SCAFFOLD) — Technology transfer clauses and co-production expertise potentially build Indian manufacturing capacity over 10-15 years. Conditional sunset: if knowledge transfer actually occurs and Indian firms graduate to independent capability, the partnership's extraction mechanism weakens. Current theater_ratio (0.64) reflects gap between 'Make in India' announcements and actual technology absorption. If sunset clause enforced (mandatory IP sharing timelines, local leadership quotas in joint ventures), constraint could degrade to coordination. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(indo_german_defense_pact, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DEFENSE PROCUREMENT BUREAUCRACY (PITON) — Formal joint oversight committees, co-production charters, and strategic dialogue mechanisms create institutional theater. Performance increases (committee meetings, strategic statements) despite stagnant actual technology transfer. theater_ratio=0.64 reflects gap between formal governance structures and real decision-making authority (concentrated in German firms, constrained by Indian military budget cycles). Institutional inertia: procurement committees persist because alternatives (bilateral deals, unilateral purchases) threaten coordination. d≈0.45, f(d)≈0.47, σ=1.0 → χ≈0.30.
constraint_indexing:constraint_classification(indo_german_defense_pact, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global scope, the partnership represents both genuine security coordination (mutual defense against China, regional stability) and asymmetric extraction (technology sovereignty loss). The constraint's classification depends on whether technology transfer actually occurs: if India absorbs manufacturing expertise and graduates to independent production, it resolves toward Rope. If Germany maintains IP control and India remains assembly-dependent, it remains Snare. Current ε=0.52, suppression=0.58 suggest Tangled Rope: real coordination overlay with extraction structure. d≈0.71, f(d)≈1.12, σ=1.2 → χ≈0.58.
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_german_defense_pact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_german_defense_pact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_german_defense_pact, TR),
    TR >= 0.70.

:- end_tests(indo_german_defense_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. German firms extract through: IP control (prevents Indian reverse-engineering), co-production terms allocating high-value components to German partners, technology licensing fees, and maintenance/upgrade dependency. But extraction is not maximal (0.66+) because India retains: military purchase authority, ability to source alternatives from France/Russia/US for some systems, and genuine coordination benefit from rapid modernization. Suppression (0.58): Moderate-high. Indian domestic manufacturers face: procurement preference for co-produced systems, inability to compete on technology (German lead), high capital barriers to independent R&D, talent drain to joint ventures, and government messaging that co-production IS Make in India (suppressing demand for domestic alternatives). Theater ratio (0.64): Moderate-high. Gap between: official commitment to 'self-reliant India' and actual technology transfer; joint committee meetings and strategic statements vs stagnant Indian firm capability growth; co-production charters' technology-sharing clauses vs enforcement mechanisms favoring German firms. Theater has increased from 0.48 to 0.64 over the interval as procurement announcements have outpaced actual capability absorption.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement (India-Germany defense co-production) generates radically different classifications across structural positions. German manufacturers see pure coordination (Rope) — they're solving market access. Indian military sees mixed coordination-extraction (Tangled Rope) — modernization benefit offset by sovereignty cost. Indian domestic firms see pure extraction (Snare) — systematically locked out, suppressed, trapped. Indian technology sovereignty (civilizational victim) sees maximum extraction (Snare) — long-term autonomy sacrificed. Indian R&D ecosystem sees temporary constraint with potential sunset (Scaffold) — IF technology transfer actually occurs, the constraint degrades. Procurement bureaucracy sees degraded ritual (Piton) — governance structures persist through inertia despite low actual decision-making authority. The analytical observer (global scope, civilizational horizon) sees Tangled Rope where coordination and extraction coexist — classification depends on whether technology transfer actually reduces India's exit options over 20 years or enables new capabilities.
 *
 * DIRECTIONALITY LOGIC:
 *   German manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; zero exit cost; negative effective extraction. Indian military: Victim (sovereignty loss) + constrained (geopolitical commitments prevent exit) → d≈0.58, f(d)≈0.74. Moderate extraction; constrained by strategic necessity. Indian domestic firms: Victim (crowded out) + trapped (cannot compete, cannot exit market) → d≈0.88, f(d)≈1.32. High extraction; trapped via procurement structure and competitive disadvantage. Indian technology sovereignty: Victim + trapped (civilizational scope, cannot escape supply-chain lock-in over reasonable timeframe) → d≈0.92, f(d)≈1.38. Maximum extraction; civilizational victim. German government: Beneficiary + mobile (can rebalance toward France/UK partners, can pivot alliance structures) → d≈0.42, f(d)≈0.42. Lower effective extraction than manufacturers due to strategic flexibility. Indian R&D ecosystem: Victim currently + potentially mobile (IF technology transfer succeeds, becomes beneficiary with mobile exit) → d≈0.50, f(d)≈0.65. Mixed current state; classification contingent on enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION RESOLUTION: ε=0.52, suppression=0.58 place this constraint firmly in the 0.40-0.66 band that must balance coordination and extraction claims. The Tangled Rope classification resolves mandatrophy by documenting BOTH coordination benefit (military modernization) and asymmetric extraction (technology dependency, domestic industry suppression). The perspectival gap confirms this: from the German side, it's near-Rope (beneficiary sees coordination); from the Indian domestic industry side, it's near-Snare (victim sees extraction). The shared label (Tangled Rope) captures that BOTH readings are structurally accurate — the constraint genuinely serves coordination functions while enforcing asymmetric extraction. Mandatrophy avoided by: (1) explicitly identifying beneficiaries (German manufacturers, Indian military modernization) to confirm coordination exists; (2) explicitly identifying victims (Indian domestic firms, technology sovereignty) to confirm extraction exists; (3) declaring requires_active_enforcement: true to show that the extraction requires constant enforcement of IP restrictions and co-production terms; (4) proposing Scaffold sunset clause contingent on actual technology transfer, which would validate that the constraint could resolve toward Rope if the stated coordination function actually delivers. The false summit risk here is naturalizing the dependency as inevitable (Mountain perspective: 'modern defense requires German expertise'). The theater ratio (0.64) and measurement trajectory (increasing from 0.48) suggests this mountain view is increasingly performative — India's 'self-reliance' rhetoric masks continued import dependency, and procurement announcements (theater) outpace actual capability transfer (function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_efficacy,
    'Will co-production and licensing agreements actually transfer manufacturing capability to Indian firms, or will they lock India into permanent component-assembly status?',
    'Historical analysis of technology transfer outcomes in defense partnerships (US-Israel, France-India past deals); tracking of Indian firm capability progression across 10-year interval; assessment of whether Indian firms can independently produce next-generation variants without German input',
    'If transfer succeeds: constraint degrades from Snare toward Rope (domestic manufacturers gain capability). If transfer fails: classification confirmed as Snare (technology lock-in permanent). If partial: remains Tangled Rope with shifting χ values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_efficacy, empirical, 'Whether co-production enables genuine Indian manufacturing autonomy or creates permanent dependency').

omega_variable(
    strategic_autonomy_cost,
    'Does military modernization via German co-production trade away strategic decision-making autonomy that India cannot recover in 20-year timeframe?',
    'Counterfactual analysis: India''s military modernization trajectory under domestic R&D vs German partnership; assessment of switching costs (logistics lock-in, supply chain dependencies, maintenance requirements); modeling of geopolitical constraints if India-Germany relations deteriorate',
    'If autonomy cost is recoverable: extraction is medium-term (Scaffold perspective valid). If autonomy cost is structural: extraction is civilizational (Snare perspective valid). This shapes whether the sunset clause mechanism works.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_autonomy_cost, conceptual, 'Whether strategic dependency created is reversible within India''s decision-making horizon').

omega_variable(
    domestic_industry_crowding_out,
    'Does German partnership suppress development of Indian domestic defense industry through market capture, talent drain to foreign firms, and procurement preference for co-produced systems?',
    'Comparative analysis of Indian private defense R&D funding and patent output before/after partnership; tracking of Indian defense engineer recruitment into German joint ventures; assessment of procurement budgets allocated to co-production vs domestic contracting',
    'If crowding-out is severe: victims (domestic manufacturers) are trapped Snare agents. If domestic industry grows alongside partnership: Rope or Scaffold classification confirmed. This determines whether suppression (0.58) understates or overstates structural coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_industry_crowding_out, empirical, 'Whether co-production partnership systematically crowds out domestic defense industry development').

omega_variable(
    geopolitical_contingency,
    'If India-Germany relations deteriorate (EU sanctions pressure, NATO realignment, trade disputes), can India exit the defense partnership without catastrophic military capability loss?',
    'Scenario modeling of relationship deterioration and military capability dependencies; assessment of alternative supply chains India could establish in 2-5 year timeframe; analysis of stockpiling and logistics redundancy built into co-production agreements',
    'If exit is catastrophic: India''s exit_options are truly trapped (Snare confirmed). If India can pivot: exit_options are constrained (Tangled Rope confirmed). This determines whether suppression reflects structural impossibility or negotiated constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_contingency, empirical, 'Whether India can exit partnership without military capability loss if geopolitical conditions change').

omega_variable(
    manufacturing_complexity_floor,
    'Are modern defense systems (missiles, radars, fire control) technically complex enough that Indian firms cannot master independent production without sustained German support, regardless of knowledge transfer?',
    'Technical capability assessment of Indian defense firms in similar technology domains (satellites, naval systems); comparison to other countries'' technology absorption timelines (South Korea, Japan, Taiwan in past decades); expert analysis of skill requirements vs Indian engineering training pipeline',
    'If complexity floor is high: even well-intentioned technology transfer cannot enable Indian autonomy (structural Snare, not policy choice). If complexity is surmountable: dependency is contingent on enforcement terms (Tangled Rope, potentially Scaffold with right incentives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manufacturing_complexity_floor, empirical, 'Whether manufacturing complexity of modern defense systems makes Indian autonomy technologically impossible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_german_defense_pact, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igdp_tr_t0, indo_german_defense_pact, theater_ratio, 0, 0.48).
narrative_ontology:measurement(igdp_tr_t5, indo_german_defense_pact, theater_ratio, 5, 0.58).
narrative_ontology:measurement(igdp_tr_t10, indo_german_defense_pact, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(igdp_be_t0, indo_german_defense_pact, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(igdp_be_t5, indo_german_defense_pact, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(igdp_be_t10, indo_german_defense_pact, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_german_defense_pact, resource_allocation).
narrative_ontology:affects_constraint(indo_german_defense_pact, indian_domestic_defense_industry_viability).
narrative_ontology:affects_constraint(indo_german_defense_pact, eu_indo_pacific_strategic_positioning).
narrative_ontology:affects_constraint(indo_german_defense_pact, global_defense_supply_chain_fragmentation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of India's broader military modernization strategy (upstream: Indian military modernization agenda) and upstream of Indian domestic defense industry structure (downstream: domestic firm viability). The partnership represents a specific enforcement mechanism for technology-dependent modernization; upstream constraints include China regional threat perception and Indian defense budget allocation. Downstream constraints include HAL/L&T competitive positioning and long-term Indian R&D autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_german_defense_pact, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
