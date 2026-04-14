% ============================================================================
% CONSTRAINT STORY: us_japan_alliance_security_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_japan_alliance_security_commitment, []).

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
 *   constraint_id: us_japan_alliance_security_commitment
 *   human_readable: US-Japan Alliance Security Commitment
 *   domain: geopolitical/military/alliance_structure
 *
 * SUMMARY:
 *   The US-Japan alliance security commitment, formalized through the 1960
 *   Treaty of Mutual Cooperation and Security and Article 5, creates a hybrid
 *   coordination-extraction structure that has evolved over 65+ years from
 *   Cold War containment framework to contemporary great-power competition
 *   dynamic. The constraint generates different structural experiences for
 *   different agents: Japan experiences security provision (coordination
 *   benefit) alongside strategic dependence (extraction cost); the US
 *   experiences forward military positioning (extraction benefit) alongside
 *   commitment entanglement (strategic cost); and regional actors (China,
 *   Russia) experience deterrent effects that constrain their options. The
 *   constraint exhibits all six classification types from different
 *   structural positions, making it a diagnostic exemplar of
 *   inter-institutional alliance dynamics. The theater_ratio (0.64) reflects
 *   that significant alliance maintenance is performative: joint exercises,
 *   ministerial affirmations, and institutional reaffirmation ceremonies
 *   sustain the commitment while structural conditions (Chinese military
 *   rise, Japanese rearmament, US commitment volatility) shift beneath.
 *   Extractiveness has risen from 0.35 (1985: Cold War clarity) to 0.52
 *   (2025: hegemonic ambiguity) as US primacy erodes and Japan's dependency
 *   becomes costlier for both parties.
 *
 * KEY AGENTS:
 *   - Japanese Strategic Autonomy: Primary victim (powerless/trapped) — locked into US-dependent military posture; cannot independently deter regional threats; constrained from pursuing nuclear option or unilateral rearmament without destabilizing alliance
 *   - Japanese Political Leadership: Secondary institutional actor (powerful/constrained) — experiencing both coordination benefits (deterrence coverage) and extraction costs (foreign policy constraints, military spending dependency); pursuing incremental rearmament (defense spending increases, strike missile acquisition) as exit strategy
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — secures permanent forward-deployed basing, continuous procurement contracts, and demonstrated alliance enforcer capability; generates political-military returns to US strategic position
 *   - US Strategic Leadership: Secondary institutional actor (institutional/constrained) — coordinates regional stability but is entangled: commitment limits freedom of action, ties military resources to alliance maintenance, creates vulnerability to entrapment in regional conflicts
 *   - Regional Security Architecture Reform Coalition: Organized agents (organized/mobile) — Quad framework, trilateral coordination (US-Japan-South Korea), Japanese defense modernization represent alternative pathways that could eventually reduce bilateral dependence
 *   - Cold War Alliance Institutional Inertia: Institutional persistence mechanism (institutional/arbitrage) — alliance maintained through treaty obligation, force structure integration, and domestic constituencies; performative maintenance despite changing strategic rationale
 *   - Analytical Observer: Civilizational analytical perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement (alliance structure) as immutable realist law of Pacific geopolitics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_japan_alliance_security_commitment, 0.52).
domain_priors:suppression_score(us_japan_alliance_security_commitment, 0.58).
domain_priors:theater_ratio(us_japan_alliance_security_commitment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_japan_alliance_security_commitment, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_japan_alliance_security_commitment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_japan_alliance_security_commitment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_japan_alliance_security_commitment, tangled_rope).
narrative_ontology:human_readable(us_japan_alliance_security_commitment, "US-Japan Alliance Security Commitment").
narrative_ontology:topic_domain(us_japan_alliance_security_commitment, "geopolitical/military/alliance_structure").

domain_priors:requires_active_enforcement(us_japan_alliance_security_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_japan_alliance_security_commitment, us_military_strategic_position).
narrative_ontology:constraint_beneficiary(us_japan_alliance_security_commitment, japan_territorial_security).
narrative_ontology:constraint_victim(us_japan_alliance_security_commitment, japanese_military_autonomy).
narrative_ontology:constraint_victim(us_japan_alliance_security_commitment, us_strategic_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JAPANESE STRATEGIC AUTONOMY (SNARE) — Japan's military and foreign policy remain structurally dependent on US security guarantee. Article 5 creates asymmetric vulnerability: US must defend Japan; Japan cannot unilaterally rearm or pursue independent nuclear deterrent without destabilizing regional alliance. Exit is materially impossible — regional great powers (China, Russia) make independent defense prohibitively costly. Maximum extraction from constraint.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JAPANESE POLITICAL LEADERSHIP (TANGLED ROPE) — Experiences genuine coordination benefit (deterrence against regional threats) alongside extraction (constrained foreign policy, limited rearmament autonomy, dependence on US strategic decisions). Can pursue gradual rearmament and normalization (Kishida-era defense spending increases, constitutional reinterpretation) but at high political and economic cost. Constrained exit — not trapped, but mobilizing resources to exit is costly.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Japan's security commitment locks in permanent US forward-deployed presence (7th Fleet homeport, bases at Yokota, Kadena, Sasebo). Commitment generates continuous procurement contracts, strategic basing rights, and demonstrates US ability to enforce alliance structure globally. Extraction runs toward this agent. Arbitrage exit — US can withdraw but would sacrifice regional influence; cost is high enough to sustain commitment.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US STRATEGIC LEADERSHIP (TANGLED ROPE) — Coordinates regional stability (deterrence against Chinese/Russian expansion) but is also extracted from: commitment to defend Japan limits US freedom of action elsewhere, ties military resources to alliance maintenance, and creates vulnerability to entrapment in regional conflicts. Cannot easily exit without ceding Pacific strategic position, but maintains choice through implied threat of withdrawal (see Trump 2016-2020 period). Constrained exit with institutional optionality.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL SECURITY ARCHITECTURE REFORM (SCAFFOLD) — Coalition of Japanese defense hawks, US policy reformers, and regional balancers see the bilateral alliance as temporary backstop. Quad framework (US-Japan-India-Australia), trilateral coordination (US-Japan-South Korea), and Japan's own defense modernization are building alternative security arrangements that could eventually reduce dependence on US bilateral commitment. Sunset logic: as Japan rearmed and regional institutions mature, bilateral security commitment could be superseded by multilateral architecture. High mobility for organized actors; low effective extraction because alternate paths are visible.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLD WAR ALLIANCE INSTITUTIONAL INERTIA (PITON) — The alliance structure persists through institutional momentum despite changing strategic conditions. Originally formed to contain Soviet expansion; now maintained through treaty obligation, force structure integration, and domestic political constituencies. Theater ratio (0.64) reflects that much alliance maintenance is performative: joint exercises, policy affirmations, and institutional reaffirmations that sustain the commitment without addressing whether the strategic rationale still holds. Piton classification indicates degraded functional fit despite continued enforcement.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST NATURAL LAW (MOUNTAIN) — Sees the alliance as an immutable structural feature of Pacific geopolitics. Given the distribution of military power (US hegemony, Chinese rise, Japanese industrial economy but constrained military), some form of security commitment is inherent to stable regional order. This perspective risks naturalizing what is actually a contingent institutional arrangement sustained by political choice and career incentives. The structural data reveals this as a false summit.
constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_japan_alliance_security_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_japan_alliance_security_commitment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_japan_alliance_security_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_japan_alliance_security_commitment, TR),
    TR >= 0.70.

:- end_tests(us_japan_alliance_security_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. The alliance generates genuine coordination benefits (mutual deterrence against regional threats) but extraction is substantial and asymmetric. Japan pays the extraction cost through strategic dependence: cannot independently pursue nuclear deterrent, cannot freely conduct independent foreign policy, must maintain constant readiness for US-triggered defense obligations. US pays through entanglement: commitment constrains freedom of action, ties military resources, creates vulnerability to regional crisis entrapment. The metric reflects that both parties experience extraction, though asymmetrically. Rising extractiveness over the interval (0.35→0.52) reflects Chinese military rise and US relative decline: as Chinese capability approaches regional parity with US, the alliance becomes more extractive because both parties need it more desperately but neither can exit. Suppression (0.58): Moderate-high. Significant barriers prevent either party from exiting: Japan faces regional threat environment (China, Russia) that makes independent security prohibitively costly. US faces strategic costs of Pacific retrenchment that make withdrawal politically difficult despite periodic rhetoric. Regional institutional integration (base locations, integrated command structures, joint military doctrine) raises switching costs. Theater ratio (0.64): Moderately high, rising. Substantial alliance maintenance is performative: joint exercises that don't improve capability significantly, ministerial affirmations that signal commitment without substantive policy change, institutional ceremonies that sustain psychological commitment. Rising theater ratio (0.52→0.64) reflects declining functional fit: as China rises and US commitment credibility decays (Trump-era withdrawal rhetoric, shifting Indo-Pacific policy), alliance performs symbolic function (reassurance) more than actual deterrent function. The theater has increased because structural conditions have made the original functional rationale less credible.
 *
 * PERSPECTIVAL GAP:
 *   The alliance constraint generates maximum perspectival divergence because it is fundamentally inter-institutional with asymmetric structural positions. Japanese strategic autonomy and US military-industrial complex have opposite directionalities: one is maximally extracted from (d ≈ 0.92), the other barely extracted from (d ≈ 0.18). This 0.74 directionality gap produces classification divergence: snare vs rope from the same structural relationship. The political leadership perspectives (Japanese and US) both see tangled_rope because both are institutionally powerful but constrained by the commitment. The scaffold and piton perspectives see the constraint as temporally bounded — either through exit strategy (reform) or through degraded functional fit (inertia). The analytical perspective risks naturalizing what is institutionally contingent. These gaps are not measurement noise — they are diagnostic signals that the constraint is genuinely hybrid (coordination + extraction) with asymmetric incidence. The constraint functions as security provision (rope-like coordination benefit) for both parties but extracts differentially: extraction from Japanese autonomy is structural and severe; extraction from US strategic flexibility is real but mitigated by the benefits of positioning.
 *
 * DIRECTIONALITY LOGIC:
 *   Each institutional perspective's directionality derives from its structural position within the alliance. Japanese strategic autonomy (powerless/trapped) has d ≈ 0.92: experiences maximum extraction because it cannot exit. Japanese political leadership (powerful/constrained) has d ≈ 0.68: experiences high extraction but retains some agency through incremental rearmament. US military-industrial complex (institutional/arbitrage) has d ≈ 0.18: experiences minimal extraction because it is the primary beneficiary. US strategic leadership (institutional/constrained) has d ≈ 0.55: experiences moderate extraction through entanglement costs, but retains option to withdraw (arbitrage asymmetrically benefits military establishment, constrains political leaders). Regional reformers (organized/mobile) have d ≈ 0.42: experience moderate extraction through alliance pressure, but visible exit pathways (Quad, Japanese rearmament) provide mobility. The derived directionality values map to constraint structural asymmetry: Japan trapped (high d), US constrained but optioned (moderate d), beneficiaries low (low d). The beneficiary/victim declarations reflect this: beneficiaries are (US military positioning, Japanese territorial security—both genuine). Victims are (Japanese military autonomy, US strategic flexibility—both extracted from despite coordination benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   The alliance resolves mandatrophy by demonstrating that tangled_rope is the correct analytical classification despite perspectival claims of rope (US beneficiary view) and snare (Japanese autonomy view). The constraint satisfies all three tangled_rope gates: (1) genuine coordination function — mutual deterrence against regional threats is real; (2) asymmetric extraction — Japan extracted from through strategic dependence, US extracted from through entanglement; (3) active enforcement — treaty obligations, integrated command structures, force positioning all require continuous enforcement. The false snare perspective (from Japanese autonomy view) is tempting because extraction appears total, but the constraint provides genuine security benefit that Japanese leadership accepts (constrained rather than trapped classification for political leadership). The false rope perspective (from US military view) is tempting because beneficiary position appears clean, but US strategic leadership experiences genuine entanglement costs that constrain retrenchment. The mandatrophy resolution shows that all six types are defensible perspectival readings, but only tangled_rope integrates both the coordination and extraction dimensions. The piton perspective is correct about degraded functional fit and rising theater — the constraint is becoming increasingly theatrical as US credibility decays and Chinese power rises — but piton misses that the coordination function remains real even as it becomes performative. The scaffold perspective is correct that reform pathways exist (Quad, Japanese rearmament) but premature to classify as terminal — both parties remain locked in because alternatives are not yet sufficient. The mountain perspective is diagnostic failure: treating alliance as immutable law of geopolitics naturalizes a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_direction_ambiguity,
    'Does the alliance primarily extract from Japan (dependency trap) or from the US (entanglement trap)?',
    'Counterfactual cost analysis: cost to Japan of unilateral security provision vs cost to US of strategic retrenchment in Pacific. Temporal analysis: who initiates renewal and under what conditions?',
    'If Japan bears higher cost: snare classification dominates. If US bears higher cost: rope classification with US as net victim. Current assessment: asymmetric — Japan trapped structurally, US constrained strategically. This determines victim designation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, empirical, 'Directional asymmetry of extraction within the alliance').

omega_variable(
    article_five_symmetry_illusion,
    'Is Article 5 genuinely symmetric (mutual defense) or functionally asymmetric (Japan defended, US not)?',
    'Scenario analysis: likelihood of US invoking Article 5 if attacked vs likelihood of Japan invoking it. Historical precedent analysis — have either invoked it? Constraint structure analysis: what would trigger mutual obligation?',
    'If truly symmetric: both agents experience moderate extraction (rope from both perspectives). If asymmetric: Japan experiences snare (defended but dependent), US experiences rope (defender but with limited reciprocal guarantee). Current assessment: functionally asymmetric in peace, potentially symmetric in crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_five_symmetry_illusion, empirical, 'Whether Article 5 provides symmetric or asymmetric mutual defense').

omega_variable(
    japan_rearmament_exit_feasibility,
    'Can Japan realistically transition to independent deterrence (nuclear or conventional) without destabilizing region and violating US preferences?',
    'Policy analysis of Japanese rearmament constraints (constitutional, political, economic, regional reaction). Timeline analysis: how quickly could Japan build independent strike capability? US response modeling: what level of Japanese rearmament triggers withdrawal of US commitment?',
    'If feasible: Japan''s exit options upgrade from trapped to constrained; snare classification becomes tangled_rope. If infeasible: Japan remains trapped; snare classification persists. Current assessment: constrained but slow — Japan is rearming incrementally (2% GDP defense spending, acquisition of strike missiles) but faces regional opposition and US preference for continued dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(japan_rearmament_exit_feasibility, empirical, 'Feasibility of Japanese independent deterrent capability').

omega_variable(
    us_commitment_credibility_decay,
    'Is US security commitment to Japan credible, or does domestic political volatility (Trump-era withdrawal rhetoric, pivot to Indo-Pacific policy uncertainty) undermine deterrent function?',
    'Signal analysis: frequency and content of US policy affirmations. Historical precedent: has US ever actually intervened militarily on behalf of Japan? Constraint structural change: does declining perceived credibility alter extraction dynamics?',
    'If credibility decays: commitment becomes theater (piton classification strengthens). If stable: commitment retains snare/tangled_rope properties. Current assessment: credibility declining since 2016 — extractiveness may remain high but effectiveness of deterrence weakens, suggesting theater_ratio rising over interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_credibility_decay, empirical, 'Credibility of US security commitment credible under regime uncertainty').

omega_variable(
    china_pressure_escalation_path,
    'As China''s military capability approaches US parity, does alliance commitment become more extractive (Japan locked in for deterrence) or less (mutual vulnerability reduces enforceability)?',
    'Military capability modeling: timeline for Chinese military parity with US in Indo-Pacific. Strategic stability analysis: does rough parity stabilize or destabilize alliance structure? Game-theoretic analysis: how do shifting power dynamics affect extraction leverage for each agent?',
    'If China reaches parity: alliance may become more extractive (Japan locked in), or it may dissolve (US cannot enforce, Japan forced to accommodate China). Current assessment: China approaching regional parity; alliance becoming more rather than less extractive because US increasingly needs Japan as counterweight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_pressure_escalation_path, empirical, 'How Chinese military rise affects alliance extraction dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_japan_alliance_security_commitment, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usja_tr_t0, us_japan_alliance_security_commitment, theater_ratio, 0, 0.52).
narrative_ontology:measurement(usja_tr_t20, us_japan_alliance_security_commitment, theater_ratio, 20, 0.58).
narrative_ontology:measurement(usja_tr_t40, us_japan_alliance_security_commitment, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(usja_be_t0, us_japan_alliance_security_commitment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usja_be_t20, us_japan_alliance_security_commitment, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(usja_be_t40, us_japan_alliance_security_commitment, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_japan_alliance_security_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(us_japan_alliance_security_commitment, japanese_military_rearmament).
narrative_ontology:affects_constraint(us_japan_alliance_security_commitment, us_indo_pacific_strategy).
narrative_ontology:affects_constraint(us_japan_alliance_security_commitment, quad_regional_framework).
narrative_ontology:affects_constraint(us_japan_alliance_security_commitment, chinese_military_rise).

% DUAL FORMULATION NOTE:
% The US-Japan alliance security commitment is a distinct constraint from its component elements (Article 5, treaty enforcement, base negotiations, force posture agreements). Each component has its own ε value and structural properties, but the alliance commitment as an integrated system represents a hybrid constraint that cannot be fully decomposed without losing the inter-institutional dynamic. This story captures the alliance as a unified coordination-extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_japan_alliance_security_commitment, powerful, 0.68).
constraint_indexing:directionality_override(us_japan_alliance_security_commitment, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
