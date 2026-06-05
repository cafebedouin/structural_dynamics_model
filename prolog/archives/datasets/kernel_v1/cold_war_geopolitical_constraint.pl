% ============================================================================
% CONSTRAINT STORY: cold_war_geopolitical_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_cold_war_geopolitical_constraint, []).

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
 *   constraint_id: cold_war_geopolitical_constraint
 *   human_readable: Cold War Superpower Bipolarity and Nuclear Stalemate (1945-1991)
 *   domain: geopolitical/military/ideological
 *
 * SUMMARY:
 *   The Cold War geopolitical constraint represents a 46-year structural
 *   arrangement (1945-1991) between the Soviet Union and United States that
 *   combined genuine coordination functions (managing postwar order,
 *   preventing direct superpower conflict through deterrence) with severe
 *   extraction (proxy wars consuming developing-world resources, military
 *   spending diverting civilian investment, ideological suppression across
 *   both blocs, and existential risk from nuclear weapons). The constraint
 *   exhibits all six classification types from different observer positions,
 *   making it a diagnostic exemplar for how the same structural phenomenon
 *   can appear as natural law (nuclear deterrence as irreducible fact), pure
 *   coordination (alliance management), mixed coordination-extraction (allied
 *   states), and pure extraction (proxy war victims and domestic dissidents).
 *   The theater ratio increases over the constraint's interval as détente
 *   periods create performative cooperation (arms control agreements signed
 *   but arsenals continue expanding) and proxy wars become increasingly
 *   distant from superpower homelands (creating psychological distance from
 *   extraction mechanisms). The extractiveness trajectory shows acceleration
 *   in the 1960s-1970s (Vietnam, Afghanistan, arms race escalation) and
 *   decline in the 1980s (Soviet economic collapse, US domestic pressure,
 *   reform movements challenging enforcement mechanisms).
 *
 * KEY AGENTS:
 *   - Soviet Leadership: Primary beneficiary (institutional/arbitrage) — designs constraint, captures alliance dominance, ideological legitimacy for regime control
 *   - US Leadership: Primary beneficiary (institutional/arbitrage) — designs constraint, captures alliance dominance, ideological legitimacy for geopolitical hegemony
 *   - Military-Industrial Complex (Both Superpowers): Secondary beneficiary (powerful/arbitrage) — sustained military spending, weapons escalation, technological contracts
 *   - Proxy War States (Vietnam, Afghanistan, Angola, Nicaragua): Primary victim (powerless/trapped) — forced into superpower competition, military occupation, sovereignty stripped, resources extracted
 *   - Civilian Populations (Both Blocs): Secondary victim (moderate/constrained) — subjected to nuclear threat, military budget diversion, ideological suppression, restricted information
 *   - Dissidents Within Blocs (Soviet, Eastern European, US): Secondary victim (moderate/trapped) — criminalized dissent, surveillance, imprisonment, exile
 *   - Allied Satellite States (Poland, East Germany, Japan, West Germany): Mixed (organized/constrained) — experience both security coordination benefits and forced alignment costs
 *   - United Nations System: Degraded institutional actor (institutional/arbitrage) — intended coordination function disabled by superpower veto; persists through inertia
 *   - Anti-Nuclear Movements: Organized opposition (organized/constrained) — suppressed by state action but achieve some legislative wins (test ban treaties)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_war_geopolitical_constraint, 0.58).
domain_priors:suppression_score(cold_war_geopolitical_constraint, 0.72).
domain_priors:theater_ratio(cold_war_geopolitical_constraint, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_war_geopolitical_constraint, tangled_rope).
narrative_ontology:human_readable(cold_war_geopolitical_constraint, "Cold War Superpower Bipolarity and Nuclear Stalemate (1945-1991)").
narrative_ontology:topic_domain(cold_war_geopolitical_constraint, "geopolitical/military/ideological").

domain_priors:requires_active_enforcement(cold_war_geopolitical_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, soviet_leadership).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, us_leadership).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, military_industrial_complex).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, alliance_partners_strategic_value).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, developing_world_proxy_states).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, global_civilian_populations).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, dissidents_within_blocs).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, post_colonial_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROXY STATE / DEVELOPING WORLD (SNARE) — Nations like Vietnam, Afghanistan, Angola, Nicaragua caught in superpower proxy conflicts. No genuine exit option; geopolitical position forces alignment. Experiences maximum extraction: military occupation, resource extraction, ideological imposition, political sovereignty stripped. Cannot exit without catastrophic cost. Powerless to resist superpower competition played on their territory.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS (BOTH BLOCS) (SNARE) — Subjected to nuclear threat, military spending diversion, ideological indoctrination, restricted information access. Exit costs are severe (defection, exile, imprisonment). Suppression is extreme: the nuclear standoff eliminates genuine peace options; military budgets consume resources that could address civilian needs; dissent is criminalized or socially suppressed.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED BLOC MEMBER / SATELLITE STATE (TANGLED ROPE) — Nations like Poland, East Germany (Soviet), or West Germany, Japan (US). Experience both coordination (alliance provides security, economic integration, diplomatic support) and extraction (ideological control, military burden, sovereignty restrictions). Constrained exit: leaving the bloc risks isolation or military intervention, but some negotiating space exists (Sino-Soviet split, NATO flexibility, Yugoslavia's non-alignment). Mixed: genuine coordination benefits exist alongside forced alignment.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPERPOWER LEADERSHIP (ROPE) — US and Soviet elite experience the constraint as coordination: nuclear deterrence defines their relationship, alliance management structures their foreign policy, proxy conflicts provide spheres of influence. Low effective extraction because these actors DESIGN the constraint. They capture primary benefits (geopolitical dominance, resource access through allies, domestic political legitimacy). Can arbitrage away from constraints at will.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Defense contractors, weapons manufacturers, military establishment. The Cold War constraint ENABLES their business model: sustained military spending, weapons development contracts, technological escalation justifies budget allocation. They experience pure coordination — the constraint solves their market problem (ensuring demand for military hardware). Primary beneficiary; can exit or reduce involvement if constraint changes (hence arbitrage status).
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-NUCLEAR / PEACE MOVEMENTS (TANGLED ROPE) — Civil society organizations experience the constraint as both coordination problem (organizing against nuclear weapons) and extraction (suppression of dissent, surveillance, limited political access). Constrained exit: movements can mobilize and advocate but face state repression; still, some win legislative successes (Limited Test Ban Treaty, Non-Proliferation Treaty). Mixed experience of constraint as both legitimating their organizing and suppressing their outcomes.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: UNITED NATIONS (PITON) — The UN's primary coordinating role was intended to prevent great-power conflict, but Security Council structure (Soviet/US veto) renders the institution performative for the Cold War's core dynamic. Theater ratio is high: UN debates, General Assembly votes, and humanitarian interventions proceed while superpower competition determines actual geopolitical outcomes. UN persists through institutional inertia despite functional degradation; called in only when superpowers agree (minor conflicts, humanitarian crises outside bloc competition). Its original coordination function has atrophied.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, nuclear bipolarity appears as an immutable structural fact: two nuclear superpowers with incompatible ideologies cannot coexist without deterrence. The constraint seems irreducible — logically necessary given the initial conditions (Soviet-US confrontation, nuclear technology diffusion, ideological opposition). However, this analysis naturalizes what is contingent: the ideological opposition was constructed; the alliance blocs were politically engineered; the proxy war system was chosen, not inevitable. False summit diagnosis: structural data reveals active enforcement and identified beneficiaries, contradicting natural law claims.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_war_geopolitical_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cold_war_geopolitical_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cold_war_geopolitical_constraint, TR),
    TR >= 0.70.

:- end_tests(cold_war_geopolitical_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Cold War constraint extracts significantly through military spending diversion (each superpower spent 5-10% of GDP on military; global total ~$1-2 trillion annually by 1980s), proxy war casualties (estimated 10-20 million deaths in proxy conflicts), suppression of dissent, and existential risk. However, extractiveness is not at maximum (0.72+) because genuine coordination functions exist: alliance members do receive security benefits, nuclear deterrence does prevent direct superpower conflict, and some coordination of technology and trade occurs. The beneficiaries (superpower elites, military-industrial complex) genuinely benefit, not merely appear to. This mixed function defines tangled_rope territory. Suppression (0.72): High. Extremely severe barriers exist to exiting the constraint: proxy states face military intervention if they try to defect (Hungarian Revolution 1956, Prague Spring 1968); civilian populations face nuclear existential threat and propaganda; dissidents face criminalization and imprisonment; even alliance members have constrained options. Nuclear threat eliminates genuine peace alternatives (choosing neither side means targeting by both). Theater ratio (0.65, increasing to 0.72 mid-period): High and rising. Détente rhetoric (arms control talks, cultural exchange) masks continuing arms escalation. UN debates proceed while real geopolitical decisions happen bilaterally. Proxy wars are fought at distance (Vietnam, Afghanistan) creating psychological separation from the extraction mechanism. Crisis management meetings (Cuban Missile Crisis, back-channel diplomacy) become performative coordination theater with genuine stakes. By the 1980s, Soviet military parades and US defense spending rhetoric become increasingly theatrical as economic reality diverges from official narratives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is extreme. Superpower leadership classifies the constraint as Rope (pure coordination) because their exit options are arbitrage-level (they can end or reshape the constraint at will) and they are net beneficiaries. Proxy war states classify the constraint as Snare (pure extraction) because their exit options are trapped (military intervention follows defection) and they bear the full cost. The gap is not a measurement disagreement — both perspectives are accurate from their structural positions. The gap IS the constraint's reality. An analytical theory that produces the same classification for both perspectives has failed to capture what the constraint actually does: it concentrates benefits among superpower elites and diffuses costs across developing-world civilians and dissidents within blocs. No single type can capture this asymmetry; the full perspectival suite can.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from three structural inputs: (1) the agent's power level (powerless → confined to trapped exit; institutional → access to arbitrage); (2) the agent's exit options (trapped → d ≈ 0.95; arbitrage → d ≈ 0.05); (3) beneficiary or victim status (beneficiary → lower d; victim → higher d). For superpower leadership: institutional power + arbitrage exit + beneficiary status → d ≈ 0.05 → f(d) ≈ -0.12 (negative effective extraction, pure benefit). For proxy war victims: powerless + trapped exit + victim status → d ≈ 0.95 → f(d) ≈ 1.42 (maximum effective extraction). For allied satellite states: organized power + constrained exit + mixed beneficiary/victim status (benefit from security + extraction via political control) → d ≈ 0.50 → f(d) ≈ 0.65 (mixed extraction). The sigmoid function produces nonlinear response: small differences in exit options create large differences in experienced extraction once the victim classification is factored in.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandate mandatrophy: identifying whether the Cold War is a coordination mechanism or pure extraction. The constraint exhibits elements of both: genuine coordination (alliance management, conflict prevention through deterrence) + genuine extraction (proxy wars, military spending diversion, suppression). The tangled_rope classification resolves the mandate by acknowledging both functions are real and not reducible to each other. The false summit signature fires on the mountain perspective: the civilizational observer risks naturalizing the constraint as an immutable law of nuclear deterrence, but the constraint was engineered by elites with identifiable interests and could have been structured differently (e.g., global nuclear disarmament treaties, international peacekeeping, collective security framework). The ocean perspective — seeing the constraint as pure snare masquerading as coordination — is the developing-world view, and it is structurally sound but under-captures the genuine coordination function for alliance members who did benefit from security provision. The complete picture is tangled: mixed coordination-extraction, not reducible to either alone. Mandatrophy resolves when the analysis abandons the single-type framing and embraces the perspectival plurality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideological_opposition_contingency,
    'Is the Soviet-US ideological opposition an irreducible feature of their political systems or a constructed antagonism deployed for domestic and international legitimacy?',
    'Historical analysis of rhetoric vs. policy pragmatism; examination of periods of détente, arms control agreement, and scientific/cultural collaboration to test whether ideological opposition is fundamental or instrumental',
    'If fundamental: the constraint is closer to mountain (ideological systems cannot coexist peacefully). If constructed: the constraint is a snare masquerading as natural law — choice and enforcement, not necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_opposition_contingency, conceptual, 'Whether superpower ideological opposition is inevitable or constructed').

omega_variable(
    proxy_war_inevitability,
    'Was proxy warfare in the developing world a necessary consequence of nuclear deterrence or a choice by superpower elites to compete without direct confrontation?',
    'Counterfactual analysis: comparison of regions where superpowers refrained from proxy competition (Western Europe, some neutrals) vs. those where they engaged; examination of decision points where alternative policies were available but rejected',
    'If necessary: suppression values are lower (unavoidable cost of bipolarity). If choice: suppression values are higher (engineered barrier to local autonomy). Affects victim classification and extraction characterization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_war_inevitability, empirical, 'Whether proxy wars were inevitable consequences of Cold War structure').

omega_variable(
    nuclear_deterrence_credibility,
    'Did mutual assured destruction genuinely stabilize great-power peace or merely create an illusion of stability backed by continuous risk of catastrophic accident?',
    'Analysis of near-miss incidents (Cuban Missile Crisis, false alarm events); comparison of Cold War military incidents vs. pre-nuclear great power periods; examination of strategic instability events that required crisis management',
    'If genuine stability: rope classification becomes stronger (coordination mechanism works). If illusory: snare classification becomes stronger (suppression mechanism is theatrical, unstable, and backed by existential threat rather than real security).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deterrence_credibility, empirical, 'Whether nuclear deterrence provided genuine or illusory stability').

omega_variable(
    alliance_voluntariness,
    'Did alliance members within each bloc genuinely benefit from their alignment or were they coerced into bloc membership by geopolitical pressure?',
    'Analysis of bloc-member exit attempts and superpower response (Hungarian Revolution 1956, Prague Spring 1968, Yugoslavia non-alignment, Polish Solidarity); examination of economic and military aid ratios to measure coercion intensity; study of intra-alliance negotiations and leverage dynamics',
    'If genuinely beneficial: tangled rope classification holds (real coordination function + coercion). If purely coerced: snare classification becomes correct (extraction with no real coordination component).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_voluntariness, empirical, 'Whether bloc membership was voluntary or coerced').

omega_variable(
    extractiveness_measurement_basis,
    'Should extractiveness be measured by military spending diversion, ideological suppression, proxy war casualties, or some weighted combination? Different measurement bases yield different epsilon values.',
    'Establish primary extractiveness metric: (a) % of GDP diverted to military spending above counterfactual peacetime level; (b) civilian casualties in proxy wars; (c) suppression incidents against dissidents; (d) foregone development in proxy states; (e) nuclear risk premium. Each metric yields different epsilon.',
    'Basis (a): ε ≈ 0.42. Basis (b): ε ≈ 0.65. Basis (c): ε ≈ 0.48. Basis (d): ε ≈ 0.72. Basis (e): ε ≈ 0.55 (current). If (b) or (d) is primary: constraint reclassifies to snare. If (a) or (e): tangled_rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_basis, conceptual, 'Which metric properly measures Cold War extractiveness').

omega_variable(
    beneficiary_longevity_mismatch,
    'Why did beneficiary elites not perpetuate the Cold War beyond 1991 if it was so extractive? Gorbachev''s dissolution contradicts pure snare dynamics.',
    'Analysis of internal Soviet collapse dynamics: economic stagnation, military spending unsustainability, technological lag, loss of ideological legitimacy. Comparison with US: military-industrial complex persisted, tried to perpetuate cold war rhetoric post-1991. Why did Gorbachev exit when American beneficiaries did not?',
    'If Soviet exit was forced (economic collapse): constraint was more extractive for USSR than US (asymmetric snare). If exit was chosen (reform initiative): beneficiaries'' rational calculation changed. Affects interpretations of constraint dynamics in final period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_longevity_mismatch, empirical, 'Why the Cold War constraint ended despite beneficiary interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_war_geopolitical_constraint, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cw_tr_t0, cold_war_geopolitical_constraint, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cw_tr_t12, cold_war_geopolitical_constraint, theater_ratio, 12, 0.55).
narrative_ontology:measurement(cw_tr_t23, cold_war_geopolitical_constraint, theater_ratio, 23, 0.68).
narrative_ontology:measurement(cw_tr_t35, cold_war_geopolitical_constraint, theater_ratio, 35, 0.72).
narrative_ontology:measurement(cw_tr_t46, cold_war_geopolitical_constraint, theater_ratio, 46, 0.65).

% Extraction over time
narrative_ontology:measurement(cw_be_t0, cold_war_geopolitical_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cw_be_t12, cold_war_geopolitical_constraint, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(cw_be_t23, cold_war_geopolitical_constraint, base_extractiveness, 23, 0.61).
narrative_ontology:measurement(cw_be_t35, cold_war_geopolitical_constraint, base_extractiveness, 35, 0.65).
narrative_ontology:measurement(cw_be_t46, cold_war_geopolitical_constraint, base_extractiveness, 46, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cw_su_t0, cold_war_geopolitical_constraint, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cw_su_t12, cold_war_geopolitical_constraint, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(cw_su_t23, cold_war_geopolitical_constraint, suppression_requirement, 23, 0.76).
narrative_ontology:measurement(cw_su_t35, cold_war_geopolitical_constraint, suppression_requirement, 35, 0.78).
narrative_ontology:measurement(cw_su_t46, cold_war_geopolitical_constraint, suppression_requirement, 46, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_war_geopolitical_constraint, enforcement_mechanism).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, proxy_war_systems).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, ideological_bloc_formation).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, alliance_dependency_structures).

% DUAL FORMULATION NOTE:
% The Cold War geopolitical constraint decomposes into distinct structural dynamics: (1) nuclear deterrence (preventing direct superpower war) — coordination function with mountain-adjacent stability; (2) proxy war system (competition in third world) — extraction mechanism with snare dynamics; (3) alliance bloc formation (Soviet and US alliance systems) — mixed coordination-extraction with tangled_rope dynamics; (4) ideological opposition (capitalism vs. communism framing) — constructed opposition enabling all other mechanisms. Each sub-constraint has distinct epsilon: nuclear deterrence ε ≈ 0.25 (mostly functional coordination); proxy war system ε ≈ 0.70 (mostly extraction); alliance coordination ε ≈ 0.45 (mixed). The aggregate constraint (0.58) represents a weighted average dominated by proxy war extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cold_war_geopolitical_constraint, institutional, 0.03).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
