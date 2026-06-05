% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__engineered_closure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_engineered_closure, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__engineered_closure_reading
 *   human_readable: Market Naturalization via Engineered Closure and Continuous State Redesign
 *   domain: political_economy/institutional_design/ideology
 *
 * SUMMARY:
 *   Market naturalization is presented in dominant ideology as an inevitable
 *   feature of human nature and economic structure—'there is no alternative,'
 *   in Margaret Thatcher's formulation. This reading instantiates the
 *   engineered_closure perspective: market naturalization is not a sedimented
 *   historical construction that people have simply forgotten, but rather an
 *   active institutional achievement maintained through continuous state
 *   redesign, visible beneficiary defense, and systematic suppression of
 *   alternatives. The constraint exhibits the signature of Tangled Rope:
 *   genuine coordination functions exist (markets do solve some allocation
 *   problems efficiently), but these are fused with asymmetric extraction
 *   where capital concentrators benefit from state structures redesigned to
 *   protect and amplify market mechanisms while suppressing non-market
 *   alternatives. The engineered closure reading makes three empirical
 *   claims: (1) Beneficiaries of market naturalization are visibly organizing
 *   to defend and expand market structures (financial deregulation, labor law
 *   weakening, intellectual property expansion, austerity). (2) Alternatives
 *   to market allocation (cooperatives, commons, mutual aid, public
 *   ownership) are systematically suppressed through legal barriers, resource
 *   denial, and ideological delegitimization. (3) Market naturalization
 *   maintenance requires continuous institutional work—without active
 *   redesign, alternative arrangements would proliferate. The rising
 *   extractiveness and suppression metrics over 1980–2026 capture the
 *   acceleration of this work: neoliberal doctrine has intensified not
 *   because it became more natural over time but because it required
 *   escalating enforcement as alternatives became materially viable and
 *   ideologically credible.
 *
 * KEY AGENTS:
 *   - Capital concentrators (financial sector, rent extraction infrastructure): Institutional beneficiaries with arbitrage exit — visible policy advocates, continuous state redesign.
 *   - Labor precariat: Primary victims (powerless/trapped) — experience market discipline as natural; alternatives suppressed.
 *   - Public goods and commons: Structural victims (powerless/trapped across generations) — systematically enclosed; access mediated by market.
 *   - Reform-minded regulators and policymakers: Moderate/constrained — perceive the constraint but path-dependent; redesigning state in beneficiary-favorable direction is presented as inevitable.
 *   - Organized labor and cooperative movements: Organized/constrained — possess agency and coordination function but face active suppression.
 *   - Emergent post-market institutions: Organized/mobile — platform cooperatives, mutual aid networks, local currencies represent genuine alternatives; scaffold perspective with sunset logic.
 *   - Neoclassical economics discipline: Institutional/arbitrage — produces naturalizing narratives; substantially degraded (piton) but maintains authority through institutional capture.
 *   - Analytical observer: Risk of false-summit classification — market naturalization appears as inevitable law rather than engineered closure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__engineered_closure_reading, 0.58).
domain_priors:suppression_score(market_as_natural_default__engineered_closure_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__engineered_closure_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__engineered_closure_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__engineered_closure_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__engineered_closure_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__engineered_closure_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__engineered_closure_reading, "Market Naturalization via Engineered Closure and Continuous State Redesign").
narrative_ontology:topic_domain(market_as_natural_default__engineered_closure_reading, "political_economy/institutional_design/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__engineered_closure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__engineered_closure_reading, '612095cb-706b-4e43-a7d3-19249ae55c2b').
narrative_ontology:cs_kernel_codification('612095cb-706b-4e43-a7d3-19249ae55c2b', distributed).
narrative_ontology:cs_authority_grounding('612095cb-706b-4e43-a7d3-19249ae55c2b', extraction).
narrative_ontology:cs_reading_relation('612095cb-706b-4e43-a7d3-19249ae55c2b', market_as_natural_default__lapsed_closure_reading, coexists_with).
narrative_ontology:cs_reading_relation('612095cb-706b-4e43-a7d3-19249ae55c2b', market_as_natural_default__dual_operation_reading, influences).
narrative_ontology:cs_axiom('612095cb-706b-4e43-a7d3-19249ae55c2b', foundational, beneficiary_defense_is_visible_and_organized).
narrative_ontology:cs_axiom_status(beneficiary_defense_is_visible_and_organized, holdable).
narrative_ontology:cs_axiom_grounding('612095cb-706b-4e43-a7d3-19249ae55c2b', beneficiary_defense_is_visible_and_organized, empirically_contingent).
narrative_ontology:cs_axiom('612095cb-706b-4e43-a7d3-19249ae55c2b', foundational, state_redesign_is_continuous_and_strategic).
narrative_ontology:cs_axiom_status(state_redesign_is_continuous_and_strategic, holdable).
narrative_ontology:cs_axiom_grounding('612095cb-706b-4e43-a7d3-19249ae55c2b', state_redesign_is_continuous_and_strategic, empirically_contingent).
narrative_ontology:cs_reference_frame('612095cb-706b-4e43-a7d3-19249ae55c2b', market_as_actively_engineered_governance).
narrative_ontology:cs_drift_state('612095cb-706b-4e43-a7d3-19249ae55c2b', contemporary_post_2008, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('612095cb-706b-4e43-a7d3-19249ae55c2b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__engineered_closure_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__engineered_closure_reading, capital_concentrators).
narrative_ontology:constraint_beneficiary(market_as_natural_default__engineered_closure_reading, financial_sector).
narrative_ontology:constraint_beneficiary(market_as_natural_default__engineered_closure_reading, rent_extraction_infrastructure).
narrative_ontology:constraint_victim(market_as_natural_default__engineered_closure_reading, labor_precariat).
narrative_ontology:constraint_victim(market_as_natural_default__engineered_closure_reading, public_goods_commons).
narrative_ontology:constraint_victim(market_as_natural_default__engineered_closure_reading, alternative_economic_arrangements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR PRECARIAT (SNARE) — No exit from market discipline. Wages, benefits, and working conditions are presented as natural market outcomes rather than engineered extraction. The continuous redesign of labor law, contract structures, and benefit regimes happens at institutional level but appears as immutable market forces. Maximum suppression: alternatives (unions, public employment, cooperative organization) are systematically defunded, delegitimized, or criminalized. Victims experience the constraint as unchangeable nature rather than active redesign.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC GOODS AND COMMONS (SNARE, GENERATIONAL) — Environmental commons, public health infrastructure, and knowledge commons are continuously privatized through intellectual property regimes, enclosure doctrine, and public-private partnerships presented as inevitable market solutions. Each generation inherits fewer commons and more market-mediated access to basic goods. Exit is trapped across generations — children cannot exit into commons-based provision because the commons have been systematically enclosed.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REFORM-MINDED REGULATOR (TANGLED ROPE) — Institutional actors within regulatory and policy bodies who perceive the constraint but are constrained by path dependency, capital mobility threats, and normalization of market frames. They benefit marginally from system stability (career continuity, institutional prestige) while bearing costs of cognitive dissonance between their reform goals and the structural necessity to maintain capital-friendly conditions. Medium extraction: some agency to design policy, but exit options (changing fundamental frameworks) are blocked by threat of disinvestment.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL CONCENTRATORS & FINANCIAL SECTOR (ROPE) — Primary beneficiaries. Experience the constraint as coordination mechanism: continuous redesign of contract law, intellectual property regimes, financial regulation, and labor law structures enables capital to extract value from precarity while maintaining the appearance of inevitable market function. No suppression experienced — these agents exercise agency in redesigning state apparatus. Net beneficiary position: extraction flows toward this agent. They see the work as necessary governance, not as exploitative.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED LABOR & ALTERNATIVES (TANGLED ROPE) — Organized groups possess agency and partial coordination function (workplace safety standards, minimum wage frameworks emerged from union pressure) but face structural suppression. They are systematically defunded (union decline), delegitimized ('inefficient,' 'corrupt'), and criminalized (strike restrictions). Benefits exist: negotiated standards do constrain capital extraction at the margin. Costs are severe: the entire ideological apparatus works to naturalize the market as inevitable, making long-term organization difficult. The constraint requires active suppression of these groups' viability.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGENT POST-MARKET INSTITUTIONS (SCAFFOLD) — Cooperative enterprises, platform cooperatives, mutual aid networks, and local currency systems represent genuine alternatives to market naturalization. They see the constraint as temporary—a cultural and institutional legacy of industrial capitalism being actively transcended by generational cohorts with different frames. Low effective extraction for these agents because they maintain exit: defection to cooperative structures, network effects favoring mutual-aid arrangements, and growing ideological legitimacy of commons-based provision. These agents see a sunset as possible—alternative economies have material viability (data-driven cooperative markets, network effects favoring mutual ownership, regenerative agriculture demonstrating viability outside commodity markets).
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: NEOCLASSICAL ECONOMICS (PITON) — The academic discipline that produces the naturalization narrative is substantially degraded: rational-agent assumptions are demonstrably false (behavioral economics), perfect-information assumptions are violated everywhere, equilibrium claims are unstable, and yet the discipline persists as the authoritative voice on market naturalness. Theater_ratio approaches 0.80 at the civilization/universal scope: the ceremonial functions (granting legitimacy to policy, training technocrats to see markets as natural) far exceed explanatory power (predicting actual market behavior). The discipline persists through inertia and institutional capture by financial interests, not through epistemic merit.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — The ideological culmination: market naturalization appears as a law of economics, human nature, or evolutionary necessity rather than as continuous institutional work. This perspective rests on neoclassical naturalizing narratives and undergirds the engineered closure—if markets are natural, then state redesign in their favor is merely 'getting out of the way.' However, the structural data (active beneficiary defense, continuous enforcement, systematic suppression of alternatives) contradicts the mountain classification. The engine's false-summit detector will flag this: beneficiaries are visibly defending, enforcement is active, and alternatives are suppressed—these are hallmarks of constructed, not natural, constraints. The mountain is a rhetorical product, not a structural reality.
constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__engineered_closure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_as_natural_default__engineered_closure_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__engineered_closure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_as_natural_default__engineered_closure_reading, TR),
    TR >= 0.70.

:- end_tests(market_as_natural_default__engineered_closure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting coordinated capital extraction from precarious labor and enclosed commons, but not maximal snare-level extraction because genuine market coordination mechanisms exist and non-market alternatives remain theoretically and empirically available. The value increased from ~0.32 in 1980 to 0.58 in 2026, capturing the acceleration of neoliberal state redesign and financialization. Suppression (0.62): High and rising, reflecting multi-mechanism suppression: legal barriers to cooperative enterprise (tax differentials, regulatory carve-outs), resource denial (union funding collapse, public-sector downsizing), and ideological delegitimization (market efficiency narratives, 'rational actor' ideology). The rise from 0.42 to 0.62 captures intensifying suppression of alternatives as their material viability and ideological appeal increased. Theater (0.48): Moderate and stable, indicating that while market naturalization is ideologically performative, it is maintained by substantial institutional machinery—policy redesign, regulatory enforcement, and beneficiary organization. The theater is lower than pure piton (which would be 0.70+) because institutional work is genuinely consequential: labor law changes do restructure bargaining, intellectual property enforcement does prevent commons, austerity does constrain public provision. The constraint is not merely ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   This reading exhibits stark perspectival gaps. The beneficiary (capital concentrators) sees rope—coordination mechanism enabling efficient allocation, with themselves as net beneficiary. The precariat sees snare—inescapable market discipline presented as natural. The reform regulator sees tangled rope—some agency and coordination functions, but structural suppression of exit. The organized alternative movement sees scaffold—temporary barrier being transcended by generational and technological shifts. The neoclassical discipline sees piton—degraded performative authority persisting through inertia. The civilizational observer risks false summit—seeing market naturalization as immutable law. The gap reveals that the constraint's classification depends entirely on structural position: beneficiaries experience it as coordination; victims experience it as extraction; organized alternatives experience it as temporary obstacle. No single type is correct from all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Capital concentrators (beneficiary + arbitrage exit) derive low d ~0.10, producing negative or near-zero effective extraction chi—they experience the constraint as coordination and benefit. Labor precariat (victim + trapped exit) derive high d ~0.95, producing high f(d) ~1.42, meaning they experience extractiveness at maximum force. Reform-minded regulators (victim + constrained exit, but also benefiting from system stability) derive moderate d ~0.55, producing f(d) ~0.75, capturing their mixed experience of some agency alongside structural suppression. Organized alternatives (victim + mobile exit) derive d ~0.65, producing f(d) ~1.00, capturing their capacity to exit through cooperative organization but also their active suppression. The scope modifier sigma(S) amplifies chi at global scope (σ=1.2)—market naturalization operates at civilizational/universal scale, making verification and exit proportionally harder.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by establishing that market naturalization is Tangled Rope, not pure Snare or pure Rope. It has genuine coordination functions (markets do solve allocation problems; some efficiency gains are real) fused with asymmetric extraction (capital concentrators benefit from state redesign; labor and commons bear costs). The mandatrophy is resolved by showing that the constraint must possess BOTH: (1) genuine coordination (beneficiaries experience it as solving real coordination problems), (2) asymmetric extraction and active enforcement (victims experience suppression; alternatives are systematically blocked). If the constraint had only coordination with no extraction, it would classify as Rope across all perspectives. If it had only extraction with no coordination, it would be Snare. The presence of both—beneficiary defense + victim suppression + genuine efficiency gains—forces Tangled Rope. The false-summit perspective (mountain) is diagnosed as naturalization of contingent arrangements; the piton is degraded authority persisting through capture. The engineered closure reading specifically claims that the coordination and extraction are fused through continuous institutional work, not sedimented ideology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engineered_vs_sedimented_mechanisms,
    'Is market naturalization maintained primarily through active institutional redesign (engineered closure) or through sedimented ideology and forgetting (lapsed closure)?',
    'Trace policy genealogy: do contemporary market-naturalizing institutions show evidence of continuous active defense and redesign, or does the historical record show original construction followed by institutional atrophy and ideological forgetting? Examine: frequency of regulatory intervention, visibility of beneficiary actors in policy formation, presence of counter-pressure requiring active suppression.',
    'If engineered: this reading''s snare/tangled-rope classifications hold; beneficiaries are visible and organizing. If sedimented: lapsed_closure_reading applies; constraint should reclassify with lower suppression and lower theater. If both (dual operation): dual_operation_reading applies; the constraint operates simultaneously as both engineered and sedimented depending on observational level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineered_vs_sedimented_mechanisms, empirical, 'Engineered vs. sedimented maintenance of market naturalization').

omega_variable(
    visibility_of_beneficiaries,
    'Are the beneficiaries of market naturalization visibly organizing to defend the constraint, or are they hidden behind market-naturalizing ideology?',
    'Historical trace of policy advocacy: follow funding flows, lobbying records, regulatory capture evidence, and policy document production around labor law, financial regulation, intellectual property, and austerity frameworks. Document instances where capital concentrated groups visibly intervene to block alternatives, redesign state structures, or defend market frames.',
    'If highly visible: engineered-closure reading is confirmed; suppression and beneficiary organization are empirically detectable. If hidden: the constraint may be sustained by internalized ideology rather than active defense; lapsed_closure_reading becomes more plausible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visibility_of_beneficiaries, empirical, 'Empirical visibility of beneficiary actors defending market naturalization').

omega_variable(
    alternative_arrangements_suppression_mechanism,
    'Are alternatives to market allocation (cooperatives, commons, mutual aid, public ownership) suppressed primarily through legal/institutional prohibition, through defunding and resource denial, or through ideological delegitimization?',
    'Comparative analysis of: legal barriers (carve-outs in cooperative law, intellectual property enforcement against commons), resource barriers (funding differentials between market and non-market enterprises, land access barriers), ideological barriers (media representation, academic dismissal, cultural narratives). Measure the relative weight of each mechanism.',
    'If primarily legal/resource suppression: the constraint requires active enforcement; engineered-closure reading holds. If primarily ideological: lapsed_closure_reading more plausible. If all three mechanisms operate with different weight by region/sector: dual_operation_reading applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_arrangements_suppression_mechanism, empirical, 'Mechanism of suppression against non-market alternatives').

omega_variable(
    state_redesign_continuity,
    'Is contemporary state redesign (austerity, privatization, labor law weakening, intellectual property expansion) driven by coherent beneficiary strategy or by accumulated cultural assumptions about market naturalness?',
    'Genealogical trace of policy doctrine through policy institutions, think tanks, and academic networks. Distinguish: (a) coherent strategic activity by organized capital to reshape state in market-favoring direction, (b) bureaucratic routinization of past decisions now taken as given, (c) genuine ideological consensus that market efficiency is natural. Examine: presence of organized actors issuing policy guidance, frequency and coordination of policy shifts, visibility of dissent within policy communities.',
    'If primarily strategic: engineered closure. If primarily routinized: lapsed closure. If mixed: dual operation with different mechanisms at different institutional scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_redesign_continuity, empirical, 'Whether state redesign is coherent strategy or accumulated tradition').

omega_variable(
    false_summit_natural_law_claim,
    'Is market naturalization a genuine structural inevitability or a constructed constraint defended by beneficiaries and maintained through suppression?',
    'Examine: (1) Are alternatives empirically viable (do cooperatives, commons, and mutual aid arrangements achieve comparable or superior outcomes on relevant dimensions)? (2) Are beneficiary actors visible and organizing to maintain market structures? (3) Is suppression of alternatives active and costly? Affirmative answers on all three suggest false summit rather than natural law.',
    'If false summit confirmed: reclassify from mountain to tangled_rope at analytical level; reveal engineered closure mechanism. The entire naturalization narrative becomes diagnostic evidence of ideological capture rather than truth claim about economic structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether market naturalization is natural law or false-summit false-natural-law claim').

omega_variable(
    reading_contest_dual_operation,
    'This reading (engineered_closure) claims active institutional defense is primary. But does dual_operation_reading accurately describe the actual structure: engineered at institutional level, lapsed at ideological level?',
    'Distinguish observational levels: (a) At the level of policy advocacy and state structure redesign, is there visible organized beneficiary activity? (b) At the level of cultural belief and ideology, do people experience market naturalization as forgotten-construction or as inevitable nature? Dual operation would show engineered defense at institutional level coexisting with lapsed ideology at cultural level.',
    'If only engineered: this reading holds. If dual operation confirmed: all three readings are partially correct at different observational scales; the constraint is better modeled as a family of three linked stories than as a single story with contested readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_dual_operation, conceptual, 'Whether engineered closure is sole mechanism or operates alongside lapsed ideology at different scales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__engineered_closure_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_eng_theater_1980, market_as_natural_default__engineered_closure_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(market_eng_theater_2000, market_as_natural_default__engineered_closure_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(market_eng_theater_2020, market_as_natural_default__engineered_closure_reading, theater_ratio, 2020, 0.48).

% Extraction over time
narrative_ontology:measurement(market_eng_extr_1980, market_as_natural_default__engineered_closure_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(market_eng_extr_2000, market_as_natural_default__engineered_closure_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(market_eng_extr_2020, market_as_natural_default__engineered_closure_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(market_eng_supp_1980, market_as_natural_default__engineered_closure_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(market_eng_supp_2000, market_as_natural_default__engineered_closure_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(market_eng_supp_2020, market_as_natural_default__engineered_closure_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__engineered_closure_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__engineered_closure_reading, labor_precarity_regime).
narrative_ontology:affects_constraint(market_as_natural_default__engineered_closure_reading, intellectual_property_enclosure).
narrative_ontology:affects_constraint(market_as_natural_default__engineered_closure_reading, austerity_fiscal_framework).
narrative_ontology:affects_constraint(market_as_natural_default__engineered_closure_reading, platform_monopoly_naturalization).
narrative_ontology:affects_constraint(market_as_natural_default__engineered_closure_reading, climate_externality_monetization).

% DUAL FORMULATION NOTE:
% This constraint (engineered_closure_reading) is one of three linked readings of the kernel 'market_as_natural_default'. All three readings share the same base properties (extractiveness, suppression, theater) but interpret the maintenance mechanism differently: (1) engineered_closure_reading claims continuous active institutional defense by visible beneficiaries, (2) lapsed_closure_reading claims sedimented ideology with original construction forgotten, (3) dual_operation_reading claims both mechanisms operate at different observational scales. Each reading is a separate constraint story with its own perspectives, omegas, and directionality logic. The three stories form a family linked by kernel kinship, not by network causal relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__engineered_closure_reading, institutional, 0.08).
constraint_indexing:directionality_override(market_as_natural_default__engineered_closure_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
