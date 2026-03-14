% ============================================================================
% CONSTRAINT STORY: us_hemispheric_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_hemispheric_dominance, []).

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
 *   constraint_id: us_hemispheric_dominance
 *   human_readable: US Hemispheric Dominance: Institutional, Military, and Economic Coordination with Asymmetric Extraction
 *   domain: geopolitical/economic/institutional
 *
 * SUMMARY:
 *   US hemispheric dominance represents a geopolitical constraint that
 *   operates as both coordination mechanism and extractive apparatus,
 *   depending on the observer's structural position. Since the Monroe
 *   Doctrine (1823), the United States has maintained institutional,
 *   military, and economic primacy over Latin American and Caribbean states
 *   through a combination of formal treaties, multilateral institutions (OAS,
 *   IMF, World Bank), military bases and interventions, and
 *   currency/financial system dominance. This constraint exhibits all six
 *   Deferential Realism types from different perspectives, making it a
 *   diagnostic exemplar for geopolitical indexical classification. The same
 *   structural phenomenon — US structural power over the hemisphere — appears
 *   as an immutable law (false mountain), pure coordination (rope from the US
 *   view), mixed coordination-extraction (tangled rope from middle powers),
 *   pure extraction (snare from smaller states), a temporary institutional
 *   arrangement being undermined by alternative infrastructure (scaffold),
 *   and a degraded Cold War ritual (piton) depending on the observer's power
 *   level, time horizon, and exit capacity. The constraint's theater ratio
 *   (0.55) reflects the performative layer: formal hemispheric institutions
 *   (OAS, IADB) invoke coordination language but lack independent enforcement
 *   capacity — real enforcement operates through bilateral pressure,
 *   sanctions, and implicit military threat. Extractiveness (0.58) reflects
 *   that the asymmetry has been gradually tightening: US capacity to impose
 *   policy conditions through financial systems, sanctions regimes, and
 *   institutional gatekeeping has increased while regional exit options
 *   (alternative financing, autonomous institutional development) remained
 *   limited until the emergence of BRICS+ and Belt and Road alternatives in
 *   the 2010s-2020s.
 *
 * KEY AGENTS:
 *   - US Government & Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures geopolitical positioning, resource access, market control, and security advantage throughout the hemisphere with minimal cost
 *   - Smaller Regional States (Central America, Caribbean, small South American states): Primary victims (powerless/trapped) — face structural barriers to institutional autonomy, currency dependence, vulnerability to sanctions and military threat, limited exit options
 *   - Middle Powers (Brazil, Mexico, Colombia): Secondary beneficiaries-victims (powerful/constrained) — benefit from security guarantees and developed-world market access while bearing policy constraints and inability to pursue independent development models
 *   - Regional Coalitions (ALBA, CELAC, PROSUR, Hugo Chavez network): Organized actors (organized/constrained) — have genuine collective agency and alternative institutional frameworks but face suppression through sanctions, exclusion, and US pressure
 *   - Alternative Infrastructure Builders (China, BRICS+, regional financing): Organized agents (organized/mobile) — building exit pathways through alternative financing, trade routing, and institutional development that bypass US dominance
 *   - OAS and Cold War Institutions: Institutional actors (institutional/arbitrage) — maintain formal coordination role but with degraded functional capacity; persist through legitimacy and habit rather than necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement (US regional hegemony) as a law of geography or economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_hemispheric_dominance, 0.58).
domain_priors:suppression_score(us_hemispheric_dominance, 0.68).
domain_priors:theater_ratio(us_hemispheric_dominance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_hemispheric_dominance, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_hemispheric_dominance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_hemispheric_dominance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_hemispheric_dominance, tangled_rope).
narrative_ontology:human_readable(us_hemispheric_dominance, "US Hemispheric Dominance: Institutional, Military, and Economic Coordination with Asymmetric Extraction").
narrative_ontology:topic_domain(us_hemispheric_dominance, "geopolitical/economic/institutional").

domain_priors:requires_active_enforcement(us_hemispheric_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_hemispheric_dominance, us_government).
narrative_ontology:constraint_beneficiary(us_hemispheric_dominance, us_financial_sector).
narrative_ontology:constraint_beneficiary(us_hemispheric_dominance, us_military_industrial_complex).
narrative_ontology:constraint_victim(us_hemispheric_dominance, regional_sovereignty_capacity).
narrative_ontology:constraint_victim(us_hemispheric_dominance, alternative_institutional_development).
narrative_ontology:constraint_victim(us_hemispheric_dominance, non_aligned_movement_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED LATIN AMERICAN STATE (SNARE) — Smaller regional nations face structural barriers to exit: currency dependence on USD, IMF structural adjustment conditions, Cold War legacy military alignments, and loss of institutional capacity from decades of US-backed intervention. Economic coercion through sanctions, IMF lending conditions, and capital flight mechanisms creates suppression >= 0.68. Experienced extraction is maximal — these agents have no genuine alternative institutional pathways.
constraint_indexing:constraint_classification(us_hemispheric_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL COALITION ACTORS (TANGLED ROPE) — Organized regional actors (Venezuela, Cuba, Nicaragua; also newer coalitions like ALBA and PROSUR) benefit from the constraint's coordination function (security guarantees, trade frameworks, financial mechanisms) while bearing asymmetric extraction through sanctions pressure, exclusion from dollar-based financial systems, and constant threat of military intervention. Constrained exit: high cost of defection from regional alignment, but genuine collective agency exists. Chi ~0.70-0.80 depending on coalition military capacity.
constraint_indexing:constraint_classification(us_hemispheric_dominance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US INSTITUTIONAL BENEFICIARY (ROPE) — US government, military, financial sector experience the constraint as pure coordination: the hemisphere's institutional architecture (OAS, IMF, World Bank, NATO extension) enables US strategic interests, capital flows, and military positioning. Net benefit is asymmetric but justified to US actors as coordination mechanism (maintaining hemisphere stability, preventing great-power competition). Arbitrage exit: US can exit specific regional commitments at will (unilateral withdrawal from agreements, sanctions changes) — exit cost is low because US holds structural power.
constraint_indexing:constraint_classification(us_hemispheric_dominance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MIDDLE POWER FENCE-SITTER (TANGLED ROPE) — Brazil, Mexico, Colombia occupy a constrained middle position: large enough to have some institutional capacity and negotiating power, but still dependent on US security guarantees, financial markets, and trade relationships. They benefit from certain coordination functions (security umbrella, developed-world market access) while bearing extraction (policy constraints from US pressure, domestic policy space limitations, inability to build independent financial infrastructure). Chi ~0.50-0.65 — moderate experienced extraction with genuine exit options but substantial costs.
constraint_indexing:constraint_classification(us_hemispheric_dominance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EMERGING ALTERNATIVE INFRASTRUCTURE (SCAFFOLD) — China's Belt and Road Initiative, BRICS financing mechanisms, and Shanghai Cooperation Organization represent organizational actors building exit pathways from US-dominated financial architecture. These agents see US hemispheric dominance as a temporary institutional arrangement with a structural sunset: as alternative financing and trade networks mature, regional actors gain alternatives to dollar-based systems and US-led institutions. Suppression and extraction mechanisms weaken as alternatives proliferate. Theater ratio remains moderate (0.45-0.55) because these alternatives are still nascent.
constraint_indexing:constraint_classification(us_hemispheric_dominance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OAS AND COLD WAR INSTITUTIONAL STRUCTURES (PITON) — The OAS, Inter-American Development Bank, and other Cold War-era regional institutions are substantially performative: they formally embody hemispheric coordination but lack independent capacity to enforce decisions against US preferences. Theater ratio >= 0.55 because these institutions persist through legitimacy and habit, not functional necessity — the constraint's real enforcement mechanisms are bilateral, military, and financial. The institutional layer is maintained through inertia rather than genuine coordination demand.
constraint_indexing:constraint_classification(us_hemispheric_dominance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOGRAPHICAL DETERMINISM (FALSE MOUNTAIN) — From a civilizational perspective, some analysts frame US hemispheric dominance as an immutable consequence of geography and relative economic power: the Americas are naturally within the US sphere of influence; smaller neighbors must accept dependency. This perspective appeals to natural law reasoning: 'regional hegemony is how great powers work.' However, the structural data contradicts this framing. Theater ratio 0.55, suppression 0.68, and active enforcement requirements show this is a contingent institutional arrangement, not a law of nature. The false summit is revealed by the existence of organized alternative infrastructure (perspective 5) and regional coalitions with genuine agency (perspective 2).
constraint_indexing:constraint_classification(us_hemispheric_dominance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_hemispheric_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_hemispheric_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_hemispheric_dominance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_hemispheric_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_hemispheric_dominance, TR),
    TR >= 0.70.

:- end_tests(us_hemispheric_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The US benefits from hemispheric dominance through: strategic military positioning (naval bases, southern command); preferential access to resources (oil, agricultural products, minerals); financial system rents (USD dominance, capital flows); and the ability to impose policy conditions through institutions and threats. However, extractiveness is not at the snare level (0.70+) because genuine coordination benefits exist — security guarantees for smaller states are real (though asymmetrically priced), trade relationships do create mutual benefit, and the institutional infrastructure provides some coordination function. The extractiveness has risen from 0.45 (1990s, post-Cold War optimism about hemispheric integration) to 0.58 (2020s, as US policy constraints have intensified and alternative pathways have emerged). Suppression (0.68): High. Multiple suppression mechanisms: currency dependence on USD creates financial vulnerability; IMF/World Bank structural adjustment conditions constrain policy autonomy; US military threat (credible despite degradation in effectiveness); sanctions regimes against autonomous actors (Venezuela, Cuba, Nicaragua); institutional gatekeeping through OAS and multilateral institutions; and loss of institutional capacity in smaller states due to Cold War legacy interventions. These create genuine barriers to exit for smaller and middle-power regional actors. Theater ratio (0.55): Moderate. The hemispheric dominance regime includes significant performative content: OAS declarations invoke multilateral consensus while real power flows bilaterally; democracy promotion rhetoric masks geopolitical pressure; institutional coordination mechanisms perform legitimacy while enforcement operates through coercive channels. However, the theater is not maximal (>0.70) because military and financial coercion still has substantive effect — real extraction mechanisms operate alongside performative language.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between US beneficiary (Rope) and smaller state victim (Snare) is the maximum possible gap in DR classification — four positions apart on the severity spectrum. This gap reflects real structural difference: the US experiences genuine coordination benefits with minimal cost; the smaller state experiences pure extraction with no alternatives. The gap is not perceptual confusion or measurement error — it is an accurate representation of asymmetric structural positions. The middle-power perspectives (Tangled Rope) sit at the boundary, which is appropriate: they benefit from some coordination functions while bearing significant extraction costs and having real but constrained exit options. The Scaffold perspective (emerging alternative infrastructure) represents a real structural shift: as BRICS+ and Belt and Road mature, regional actors genuinely do gain exit options that degrade US suppression mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural position relative to the extraction flow. US institutional beneficiary with arbitrage exit (d~0.10, f(d)~-0.05): can exit specific commitments at will, captures net benefit, experiences negative effective extraction. Smaller trapped state (d~0.95, f(d)~1.42): has no exit options, bears maximum cost, experiences maximum effective extraction chi. Middle power (d~0.55, f(d)~0.75): mixed position — some exit capacity, mixed benefit/cost relationship, moderate experienced extraction. Regional coalition (d~0.65, f(d)~1.05): significant exit costs (sanctions, exclusion), bears net extraction, but has collective agency (power >= organized) to negotiate some terms. Alternative infrastructure builder (d~0.40, f(d)~0.40): mobile exit options (can build parallel systems), moderate involvement in current constraint, low d produces moderate chi. The directionality pipeline correctly differentiates these positions without overrides because beneficiary/victim declarations and exit_options are sufficiently detailed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends entirely on the agent's structural position, power level, exit options, and time horizon. There is no single 'correct' type — the presheaf over observation sites (the set of all perspectives) is the correct characterization. The US analytical observer's 'coordination' (Rope) is their genuine experience within the constraint. The smaller state's 'extraction' (Snare) is their genuine experience. The regional coalition's 'mixed coordination-extraction' (Tangled Rope) is their genuine experience. The emergence of alternative infrastructure (Scaffold) is a real structural feature that will eventually shift the constraint landscape. The analytical observer's temptation to frame this as natural law (Mountain) is the false summit that mandatrophy analysis is designed to detect: US dominance is not a law of geography or economics, it is a contingent institutional arrangement that depends on suppression mechanisms, financial system dominance, and military credibility. As these mechanisms degrade or are circumvented (by alternative financing, regional coalition building), the constraint will shift toward Scaffold and eventually dissolve. The constraint is real and significant, but not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_hegemony_boundary,
    'Where is the line between genuine hemispheric coordination (collective security, trade benefits) and hegemonic extraction (policy coercion, institutional subordination)?',
    'Longitudinal comparison of policy autonomy across regional actors over time; measurement of benefit concentration (do middle powers and smaller states capture benefits proportional to their institutional contributions?); analysis of exit costs for actors attempting regional autonomous development.',
    'If line favors coordination: US dominance reclassifies as high-extraction Rope from regional perspectives. If line favors extraction: US dominance reclassifies as Snare or Tangled Rope from all regional perspectives. Current value (Tangled Rope) sits at the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_hegemony_boundary, empirical, 'Boundary between genuine coordination and extractive hegemony').

omega_variable(
    alternative_infrastructure_maturation_timeline,
    'What timeline characterizes the maturation of alternative financing and institutional pathways (BRICS+, Belt and Road, regional autonomous institutions)?',
    'Capacity measurement of alternative institutions (financing volume, trade routing, institutional independence from US veto); adoption rates by regional actors; correlation with reduction in US structural leverage.',
    'If maturation accelerates (5-10 years): Scaffold perspective becomes active structural alternative, suppression drops, extraction weakens. If maturation stalls (20+ years): Scaffold is aspirational rather than real, and structural extraction persists longer. Current estimate: 10-15 year sunset timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_maturation_timeline, empirical, 'Timeline for alternative infrastructure to mature and provide viable exit').

omega_variable(
    dollar_system_dependency_reversibility,
    'Is USD currency dependency structurally reversible (can regional actors build independent financial systems) or is it now path-dependent and sticky?',
    'Feasibility analysis of regional financial de-dollarization; cost-benefit modeling of alternative reserve currencies or regional currency systems; empirical test: can major regional actors sustain trade and investment flows using non-USD mechanisms when US pressure is applied?',
    'If reversible: regional actors have genuine mobile exit options, reclassifying from trapped/constrained to constrained/mobile. If sticky: dollar dependency becomes a structural trap, suppression remains high. Current classification assumes partial reversibility (constrained exit, not trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dollar_system_dependency_reversibility, empirical, 'Reversibility of USD currency dependency').

omega_variable(
    military_intervention_threshold_change,
    'Has the credibility of US military intervention as an enforcement mechanism degraded since the Cold War, and if so, how does this affect suppression?',
    'Comparative analysis of intervention success rates and costs in Cold War (1945-1990) vs. post-Cold War period; measurement of regional agents'' belief updates about intervention probability in response to failures (Iraq, Afghanistan); correlation between intervention credibility and regional policy autonomy.',
    'If credibility degraded significantly: suppression should be lower than 0.68 estimate — actors perceive more genuine exit options. If credibility maintained through newer mechanisms (sanctions, financing restrictions): suppression remains high. Current value reflects partial degradation but persistent credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_intervention_threshold_change, empirical, 'Credibility and effectiveness of military intervention as enforcement mechanism').

omega_variable(
    regional_coalition_coalescence,
    'Are regional coalitions (ALBA, CELAC, PROSUR) converging on a unified alternative institutional model or remaining fragmented?',
    'Institutional coherence analysis: do these coalitions have consistent policy positions, binding enforcement mechanisms, and financial capacity? Measurement of membership stability and defection rates. Test: can regional coalitions coordinate sufficient capacity to deter US intervention or enforce counter-sanctions?',
    'If converging: organized perspective reclassifies from constrained to mobile, suppression drops. If fragmenting: organized actors lose collective agency, reverting toward trapped/powerless classification. Current state shows fragmentation with periodic convergence efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_coalition_coalescence, empirical, 'Degree of regional coalition coalescence and institutional binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_hemispheric_dominance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_h_tr_t0, us_hemispheric_dominance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(us_h_tr_t15, us_hemispheric_dominance, theater_ratio, 15, 0.48).
narrative_ontology:measurement(us_h_tr_t30, us_hemispheric_dominance, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(us_h_be_t0, us_hemispheric_dominance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_h_be_t15, us_hemispheric_dominance, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(us_h_be_t30, us_hemispheric_dominance, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_hemispheric_dominance, enforcement_mechanism).
narrative_ontology:affects_constraint(us_hemispheric_dominance, latin_american_institutional_capacity_erosion).
narrative_ontology:affects_constraint(us_hemispheric_dominance, dollar_system_structural_dependence).
narrative_ontology:affects_constraint(us_hemispheric_dominance, regional_military_deterrence_capacity).

% DUAL FORMULATION NOTE:
% US hemispheric dominance is a unified constraint with multiple instantiations: military dominance (enforcement through threat), financial dominance (enforcement through currency/credit systems), and institutional dominance (enforcement through OAS/IMF/World Bank gatekeeping). These could be decomposed into separate stories with different extractiveness values, but the institutional unity (all three channels serve the same geopolitical objective of maintaining US primacy) justifies a single story with multiple perspectives. The three downstream constraints represent structural consequences that feed back into hemispheric dominance: institutional capacity erosion makes regional exit more difficult, dollar dependency makes financial coercion more effective, and military deterrence degradation makes threat-based suppression less credible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_hemispheric_dominance, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
