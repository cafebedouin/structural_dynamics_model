% ============================================================================
% CONSTRAINT STORY: sotu_1960_eisenhower_foreign_aid_cooperation_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1960_eisenhower_foreign_aid_cooperation_framework, []).

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
 *   constraint_id: sotu_1960_eisenhower_foreign_aid_cooperation_framework
 *   human_readable: U.S. Foreign Aid and Technical Cooperation as Anti-Communist Deterrent (1960)
 *   domain: foreign_policy/development_assistance/geopolitical_strategy
 *
 * SUMMARY:
 *   The 1960 State of the Union context frames foreign aid and technical
 *   cooperation as a strategic deterrent to Soviet influence in uncommitted
 *   nations. The constraint embeds genuine coordination (preventing regional
 *   instability and capital starvation) within an asymmetric extraction
 *   mechanism (conditioning development on geopolitical alignment).
 *   Developing nations receive capital and expertise they structurally need
 *   but cannot access autonomously, creating dependency that translates into
 *   Cold War positioning. Wealthy donor nations bear resource costs but
 *   capture strategic leverage and prevent adversarial alignment. The
 *   constraint's extractiveness increases over the interval as donor nations
 *   recognize and systematize the leverage potential of conditionality,
 *   raising the theater ratio as bureaucratic apparatus formalizes what began
 *   as organic strategic interest. The constraint demonstrates all six DR
 *   types from different perspectives, with the critical gap between the
 *   unaligned nation's Snare experience (trapped, no exit) and the donor
 *   institution's Rope experience (flexible leverage, multiple options).
 *
 * KEY AGENTS:
 *   - Unaligned Developing Nations: Primary victims (powerless/trapped) — structurally dependent on external capital for development; refusal of aid means stagnation; acceptance means subordination to donor geopolitical strategy
 *   - Wealthy Donor Nations (U.S., Western Europe): Primary beneficiaries (institutional/arbitrage) — gain strategic leverage and prevent Soviet alignment; distribute costs across population
 *   - Non-Aligned Movement Coalition: Organized agents (organized/mobile) — seeks to extract benefits from aid competition; has exit options through trade-offs between blocs
 *   - U.S. State Department Strategic Leadership: Beneficiary with control (institutional/arbitrage) — experiences aid as elegant coordination mechanism with strategic conditionality
 *   - Development Assistance Bureaucracy: Intermediate institutional actor (institutional/arbitrage) — maintains procedural apparatus; theater ratio indicates performative development goals masking strategic function
 *   - Donor Nation Populations and Domestic Priorities: Secondary victims (moderate/constrained) — bear resource costs through foregone domestic investment; constrained by Cold War geopolitical consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1960_eisenhower_foreign_aid_cooperation_framework, 0.48).
domain_priors:suppression_score(sotu_1960_eisenhower_foreign_aid_cooperation_framework, 0.52).
domain_priors:theater_ratio(sotu_1960_eisenhower_foreign_aid_cooperation_framework, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1960_eisenhower_foreign_aid_cooperation_framework, extractiveness, 0.48).
narrative_ontology:constraint_metric(sotu_1960_eisenhower_foreign_aid_cooperation_framework, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1960_eisenhower_foreign_aid_cooperation_framework, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1960_eisenhower_foreign_aid_cooperation_framework, tangled_rope).
narrative_ontology:human_readable(sotu_1960_eisenhower_foreign_aid_cooperation_framework, "U.S. Foreign Aid and Technical Cooperation as Anti-Communist Deterrent (1960)").
narrative_ontology:topic_domain(sotu_1960_eisenhower_foreign_aid_cooperation_framework, "foreign_policy/development_assistance/geopolitical_strategy").

domain_priors:requires_active_enforcement(sotu_1960_eisenhower_foreign_aid_cooperation_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1960_eisenhower_foreign_aid_cooperation_framework, wealthy_donor_nations).
narrative_ontology:constraint_beneficiary(sotu_1960_eisenhower_foreign_aid_cooperation_framework, developing_nations_aligned_with_west).
narrative_ontology:constraint_victim(sotu_1960_eisenhower_foreign_aid_cooperation_framework, donor_nation_material_welfare).
narrative_ontology:constraint_victim(sotu_1960_eisenhower_foreign_aid_cooperation_framework, genuine_development_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNALIGNED DEVELOPING NATION (SNARE) — Faces existential material scarcity and no alternative source of capital or expertise. Aid is conditional on alignment (implicit or explicit). Refusal of aid means stagnation; acceptance means subordination to donor nation's geopolitical strategy. Exit options are minimal — cannot invest in infrastructure without external capital, cannot industrialize without technical transfer. Bears full cost of the constraint: loss of development autonomy, entanglement in Cold War proxy dynamics, institutional dependence on donor preferences.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DONOR NATION MIDDLE BUREAUCRACY (TANGLED ROPE) — Constrained by resource allocation and career incentives. Implementing development assistance provides real coordination benefits: technical expertise transfer, infrastructure development, educational opportunity. But also bears extraction: scarce resources diverted from domestic priorities, career advancement dependent on maintaining geopolitical alignments rather than genuine development outcomes. Mixed experience — real benefit to recipient nations coexists with asymmetric extraction of strategic control.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. STATE DEPARTMENT STRATEGIC LEADERSHIP (ROPE) — Primary beneficiary. Experiences foreign aid as elegant coordination mechanism: solving the collective action problem of containing Soviet expansion while enabling development. Retains strategic flexibility through conditionality. Aid becomes leverage — can condition additional assistance on alignment, military cooperation, trade preference. Net benefit outweighs cost for donor institutions. Sees constraint as coordination rather than extraction.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-ALIGNED MOVEMENT COALITION (SCAFFOLD) — Organized developing nations seeking to extract benefits from aid competition without Cold War alignment. Mobile exit option: can choose between U.S., Soviet, or genuinely independent development pathways. Bandung Conference (1955) and NAM founding (1961) frame this as a temporary coordination gap that organized agents can exploit. The constraint is real but sunset logic applies — as developing nations build state capacity and negotiate aid packages from competing blocs, the coercive dimension weakens. Theater moderately high because both donors and recipients performatively claim development goals while pursuing geopolitical positioning.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPMENT ASSISTANCE BUREAUCRACY (DEGRADED PITON) — The institutional apparatus for administering foreign aid (State Department Office of Development, AID precursor agencies) exists primarily to maintain the geopolitical function, not to achieve stated development goals. Theater ratio indicates that aid bureaucracy allocates significant resources to compliance reporting, diplomatic coordination, and strategic alignment verification rather than actual development impact. The machinery persists through institutional inertia and budget appropriation cycle, even when development outcomes are weak or contradicted by strategic priorities.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital scarcity in developing regions creates an immutable structural condition: without external investment, regions lack development capacity; without development, they become unstable and vulnerable to revolutionary movements. The constraint appears as a law of economics and geopolitics — no actor can change these structural facts. However, this naturalizes what is actually a contingent historical arrangement: alternative funding sources (Soviet aid, indigenous capital formation, multilateral development without geopolitical conditionality) exist. The false summit detector will identify this as naturalization of a political choice framed as natural necessity.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1960_eisenhower_foreign_aid_cooperation_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1960_eisenhower_foreign_aid_cooperation_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1960_eisenhower_foreign_aid_cooperation_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1960_eisenhower_foreign_aid_cooperation_framework, TR),
    TR >= 0.70.

:- end_tests(sotu_1960_eisenhower_foreign_aid_cooperation_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, trending upward. Base extractiveness reflects the asymmetry between donor control (conditionality, leverage) and recipient dependence (capital scarcity, structural need). The increase from 0.35 to 0.48 over the interval indicates that donor nations systematize and formalize the extraction potential of aid conditionality once the geopolitical utility becomes clear. Theater ratio (0.58, rising): Moderate. Development assistance is presented as humanitarian coordination (genuine need, genuine benefit) but functions primarily as geopolitical strategy. Theater increases as bureaucratic apparatus formalizes what began as strategic interest — reporting, compliance mechanisms, and 'development impact' metrics become ritualized while actual resource allocation remains driven by Cold War positioning. Suppression (0.52): Moderate. Unaligned nations face real material barriers to autonomy (capital scarcity is structural, not invented) but also face constraint-imposed barriers (aid conditionality, pressure against Soviet engagement). The constraint enhances and institutionalizes natural scarcity into geopolitical dependence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence between beneficiary and victim perspectives. The U.S. State Department sees Rope — an elegant coordination mechanism solving a collective action problem (preventing Soviet expansion while enabling development). Aid becomes strategic leverage with minimal cost to the donor system. Unaligned developing nations see Snare — material need creates structural dependence; aid is contingent on alignment; no exit option exists that doesn't carry catastrophic cost (stagnation). The Non-Aligned Movement sees Scaffold — organized developing nations can exploit aid competition and extract concessions from both blocs; exit options exist for sophisticated actors. The development assistance bureaucracy sees Piton — the apparatus persists through institutional inertia and budget appropriation cycles even when development goals are contradicted by strategic priorities; theater ratio reflects performative development claims. The analytical civilizational observer risks seeing Mountain — development scarcity and geopolitical instability appear as immutable structural facts. But the false summit detector reveals that this naturalizes contingent political choices (the decision to condition aid on alignment, the decision not to establish multilateral unconditional development assistance) as natural necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural position relative to the extraction flow. Unaligned developing nations are full targets (d ≈ 0.95) — they receive aid but lose autonomy; trapped exit options + victim status → maximum experienced extraction (f(d) ≈ 1.42). Donor institutional actors are beneficiaries (d ≈ 0.05) — they gain strategic leverage; beneficiary status + arbitrage exit → negative/minimal experienced extraction (f(d) ≈ -0.12). Non-Aligned Movement organized agents have moderate d (≈ 0.45) — they are partly targets (material need) and partly beneficiaries (can play blocs against each other); mobile exit option + mixed structural position → moderate experienced extraction. Domestic donor populations are secondary targets (d ≈ 0.65) — they bear resource costs but don't control the geopolitical strategy; constrained exit + victim status → moderate-to-high experienced extraction. The widest perspectival gap is between beneficiary institutional actors (who see coordination and leverage) and trapped developing nations (who see extraction with no exit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how the same structural data (capital transfer from wealthy to developing nations) can be classified as pure coordination (Rope) or pure extraction (Snare) depending on the observer's structural position relative to the conditionality mechanism. The donor beneficiary sees coordination: aid solves the problem of preventing Soviet alignment and regional destabilization. The trapped recipient sees extraction: aid is the only available capital source, and accepting it means accepting geopolitical subordination. The organized coalition sees temporary constraint (Scaffold): aid competition creates negotiating leverage, and organized agents can extract benefits without full alignment. The analytical observer risks naturalizing the constraint as Mountain — geopolitical competition and capital scarcity appear inevitable. But the false summit detector identifies this as naturalization of a political choice. The mandatrophy is not resolved by choosing one classification as 'correct' but by recognizing that the constraint's structure legitimately produces all six types from different observational positions. The key insight is that the distribution of types across perspectives reveals the constraint's asymmetry: beneficiaries see low-theater coordination (Rope), victims see high-theater extraction (Snare), organized agents see temporary problems (Scaffold), degraded institutions see inertial performance (Piton), and natural law observers risk seeing immutable facts (Mountain). This perspectival distribution is diagnostic: the constraint is extractive, but only from certain structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_explicitness,
    'Is Cold War alignment required as explicit condition for aid, or is it an implicit assumption that shapes recipient behavior?',
    'Analysis of aid agreements and declassified policy documents; comparison of disbursement patterns correlated with recipient geopolitical alignment vs development need',
    'If explicit: constraint is clearly coercive (higher suppression, higher χ from victim perspectives). If implicit: constraint operates through structural incentives rather than formal coercion (lower measured suppression, higher theater ratio). Classification may shift from Snare toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_explicitness, empirical, 'Whether alignment is explicit aid condition or implicit structural incentive').

omega_variable(
    soviet_aid_effectiveness_parity,
    'Does Soviet aid offer genuine alternative (comparable capital, technical expertise) or is it systematically inferior to Western aid in development outcomes?',
    'Comparative analysis of Soviet vs U.S. aid disbursements; assessment of technical quality and sustainability; recipient nation satisfaction and repeat engagement patterns',
    'If parity: developing nations have real exit option (shift to mobile from trapped); constraint becomes coordination problem rather than pure extraction. If Soviet aid inferior: developing nations face asymmetric choice — acceptance of Western conditionality or inferior development outcomes. Confirms Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_aid_effectiveness_parity, empirical, 'Whether Soviet aid provides comparable alternative or is systematically inferior').

omega_variable(
    development_autonomy_recovery_timeline,
    'Can recipient nations transition from aid dependence to autonomous development capacity, or does aid create structural lock-in preventing self-sufficiency?',
    'Longitudinal analysis of aid recipient nations through 1970s-1990s; measurement of aid dependency ratio over time; correlation between long-term aid receipt and institutional capacity development',
    'If autonomy recovery possible: constraint is temporary (Scaffold sunset logic valid). If lock-in occurs: constraint becomes permanent extraction mechanism (Snare classification confirmed). Theater ratio interpretation changes — high theater indicates performative development claims masking dependency maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_autonomy_recovery_timeline, empirical, 'Whether aid recipients can achieve development autonomy or face structural lock-in').

omega_variable(
    western_donor_resource_cost_allocation,
    'What proportion of aid budget represents genuine cost to donor nations vs. strategic leverage, trade benefit, or domestic constituency service?',
    'Budget analysis: aid disbursements as percentage of donor GDP and government budgets; correlation with tied aid (requirement to purchase donor nation goods/services); comparison with stated development goals vs geopolitical outcomes',
    'If primarily strategic (aid as leverage with minimal cost to donors): constraint benefits donors substantially more than stated development rationale suggests (higher χ for institutional beneficiaries, lower theater). If genuine cost: constraint shows more authentic coordination function (lower theater, more Rope-like from donor perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_donor_resource_cost_allocation, empirical, 'Proportion of aid cost borne by donors vs recovered through strategic leverage and tied aid').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1960_eisenhower_foreign_aid_cooperation_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1960_eisenhower_foreign_aid_cooperation_framework, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu_tr_t5, sotu_1960_eisenhower_foreign_aid_cooperation_framework, theater_ratio, 5, 0.53).
narrative_ontology:measurement(sotu_tr_t10, sotu_1960_eisenhower_foreign_aid_cooperation_framework, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1960_eisenhower_foreign_aid_cooperation_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t5, sotu_1960_eisenhower_foreign_aid_cooperation_framework, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(sotu_be_t10, sotu_1960_eisenhower_foreign_aid_cooperation_framework, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1960_eisenhower_foreign_aid_cooperation_framework, resource_allocation).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_foreign_aid_cooperation_framework, soviet_aid_competition_mechanism).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_foreign_aid_cooperation_framework, non_aligned_movement_power_asymmetry).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_foreign_aid_cooperation_framework, structural_adjustment_conditionality_lock_in).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family covering Cold War development assistance mechanisms. Upstream: ideological competition between Soviet and Western blocs (higher ε, more clearly Tangled Rope/Snare). This story: the institutionalized aid framework as deterrent mechanism (moderate ε, mixed Rope/Snare depending on perspective). Downstream: structural adjustment and debt-trap conditionality that emerges in the 1980s-90s (higher ε, more clearly Snare). The family shows increasing extractiveness over time as conditionality mechanisms are formalized and expanded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1960_eisenhower_foreign_aid_cooperation_framework, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
