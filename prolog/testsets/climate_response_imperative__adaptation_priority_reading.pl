% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Climate Adaptation Imperative (Adaptation-Priority Reading)
 *   domain: climate_policy/intergenerational_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the ADAPTATION-PRIORITY READING of the
 *   contested kernel: climate_response_imperative. The kernel is a stabilized
 *   commitment that all parties (Global North, Global South, scientific
 *   community, finance institutions) accept in some form: climate response is
 *   necessary and urgent. But parties dispute how to structure that response.
 *   This reading claims that present-day adaptation (resilience-building,
 *   damage reduction in exposed regions) is the primary climate response,
 *   with mitigation as aspirational (promised, non-binding, perpetually
 *   delayed). The structural delta is sharp: present-day developing nations
 *   and small-island states enter the victim set via immediate, unaffordable
 *   capital requirements for adaptation infrastructure. Those least
 *   responsible for historical emissions (Global South populations) bear the
 *   highest present costs. This creates a vicious circle: adaptation debt
 *   accumulates; institutional capacity locks into adaptation technology;
 *   future generations inherit both the climate damages and the debt-driven
 *   adaptation pathways. The constraint exhibits Tangled Rope structure
 *   (genuine coordination function — shared interest in reducing damages —
 *   embedded within asymmetric extraction of costs to Global South). But from
 *   the perspective of trapped nations and future generations, it appears as
 *   a Snare. The analytical observer risks naturalizing the constraint as a
 *   mountain (locked-in damages require adaptation as an immutable law) while
 *   missing the false summit: the beneficiaries (Global North capital,
 *   adaptation vendors, financial intermediaries) profit from naturalizing a
 *   contingent political choice as physical necessity.
 *
 * KEY AGENTS:
 *   - Global North Capital Holders: Primary beneficiary (institutional/arbitrage) — adaptation-priority opens new investment markets, concentrates wealth in Northern firms, delays costly domestic mitigation. Captures extraction while appearing to coordinate.
 *   - Present-Day Developing Nations: Primary victim (powerless/trapped) — face immediate unaffordable adaptation capital requirements; cannot refuse climate impacts; no exit available. Maximum extraction.
 *   - Small Island States: Primary victim (moderate/constrained) — existential threat from sea-level rise; constrained exits (managed relocation, unequal climate finance, uncertain litigation). Territorial existence held hostage.
 *   - Adaptation Technology Vendors: Secondary beneficiary (institutional/arbitrage) — supply adaptation goods/services to Global South; extract value through IP, conditional technology transfer, market concentration.
 *   - Climate-Vulnerable Nations Coalition: Organized victim (organized/constrained) — AOSIS, LDC group, African Union pool negotiating power but lack enforcement mechanism; remain constrained even when organized.
 *   - Future Generations Global South: Intergenerational victim (powerless/trapped) — inherit adaptation debt, locked-in maladaptive infrastructure, depleted natural capital, path-dependent adaptation dependency. Trapped across time.
 *   - International Climate Finance Architecture: Institutional actor (institutional/constrained) — Green Climate Fund, development banks, bilateral adaptation finance persist through inertia; largely performative with high institutional overhead.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political choice as immutable climate physics; false summit signature fires when beneficiaries are identified.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.58).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Climate Adaptation Imperative (Adaptation-Priority Reading)").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/intergenerational_justice/political_economy").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'e562ad0a-9fac-4da4-ba76-e83cdaaef8bd').
narrative_ontology:cs_kernel_codification('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', fixed_text).
narrative_ontology:cs_authority_grounding('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', lineage).
narrative_ontology:cs_interpretation_layer_present('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd').
narrative_ontology:cs_reading_relation('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', foundational, locked_in_damages_unavoidable).
narrative_ontology:cs_axiom_status(locked_in_damages_unavoidable, holdable).
narrative_ontology:cs_axiom_grounding('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', locked_in_damages_unavoidable, empirically_contingent).
narrative_ontology:cs_axiom('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', foundational, adaptation_enables_immediate_cost_reduction).
narrative_ontology:cs_axiom_status(adaptation_enables_immediate_cost_reduction, holdable).
narrative_ontology:cs_axiom_grounding('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', adaptation_enables_immediate_cost_reduction, instrumental).
narrative_ontology:cs_created_at('e562ad0a-9fac-4da4-ba76-e83cdaaef8bd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, global_north_capital_holders).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_technology_vendors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, insurance_and_financial_intermediaries).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, present_day_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, small_island_states).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_global_south).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT-DAY DEVELOPING NATION (SNARE) — Faces immediate capital requirements for adaptation infrastructure (flood barriers, drought-resistant agriculture, climate-resilient water systems) that it cannot meet. Global North has delayed mitigation, shifting climate impacts forward; now developing nations must pay adaptation costs immediately while mitigation remains aspirational. No exit: cannot refuse climate impacts, cannot afford necessary adaptation, cannot wait for slow mitigation to reduce future impacts. Maximum extraction — trapped between present climate damages and unaffordable adaptation capital.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL ISLAND STATE (SNARE) — Existential threat from sea-level rise; cannot adapt in-place (islands are geographically determined). Constrained exits: (a) managed relocation with loss of territorial sovereignty and cultural continuity, (b) climate finance negotiated under unequal power dynamics, (c) international climate litigation with uncertain outcomes. High suppression of alternatives — the state's fundamental existence is held hostage to Global North mitigation that is not forthcoming. Theater component: international climate pledges (Paris Agreement, etc.) create appearance of action without delivering mitigation at required pace.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADAPTATION-DEPENDENT NATION (TANGLED ROPE) — Must invest in adaptation; receives some coordination benefit (climate finance, technology transfer, capacity building) while bearing asymmetric extraction (finance is conditional loans, not grants; technology transfer maintains IP barriers; capacity building is donor-driven). The coordination function is genuine — shared interest in reducing climate damages — but extraction is embedded: finance creates debt, technology creates dependency, capacity building imposes policy conditions. Experiences the constraint as both necessary coordination and asymmetric cost-shifting.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL NORTH CAPITAL HOLDERS (ROPE) — Adaptation-priority framing benefits capital: (a) opens new investment markets (climate-resilient infrastructure, green bonds, adaptation technology), (b) concentrates wealth in Global North firms providing adaptation goods/services, (c) shifts costs to developing nations via debt and conditional finance, (d) delays mitigation (which would require structural Northern economic change). Sees the constraint as coordination — helping vulnerable regions adapt — while actually capturing surplus and delaying costly domestic mitigation. Arbitrage exit: can shift to mitigation whenever profitable; can shift to degrowth whenever advantageous.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE FINANCE ARCHITECTURE (PITON) — Green Climate Fund, adaptation-focused development banks, and bilateral adaptation financing are largely performative. Theater elements: (a) pledged adaptation finance is below actual needs by 3-4x, (b) much declared 'adaptation' is relabeled development (would happen anyway), (c) additionality claims are unverifiable, (d) institutional overhead consumes 30-40% of climate finance before reaching beneficiaries. The architecture persists because alternative mechanisms haven't been built, not because it works. Constrained by path dependency — shifting to direct transfers or debt relief would require institutional innovation developing nations cannot force.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE-VULNERABLE NATIONS COALITION (TANGLED ROPE) — Organized voice of Small Island Developing States (AOSIS), Least Developed Countries (LDC group), and African Union positions create genuine coordination function: pooling negotiating power, sharing adaptation strategies, building South-South cooperation. But constraint remains extraction: even organized, these nations cannot compel mitigation or adequate finance from Global North. Coalition constrainment: developed nations can defect from collective agreements (Paris 'pledges' are non-binding); coalition members have no enforcement mechanism.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER CIVILIZATION SCALE (MOUNTAIN AS FALSE SUMMIT) — Civilizational timescale view: climate damages are physical facts (degrees of warming locked in by current emissions); adaptation to locked-in damages is a permanent structural requirement; mitigation cannot undo damages already committed. This framing naturalizes adaptation-priority as an immutable law of climate physics. However, the false summit signature fires: identifiable beneficiaries (Global North capital, adaptation vendors) benefit from naturalizing this reading, and the reading disguises a contingent political choice (delay mitigation, shift costs to Global South) as physical necessity. The ocean levels will rise regardless; but who pays for adaptation and who bears mitigation costs are political decisions, not physical laws.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: FUTURE GENERATIONS GLOBAL SOUTH (SNARE, INTERGENERATIONAL) — Present Global North mitigation delay shifts costs to future generations of vulnerable regions. If adaptation-priority dominates policy for 30 years while mitigation remains aspirational, future generations inherit (a) higher climate damages requiring even larger adaptation investment, (b) debt accumulated from present adaptation finance, (c) depleted natural capital due to present maladaptation (groundwater extraction, ecosystem conversion for resilience), (d) political economy locked into adaptation dependency. Maximum extraction across generations — those least responsible (future Global South populations) bear maximum costs. Trapped across time: cannot refuse inheritance of debt and damages.
constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_imperative__adaptation_priority_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, TR),
    TR >= 0.70.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58): Moderate-high. The adaptation-priority framing enables cost-shifting from Global North to Global South through multiple mechanisms: (1) adaptation is funded as loans, not grants, creating debt for developing nations; (2) technology transfer maintains IP barriers, extracting value; (3) climate finance is conditional on policy choices favoring Northern interests; (4) adaptation investments follow Northern priorities rather than developing-nation needs. The extraction mechanism is partially masked by coordination language (shared interest in climate resilience) but is substantial. The value (0.58 vs v1.0 estimate of 0.72) reflects that some genuine coordination value exists — adaptation investments do reduce some climate damages — but the extraction component is dominant. SUPPRESSION (0.62): High. Barriers to alternative responses are substantial: (1) developing nations cannot credibly refuse adaptation given present climate damages; (2) mitigation requires political-economic change in Global North that Global North opposes; (3) degrowth is dismissed as unrealistic; (4) debt constrains future policy flexibility; (5) climate impacts are ongoing, creating urgency that precludes waiting for slow mitigation. The suppression is structural, not merely perceptual. THEATER_RATIO (0.65): Moderate-high. International climate finance claims (Paris Agreement pledges, Green Climate Fund targets) are substantially performative: pledged adaptation finance is 3-4x below actual needs; much declared 'adaptation' is relabeled development; additionality is unverifiable; institutional overhead is high. But theater is not complete — some real adaptation investment occurs. The ratio has risen over the decade (0.48 → 0.65) as the gap between pledged and delivered finance has widened and as maladaptation risks have become clearer without being addressed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival spectrum. Global North capital holders see Rope — genuine coordination with beneficial outcomes for climate resilience. Present-day developing nations see Snare — immediate unaffordable costs with no exit. Organized nations coalitions see Tangled Rope — some coordination benefits from pooled negotiating power but asymmetric extraction remains. The international climate finance architecture sees Piton — its review and approval processes persist through institutional inertia despite low functional verification and high overhead. Future generations Global South see intergenerational Snare — inheriting debt and maladaptive infrastructure without agency. The analytical observer risks seeing Mountain — naturalizing adaptation necessity as an immutable law of climate physics — but the false summit detector fires because identifiable beneficiaries benefit from this naturalization. The perspectival gaps reveal the structural ambiguity: is adaptation-priority a genuine coordination problem (we all benefit from resilience), or a cover story for cost-shifting (Global North escapes mitigation costs by framing damages as natural rather than caused)?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: beneficiary status, victim status, and exit options. Global North capital holders are beneficiaries with arbitrage options (can shift to mitigation whenever profitable) → low d → negative or low chi despite high base extractiveness. Present-day developing nations are victims with trapped exits (cannot refuse climate impacts, cannot afford adaptation, cannot wait for mitigation) → high d → high chi. Organized nations coalitions are victims with constrained exits (can pool power but cannot enforce mitigation on Global North) → high d but moderated by organization → moderate chi. The international finance architecture is institutionally constrained but benefits from adaptation-priority (institutional interest in continued adaptation finance flows) → moderate d → moderate chi. Future generations Global South are victims with no present agency (trapped across time dimension) → maximum d → maximum experienced chi. The pipeline computes chi = ε × f(d) × σ(S), where f(d) is the sigmoid directionality function and σ(S) is the scope modifier. Regional scope (σ=0.9) dampens extraction slightly from global (σ=1.2) because verification is easier and concentration of power is lower at regional scale. The perspectival gaps in chi explain why the same constraint appears as different types from different positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how the adaptation-priority reading is embedded in a contested kernel with real alternatives. The mandatrophy question is not 'is this a genuine coordination problem or pure extraction?' (the answer is: both, depending on perspective) but 'which reading of climate response is institutionalized?' Under adaptation-priority reading: Global North capital benefits from cost-shifting while maintaining appearance of action (Rope from beneficiary view, Snare from victim view). Under mitigation-priority reading: beneficiaries would shift to technology vendors and energy innovators in Global North; extraction would be framed as necessary innovation costs rather than adaptation debt. Under degrowth reading: beneficiaries would shift to Global South populations and ecosystems; extraction would be reversed (Global North bears costs). The constraint's classification is stable within the adaptation-priority reading but would change dramatically under alternative readings. The mandatrophy resolves by recognizing that the kernel contest IS the constraint — the disagreement about which reading should structure climate response is the irreducible structural ambiguity. The analytical observer cannot adjudicate between readings without declaring normative commitments about who should bear costs and how intergenerational justice should be structured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_specification_adaptation_primary,
    'What operationally distinguishes ''adaptation-priority'' from ''mitigation-adequate-with-residual-adaptation''?',
    'Policy portfolio analysis: ratio of climate finance directed to adaptation vs mitigation; emissions reduction targets vs adaptation investment targets; narrative emphasis in national climate pledges; sequencing of policy implementation (which is funded first, which is delayed).',
    'If adaptation receives >60% of climate finance and mitigation targets are non-binding: adaptation-priority reading confirmed. If mitigation targets are binding and receive >50% of finance: reading collapses into mitigation-priority with adaptation supplement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specification_adaptation_primary, empirical, 'Operational definition: what ratio of finance/policy effort distinguishes adaptation-priority from mitigation-adequate?').

omega_variable(
    contesting_kernel_adaptation_vs_mitigation,
    'This constraint is one reading of a contested kernel. Which other readings of climate response are live alternatives to the adaptation-priority reading?',
    'This is a committer-axis question routed to omega per Rule 2. Sibling readings are: (1) mitigation_priority_reading — climate response is primarily emissions reduction via technological innovation; (2) degrowth_reading — climate response requires structural economic transformation in Global North. Each reading instantiates a different constraint with different beneficiaries, victims, and ε values. This omega documents that the kernel contest is real and structural: parties are not disagreeing about facts (all readings accept that emissions are rising, damages are accumulating), but about the normative political-economic organization that should structure response.',
    'If mitigation-priority or degrowth readings are adopted instead, beneficiary/victim sets shift: Global North capital loses extraction advantage under degrowth; adaptation technology vendors lose markets under mitigation-priority. Classification stability depends on which reading the policy apparatus institutionalizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contesting_kernel_adaptation_vs_mitigation, conceptual, 'Kernel contest: this reading vs sibling readings (mitigation_priority, degrowth) as live alternatives').

omega_variable(
    vicious_circle_lock_in,
    'Does adaptation finance create structural lock-in where developing nations cannot credibly pivot to mitigation or degrowth as policy emphasis?',
    'Path dependency analysis: debt incurred for adaptation; institutional capacity built around adaptation technology; political relationships established with adaptation lenders; opportunity cost of mitigation investment foregone. Counterfactual: what would mitigation-priority or degrowth policy look like for a nation already locked into adaptation debt and capacity?',
    'If lock-in is severe (high confidence): adaptation-priority becomes the structural destiny of Global South for 30+ years regardless of official reading changes. The constraint''s extraction mechanism is not just immediate cost-shifting but long-term path-dependent entrenchment. If lock-in is weak: policy reading changes could redirect finance and capacity toward mitigation or degrowth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vicious_circle_lock_in, empirical, 'Whether adaptation finance creates irreversible path-dependency lock-in').

omega_variable(
    climate_liability_vs_adaptation_mutual_interest,
    'Is adaptation-priority framing based on mutual interest (all parties benefit from reducing climate damages) or on liability denial (Global North escapes responsibility for historical emissions by framing damages as ''natural'' rather than ''caused'')?',
    'Historical causation attribution: which nations'' cumulative emissions caused which regions'' present damages? Counterfactual emissions modeling: what would climate damages be if Global North had mitigation-first since 1990? Does adaptation-priority framing emerge in contexts where Global North mitigation obligation is strongest (vs weakest)?',
    'If mutual interest: adaptation-priority is a genuine coordination problem (Rope at the beneficiary-plus-victim level). If liability denial: adaptation-priority is a cover story for cost-shifting (Snare from developing nation perspective). This omega localizes the false summit problem: which interpretation makes the mountain false?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_liability_vs_adaptation_mutual_interest, conceptual, 'Whether adaptation-priority reflects mutual interest or liability denial').

omega_variable(
    mitigation_aspirationality_mechanism,
    'What structural mechanisms cause mitigation to remain ''aspirational'' (promised, non-binding, perpetually delayed) while adaptation is immediate and mandatory?',
    'Political economy analysis: mitigation requires structural change in Global North (energy transition, growth constraints, redistribution); adaptation allows cost-shifting to Global South via debt and conditional finance. Mitigation has concentrated costs (Northern energy/industrial sectors); adaptation has diffuse benefits (spreading climate damages). Track policy sequencing: when are binding mitigation targets adopted vs when are adaptation targets adopted?',
    'If aspirationality is structural (differential concentrated costs): adaptation-priority is the predicted equilibrium outcome of North-dominated climate politics. If aspirationality is contingent (could change with political will): adaptation-priority reading depends on present power asymmetries and could shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_aspirationality_mechanism, empirical, 'Structural causes of mitigation aspirationality vs adaptation immediacy').

omega_variable(
    adaptation_maladaptation_threshold,
    'At what point does ''adaptation'' become ''maladaptation'' — investing in resilience to a climate state that will not stabilize, locking in practices that worsen long-term outcomes?',
    'Empirical tracking: adaptation strategies (water infrastructure, agricultural practices, settlement patterns) designed for X°C of warming; if actual warming exceeds X°C, are adaptations counterproductive? Historical analysis: did past adaptations to colonial resource scarcity create lock-in that worsened present vulnerability?',
    'If adaptation strategies are locked into conservative (low-warming) scenarios but warming accelerates beyond scenario: adaptation investment becomes sunk cost in failed strategies. This could flip the constraint classification from Tangled Rope to Snare — the coordination function (planning together for climate future) fails because the future is moving faster than adaptation can track. If mitigation had proceeded faster, maladaptation risk would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_maladaptation_threshold, empirical, 'Maladaptation risk: adaptation investments become counterproductive if warming exceeds design assumptions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 2010, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adapt_theater_2010, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(adapt_theater_2015, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.57).
narrative_ontology:measurement(adapt_theater_2020, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(adapt_extract_2010, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(adapt_extract_2015, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(adapt_extract_2020, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(adapt_suppress_2010, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(adapt_suppress_2015, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(adapt_suppress_2020, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_finance_debt_dynamics).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, global_north_energy_transition_barriers).

% DUAL FORMULATION NOTE:
% The adaptation-priority reading is one reading of the contested kernel climate_response_imperative. The network links show the kernel contest structure: (1) affects mitigation-priority and degrowth readings (sibling readings with different beneficiary/victim structures and ε values); (2) affects downstream constraints on climate finance debt and Northern transition barriers (because adaptation-priority delays mitigation and locks in finance structures). The three readings of the kernel should not be combined into one story — they are three structurally distinct constraints with different ε values, beneficiaries, victims, and measurements. The kernel itself is the committer-axis object unifying them; the readings are the structures each reading instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
