% ============================================================================
% CONSTRAINT STORY: hershey_salt_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hershey_salt_strategy, []).

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
 *   constraint_id: hershey_salt_strategy
 *   human_readable: Hershey's Salt Inclusion Strategy in Confectionery Products
 *   domain: economic/consumer_goods/food_industry
 *
 * SUMMARY:
 *   Hershey's strategy to include salt in more of its product portfolio
 *   represents a classic extraction mechanism masked as product innovation.
 *   Salt enhances flavor contrast and extends shelf life, providing
 *   legitimate technical benefits. However, the strategy simultaneously
 *   exploits biological taste preference vulnerabilities and creates
 *   path-dependency in consumer consumption patterns. The constraint operates
 *   through product formulation choices that narrow the choice set for
 *   health-conscious or sodium-restricted consumers while increasing hedonic
 *   reward for price-sensitive consumers, locking them into higher
 *   consumption frequency. The strategy's effectiveness depends on
 *   suppression: regulatory oversight is weak, consumer information about
 *   salt content is poorly aggregated, and reformulation alternatives are
 *   economically marginalized. The theater_ratio (0.58) reflects the mixed
 *   character: some communication about salt (labeling, marketing emphasis on
 *   taste enhancement) is functional, while much of the health risk
 *   externalization (not advertising sodium content prominently, framing salt
 *   as flavor enhancement rather than behavioral driver) is performative.
 *
 * KEY AGENTS:
 *   - Hershey Corporation: Primary beneficiary (institutional/arbitrage) — captures margin improvement and market share growth through salt-enhanced product differentiation
 *   - Sodium-Sensitive Consumers: Primary victim (powerless/trapped) — face shrinking choice set and health risk as salt-enhanced products proliferate
 *   - Children Developing Preferences: Primary victim (powerless/trapped) — exposed to salt-enhanced sweets during taste preference formation; preferences lock before autonomous choice becomes possible
 *   - Price-Conscious Consumers: Secondary victim (moderate/constrained) — benefit from low-cost confectionery but bear extraction cost through habit formation and health externalization
 *   - Processed Food Industry: Beneficiary (institutional/arbitrage) — salt inclusion strategy is industry-wide; Hershey's approach reinforces category norms and creates competitive pressure to match
 *   - FDA/USDA Regulatory Agencies: Institutional observer (institutional/constrained) — issue guidance but lack enforcement capacity; constraint persists through agency resource scarcity
 *   - Public Health Advocacy: Organized observer (organized/constrained) — can organize and document harms but face industry resource asymmetry and proprietary data barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hershey_salt_strategy, 0.52).
domain_priors:suppression_score(hershey_salt_strategy, 0.68).
domain_priors:theater_ratio(hershey_salt_strategy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hershey_salt_strategy, extractiveness, 0.52).
narrative_ontology:constraint_metric(hershey_salt_strategy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hershey_salt_strategy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hershey_salt_strategy, snare).
narrative_ontology:human_readable(hershey_salt_strategy, "Hershey's Salt Inclusion Strategy in Confectionery Products").
narrative_ontology:topic_domain(hershey_salt_strategy, "economic/consumer_goods/food_industry").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hershey_salt_strategy, hershey_corporation).
narrative_ontology:constraint_beneficiary(hershey_salt_strategy, processed_food_industry).
narrative_ontology:constraint_victim(hershey_salt_strategy, health_conscious_consumers).
narrative_ontology:constraint_victim(hershey_salt_strategy, sodium_sensitive_populations).
narrative_ontology:constraint_victim(hershey_salt_strategy, public_health_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SODIUM-SENSITIVE CONSUMER (SNARE) — Consumers with hypertension or dietary sodium restrictions face narrowing product choice. Salt-enhanced formulations increase reward stimulation, creating habit-forming preferences that lock consumers into higher sodium intake. Exit options are severely limited: most mass-market confectionery now includes salt, and reformulation to low-sodium alternatives requires significant effort and carries social friction. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(hershey_salt_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHILDREN DEVELOPING TASTE PREFERENCES (SNARE) — Early exposure to salt-enhanced sweets during critical taste preference development (ages 2-12) establishes high salt tolerance and preference. Once formed, these preferences persist into adulthood. Children have no exit option and no agency in product selection. Trapped by cognitive development biology. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(hershey_salt_strategy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: HERSHEY CORPORATION (ROPE) — From management perspective, salt inclusion is a coordination mechanism solving legitimate product differentiation and shelf-life challenges. Salt enhances flavor contrast, extends product freshness, and creates competitive product positioning. Hershey experiences this as beneficial coordination: they capture market share and margin improvement through formulation innovation. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(hershey_salt_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRICE-CONSCIOUS CONSUMER (TANGLED ROPE) — Budget-constrained consumers benefit from low-cost confectionery but bear extraction cost of salt-induced consumption patterns. Salt increases hedonic reward, driving repeat purchases and consumption frequency. Consumers experience both coordination (access to affordable treats) and extraction (habit formation and health cost externalization). Exit options are constrained by budget limitations. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(hershey_salt_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FOOD REGULATORY AGENCIES (PITON) — FDA/USDA oversight of salt levels in food is largely performative. Regulatory guidance (like 2016 sodium reduction targets) exists but lacks enforcement mechanism. Agencies lack resources to audit compliance or impose penalties for salt strategy intensification. The theater_ratio=0.58 reflects mixed performative and functional content: agencies issue guidance, companies acknowledge it, but no real constraint on product formulation exists. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(hershey_salt_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC HEALTH ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Health advocates benefit from research on salt-health links (funding, visibility, institutional legitimacy) but bear extraction cost of industry resource asymmetry. They can organize (organized power) but face structural constraints: industry has superior lobbying resources, data on taste preferences is proprietary, and health costs are externalized to society. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(hershey_salt_strategy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational scale, the strategy represents asymmetric extraction: salt use exploits the biological fact that human taste receptors evolved under salt-scarcity conditions. Hershey leverages this ancient adaptation to drive consumption in conditions of modern salt abundance. The extraction is stable and difficult to counter through individual choice because preference formation is biological/developmental. d≈0.85, f(d)≈1.23, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(hershey_salt_strategy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hershey_salt_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hershey_salt_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hershey_salt_strategy, TR),
    TR >= 0.70.

:- end_tests(hershey_salt_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Salt inclusion in confectionery exploits biological taste preference mechanisms and creates consumption lock-in. The extraction is not total (consumers can theoretically switch to unsalted alternatives) but highly effective because: (1) alternatives are economically marginalized, (2) preference formation is developmental/biological, (3) habit formation creates path-dependency. The value increased from 0.28 (early period, limited salt inclusion) to 0.52 (current period, widespread salt formulation) reflecting deliberate strategy intensification. Suppression (0.68): High. Multiple mechanisms suppress alternatives: regulatory guidance lacks enforcement, health information is not prominently disclosed, reformulation alternatives have higher cost/lower availability, and taste preference lock-in is biological. However, suppression is not total (medical information exists, some low-sodium brands persist) and activism creates occasional pressure. Theater ratio (0.58): Moderate-high. Some communication is functional (salt does enhance flavor, shelf-life extension is real). But significant performative content: industry frames salt inclusion as taste innovation rather than behavioral exploitation, health costs are externalized from product marketing, and regulatory theater (guidance without enforcement) maintains appearance of oversight. The theater has increased over time as health communication about salt risks has become more visible — industry response is partly functional adaptation and partly performative CSR messaging.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap divides sharply by power and exit options. Hershey and the food industry see Rope (coordination through flavor innovation). Sodium-sensitive and developing-preference populations see pure Snare (trapped by both preference lock and market narrowing). Price-conscious consumers see Tangled Rope (benefits from affordable products but extraction through habit). Regulatory agencies see Piton (performative oversight). Organized advocates see Tangled Rope (can advocate but face resource asymmetry). The analytical observer sees Snare (biological exploitation). The gap reveals how the same formulation choice is simultaneously product innovation (beneficiary view) and behavioral lock-in (victim view). No single perspective is 'wrong' — they capture different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Hershey: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; experiences the constraint as profitable innovation. Sodium-sensitive consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option. Developing children: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; cognitive biology locks preferences before autonomy. Price-conscious consumers: Victim + constrained (budget limits their options) + secondary beneficiary (afford treats) → d≈0.65, f(d)≈0.95. Mixed extraction; constrained exit reflects budget limitation. Regulatory agencies: Constrained (by resource limits, not active enforcement) → d≈0.50, f(d)≈0.65. Moderate extraction; piton classification comes from theater_ratio gate, not from chi. Public health advocates: Organized but constrained (resource asymmetry) + victim of suppressed alternatives → d≈0.58, f(d)≈0.78. Moderate extraction; organized power reduces d but constraints remain. Analytical observer: Civilizational + global scope amplifies χ via σ=1.2, producing high effective extraction (χ≈0.65).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION STABILITY: The constraint does not resolve into Tangled Rope because the beneficiary (Hershey) does not provide meaningful coordination function — salt is added to increase consumption, not to solve a collective action problem. The extraction mechanism is asymmetric: Hershey captures private benefit (margin, market share) while health costs are externalized to consumers. Active enforcement is not required for the Snare to function — enforcement is implicit in preference lock and market structure. FALSE ROPE TEST: Could this be misclassified as pure Rope (coordination)? No. Rope requires base_extractiveness ≤ 0.45 and χ ≤ 0.35. This constraint has ε=0.52 and effective χ (from most victim perspectives) ≥ 0.65. REFORMULATION FEASIBILITY: The constraint remains Snare even if reformulation is technically feasible, because Hershey has chosen not to reformulate, suppression keeps alternatives marginalized, and the strategy is deliberate rent-seeking. Feasibility does not change classification; it clarifies that the constraint is maintained by choice and structure, not by impossibility. The constraint's stability depends on sustained suppression: if regulatory enforcement were to increase or consumer information were to improve dramatically, the classification might shift toward Tangled Rope (still extractive but with visible alternatives). Currently, suppression is sufficient to maintain pure Snare from victim perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taste_preference_malleability,
    'Are salt-taste preferences genuinely habit-forming/locked, or can consumers re-calibrate sensitivity with effort?',
    'Longitudinal studies of taste preference adaptation; measurement of salt sensitivity recovery in populations shifting to low-sodium diets; genetic vs environmental contribution to salt preference thresholds',
    'If preferences are malleable: exit options improve from ''trapped'' to ''constrained'' → classification shifts from Snare toward Tangled Rope. If locked: Snare classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taste_preference_malleability, empirical, 'Whether salt taste preferences can be re-calibrated or are permanently locked').

omega_variable(
    salt_health_attribution,
    'What proportion of sodium-induced health costs (hypertension, cardiovascular disease) are directly attributable to Hershey''s salt strategy vs broader industrial salt use?',
    'Population attributable risk analysis; dose-response modeling of confectionery salt contribution to total dietary sodium; market share analysis of Hershey vs competitors in categories with high salt inclusion',
    'If Hershey''s contribution is <10% of total salt intake: extraction magnitude is lower than currently assessed (ε might decrease to 0.38). If >25%: extraction is severe and ε might increase to 0.62.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salt_health_attribution, empirical, 'Proportion of sodium health costs attributable to Hershey''s strategy').

omega_variable(
    reformulation_technical_feasibility,
    'Is salt reduction technically feasible in mass-market confectionery without significant cost increase or consumer rejection?',
    'Benchmarking against low-sodium confectionery brands; cost analysis of salt-replacement ingredients; sensory panel testing of reformulated products; market penetration data for existing low-sodium alternatives',
    'If reformulation is feasible: suppression drops from 0.68 to 0.40 → reclassifies toward Tangled Rope (still extractive but with visible alternatives). If not feasible: Snare classification solidifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformulation_technical_feasibility, empirical, 'Technical and economic feasibility of salt reduction in confectionery').

omega_variable(
    regulatory_capture_mechanism,
    'Does Hershey''s lobbying presence prevent regulatory action on salt limits, or do regulatory agencies lack resources independent of industry influence?',
    'Analysis of FDA staff capacity for sodium monitoring; comparison of regulatory stringency in jurisdictions with vs without strong industry lobbying presence; tracking of draft regulations and industry comment responses',
    'If capture is active: suppression increases to 0.80, constraint becomes harder to exit. If agencies lack resources: suppression reflects capacity constraints, not active enforcement of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether regulatory suppression is due to active capture or resource constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hershey_salt_strategy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hershey_salt_tr_t0, hershey_salt_strategy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hershey_salt_tr_t15, hershey_salt_strategy, theater_ratio, 15, 0.48).
narrative_ontology:measurement(hershey_salt_tr_t30, hershey_salt_strategy, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(hershey_salt_be_t0, hershey_salt_strategy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hershey_salt_be_t15, hershey_salt_strategy, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(hershey_salt_be_t30, hershey_salt_strategy, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hershey_salt_strategy, resource_allocation).
narrative_ontology:affects_constraint(hershey_salt_strategy, refined_sugar_consumption_norm).
narrative_ontology:affects_constraint(hershey_salt_strategy, dietary_sodium_externalization).
narrative_ontology:affects_constraint(hershey_salt_strategy, food_industry_regulatory_capture).

% DUAL FORMULATION NOTE:
% Hershey's salt strategy is downstream of broader processed food industry norm-setting around salt inclusion for cost/shelf-life optimization. The upstream constraint (industry standard salt use) has ε≈0.35 (Rope from industry perspective, coordination mechanism). Hershey's strategy intensifies and weaponizes this norm, creating a distinct downstream constraint (ε≈0.52, Snare) where salt inclusion becomes deliberate consumption-driving mechanism rather than incidental technical benefit. The two constraints are linked through industry coordination: Hershey's strategy reinforces norms that make low-sodium formulation a competitive disadvantage across the sector.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hershey_salt_strategy, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
