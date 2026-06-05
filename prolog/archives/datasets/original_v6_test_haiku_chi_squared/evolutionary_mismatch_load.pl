% ============================================================================
% CONSTRAINT STORY: evolutionary_mismatch_load
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_mismatch_load, []).

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
 *   constraint_id: evolutionary_mismatch_load
 *   human_readable: The Paleolithic Circuit Break: Evolutionary Mismatch Load
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   The Paleolithic Circuit Break describes the structural friction between
 *   human biological traits optimized for ancestral environments and the
 *   hypermediated, abundance-engineered modern landscape. This is not merely
 *   a biological problem — it is an extractive constraint that has been
 *   actively engineered into commercial infrastructure. Human reward
 *   circuitry evolved for scarce calories, novel information in
 *   tribe-relevant contexts, and status signals tied to material
 *   provisioning. Modern food systems exploit caloric abundance triggers.
 *   Algorithmic feeds exploit novelty-seeking and social-status circuits.
 *   Pharmaceutical markets exploit pain-avoidance and cognitive-enhancement
 *   desires. Individually, each person confronts a biological disposition
 *   that cannot be voluntarily overridden; structurally, this disposition is
 *   the target of commercial extraction valued at trillions of dollars. The
 *   constraint exhibits tangled rope structure: genuine coordination function
 *   (information access, economic opportunity, health interventions) coupled
 *   with asymmetric extraction (attention capture, metabolic disease,
 *   cognitive hijacking, pharmaceutical dependency). The theater ratio (0.48)
 *   reflects that discourse about evolutionary mismatch often becomes
 *   performative justification for inaction — 'we can't change our biology' —
 *   while the actual problem is contingent institutional design (engagement
 *   metrics, food formulation, dosing schedules) that could be reformed but
 *   isn't. The extractiveness trajectory (0.18 → 0.58 over 60 years) shows
 *   accumulation: the mismatch load has grown as technology has scaled and
 *   behavioral exploitation has become more sophisticated.
 *
 * KEY AGENTS:
 *   - Embodied humans (powerless/trapped): Biological dispositions cannot be exited; evolutionary heritage is a permanent constraint on voluntary behavior
 *   - Tech platforms and attention economy (institutional/arbitrage): Beneficiary. Algorithmic engagement optimization weaponizes reward circuitry; arbitrage exists between contexts (can sell user attention or pivots to well-being messaging)
 *   - Food industry and agricultural subsidies (institutional/arbitrage): Beneficiary. Caloric engineering and volume-based business models extract from satiety mechanisms; arbitrage into 'health foods' or direct-to-consumer models available
 *   - Pharmaceutical manufacturers (institutional/arbitrage): Beneficiary. Mismatch-driven diseases (metabolic syndrome, ADHD, depression, anxiety) create captive customer bases; arbitrage into 'nutraceuticals' or preventive claims available
 *   - Public health and medical institutions (organized/constrained): Victim-turned-partial-enforcer. Constrained by regulatory capture, industry funding, and evidence gaps; coordination mandate (disease prevention) conflicts with enforcement burden (defending against exploitation)
 *   - Individual health decision-makers (moderate/constrained): Victims with partial agency. Trapped by network effects (social media value depends on participation), economic necessity (processed foods are cheaper), and information asymmetry
 *   - Evolutionary psychology discourse (institutional/arbitrage): Piton. Once illuminating, now theatrical justification for passivity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_mismatch_load, 0.58).
domain_priors:suppression_score(evolutionary_mismatch_load, 0.65).
domain_priors:theater_ratio(evolutionary_mismatch_load, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_mismatch_load, extractiveness, 0.58).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_mismatch_load, tangled_rope).
narrative_ontology:human_readable(evolutionary_mismatch_load, "The Paleolithic Circuit Break: Evolutionary Mismatch Load").
narrative_ontology:topic_domain(evolutionary_mismatch_load, "biological/technological/social").

domain_priors:requires_active_enforcement(evolutionary_mismatch_load).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, attention_economy_extractors).
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, sedentary_lifestyle_profiteers).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, human_metabolic_health).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, cognitive_attention_systems).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, social_bonding_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBODIED HUMAN (SNARE) — Paleolithic neurology cannot exit the modern environment. Reward circuitry evolved for scarce calories, novel stimuli, and tribal status signals; modern food systems, algorithmic feeds, and social media weaponize these systems. No biological exit available. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH INSTITUTIONS (TANGLED ROPE) — Face dual mandate: disease prevention (coordination function) AND defense against commercial exploitation (enforcement burden). Can organize research and policy but constrained by regulatory capture and industry funding dependencies. Coordination benefit real but asymmetrically extracted. d≈0.62, f(d)≈0.82, σ=1.2 → χ≈0.57.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TECH AND FOOD INDUSTRY (ROPE) — Benefits from mismatch exploitation (attention capture, food volume sales, pharma dependence) while framing as coordination solution ('personalized nutrition', 'mental health apps', 'engagement optimization'). Can arbitrage between contexts; sees constraint as manageable operational problem. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL DECISION-MAKERS (TANGLED ROPE) — Coordination benefit: access to information about health, social connection, economic opportunity. Extraction burden: cognitive hijacking, false dietary guidance, pharmaceutical dependency, social comparison harm. Mobile theoretically but constrained by network effects and economic necessity. d≈0.68, f(d)≈1.03, σ=0.9 → χ≈0.60.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EVOLUTIONARY PSYCHOLOGY DISCOURSE (PITON) — Once functioned to illuminate mismatch mechanisms; now largely theatrical justification for inaction ('we can't change our biology'). Theater_ratio=0.48 approaches piton threshold; discourse persists through institutional citations despite reduced predictive power. Performative while actual mechanisms are regulatory capture.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From pure evolutionary distance, mismatch between ancestral and modern environments is logically necessary and irreducible. Any civilization technology-rich enough to exist will generate some mismatch; the structural mismatch cannot be eliminated. However, base extractiveness (0.58) and suppression (0.65) contradict the mountain gate — the constraint is partly contingent institutional extraction, not natural law.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_mismatch_load_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evolutionary_mismatch_load, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_mismatch_load, TR),
    TR >= 0.70.

:- end_tests(evolutionary_mismatch_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mismatch load is not pure extraction — genuine coordination benefits exist (information access, economic participation, medical interventions). But 60+ years of accumulation shows systematic exploitation: food industry profit margins depend on processing-driven metabolic damage that creates pharmaceutical customers; tech platforms depend on cognitive hijacking; pharma depends on mismatch-driven disease chronicity. The extractiveness trajectory (0.18 → 0.58) shows layering of extraction onto real coordination. Suppression (0.65): High. Exit barriers are substantial: biological (reward circuitry cannot be voluntarily disabled), economic (participation is employment necessity), social (network effects lock in participation), regulatory (enforcement against exploitation is captured). But not total — some communities have lower adoption; some individuals maintain discipline. Theater ratio (0.48): Moderate. Evolutionary psychology discourse serves some informational function but increasingly justifies inaction. Public health messaging about 'personal responsibility' is theatrical (individual willpower against trillion-dollar engagement optimization). Industry 'wellness' initiatives are theatrical. But the core mechanisms (algorithmic feeds, food formulation, dosing schedules) are functionally effective, not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The embodied human experiences maximum extraction (snare) — biology cannot exit. The individual decision-maker experiences mixed coordination and extraction (tangled rope) — real benefits and real harms, constrained by necessity. Public health sees coordination duty alongside enforcement burden (tangled rope) — must address mismatch but constrained by regulatory capture. Tech and food industries see manageable business problem (rope) — they coordinate the extraction mechanism; arbitrage options exist. Evolutionary discourse sees natural law (mountain) — mismatch is inherent to civilization technology. The gap reveals how institutional beneficiaries naturalize what is contingent extraction: 'we can't change human nature' disguises 'we profit from mismatch exploitation, so we will not change institutional design.'
 *
 * DIRECTIONALITY LOGIC:
 *   Embodied humans: Victim + trapped → d≈0.95, f(d)≈1.42. Biological inheritance is inescapable; maximum extraction. Individual decision-makers: Victim + constrained → d≈0.68, f(d)≈1.03. Network effects and economic necessity create constraint; real benefits present but extraction significant. Public health: Victim (of capture) + organized but constrained → d≈0.62, f(d)≈0.82. Can coordinate research and policy but extraction through regulatory capture and funding dependencies. Tech/food industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Can pivot institutional forms; net beneficiary. Evolutionary discourse: Institutional + arbitrage → d≈0.08, f(d)≈-0.09. Piton classification comes from theater gate (0.48 toward 0.70), not from high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in evolutionary mismatch load is the conflation of biological inevitability with institutional inevitability. The natural law perspective (mountain) claims mismatch is immutable — any technology-rich civilization creates some mismatch. This is partially true at the level of inevitable lag between evolutionary history and instantaneous environment. But the specificity and severity of modern mismatch load is institutional: the profitability of attention hijacking, caloric engineering, and pharmaceutical dependency are contingent policy and design choices. The tangled rope classification resolves the mandatrophy: the mismatch is partially natural (inevitable lag — mountain structure) but layered with institutional extraction (deliberate exploitation — snare or tangled rope structure). Institutional reform cannot eliminate mismatch lag but can reduce exploitation layering. The distinction matters: a resignation to 'evolution made us this way' (false mountain) leads to fatalism about industry practices that are actually reformable. The tangled rope classification preserves the real natural law element while identifying the contingent extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_neuroplasticity,
    'How much can human reward circuitry, attention allocation, and social bonding systems adapt to modern environments within a single lifetime or across generations?',
    'Longitudinal neuroscience studies of brain adaptation to digital environments; epigenetic studies of behavioral plasticity; cross-cultural variation in mismatch severity (populations with lower adoption rates as controls)',
    'If high plasticity: mismatch is temporary (scaffold); constraint resolves through cultural adaptation. If low plasticity: mismatch is inherent (mountain); no institutional intervention removes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_neuroplasticity, empirical, 'Capacity of human neural systems to adapt to modern environments').

omega_variable(
    extraction_intent_vs_misalignment,
    'To what degree is the mismatch actively exploited (intentional extraction) versus passively misaligned (orthogonal business incentives)?',
    'Industry documentation analysis (product design docs, engagement metrics as KPIs); expert interviews on intent; comparison of engagement metrics across products with high vs low exploitation (chronotype respect, caloric transparency, social features)',
    'If intentional: snare or tangled rope (enforced constraint). If passively misaligned: rope or scaffold (coordination with unintended harms). Classification implications for enforcement strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_intent_vs_misalignment, empirical, 'Whether mismatch exploitation is intentional or incidental to business models').

omega_variable(
    institutional_remedy_feasibility,
    'Can regulatory frameworks (interface transparency, algorithmic audits, food labeling, attention taxation, chronotype protections) materially reduce mismatch load or do they become performative theater?',
    'Natural experiments: jurisdictions with strong regulations (EU digital Act compliance, strong food labeling) vs weak (US, developing markets); outcome metrics (obesity rates, screen time, attentional disorders, mental health); tracking theater metric over time as regulations mature',
    'If feasible: scaffold perspective confirmed (sunset is real). If unfeasible: snare perspective confirmed (extraction mechanism too profitable to regulate away).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_remedy_feasibility, empirical, 'Whether regulatory remedies can substantively reduce mismatch load').

omega_variable(
    substitution_risk,
    'If attention economy and pharmaceutical dependency are constrained, will commercial interests find equally or more extractive substitute mechanisms targeting the same vulnerabilities?',
    'Historical pattern analysis of regulatory substitution (e.g., tobacco → vaping → novel nicotine); sector diversity analysis of venture capital in behavioral exploitation; prediction markets on next exploitation frontier',
    'If high substitution risk: tangled rope or snare (extraction mechanism is stable across interventions). If low: scaffold or rope (specific institutional forms can be reformed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_risk, conceptual, 'Whether constraining one extraction mechanism triggers substitution to others').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_mismatch_load, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evomis_tr_t0, evolutionary_mismatch_load, theater_ratio, 0, 0.25).
narrative_ontology:measurement(evomis_tr_t30, evolutionary_mismatch_load, theater_ratio, 30, 0.38).
narrative_ontology:measurement(evomis_tr_t60, evolutionary_mismatch_load, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(evomis_be_t0, evolutionary_mismatch_load, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(evomis_be_t30, evolutionary_mismatch_load, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(evomis_be_t60, evolutionary_mismatch_load, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_mismatch_load, resource_allocation).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, metabolic_disease_chronicity).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, attention_economy_capture).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, pharmaceutical_dependency_spiral).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, social_comparison_weaponization).

% DUAL FORMULATION NOTE:
% Evolutionary mismatch load is a parent constraint operating across multiple domains. The base constraint (biological predispositions in modern environment) has ε≈0.18 and would classify as rope (natural coordination problem). But institutional extraction layered onto this base constraint raises effective ε to 0.58 (tangled rope). The network links decompose the mismatch into domain-specific extraction mechanisms (metabolic disease profits, attention monetization, pharma chronicity spirals, social comparison harms), each with its own ε values reflecting the specific institutional exploitation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_mismatch_load, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
