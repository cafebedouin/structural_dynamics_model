% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Electronic Money Emergence — The Became-Thinkable Reading
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   Electronic money emergence as 'became thinkable' characterizes the
 *   constraint as a cognitive/conceptual shift spanning decades prior to
 *   institutional measurement. The reading frames the emergence of digital
 *   currency not as a discrete invention event (the first credit card, the
 *   first electronic fund transfer) but as a gradual diffusion of the idea
 *   that money could be instantiated in digital form—a shift in what
 *   economists, engineers, and financial actors could coherently imagine as
 *   possible. This reading emphasizes that 'thinkability' precedes both
 *   technical implementation and institutional measurement by decades. The
 *   constraint itself is the institutional lock that arose once digital money
 *   became thinkable: once the possibility entered the conceptual commons,
 *   financial gatekeepers had to manage the extraction of value from the
 *   transition, suppressing alternative monetary arrangements and the
 *   legitimacy of cash-based systems. The extractiveness trajectory reflects
 *   accumulating institutional enforcement as digital payment networks mature
 *   and cash infrastructure is deliberately degraded. Theater ratio rises as
 *   the performative aspects of banking (branches, ATMs, teller services)
 *   persist despite functional obsolescence, maintained for symbolic
 *   stability rather than actual necessity.
 *
 * KEY AGENTS:
 *   - Fintech innovators and payment-network designers: Primary beneficiaries (institutional/arbitrage) — capture fee revenue, data harvesting rights, and network-effects monopoly power as digital money diffuses
 *   - Central banks and state monetary authorities: Secondary beneficiary but constrained (institutional/constrained) — gain superior transaction surveillance and capital control but lose direct monetary monopoly and seigniorage
 *   - Large institutional financial actors: Beneficiary (powerful/mobile) — arbitrage between payment networks, benefit from efficiency gains; maintain exit optionality
 *   - Cash-dependent populations: Primary victims (powerless/trapped) — have no meaningful exit as cash payment systems are deliberately degraded; forced into digital surveillance
 *   - Independent merchants: Secondary victims (moderate/constrained) — face payment-network gatekeeping and high fees but also benefit from expanded reach; constrained but not trapped
 *   - Physical banking infrastructure: Institutional actor (institutional/arbitrage) — persists through inertia (piton) despite functional obsolescence; theater infrastructure for legitimacy
 *   - Monetary sovereignty doctrine: Victim (institutional/trapped) — the conceptual framework that money is a state monopoly is undermined by private digital money; no exit available to this idea once digital thinkability becomes hegemonic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.42).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Electronic Money Emergence — The Became-Thinkable Reading").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'e3ea2353-a2b0-48b3-ae57-5ba234ca0958').
narrative_ontology:cs_kernel_codification('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', distributed).
narrative_ontology:cs_authority_grounding('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', extraction).
narrative_ontology:cs_interpretation_layer_present('e3ea2353-a2b0-48b3-ae57-5ba234ca0958').
narrative_ontology:cs_reading_relation('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_reading_relation('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', foundational, money_thinkability_precedes_instantiation).
narrative_ontology:cs_axiom_status(money_thinkability_precedes_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', money_thinkability_precedes_instantiation, empirically_contingent).
narrative_ontology:cs_axiom('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', secondary, institutional_thinkability_captures_emergence_narrative).
narrative_ontology:cs_axiom_status(institutional_thinkability_captures_emergence_narrative, holdable).
narrative_ontology:cs_axiom_grounding('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', institutional_thinkability_captures_emergence_narrative, deontological).
narrative_ontology:cs_reference_frame('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', state_monetary_monopoly).
narrative_ontology:cs_drift_state('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', contemporary_digital_hegemony_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3ea2353-a2b0-48b3-ae57-5ba234ca0958', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, financial_technology_innovators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, institutional_finance_gatekeepers).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, cash_payment_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CASH-DEPENDENT AGENT (SNARE) — Trapped within digital payment infrastructure forced onto them; cannot exit to alternatives as cash systems decay. Bears extraction costs (surveillance, transaction fees, account closure risk) without meaningful choice. Digital money's inevitability is manufactured through infrastructure degradation, not emerged through neutral technical superiority.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT MERCHANTS (TANGLED ROPE) — Constrained by payment network gatekeeping (high merchant fees, account closure, algorithmic moderation) but also benefit from expanded market reach and reduced physical security costs. Extraction through payment-system coercion combined with genuine coordination benefit. Cannot fully exit without market access, but exit costs are surmountable.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINTECH INNOVATORS (ROPE) — Primary beneficiaries experiencing the constraint as coordination problem: enabling electronic transactions, reducing friction, expanding addressable market. Net extractive power flows toward this group through fee capture, data harvesting, network effects. They perceive 'thinkability' as a genuine innovation problem solved.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANKS (TANGLED ROPE) — Constrained by loss of direct money-creation control and seigniorage capture via private payment networks, but also benefit from digital money's superior control and surveillance capacity. Genuine coordination problem (payment system efficiency) paired with asymmetric extraction (loss of monetary monopoly). Active enforcement required to regulate payment networks while maintaining the fiction of their independence.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE INSTITUTIONAL FINANCE (ROPE) — Powerful actors who benefit from digital money's efficiency for wholesale transactions and cross-border settlement. Can arbitrage between payment networks and maintain optionality. Low suppression, high exit capacity. Experience the constraint as pure coordination solution.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY BANKING INFRASTRUCTURE (PITON) — Branches, ATMs, teller services persist through institutional inertia despite declining functional role. Digital money's emergence has made the physical infrastructure largely performative — maintained for retail legitimacy and symbolic stability rather than actual necessity. Theater ratio high; coordination function degraded.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, digital money is an inevitable technical evolution: once computational technology existed, some form of digital value transfer became physically/logically necessitated. Emergence follows from technological capability, not institutional design. This perspective risks naturalizing what is actually a path-dependent institutional arrangement.
constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electronic_money_emergence__became_thinkable_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, TR),
    TR >= 0.70.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits significant but not maximal extraction. Institutional beneficiaries (fintechs, large banks) extract through fee capture, data harvesting, and network monopolies; central banks extract through superior surveillance but lose monopoly control. The extraction is substantial but paired with genuine coordination benefits — digital payments do reduce friction and enable expanded commerce. The reading 'became thinkable' emphasizes the cognitive precursor to extraction: once digital money was conceptually possible, it became inevitable that institutional actors would manage the transition to capture value. Suppression (0.42): Moderate-high. Active enforcement is required to degrade cash infrastructure, regulate payment networks, suppress alternative monetary schemes (local currencies, cryptocurrencies, cash-only communities), and maintain the institutional fiction that digital money emerged naturally rather than through deliberate design. Suppression is not total — cash persists in pockets and informal economies — but is increasing as ATM networks close and merchant adoption of card-only systems accelerates. Theater ratio (0.55): Moderate-high. Physical banking branches and ATMs are becoming largely performative — maintained for psychological assurance and retail legitimacy rather than actual transaction necessity. Digital payment networks add their own theater (security claims, convenience narratives) that obscures the underlying extraction mechanisms. The theater ratio has risen over time as the actual coordination function (moving value) has decoupled from the institutional role-playing (branches, tellers, banking hours).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Fintech innovators see pure coordination (Rope) — solving the legitimate problem of frictionless payments. Central banks see mixed benefit and loss (Tangled Rope) — gaining surveillance capacity while losing monetary monopoly. Large institutional finance sees efficiency gains with optionality (Rope). Independent merchants see mixed extraction and benefit (Tangled Rope) — fees and gatekeeping paired with market access. Cash-dependent populations see pure extraction (Snare) — forced into systems they cannot exit, bearing surveillance and account-closure risk. Physical banking sees itself as degraded (Piton) — once-functional, now maintained through inertia. The analytical observer risks naturalizing the entire process as inevitable technical evolution (Mountain) — a false summit that obscures how institutional actors managed the thinkability-to-implementation pipeline to their advantage.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit-option capacity. Fintech innovators and large banks benefit with high exit capacity (d ≈ 0.15, negative f(d)) — they experience low or negative effective extraction because the flow of value runs toward them. Central banks benefit in some dimensions (surveillance) but lose in others (monopoly), paired with constrained exit from the system — derived d ≈ 0.40 (moderate f(d)). Independent merchants are both victims of fees and beneficiaries of reach — derived d ≈ 0.55 (high f(d) approaching 1.0). Cash-dependent populations are clear victims with trapped exit — derived d ≈ 0.92 (maximum f(d) ≈ 1.42). Physical banking infrastructure benefits from institutional support but experiences its own function atrophying — derived d ≈ 0.50 (moderate f(d)). The perspectival gaps emerge from these differentiated directionalities: same constraint, different experienced extractiveness based on position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_inevitability,
    'Does ''became thinkable'' describe an autonomous cognitive/conceptual shift, or does it track the material infrastructure that made digital money technically possible and institutionally incentivized?',
    'Historical analysis of concept emergence: did key innovators reference novel theoretical breakthroughs or existing technical components recombined? Timeline correlation between theoretical papers, technical breakthroughs, and practical implementations.',
    'If autonomous cognitive shift: thinkability is a real independent variable; ''became thinkable'' is the right reading. If infrastructure-driven: ''became thinkable'' is epiphenomenal; a material-conditions reading better explains emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_inevitability, empirical, 'Whether thinkability is an autonomous cognitive shift or driven by material/technical conditions').

omega_variable(
    measurement_lag_vs_deliberate_obscurity,
    'Does institutional measurement lag behind conceptual emergence due to genuine epistemic difficulty (hard to measure what hasn''t materialized), or is the lag deliberately maintained to obscure the timing of extraction onset?',
    'Comparison of contemporaneous technical documentation vs retrospective institutional narratives. Analysis of when key metrics (payment system penetration, cash-to-electronic ratio, transaction volumes) were actually compiled vs when they began to be tracked.',
    'If genuine epistemic lag: measurement problem is structural. If deliberate obscurity: the constraint is partly a story-management mechanism; falsification of emergence narrative becomes a constitutive extraction tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_lag_vs_deliberate_obscurity, empirical, 'Whether measurement lag is epistemic difficulty or deliberate institutional obscurity').

omega_variable(
    reading_identity_ambiguity,
    'Is this reading (became-thinkable) distinct from the first-held reading, or do they describe the same emergence event from different temporal angles?',
    'Definition clarification: does ''became thinkable'' track when actors first articulated digital money as a possibility (could be decades before first holding), or does it collapse into ''first held'' when the first functional implementation was demonstrated?',
    'If truly distinct: three readings capture genuine structural difference (conceptual possibility, first implementation, measurement/recognition). If collapsed: this reading is a temporal slice of a single event, not an alternative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether became-thinkable is distinct from first-held or a temporal phase of same event').

omega_variable(
    institutional_coordination_vs_market_coordination,
    'Did digital money ''become thinkable'' through top-down institutional planning (central banks, large banks designing the system), or through bottom-up market discovery and technological innovation?',
    'Historical archive analysis: ratio of institutional design documents vs decentralized innovation. Who proposed the first conceptualizations? Were they institutional authorities or peripheral technical communities?',
    'If institutional: the coordination function and active enforcement make tangled_rope classification robust. If market-driven: the ''enforcement'' may be post-hoc institutional capture of an emerged system, changing the structural relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_coordination_vs_market_coordination, empirical, 'Whether digital money emergence was institutional design or decentralized market discovery').

omega_variable(
    false_summit_natural_law_candidate,
    'Is digital money emergence a genuine natural law (technological determinism — once computers exist, digital money inevitably follows), or is it a constructed institutional arrangement that benefits specific financial actors and naturalizes itself as inevitable?',
    'Counterfactual institutional history: were there viable alternative monetary arrangements (decentralized cash, credit-union networks, mixed cash-digital systems) that were foreclosed by deliberate institutional choices rather than technical necessity?',
    'If natural law: mountain classification from the analytical perspective is correct. If constructed: mountain is a false summit; the constraint is actually tangled_rope or snare; institutional beneficiaries are hiding behind technological inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether digital money emergence is technological inevitability or constructed institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_think_tr_t1950, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(emerg_think_tr_t1975, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(emerg_think_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(emerg_think_tr_t2020, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2020, 0.68).

% Extraction over time
narrative_ontology:measurement(emerg_think_be_t1950, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(emerg_think_be_t1975, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(emerg_think_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(emerg_think_be_t2020, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2020, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(emerg_think_su_t1975, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(emerg_think_su_t2000, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(emerg_think_su_t2020, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2020, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, resource_allocation).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, central_bank_digital_currency_dilemma).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, merchant_fee_extraction_via_payment_networks).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three structurally distinct readings: became-thinkable (conceptual-possibility phase, ε≈0.38), first-held (first practical instantiation, ε≈0.28), and m4-m5-collapse (measurement lag, ε≈0.52). Each reading has different beneficiary/victim structures and different institutional enforcement requirements. They are linked via affects_constraints to show kinship in the constraint family, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__became_thinkable_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
