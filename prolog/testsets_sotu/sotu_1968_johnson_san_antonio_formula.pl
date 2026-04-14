% ============================================================================
% CONSTRAINT STORY: sotu_1968_johnson_san_antonio_formula
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1968_johnson_san_antonio_formula, []).

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
 *   constraint_id: sotu_1968_johnson_san_antonio_formula
 *   human_readable: San Antonio Formula: Conditional Bombing Cessation in Vietnam War Negotiations
 *   domain: foreign_policy/military_strategy/negotiation
 *
 * SUMMARY:
 *   The San Antonio Formula (announced March 1968 by President Johnson) tied
 *   U.S. bombing cessation to North Vietnamese commitment to 'prompt,
 *   productive peace talks' without simultaneously expanding the war. This
 *   constraint exhibits the core structure of Tangled Rope: genuine
 *   coordination elements (establishing verifiable conditions for dialogue)
 *   combined with asymmetric extraction (continuation of bombing against
 *   civilian population until conditions are met). The constraint benefits
 *   the U.S. military establishment by preserving bombing advantage during
 *   negotiations and maintains coercive pressure on Hanoi while claiming
 *   pursuit of peace. It costs Vietnamese civilians in North and South active
 *   conflict zones, who bear continued bombing harm while political
 *   conditionality is debated. The constraint's theater ratio (0.55) reflects
 *   moderate performative content: the language of 'productive talks'
 *   obscures that productivity thresholds were asymmetrically defined and
 *   that bombing continuation served coercive rather than purely military
 *   purposes. Extractiveness increased over the formula's application period
 *   (0.42 to 0.58) as civilian costs accumulated and negotiation
 *   preconditions remained unmet.
 *
 * KEY AGENTS:
 *   - United States Military Establishment and Johnson Administration: Primary beneficiary (institutional/arbitrage) — preserve bombing advantage, maintain coercive leverage, claim pursuit of peace while continuing war
 *   - North Vietnamese Leadership: Secondary actor (powerful/constrained) — constrained by military and geopolitical position; seek de-escalation but cannot accept conditionality without appearing to capitulate
 *   - North Vietnamese Civilian Population: Primary victim (powerless/trapped) — bear continued bombing harm with no voice in conditionality debate
 *   - South Vietnamese Civilian Population: Secondary victim (powerless/trapped) — continued warfare in their territory while U.S. conditions for ceasefire remain unmet
 *   - American Anti-War Movement: Organized observer (organized/constrained) — recognize formula as theater but have limited political power to change policy
 *   - International Mediation Framework (UN, Non-Aligned Nations): Organized observer (organized/arbitrage) — see formula as temporary coordination mechanism with sunset logic toward genuine negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1968_johnson_san_antonio_formula, 0.58).
domain_priors:suppression_score(sotu_1968_johnson_san_antonio_formula, 0.68).
domain_priors:theater_ratio(sotu_1968_johnson_san_antonio_formula, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1968_johnson_san_antonio_formula, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1968_johnson_san_antonio_formula, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1968_johnson_san_antonio_formula, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1968_johnson_san_antonio_formula, tangled_rope).
narrative_ontology:human_readable(sotu_1968_johnson_san_antonio_formula, "San Antonio Formula: Conditional Bombing Cessation in Vietnam War Negotiations").
narrative_ontology:topic_domain(sotu_1968_johnson_san_antonio_formula, "foreign_policy/military_strategy/negotiation").

domain_priors:requires_active_enforcement(sotu_1968_johnson_san_antonio_formula).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1968_johnson_san_antonio_formula, united_states_military_establishment).
narrative_ontology:constraint_beneficiary(sotu_1968_johnson_san_antonio_formula, johnson_administration_negotiators).
narrative_ontology:constraint_victim(sotu_1968_johnson_san_antonio_formula, north_vietnamese_civilian_population).
narrative_ontology:constraint_victim(sotu_1968_johnson_san_antonio_formula, south_vietnamese_civilian_population).
narrative_ontology:constraint_victim(sotu_1968_johnson_san_antonio_formula, mutual_de_escalation_possibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VIETNAMESE CIVILIANS (SNARE) — Trapped in active conflict zones with no exit option and no voice in the negotiation structure. Bear the full extraction cost of the bombing continuation while conditionality is debated. The constraint uses their vulnerability (inability to stop bombing without reciprocal commitment) as enforcement mechanism. Maximum experienced extraction — no coordination benefit, pure coercion.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NORTH VIETNAMESE LEADERSHIP (TANGLED ROPE) — Constrained by military capability limitations and geopolitical isolation; also benefits from the formula's implicit recognition that negotiations are possible. The constraint creates a genuine coordination function (establishing preconditions for talks) but embeds asymmetric extraction (bombing continues unless Hanoi capitulates to conditionality). Mixed cost and benefit — constrained agency but not powerless.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. MILITARY AND DIPLOMATIC ESTABLISHMENT (ROPE) — Benefits from the formula's preservation of military advantage during negotiations and from the appearance of pursuing peace while maintaining coercive pressure. The constraint is experienced as pure coordination: establishing clear terms for de-escalation. Experiences minimal extraction — the mechanism channels benefits toward this agent. Arbitrage exit option because the establishment can withdraw from negotiations unilaterally and resume full bombing.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AMERICAN ANTI-WAR MOVEMENT (PITON) — Organized but constrained by limited political power during wartime. The formula is experienced as degraded rhetoric: it appears to pursue peace ('productive talks') while maintaining bombing until conditions are met. Theater is moderate-to-high (0.55) because the formula's language suggests genuine deescalation pathway while institutional reality continues extraction. The movement has agency and exit (protest, political pressure) but constrained effectiveness.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL MEDIATION FRAMEWORK (SCAFFOLD) — Organizations like the UN and non-aligned nations see the formula as a temporary coordination mechanism with sunset logic: once talks commence and demonstrate productivity, bombing should cease. The mechanism is experienced as Scaffold because it has explicit conditions for termination and represents institutional learning from previous escalation spirals. International mediators have agency (can mediate alternative frameworks) and see the formula as transitional toward sustainable negotiation.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the San Antonio Formula exhibits genuine coordination function (establishing preconditions for dialogue that both sides can verify and honor) simultaneously with asymmetric extraction (continuing civilian harm to enforce conditionality that favors the more militarily powerful party). The constraint is not a mountain (not inevitable to war) nor a pure snare (genuine coordination elements exist), but a true hybrid where the coordination and extraction functions cannot be separated.
constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1968_johnson_san_antonio_formula_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1968_johnson_san_antonio_formula, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1968_johnson_san_antonio_formula, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1968_johnson_san_antonio_formula, TR),
    TR >= 0.70.

:- end_tests(sotu_1968_johnson_san_antonio_formula_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The formula preserves military advantage (benefits U.S.) while requiring North Vietnam to meet preconditions (costs North Vietnam). The extraction flow is asymmetric but not total — genuine coordination elements exist (establishing verifiable conditions for talks), so this is not a pure snare (0.66+). Suppression (0.68): High. Bombing continuation against civilian population is the enforcement mechanism; North Vietnam has no exit option except acceptance of conditions. Civilian population has zero exit options. High suppression reflects that continued warfare is the coercive instrument. Theater ratio (0.55): Moderate. The formula's language emphasizes 'productive peace talks' and de-escalation, but the underlying mechanism (bombing continuation) is performative theater masking coercive pressure. Theater increases over time as the formula's conditions remain unmet and bombing continues, suggesting performative content is masking unchanged policy.
 *
 * PERSPECTIVAL GAP:
 *   Gap between beneficiary (institutional U.S., experiences Rope) and trapped victim (civilians, experiences Snare): U.S. sees coordination mechanism establishing clear cessation terms; civilians see bombing continuation as extraction mechanism masquerading as conditionality negotiation. Gap between constrained actor (North Vietnamese leadership, experiences Tangled Rope) and beneficiary (U.S., experiences Rope): leadership recognizes both coordination elements and extraction pressure; U.S. emphasizes coordination while downplaying coercive pressure. Gap between organized observer (anti-war movement, experiences Piton) and institutional beneficiary (U.S., experiences Rope): movement recognizes formula as degraded theater (bombing continues while talking about peace); institutional actors frame formula as successful coordination. The analytical observer resolves these gaps by identifying the constraint as genuinely Tangled Rope: the coordination and extraction functions cannot be disentangled because the conditionality mechanism is simultaneously the coordination tool and the coercive enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The formula's directionality is structured around military power asymmetry. The U.S. side has arbitrage options (can resume full bombing, can negotiate with South Vietnam, has geopolitical alternatives); North Vietnam is constrained (limited military options, international isolation, cannot withstand indefinite bombing). Vietnamese civilians are trapped with no exit. This asymmetry in exit options drives the directionality: low d (benefits) for U.S. institutional actors, high d (extraction) for North Vietnamese leadership and trapped civilians. The formula systematizes this power asymmetry by encoding it into negotiation preconditions. Beneficiaries (U.S. military) derive low effective extraction; victims (civilians, constrained North Vietnamese) derive high extraction. The suppression value (0.68) reflects that the coercive mechanism (bombing) is the primary enforcement tool, not agreement or shared interest.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is legitimately Tangled Rope, not misclassified as pure Rope or pure Snare. The mandatrophy is resolved by recognizing that genuine coordination elements (establishing preconditions for dialogue that both sides can verify) coexist with asymmetric extraction (bombing continuation enforced by military power differential). The beneficiary (U.S.) experiences genuine coordination benefit (establishes clear terms for cessation, channels leverage). The victim (civilians, constrained North Vietnam) experiences genuine extraction cost (continued bombing harm, asymmetric negotiation burden). This is not a false positive where extraction is hidden in coordination language — the extraction is explicit and structural. The formula is not a pure snare because some agents (international mediators, negotiators on both sides) perceive genuine coordination value. It is not a pure rope because asymmetric extraction is fundamental, not incidental. The Tangled Rope classification is confirmed by: (1) requires_active_enforcement=true (bombing is the enforcement mechanism), (2) beneficiaries=[U.S. military], (3) victims=[civilians, mutual de-escalation], (4) extractiveness > 0.46, (5) genuine coordination function exists (establishing verifiable preconditions) and genuine extraction flow (bombing asymmetry). All gates pass.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_threshold_ambiguity,
    'What constitutes ''prompt, productive peace talks'' sufficient to trigger bombing cessation?',
    'Analysis of stated criteria vs. actual implementation; examination of how negotiators on each side defined ''productivity'' and whether definitions were symmetrical or asymmetrically favored U.S. interests',
    'If threshold was asymmetric (favoring U.S. interpretation): constraint functions as pure extraction mechanism disguised as coordination. If symmetric: genuine coordination mechanism with residual power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_threshold_ambiguity, empirical, 'Definition and symmetry of productivity criteria for bombing cessation').

omega_variable(
    conditionality_enforceability,
    'Could North Vietnam realistically meet the formula''s conditions (commit to productive talks, cease infiltration, recognize South Vietnam sovereignty) without losing negotiating position?',
    'Game-theoretic analysis of North Vietnamese position: whether accepting conditions meant conceding political goals; examination of historical parallel cases where conditionality was symmetric vs. imposed by dominant power',
    'If conditions were non-survivable: formula is extraction mechanism using civilian harm as enforcement. If conditions were negotiable: genuine coordination mechanism with power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_enforceability, conceptual, 'Realistic feasibility of meeting conditions without capitulation').

omega_variable(
    bombing_continuation_justification,
    'Was bombing continuation during the ''conditionality waiting period'' militarily necessary, or was it primarily coercive leverage?',
    'Military analysis of U.S. strategic position; comparison of bombing effectiveness during negotiation period vs. previous phases; examination of military command memoranda on bombing rationale',
    'If military necessity: suppression justified as war cost. If coercive leverage: suppression is pure extraction mechanism, raising snare classification probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bombing_continuation_justification, empirical, 'Military necessity vs. coercive leverage for bombing continuation').

omega_variable(
    civilian_cost_distribution,
    'How were civilian casualties distributed between North and South Vietnam, and did the formula implicitly allocate higher costs to one side?',
    'Historical casualty data by geography; analysis of bombing tonnage distribution; examination of whether Northern civilians bore disproportionate cost due to formula mechanics',
    'If asymmetric: constraint functions as explicit extraction from civilian population of less powerful party. Raises snare severity and clarifies victim status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_cost_distribution, empirical, 'Civilian casualty distribution and formula asymmetry').

omega_variable(
    alternative_cessation_pathways,
    'What other de-escalation mechanisms were theoretically available, and why was conditionality selected?',
    'Analysis of Johnson administration deliberations; examination of alternative proposals from military, State Department, and international mediators; comparison of constraint types each alternative would instantiate',
    'If alternatives existed but were rejected: choice of conditionality reveals preference for extraction mechanism. If formula was unique viable option: may reflect genuine coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_cessation_pathways, empirical, 'Availability and rejection of alternative de-escalation mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1968_johnson_san_antonio_formula, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(saa_tr_t0, sotu_1968_johnson_san_antonio_formula, theater_ratio, 0, 0.48).
narrative_ontology:measurement(saa_tr_t4, sotu_1968_johnson_san_antonio_formula, theater_ratio, 4, 0.52).
narrative_ontology:measurement(saa_tr_t8, sotu_1968_johnson_san_antonio_formula, theater_ratio, 8, 0.55).

% Extraction over time
narrative_ontology:measurement(saa_be_t0, sotu_1968_johnson_san_antonio_formula, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(saa_be_t4, sotu_1968_johnson_san_antonio_formula, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(saa_be_t8, sotu_1968_johnson_san_antonio_formula, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1968_johnson_san_antonio_formula, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1968_johnson_san_antonio_formula, vietnam_war_bombing_campaign).
narrative_ontology:affects_constraint(sotu_1968_johnson_san_antonio_formula, paris_peace_negotiations).
narrative_ontology:affects_constraint(sotu_1968_johnson_san_antonio_formula, operation_rolling_thunder).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1968_johnson_san_antonio_formula, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
