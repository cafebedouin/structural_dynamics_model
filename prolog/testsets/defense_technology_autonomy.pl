% ============================================================================
% CONSTRAINT STORY: defense_technology_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_defense_technology_autonomy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: defense_technology_autonomy
 *   human_readable: Defense Technology Autonomy Constraint
 *   domain: military/defense_technology/governance
 *
 * SUMMARY:
 *   The defense technology autonomy constraint describes the structural
 *   tension between military incentives to develop autonomous weapons systems
 *   and civilian/democratic interests in maintaining human control over
 *   lethal decision-making. The constraint exhibits characteristics of all
 *   six Deferential Realism types across different observer perspectives,
 *   revealing how the same institutional phenomenon appears simultaneously as
 *   pure extraction (from civilians' view), coordination (from military
 *   procurement's view), a temporary problem with governance solutions (from
 *   human-in-the-loop advocates), a degraded legal framework (from
 *   international law's view), and potentially immutable technological
 *   necessity (from civilization-scale analysis). The extractiveness has
 *   grown from 0.35 (early development phase) to 0.58 (current deployment
 *   phase) as autonomous systems move from research to operational
 *   integration, while theater_ratio has increased from 0.52 to 0.65 as
 *   military classification and doctrinal ambiguity about autonomous
 *   deployment status have grown. Suppression is high (0.68) due to technical
 *   complexity, security classification, and the structural powerlessness of
 *   civilian populations to exit or consent to targeting regimes they cannot
 *   observe or influence.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victims (powerless/trapped) — bear extraction risk of autonomous targeting with zero participation in governance decisions; no exit option
 *   - International Community and Non-Combatants: Secondary victims (powerless/trapped) — trapped in arms dynamics spiral; all states pressured to develop autonomous systems regardless of doctrine; security dilemma prevents collective exit
 *   - Military Procurement Establishments: Primary beneficiary (institutional/arbitrage) — solves operator load and decision speed problems; captures strategic advantage during transition window; can arbitrage to traditional systems if needed
 *   - Defense Contractors and Developers: Primary beneficiary (institutional/arbitrage) — creates new markets for AI/ML capabilities; justifies R&D investment; enables premium pricing; intellectual property value from defense classification
 *   - Democratic Legislative Bodies: Mixed position (organized/constrained) — face coordination problem requiring secrecy vs oversight; constrained by classification barriers; also benefit from capability coordination function
 *   - Allied State Partners: Mixed position (powerful/constrained) — gain autonomous system access through transfer; constrained by dependency on developer state; vulnerable to technology embargoes; face alliance politics constraints
 *   - International Legal Framework: Institutional observer (institutional/arbitrage) — maintains treaty obligations and law-of-armed-conflict frameworks despite their degraded technical relevance; persists through inertia
 *   - Human-in-the-Loop Advocacy Movements: Organized reformers (organized/mobile) — work to establish meaningful human control requirements and verification regimes; see sunset pathway through technical standards and transparency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as inherent technological necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(defense_technology_autonomy, 0.58).
domain_priors:suppression_score(defense_technology_autonomy, 0.68).
domain_priors:theater_ratio(defense_technology_autonomy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(defense_technology_autonomy, extractiveness, 0.58).
narrative_ontology:constraint_metric(defense_technology_autonomy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(defense_technology_autonomy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(defense_technology_autonomy, tangled_rope).
narrative_ontology:human_readable(defense_technology_autonomy, "Defense Technology Autonomy Constraint").
narrative_ontology:topic_domain(defense_technology_autonomy, "military/defense_technology/governance").

domain_priors:requires_active_enforcement(defense_technology_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(defense_technology_autonomy, military_procurement_establishments).
narrative_ontology:constraint_beneficiary(defense_technology_autonomy, defense_contractors).
narrative_ontology:constraint_beneficiary(defense_technology_autonomy, weapons_system_developers).
narrative_ontology:constraint_victim(defense_technology_autonomy, civilian_populations).
narrative_ontology:constraint_victim(defense_technology_autonomy, international_stability).
narrative_ontology:constraint_victim(defense_technology_autonomy, democratic_oversight_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped subjects of weapons system deployment with no exit option. Cannot refuse targeting, cannot negotiate terms, cannot withdraw consent. Extreme suppression: targeting occurs without warning or recourse. Maximum extraction — bear full cost of autonomous weapon deployment with zero participation in governance decisions that enable it.
constraint_indexing:constraint_classification(defense_technology_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL COMMUNITY (SNARE) — Trapped in an arms dynamics spiral. States deploying autonomous weapons create security dilemmas forcing other states to develop competing systems. Non-combatants in all states bear extraction risk. No exit from the collective action trap. Suppression maintained through security classification, technical opacity, and strategic ambiguity about deployment status.
constraint_indexing:constraint_classification(defense_technology_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEMOCRATIC LEGISLATURES (TANGLED ROPE) — Face genuine coordination problem: military capability development requires secrecy and speed, while democratic oversight requires transparency and deliberation. Constraints on disclosure limit legislatures' ability to oversee. Yet legislatures also benefit from autonomous systems' coordination function (more precise targeting, reduced operator burden). Extraction operates through technical complexity barriers and classification restrictions that prevent effective democratic deliberation.
constraint_indexing:constraint_classification(defense_technology_autonomy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY PROCUREMENT (ROPE) — Primary beneficiary. Experiences autonomy constraint as pure coordination: solves the problem of operator load, decision speed under uncertainty, and weapons system integration. Can exit the constraint through traditional systems if desired (arbitrage option via alternative procurement). Net benefit: capability gains, career advancement through modernization, strategic advantage during transition window.
constraint_indexing:constraint_classification(defense_technology_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFENSE CONTRACTORS (ROPE) — Primary beneficiary. Autonomy constraint solves market coordination problem: creating demand for cutting-edge AI/ML capabilities, justifying R&D investment, enabling premium pricing for 'intelligent' systems. Can arbitrage into other AI markets if defense market contracts. Net benefit: market creation, intellectual property value, competitive moat from defense classification.
constraint_indexing:constraint_classification(defense_technology_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALLIED STATE PARTNERS (TANGLED ROPE) — Face security dilemma within alliance context. Coordination benefit: access to advanced autonomous systems through technology transfer or co-development. Extraction: dependent on primary developer state for capability updates, vulnerable to technology embargoes, constrained by alliance politics from developing independent systems. Suppression through technology control regimes (ITAR-equivalent restrictions).
constraint_indexing:constraint_classification(defense_technology_autonomy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL LAW (PITON) — Existing law of armed conflict frameworks (distinction, proportionality, precaution) persist as governance mechanisms despite their degraded function for autonomous systems. International treaties on weapons control (Convention on Certain Conventional Weapons, etc.) attempt to constrain autonomous weapons but operate through performative compliance and definitional ambiguity. Theater ratio high: treaty obligations exist but enforcement is minimal and loopholes abundant. Piton classification reflects the persistence of legal frameworks through institutional inertia despite their technical irrelevance.
constraint_indexing:constraint_classification(defense_technology_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: HUMAN-IN-THE-LOOP ADVOCACY (SCAFFOLD) — Organized actors (NGOs, academic coalitions, ethics boards) working to establish human meaningful control requirements and technical oversight mechanisms. See the autonomy constraint as a temporary coordination failure with potential sunset through norm-setting and verification protocols. Mobile exit options: deploy these governance mechanisms to constrain development. Sunset logic: technical standards for meaningful human control, international verification regimes, and transparency requirements could create alternative pathways that bypass military autonomy imperative.
constraint_indexing:constraint_classification(defense_technology_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / TECHNOLOGY IMPERATIVE (MOUNTAIN) — From civilizational/universal perspective, autonomous weapons are treated as inevitable: technology always advances, militaries always adopt best available capabilities, arms races always accelerate competitive dynamics. This view naturalizes the constraint as inherent to technological civilization. However, structural data contradicts the mountain classification — the engine detects false summit. The 'technological inevitability' framing naturalizes what is actually a contingent policy choice (funding decisions, procurement priorities, classification regimes) maintained through institutional arrangements rather than laws of nature.
constraint_indexing:constraint_classification(defense_technology_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(defense_technology_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(defense_technology_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(defense_technology_autonomy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(defense_technology_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(defense_technology_autonomy, TR),
    TR >= 0.70.

:- end_tests(defense_technology_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint benefits military procurement and defense contractors through capability gains and market creation, while extracting from civilian populations who bear targeting risk without consent. The extraction increased from 0.35 to 0.58 as autonomous systems moved from research to deployment phases, reflecting the growing real-world impact. The 0.58 value reflects genuine mixed function: militaries do solve legitimate coordination problems (operator load, decision speed), but the benefits accrue asymmetrically to organizational actors while costs fall on powerless civilians. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) technical complexity barriers preventing democratic understanding, (2) military classification preventing information access, (3) strategic ambiguity about deployment status, (4) structural powerlessness of civilian populations to exit or negotiate. Theater ratio (0.65): Moderate-high. Significant performative elements include treaty compliance theater (CCWC negotiations without enforcement), doctrinal ambiguity about what 'autonomous' actually means, international legal frameworks persisting despite technical irrelevance (laws of armed conflict designed for human decision-makers), and public reassurance narratives about human oversight that may not reflect operational reality. The theater has increased as classification has grown and definitional ambiguity has deepened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap through maximum distance: civilian victims and military beneficiaries experience fundamentally different realities of the same institutional system. Civilians experience snare (no exit, no coordination benefit, maximum extraction). Military procurement experiences rope (pure coordination benefit, arbitrage exits available). Democratic legislatures experience tangled rope (both coordination function and extraction through classification barriers). International law experiences piton (degraded function but institutional persistence). The human-in-the-loop coalition experiences scaffold (temporary problem with sunset pathway). The civilizational analyst risks mountain (technological inevitability) but structural data reveals false summit. These are not measurement disagreements — they are genuine structural differences in how the constraint's extraction flows toward and away from different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position relative to the constraint's extraction flow. Civilian populations are trapped with no exit, so d approaches 1.0 — maximum experienced extractiveness. Military procurement institutions have arbitrage exits (can choose traditional systems) and are beneficiaries, so d approaches 0.15 — low or negative experienced extractiveness. Democratic legislatures face high constraints but also benefit from coordination, so d ≈ 0.55 — moderate experienced extraction. Allied states face constraints but also gain access, so d ≈ 0.50 — mixed experience. The engine derives these from the beneficiary/victim declarations and exit options. The constraint's effective extractiveness (χ) is scaled by f(d) and the spatial scope modifier σ(S=global=1.2), raising the calculated χ from the base 0.58 to approximately 0.70 at global scope for powerless/trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the defense autonomy constraint simultaneously exhibits genuine coordination function (reducing operator cognitive load, improving decision speed, solving military integration problems) AND asymmetric extraction (benefits flow to institutional beneficiaries while costs and risks concentrate on powerless civilians and non-combatants). This is the defining signature of tangled_rope: both functions are real and both are structurally necessary to understand the constraint. The constraint cannot be reduced to pure extraction (snare) because military autonomy genuinely solves coordination problems that exist independently. It cannot be reduced to pure coordination (rope) because the benefits and costs are asymmetrically distributed and suppression is high. The claimed_type (tangled_rope) is analytically accurate: the constraint requires both coordination AND enforcement, both benefits AND extraction, both real functionality AND real harm. The mandatrophy is satisfied when all six perspectives are acknowledged as legitimate readings of these dual functions from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_human_control_definition,
    'What constitutes ''meaningful human control'' over autonomous weapons — is it a technical property (human decision on every lethal act) or a governance property (human responsibility for system design and deployment)?',
    'International treaty negotiation outcomes; technical standards bodies (IEEE, ISO) definitions; national military doctrine adoption of human control principles',
    'If technical (human per-shot decision required): autonomous weapons cannot legally deploy at scale, sharply constraining military extraction. If governance (designer/deployer responsibility): current architectures satisfy control requirement, extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meaningful_human_control_definition, conceptual, 'Definition of meaningful human control determines legal permissibility').

omega_variable(
    arms_race_inevitability,
    'Is autonomous weapons development inherent to military competition or a contingent choice driven by specific strategic incentives and institutional actors?',
    'Historical analysis of weapons development decisions; counterfactual policy scenarios where development was constrained; comparison of military autonomy adoption across states with different governance structures and strategic doctrines',
    'If inherent: constraint is mountain-like (ineliminable); scaffold sunset is unrealistic. If contingent: constraint is tangled_rope/snare (changeable through policy); scaffold perspective is structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_race_inevitability, empirical, 'Whether autonomous weapons development is inevitable or contingent').

omega_variable(
    transparency_security_tradeoff,
    'Does military security classification genuinely protect strategic advantage or primarily enable extraction by preventing democratic oversight?',
    'Declassification impact analysis; comparison of capability retention across declassified vs still-classified systems; vulnerability disclosure patterns in defense contractors',
    'If genuine security need: suppression (0.68) may be justified as coordination cost. If primarily political: suppression is an extraction mechanism; reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_security_tradeoff, empirical, 'Whether classification serves security or enables extraction').

omega_variable(
    coalition_power_emergence,
    'Can powerless civilian populations organize effective political pressure on autonomous weapons deployment despite information barriers and security classification?',
    'Tracking of advocacy movement growth, policy influence, and treaty negotiation participation; comparison of public opinion vs deployment decisions across democracies',
    'If coalition emerges with political power: powerless perspective may upgrade to organized; snare classification could shift toward tangled_rope. If pressure remains ineffective: powerless agents remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_emergence, empirical, 'Whether powerless agents can organize effective political coalition').

omega_variable(
    technology_transfer_chain_extraction,
    'Does technology transfer to allied states constitute genuine coordination (allies gain capability) or embedded extraction (allies become dependent on primary developer state for updates and remain strategically subordinate)?',
    'Analysis of technology transfer agreements; comparison of independent development capability across allied states pre- and post-transfer; strategic autonomy outcomes for recipient states',
    'If genuine coordination: allied state perspective is rope. If extraction through dependency: allied state perspective is snare; revise to higher suppression in multi-state contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_chain_extraction, empirical, 'Whether technology transfer enables autonomy or creates dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(defense_technology_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deftech_tr_t0, defense_technology_autonomy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(deftech_tr_t5, defense_technology_autonomy, theater_ratio, 5, 0.6).
narrative_ontology:measurement(deftech_tr_t10, defense_technology_autonomy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(deftech_be_t0, defense_technology_autonomy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deftech_be_t5, defense_technology_autonomy, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(deftech_be_t10, defense_technology_autonomy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(defense_technology_autonomy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(defense_technology_autonomy, 0.12).
narrative_ontology:affects_constraint(defense_technology_autonomy, ai_weapons_development_imperative).
narrative_ontology:affects_constraint(defense_technology_autonomy, military_classification_opacity).
narrative_ontology:affects_constraint(defense_technology_autonomy, international_arms_control_effectiveness).

% DUAL FORMULATION NOTE:
% Defense technology autonomy is downstream of both AI capability development (which drives what military systems can do) and of institutional military incentives (which drive deployment decisions). It also affects international stability through arms race dynamics. This story focuses on the governance constraint; separate stories should address (1) AI technical capability constraints and (2) institutional military incentive structures. The network links show this constraint's influence on downstream governance challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(defense_technology_autonomy, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
