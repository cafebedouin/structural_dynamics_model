% ============================================================================
% CONSTRAINT STORY: israel_gaza_ceasefire_violation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_gaza_ceasefire_violation, []).

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
 *   constraint_id: israel_gaza_ceasefire_violation
 *   human_readable: Israel-Gaza Ceasefire Violation Escalation Trap
 *   domain: geopolitical/conflict_resolution
 *
 * SUMMARY:
 *   The Israel-Gaza ceasefire violation escalation trap is a structurally
 *   complex constraint that exhibits snare characteristics for civilian
 *   populations while showing tangled-rope and piton features at
 *   institutional levels. Following a ceasefire agreement (often brokered by
 *   regional mediators like Egypt and Qatar), hardline factions on both
 *   sides—whether Hamas military wings seeking to prevent Palestinian
 *   Authority consolidation or Israeli far-right constituencies opposing
 *   territorial concessions—deliberately violate terms to trigger retaliatory
 *   cycles. Each violation generates a proportional (or disproportional)
 *   response that re-traumatizes civilian populations, undermines moderate
 *   leadership, and perpetuates a cycle of extraction where hardline factions
 *   consolidate political control through sabotage of diplomacy. The
 *   constraint's extractiveness (0.68) reflects systematic asymmetry:
 *   hardliners benefit from resumed conflict (political consolidation,
 *   security budgets, factional dominance), while civilians bear costs
 *   (displacement, death, infrastructure destruction). Suppression (0.78) is
 *   severe: civilians cannot exit the geographic theater, cannot prevent
 *   violations by factions they don't support, and cannot enforce ceasefire
 *   terms without international backing that is often performative. The
 *   theater_ratio (0.65) indicates substantial performance: international
 *   legal frameworks and UN resolutions provide legitimacy theater while
 *   enforcement mechanisms remain ineffective. This constraint demonstrates
 *   how a coordination problem (maintaining ceasefire) gets captured by
 *   extraction (hardline factions using violations to consolidate power and
 *   suppress moderate alternatives).
 *
 * KEY AGENTS:
 *   - Civilian Population (Gaza and Israel): Primary victim (powerless/trapped) — bears full cost of escalation cycles; no capacity to prevent violations or enforce ceasefire
 *   - Hardline Factions (Hamas military wings, Israeli far-right): Primary beneficiary (organized/constrained) — consolidate political control through sabotage; suppress moderate leadership by showing diplomacy fails
 *   - State Security Apparatus (Israeli military, Gaza armed groups): Secondary beneficiary (institutional/constrained) — justify budgets, expand institutional power, suppress civilian dissent under security pretext
 *   - Regional Mediators (Egypt, Qatar, UAE): Secondary victim (institutional/constrained) — must maintain ceasefire for regional stability but face extraction from both parties' manipulation
 *   - International Legal Framework (UN, ICC, humanitarian organizations): Institutional actor (institutional/arbitrage) — provides performative legitimacy; actual enforcement capacity near-zero
 *   - Moderate Leadership (Palestinian Authority, Israeli center-left): Secondary victim (moderate/constrained) — undermined by hardline sabotage; must either capitulate to hardliners or lose credibility
 *   - International Peace Movement: Organized actor (organized/mobile) — attempts temporary support with sunset logic; reallocates effort if stabilization fails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_gaza_ceasefire_violation, 0.68).
domain_priors:suppression_score(israel_gaza_ceasefire_violation, 0.78).
domain_priors:theater_ratio(israel_gaza_ceasefire_violation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, extractiveness, 0.68).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_gaza_ceasefire_violation, snare).
narrative_ontology:human_readable(israel_gaza_ceasefire_violation, "Israel-Gaza Ceasefire Violation Escalation Trap").
narrative_ontology:topic_domain(israel_gaza_ceasefire_violation, "geopolitical/conflict_resolution").

domain_priors:requires_active_enforcement(israel_gaza_ceasefire_violation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_gaza_ceasefire_violation, hardline_political_factions_israel).
narrative_ontology:constraint_beneficiary(israel_gaza_ceasefire_violation, hardline_political_factions_gaza).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, civilian_population_gaza).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, civilian_population_israel).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, ceasefire_mechanism).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped in escalation cycle with no exit option. Bears full cost of retaliatory strikes. No capacity to enforce ceasefire or prevent violations. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.77. Pure extraction with maximum coercion and zero alternatives.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL MEDIATORS (TANGLED ROPE) — Egypt, Qatar, UAE experience mixed coordination and extraction. Must maintain ceasefire for regional stability (coordination function) but face extraction from both parties' violations and manipulation. Constrained exit: cannot simply withdraw mediation without regional destabilization. d≈0.68, f(d)≈0.95, σ=0.9 → χ≈0.63. Asymmetric extraction masks underlying coordination role.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HARDLINE FACTIONS / SPOILERS (SNARE) — Both Hamas hardliners and Israeli right-wing factions benefit from ceasefire violations. Use rockets/strikes to sabotage diplomatic resolution and consolidate political power. Constrained exit: cannot openly advocate for war resumption without political cost. Classify as snare from the perspective of moderates who become trapped by factional escalation. d≈0.75, f(d)≈1.10, σ=0.8 → χ≈0.59. Effective extractors who suppress moderate alternatives.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, snare,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ISRAEL STATE SECURITY APPARATUS (TANGLED ROPE) — Must maintain deterrence credibility (coordination function) through proportional response to violations. But extraction occurs: security establishment consolidates institutional power, justifies military budgets, suppresses civilian dissent on grounds of security. d≈0.45, f(d)≈0.40, σ=1.0 → χ≈0.27. Hybrid: real coordination role shadowed by institutional extraction.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HAMAS/GAZA AUTHORITY (TANGLED ROPE) — Must maintain ceasefire credibility for governance (coordination function). But extraction occurs: factions extract control from moderate leadership, use violations to consolidate power against Fatah/Palestinian Authority, suppress civilian opposition to militancy. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.35. Dual extraction: inter-Palestinian factional and external deterrence.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Formal ceasefire mechanisms, UN resolutions, international humanitarian law provide performative legitimacy (theater_ratio=0.65). Actual enforcement capacity is near-zero: no mechanism to prevent violations or compel de-escalation. Persists through institutional inertia and diplomatic theater despite demonstrated ineffectiveness. Theater exceeds function.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL PEACE ACTORS (SCAFFOLD) — NGOs, peace-building organizations, Track 2 diplomacy actors see escalation as a temporary failure with sunset potential. Access to both sides enables shuttle diplomacy and confidence-building. Mobile exit: can redirect funding/effort to neighboring conflicts if this one degrades beyond repair. χ≤0.30, theater≤0.70. Temporary support with genuine sunset logic: 'if we don't stabilize in X years, we pivot to Syria or Yemen.' Has_sunset_clause rationale: international peace fatigue sets renewal deadlines for major funding cycles every 3-5 years.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN?) — Risk of misclassification as natural law. Realist IR scholars argue that ceasefire violations are inevitable structural products of anarchy (no enforcer), mutual distrust, and security dilemmas — immutable features of interstate/non-state conflict. ε=0.68, suppression=0.78 contradict mountain gates; false summit. The constraint is contingent on factional politics and weak enforcement institutions, not immutable.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_gaza_ceasefire_violation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_gaza_ceasefire_violation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_gaza_ceasefire_violation, TR),
    TR >= 0.70.

:- end_tests(israel_gaza_ceasefire_violation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Hardline factions extract political control, institutional power, and security budgets from the civilian population by systematically sabotaging ceasefire agreements. The extraction is not accidental or incidental — it is the primary function for spoiler factions. Initial value (0.42) reflects the ceasefire period itself: violations are sporadic, extraction is latent but present. Final value (0.68) reflects sustained escalation cycle where violations become normalized and institutional extraction (military budgets, security apparatus expansion) reaches steady state. Suppression (0.78): Very high. Civilians cannot exit the geographic theater; cannot prevent violations by factions; cannot enforce international law without state capacity; cannot publicly oppose military action without security accusations. Armed groups control information and restrict freedom of movement. Media coverage is heavily politicized. Moderate voices are suppressed by hardline intimidation. Theater ratio (0.65): Moderate-high. International legal frameworks (UN resolutions, humanitarian law) provide legitimacy theater but minimal enforcement. Ceasefire negotiations are performed repeatedly without structural conditions changing. Peace negotiations generate media theater without behavioral change. Some genuine coordination function exists (mediators do occasionally negotiate localized ceasefires), but it is overwhelmed by hardline sabotage and institutional extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival gap: civilians see pure snare (extraction with no exit); hardliners see snare for moderates but rope for themselves (coordination of factional control); institutional security sees tangled_rope (coordination function shadowed by extraction); mediators see tangled_rope with asymmetric extraction from both sides; international legal framework sees itself as providing scaffolding (sunset logic) but actually provides piton (performative theater); peace movement sees scaffold with sunset potential; analytical observer risks mountain classification (mutual distrust is structural, ceasefires always fail) but this is false summit — contingent on weak enforcement institutions and factional incentives, not immutable anarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian population: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit options. Hardline factions: Beneficiary (from resumed conflict) + constrained → d≈0.35, f(d)≈0.25. Low effective extraction from hardlines' perspective because they benefit from the constraint despite nominal constraint status. State security apparatus (Israel): Beneficiary (budget expansion, institutional power) + constrained → d≈0.40, f(d)≈0.40. Institutional extraction through security theater. Gaza armed groups: Complex — some benefit from anti-occupation narrative, some are extracted by Hamas control → d≈0.55, f(d)≈0.75. Regional mediators: Victim + constrained → d≈0.68, f(d)≈0.95. Extraction from manipulation by both sides. International framework: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Piton classification comes from theater gate, not directionality; the framework's primary function (coordination) has atrophied relative to performance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how a coordination problem (maintaining ceasefire) becomes captured by extraction (hardline factions sabotaging to consolidate power). The resolution is perspectival: from the civilian view, this is pure snare. From the hardline factional view, it is rope (their coordination of power consolidation). From the institutional view, it is tangled_rope (real coordination function shadowed by extraction). From the mediator view, it is snare for their role (being manipulated by both sides). The mandatrophy is resolved by the engine's perspectival decomposition: each perspective produces its classification, and the presheaf over observation sites is the complete answer. The false summit risk (realist 'anarchy makes ceasefires impossible') is caught by the engine's empirical gates: if extractiveness and suppression are contingent on weak institutions and factional choices (not immutable anarchy), the mountain classification fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    violation_attribution_ambiguity,
    'When a rocket is fired from Gaza, is it attributable to Hamas central command, local Hamas factions, Palestinian Islamic Jihad, or rogue actors claiming Hamas affiliation?',
    'Forensic analysis of weaponry, launch patterns, claimed responsibility statements; comparison with Hamas military wing (Izz ad-Din al-Qassam Brigades) direct communications; cellular network analysis of command networks during incident',
    'If attribution to central command: ceasefire holder (Hamas leadership) is violator. If attribution to rogue/other factions: Hamas can disclaim and negotiate enforcement mechanisms. Changes whether tangled_rope moderates can contain hardliners or are captured by them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violation_attribution_ambiguity, empirical, 'Whether rocket violations can be attributed to ceasefire-signatory authority vs rogue factions').

omega_variable(
    proportionality_measurement_asymmetry,
    'What metric constitutes ''proportional response''? Casualty equivalence? Infrastructure damage equivalence? Time window for response? Civilian protection standards?',
    'Ex-post analysis of response patterns: ratio of casualties to initiating act; timing of response relative to violation; civilian protection measures deployed; comparison to prior historical cycles',
    'If response metrics are ambiguous: both sides claim violations while defending as proportional. Allows extraction to persist unchecked under coordination theater. If metrics are clarified: enforcement becomes possible, snare can be converted to tangled_rope with enforceable limits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_measurement_asymmetry, conceptual, 'How proportionality is measured in escalation response').

omega_variable(
    factional_moderation_capacity,
    'Can moderate leadership in Hamas and Israeli government suppress hardline factions'' capacity to violate ceasefire unilaterally?',
    'Historical analysis of factional control: how many prior violations were prevented by central command enforcement? Security force loyalty patterns. Comparison with other multi-faction armed groups (Hezbollah, PKK) where central authority has successfully disciplined splinters.',
    'If moderation capacity exists: tangled_rope classification valid — coordination role is real, extraction is institutional capture. If moderation capacity is illusory: snare classification dominates — hardliners structurally control both sides, civilians have no exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factional_moderation_capacity, empirical, 'Whether moderate factional leadership can enforce discipline on hardline splinters').

omega_variable(
    international_enforcement_credibility,
    'Can international mediators (Egypt, Qatar, US, UN) credibly threaten sanctions, aid withdrawal, or military intervention to enforce ceasefire compliance?',
    'Prior enforcement success rates; leverage analysis of economic aid, military support, diplomatic isolation; comparison to enforcement in Cyprus, Dayton (Bosnia), or other sustained ceasefires',
    'If enforcement credibility is high: piton classification is wrong, scaffold or tangled_rope becomes operative. If enforcement is theatrical (aid leverage exists but won''t be used): piton confirmed — performative international framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_enforcement_credibility, empirical, 'Whether international mediators have credible enforcement leverage').

omega_variable(
    ceasefire_legitimacy_asymmetry,
    'Do both parties accept the ceasefire agreement''s legitimacy, or does one side view it as externally imposed, temporary truce while maintaining war aims?',
    'Discourse analysis of leadership statements; comparison of public vs private acceptance; survey data on civilian/militia acceptance; analysis of military doctrine during ceasefire period',
    'If legitimacy is symmetric: coordination problem is real, mediation can work. If asymmetric: one side uses ceasefire to rearm while claiming compliance — extraction masks as coordination, hardline sabotage is structural feature not deviation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ceasefire_legitimacy_asymmetry, conceptual, 'Whether ceasefire is viewed as legitimate agreement vs temporary truce by both parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_gaza_ceasefire_violation, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igcv_tr_t0, israel_gaza_ceasefire_violation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(igcv_tr_t6, israel_gaza_ceasefire_violation, theater_ratio, 6, 0.58).
narrative_ontology:measurement(igcv_tr_t12, israel_gaza_ceasefire_violation, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(igcv_be_t0, israel_gaza_ceasefire_violation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(igcv_be_t6, israel_gaza_ceasefire_violation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(igcv_be_t12, israel_gaza_ceasefire_violation, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_gaza_ceasefire_violation, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, palestinian_authority_delegitimation).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, regional_proxy_warfare_escalation).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, humanitarian_access_blockade).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, international_law_enforcement_credibility).

% DUAL FORMULATION NOTE:
% The ceasefire violation constraint is downstream of factional political incentives (Hamas internal competition with Palestinian Authority, Israeli right-wing opposition to territorial concessions) but represents a distinct structural constraint on civilian populations and mediating powers. Each upstream constraint has its own ε reflecting the political economy of factional competition; the ceasefire violation constraint has ε=0.68 reflecting the extraction mechanism operating through escalation cycles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_gaza_ceasefire_violation, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
