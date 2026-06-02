% ============================================================================
% CONSTRAINT STORY: factional_power_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_factional_power_consolidation, []).

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
 *   constraint_id: factional_power_consolidation
 *   human_readable: Factional Power Consolidation
 *   domain: political/organizational
 *
 * SUMMARY:
 *   Factional power consolidation represents a structural constraint that
 *   transforms organizational or political contexts through the progressive
 *   concentration of authority, resources, and decision-making power within a
 *   dominant faction. The constraint operates across multiple levels
 *   simultaneously: it solves genuine coordination problems (factions enable
 *   collective action, resource pooling, shared identity) while
 *   simultaneously extracting asymmetric benefits for leadership, suppressing
 *   alternative power centers, and degrading institutional neutrality norms.
 *   The measurement trajectory shows extractiveness rising from 0.32 to 0.68
 *   over ten time periods, with theater ratio increasing from 0.22 to 0.58 —
 *   indicating both a shift toward more extractive mechanisms and a parallel
 *   increase in performative legitimation as consolidation advances. The
 *   constraint is empirically classified as a snare from powerless and
 *   identity-locked perspectives but appears as tangled rope from organized
 *   and institutional perspectives. The analytical observer risks mistaking
 *   consolidation for immutable natural law, exemplifying false summit
 *   classification.
 *
 * KEY AGENTS:
 *   - Faction Leadership: Primary beneficiary (institutional/arbitrage) — captures disproportionate authority, resources, and decision control; can exit to neutral positions but chooses consolidation
 *   - Peripheral Faction Members: Primary victim (powerless/trapped) — bears extraction through constrained choices, social ostracism barriers, economic dependency; cannot exit
 *   - Mid-Level Operatives: Secondary victim (moderate/identity_locked) — structurally mobile but identity-fused; self-concept constituted through factional membership; epistemic capture maintains suppression
 *   - Rival Factions: Organized actor (organized/mobile) — experiences mixed coordination and extraction; can contest or exit but operates within factional system structure
 *   - Institutional Neutrality Norm: Institutional structure (institutional/constrained) — maintains performative neutrality language while actual decision-making is factionally captured; persistence through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent consolidation dynamics as immutable political law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(factional_power_consolidation, 0.68).
domain_priors:suppression_score(factional_power_consolidation, 0.72).
domain_priors:theater_ratio(factional_power_consolidation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(factional_power_consolidation, extractiveness, 0.68).
narrative_ontology:constraint_metric(factional_power_consolidation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(factional_power_consolidation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(factional_power_consolidation, snare).
narrative_ontology:human_readable(factional_power_consolidation, "Factional Power Consolidation").
narrative_ontology:topic_domain(factional_power_consolidation, "political/organizational").

domain_priors:requires_active_enforcement(factional_power_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(factional_power_consolidation, faction_leadership).
narrative_ontology:constraint_victim(factional_power_consolidation, peripheral_faction_members).
narrative_ontology:constraint_victim(factional_power_consolidation, competing_factions).
narrative_ontology:constraint_victim(factional_power_consolidation, institutional_neutrality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL FACTION MEMBER (SNARE) — Structurally trapped within factional boundaries. Social ostracism, economic dependency, and relational identity fusion make exit prohibitively costly. Maximum experienced extraction — constrained choices presented as voluntary alignment while real alternatives are suppressed through social pressure and institutional gatekeeping.
constraint_indexing:constraint_classification(factional_power_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-LEVEL FACTIONAL OPERATIVE (SNARE) — Identity fused with factional commitment. Structurally mobile (could leave) but cognitively captured — self-concept, professional legitimacy, and social standing are entirely constituted through factional membership. Exit would require becoming a different person. High suppression maintained through epistemic closure: competing narratives are filtered or reframed as threats rather than alternatives.
constraint_indexing:constraint_classification(factional_power_consolidation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: RIVAL FACTION (TANGLED ROPE) — Experiences the consolidation as both coordination threat and extractive competition. The constraint includes genuine coordination function (factional identity enables collective action, resource pooling, information networks) but is layered with asymmetric extraction (leadership concentrates resources and decision authority). Rival faction has mobility and organization — can exit or contest — but does so within a system where factional consolidation is the dominant constraint structure.
constraint_indexing:constraint_classification(factional_power_consolidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: FACTION LEADERSHIP (TANGLED ROPE) — Primary beneficiary. Experiences consolidation as effective coordination mechanism: mobilizes members, allocates resources, enforces discipline, aggregates power. Genuine coordination benefits exist (faction solves collective action problems) but are inseparable from asymmetric extraction (leadership captures disproportionate resources, authority, and decision control). Leadership has arbitrage options — can shift allegiances, exit to neutral positions — but chooses consolidation because extraction exceeds coordination costs.
constraint_indexing:constraint_classification(factional_power_consolidation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL NEUTRALITY NORM (PITON) — Formal commitment to non-partisan governance exists but is substantially performative. Institutions maintain neutral rhetoric while factional consolidation proceeds unchecked. Theater ratio reflects that neutrality language persists (formal rules, public statements, procedural theater) while actual decision-making is factionally captured. The norm persists through inertia — it is institutionalized through rules and norms that have lost functional force.
constraint_indexing:constraint_classification(factional_power_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing factional dynamics as immutable feature of political organization. From civilizational perspective, factional consolidation could appear as irreducible consequence of human group psychology or organizational structure. However, base properties contradict mountain criteria: extractiveness (0.68) exceeds mountain threshold (≤0.25), suppression (0.72) exceeds mountain threshold (≤0.05), and emerges_naturally is false. This perspective exemplifies false summit risk — treating contingent institutional arrangements as natural laws.
constraint_indexing:constraint_classification(factional_power_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(factional_power_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(factional_power_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(factional_power_consolidation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(factional_power_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(factional_power_consolidation, TR),
    TR >= 0.70.

:- end_tests(factional_power_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accelerating. The measurement trajectory shows extractiveness rising from 0.32 to 0.68 over ten periods, reflecting the consolidation process: early phases involve genuine coordination benefits (extraction moderate), but leadership increasingly concentrates authority and resources (extraction rises). Final plateau at 0.68 indicates consolidation has reached equilibrium — further extraction would destabilize the system through revolt or institutional collapse. Suppression (0.72): High. Multiple suppression mechanisms operate: formal barriers to factional exit (social ostracism, economic sanctions, institutional gatekeeping); epistemic closure (alternative narratives suppressed or reframed as threats); identity fusion (self-concept dependent on factional membership). For peripheral members, suppression is primarily coercive (trapped exit option). For mid-level operatives, suppression is primarily cognitive (identity_locked exit option). Theater ratio (0.58): Moderate-high. Consolidation is legitimated through performative institutional language (neutrality norms persist in rhetoric), procedural theater (formal rules maintained while factionally applied), and narrativization (consolidation reframed as necessary or natural). Theater is lower than in purely performative constraints (piton) because consolidation includes real coordination function and real extraction — the theater supplements rather than replaces the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The measurement trajectory shows extractiveness accelerating over time (0.32 → 0.68), but different perspectives perceive this acceleration differently. Faction leadership perceives rising extractiveness as increasing coordination effectiveness — they see themselves solving problems, aggregating power, and improving faction capacity. Peripheral members perceive the same trajectory as tightening suppression — choices narrow, exit costs rise, identity fusion deepens. Mid-level operatives may not consciously perceive the trajectory at all, experiencing each year as 'business as usual' while their epistemic frame gradually restricts toward faction-provided interpretations. Rival factions perceive the trajectory as threatening — the consolidating faction's rising extraction becomes their competition pressure. The institutional neutrality norm maintains consistent performative language throughout (theater ratio rises, but absolute neutrality rhetoric is constant), creating a gap between formal institutional position and empirical factional capture. The analytical observer risks seeing mountain stability (consolidation appears unchangeable) when actually the system is dynamically unstable — sustained by active enforcement and suppression mechanisms that would collapse if those mechanisms failed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position within the consolidation flow. Peripheral members with trapped exit and victim status have high d (~0.95) → high f(d) → high experienced χ. Mid-level operatives with identity_locked exit and mixed beneficiary/victim status have d derived from their epistemic capture (beneficiary status → lower d, but victim status in constraint → higher d); the engine derives d ≈ 0.78 reflecting the captured operative's partial benefit from factional resources and status combined with extraction in terms of autonomy. Rival factions with mobile exit and organized power have moderate d (~0.55) reflecting that they benefit from factional coordination mechanisms (resource pooling, collective action capacity) but bear extraction costs through competition and resource concentration asymmetry. Faction leadership with arbitrage exit and primary beneficiary status has low d (~0.15), producing negative effective extraction (they experience the constraint as beneficial coordination). The piton institutional norm has constrained exit (cannot easily reform institutions) but beneficiary status (the norm legitimates factional capture), yielding d ≈ 0.35. The analytical observer uses canonical d for analytical power (~0.73), mapping to moderate effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION via perspectival pluralism: This constraint demonstrates that snare and tangled rope are simultaneous truths from different positions, not competing hypotheses to be resolved. The mandatrophy question — 'Is this pure extraction (snare) or mixed coordination-extraction (tangled rope)?' — is resolved by recognizing that BOTH are structurally correct: from the perspective of powerless trapped members, the constraint IS a snare (they perceive only extraction, no benefit); from the perspective of organized rivals or institutional leadership, the constraint IS tangled rope (they perceive mixed benefits and costs). The constraint contains genuine coordination function (factions solve collective action problems, enable resource pooling, provide identity and belonging) and genuine extraction (leadership concentrates authority, suppresses alternatives, prevents institutional neutrality). The resolution is not to choose one type but to recognize that the perspectival gap itself — the fact that powerless see pure extraction while powerful see mixed function — is the diagnostic signature of the consolidation mechanism. The constraint works precisely because it can be experienced as pure coordination by beneficiaries and pure extraction by victims from the same base structure. False summit risk (mountain) is resolved by noting that extractiveness (0.68) and suppression (0.72) both violate mountain thresholds (≤0.25 and ≤0.05 respectively), and emerges_naturally is false. The apparent naturalness of factional consolidation is a narrativization effect, not a structural property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_coercion,
    'Is the binding mechanism in mid-level operatives primarily identity fusion or primarily material coercion and exit costs?',
    'Post-exit trajectory analysis: if operatives retain suppressive beliefs/behaviors after leaving the faction, binding was identity-locked; if beliefs rapidly update toward factional-neutral positions, binding was coercive/material. Comparison with exited members who maintain vs abandon factional framing.',
    'If primarily identity-locked: constraint is more stable and harder to dissolve (identity frames persist); interventions must address epistemic capture, not just exit barriers. If primarily coercive: constraint is more fragile; lowering exit costs or removing material dependencies could rapidly destabilize consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Whether suppression is identity-based or material').

omega_variable(
    rival_faction_coalition_threshold,
    'At what critical mass of organized rivals can competition shift from tangled rope (mixed coordination-extraction) to collective coordination against consolidation?',
    'Analysis of multi-faction dynamics: when do rival factions coordinate against the primary consolidating faction rather than competing individually? Measurement of coalition formation thresholds in historical factional systems.',
    'If threshold < 2 competing organized factions: consolidation is vulnerable to horizontal coalition formation. If threshold > 4: consolidation is robust because rival coordination is harder than individual competition. Changes classification dynamics for rival faction perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_faction_coalition_threshold, empirical, 'Critical mass for rival coalition formation').

omega_variable(
    institutional_capture_reversal_cost,
    'What is the institutional cost of reversing factional capture once the piton (performative neutrality) has been fully absorbed into decision-making structure?',
    'Comparison of institutional reset costs across historical cases: rule reform, personnel restructuring, cultural reprogramming time. Measurement of reversibility timeframe.',
    'If reversal cost becomes prohibitively high: piton transitions to snare for the institution itself (the institution becomes trapped). If reversal remains possible: piton classification is accurate — degraded but not trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_reversal_cost, empirical, 'Cost of institutional capture reversal').

omega_variable(
    narrativization_of_necessity,
    'Does factional consolidation eventually produce a narrative that reframes consolidation as necessary (natural law) rather than contingent (institutional choice)?',
    'Content analysis of faction leadership rhetoric over time: measurement of frequency and intensity of necessity-framing (''this is how power works,'' ''human nature demands hierarchy,'' ''organization requires clear authority''). Comparison with early consolidation phase when alternatives were still verbally available.',
    'If narrativization succeeds: consolidation moves toward false mountain classification (perceived as natural law by participants). If narrativization fails: consolidation remains perceived as extractive snare/tangled_rope. Affects sustainability and exit barrier perception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrativization_of_necessity, conceptual, 'Narrativization of consolidation as natural necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(factional_power_consolidation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpc_tr_t0, factional_power_consolidation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fpc_tr_t2, factional_power_consolidation, theater_ratio, 2, 0.35).
narrative_ontology:measurement(fpc_tr_t4, factional_power_consolidation, theater_ratio, 4, 0.45).
narrative_ontology:measurement(fpc_tr_t6, factional_power_consolidation, theater_ratio, 6, 0.55).
narrative_ontology:measurement(fpc_tr_t8, factional_power_consolidation, theater_ratio, 8, 0.58).
narrative_ontology:measurement(fpc_tr_t10, factional_power_consolidation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fpc_be_t0, factional_power_consolidation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fpc_be_t2, factional_power_consolidation, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(fpc_be_t4, factional_power_consolidation, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(fpc_be_t6, factional_power_consolidation, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(fpc_be_t8, factional_power_consolidation, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(fpc_be_t10, factional_power_consolidation, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(factional_power_consolidation, identity_coordination).
narrative_ontology:boltzmann_floor_override(factional_power_consolidation, 0.12).
narrative_ontology:affects_constraint(factional_power_consolidation, institutional_capture).
narrative_ontology:affects_constraint(factional_power_consolidation, epistemic_closure).
narrative_ontology:affects_constraint(factional_power_consolidation, hierarchical_inversion).

% DUAL FORMULATION NOTE:
% Factional power consolidation is an organizational dynamic that appears across multiple domains (political parties, religious movements, criminal organizations, academic departments, corporate divisions). This constraint story captures the generic structure; domain-specific instantiations will have the same ε and suppression profile but may vary in theater_ratio and temporal trajectory. Related constraints: institutional_capture (consolidation enables capture), epistemic_closure (consolidation produces closure), hierarchical_inversion (consolidation inverts nominal authority structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(factional_power_consolidation, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
