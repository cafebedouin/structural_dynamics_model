% ============================================================================
% CONSTRAINT STORY: parliamentary_supermajority_amendment_requirements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliamentary_supermajority_amendment_requirements, []).

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
 *   constraint_id: parliamentary_supermajority_amendment_requirements
 *   human_readable: Parliamentary Supermajority Amendment Requirements
 *   domain: political_governance/constitutional_law
 *
 * SUMMARY:
 *   Parliamentary supermajority amendment requirements create a structural
 *   constraint that simultaneously enables constitutional stability and
 *   blocks democratic reform. The constraint operates between institutional
 *   actors with fundamentally different relationships to constitutional
 *   change: those defending existing rules benefit from high barriers; those
 *   elected on reform mandates are trapped behind those barriers. This
 *   constraint exemplifies how the same rule can appear as pure coordination
 *   (rope), mixed coordination-extraction (tangled_rope), pure extraction
 *   (snare), temporary structure (scaffold), degraded ritual (piton), or
 *   immutable natural law (mountain) depending on the observer's structural
 *   position relative to constitutional change. The constraint's
 *   extractiveness has increased over the 50-year measurement interval as
 *   democratic reform demands have grown while supermajority thresholds
 *   remained static, indicating that the constraint's extraction mechanism
 *   (slowing pace of constitutional change) has accumulated. Theater ratio
 *   has increased as mature democracies increasingly use informal workarounds
 *   (executive reinterpretation, procedure manipulation) rather than formal
 *   amendments, suggesting the constraint is degrading into theater in
 *   democracies with high institutional development.
 *
 * KEY AGENTS:
 *   - Reform-Minded Electorate: Primary victim (powerless/trapped) — elected supermajorities with reform mandates find constitutional barriers immovable
 *   - Majority Coalition: Secondary victim (organized/constrained) — holds simple majority but cannot implement platform; forced into consensus-building to reach supermajority
 *   - Institutional Continuity Advocates: Primary beneficiary (institutional/arbitrage) — benefits from high barriers to constitutional change; can shift between coalition positions depending on which is supermajority
 *   - Minority Rights Protection: Secondary beneficiary (institutional/arbitrage) — supermajority requirement prevents tyranny of simple majority; can arbitrage between protecting rights and enabling reform
 *   - Post-Authoritarian States: Temporary beneficiary (moderate/constrained) — supermajority requirements prevent revenge cycles during democratic transition; have explicit sunset as norms mature
 *   - Constitutional Formalism: Institutional actor (institutional/arbitrage) — maintains the rule through ritualistic reverence; benefits from its existence even as functional use declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliamentary_supermajority_amendment_requirements, 0.52).
domain_priors:suppression_score(parliamentary_supermajority_amendment_requirements, 0.68).
domain_priors:theater_ratio(parliamentary_supermajority_amendment_requirements, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliamentary_supermajority_amendment_requirements, extractiveness, 0.52).
narrative_ontology:constraint_metric(parliamentary_supermajority_amendment_requirements, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(parliamentary_supermajority_amendment_requirements, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliamentary_supermajority_amendment_requirements, tangled_rope).
narrative_ontology:human_readable(parliamentary_supermajority_amendment_requirements, "Parliamentary Supermajority Amendment Requirements").
narrative_ontology:topic_domain(parliamentary_supermajority_amendment_requirements, "political_governance/constitutional_law").

domain_priors:requires_active_enforcement(parliamentary_supermajority_amendment_requirements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliamentary_supermajority_amendment_requirements, coalition_builders).
narrative_ontology:constraint_beneficiary(parliamentary_supermajority_amendment_requirements, institutional_continuity_advocates).
narrative_ontology:constraint_beneficiary(parliamentary_supermajority_amendment_requirements, minority_rights_protection).
narrative_ontology:constraint_victim(parliamentary_supermajority_amendment_requirements, majority_coalitions).
narrative_ontology:constraint_victim(parliamentary_supermajority_amendment_requirements, rapid_reform_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM-MINDED ELECTORATE (SNARE) — Voters who elect a supermajority mandating change find their mandate locked behind the supermajority amendment requirement. They cannot exit: the constraint persists regardless of electoral outcome. Bears full cost of constitutional gridlock with no recourse except future supermajorities.
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJORITY COALITION (TANGLED ROPE) — Benefits from coordination: supermajority rules force consensus-building and prevent swinging constitutional pendulum. But simultaneously trapped: holds a simple majority yet cannot govern unilaterally. Genuine coordination function (broad consensus prevents tyranny) with genuine asymmetric extraction (majority cannot implement elected platform).
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL CONTINUITY ADVOCATES (ROPE) — Experiences the constraint as pure coordination. Supermajority requirement prevents destabilizing constitutional churn. Can arbitrage: shifting between 'reform' and 'preservation' coalitions depending on which is supermajority. Net beneficiary — the constraint solves their coordination problem (preventing radical pendulum swings) without meaningful cost.
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSITIONAL DEMOCRACY BUILDERS (SCAFFOLD) — In post-authoritarian contexts, supermajority requirements are intentional sunset structures: high thresholds during transition period force consensus and prevent revenge cycles, but are designed to relax as democratic norms mature. Theater ratio low — the mechanism is explicitly functional, not performative. Exit path: as democratic institutions mature, supermajority thresholds often drop or become negotiable.
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, scaffold,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL FORMALISM (PITON) — The supermajority requirement persists as a degraded institution in mature democracies where norms have evolved to achieve consensus without formal threshold enforcement. The rule remains but is increasingly circumvented through informal mechanisms (backdoor negotiations, executive orders reframing as non-constitutional, chamber procedure manipulation). Theater ratio high — the rule is maintained through ritualistic reverence for 'constitutional stability' even as its functional role has atrophied.
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At civilizational scale with universal scope, supermajority requirements appear as an immutable structural constraint on constitutional change: any system requiring consensus for fundamental rules must have high barriers to modification, or the rules themselves dissolve. This perspective sees the constraint as a natural law of governance. However, structural data reveals this as false naturalization: supermajority thresholds vary widely across democracies (two-thirds, three-fifths, unanimous minus one), demonstrating contingency rather than natural law.
constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliamentary_supermajority_amendment_requirements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliamentary_supermajority_amendment_requirements, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parliamentary_supermajority_amendment_requirements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parliamentary_supermajority_amendment_requirements, TR),
    TR >= 0.70.

:- end_tests(parliamentary_supermajority_amendment_requirements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The supermajority requirement slows constitutional change relative to electoral mandates. Measuring extractiveness at the national scope reveals asymmetric power: majorities cannot unilaterally govern, minorities can block change. The measurement has increased over time (0.35→0.52) because reform pressure has grown while thresholds remain static, indicating accumulation of extraction. Suppression (0.68): Moderate-high. Significant barriers to exit: voters cannot overturn the supermajority rule itself (you need a supermajority to amend the supermajority requirement — circular trap). However, suppression is not absolute: informal workarounds exist (executive orders, reinterpretation), and some democracies have renegotiated thresholds through political commitment. Theater ratio (0.35): Low-moderate. The mechanism is substantially functional in maintaining constitutional stability, but has degraded: mature democracies increasingly bypass formal amendment processes through informal mechanisms. The rise from 0.15 to 0.35 indicates increasing reliance on theatrical workarounds rather than genuine structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The reform electorate sees a snare: they are locked behind a barrier they cannot exit. The majority coalition sees tangled_rope: forced consensus-building that prevents their full agenda while enabling cross-coalition negotiation. Institutional continuity advocates see rope: a pure coordination mechanism that solves the problem of constitutional stability. Post-authoritarian states see scaffold: a temporary structure with explicit sunset. Constitutional formalism sees piton: a rule maintained through ritual despite atrophied function. The analytical observer at civilizational scope risks seeing mountain: immutable natural law that any constitutional system requires high barriers to self-modification. But the structural data contradicts this: empirical variation across democracies (two-thirds, three-fifths, unanimous minus one) demonstrates contingency, revealing the mountain classification as false naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent and context. Reform-minded voters facing a trapped exit experience high d (→0.95) because they are victims bearing full cost with no alternatives. Majority coalitions with constrained but not trapped exits experience moderate-high d (→0.65) because they have some agency (can build supermajorities) but limited unilateral power. Institutional continuity advocates with arbitrage exits experience low d (→0.15) because they can shift coalition positions and benefit from the rule's existence. The spread in d values (0.15 to 0.95) produces a perspectival gap: the same constraint yields snare (powerless), tangled_rope (organized), and rope (institutional) classifications from different structural positions. This gap is diagnostic: if all agents perceived the same classification, the constraint's asymmetry would be obscured.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the perspectival gap IS the analytical content. The question 'is supermajority amendment a good institution?' collapses into: 'from whose structural position?' Powerless agents see trap. Organized agents see constraint on their power. Institutional defenders see stability mechanism. Post-authoritarian builders see temporary safety device. Constitutionalists see degraded ritual. No single type is correct — the presheaf over observation positions is the answer. The constraint's increasing extractiveness (0.35→0.52) and theater ratio (0.15→0.35) over the 50-year interval signal that the institutional tradeoff has shifted: the stability benefit (coordination) has not grown, but the reform blockage cost (extraction) has grown because reform demand has increased. This temporal pattern is diagnostic: if extractiveness is rising while beneficiary satisfaction is stable, the constraint is drifting toward snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_ambiguity,
    'What supermajority threshold (two-thirds, three-fifths, unanimous) represents genuine coordination burden vs arbitrary extraction barrier?',
    'Comparative institutional analysis across democracies with different thresholds; measurement of amendment passage rates vs reform demand; correlation with democratic satisfaction and institutional stability',
    'Low threshold: appears as rope (coordination mechanism). High threshold: appears as snare (extraction barrier). The empirical facts about amendment success rates would calibrate extraction measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_calibration_ambiguity, empirical, 'Threshold calibration distinguishing coordination from extraction').

omega_variable(
    informal_circumvention_scope,
    'Do executive orders, constitutional reinterpretation, and procedure manipulation constitute functional exits around supermajority requirements, or do they represent the constraint degrading into theater?',
    'Longitudinal analysis of amendment attempts vs reframed policy; measurement of executive power growth in supermajority-constrained systems; comparison of formal amendment rates to substantive constitutional change rates',
    'If exits are effective: constraint is piton (formally maintained, functionally atrophied). If exits are constrained: constraint remains snare or tangled_rope. Piton classification depends on circumvention actually working.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_circumvention_scope, empirical, 'Whether informal mechanisms effectively circumvent supermajority requirements').

omega_variable(
    coalition_signaling_equilibrium,
    'Does the supermajority requirement force genuinely broader consensus (coordination) or does it merely require strategic coalition-signaling that produces illusory consensus?',
    'Policy divergence analysis: measure how much winning coalitions differ from single-party platforms; compare post-amendment legislative coherence across high-threshold vs low-threshold systems; analyze whether amendments passed under supermajority have greater durability than regular legislation',
    'If broad consensus is genuine: rope/tangled_rope (real coordination benefit). If illusory (minorities co-opted for votes but excluded from negotiation): snare (extraction through forced coalition participation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_signaling_equilibrium, empirical, 'Whether supermajority produces genuine consensus or strategic signaling').

omega_variable(
    constitutional_mutation_boundary,
    'At what point does constitutional reinterpretation or procedure change constitute a de facto amendment that should trigger supermajority requirements but does not?',
    'Doctrinal analysis of constitutional reinterpretations that produced transformative effects equivalent to formal amendments; comparison of intended application vs actual application of supermajority rules',
    'If boundary is clear and enforced: supermajority rule is mountain (immutable). If boundary is fluid and manipulated: rule is piton (theater) or snare (extraction through definitional control).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_mutation_boundary, conceptual, 'Boundary between amendment and reinterpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliamentary_supermajority_amendment_requirements, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parlsup_tr_t0, parliamentary_supermajority_amendment_requirements, theater_ratio, 0, 0.15).
narrative_ontology:measurement(parlsup_tr_t25, parliamentary_supermajority_amendment_requirements, theater_ratio, 25, 0.28).
narrative_ontology:measurement(parlsup_tr_t50, parliamentary_supermajority_amendment_requirements, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(parlsup_be_t0, parliamentary_supermajority_amendment_requirements, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(parlsup_be_t25, parliamentary_supermajority_amendment_requirements, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(parlsup_be_t50, parliamentary_supermajority_amendment_requirements, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliamentary_supermajority_amendment_requirements, enforcement_mechanism).
narrative_ontology:affects_constraint(parliamentary_supermajority_amendment_requirements, constitutional_amendment_deadlock).
narrative_ontology:affects_constraint(parliamentary_supermajority_amendment_requirements, minority_veto_power).
narrative_ontology:affects_constraint(parliamentary_supermajority_amendment_requirements, democratic_responsiveness_lag).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parliamentary_supermajority_amendment_requirements, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
