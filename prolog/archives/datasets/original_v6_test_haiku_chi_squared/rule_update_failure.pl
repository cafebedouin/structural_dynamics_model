% ============================================================================
% CONSTRAINT STORY: rule_update_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rule_update_failure, []).

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
 *   constraint_id: rule_update_failure
 *   human_readable: Obsolete Protocol Enforcement
 *   domain: technological/social
 *
 * SUMMARY:
 *   Obsolete protocol enforcement occurs when a system continues to mandate
 *   compliance with a rule designed to solve a problem that no longer exists,
 *   yet enforcement persists due to institutional inertia, hidden
 *   dependencies, or capture by enforcement institutions. The constraint
 *   spans multiple domains: legacy software requiring backward compatibility
 *   with deprecated APIs; regulatory compliance mandates that outlive the
 *   risks they address; organizational policies that persist after the
 *   conditions they were designed to manage have changed; and communication
 *   protocols that maintain redundant acknowledgment procedures originally
 *   required by hardware limitations that no longer exist. The structural
 *   tension is between the legitimate coordination function of having
 *   consistent, predictable rules across a system and the extractive cost
 *   imposed by rules that serve no current purpose. End users face
 *   suppression (compliance overhead with zero benefit), while enforcement
 *   institutions benefit from job security and audit scope. System innovators
 *   face constrained choices: maintain expensive backward compatibility or
 *   risk breaking systems that depend on legacy compliance. This constraint
 *   exhibits all six types from different perspectives, revealing how
 *   institutional inertia can transform a legitimate coordination mechanism
 *   (rope) into a persistent extraction mechanism (snare) as the
 *   environmental context that justified the rule shifts.
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — bound by rules that no longer serve coordination; cannot exit system without abandoning functionality
 *   - System Innovators: Secondary victim (moderate/constrained) — must maintain backward compatibility overhead; face resource constraints and architecture lock-in
 *   - Legacy System Maintainers: Primary beneficiary (institutional/arbitrage) — gain job security and credibility from enforcing known, auditable rules; experience constraint as predictable coordination
 *   - Compliance Audit Institutions: Secondary beneficiary (institutional/arbitrage) — maintain enforcement scope and budget through continued rule checking; see compliance as their function regardless of rule utility
 *   - Standards Committee: Organized coordinator (organized/mobile) — can establish deprecation timelines and migration paths; has agency to sunset rules incrementally
 *   - Operational Efficiency: Victim (analytical/trapped) — abstract aggregate cost of redundant enforcement accumulates across system; nobody directly represents it
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing rule persistence as necessary law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rule_update_failure, 0.52).
domain_priors:suppression_score(rule_update_failure, 0.68).
domain_priors:theater_ratio(rule_update_failure, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rule_update_failure, extractiveness, 0.52).
narrative_ontology:constraint_metric(rule_update_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rule_update_failure, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rule_update_failure, tangled_rope).
narrative_ontology:human_readable(rule_update_failure, "Obsolete Protocol Enforcement").
narrative_ontology:topic_domain(rule_update_failure, "technological/social").

domain_priors:requires_active_enforcement(rule_update_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rule_update_failure, legacy_system_maintainers).
narrative_ontology:constraint_beneficiary(rule_update_failure, institutional_compliance_auditors).
narrative_ontology:constraint_victim(rule_update_failure, end_users).
narrative_ontology:constraint_victim(rule_update_failure, system_innovators).
narrative_ontology:constraint_victim(rule_update_failure, operational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Bound by rules that no longer serve legitimate coordination. Cannot exit system; bears full cost of outdated enforcement. Verification requires compliance overhead with zero functional benefit. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(rule_update_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM INNOVATOR (TANGLED ROPE) — Constrained by requirement to maintain backward compatibility with obsolete rules. Benefits from coordination legacy but extraction through version-lock overhead. d≈0.72, f(d)≈1.08, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(rule_update_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY SYSTEM MAINTAINER (ROPE) — Benefits from continued enforcement through job security and regulatory compliance credibility. Experiences constraint as coordination: enforcing known rules reduces uncertainty. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(rule_update_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS COMMITTEE (SCAFFOLD) — Organized actors can establish deprecation timelines and migration paths. Sees obsolete enforcement as temporary coordination problem with planned sunset. d≈0.38, f(d)≈0.38, σ=1.2 → χ≈0.19. Low extraction through agency and exit path.
constraint_indexing:constraint_classification(rule_update_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE AUDIT SYSTEM (PITON) — Enforcement ritual persists despite loss of functional purpose. Auditors continue checking obsolete rules because the audit procedure itself became institutionalized independent of the rule's original rationale. theater_ratio=0.81 exceeds piton gate (≥0.70). System maintains itself through inertia.
constraint_indexing:constraint_classification(rule_update_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (APPARENT MOUNTAIN) — Observer risks misclassifying obsolete enforcement as an immutable law: 'All systems must maintain backward compatibility forever.' But extractiveness=0.52 and suppression=0.68 contradict the mountain requirements (ε≤0.25, suppression≤0.05). This is a false summit: backward compatibility is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(rule_update_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rule_update_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rule_update_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rule_update_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rule_update_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rule_update_failure, TR),
    TR >= 0.70.

:- end_tests(rule_update_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes real compliance costs on end users and innovators, but the extraction is not total because some legacy compliance requirements do serve residual coordination functions (version compatibility). The original problem has diminished, but not disappeared entirely. The value reflects the gap between original justification and current utility. Suppression (0.68): Moderately high. Users face significant barriers to non-compliance: system integration dependencies, vendor requirements, regulatory mandates. They cannot easily exit. Workarounds exist but carry hidden costs. Theater ratio (0.81): Very high. Compliance auditing for obsolete rules becomes substantially performative — auditors verify compliance with rules whose original purpose has ceased, conducting ritualized checks that follow procedures rather than addressing current risks. The theater has increased over time as the original functional rationale faded while the enforcement procedure became institutionalized. Measurement trajectory shows classic piton degradation: theater rising as extractiveness rises suggests institutional decay (enforcement persisting despite lost function).
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is stark. Legacy system maintainers see a rope (predictable coordination, manageable rules, clear audit procedures). End users see a snare (immovable barriers to efficiency with no purpose). System innovators see tangled rope (coordination benefits from compatibility offset by extraction costs of maintenance). The standards committee sees a solvable scaffold (they can write deprecation timelines). The compliance audit system sees a piton (the ritual persists through institutional momentum, no longer connected to the original risk). The civilizational observer risks seeing mountain (concluding that backward compatibility is eternally necessary), but the rising extractiveness and theater ratio over time reveal that this is not a natural law — it is an institutional arrangement that has decoupled from its justification.
 *
 * DIRECTIONALITY LOGIC:
 *   End users: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — no meaningful exit options, full cost bearer. Legacy system maintainers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with low directionality; can define what 'compliance' means. System innovators: Victim + constrained → d≈0.72, f(d)≈1.08. High extraction but not maximum; can work around constraints at cost. Compliance auditors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Their institutional interest lies in continued rule existence. Standards committee: Organized + mobile → d≈0.38, f(d)≈0.38. Low effective extraction through agency. Operational efficiency: Abstract victim → d≈0.95, f(d)≈1.42. Cannot organize to represent itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is: IS BACKWARD COMPATIBILITY A COORDINATION REQUIREMENT (ROPE) OR AN EXTRACTION MECHANISM (SNARE)? The answer is BOTH, but in different proportions at different timescales. At t=0, when the protocol change occurs and systems still depend on the old rule, enforcement is legitimate coordination (rope): breaking compatibility would cause real cascade failures. At t=4-8, when adaptation has occurred in most systems and only legacy hold-outs remain, enforcement becomes increasingly extractive (snare): the rule now prevents rather than enables coordination. The constraint resolves the mandatrophy through TIME-DEPENDENT CLASSIFICATION: early enforcement is rope, late enforcement is snare, and the transition happens when usage falls below a threshold. The measurements show this transition: extractiveness rises from 0.28 to 0.52, theater rises from 0.35 to 0.81. The high theater indicates piton degradation — the audit procedure persists independent of functional justification. The false summit perspective (mountain) is definitively rejected: if backward compatibility were immutable, theater would remain constant (ritual would maintain original function). Instead, rising theater while extractiveness rises indicates institutional decay. The constraint is a DEGRADING TANGLED ROPE: it maintains some coordination function (legacy systems do still need compatibility) but increasingly functions as extraction (for systems that have already adapted, compliance is pure overhead). The scaffold perspective (sunset) is actionable: standards committees CAN establish deprecation timelines. The mandatrophy is resolved by recognizing time-dependent function: the same rule is honest coordination early and dishonest extraction late.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_obsolescence_threshold,
    'At what point does a rule transition from ''still has some users'' to ''actively harmful coordination cost exceeds coordination benefit''?',
    'User impact analysis: measure transaction costs imposed by compliance vs measured benefits of interoperability. Threshold: cost/benefit ratio exceeds 5:1.',
    'If threshold set too high: rules persist as snares. If too low: premature deprecation breaks legitimate legacy systems. Mandatrophy hinges on this measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_obsolescence_threshold, empirical, 'Functional obsolescence threshold for rule deprecation').

omega_variable(
    migration_path_feasibility,
    'Can systems migrate away from obsolete enforcement without catastrophic compatibility breakage? Is the transition actually gradual (scaffold) or effectively impossible (snare)?',
    'Pilot deprecation in controlled environments; measure system failures, user friction, and migration costs. Test whether deprecation warnings + extended timelines enable smooth transitions.',
    'If feasible: constraint is a degrading scaffold (sunset is real, structure is honest). If infeasible: constraint is a perpetual snare (exit options were overstated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(migration_path_feasibility, empirical, 'Whether migration from obsolete rules is feasible').

omega_variable(
    hidden_dependency_proliferation,
    'Do systems that appear to ''just enforce an old rule'' actually depend on unstated assumptions embedded in that rule? Are there hidden dependencies that make removal catastrophic even if surface-level compliance seems obsolete?',
    'Dependency graph analysis: trace all systems that reference the rule, not just those that explicitly comply. Identify implicit assumptions. Test rule removal in isolation to detect cascade failures.',
    'If hidden dependencies are pervasive: ''obsolete'' classification is false — the rule is still functional but poorly understood (tangled rope). If dependencies are minimal: enforcement is purely extractive (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_dependency_proliferation, empirical, 'Proliferation of hidden dependencies on apparently obsolete rules').

omega_variable(
    enforcement_beneficiary_capture,
    'Have enforcement institutions (auditors, compliance officers, vendors) developed vested interests in rule persistence independent of the rule''s original purpose?',
    'Institutional history analysis: trace how compliance audit procedures evolved. Measure proportion of auditor resource allocation to obsolete rules. Interview stakeholders about whether rule deprecation threatens their roles.',
    'If capture is high: extractive suppression is maintained by institutional inertia (piton/snare hybrid). If capture is low: rule persistence is legitimate coordination cost (rope/scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_beneficiary_capture, empirical, 'Institutional capture of obsolete rule enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rule_update_failure, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ruf_tr_t0, rule_update_failure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ruf_tr_t2, rule_update_failure, theater_ratio, 2, 0.52).
narrative_ontology:measurement(ruf_tr_t4, rule_update_failure, theater_ratio, 4, 0.68).
narrative_ontology:measurement(ruf_tr_t6, rule_update_failure, theater_ratio, 6, 0.75).
narrative_ontology:measurement(ruf_tr_t8, rule_update_failure, theater_ratio, 8, 0.81).

% Extraction over time
narrative_ontology:measurement(ruf_be_t0, rule_update_failure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ruf_be_t2, rule_update_failure, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(ruf_be_t4, rule_update_failure, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ruf_be_t6, rule_update_failure, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rule_update_failure, information_standard).
narrative_ontology:affects_constraint(rule_update_failure, technical_debt_accumulation).
narrative_ontology:affects_constraint(rule_update_failure, vendor_lock_in_persistence).

% DUAL FORMULATION NOTE:
% Obsolete protocol enforcement is downstream of system evolution and environmental change. The upstream constraint (technical debt accumulation) represents the structural drift that makes rules obsolete; the downstream constraint (vendor lock-in persistence) represents how enforcement of obsolete rules enables vendor capture. These three constraints form a family where each affects the others' ε values: as technical debt rises, rules become more obsolete; as obsolete rules persist, vendor lock-in deepens.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rule_update_failure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
