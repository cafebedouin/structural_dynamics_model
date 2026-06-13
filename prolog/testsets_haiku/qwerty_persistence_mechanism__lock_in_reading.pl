% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Path-Dependent Coordination Lock-In
 *   domain: economic/technological
 *
 * SUMMARY:
 *   QWERTY keyboard layout persists globally despite documented technical
 *   inferiority (higher finger travel, ergonomic burden, carpal-tunnel
 *   association) relative to alternatives like Dvorak and Colemak. This story
 *   instantiates the LOCK-IN READING of the contested kernel: QWERTY persists
 *   through path-dependent coordination failure—rational individual decisions
 *   (users learn the standard because it is universal; manufacturers produce
 *   QWERTY because demand locks around it) produce collectively suboptimal
 *   outcomes. No identifiable beneficiary actively maintains the standard for
 *   rent extraction; instead, users bear the efficiency cost to participate
 *   in coordination, and no individual has sufficient incentive to switch
 *   unilaterally. The constraint is not maintained by coercion but by the
 *   economics of learning, compatibility, and switching cost.
 *
 * KEY AGENTS:
 *   - keyboard_users: absorb the ergonomic and productivity cost; identity-locked through training and professional investment
 *   - keyboard_manufacturers: enforce through supply-side lock (only produce QWERTY at scale), but are themselves constrained by demand expectations
 *   - software_systems: benefit from a single default, reducing implementation complexity
 *   - alternative_layout_advocates: excluded from the coordination; objections come too late to shift equilibrium
 *   - coordination_analysis_community: external observer studying the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.31).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.18).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Path-Dependent Coordination Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '0e7fda7c-9642-47c0-b85a-9ff862a87afc').
narrative_ontology:cs_kernel_codification('0e7fda7c-9642-47c0-b85a-9ff862a87afc', distributed).
narrative_ontology:cs_authority_grounding('0e7fda7c-9642-47c0-b85a-9ff862a87afc', distributed).
narrative_ontology:cs_reading_relation('0e7fda7c-9642-47c0-b85a-9ff862a87afc', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e7fda7c-9642-47c0-b85a-9ff862a87afc', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('0e7fda7c-9642-47c0-b85a-9ff862a87afc', foundational, rational_individual_switching_cost_traps_collective_optimum).
narrative_ontology:cs_axiom_status(rational_individual_switching_cost_traps_collective_optimum, holdable).
narrative_ontology:cs_axiom_grounding('0e7fda7c-9642-47c0-b85a-9ff862a87afc', rational_individual_switching_cost_traps_collective_optimum, empirically_contingent).
narrative_ontology:cs_axiom('0e7fda7c-9642-47c0-b85a-9ff862a87afc', foundational, no_centralized_beneficiary_maintaining_standard).
narrative_ontology:cs_axiom_status(no_centralized_beneficiary_maintaining_standard, holdable).
narrative_ontology:cs_axiom_grounding('0e7fda7c-9642-47c0-b85a-9ff862a87afc', no_centralized_beneficiary_maintaining_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('0e7fda7c-9642-47c0-b85a-9ff862a87afc', mechanical_constraint_solution_era).
narrative_ontology:cs_drift_state('0e7fda7c-9642-47c0-b85a-9ff862a87afc', digital_computing_maturity, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('0e7fda7c-9642-47c0-b85a-9ff862a87afc', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at present) because no central agent collects rents—the cost is diffuse, absorbed by users who learn QWERTY without choice. Suppression is low (0.18) because the constraint operates through passive coordination, not active coercion; users are not prevented from learning Dvorak, but they have no incentive to bear the individual switching cost when everyone else uses QWERTY. Theater is minimal (0.12) because there is little performative maintenance—the standard simply persists through inertia and network effects. The measurement series show extractiveness rising modestly from 1873 to ~1980 (as digital computing scaled and QWERTY was inherited without re-evaluation), then plateauing (no further lock-in intensification, just stasis). Suppression rises similarly, plateauing when digital systems matured enough to support layout-switching but chose not to force it. The shared time grid anchors all three metrics at the same observation points.
 *
 * PERSPECTIVAL GAP:
 *   Keyboard users and alternative-layout advocates experience the constraint as costly, but their divergent exit options produce different perceptions: users see it as natural/inevitable (identity-locked, switching cost too high); advocates see it as market failure. Manufacturers experience the constraint as benign coordination (producing the expected standard reduces risk), not as oppressive. The engine should compute this divergence from the structural data: low power + identity-locked exit drives high directionality (user as target) despite no beneficiary; institutional agenda-setter with constrained exit moderates their extraction assessment. No beneficiary is declared because no agent collects the lock-in's value—it dissipates as coordination surplus that avoids fragmentation, not as extracted rent.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard users (powerless + identity_locked + absorbed cost) compute as near full-target end of directionality (d near 1.0) despite low extractiveness—they bear the cost and cannot exit. Manufacturers (organized + constrained exit + no collection) compute as near-symmetric or slightly-beneficiary (d near 0.5) because they avoid costly inventory diversification through the standard but do not profit from it. Software systems (institutional + passive beneficiary) compute as beneficiary-seat (d near 0.0) because they save implementation cost without active maintenance. Alternative-layout advocates have no seat in the constraint (role=excluded) so directionality is inapplicable. This structure—low extraction but asymmetric cost-bearing—is diagnostic of coordination failure without exploitation, not snare or tangled_rope dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typewriter type-bar collision avoidance) is DEAD—it ceased to be relevant around 1980 when digital keyboards became dominant. Yet the constraint persists because the coordination it provides (single universal standard) is still valuable, even though no one maintains it for its original reason. This is the core mandatrophy signature: the original mandate (solve mechanical constraint) is obsolete, but the constraint's persistence is justified by a new mandate (coordination on a single standard). The newer mandate is real but weaker—it could be satisfied by alternative layouts if they reached critical mass. Classifying this as pure rope understates the extraction (users genuinely absorb suboptimal ergonomics); classifying it as snare overstates it (no beneficiary is extracting). The lock-in reading positions it as rope-with-failure: genuine coordination function, but coordination failure because individual incentives trap the system in a suboptimal equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_superiority_claim,
    'Is the superiority of Dvorak and Colemak layouts over QWERTY empirically established, or contested/context-dependent?',
    'Controlled studies comparing learning curves, typing speed, ergonomic strain, and long-term injury rates across populations (varied expertise, languages, task profiles). Meta-analysis of published studies to assess effect size homogeneity.',
    'If superiority is robust across contexts, lock-in reading holds—the coordination persists despite inferiority. If superiority is marginal or task-dependent, the naturalization reading gains ground—QWERTY may be adequate and alternatives'' failure reflects fair competition, not lock-in. If superiority is context-sensitive (Dvorak better for English prose, QWERTY better for code entry), the constraint decomposes into multiple reading contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_superiority_claim, empirical, 'Whether alternative layouts are empirically superior or the superiority claim is overstated.').

omega_variable(
    critical_mass_threshold_switchability,
    'What is the minimum market share alternative layout would need to achieve for a self-sustaining switch to become likely? Is that threshold technically reachable?',
    'Agent-based modeling of adoption dynamics; historical case study of standards switches (QWERTY->Dvorak in Esperanto communities, regional keyboard shifts). Natural experiment from jurisdictions attempting layout mandates (Korea, Russia have experimented with native layouts).',
    'If threshold is reachable with plausible intervention (policy, network seeding), lock-in reading supports remedial scenarios. If threshold is dynamically unstable (whenever a challenger rises, incumbent users block at 10–15% share), lock-in is robust and structural. If threshold has been reached and still failed to switch, the beneficiary_extraction reading gains ground—active suppression may be present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_threshold_switchability, empirical, 'Whether path-dependence is locked in or merely resistant to switching.').

omega_variable(
    identity_lock_vs_rational_choice,
    'Is users'' inability to switch QWERTY rooted in internalized identity (muscle memory, professional self-concept) or in rational cost-benefit calculus where switching cost exceeds personal lifetime benefit?',
    'Survey and interview data from users who attempted to switch layouts: what proportion cite identity-fusion barriers vs. economic switching cost? Retraining studies isolating learning curve effects from identity effects.',
    'If mostly identity-locked, the constraint has internalized suppression that persists even after switching infrastructure improves—the target (users) carries the suppression with them. If mostly rational-choice, improving switching infrastructure and social coordination could shift equilibrium. This informs whether the constraint''s suppression is overstated (low measured suppression but high internalized component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_choice, empirical, 'Whether switching barriers are structural/economic or internalized/psychological.').

omega_variable(
    kernel_reading_vs_beneficiary_extraction_discrimination,
    'Is QWERTY''s persistence a path-dependent coordination failure (lock-in reading) or active maintenance by beneficiaries to protect training investments (beneficiary_extraction_reading)?',
    'Historical record examination: did keyboard manufacturers, software firms, or institutions actively lobby against alternative-layout standards, invest in compatibility locks, or suppress research on alternatives? Or did they simply supply what demand requested? Did ergonomic concerns ever force regulatory attention that was then defeated by lobbying, or was the issue never salient enough to reach policymakers?',
    'If active suppression is documented (manufacturers refused to produce Dvorak despite demand, OS vendors blocked layout switching, typing instructors were discouraged from teaching alternatives), the beneficiary_extraction_reading is correct and the constraint reclassifies toward snare. If persistence is simply demand-driven inertia with no active suppression, the lock-in reading holds. This discriminates between two readings of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_beneficiary_extraction_discrimination, empirical, 'Whether QWERTY''s persistence is passive coordination failure or active beneficiary maintenance.').

omega_variable(
    reading_committer_ambiguity,
    'Does this constraint instantiate genuine path-dependent coordination failure, or is the ''coordination failure'' framing itself a narrative that obscures active beneficiary interest?',
    'Examine whether the lock-in reading''s core axioms (rational individual choice, no active suppression, coordination solution would require collective action problem resolution) are consistent with the historical record. Look for evidence of suppressed alternatives, industry-level coordination to prevent switching, or asymmetric information about superior layouts.',
    'If axioms hold, lock-in reading is correct and the constraint belongs in the coordination-failure class (not snare). If axioms fail, the beneficiary_extraction_reading or naturalization_reading is more accurate. This is the deepest omega because it questions the reading''s own foundational commitments—it is the omega of the committer frame itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether the lock-in reading''s foundational axioms about rational choice and passive coordination are structurally accurate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.08).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1873, 0.02).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1920, 0.05).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1980, 0.16).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% The QWERTY kernel has three structurally distinct readings: lock-in (this story), beneficiary_extraction, and naturalization. Each reading posits different mechanisms (path-dependent coordination failure vs. active suppression vs. fair competition), different beneficiary structures (none vs. manufacturers/incumbents vs. none), and different ε values (moderate coordination cost vs. high extraction vs. negligible). The three readings are sibling constraints linked by this network edge. Discrimination between readings depends on resolving empirical questions about historical evidence, technical superiority claims, and suppression mechanisms (captured in omegas). The readings coexist as live positions in the scholarly and policy discourse on standards and lock-in; no single reading logically forecloses another without empirical arbitration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
