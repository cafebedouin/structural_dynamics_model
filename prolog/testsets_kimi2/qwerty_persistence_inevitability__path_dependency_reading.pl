% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence as Path-Dependent Coordination Mountain
 *   domain: technology history / political economy / institutional analysis
 *
 * SUMMARY:
 *   This constraint instantiates the path-dependency reading of the QWERTY
 *   persistence kernel: the keyboard layout persists not because any actor
 *   strategically maintains it, but because an early quasi-accidental
 *   standard was reinforced by increasing returns to adoption. Manufacturers
 *   produce QWERTY hardware because consumers demand compatibility; consumers
 *   demand compatibility because the installed base makes individual
 *   switching irrational. The resulting efficiency loss relative to
 *   counterfactual optimal layouts is a diffuse externality, not a targeted
 *   extraction. No party collects rents from the persistence itself, and no
 *   party is structurally victimized by it. The classification as mountain
 *   reflects the reading's claim that the persistence is a structural feature
 *   of path-dependent coordination, not a maintained human arrangement.
 *
 * KEY AGENTS:
 *   - keyboard_users: Diffuse cost-bearers â individually bear small typing-efficiency losses but are not structurally targeted; their 'trapped' condition is a coordination equilibrium, not coerced lock-in
 *   - hardware_manufacturers: Responsive producers â produce QWERTY devices because market demand rewards compatibility; they do not capture surplus from the standard itself and would switch layouts if demand shifted
 *   - alternative_layout_advocates: Marginal challengers â promote Dvorak, Colemak, and other layouts but cannot overcome the coordination threshold; they are not suppressed but simply cannot coordinate a critical mass
 *   - economic_historians: Analytical observers â document the path-dependent process without positing a maintaining agenda or beneficiary class
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence as Path-Dependent Coordination Mountain").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology history / political economy / institutional analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '94660f8d-f705-4246-81b1-0a80594707e0').
narrative_ontology:cs_kernel_codification('94660f8d-f705-4246-81b1-0a80594707e0', implicit).
narrative_ontology:cs_authority_grounding('94660f8d-f705-4246-81b1-0a80594707e0', self_enforcing).
narrative_ontology:cs_reading_relation('94660f8d-f705-4246-81b1-0a80594707e0', qwerty_persistence_inevitability__strategic_lock_in_reading, forecloses).
narrative_ontology:cs_axiom('94660f8d-f705-4246-81b1-0a80594707e0', foundational, initial_accidents_determine_standards).
narrative_ontology:cs_axiom_status(initial_accidents_determine_standards, holdable).
narrative_ontology:cs_axiom_grounding('94660f8d-f705-4246-81b1-0a80594707e0', initial_accidents_determine_standards, empirically_contingent).
narrative_ontology:cs_axiom('94660f8d-f705-4246-81b1-0a80594707e0', foundational, no_concentrated_agency_in_persistence).
narrative_ontology:cs_axiom_status(no_concentrated_agency_in_persistence, holdable).
narrative_ontology:cs_axiom_grounding('94660f8d-f705-4246-81b1-0a80594707e0', no_concentrated_agency_in_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('94660f8d-f705-4246-81b1-0a80594707e0', path_dependent_equilibrium).
narrative_ontology:cs_drift_state('94660f8d-f705-4246-81b1-0a80594707e0', post_empirical_challenge_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('94660f8d-f705-4246-81b1-0a80594707e0', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a pure coordination problem: all typists and device producers need to agree on a single character layout to avoid interoperability and retraining costs, and the cost of failing to coordinate exceeds the efficiency cost of using a suboptimal standard.
% TRANSFER_FUNCTION: No concentrated transfer. The arrangement moves potential typing efficiency from the counterfactual optimal layout into the friction of network maintenance and retraining barriers, which is borne diffusely by all keyboard users as an uncollected externality.
% ABSENT_VOICES: Alternative-layout advocates (Dvorak, Colemak) and ergonomic reformers are present in niche discourse but structurally marginal; their absence from the mainstream is due to coordination costs, not exclusion.
% DISAPPEARANCE_RATIONALE: If the path-dependent lock-in evaporated, keyboard users would gradually migrate to more efficient layouts, but no specific human arrangement would collapse because no party's income or authority depends on QWERTY persistence. The constraint is a feature of coordination dynamics, not a maintained institution.
% FOUNDING_PROBLEM: The lack of a universal typewriter standard in the 1870s created interoperability failures and training costs across competing machines.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (David 1985; Liebowitz and Margolis 1990) attest to the standardization problem from outside any benefiting party. In this reading there is no beneficiary class, so all attestation is external to any extraction structure.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_unchanged).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 because the only 'cost' is a diffuse, uncollected externality: no agent appropriates the difference between QWERTY and counterfactual optimal layouts. Suppression is 0.05 because no active enforcement maintains the standard; persistence is self-enforcing through network effects. Theater ratio is 0.03 because there is no performative maintenance â no institution stages compliance rituals to sustain QWERTY. Accessibility collapse is 0.92 because, once the standard is understood as dominant, alternatives effectively vanish for any individual user due to interoperability requirements. Resistance is 0.08 because alternative-layout movements exist but do not face coercion; they simply fail to solve the coordination problem. The measurement series use a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   Because no beneficiaries or victims are declared, directionality is structurally uniform: all agents sit near symmetric (d â 0.5) with respect to this constraint. The analytical observer and the typist experience the same coordination equilibrium. The only divergence is between the individual user (who feels trapped by network effects) and the population level (where no one is trapped by any other agent's will). The engine will compute near-zero effective extraction for all seats because the base epsilon is low and no directional amplification applies.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored, so the engine derives neutral directionality for all positions. Keyboard users, manufacturers, and alternative advocates are all embedded in the same coordination structure; none subsidizes or extracts from the others through the constraint itself. The constraint is not a transfer device but a structural feature of standardization dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents misclassification by explicitly denying the three features that would mandate a different type: there is no active enforcement (ruling out snare and tangled_rope), no sunset clause or transitional justification (ruling out scaffold), and no theatrical maintenance of an atrophied function (ruling out piton). The constraint also lacks the beneficiary/victim asymmetry required for extraction types. Were one to misread manufacturer profit from keyboard sales as extraction from the standard itself, one would commit the fallacy of confusing rents in the hardware market with rents from the layout constraint; the path-dependency reading blocks this by noting that manufacturers respond to demand and would happily produce alternative layouts if demand shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_lock_in,
    'Is QWERTY persistence fully explained by accident-driven path dependency, or does it conceal strategic reinforcement by manufacturers and standards bodies?',
    'Historical archival discovery of manufacturer cartel behavior in the 1890s-1910s, or econometric analysis showing supracompetitive returns to QWERTY maintenance.',
    'If strategic reinforcement is documented, the constraint shifts from mountain to tangled_rope or snare (identifiable beneficiaries and victims); if absent, the path dependency reading retains its structural classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_lock_in, empirical, 'Whether QWERTY lock-in is pure path dependency or strategically maintained.').

omega_variable(
    diffuse_externality_extraction_status,
    'Does the diffuse efficiency loss from QWERTY constitute extraction in the Deferential Realism framework when no party captures the surplus?',
    'Calibrated measurement of typing speed differentials and retraining costs across the global installed base.',
    'If the externality is large and systematic, base_extractiveness may need upward revision even without a capturer; if negligible, the mountain classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_externality_extraction_status, conceptual, 'Whether un-captured diffuse costs count as extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 25, 0.02).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 50, 0.02).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 75, 0.03).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.03).
narrative_ontology:measurement(qwer_tr_t125, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 125, 0.04).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.04).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 25, 0.06).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.06).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 75, 0.07).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(qwer_be_t125, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 125, 0.09).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence_inevitability kernel decomposes into two structurally distinct constraints: this path_dependency_reading (mountain, no beneficiaries, diffuse externality) and the strategic_lock_in_reading (snare or tangled_rope, with identifiable manufacturers as beneficiaries and typists as victims). They share the same referent â the continued dominance of QWERTY â but assign different epsilon values and structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
