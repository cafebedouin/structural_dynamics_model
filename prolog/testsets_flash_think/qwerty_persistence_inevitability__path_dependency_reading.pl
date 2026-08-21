% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: QWERTY Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'path_dependency_reading' of QWERTY
 *   keyboard persistence. It posits that QWERTY's continued dominance is a
 *   result of self-reinforcing historical accidents, network effects, and
 *   high switching costs, rather than active strategic design or ongoing
 *   extraction by identifiable beneficiaries. The efficiency losses from its
 *   suboptimal design are considered diffuse externalities, not concentrated
 *   extraction. This reading classifies QWERTY as a Mountain due to its
 *   perceived technological inevitability given initial conditions and the
 *   absence of active enforcement or concentrated beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.1).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'cd792339-5c7d-4fa3-a650-2e9480d3eaab').
narrative_ontology:cs_kernel_codification('cd792339-5c7d-4fa3-a650-2e9480d3eaab', implicit).
narrative_ontology:cs_authority_grounding('cd792339-5c7d-4fa3-a650-2e9480d3eaab', practice).
narrative_ontology:cs_reading_relation('cd792339-5c7d-4fa3-a650-2e9480d3eaab', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('cd792339-5c7d-4fa3-a650-2e9480d3eaab', foundational, technological_adoption_is_self_reinforcing).
narrative_ontology:cs_axiom_status(technological_adoption_is_self_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('cd792339-5c7d-4fa3-a650-2e9480d3eaab', technological_adoption_is_self_reinforcing, empirically_contingent).
narrative_ontology:cs_axiom('cd792339-5c7d-4fa3-a650-2e9480d3eaab', secondary, efficiency_losses_are_diffuse_externalities).
narrative_ontology:cs_axiom_status(efficiency_losses_are_diffuse_externalities, holdable).
narrative_ontology:cs_axiom_grounding('cd792339-5c7d-4fa3-a650-2e9480d3eaab', efficiency_losses_are_diffuse_externalities, empirically_contingent).
narrative_ontology:cs_reference_frame('cd792339-5c7d-4fa3-a650-2e9480d3eaab', historical_network_effect_entrenchment).
narrative_ontology:cs_drift_state('cd792339-5c7d-4fa3-a650-2e9480d3eaab', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cd792339-5c7d-4fa3-a650-2e9480d3eaab', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce QWERTY keyboards because that is what the market demands, perpetuating the standard. They do not actively extract rents from the layout itself, but respond to established user preferences and training infrastructure. Shifting to an alternative layout would incur massive retooling and market education costs, making exit from QWERTY production economically constrained.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Learn and use QWERTY keyboards due to its ubiquity in education and workplaces. They bear the diffuse efficiency costs of a suboptimal layout (e.g., slower typing speeds, increased strain), but the cost of retraining for an alternative (e.g., Dvorak) is prohibitive given the lack of widespread adoption for alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typists, payer,
    powerless, biographical, constrained, global).

% Study the efficiency and health impacts of keyboard layouts, identifying QWERTY's suboptimal design. They analyze the path dependency but do not directly participate in its perpetuation or challenge its existence in the market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% Promote more efficient keyboard layouts (e.g., Dvorak, Colemak) but face immense barriers to adoption due to QWERTY's entrenched network effects and the high switching costs for typists and manufacturers. Their voices are largely unheard in the mainstream market, making them structurally excluded from influencing the dominant standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, standardized interface for human-computer interaction, enabling mass production of keyboards and widespread typing instruction across diverse contexts.
% TRANSFER_FUNCTION: Diffuse, uncaptured efficiency losses (e.g., slower typing speeds, increased strain, higher training costs for optimal alternatives) are borne by typists globally, without direct, concentrated transfer to a specific beneficiary.
% ABSENT_VOICES: Advocates for alternative, more efficient keyboard layouts (e.g., Dvorak, Colemak) whose innovations are suppressed by the network effects and switching costs of the entrenched QWERTY standard. Their proposals are technically superior but cannot overcome the inertia.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would collapse, requiring a complete re-education of typists and retooling of manufacturing, leading to immense economic disruption until a new standard emerged and became entrenched.
% FOUNDING_PROBLEM: The need for a robust, standardized mechanical typewriter layout in the 1870s that prevented key jamming in early machines and allowed for rapid, consistent input, given the technological constraints of the era.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology widely corroborate the original mechanical constraints and the accidental, non-optimal evolution of the QWERTY layout. Ergonomics researchers confirm the suboptimal nature of the layout for modern typing, indicating the original problem is no longer relevant to its persistence.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.05) reflects the absence of strategic beneficiaries; efficiency losses are diffuse. Suppression (0.1) is minimal, arising from inertia and network effects rather than active coercion. The theater ratio (0.05) is low, as the constraint is genuinely functional in its coordination role, not performative. Accessibility collapse (0.9) is high because alternatives, though technically superior, face immense barriers to adoption. Resistance (0.15) is low, confined to niche communities. The claimed type is Mountain because, from this reading, QWERTY's persistence is an irreducible structural feature of the technological landscape, given its historical entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The 'path_dependency_reading' views QWERTY as a stable, almost natural, outcome of historical forces. The 'strategic_lock_in_reading' (a sibling constraint) would see the same persistence as a result of active, extractive strategies by manufacturers. This divergence is precisely what the kernel framework is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers are agenda-setters by virtue of producing the standard, but are constrained by market demand, not actively extracting from the layout itself. Typists are payers, bearing the diffuse efficiency costs. Alternative layout advocates are excluded, unable to overcome the entrenched network effects. No party is a strategic beneficiary in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_lock_in,
    'Is QWERTY''s persistence primarily an accident-driven path dependency, or is it maintained by strategic lock-in engineered by manufacturers or other actors?',
    'Detailed historical and economic analysis of manufacturer incentives, marketing strategies, and training infrastructure over time, specifically looking for evidence of active suppression of alternatives or rent-seeking from the standard.',
    'If strategic lock-in is found, the constraint would reclassify from Mountain to Snare or Tangled Rope, with identifiable beneficiaries and victims, and higher extractiveness and suppression, as per the ''strategic_lock_in_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_lock_in, conceptual, 'Ambiguity between historical accident and strategic design in QWERTY''s persistence.').

omega_variable(
    diffuse_vs_concentrated_efficiency_loss,
    'Are the efficiency losses from QWERTY''s suboptimal design truly diffuse externalities, or do they indirectly concentrate benefits for specific actors (e.g., by reducing competition for keyboard manufacturers, or simplifying training for institutions)?',
    'Economic modeling of the distribution of costs and benefits across the ecosystem, including analysis of market structure and competitive dynamics in keyboard manufacturing and typing education.',
    'If concentrated benefits are identified, the constraint''s extractiveness would be higher, and identifiable beneficiaries would be declared, potentially shifting classification towards Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_vs_concentrated_efficiency_loss, empirical, 'Whether QWERTY''s efficiency losses are truly diffuse or indirectly benefit specific actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1870, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1870, 0.05).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1870, 0.05).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1930, 0.05).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2020, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1870, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1870, 0.1).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1930, 0.1).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(qwer_su_t2020, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
