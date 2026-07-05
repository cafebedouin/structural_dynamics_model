% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: QWERTY Keyboard Layout Persistence — Path Dependency Reading
 *   domain: technology_history/institutional_economics
 *
 * SUMMARY:
 *   QWERTY was designed in the 1870s to slow typists and prevent typebar jams
 *   in mechanical typewriters. That mechanical problem vanished with electric
 *   and then electronic keyboards, yet QWERTY remains dominant globally. This
 *   reading holds that the persistence is explained entirely by
 *   coordination-cost path dependency: once a critical mass of typists,
 *   typing schools, and keyboard manufacturers converged on one standard,
 *   switching costs for any single actor exceeded switching benefits,
 *   producing a stable equilibrium that nobody needs to enforce and from
 *   which nobody extracts rent. The efficiency loss relative to a
 *   hypothetically superior layout (contested even empirically — the
 *   celebrated Dvorak efficiency studies have been challenged on methodology)
 *   is diffuse, small per capita, and borne by the entire population rather
 *   than captured by an identifiable party.
 *
 * KEY AGENTS:
 *   - existing_typists_with_trained_muscle_memory: incidental beneficiary of sunk-cost protection, not a strategic actor
 *   - keyboard_manufacturers: demand-followers who would switch tooling costlessly if the market moved
 *   - new_typists_learning_from_scratch: bear diffuse externality of any suboptimality, at low per-person cost
 *   - ergonomic_layout_advocates: excluded by coordination-cost structure, not by suppression
 *   - economic_historians: analytical observers documenting the equilibrium mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence — Path Dependency Reading").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/institutional_economics").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'dcc12510-0e96-423d-ad67-98bbdf96c9d2').
narrative_ontology:cs_kernel_codification('dcc12510-0e96-423d-ad67-98bbdf96c9d2', distributed).
narrative_ontology:cs_authority_grounding('dcc12510-0e96-423d-ad67-98bbdf96c9d2', distributed).
narrative_ontology:cs_reading_relation('dcc12510-0e96-423d-ad67-98bbdf96c9d2', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('dcc12510-0e96-423d-ad67-98bbdf96c9d2', foundational, persistence_without_designer_is_not_extraction).
narrative_ontology:cs_axiom_status(persistence_without_designer_is_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('dcc12510-0e96-423d-ad67-98bbdf96c9d2', persistence_without_designer_is_not_extraction, empirically_contingent).
narrative_ontology:cs_axiom('dcc12510-0e96-423d-ad67-98bbdf96c9d2', secondary, coordination_cost_alone_explains_lock_in).
narrative_ontology:cs_axiom_status(coordination_cost_alone_explains_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('dcc12510-0e96-423d-ad67-98bbdf96c9d2', coordination_cost_alone_explains_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('dcc12510-0e96-423d-ad67-98bbdf96c9d2', mechanical_typebar_interference_solution).
narrative_ontology:cs_drift_state('dcc12510-0e96-423d-ad67-98bbdf96c9d2', contemporary_digital_keyboards, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('dcc12510-0e96-423d-ad67-98bbdf96c9d2', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, existing_typists_with_trained_muscle_memory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, new_typists_learning_from_scratch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have already invested years internalizing QWERTY finger patterns through repetition. They benefit incidentally from the layout's persistence because switching to any alternative (Dvorak, Colemak) would require re-learning at real personal cost, so the status quo protects sunk investment they made for reasons unrelated to any layout's intrinsic merit. No one designed this benefit for them; it falls out of coordination timing.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, existing_typists_with_trained_muscle_memory, beneficiary,
    moderate, biographical, constrained, global).

% Manufacture keyboards to whatever layout the market demands. In this reading, they are demand-followers, not demand-shapers: the installed base of QWERTY-trained typists constitutes the market, and manufacturers would switch tooling immediately if buyers wanted an alternative layout, because tooling cost is trivial relative to programmatic legacy of the standard. They set the day-to-day manufacturing agenda but do not control which standard prevails.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, observer).

% Bear whatever marginal inefficiency QWERTY carries relative to a theoretically superior layout, since they must learn whatever layout dominates the installed base of keyboards, software defaults, and touch-typing instruction. Their cost is diffuse and small per-person (perhaps modestly slower typing speed over a lifetime) rather than an extracted rent captured by any identifiable party.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, new_typists_learning_from_scratch, payer,
    powerless, biographical, constrained, global).

% Argue for Dvorak or similar layouts on efficiency and repetitive-strain grounds. They are structurally unable to coordinate a mass switch because doing so requires simultaneous retraining of typists, retooling of manufacturing, and rewriting of software/OS defaults — a coordination problem with no central point of leverage to pull, not a suppression mechanism anyone maintains against them.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, ergonomic_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Study QWERTY as the canonical path-dependency case study. They document the coordination-cost mechanics that make switching costly for everyone simultaneously, and in this reading conclude the pattern is a network-effect equilibrium rather than a rent extracted by an identifiable party.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally-known keyboard layout lets any typist sit at any keyboard, any software vendor build to one input standard, and any typing-instruction curriculum teach one set of finger patterns — solving a genuine multi-party coordination problem where the specific layout matters far less than that everyone converge on the same one.
% TRANSFER_FUNCTION: No systematic transfer occurs between identifiable parties. Whatever marginal efficiency is foregone (relative to a hypothetically superior layout) is a diffuse externality distributed across the entire population of typists, not a rent collected by any beneficiary. Existing typists retain sunk training value, but this is a retained cost-avoidance, not an extraction from anyone else.
% ABSENT_VOICES: Ergonomic layout advocates would object that switching costs are systematically underestimated and lock-in is more deliberate than accidental, but they lack a coordination point from which to force reconsideration — their absence from the outcome is a collective-action failure, not an engineered exclusion.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight with no replacement standard, every typist, every keyboard manufacturer, and every software input-handling layer would need to re-coordinate around a new default simultaneously — a real coordination cost would be incurred, demonstrating that the arrangement, however accidental its origin, now performs a genuine present-tense coordination function that the world depends on.
% FOUNDING_PROBLEM: Early typewriter mechanisms (1870s) jammed when adjacent typebars were struck in quick succession; QWERTY's letter arrangement was selected substantially to slow typists down and separate commonly-paired letters, solving a mechanical interference problem specific to that hardware generation.
% FOUNDING_PROBLEM_CORROBORATION: Typewriter mechanism historians and mechanical engineers (outside any beneficiary group) corroborate that the typebar-jamming problem QWERTY was built to solve was fully eliminated by electric typewriters and later by electronic keyboards with no mechanical typebars at all — the original founding problem is uncontested as extinct. What is contested (and belongs to the sibling reading) is why the layout nonetheless persisted after its founding problem died; this reading attributes persistence to coordination-cost path dependency rather than to any party's strategic action.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-zero (0.08) and essentially flat across 150+ years because no party's take grows: manufacturers earn ordinary margins on whatever layout sells, and no rent-collecting intermediary exists. Suppression is authored low (0.05) because nothing actively blocks an alternative layout from being adopted — anyone can buy a Dvorak keyboard and remap their OS today; the barrier is coordination cost among independent actors, not enforcement. Accessibility collapse is authored high (0.88) because once the population-scale coordination equilibrium set in, switching became functionally near-impossible for any single actor to unilaterally achieve, which is the hallmark of a mountain: not because alternatives are forbidden, but because the physics of large-population coordination make unilateral defection self-defeating. Resistance is low (0.12) — there is no active resistance to QWERTY as such, only occasional advocacy for alternatives that never accumulates critical mass.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing typists sit near the beneficiary end only incidentally — the constraint's persistence protects their sunk investment, but they did not design or lobby for that protection; d is derived low but not for extractive reasons. New typists sit slightly toward the target end because they bear a real (if small) marginal cost from learning a possibly-suboptimal layout, but this is diffuse externality rather than a targeted extraction, so d remains close to symmetric rather than approaching the full-target pole. Manufacturers are treated as pass-through demand-responders with no directional stake either way.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy framing entirely: there is no mandate to have outlived, because there was never a strategic actor whose founding purpose could decay into rent-seeking. The founding problem (typebar jamming) is genuinely dead, but its death does not imply capture by a beneficiary — it implies a coordination equilibrium that nothing but the coordination cost itself now sustains. This is precisely the structural claim the sibling reading (strategic_lock_in_reading) contests: that reading argues an identifiable coalition (early typewriter manufacturer cartels, typing-school licensing arrangements) actively worked to standardize and defend QWERTY beyond its mechanical justification, which would reclassify the constraint as tangled_rope or snare with real beneficiaries and victims. This story does not adjudicate that dispute; it presents the alternative structural hypothesis as a clean, internally consistent reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qwerty_kernel_reading_selection,
    'Is QWERTY''s persistence better explained by pure coordination-cost path dependency (this reading) or by deliberate manufacturer/institutional lock-in engineering (the strategic_lock_in_reading)?',
    'Archival research into early 20th-century typewriter manufacturer agreements, typing-school curriculum licensing contracts, and any documented coordination among Remington-era manufacturers to standardize training materials; absence of such coordination evidence would support this reading, presence would support the sibling.',
    'If the strategic_lock_in_reading is corroborated, the constraint reclassifies from mountain to tangled_rope or snare with named beneficiaries (manufacturer cartels, licensed typing-school operators) and named victims (typists paying an engineered inefficiency tax) — a structurally different constraint, not a re-measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_kernel_reading_selection, conceptual, 'Which kernel reading (path-dependency vs. strategic lock-in) correctly characterizes the causal mechanism of QWERTY persistence.').

omega_variable(
    efficiency_loss_magnitude_ambiguity,
    'Is there a genuine, measurable efficiency loss from QWERTY relative to alternative layouts, or have the classic Dvorak-superiority studies been methodologically discredited to the point that no diffuse externality exists at all?',
    'A rigorously controlled, large-sample comparative typing-speed study accounting for training-time confounds (the original Dvorak studies were conducted or funded by Dvorak himself and have been contested since Liebowitz & Margolis''s 1990 re-analysis).',
    'If no genuine efficiency loss exists, even the diffuse-externality claim in this reading weakens further, pushing extractiveness toward zero and strengthening the mountain classification; if a substantial loss is empirically confirmed, the externality becomes more concrete, though it would still lack a capturing beneficiary under this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_loss_magnitude_ambiguity, empirical, 'Whether QWERTY carries any real efficiency cost at all, independent of who if anyone benefits from it.').

omega_variable(
    natural_vs_constructed_equilibrium,
    'Does a network-effect equilibrium that emerged without central design still count as emerging naturally in the mountain sense, or is treating coordination-cost lock-in as a natural law itself a category error that launders historically contingent human choices into false inevitability?',
    'Comparative analysis of other historically contingent standards that were successfully displaced (e.g., metric conversion in some countries, VHS-to-digital format transitions) to establish whether QWERTY-style lock-ins are genuinely harder to escape or merely under-attempted.',
    'If comparable path-dependent standards have been displaced through coordinated policy action, the ''mountain'' framing for QWERTY becomes harder to sustain and the constraint would sit closer to a degraded/inertial piton (persisting by inertia rather than physical necessity) than to a genuine mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_equilibrium, conceptual, 'Whether coordination-cost lock-in with no designer is properly classified as a mountain or is a naturalization of contingent historical choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1870, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1870, 0.02).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1950, 0.03).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(qwer_tr_t2026, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1870, 0.03).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1900, 0.04).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1980, 0.06).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement(qwer_be_t2026, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2026, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This story is the path_dependency_reading half of the qwerty_persistence_inevitability kernel decomposition. The sibling file, qwerty_persistence_inevitability__strategic_lock_in_reading, reads the identical observable persistence as manufacturer-engineered lock-in with named beneficiaries and victims, and classifies as tangled_rope or snare rather than mountain. The two files share no beneficiary/victim overlap by design: this file's sole beneficiary entry (existing_typists_with_trained_muscle_memory) is an incidental, non-strategic beneficiary, structurally distinct from any coordinated manufacturer/institution beneficiary the sibling would name. Link maintained per the ε-invariance decomposition principle — do not merge these into one constraint with an averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
