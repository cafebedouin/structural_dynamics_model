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
 *   human_readable: QWERTY Persistence as Accident-Driven Path Dependency
 *   domain: technology history / political economy / institutional analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the path_dependency_reading of the
 *   qwerty_persistence_inevitability kernel. It treats QWERTY dominance not
 *   as a strategically maintained lock-in but as a mountain-like emergent
 *   property of cumulative adoption dynamics: once a layout achieves critical
 *   mass, network externalities make switching individually irrational
 *   regardless of collective efficiency losses. The constraint has no
 *   strategic beneficiaries extracting rents and no identifiable victims
 *   being coerced; the efficiency loss is a diffuse externality distributed
 *   across all keyboard users. This reading is contested by the
 *   strategic_lock_in_reading, which posits manufacturer-engineered
 *   extraction.
 *
 * KEY AGENTS:
 *   - keyboard_users: Diffuse population bearing suboptimal efficiency costs with no individual exit
 *   - typewriter_manufacturers: Historical adopters responding to demand, not agenda-setters maintaining the constraint
 *   - alternative_layout_advocates: Excluded voices arguing for ergonomic efficiency but lacking leverage against the installed base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.06).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence as Accident-Driven Path Dependency").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology history / political economy / institutional analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '678677f8-7182-4143-b373-2bbf2960c26e').
narrative_ontology:cs_kernel_codification('678677f8-7182-4143-b373-2bbf2960c26e', implicit).
narrative_ontology:cs_authority_grounding('678677f8-7182-4143-b373-2bbf2960c26e', distributed).
narrative_ontology:cs_reading_relation('678677f8-7182-4143-b373-2bbf2960c26e', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('678677f8-7182-4143-b373-2bbf2960c26e', foundational, historical_path_dependency_is_sufficient_explanation).
narrative_ontology:cs_axiom_status(historical_path_dependency_is_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('678677f8-7182-4143-b373-2bbf2960c26e', historical_path_dependency_is_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_axiom('678677f8-7182-4143-b373-2bbf2960c26e', foundational, absence_of_strategic_manufacturer_intent).
narrative_ontology:cs_axiom_status(absence_of_strategic_manufacturer_intent, holdable).
narrative_ontology:cs_axiom_grounding('678677f8-7182-4143-b373-2bbf2960c26e', absence_of_strategic_manufacturer_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('678677f8-7182-4143-b373-2bbf2960c26e', cumulative_adoption_equilibrium).
narrative_ontology:cs_drift_state('678677f8-7182-4143-b373-2bbf2960c26e', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('678677f8-7182-4143-b373-2bbf2960c26e', '').
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
% COORDINATION_FUNCTION: Provides interoperable keyboard layout standards across typewriter and computer industries without requiring centralized design or enforcement.
% TRANSFER_FUNCTION: No intentional transfer; diffuse typing efficiency costs are borne by all keyboard users collectively as an uncompensated externality of cumulative adoption dynamics.
% ABSENT_VOICES: Advocates of ergonomically optimized alternative layouts (Dvorak, Colemak) and efficiency-focused reformers are structurally excluded from influence by the sheer scale of the installed base, not by active suppression.
% DISAPPEARANCE_RATIONALE: Path dependency is an emergent feature of cumulative adoption; if the inertial force vanished, users would simply migrate toward efficiency-optimal layouts without institutional rearrangement.
% FOUNDING_PROBLEM: Preventing typebar mechanical jamming on early Sholes typewriters and achieving de facto hardware interoperability in the absence of a central standards authority.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (David, 1985; Liebowitz and Margolis) and technology studies scholars attest from outside any concentrated beneficiary position that the mechanical problem is obsolete and the persistence is now purely inertial.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_unchanged).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.06, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-zero (0.06) because the constraint extracts nothing for any party; the diffuse efficiency loss is an uncompensated externality, not a transfer. Suppression is negligible (0.02) because persistence requires no active enforcementâalternatives are simply uneconomical at small scale. Theater ratio is zero because no one performs maintenance of a natural path dependency. Accessibility collapse is very high (0.94): once the standard is entrenched, alternatives collapse due to network effects. Resistance is minimal (0.02) because the constraint is experienced as background reality, not an imposed arrangement.
 *
 * PERSPECTIVAL GAP:
 *   In a genuine mountain, all seats experience the constraint similarly: keyboard users, manufacturers, and observers all face the same structural fact of installed-base dominance. There is no directional asymmetry because no one is subsidized and no one is targeted.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations; directionality is uniform across all positions. The engine computes near-symmetric d for all agents because the constraint imposes a diffuse cost without concentrated extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy because the constraint was never a scaffold, rope, or snare that decayed into a piton. It is a mountain: an emergent structural feature that persists through the natural logic of cumulative adoption rather than through institutional inertia or theatrical maintenance. The classification prevents mislabeling by requiring the absence of both active enforcement and concentrated beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_intent,
    'Is QWERTY persistence driven purely by accidental path dependency, or by manufacturer-engineered strategic lock-in?',
    'Historical archival discovery of manufacturer deliberation records (training partnership contracts, cartel standardization agreements) or systematic absence of such evidence.',
    'If strategic intent is documented, the constraint is not a natural law of path dependency but a constructed extraction mechanism, reclassifying toward tangled_rope or snare; if no such evidence exists, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_intent, empirical, 'Ambiguity between accidental path dependency and strategic lock-in as the cause of QWERTY persistence.').

omega_variable(
    efficiency_loss_magnitude,
    'Is the efficiency loss from QWERTY substantial enough to constitute meaningful extraction, or is it trivial given modern typing contexts?',
    'Controlled ergonomic studies comparing QWERTY to optimized alternatives in mechanical and electronic contexts, adjusted for learning-curve effects.',
    'If substantial, the diffuse externality challenges the negligible-extraction mountain claim; if trivial, the mountain framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_loss_magnitude, empirical, 'Whether QWERTY''s efficiency penalty is material or negligible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 75, 0.0).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.0).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 75, 0.06).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This constraint and strategic_lock_in_reading are decomposed readings of the qwerty_persistence_inevitability kernel per the Îµ-invariance principle. The path_dependency_reading posits negligible extraction and no beneficiary structure; the strategic_lock_in_reading posits active manufacturer extraction. They cannot share a single Îµ or classification and are authored as separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
