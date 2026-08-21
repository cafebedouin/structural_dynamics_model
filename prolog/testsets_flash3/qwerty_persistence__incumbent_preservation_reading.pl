% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout (Incumbent Preservation Reading)
 *   domain: technology_history/industrial_standards
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   viewed through the lens of incumbent preservation. It argues that
 *   QWERTY's dominance is not merely due to network effects or the natural
 *   obsolescence of alternatives, but is actively maintained by beneficiaries
 *   (manufacturers, trained typists, training institutions) who have sunk
 *   costs and vested interests in its perpetuation. This active defense
 *   constitutes a form of extraction, as it suppresses more efficient or
 *   ergonomic alternatives, imposing costs on those who would benefit from
 *   them.
 *
 * KEY AGENTS:
 *   - qwerty_keyboard_manufacturers: Primary beneficiary/agenda_setter (institutional/constrained)
 *   - trained_typists: Beneficiary (moderate/identity_locked)
 *   - typing_training_institutions: Beneficiary (organized/constrained)
 *   - alternative_keyboard_manufacturers: Primary victim (powerless/trapped)
 *   - efficiency_seeking_users: Victim (moderate/constrained)
 *   - ergonomics_researchers: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout (Incumbent Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '9e51f241-8139-43b9-84a4-f57d47a4854d').
narrative_ontology:cs_kernel_codification('9e51f241-8139-43b9-84a4-f57d47a4854d', implicit).
narrative_ontology:cs_authority_grounding('9e51f241-8139-43b9-84a4-f57d47a4854d', extraction).
narrative_ontology:cs_interpretation_layer_present('9e51f241-8139-43b9-84a4-f57d47a4854d').
narrative_ontology:cs_reading_relation('9e51f241-8139-43b9-84a4-f57d47a4854d', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('9e51f241-8139-43b9-84a4-f57d47a4854d', foundational, incumbent_investment_protection_is_paramount).
narrative_ontology:cs_axiom_status(incumbent_investment_protection_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9e51f241-8139-43b9-84a4-f57d47a4854d', incumbent_investment_protection_is_paramount, conventional).
narrative_ontology:cs_axiom('9e51f241-8139-43b9-84a4-f57d47a4854d', secondary, market_dominance_justifies_status_quo).
narrative_ontology:cs_axiom_status(market_dominance_justifies_status_quo, holdable).
narrative_ontology:cs_axiom_grounding('9e51f241-8139-43b9-84a4-f57d47a4854d', market_dominance_justifies_status_quo, conventional).
narrative_ontology:cs_reference_frame('9e51f241-8139-43b9-84a4-f57d47a4854d', qwerty_as_uncontested_default).
narrative_ontology:cs_drift_state('9e51f241-8139-43b9-84a4-f57d47a4854d', contemporary_ergonomics_awareness, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9e51f241-8139-43b9-84a4-f57d47a4854d', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomics_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested heavily in QWERTY production lines and supply chains. They actively resist adoption of alternative layouts through marketing, lobbying, and by ensuring QWERTY remains the default on most devices. They benefit from the stability of the standard and the sunk costs of their infrastructure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Have invested time and effort in mastering QWERTY. They benefit from its ubiquity, as they can use any keyboard without retraining. Their 'skill' is tied to the QWERTY layout, making them resistant to change.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    moderate, biographical, identity_locked, global).

% Their curriculum and teaching materials are based on QWERTY. They benefit from the continued demand for QWERTY training and would incur significant costs to switch to alternative layouts, thus actively promoting QWERTY as the standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Produce more ergonomically or efficiently designed keyboard layouts (e.g., Dvorak, Colemak). They face immense market resistance due to QWERTY's entrenched position and the high switching costs for users and manufacturers. They bear the cost of market exclusion and low adoption.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers, payer,
    powerless, biographical, trapped, global).

% Are aware of the ergonomic and speed benefits of alternative layouts but are constrained by the ubiquity of QWERTY hardware and the social cost of retraining. They pay in lost efficiency and potential ergonomic strain.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    moderate, immediate, constrained, local).

% Provide scientific evidence for the superiority of alternative layouts but face an uphill battle against entrenched interests. Their findings are often ignored or downplayed by the beneficiaries of the QWERTY standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, albeit suboptimal, standard for keyboard input, allowing users to operate any keyboard without needing to learn a new layout, and manufacturers to produce a single dominant product.
% TRANSFER_FUNCTION: Transfers market dominance and sustained revenue to QWERTY manufacturers and training institutions, and preserves the skill capital of trained typists, at the cost of ergonomic efficiency and market access for alternative layouts.
% ABSENT_VOICES: The voices of efficiency-seeking users and alternative keyboard innovators are largely absent from the dominant discourse, drowned out by the inertia of the installed base and the active defense by incumbents. Ergonomics researchers provide evidence but lack institutional power to shift the standard.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, there would be a chaotic but rapid transition to more efficient or ergonomic layouts. Manufacturers would retool, typists would retrain, and the market for alternative layouts would explode, leading to a more diverse and potentially more efficient input landscape.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating commonly used letter pairs, a mechanical constraint of early typewriters.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts and engineering analyses confirm that the original mechanical problem is long obsolete with modern keyboard technology. However, QWERTY manufacturers and typing institutions often implicitly or explicitly defend its continued use by citing 'familiarity' or 'established practice' rather than the original technical rationale. No independent corroboration exists for the founding problem being 'live'.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the continued dominance of QWERTY imposes significant costs in terms of lost efficiency and ergonomic strain, which are captured as sustained market share and skill relevance by beneficiaries. Suppression (0.75) is also high, reflecting the active resistance to alternatives through marketing, default settings, and the high switching costs for users and manufacturers. The theater ratio (0.20) is relatively low, as the 'coordination' function of QWERTY is still genuinely present, but a growing portion of its maintenance is defensive rather than purely functional. The claimed type is Tangled Rope because it provides a coordination function (universal layout) but also involves asymmetric extraction and active enforcement (suppression of alternatives).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY manufacturers and trained typists, the layout is a beneficial standard that ensures compatibility and preserves their skills/investments. From the perspective of alternative keyboard manufacturers and efficiency-seeking users, it is an outdated, actively enforced barrier that extracts costs in the form of lost innovation and suboptimal performance.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY manufacturers, trained typists, and training institutions are beneficiaries, as the constraint directly preserves their investments and skills. Alternative keyboard manufacturers and efficiency-seeking users are victims, bearing the costs of market exclusion and suboptimal design. The active enforcement by beneficiaries to maintain QWERTY's dominance drives the high suppression and extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (preventing typewriter jams) is long dead. Its persistence is now driven by the active defense of incumbent beneficiaries, not by a live coordination problem. This classification as a Tangled Rope prevents mislabeling it as a simple Rope (pure coordination) or a Piton (inertial decay), highlighting the active, extractive nature of its maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_inertia,
    'What proportion of QWERTY''s persistence is due to active defense by incumbents versus passive network effects and user inertia?',
    'Empirical studies quantifying lobbying efforts, marketing spend, and default setting policies by QWERTY beneficiaries, compared to surveys on user switching costs and awareness of alternatives.',
    'If active defense is dominant, the extractiveness and suppression metrics are accurate. If passive inertia is the primary driver, the constraint might be closer to a Piton, with lower effective extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_passive_inertia, empirical, 'Distinguishes between active maintenance and passive persistence of the standard.').

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point did QWERTY''s coordination function (universal compatibility) become secondary to its extractive function (incumbent protection)?',
    'Historical analysis of technological shifts (e.g., electric typewriters, computers) that rendered the original mechanical rationale obsolete, coupled with economic analysis of market concentration and pricing power in the keyboard industry over time.',
    'Identifying a clear inflection point would refine the temporal measurements and potentially shift the classification of earlier periods towards a Rope, and later periods more firmly towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Pinpoints the transition from genuine coordination to primarily extractive function.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative layouts structural (market barriers, default settings) or internalized (user resistance to retraining, perceived difficulty)?',
    'Pilot programs offering free, pre-installed alternative layouts and retraining to new users: if adoption remains low, internalized suppression is higher; if adoption increases, structural barriers were dominant.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as users carry the suppression with them. If structural, policy interventions (e.g., mandating layout choice) would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative keyboard layouts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.25).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.2).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This story and 'lapsed_alternatives_reading' are two distinct readings of the 'qwerty_persistence' kernel. This reading emphasizes active incumbent defense and extraction, while the sibling focuses on passive network effects and the failure of alternatives to reach critical mass. Both are necessary for a complete understanding of QWERTY's persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
