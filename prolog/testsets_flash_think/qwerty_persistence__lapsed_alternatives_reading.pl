% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence: Lapsed Alternatives Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint, the 'lapsed_alternatives_reading' of the
 *   'qwerty_persistence' kernel, describes how the QWERTY keyboard layout
 *   persists primarily due to its coordination value and the failure of
 *   alternative layouts to achieve critical mass. The high switching costs
 *   for users and manufacturers, combined with the network effects of a
 *   dominant standard, effectively 'trap' the system in QWERTY, even if
 *   ergonomically superior alternatives exist. This reading emphasizes the
 *   self-reinforcing nature of a successful coordination mechanism, where
 *   alternatives simply lapse due to lack of adoption, rather than being
 *   actively suppressed by an incumbent.
 *
 * KEY AGENTS:
 *   - keyboard_users: Beneficiary/Payer (from coordination/switching costs)
 *   - keyboard_manufacturers: Beneficiary (from stable standard)
 *   - software_developers: Beneficiary (from predictable input)
 *   - alternative_layout_advocates: Excluded (from mainstream adoption)
 *   - ergonomics_researchers: Observer (analytical seat)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.22).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.18).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence: Lapsed Alternatives Reading").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'ec3a4e30-3019-4647-8503-bfb6ddf1c7e4').
narrative_ontology:cs_kernel_codification('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', implicit).
narrative_ontology:cs_authority_grounding('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', practice).
narrative_ontology:cs_reading_relation('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', foundational, network_effects_drive_adoption).
narrative_ontology:cs_axiom_status(network_effects_drive_adoption, holdable).
narrative_ontology:cs_axiom_grounding('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', network_effects_drive_adoption, empirically_contingent).
narrative_ontology:cs_axiom('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', secondary, switching_costs_lock_in).
narrative_ontology:cs_axiom_status(switching_costs_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', switching_costs_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', ubiquitous_coordination_standard).
narrative_ontology:cs_drift_state('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ec3a4e30-3019-4647-8503-bfb6ddf1c7e4', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_developers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, keyboard_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the universal compatibility and ease of use of a single standard, but bear the diffuse cost of switching if they desire an alternative layout. Their muscle memory is deeply invested in QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, keyboard_users, payer).

% Benefit from a stable, universally accepted standard that simplifies production and reduces market fragmentation. While they could produce alternative layouts, the market demand for QWERTY makes it the dominant production choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% Benefit from a predictable input standard, simplifying software design and ensuring broad compatibility. They face minimal costs related to QWERTY's persistence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for more ergonomically efficient keyboard layouts (e.g., Dvorak, Colemak) but face immense barriers to adoption due to QWERTY's network effects. They bear the costs of non-standardization and are largely excluded from mainstream influence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    powerless, generational, identity_locked, global).

% Study the efficiency and health impacts of various keyboard layouts, often highlighting the sub-optimality of QWERTY from a purely ergonomic perspective. They provide analytical insights but have limited direct power to change the standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, predictable keyboard layout that enables efficient communication and data entry across diverse hardware and software, solving a fundamental collective action problem for digital input.
% TRANSFER_FUNCTION: The constraint diffuses the costs of learning new layouts and manufacturing diverse hardware across the system, while transferring the benefit of universal compatibility to all users and manufacturers.
% ABSENT_VOICES: Advocates for alternative, ergonomically superior keyboard layouts are present but marginalized; their voices are not integrated into the implicit standard-setting process, which is driven by inertia and network effects.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire digital input ecosystem would face immediate chaos. Hardware, software, and human muscle memory are deeply intertwined with this layout, necessitating a complete retooling and relearning process that would cause massive disruption and eventually lead to the emergence of new, likely fragmented, standards.
% FOUNDING_PROBLEM: The original problem QWERTY was designed to solve was preventing key jamming on mechanical typewriters by separating commonly used letter pairs.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomics researchers widely corroborate that the original mechanical problem QWERTY solved is entirely dead for modern digital keyboards. However, the 'need for a standard' is implicitly treated as live by users and manufacturers, without specific reference to the original jamming issue.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.22, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.22) is low, reflecting only the diffuse switching costs inherent in adopting a new standard, not active rent extraction. Suppression (0.18) is also low, as there's no active enforcement; rather, the 'suppression' comes from the overwhelming network effects that make alternatives non-viable. Theater ratio (0.08) is minimal, as QWERTY remains highly functional as a coordination device. Accessibility collapse (0.85) is high because, despite the existence of alternatives, their practical accessibility in the mainstream market is severely limited. Resistance (0.15) is low because most users accept QWERTY as the default.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a typical keyboard user or manufacturer, QWERTY is simply 'the standard' that works, and its persistence is a natural outcome of coordination. From the perspective of an ergonomics researcher or alternative layout advocate, its persistence is a suboptimal outcome driven by path dependence and network effects, representing a 'lost opportunity' for better design. This reading focuses on the latter, but without attributing active malice or concentrated extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   All parties are, in a sense, 'beneficiaries' of the coordination provided by a universal standard, but they also bear the 'cost' of being locked into that standard (symmetric costs). Keyboard users and manufacturers benefit from compatibility but face switching costs. Alternative layout advocates are effectively 'excluded' by the market dynamics, bearing the highest costs of non-standardization. The 'no beneficiary set' and 'victim set empty' in base_properties reflects the diffuse, non-concentrated nature of these benefits and costs in this specific reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_incumbent_defense,
    'To what extent is QWERTY''s persistence due to its inherent coordination value and network effects (this reading), versus active defense by incumbent manufacturers protecting their capital investments (the ''incumbent_preservation_reading'')?',
    'Economic analysis of manufacturer lobbying efforts, patent defense strategies, and the cost structure of producing alternative layouts versus the market share of QWERTY. If active defense is negligible, this reading is strengthened.',
    'If incumbent defense is significant, the constraint shifts towards a Tangled Rope or Snare, with identifiable beneficiaries actively extracting rents. If coordination value is dominant, it remains a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_incumbent_defense, empirical, 'Distinguishing between passive network effects and active incumbent defense.').

omega_variable(
    true_cost_of_switching,
    'What is the actual, quantifiable cost (time, productivity, retraining) for an average user or organization to switch from QWERTY to an alternative layout, and how does this compare to the ergonomic benefits?',
    'Longitudinal studies of organizations that have successfully transitioned to alternative layouts, measuring productivity changes, training costs, and user satisfaction over time.',
    'If switching costs are lower than perceived, or ergonomic benefits are higher, the ''lapsed alternatives'' argument weakens, suggesting a greater degree of ''choice'' than implied. If costs are prohibitive, this reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_switching, empirical, 'Quantifying the real-world barriers to adopting alternative keyboard layouts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1920, 0.06).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1873, 0.05).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'qwerty_persistence' kernel. This 'lapsed_alternatives_reading' focuses on coordination value and network effects, while the 'incumbent_preservation_reading' focuses on active defense by beneficiaries. Both are distinct but related analyses of the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
