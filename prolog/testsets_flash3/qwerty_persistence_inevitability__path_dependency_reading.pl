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
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a case of accident-driven path dependency. The initial adoption of
 *   QWERTY was for mechanical reasons (preventing key jams) and early
 *   telegrapher efficiency, not optimal typing speed. Once established,
 *   network effects and high switching costs for typists and manufacturers
 *   created an insurmountable barrier to alternative, potentially more
 *   efficient, layouts. This reading asserts that no strategic actor actively
 *   benefits from or enforces QWERTY's dominance; it persists due to inertia
 *   and the self-reinforcing nature of a widely adopted standard. The
 *   constraint is claimed as a Mountain due to its perceived inevitability
 *   given initial conditions and the lack of a clear extractive agent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.95).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '2fa18187-f0d2-4356-9967-619bc7a23f3f').
narrative_ontology:cs_kernel_codification('2fa18187-f0d2-4356-9967-619bc7a23f3f', implicit).
narrative_ontology:cs_authority_grounding('2fa18187-f0d2-4356-9967-619bc7a23f3f', practice).
narrative_ontology:cs_reading_relation('2fa18187-f0d2-4356-9967-619bc7a23f3f', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('2fa18187-f0d2-4356-9967-619bc7a23f3f', foundational, technological_standards_emerge_accidentally).
narrative_ontology:cs_axiom_status(technological_standards_emerge_accidentally, holdable).
narrative_ontology:cs_axiom_grounding('2fa18187-f0d2-4356-9967-619bc7a23f3f', technological_standards_emerge_accidentally, empirically_contingent).
narrative_ontology:cs_axiom('2fa18187-f0d2-4356-9967-619bc7a23f3f', foundational, network_effects_are_self_sustaining).
narrative_ontology:cs_axiom_status(network_effects_are_self_sustaining, holdable).
narrative_ontology:cs_axiom_grounding('2fa18187-f0d2-4356-9967-619bc7a23f3f', network_effects_are_self_sustaining, empirically_contingent).
narrative_ontology:cs_reference_frame('2fa18187-f0d2-4356-9967-619bc7a23f3f', initial_accidental_adoption).
narrative_ontology:cs_drift_state('2fa18187-f0d2-4356-9967-619bc7a23f3f', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2fa18187-f0d2-4356-9967-619bc7a23f3f', '').
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

% Produce QWERTY keyboards because that is what the market demands. They have no incentive to switch to alternative layouts due to the high cost of retooling and the lack of demand for non-QWERTY products. They respond to the existing installed base.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    organized, biographical, constrained, global).

% Learn QWERTY as the default and are locked into it by muscle memory and the ubiquity of QWERTY keyboards. Switching to a more efficient layout would require significant retraining and would be impractical given the lack of widespread availability of alternative keyboards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Promote more efficient keyboard layouts (e.g., Dvorak, Colemak) but face an insurmountable barrier to adoption due to the entrenched QWERTY standard. Their efforts are largely academic or niche, with little impact on the mainstream market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze the historical development and persistence of QWERTY, debating whether its dominance is due to inherent efficiency, accidental path dependency, or strategic lock-in by manufacturers. This reading emphasizes the accidental and self-reinforcing nature of the standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal standard for keyboard layouts, allowing typists to use any keyboard and manufacturers to produce a single, widely compatible product.
% TRANSFER_FUNCTION: No direct transfer of value. The 'cost' is the foregone efficiency of potentially better layouts, borne diffusely by all typists and the economy as a whole, without accruing to any specific beneficiary.
% ABSENT_VOICES: Advocates for alternative, more efficient keyboard layouts are effectively excluded from the mainstream market and public discourse, their arguments unable to overcome the inertia of the installed base.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would collapse, requiring a massive, coordinated effort to adopt a new standard. The world would rearrange itself around a new, likely more efficient, but initially chaotic, keyboard landscape.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating frequently used letter pairs, and to facilitate telegraphers' transcription by placing common letters on the home row.
% FOUNDING_PROBLEM_CORROBORATION: The original mechanical jamming problem is long obsolete with modern keyboards. The telegrapher's transcription problem is also irrelevant. Economic historians and ergonomists widely corroborate that the original technical justifications are no longer valid, and the persistence is due to network effects and switching costs, not current efficiency. No benefiting party can credibly claim the original problem is live.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is very low (0.05) because no identifiable party directly extracts rents from QWERTY's persistence; the 'cost' is a diffuse externality of foregone efficiency. Suppression is very high (0.95) because the network effects and installed base create an almost insurmountable barrier to alternatives, effectively 'suppressing' their adoption. Accessibility collapse is high (0.9) as alternatives are practically non-existent in the mainstream. Resistance is low (0.05) because most users accept QWERTY as a given, and organized resistance from alternative layout advocates has minimal impact. Theater ratio is 0.0 as there's no performative maintenance; the system simply runs on its own inertia. The claimed type is Mountain because, from this reading, QWERTY's persistence is a technological inevitability given its historical path, not a choice maintained by an agent.
 *
 * PERSPECTIVAL GAP:
 *   The 'path_dependency_reading' views QWERTY's persistence as a natural outcome of historical accidents and network effects, making it a Mountain. The 'strategic_lock_in_reading' (a sibling constraint) would view it as a Snare, where manufacturers actively maintain QWERTY's dominance through strategic actions (e.g., training programs, cartel standardization) to lock in users and extract rents. The key difference is the presence of an active, extractive beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers are agenda-setters in the sense that they produce what the market demands, but they are also constrained by the existing standard. Typists are payers, bearing the diffuse cost of suboptimal efficiency and the high switching costs. No specific beneficiary captures the 'extraction' in this reading; the efficiency loss is a system-level externality. Alternative layout advocates are excluded, unable to penetrate the entrenched standard.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accidental_vs_strategic_origin,
    'Is QWERTY''s persistence primarily an accident-driven path dependency, or is it maintained by strategic actions of manufacturers to lock in users and extract rents?',
    'Historical analysis of manufacturer lobbying, standardization efforts, and training program subsidies; economic analysis of profit margins on QWERTY vs. potential alternative layouts if market were open.',
    'If strategic lock-in is confirmed, the constraint would reclassify from Mountain to Snare or Tangled Rope, with identifiable beneficiaries (manufacturers) and victims (typists, alternative layout developers) and a higher extractiveness score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accidental_vs_strategic_origin, empirical, 'Distinguishing between accidental historical inertia and active, strategic maintenance of a suboptimal standard.').

omega_variable(
    diffuse_cost_vs_captured_rent,
    'Is the efficiency loss from QWERTY truly a diffuse externality, or is there an identifiable, albeit indirect, capture of this ''lost'' value by specific actors?',
    'Detailed economic modeling of the value chain, including software development, training, and hardware manufacturing, to trace where the ''cost'' of QWERTY''s inefficiency might accrue as ''rent'' to specific industry segments.',
    'If captured rent is identified, the extractiveness score would increase, and the constraint would shift away from Mountain, as a Mountain should not have identifiable beneficiaries of its ''costs''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_vs_captured_rent, empirical, 'Determining if the efficiency cost is truly diffuse or if it indirectly benefits specific actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1874, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1874, 0.0).
narrative_ontology:measurement(qwer_tr_t1924, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1924, 0.0).
narrative_ontology:measurement(qwer_tr_t1974, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1974, 0.0).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1874, 0.01).
narrative_ontology:measurement(qwer_be_t1924, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1924, 0.02).
narrative_ontology:measurement(qwer_be_t1974, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1974, 0.03).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1874, 0.8).
narrative_ontology:measurement(qwer_su_t1924, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1924, 0.85).
narrative_ontology:measurement(qwer_su_t1974, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1974, 0.9).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the 'path_dependency_reading' of the 'qwerty_persistence_inevitability' kernel. It is linked to the 'strategic_lock_in_reading' (qwerty_persistence_inevitability__strategic_lock_in_reading) as a sibling, representing a competing interpretation of the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
