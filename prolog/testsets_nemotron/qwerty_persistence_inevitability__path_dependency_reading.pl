% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: QWERTY Keyboard Layout Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   The QWERTY keyboard layout originated as a mechanical solution to typebar
 *   jamming on the 1874 Sholes & Glidden typewriter. This reading holds that
 *   its persistence to the present is the result of accident-driven path
 *   dependency: an early historical accident (the specific arrangement chosen
 *   to reduce jamming) created a founding condition; network effects in
 *   typing skill transfer, manufacturing tooling, and training infrastructure
 *   then amplified the initial advantage until QWERTY became a de facto
 *   universal standard. No strategic actor engineered or maintains the
 *   lock-in. The efficiency loss relative to alternative layouts (Dvorak,
 *   Colemak) is a diffuse externality borne by all users equally;
 *   manufacturers simply respond to market demand for the standard. The
 *   constraint is a mountain — a technological inevitability given the
 *   initial conditions, not a human choice that could be reversed.
 *
 * KEY AGENTS:
 *   - early_typists: Skill-invested users (moderate/constrained) — bear switching costs but no extraction
 *   - typewriter_manufacturers: Respond to demand (institutional/mobile) — no strategic beneficiary
 *   - training_institutions: Teach the standard (organized/constrained) — coordination function
 *   - alternative_layout_advocates: Dvorak/Colemak proponents (moderate/mobile) — excluded from mainstream
 *   - analytical_observer: Sees full structure (analytical/analytical) — no stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.03).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'c1155493-f70a-43b7-b1b6-81d8c0c0d5a1').
narrative_ontology:cs_kernel_codification('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', implicit).
narrative_ontology:cs_authority_grounding('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', practice).
narrative_ontology:cs_reading_relation('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', foundational, persistence_by_network_effects_not_design).
narrative_ontology:cs_axiom_status(persistence_by_network_effects_not_design, holdable).
narrative_ontology:cs_axiom_grounding('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', persistence_by_network_effects_not_design, empirically_contingent).
narrative_ontology:cs_axiom('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', foundational, no_strategic_beneficiary_of_qwerty).
narrative_ontology:cs_axiom_status(no_strategic_beneficiary_of_qwerty, holdable).
narrative_ontology:cs_axiom_grounding('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', no_strategic_beneficiary_of_qwerty, empirically_contingent).
narrative_ontology:cs_reference_frame('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', accidental_founding_condition_1874).
narrative_ontology:cs_drift_state('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c1155493-f70a-43b7-b1b6-81d8c0c0d5a1', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, early_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependency_in_standardization).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, network_effects_as_coordination_mechanism).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, historical_accident_as_founding_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested in learning QWERTY typing skills. Bear switching costs if they change layouts, but these are sunk skill investments, not ongoing extraction. No one collects rents from their continued use of QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, early_typists, payer,
    moderate, biographical, constrained, global).

% Produce keyboards in the standard layout because that is what the market demands. They would switch production to any layout that became the new standard — they have no strategic interest in preserving QWERTY per se. Their benefit is incidental coordination, not extraction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typewriter_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Teach QWERTY because it is the universal standard. Their curricula and certifications are built around it. They benefit from coordination (stable teaching target) but do not extract from learners — they would teach any layout that became standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, training_institutions, beneficiary,
    organized, biographical, constrained, global).

% Advocate for Dvorak, Colemak, or other layouts on efficiency grounds. They are excluded from mainstream adoption not by active suppression but by the coordination equilibrium — no employer, school, or OS defaults to their preferred layout. They can and do use alternatives personally.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Observes the full structural history: an accident in 1874 created a founding condition; network effects in skill, tooling, and infrastructure amplified it into a universal standard. No strategic actor engineered or maintains the lock-in. The efficiency loss is real but diffuse.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__path_dependency_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__path_dependency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal keyboard layout standard that enables interoperability across devices, operating systems, training programs, and user skills — a single layout learned once works everywhere.
% TRANSFER_FUNCTION: No transfer function — there is no extraction. The efficiency loss (if any) relative to alternative layouts is a diffuse externality borne by all users equally, not a transfer to any beneficiary.
% ABSENT_VOICES: No voices are structurally excluded from the conversation about keyboard layouts. Alternative layout advocates participate in public discourse, develop software, and publish research. Their exclusion is from mainstream adoption, not from the conversation.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the world would rearrange: a new standard would emerge (likely through OS defaults or a major manufacturer's choice), billions of users would need to relearn typing, training infrastructure would need to be rebuilt, and the transition would take decades. The coordination function is real and its disappearance would be catastrophic.
% FOUNDING_PROBLEM: Mechanical typebar jamming on early typewriters (1870s) — the QWERTY arrangement spaced commonly-used letter pairs apart to reduce collisions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (mechanical typebar jamming) is corroborated as dead by the entire history of keyboard technology: electric typewriters (1920s), computer terminals (1960s), and modern keyboards (1980s-present) have no typebars. No party disputes this. The arrangement persists despite the founding problem's disappearance — this is the signature of path dependency, not mandatrophy (which requires extraction).
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.03) because no party collects rents from QWERTY's persistence — manufacturers sell what the market demands, and any efficiency loss is a diffuse user externality, not a transfer to a beneficiary. Suppression is negligible (0.02) because alternatives exist (Dvorak since 1936, Colemak since 2006) and nothing prevents adoption except individual switching costs. Theater ratio is minimal (0.05) — the constraint persists because of real network effects, not performative maintenance. Accessibility collapse is very high (0.92) because once the standard is established, alternatives are structurally inaccessible to new users (skill investment, hardware compatibility, software defaults). Resistance is near-zero (0.08) — the constraint meets no organized opposition; alternative layouts persist as niche choices.
 *
 * PERSPECTIVAL GAP:
 *   From the typist's seat, QWERTY feels like a mountain — a fixed feature of the world they must learn. From the manufacturer's seat, it is a rope — a coordination standard that solves the interoperability problem. From the alternative layout advocate's seat, it appears as a snare — an inefficient standard that could be changed but resists change. The engine computes these per-seat divergences from the structural data (power, exit, scope). This reading authors the mountain claim because the structural referent (the standing arrangement) has no extraction or suppression machinery; the apparent coercion is an illusion of network effects.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared. The efficiency loss is a diffuse externality, not a transfer. Manufacturers are not beneficiaries — they respond to demand and would switch if demand shifted (mobile exit). Typists bear switching costs but these are sunk skill investments, not ongoing extraction. The constraint's persistence is self-sustaining via network effects, not enforced by any party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typebar jamming) is dead — modern keyboards have no typebars. Yet the arrangement persists without extraction or enforcement. This is not mandatrophy in the extraction sense (no mandate outlived its function to become a rent-collection mechanism). It is pure path dependency: a coordination equilibrium that persists because the cost of coordinated switching exceeds the distributed benefit, with no actor positioned to capture the difference. The mountain classification correctly captures this — it is a structural feature of the technological ecosystem, not a degraded institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is QWERTY persistence a reading of the contested kernel ''qwerty_persistence_inevitability'' rather than a standalone constraint?',
    'The committer frame identifies this as one reading (path_dependency_reading) of a kernel with a sibling reading (strategic_lock_in_reading). The structural delta between readings is the presence/absence of victim and beneficiary sets. Resolution requires committing to one reading''s structural commitments.',
    'If this is a kernel reading, ε=0.03 describes the standing arrangement under contest as this reading sees it; the sibling reading would author a different constraint with different ε, different beneficiaries/victims, and different claimed_type. The engine classifies each reading independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this constraint is a kernel reading and how that structures its ε and classification.').

omega_variable(
    strategic_lock_in_evidence,
    'Did typewriter manufacturers or early computer firms actively engineer lock-in through training partnerships, procurement contracts, or cartel standardization?',
    'Historical research on Remington, Underwood, Royal, and early computer procurement (e.g., IBM, Teletype) examining training contracts, government standards, and industry coordination. The sibling reading asserts active engineering; this reading asserts accident and diffusion.',
    'If evidence of active lock-in engineering exists, the strategic_lock_in_reading gains structural support (victim set: typists locked into inefficient layout; beneficiary set: manufacturers capturing training/rental revenue). This reading would then be a false mountain (FSM candidate). If no such evidence, this reading''s mountain claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_lock_in_evidence, empirical, 'Whether historical record supports active lock-in engineering vs. pure accident and diffusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1874, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_path_dep_tr_t1874, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1874, 0.01).
narrative_ontology:measurement(qwerty_path_dep_tr_t1900, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(qwerty_path_dep_tr_t1930, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1930, 0.03).
narrative_ontology:measurement(qwerty_path_dep_tr_t1960, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(qwerty_path_dep_tr_t1980, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(qwerty_path_dep_tr_t2000, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(qwerty_path_dep_tr_t2025, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(qwerty_path_dep_be_t1874, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1874, 0.02).
narrative_ontology:measurement(qwerty_path_dep_be_t1900, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1900, 0.02).
narrative_ontology:measurement(qwerty_path_dep_be_t1930, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1930, 0.02).
narrative_ontology:measurement(qwerty_path_dep_be_t1960, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(qwerty_path_dep_be_t1980, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1980, 0.03).
narrative_ontology:measurement(qwerty_path_dep_be_t2000, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2000, 0.03).
narrative_ontology:measurement(qwerty_path_dep_be_t2025, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2025, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_path_dep_su_t1874, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1874, 0.01).
narrative_ontology:measurement(qwerty_path_dep_su_t1900, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1900, 0.01).
narrative_ontology:measurement(qwerty_path_dep_su_t1930, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1930, 0.01).
narrative_ontology:measurement(qwerty_path_dep_su_t1960, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(qwerty_path_dep_su_t1980, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement(qwerty_path_dep_su_t2000, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(qwerty_path_dep_su_t2025, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2025, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This constraint and strategic_lock_in_reading form a constraint family decomposing the natural-language concept 'QWERTY persistence'. This reading (path_dependency) has ε≈0.03, no beneficiaries/victims, mountain. The sibling reading (strategic_lock_in) would have ε≈0.4-0.6, beneficiaries (manufacturers), victims (typists), tangled_rope/snare. They share the same empirical referent (QWERTY's persistence) but differ on the structural mechanism. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
