% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Archival Practice: Study Preserves Textual Tradition Without Normative Force
 *   domain: religious/textual/cultural
 *
 * SUMMARY:
 *   Under this reading, the obligation to perform sacrifice ceased when
 *   historical conditions made performance impossible. Study of the sacrifice
 *   texts continues as a cultural and scholarly practice, preserving textual
 *   knowledge and interpretive tradition. However, study carries NO normative
 *   force — no one is obligated to engage in it, it creates no binding
 *   requirement, it extracts nothing from those who participate. The
 *   constraint is instantiated as a mountain: it emerges from historical fact
 *   (the Temple's destruction) and remains stable across time because the
 *   condition that ended the obligation (absence of the sacrificial
 *   institution) persists. Study happens because communities value cultural
 *   continuity, not because law requires it.
 *
 * KEY AGENTS:
 *   - rabbinic_scholars: Specialists in textual interpretation; engage in sacrifice study as professional practice but are not bound by obligation under this reading
 *   - religious_community_members: Participate in study voluntarily; have access to texts and interpretations but face no requirement to engage
 *   - sibling_reading_communities: Hold alternative interpretations (study_as_performance, messianic_suspension, performance_only) within the same textual tradition; the readings compete but coexist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, mountain).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Archival Practice: Study Preserves Textual Tradition Without Normative Force").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/textual/cultural").

domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '8e46d6a2-62ed-4dd8-830a-a297fbe18a50').
narrative_ontology:cs_kernel_codification('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', fixed_text).
narrative_ontology:cs_authority_grounding('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', lineage).
narrative_ontology:cs_interpretation_layer_present('8e46d6a2-62ed-4dd8-830a-a297fbe18a50').
narrative_ontology:cs_reading_relation('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', foundational, obligation_historically_terminated).
narrative_ontology:cs_axiom_status(obligation_historically_terminated, holdable).
narrative_ontology:cs_axiom_grounding('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', obligation_historically_terminated, empirically_contingent).
narrative_ontology:cs_axiom('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', secondary, study_severed_from_normative_force).
narrative_ontology:cs_axiom_status(study_severed_from_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', study_severed_from_normative_force, deontological).
narrative_ontology:cs_reference_frame('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', obligation_terminated_knowledge_preserved).
narrative_ontology:cs_drift_state('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', contemporary_secular_and_liberal_religious_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e46d6a2-62ed-4dd8-830a-a297fbe18a50', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual knowledge and cultural memory: the detailed study of sacrifice law maintains continuous engagement with foundational religious texts and the interpretive tradition surrounding them, ensuring no knowledge of the ancient practice vanishes from the community's collective record.
% TRANSFER_FUNCTION: No transfer occurs. Study is a cultural practice that sustains itself through community participation; no normative obligation binds anyone to engage in it.
% ABSENT_VOICES: None. Under this reading, the absence of binding obligation means no one is structurally excluded from the conversation — participation is voluntary and textual engagement is open to any competent interpreter.
% DISAPPEARANCE_RATIONALE: If sacrifice study ceased, the community's knowledge of sacrifice law would gradually fade, but no normative requirement would be violated and no enforcement mechanism would activate. The constraint is descriptive (study happens because the tradition values it) rather than prescriptive (study must happen because law commands it). The world's arrangement of obligations and permissions does not depend on study continuing.
% FOUNDING_PROBLEM: After the destruction of the sacrificial system (historically, the destruction of the Second Temple in 70 CE), the religious obligation to perform sacrifices could no longer be fulfilled through physical practice. The reading being instantiated holds that this ended the obligation entirely: sacrifice law remains textually preserved and studied, but carries no normative force.
% FOUNDING_PROBLEM_CORROBORATION: This reading is attested within Jewish legal tradition (particularly in rationalist and philosophical streams — Maimonides' codification treats sacrifice law as binding only when the Temple exists; modern Conservative and Reform Judaism largely treat it as historical/cultural rather than obligatory). It is also supported by secular historians of religion who treat sacrifice as an institution whose social function ended with its cultic practice. It is contested by other readings within the same tradition (see sibling_readings).
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because there is no normative claim — study is described as voluntary cultural practice, not as binding obligation. Suppression is zero because no coercive mechanism enforces participation; exit is costless (one simply stops studying) and alternatives exist (one can engage with the texts through different frameworks or not at all). Theater ratio is zero: there is no performance of obligation, no theatrical maintenance of binding force where the force has ended. Accessibility_collapse is high (0.95) because once the reading is adopted, alternatives (that study carries normative weight, or that obligation persists in other forms) become epistemically inaccessible within this framework — the framework has answered the question decisively. Resistance is near-zero (0.05) because the reading aligns with historical fact and faces minimal active opposition from the secular scholarly community; resistance comes from rival religious readings, which is internal to the kernel dispute, not resistance to the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The archival_preservation reading has no identified beneficiaries or victims because extractiveness is zero — no one bears costs and no one collects benefits from the constraint's operation. Study happens; knowledge persists; no transfer occurs. The reading decouples study from obligation entirely, making the structure symmetric across all participants: scholars and lay community members alike have the same relationship to the texts (voluntary engagement, no binding force, no extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy in this reading because the founding obligation (to perform sacrifice) is treated as genuinely terminated, not as a command whose function has atrophied. The founding problem (how to relate to sacrifice law after the Temple's destruction) is treated as SOLVED under this reading: the solution is that obligation ends. Other readings treat the problem as UNSOLVED or SUSPENDED (obligation persists in transformed form or awaits restoration), which leads to different constraint types. This reading avoids mandatrophy by declaring the obligation extinct rather than degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_reading_choice,
    'Is the non-binding status of sacrifice law a natural consequence of historical fact (the Temple destruction made performance physically impossible), or a theological/legal reading choice (the obligation persists in other forms — study, intention, or suspended readiness)?',
    'Textual analysis of foundational sources (Talmudic dispute records, codification histories) to determine whether the sources themselves entail the non-binding reading or whether it represents one defensible interpretation among others.',
    'If natural consequence: the constraint is genuinely a mountain (obligation ceases when its condition becomes impossible). If reading choice: the constraint is one outcome of a contested kernel, and the reading''s authority rests on interpretive tradition and community practice, not on logical necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_reading_choice, conceptual, 'Whether the non-binding status follows necessarily from historical fact or represents a chosen reading of contested sources.').

omega_variable(
    voluntary_study_vs_cultural_obligation,
    'Is study of sacrifice law truly voluntary practice (extractiveness = 0) or does cultural expectation create informal obligation for religious specialists (scholars, rabbis) to engage in it?',
    'Ethnographic observation: do religious scholars experience study as chosen practice or as expected duty? Do those who cease study face community pressure? How is non-engagement socially framed?',
    'If genuinely voluntary: extractiveness remains zero and the constraint is a natural coordination fact (knowledge preservation). If culturally obligatory: extractiveness may be non-zero (informal extraction via reputation/status) and the constraint becomes rope or tangled_rope for the specialist seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_study_vs_cultural_obligation, empirical, 'Whether cultural practice creates informal obligation for certain seats.').

omega_variable(
    competing_readings_live_in_same_framework,
    'Can the same Jewish legal and theological framework hold multiple readings of sacrifice obligation simultaneously, or does commitment to one reading foreclose the others?',
    'Analysis of Jewish legal sources and contemporary interpretive authority: do recognized scholars and communities hold different readings without declaring each other''s positions incoherent? Or does each reading require its own distinct framework?',
    'If coexistable: sibling readings remain live competitors within a single tradition. If not: this reading forecloses some siblings (or vice versa) within any single framework, and the four readings do not form a simple four-way kernel but rather a nested set of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_readings_live_in_same_framework, conceptual, 'Whether multiple readings are logically compatible within shared foundational commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, information_standard).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__archival_preservation, 0.0).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).

% DUAL FORMULATION NOTE:
% The four readings (archival_preservation, study_as_performance, messianic_suspension, performance_only) are structurally distinct constraints instantiating different readings of the same contested kernel: what is the status of sacrifice obligation after the Temple's destruction? Each reading produces a different ε, different beneficiary/victim structure, and different type. They are linked as a constraint family through the network.affects_constraints array. Archival_preservation (this story) has zero extractiveness and is classified as a mountain; the siblings have non-zero extractiveness and organize around different conceptions of binding force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
