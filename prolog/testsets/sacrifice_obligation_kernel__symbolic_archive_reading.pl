% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Cultural-Historical Archive (Symbolic Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Under the symbolic-archive reading, Jewish law's sacrifice corpus
 *   (detailed rules about Temple offerings, their conditions, their meanings)
 *   functions as a cultural-historical archive. Study of these texts is
 *   voluntary, preserves Jewish collective memory and legal reasoning
 *   tradition, and contributes to community identity — but makes NO claim
 *   that study fulfills a binding halakhic obligation. This reading separates
 *   the preservation function (real, valuable, non-coercive) from the
 *   obligation function (contested across the four sibling readings). The
 *   extractiveness is zero because no one is compelled to study, no one is
 *   punished for non-study, and no authority enforces participation as a
 *   mitzvah. The constraint persists not through coercion but through
 *   cultural valuation of learning itself.
 *
 * KEY AGENTS:
 *   - jewish_community_scholars: voluntary participants in a cultural learning practice; no obligation, no penalty for exit
 *   - jewish_institutional_memory: the non-agent collective archive that benefits from study under this reading
 *   - individual_learners: gain identity and intellectual engagement through voluntary participation
 *   - competing_interpretive_readings: other readings (messianic_suspension, performance_only, study_as_exercise) coexist and contest; they are not external challengers but internal alternative framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive (Symbolic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca').
narrative_ontology:cs_kernel_codification('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', distributed).
narrative_ontology:cs_authority_grounding('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', distributed).
narrative_ontology:cs_reading_relation('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', foundational, study_is_voluntary_preservation).
narrative_ontology:cs_axiom_status(study_is_voluntary_preservation, holdable).
narrative_ontology:cs_axiom_grounding('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', study_is_voluntary_preservation, conventional).
narrative_ontology:cs_axiom('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', secondary, zero_extraction_cultural_transmission).
narrative_ontology:cs_axiom_status(zero_extraction_cultural_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', zero_extraction_cultural_transmission, deontological).
narrative_ontology:cs_reference_frame('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', post_temple_voluntary_cultural_preservation).
narrative_ontology:cs_drift_state('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', contemporary_diaspora_halakhic_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a93ed5d0-372d-4bb7-b100-fd4cb0bd3eca', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness=0.0 because the reading itself asserts zero binding obligation. No one violates the constraint by not studying; no punishment or coercion enforces study. Suppression=0.0 for the same reason — the constraint does not suppress alternatives; other readings remain available. Theater_ratio=0.0 because there is no functional claim being performed — the reading is transparent about what it asserts: preservation, not obligation. Accessibility_collapse=0.95 (very high) because once a learner understands this reading, the alternative (non-study) is fully available and culturally acceptable; the constraint does not collapse alternatives at all. Resistance=0.05 (very low) because there is almost nothing to resist — a voluntary cultural practice meets negligible resistance. The measurement series is empty because the constraint exhibits no temporal drift: voluntary participation rates may fluctuate due to external factors (community size, education availability, cultural salience), but the constraint itself (zero obligation, zero coercion, pure preservation) does not change over time.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence because the reading asserts no binding position for any seat. A scholar studying sacrifice law under the symbolic reading sees themselves as a voluntary participant in cultural transmission. A competing performance-obligation scholar sees the same person as either fulfilling an obligation (exercise reading), failing to fulfill it (performance-only reading), or maintaining readiness for it (messianic-suspension reading). The divergence is in the reading-assignment, not in the per-seat classification of the constraint itself. From the symbolic-archive reading's seat, the constraint computes as zero-extractive rope (pure coordination around memory). From the performance-only reading's seat, the same acts (textual study) are preparatory but do not occupy the obligation, so the constraint would compute as snare-like (obligation imposed; study insufficient to discharge it). The engine's per-seat classification differs because the readings have fundamentally different ε values.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no extraction directionality because there is no coercive mechanism. Both beneficiary groups (jewish_community_scholars and the non-agent jewish_institutional_memory) participate in and benefit from study voluntarily. Individual learners choose to study and benefit culturally without constraint; they could exit costlessly. The directionality framework assumes some mechanism that binds an agent toward or away from the constraint — this reading has none. The entire framework registers as symmetric (d ≈ 0.5 per mechanical derivation from zero suppression, zero extraction) but the commentary notes that 'symmetric' mislabels a voluntary cultural practice. The engine computes d correctly; the interpretation is that d-values are inapplicable here because the constraint operates outside the obligation-enforcement domain.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is immune to mandatrophy because it makes no mandate claim. A mandate — a binding obligation to study, to know, to transmit — would become mandatrophic if its founding purpose (preparation for messianic restoration, exercise of obligation, or performance-readiness) were dead and only preservation remained. Under the symbolic-archive reading, preservation IS the purpose. Study persists because cultural transmission is valued, not because an obsolete obligation is theatrically maintained. The constraint has no sunset clause and no mandate that could become stranded. If this reading were to become the consensus position (displacing the other three), it would represent a successful shift in halakhic framing, not a mandatrophic decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the sacrifice obligation kernel is binding halakhic authority? Does sacrifice obligation remain live, suspended, redefined through study, or exist only as cultural archive?',
    'Authoritative halakhic ruling from a recognized posek (decisor); survey of contemporary Jewish legal consensus; historical analysis of how diaspora Jewish communities came to frame the constraint post-Temple.',
    'The classification of THIS reading depends on the kernel''s resolution. If the symbolic-archive reading becomes authoritative, it stabilizes as rope (voluntary coordination). If performance-only becomes authoritative, this reading reclassifies as preparatory sub-constraint (possibly snare if obligation persists but study is deemed insufficient). If study-as-exercise becomes authoritative, this reading becomes an alternative formulation of the same obligation (functionally equivalent).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which halakhic reading of sacrifice obligation carries binding authority.').

omega_variable(
    cultural_obligation_boundary,
    'Does the voluntary transmission of cultural memory constitute a binding obligation, or is cultural valuation structurally distinct from halakhic obligation?',
    'Philosophical analysis of obligation sources in Jewish law (biblical, rabbinic, custom, consensus); ethnographic study of how Jewish learners themselves characterize study participation (choice vs. duty vs. identity-constitution).',
    'If cultural memory transmission is intrinsically non-obligatory, this reading''s zero-extractiveness is secure. If cultural obligation is a binding category, the reading might reclassify to low-extractiveness rope (cultural pressure substituting for legal compulsion). The boundary between ''valued cultural practice'' and ''obligatory cultural reproduction'' is empirically contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_obligation_boundary, empirical, 'Whether voluntary cultural transmission carries binding force.').

omega_variable(
    identity_fusion_vs_coercion,
    'Is learning sacrifice law experienced as constitutive of Jewish identity (fusion), or as an independent cultural choice? If fusion, does that constitute a form of suppression through identity-lock?',
    'Ethnographic interview with learners about their own experience of study obligation; comparison of exit costs for those who identify strongly with tradition vs. those who do not; analysis of whether study participation correlates with identity affiliation.',
    'If identity fusion is present and constitutive, suppression might be recalibrated upward (internalized obligation through identity identification). If study is experienced as genuinely optional, the zero-suppression value holds. Identity-locked individuals might face higher psychological exit cost even though structural coercion is absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_coercion, empirical, 'Whether cultural learning is experienced as identity-constitutive (internalized obligation) or autonomous choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the sacrifice_obligation_kernel family. All four readings (symbolic_archive, messianic_suspension, performance_only, study_as_exercise) address the same founding problem: how Jewish law maintains continuity of sacrifice knowledge after Temple destruction. Each reading has a distinct ε value and occupies a different structural position. They are NOT perspectives on one constraint; they are FOUR DIFFERENT CONSTRAINTS sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
