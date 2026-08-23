% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto Endogenous Reinterpretation Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) suspended the practice of
 *   plural marriage while retaining the doctrine. This reading — the
 *   endogenous reinterpretation reading — holds that God revealed the
 *   temporal suspension to preserve the church's salvific mission (temple
 *   ordinances, missionary work, institutional continuity). The constraint
 *   coordinates the global church around this new prophetic directive. The
 *   church institution and mainstream membership benefit from survival and
 *   continued sacramental access. Fundamentalist dissenters who maintain the
 *   original practice are excommunicated and bear the costs of exclusion. The
 *   constraint operates as a rope: genuine coordination around a prophetic
 *   directive with active enforcement (excommunication) but minimal
 *   extraction from the coordinated majority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.35).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.45).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto Endogenous Reinterpretation Reading").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1').
narrative_ontology:cs_kernel_codification('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', formalized).
narrative_ontology:cs_authority_grounding('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', lineage).
narrative_ontology:cs_interpretation_layer_present('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1').
narrative_ontology:cs_reading_relation('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', foundational, prophetic_reinterpretation_legitimate).
narrative_ontology:cs_axiom_status(prophetic_reinterpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', prophetic_reinterpretation_legitimate, theological).
narrative_ontology:cs_axiom('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', foundational, salvific_mission_preservation_justifies_practice_suspension).
narrative_ontology:cs_axiom_status(salvific_mission_preservation_justifies_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', salvific_mission_preservation_justifies_practice_suspension, theological).
narrative_ontology:cs_reference_frame('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', prophetic_succession_continuity).
narrative_ontology:cs_drift_state('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', contemporary_correlation_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('2e46e8cd-33fc-44b6-88c1-f99ebb2c67f1', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_continuity).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, salvific_mission_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto suspending plural marriage practice while retaining the doctrine. Preserves the church's legal existence, temple access, missionary operations, and institutional continuity. Holds the prophetic office that authorizes the reinterpretation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains continued temple access, missionary service eligibility, and community standing by accepting the Manifesto. Avoids the legal penalties and social ostracism faced by non-compliant members. Their religious practice is coordinated around the new prophetic directive.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    organized, biographical, constrained, global).

% Maintain the original plural marriage practice as a divine requirement. Face excommunication, loss of temple access, severance from mainstream community, and legal prosecution. Their identity is fused to the original mandate; exit means abandoning what they believe is a salvation-necessary covenant.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    moderate, biographical, identity_locked, regional).

% Enacted escalating anti-polygamy legislation (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatening church disincorporation and asset seizure. The Manifesto emerged under this pressure. They monitor compliance but are not direct parties to the theological reinterpretation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Scholarly observers of Mormon history, religious studies, and political theology. They see the full structural field: the contested kernel, the three competing readings, and the institutional dynamics. They neither collect nor pay within the constraint.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's survival and continued operation by uniting the membership around a new prophetic directive that suspends a practice causing existential legal threat, while preserving the underlying doctrine for future fulfillment.
% TRANSFER_FUNCTION: Moves institutional legitimacy, legal survival, and sacramental access from the fundamentalist practice of plural marriage to the prophetic reinterpretation that suspends it. The church institution retains its corporate existence and temple operations; fundamentalist dissenters bear the cost of exclusion and prosecution.
% ABSENT_VOICES: Fundamentalist dissenters are structurally excluded by excommunication; they would argue the Manifesto is a capitulation, not revelation, and that the original mandate remains binding. Early post-Manifesto plural marriage participants (1890-1904) were also excluded from the new coordination once the Second Manifesto enforced compliance.
% DISAPPEARANCE_RATIONALE: If the endogenous reinterpretation constraint vanished overnight, the church would lose its primary coordination mechanism for navigating the federal threat. The membership would fragment between fundamentalist maintenance and institutional accommodation, temple access would be contested, and the church's legal standing would revert to the pre-1890 existential crisis.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation (culminating in the Edmunds-Tucker Act 1887) threatened the church's legal existence, temple access, and missionary operations — the institutional vehicles of its salvific mission.
% FOUNDING_PROBLEM_CORROBORATION: The federal legislative record (Edmunds Act, Edmunds-Tucker Act), contemporary newspaper accounts, and the church's own 1890-1904 correspondence confirm the existential legal threat. Non-beneficiary corroboration comes from federal congressional reports, gentile Utah press, and independent legal historians who document the disincorporation threat as real and immediate.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) — the constraint extracts from fundamentalist dissenters (excommunication, loss of standing) but provides genuine coordination value to the mainstream. Suppression is moderate (0.45) — enforcement targets dissenters but the coordinated majority participates voluntarily. Theater ratio is low (0.2) — the prophetic directive is treated as binding revelation, not performative. Accessibility collapse is moderate (0.5) — alternatives exist (fundamentalist groups) but are costly. Resistance is moderate (0.5) — fundamentalist schisms persist but are contained. The measurement series shows declining extractiveness and suppression as the federal threat receded, with a slight uptick in suppression recently as fundamentalist groups gain visibility.
 *
 * PERSPECTIVAL GAP:
 *   From the church institution's seat, the constraint is pure coordination (rope) — a prophetic solution to an existential threat. From the fundamentalist dissenter's seat, the same structure operates as extraction (snare-like) — the prophetic claim masks a capitulation to coercion, and their exclusion is the price. The engine computes this divergence from the structural data: identity_locked exit for dissenters amplifies their effective extraction, while arbitrage exit for the institution damps it.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institution (agenda_setter, institutional power, arbitrage exit) sits at the beneficiary end (d ~ 0.15) — it controls the reinterpretation and gains survival. Mainstream membership (beneficiary, organized power, constrained exit) sits near symmetric (d ~ 0.4) — genuine coordination benefit with some diffuse cost. Fundamentalist dissenters (payer, moderate power, identity_locked exit) sit at the target end (d ~ 0.85) — their identity is fused to the original mandate, making exit existentially costly. Federal authorities and analytical observers are external to the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — the legal threat was resolved by Utah statehood (1896) and the Smoot hearings (1904-1907). Yet the constraint persists because the prophetic directive became a coordination anchor for institutional identity, not merely a tactical response. This is not mandatrophy (coordination outliving its function) but rather function transformation: the reinterpretation that saved the institution became the institution's self-understanding. The mandate is resolved in its original tactical sense but lives on as constitutional identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (endogenous_reinterpretation_reading) of the contested kernel plural_marriage_mandate. What structural elements distinguish it from its sibling readings (exogenous_override_reading, institutional_pragmatism_reading)?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, claimed types, and cs_structure axioms. The kernel''s constraint family should be generated as three linked stories.',
    'If the readings are not properly decomposed, the kernel''s contested nature is obscured and ε becomes observer-relative. Proper decomposition yields three ε-invariant constraints with distinct classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer frame: this story is one reading of a contested kernel; structural deltas to siblings must be authored in separate constraint files.').

omega_variable(
    prophetic_legitimacy_vs_coercion,
    'Is the Manifesto''s prophetic authority genuinely experienced by the coordinated majority, or is the revelation narrative a post-hoc legitimating cover for coercion-driven capitulation?',
    'Historical analysis of contemporary reception (1890-1904): membership acceptance patterns, leadership discourse, fundamentalist schism timing. If acceptance was immediate and widespread among the coordinated, the prophetic experience is genuine; if acceptance followed enforcement and required discipline, the cover-story hypothesis gains weight.',
    'If the prophetic experience is genuine, the constraint is a rope (coordination around authentic revelation). If it is a cover story, the constraint is a tangled_rope (coordination function masking extraction from fundamentalists) or snare (pure extraction legitimated by false revelation claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_legitimacy_vs_coercion, empirical, 'Whether the revelation claim functions as genuine coordination anchor or legitimating cover.').

omega_variable(
    fundamentalist_victim_status,
    'Are fundamentalist dissenters genuine victims of extraction (excommunicated for maintaining a binding divine requirement) or schismatics who rejected legitimate prophetic authority?',
    'Theological analysis of the kernel''s own criteria for prophetic authority and dissent. If the kernel''s internal logic treats the Manifesto as binding revelation, dissenters are schismatics (not victims). If the kernel''s logic treats the original mandate as irrevocable, the Manifesto is an innovation and dissenters are victims of exclusion.',
    'If dissenters are schismatics, the victim declaration in this story is misplaced — the constraint has no victims and is a pure rope. If dissenters are victims, the constraint has asymmetric extraction (tangled_rope) despite its rope claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fundamentalist_victim_status, conceptual, 'Whether the fundamentalist position constitutes victimhood or schism within the kernel''s own logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(plur_tr_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(plur_tr_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(plur_tr_t50, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(plur_tr_t75, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(plur_tr_t100, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(plur_tr_t130, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 130, 0.2).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(plur_be_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(plur_be_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(plur_be_t50, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(plur_be_t75, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 75, 0.35).
narrative_ontology:measurement(plur_be_t100, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 100, 0.33).
narrative_ontology:measurement(plur_be_t130, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 130, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(plur_su_t10, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(plur_su_t25, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(plur_su_t50, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(plur_su_t75, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(plur_su_t100, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(plur_su_t130, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 130, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint family (three readings of plural_marriage_mandate kernel) demonstrates the ε-invariance principle: the endogenous reinterpretation reading yields a rope (coordination around prophetic directive), the exogenous override reading yields a snare (coercion masking as revelation), and the institutional pragmatism reading yields a tangled_rope (strategic adaptation with revelation narrative as legitimating cover). Each reading has a stable ε relative to its own structural referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__endogenous_reinterpretation_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
