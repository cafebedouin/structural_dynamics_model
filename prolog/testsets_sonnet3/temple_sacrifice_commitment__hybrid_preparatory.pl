% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Sacrificial Law Study as Suspended Preparatory Commitment (Hybrid Reading)
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates the hybrid_preparatory reading of the
 *   temple_sacrifice_commitment kernel: the claim that intensive study of
 *   sacrificial law (Seder Kodashim) occupies a suspended middle state
 *   between full performance (impossible absent the Temple) and mere
 *   archiving (which this reading rejects as insufficiently reverent toward
 *   the eventual commandment). Under this reading, study is deliberately
 *   positioned as preparatory exercise — neither fulfilling the commandment
 *   nor abandoning it, but readying the community and its scholars for
 *   messianic restoration. The reading differs structurally from its
 *   siblings: study_as_exercise holds intellectual engagement itself fully
 *   occupies the commandment (lower extraction, since nothing is deferred);
 *   performance_only holds study without material performance is mere
 *   archival preservation with no live commandment content (near-zero
 *   extraction, since nothing contested is being extracted for);
 *   symbolic_transformation holds prayer/study have been authorized as full
 *   replacements, closing the deferral altogether. This reading's moderate
 *   extraction sits between those poles precisely because it keeps the
 *   commitment open-ended and unresolved — resources continue to flow toward
 *   a state that is deliberately never fully cashed out.
 *
 * KEY AGENTS:
 *   - kodashim_scholars: Primary beneficiary/agenda_setter (organized/identity_locked) — career and institutional standing ride on the suspended-preparatory framing
 *   - yeshiva_institutions: Institutional beneficiary (institutional/arbitrage) — fundraising and curricular authority
 *   - community_donors_funding_study: Primary payer (moderate/constrained) — funds indefinite deferral
 *   - students_diverted_from_practical_torah_study: Payer (powerless/constrained) — bears opportunity cost of study time
 *   - families_of_full_time_kollel_scholars: Payer (powerless/trapped) — bears household economic cost
 *   - reform_and_conservative_halakhic_authorities: Excluded voice — holds the symbolic_transformation view, unrepresented in Orthodox curricular decisions
 *   - comparative_religion_scholars: Analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.42).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.38).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Sacrificial Law Study as Suspended Preparatory Commitment (Hybrid Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious/legal/institutional").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'c4f85dc9-ab12-4335-a98e-6d772c81630b').
narrative_ontology:cs_kernel_codification('c4f85dc9-ab12-4335-a98e-6d772c81630b', fixed_text).
narrative_ontology:cs_authority_grounding('c4f85dc9-ab12-4335-a98e-6d772c81630b', lineage).
narrative_ontology:cs_interpretation_layer_present('c4f85dc9-ab12-4335-a98e-6d772c81630b').
narrative_ontology:cs_reading_relation('c4f85dc9-ab12-4335-a98e-6d772c81630b', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('c4f85dc9-ab12-4335-a98e-6d772c81630b', temple_sacrifice_commitment__performance_only, influences).
narrative_ontology:cs_reading_relation('c4f85dc9-ab12-4335-a98e-6d772c81630b', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('c4f85dc9-ab12-4335-a98e-6d772c81630b', foundational, commitment_remains_open_pending_restoration).
narrative_ontology:cs_axiom_status(commitment_remains_open_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('c4f85dc9-ab12-4335-a98e-6d772c81630b', commitment_remains_open_pending_restoration, deontological).
narrative_ontology:cs_axiom('c4f85dc9-ab12-4335-a98e-6d772c81630b', foundational, study_alone_does_not_discharge_the_commandment).
narrative_ontology:cs_axiom_status(study_alone_does_not_discharge_the_commandment, holdable).
narrative_ontology:cs_axiom_grounding('c4f85dc9-ab12-4335-a98e-6d772c81630b', study_alone_does_not_discharge_the_commandment, conventional).
narrative_ontology:cs_reference_frame('c4f85dc9-ab12-4335-a98e-6d772c81630b', second_temple_active_performance).
narrative_ontology:cs_drift_state('c4f85dc9-ab12-4335-a98e-6d772c81630b', contemporary_post_exile_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4f85dc9-ab12-4335-a98e-6d772c81630b', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_restorationist_movements).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_donors_funding_study).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_diverted_from_practical_torah_study).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, families_of_full_time_kollel_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, eventual_temple_restoration_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, unbroken_commitment_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote careers to studying the order of sacrificial law (Seder Kodashim) despite no functioning Temple. Their professional standing, publication record, and institutional position depend on this study being treated as a genuine occupation of the commandment, not archival curiosity. They set the norms of what counts as rigorous engagement and administer the curriculum that channels students toward this material.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars, agenda_setter).

% Draw donor funding and student enrollment partly on the strength of maintaining full-curriculum coverage, including tractates on sacrifice that can never be practiced. Institutional prestige and continuity narratives are built on the claim that the community must be ready to resume practice at any moment; the institutions control how resources are allocated toward this preparatory function versus other communal needs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Advocate actively for readiness postures (including physical preparation of ritual objects) whose legitimacy depends on sacrificial law remaining a live, occupiable commitment rather than a settled archive or a superseded practice. Their organizational purpose and fundraising rest on the suspended-but-live framing this reading provides.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_restorationist_movements, beneficiary,
    organized, civilizational, identity_locked, global).

% Contribute charitable funds understanding them to sustain Torah study broadly; a portion is allocated to sustaining full-curriculum Kodashim scholarship whose practical payoff is indefinitely deferred. Exit means redirecting charitable giving elsewhere, which risks communal disapproval and loss of social standing within observant networks.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_donors_funding_study, payer,
    moderate, generational, constrained, national).

% Spend formative study years mastering sacrificial law that has no practical civil, ritual, or interpersonal application in their present lives, at the expense of time that could go toward areas of law with immediate applicability. Curricular structure and communal expectation make opting out of this material costly to social and religious standing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_diverted_from_practical_torah_study, payer,
    powerless, biographical, constrained, local).

% Bear the household economic cost when a scholar's study time, including substantial hours on non-performable sacrificial law, is treated as a full occupational commitment justifying continued communal stipends rather than wage labor. Their exit options are limited by social and religious expectations tying household honor to the scholar's continued study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, families_of_full_time_kollel_scholars, payer,
    powerless, biographical, trapped, local).

% Hold that sacrifice law has been superseded or transformed (the symbolic_transformation reading) and would object that treating it as a live, merely-suspended commitment misdescribes its actual status, but their view is not represented within the institutions that set curriculum and fundraising narratives for this reading's adherents.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, reform_and_conservative_halakhic_authorities, excluded,
    organized, generational, mobile, national).

% Study the sociological function of suspended-commitment doctrines across religious traditions, documenting how such doctrines sustain institutional continuity and resource allocation independent of practical performability.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of sacrificial procedure across generations without material practice, so that if messianic restoration occurs, the community is not required to reconstruct the law from scratch — coordinating collective memory against a low-probability, high-stakes future contingency.
% TRANSFER_FUNCTION: Moves charitable donations, student study-hours, and household economic sacrifice from donors, students, and scholar families toward institutions and scholars who administer and benefit from the suspended-preparatory framing, in exchange for continuity of a communal readiness narrative.
% ABSENT_VOICES: Reform and Conservative halakhic authorities who hold the symbolic_transformation reading are not consulted in Orthodox curricular decisions; students who might prefer applied halakha have limited standing to challenge tractate allocation within yeshiva structures.
% DISAPPEARANCE_RATIONALE: Institutions and scholars insist the study's disappearance would represent abandonment of covenantal continuity and would rearrange communal identity; critics (including some within Orthodoxy) argue that redirecting the same hours toward applied law would change little in practice except freeing resources — the parties dispute which outcome would actually occur.
% FOUNDING_PROBLEM: After the Temple's destruction, the rabbinic tradition needed to prevent the sacrificial commandments from being treated as abrogated or forgotten, since abandonment might be read as conceding the commandments were never binding or that restoration was theologically impossible.
% FOUNDING_PROBLEM_CORROBORATION: Kodashim scholars and yeshiva institutions attest the founding problem remains live because restoration remains an article of faith. Comparative religion scholars and some Modern Orthodox halakhic commentators, writing from outside the institutions that fund and administer this study, note that the practical function has shifted from restoration-readiness to identity-maintenance and institutional continuity, a shift the benefiting institutions do not foreground.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at moderate (0.42) because the hybrid_preparatory reading extracts real cognitive and financial resources for a benefit whose realization is deliberately indefinite — the reading's coordination function (preserving technical knowledge against restoration) is genuine but is bundled with concentrated institutional benefit (career structures, fundraising narratives) that does not depend on restoration ever occurring. Suppression is moderate (0.38): social and communal pressure, not legal coercion, keeps students and donor families inside the arrangement. Theater ratio is moderate and rising (0.18 to 0.30) reflecting the gradual institutionalization of readiness rhetoric (e.g., reconstructed ritual implements, dedicated preparatory associations) that increasingly performs restoration-readiness for community consumption rather than purely transmitting technical content.
 *
 * DIRECTIONALITY LOGIC:
 *   Kodashim scholars and yeshiva institutions sit near the beneficiary end: they administer the framing, collect funding and prestige from it, and face no material cost if restoration never occurs. Donors, diverted students, and kollel families sit toward the target end: their resources fund a commitment structure whose payoff is permanently deferred by design, and their exit is constrained by identity and communal-standing costs rather than by law.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_preparatory reading is structurally built to resist mandatrophy detection: because the commitment is defined as perpetually suspended rather than either fulfilled or abandoned, the founding problem (preventing abrogation) can never be definitively resolved as 'dead,' which forecloses the ordinary test for whether an arrangement has outlived its function. This is precisely why the founding_problem_status is authored as contested rather than dead — the reading's own structure makes the mandatrophy question undecidable from inside it, and only external corroboration (comparative religion scholars, dissenting halakhic voices) can surface the shift toward identity-maintenance function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspended_state_coherence,
    'Is ''suspended commitment'' a coherent intermediate category, or is it a rhetorical device that lets institutions claim the benefits of both full occupation (moral seriousness, communal centrality) and mere archiving (no obligation to actually perform) without the costs of either?',
    'Comparative analysis across religious traditions with structurally similar suspended-commitment doctrines (e.g., dormant covenants, deferred eschatological practices) to see whether the category reliably tracks a distinct social function or reliably tracks institutional resource capture.',
    'If the category is a genuine third state, the moderate extraction reflects real coordination cost of maintained readiness. If it is a rhetorical device, the extraction is closer to pure institutional rent dressed in preparatory language, pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspended_state_coherence, conceptual, 'Whether the suspended-preparatory category is structurally real or a legitimating device.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does this particular community/institution hold the hybrid_preparatory reading rather than one of the three sibling readings (study_as_exercise, performance_only, symbolic_transformation), and does that selection track theological reasoning or institutional interest?',
    'Historical and sociological tracing of which reading dominates in which institutional contexts, correlated with funding structures and curricular incentives — does the reading track denominational lineage independent of institutional benefit, or does institutional benefit predict reading adoption?',
    'If reading adoption tracks institutional benefit rather than independent theological reasoning, this is evidence the reading itself functions partly as a legitimating artifact of the beneficiary structure rather than a pure doctrinal position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading-selection across the kernel''s four readings correlates with institutional benefit.').

omega_variable(
    restoration_probability_discount,
    'What probability of actual messianic restoration would the community itself assign if pressed, and does the study''s continued funding level track that probability or exceed what the probability alone would justify?',
    'Compare resource allocation to sacrificial-law study against resource allocation to other low-probability, high-stakes communal preparations (disaster preparedness, etc.) to see if the ratio is anomalous.',
    'If funding substantially exceeds what the community''s own implicit probability estimate would justify, this supports reading the arrangement as extraction wearing preparatory justification rather than rational hedging.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_probability_discount, empirical, 'Whether resource allocation to the study is proportionate to the community''s actual credence in restoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.18).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.21).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.24).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.26).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.28).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(temp_su_t80, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the temple_sacrifice_commitment kernel. Each reading has a distinct ε and beneficiary/victim structure per the ε-invariance principle: study_as_exercise (lower ε, no deferral — study itself is the full commandment), performance_only (near-zero ε, no live commitment to extract from — study is acknowledged archiving), symbolic_transformation (different ε and beneficiary set — the deferral is closed by authorized replacement), and this hybrid_preparatory reading (moderate ε, ongoing extraction sustained precisely by refusing to resolve into either occupation or archiving).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
