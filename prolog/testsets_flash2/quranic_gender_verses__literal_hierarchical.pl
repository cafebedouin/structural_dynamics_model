% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Quranic Gender Verses (Literal Hierarchical Reading)
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint represents the 'literal_hierarchical' reading of specific
 *   Quranic verses (4:11, 2:282, 4:34) concerning gender roles and rights. In
 *   this reading, these verses are understood as direct, timeless legal
 *   injunctions establishing male guardianship (qawamah) and differentiated
 *   rights (e.g., inheritance, testimony) as divine ordinance. This
 *   interpretation leads to high base extractiveness for women and strong
 *   suppression of alternative readings, as it is presented as immutable
 *   divine law. The claimed type is 'snare' because the coordination story
 *   (divine order) is cover for substantial, enforced extraction from women,
 *   with high exit costs due to identity-lock and social pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.88).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.92).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Quranic Gender Verses (Literal Hierarchical Reading)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'defe9992-6bdb-4df4-aea7-2cb235011c33').
narrative_ontology:cs_kernel_codification('defe9992-6bdb-4df4-aea7-2cb235011c33', fixed_text).
narrative_ontology:cs_authority_grounding('defe9992-6bdb-4df4-aea7-2cb235011c33', lineage).
narrative_ontology:cs_interpretation_layer_present('defe9992-6bdb-4df4-aea7-2cb235011c33').
narrative_ontology:cs_reading_relation('defe9992-6bdb-4df4-aea7-2cb235011c33', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('defe9992-6bdb-4df4-aea7-2cb235011c33', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('defe9992-6bdb-4df4-aea7-2cb235011c33', foundational, divine_text_is_literal_law).
narrative_ontology:cs_axiom_status(divine_text_is_literal_law, holdable).
narrative_ontology:cs_axiom_grounding('defe9992-6bdb-4df4-aea7-2cb235011c33', divine_text_is_literal_law, theological).
narrative_ontology:cs_axiom('defe9992-6bdb-4df4-aea7-2cb235011c33', foundational, male_guardianship_is_divine_order).
narrative_ontology:cs_axiom_status(male_guardianship_is_divine_order, holdable).
narrative_ontology:cs_axiom_grounding('defe9992-6bdb-4df4-aea7-2cb235011c33', male_guardianship_is_divine_order, theological).
narrative_ontology:cs_reference_frame('defe9992-6bdb-4df4-aea7-2cb235011c33', early_islamic_legal_precedent).
narrative_ontology:cs_drift_state('defe9992-6bdb-4df4-aea7-2cb235011c33', contemporary_global_feminist_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('defe9992-6bdb-4df4-aea7-2cb235011c33', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, conservative_clergy).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_literalist_contexts).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, daughters_in_inheritance).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from explicit divine sanction for their authority within the household, including financial control and disciplinary rights. This reading reinforces their social and legal standing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter,
    powerful, biographical, mobile, local).

% Derive their legal authority from interpreting and enforcing these verses literally. They adjudicate family law, inheritance, and testimony based on these hierarchical principles, consolidating their power.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, constrained, national).

% Gain influence and legitimacy by upholding and teaching this literal, hierarchical interpretation as divine, immutable law. Their authority is tied to the preservation of traditional gender roles.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, conservative_clergy, beneficiary,
    organized, generational, mobile, global).

% Bear the primary costs of this interpretation, experiencing constrained legal autonomy, reduced inheritance shares, and diminished weight of testimony. Their identity is often deeply intertwined with their religious community, making exit extremely difficult.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_literalist_contexts, payer,
    powerless, biographical, identity_locked, local).

% Receive half the inheritance share of sons, as per the literal reading of 4:11. This is a direct financial extraction with no legal recourse within this framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, daughters_in_inheritance, payer,
    powerless, immediate, trapped, local).

% Face challenges in legal proceedings where their testimony may be valued less than a man's (as per 2:282's interpretation), impacting their ability to seek justice or defend their rights.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_litigants, payer,
    powerless, immediate, constrained, local).

% Advocate for egalitarian interpretations but are often marginalized or silenced in contexts dominated by this literalist reading. Their voices are excluded from mainstream religious discourse and legal application.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, liberal_islamic_scholars, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, divinely ordained social and legal hierarchy within families and society, aiming to prevent disputes over authority and resource distribution by assigning fixed roles.
% TRANSFER_FUNCTION: Transfers authority, financial control, and legal precedence from women to men, particularly male household heads and religious institutions, based on divine command.
% ABSENT_VOICES: Feminist theologians and liberal Islamic scholars are largely excluded from the interpretive and legal application processes, where their contextual or egalitarian readings are dismissed as heterodox or un-Islamic. Their arguments for gender equality would directly challenge the divine basis of this hierarchy.
% DISAPPEARANCE_RATIONALE: If the literal, hierarchical interpretation of these verses vanished overnight, the legal and social structures in many Muslim-majority societies would undergo profound reorganization. Family laws, inheritance practices, and the authority of religious courts would be fundamentally challenged, leading to a re-evaluation of gender roles and rights.
% FOUNDING_PROBLEM: To establish clear social order and justice in a nascent Muslim community, particularly regarding family structure, financial responsibilities, and legal testimony, in a manner believed to be divinely guided.
% FOUNDING_PROBLEM_CORROBORATION: Conservative clergy and literalist interpreters attest the problem is live, arguing that divine order is timeless. Liberal scholars and women's rights advocates, from outside the benefiting parties, attest that the original problems were addressed in a specific historical context and that the current literal application creates new injustices, making the founding problem 'dead' in its original form and 'contested' in its contemporary relevance.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because this reading directly mandates significant legal and social disadvantages for women, transferring substantial authority and resources to men. Suppression is also very high (0.92) as this interpretation is often enforced by religious institutions, legal systems, and social norms, with severe consequences (social ostracization, legal penalties) for those who challenge it. Accessibility collapse is high (0.8) because within contexts where this reading is dominant, alternatives are severely limited or actively suppressed. Resistance is moderate (0.4) but growing, as women's rights movements and liberal Islamic scholarship increasingly challenge this interpretation. Theater ratio is low (0.1) because the constraint is actively functional in its extractive and suppressive capacity; there is little performative maintenance without real effect.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male household heads and religious authorities, this constraint is a divinely ordained, stable social order (potentially a 'mountain' or 'rope' of divine law). From the perspective of women, it is a deeply extractive and suppressive 'snare' that limits their autonomy and rights. The engine's classification will reflect the latter due to the high extractiveness and suppression metrics, despite the 'divine ordinance' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads, religious courts, and conservative clergy are clear beneficiaries and agenda-setters, gaining structural authority and resource control (low directionality). Women in these contexts, particularly regarding inheritance and legal testimony, are the primary victims and targets of extraction (high directionality). Liberal Islamic scholars are excluded, their alternative readings suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a divinely-framed hierarchical structure as mere coordination. By identifying the high extractiveness and suppression, it highlights that the 'divine order' narrative serves to legitimize and maintain an arrangement that disproportionately benefits specific groups while imposing significant costs on others, rather than solving a neutral coordination problem. The persistence is due to active enforcement and the suppression of alternatives, not inherent naturalness or universal benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_interpretation,
    'Is the hierarchical interpretation of these verses an immutable divine command, or a historically contingent human interpretation of divine text?',
    'Theological and jurisprudential consensus shifts over centuries, or a definitive re-interpretation by a widely accepted religious authority that gains widespread adherence.',
    'If reclassified as human interpretation, the constraint''s legitimacy as ''divine ordinance'' would collapse, significantly reducing its suppression and extractiveness, potentially reclassifying it as a ''tangled_rope'' or ''piton'' of tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_vs_human_interpretation, conceptual, 'Ambiguity between divine command and human interpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, social ostracism) or internalized (belief in divine command, identity fusion with community)?',
    'Post-exit suppression trajectory: if suppression persists after legal/social barriers are removed (e.g., in diaspora communities), reclassify as partially internalized. If it collapses, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — women carry the suppression with them after exit, making true liberation harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for women.').

omega_variable(
    framing_under_determination,
    'Does the ''literal_hierarchical'' framing accurately capture the constraint, or would a ''contextual_egalitarian'' or ''progressive_abrogation'' framing yield a more accurate classification?',
    'Analysis of the lived experiences of women under different interpretive regimes, and the degree to which each reading aligns with broader Quranic principles of justice and equity.',
    'Adopting an egalitarian or abrogationist framing would drastically reduce the perceived extractiveness and suppression, likely reclassifying the constraint as a ''rope'' or even a ''mountain'' of justice, but this would be a different constraint (a different reading of the kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framings of the Quranic verses lead to different constraint classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t350, quranic_gender_verses__literal_hierarchical, theater_ratio, 350, 0.1).
narrative_ontology:measurement(qura_tr_t700, quranic_gender_verses__literal_hierarchical, theater_ratio, 700, 0.1).
narrative_ontology:measurement(qura_tr_t1050, quranic_gender_verses__literal_hierarchical, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(qura_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(qura_be_t350, quranic_gender_verses__literal_hierarchical, base_extractiveness, 350, 0.85).
narrative_ontology:measurement(qura_be_t700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 700, 0.88).
narrative_ontology:measurement(qura_be_t1050, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1050, 0.88).
narrative_ontology:measurement(qura_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(qura_su_t350, quranic_gender_verses__literal_hierarchical, suppression_requirement, 350, 0.9).
narrative_ontology:measurement(qura_su_t700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 700, 0.92).
narrative_ontology:measurement(qura_su_t1050, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1050, 0.92).
narrative_ontology:measurement(qura_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_family_law_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, inheritance_laws_in_muslim_majority_states).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, womens_testimony_rules).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quranic_gender_verses' kernel. This 'literal_hierarchical' reading is linked to 'contextual_egalitarian' and 'progressive_abrogation' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
