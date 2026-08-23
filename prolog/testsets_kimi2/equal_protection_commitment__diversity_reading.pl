% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection Diversity Reading (University Discretion)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint story captures the diversity reading of the Equal
 *   Protection Clause: the constitutional authorization for selective
 *   universities to use race as one factor among many in holistic admissions
 *   to obtain the educational benefits of student-body diversity. The reading
 *   treats diversity as a compelling state interest and subjects its
 *   implementation to strict scrutiny. It is claimed as a coordination
 *   mechanism for a collective educational good, but it procedurally extracts
 *   individualized transparency and claim clarity from applicants. The story
 *   is authored as a tangled_rope to reflect that hybrid structure, with
 *   low-moderate Îµ because the extraction is procedural rather than a direct
 *   resource transfer.
 *
 * KEY AGENTS:
 *   - universities: beneficiary (institutional/constrained) â gain admissions discretion under strict scrutiny
 *   - applicants: payer (powerless/trapped) â bear the cost of opaque, multi-factor evaluation
 *   - federal_judiciary: agenda_setter (institutional/analytical) â administers the strict scrutiny framework
 *   - colorblind_advocates: excluded (organized/constrained) â their absolutist reading is ruled constitutionally illegitimate under this regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.4).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Reading (University Discretion)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'cb95042f-4046-48bf-8cb1-e42d170e26f2').
narrative_ontology:cs_kernel_codification('cb95042f-4046-48bf-8cb1-e42d170e26f2', fixed_text).
narrative_ontology:cs_authority_grounding('cb95042f-4046-48bf-8cb1-e42d170e26f2', lineage).
narrative_ontology:cs_interpretation_layer_present('cb95042f-4046-48bf-8cb1-e42d170e26f2').
narrative_ontology:cs_reading_relation('cb95042f-4046-48bf-8cb1-e42d170e26f2', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('cb95042f-4046-48bf-8cb1-e42d170e26f2', equal_protection_commitment__remedial_reading, influences).
narrative_ontology:cs_axiom('cb95042f-4046-48bf-8cb1-e42d170e26f2', foundational, student_body_diversity_compelling).
narrative_ontology:cs_axiom_status(student_body_diversity_compelling, holdable).
narrative_ontology:cs_axiom_grounding('cb95042f-4046-48bf-8cb1-e42d170e26f2', student_body_diversity_compelling, empirically_contingent).
narrative_ontology:cs_axiom('cb95042f-4046-48bf-8cb1-e42d170e26f2', secondary, individualized_holistic_review_required).
narrative_ontology:cs_axiom_status(individualized_holistic_review_required, holdable).
narrative_ontology:cs_axiom_grounding('cb95042f-4046-48bf-8cb1-e42d170e26f2', individualized_holistic_review_required, conventional).
narrative_ontology:cs_reference_frame('cb95042f-4046-48bf-8cb1-e42d170e26f2', diversity_compelling_regime).
narrative_ontology:cs_drift_state('cb95042f-4046-48bf-8cb1-e42d170e26f2', contemporary_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb95042f-4046-48bf-8cb1-e42d170e26f2', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive constitutional authorization to consider race as one factor among many in holistic admissions to pursue student-body diversity. This grants mission-driven discretion but also subjects them to ongoing strict scrutiny litigation, compliance burdens, and the need to demonstrate narrow tailoring.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities, beneficiary,
    institutional, generational, constrained, national).

% Compete for admission under holistic review where race may operate as a plus factor alongside academic and personal criteria. The individualized basis for any single decision is obscured by the multi-factor balancing, making it difficult to adjudicate claims of individualized unfairness or to access a race-blind alternative at the same selective institution.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, applicants, payer,
    powerless, biographical, trapped, national).

% Interprets the Equal Protection Clause to permit race-conscious admissions when tied to the compelling interest of educational diversity and subjected to strict scrutiny. It sets the legal boundaries, accepts or rejects specific university plans, and does not collect material rents from the arrangement.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Maintain that any state use of racial classification violates the Equal Protection Clause. Under the diversity reading their position is treated as constitutionally illegitimate, so they are structurally excluded from the prevailing doctrinal framework despite sustained legal and political advocacy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the composition of selective student bodies by permitting universities to use race as one factor among many, aiming to obtain the educational benefits of diversityâcross-racial understanding, viewpoint variety, and preparation for a heterogeneous societyâwithout resorting to individualized quotas.
% TRANSFER_FUNCTION: Moves admissions discretion from a race-blind or purely individualized-merit baseline to universities, and moves the burden of procedural opacity from institutions to individual applicants, who cannot readily isolate the weight of any single factor in a holistic review.
% ABSENT_VOICES: Colorblind advocates who view any racial classification as constitutionally forbidden are structurally excluded; their reading is defined as illegitimate within this framework. Individual applicants seeking a purely test-score-based or race-blind review are also absent from the constitutional balance struck by the doctrine.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, selective universities would lose the constitutional authorization to consider race in admissions. They would rearrange toward race-neutral holistic criteria or purely meritocratic metrics, and the demographic composition of selective higher education would shift measurably.
% FOUNDING_PROBLEM: Racial segregation and the systematic exclusion of minority students from selective higher education produced homogeneous campuses and perpetuated racial stratification in professional and civic leadership.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians attest that exclusion and segregation were real historical problems. However, the framing of student-body diversity as the compelling interestârather than the dismantling of subordinationâis primarily advanced by universities and their counsel. Critics from the remedial and colorblind readings, along with some constitutional scholars outside the beneficiary set, dispute that diversity is the correct or still-live framing.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored at 0.28 because the constraint does not move money or labor directly; it reallocates decision-making authority and procedural opacity. Suppression is 0.40 because the reading structurally suppresses race-blind and individualized-merit alternatives within the constitutional framework. Theater_ratio is 0.25 because the holistic review process carries genuine educational content, though a portion of the multi-factor rhetoric serves compliance demonstration. Accessibility_collapse is 0.60 because once the diversity reading is accepted, colorblind alternatives collapse within the legal order. Resistance is 0.55 because sustained litigation, political opposition, and scholarly critique generate significant contestation. Temporal measurements trace rising enforcement pressure and modest theater accumulation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   Universities experience the constraint as a grant of discretionary authority that advances their educational mission. Applicants experience it as an opaque evaluative barrier that obscures individualized claim-making. The federal judiciary experiences it as a manageable doctrinal balance test. The engine should compute divergent seat classifications from this structural asymmetry: low directionality for the beneficiary, high directionality for the payer, and near-neutral for the agenda_setter.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are the declared beneficiaries: they collect admissions discretion and missional flexibility, placing them near the beneficiary end of directionality. Applicants are the declared payers: they bear the burden of non-transparent holistic review and the inability to isolate the weight of race, placing them near the target end. The federal judiciary is neither beneficiary nor victim; its directionality derives from analytical distance. No override is needed because the structural derivation matches these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The low-moderate Îµ and the presence of a genuine coordination function (educational diversity) prevent mislabeling this as a snare. The explicit victim set (applicants) and the requirement of active judicial enforcement prevent mislabeling it as a pure rope. The reading is therefore classified as tangled_rope: a real coordination mechanism that asymmetrically extracts procedural clarity from individuals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_reading_sibling_boundary,
    'Does the diversity reading''s core premiseâthat student-body diversity is a compelling interestâlogically foreclose the colorblind reading, or do they merely coexist in different doctrinal frameworks?',
    'Doctrinal analysis of whether a single legal framework can simultaneously treat diversity as compelling and racial classification as categorically forbidden; Supreme Court jurisprudence provides the natural experiment.',
    'If foreclosed, the kernel generates irreconcilable readings and the constraint carries higher structural tension; if coexistent, the classification should reflect live contest rather than logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_reading_sibling_boundary, conceptual, 'Boundary between diversity reading and colorblind sibling').

omega_variable(
    procedural_extraction_ambiguity,
    'Is the obscuring of individual claims in holistic review an inherent cost of diversity coordination, or does it function as extractive information asymmetry that benefits universities?',
    'Comparative transparency analysis of admissions decisions under race-blind, diversity-reading, and remedial regimes; measure whether individualized feedback and decisional clarity differ across frameworks.',
    'If inherent cost, extraction should be damped toward the coordination floor; if extractive asymmetry, the current Îµ is accurately scored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_extraction_ambiguity, conceptual, 'Whether procedural opacity is coordination cost or extraction').

omega_variable(
    founding_problem_liveness,
    'Is the lack of student-body diversity still a live problem that justifies race-conscious admissions, or has the problem been superseded by demographic change or alternative remedies?',
    'Longitudinal demographic, campus-climate, and leadership-pipeline data reviewed by scholars without direct institutional stake in admissions policy.',
    'If the problem is dead, the constraint risks reclassification toward scaffold or piton; if contested, the tangled_rope classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether diversity remains a live compelling interest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t9, equal_protection_commitment__diversity_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(equa_tr_t18, equal_protection_commitment__diversity_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(equa_tr_t27, equal_protection_commitment__diversity_reading, theater_ratio, 27, 0.25).
narrative_ontology:measurement(equa_tr_t36, equal_protection_commitment__diversity_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement(equa_tr_t45, equal_protection_commitment__diversity_reading, theater_ratio, 45, 0.25).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(equa_be_t9, equal_protection_commitment__diversity_reading, base_extractiveness, 9, 0.22).
narrative_ontology:measurement(equa_be_t18, equal_protection_commitment__diversity_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement(equa_be_t27, equal_protection_commitment__diversity_reading, base_extractiveness, 27, 0.28).
narrative_ontology:measurement(equa_be_t36, equal_protection_commitment__diversity_reading, base_extractiveness, 36, 0.3).
narrative_ontology:measurement(equa_be_t45, equal_protection_commitment__diversity_reading, base_extractiveness, 45, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(equa_su_t9, equal_protection_commitment__diversity_reading, suppression_requirement, 9, 0.35).
narrative_ontology:measurement(equa_su_t18, equal_protection_commitment__diversity_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(equa_su_t27, equal_protection_commitment__diversity_reading, suppression_requirement, 27, 0.48).
narrative_ontology:measurement(equa_su_t36, equal_protection_commitment__diversity_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement(equa_su_t45, equal_protection_commitment__diversity_reading, suppression_requirement, 45, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three structurally distinct readingsâcolorblind, diversity, and remedialâeach with its own Îµ, beneficiary/victim structure, and classification. This story captures the diversity reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
