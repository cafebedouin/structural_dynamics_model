% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses: Progressive Abrogation Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint represents the 'progressive abrogation' reading of
 *   Qur'anic gender verses, where later egalitarian principles are understood
 *   to supersede earlier, gender-specific rules via the principle of naskh
 *   (abrogation). This reading is highly extractive, as it fundamentally
 *   challenges and delegitimizes traditional authority structures and
 *   interpretations that uphold gender hierarchy. It is a snare because its
 *   persistence depends on actively suppressing literalist readings and the
 *   institutional power they represent, with identifiable victims in
 *   traditional interpretive communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.95).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.95).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, snare).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses: Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'aa6a7ba6-f47e-4a3b-a62a-79a14248d438').
narrative_ontology:cs_kernel_codification('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', fixed_text).
narrative_ontology:cs_authority_grounding('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', lineage).
narrative_ontology:cs_interpretation_layer_present('aa6a7ba6-f47e-4a3b-a62a-79a14248d438').
narrative_ontology:cs_reading_relation('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', foundational, universal_human_dignity_supersedes_particulars).
narrative_ontology:cs_axiom_status(universal_human_dignity_supersedes_particulars, holdable).
narrative_ontology:cs_axiom_grounding('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', universal_human_dignity_supersedes_particulars, deontological).
narrative_ontology:cs_axiom('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', foundational, quranic_trajectory_towards_equality).
narrative_ontology:cs_axiom_status(quranic_trajectory_towards_equality, holdable).
narrative_ontology:cs_axiom_grounding('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', quranic_trajectory_towards_equality, conventional).
narrative_ontology:cs_reference_frame('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', quranic_egalitarian_telos).
narrative_ontology:cs_drift_state('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', contemporary_feminist_theology, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('aa6a7ba6-f47e-4a3b-a62a-79a14248d438', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_islamic_authority_structures).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_interpretive_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for this reading, reinterpreting classical hermeneutics to prioritize later, universal principles. They face significant institutional resistance and potential ostracization from traditional bodies, but gain influence among reform-minded Muslims.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, agenda_setter,
    organized, generational, constrained, global).

% Benefit from the legal and social implications of this reading, which grants them full legal and social parity. They actively support and promote this interpretive approach as a path to justice and equality within an Islamic framework.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_parity, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of delegitimization as their established interpretations and associated power structures are challenged. They actively resist this reading, viewing it as an illegitimate innovation that undermines the integrity of Islamic law and tradition.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_islamic_authority_structures, payer,
    institutional, generational, trapped, global).

% Experience epistemic violence and identity crisis as their deeply held, literal interpretations are declared superseded. Their social and religious identity is often bound to these traditional readings, making acceptance of abrogation a profound challenge.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_interpretive_communities, payer,
    moderate, generational, identity_locked, local).

% Observe and analyze this interpretive shift, often from outside the Islamic tradition. They may see it as a positive internal reform or as an insufficient compromise, but their analysis does not directly participate in the internal theological debate.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_feminist_critics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to reconcile the Qur'an's internal textual tensions regarding gender roles with modern egalitarian values, providing a coherent theological framework for progressive legal and social reforms.
% TRANSFER_FUNCTION: Transfers normative authority from earlier, gender-specific verses to later, universal principles, effectively reallocating rights and responsibilities from men to women within Islamic legal discourse.
% ABSENT_VOICES: Many traditional scholars and communities, particularly those in conservative regions, are excluded from the discourse that legitimizes this reading. They would vehemently object, arguing it distorts divine revelation and undermines established Islamic jurisprudence.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the internal theological justification for gender parity within Islam would be significantly weakened, leading to a resurgence of traditional interpretations and a setback for progressive reforms. The legal and social landscape for Muslim women would revert to more hierarchical norms.
% FOUNDING_PROBLEM: The perceived contradiction between early Qur'anic verses establishing gender-specific rules and later verses emphasizing universal human dignity, creating a hermeneutical challenge for contemporary Muslims seeking to reconcile faith with modern egalitarian ethics.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Islamic scholars and women's rights advocates within Muslim communities attest to the live status of this problem, citing ongoing legal and social inequalities. Traditional authorities, while acknowledging textual differences, dispute the 'problem' framing, asserting divine wisdom in the original distinctions.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is very high because this reading demands a complete normative reversal, effectively 'extracting' legitimacy from traditional hierarchical interpretations and 'transferring' it to egalitarian ones. Suppression (0.88) is also very high, as traditional institutions actively resist and suppress this progressive interpretation, often through social ostracization, denial of academic positions, or theological condemnation. Resistance (0.92) is high from traditionalists, reflecting the profound challenge this reading poses to their established worldview. Accessibility collapse (0.75) is substantial because, for those who accept this reading, the traditional, literal interpretations become epistemically untenable.
 *
 * PERSPECTIVAL GAP:
 *   For progressive scholars and Muslim women, this reading is a liberating force, a 'rope' or 'scaffold' for justice. For traditional authority structures and literalist communities, it is a 'snare' that undermines their identity and power. The engine's classification as a snare reflects the structural reality of its impact on those who resist its normative reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars and Muslim women are beneficiaries, as this reading empowers them and aligns with their values. Traditional authority structures and literalist communities are victims, as their established norms and power are directly challenged and delegitimized. The high suppression and resistance metrics reflect the ongoing struggle over this interpretive shift.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it is an active, contested interpretive framework. Its 'mandate' is to provide a coherent, egalitarian reading of sacred texts, a problem that remains live. The high extractiveness and suppression indicate it is far from an atrophied 'piton'; it is a live, actively enforced snare that seeks to overturn an existing order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_violence_vs_liberation,
    'Is the ''extraction'' experienced by traditional communities a necessary act of liberation for women, or an act of epistemic violence that disregards their identity and interpretive heritage?',
    'Longitudinal study of communities adopting this reading: assess self-reported identity coherence, social cohesion, and perceived justice outcomes among both beneficiaries and those who initially resisted.',
    'If primarily epistemic violence, the ''snare'' classification is reinforced, highlighting the human cost of such a radical interpretive shift. If primarily liberation, the ''beneficiary'' aspect is amplified, suggesting a ''scaffold'' for a new, more just order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_vs_liberation, conceptual, 'The ethical framing of the interpretive shift''s impact on traditional communities.').

omega_variable(
    naskh_hermeneutical_legitimacy,
    'Is the application of naskh (abrogation) in this context a legitimate hermeneutical tool within classical Islamic jurisprudence, or a novel reinterpretation driven by modern values?',
    'Historical-critical analysis of classical tafsir (exegesis) and usul al-fiqh (principles of jurisprudence) texts to determine the historical scope and application of naskh to gender-related verses.',
    'If legitimate, the ''snare'' classification is strengthened by the internal consistency of the argument, making it harder for traditionalists to dismiss. If novel, it highlights the ''constructed'' nature of the constraint, potentially weakening its authority for some adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naskh_hermeneutical_legitimacy, empirical, 'The historical and jurisprudential legitimacy of applying naskh to gender verses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(qura_be_t2000, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(qura_be_t2010, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2010, 0.92).
narrative_ontology:measurement(qura_be_t2024, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(qura_su_t2000, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(qura_su_t2010, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(qura_su_t2024, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quranic_gender_verses' kernel. This 'progressive_abrogation' reading directly challenges the 'literal_hierarchical' reading and influences the 'contextual_egalitarian' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
