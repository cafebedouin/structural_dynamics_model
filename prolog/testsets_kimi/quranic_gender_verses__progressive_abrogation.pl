% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive Abrogation Reading of Qur'anic Gender Verses
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This constraint instantiates the progressive_abrogation reading of the
 *   quranic_gender_verses kernel: the claim that later egalitarian Qur'anic
 *   verses (especially 49:13 on universal human dignity) supersede earlier
 *   gender-differentiated rulings (4:11 inheritance, 2:282 testimony, 4:34
 *   qiwamah) via the classical jurisprudential principle of naskh
 *   (abrogation). The reading operates as a legal-hermeneutic mechanism that
 *   comprehensively delegitimizes traditional authority structures and
 *   reassigns legal parity to women. It is structurally extractive toward
 *   literalist communities and institutional ulema, while coordinating an
 *   egalitarian legal subjecthood for women. As a contested kernel reading,
 *   it is Îµ-invariant: its metrics describe the constraint this specific
 *   reading produces, not an average over all readings of the kernel.
 *
 * KEY AGENTS:
 *   - women (beneficiary, organized/constrained): gain full legal parity under the abrogation reading but remain socially constrained in traditional environments
 *   - traditional_ulema (payer, institutional/identity_locked): lose canonical authority to enforce gender hierarchy; their interpretive methodology is delegitimized
 *   - literalist_communities (payer, organized/identity_locked): experience epistemic violence as foundational gender norms are hermeneutically nullified
 *   - progressive_scholars (agenda_setter/payer, moderate/identity_locked): advance the reading within hostile institutions at high personal cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.87).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.82).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.87).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation Reading of Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'f01e8ca9-0fbd-4289-b37f-cacf0a86edaa').
narrative_ontology:cs_kernel_codification('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', fixed_text).
narrative_ontology:cs_authority_grounding('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', lineage).
narrative_ontology:cs_interpretation_layer_present('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa').
narrative_ontology:cs_reading_relation('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', foundational, universal_dignity_paramount).
narrative_ontology:cs_axiom_status(universal_dignity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', universal_dignity_paramount, deontological).
narrative_ontology:cs_axiom('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', foundational, naskh_applies_to_gender_verses).
narrative_ontology:cs_axiom_status(naskh_applies_to_gender_verses, holdable).
narrative_ontology:cs_axiom_grounding('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', naskh_applies_to_gender_verses, conventional).
narrative_ontology:cs_reference_frame('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', abrogative_trajectory_universal_dignity).
narrative_ontology:cs_drift_state('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', contemporary_literalist_hegemony, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f01e8ca9-0fbd-4289-b37f-cacf0a86edaa', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_ulema).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, progressive_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain full legal parity in inheritance, testimony, and guardianship under the abrogation reading. Previously constrained by gender-specific rules now deemed abrogated. Their structural position shifts from subordinate to equal legal subject, but social enforcement of traditional norms remains a barrier in non-reformist environments.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women, beneficiary,
    organized, generational, constrained, global).

% Lose canonical authority to adjudicate gender hierarchy. Their interpretive methodology and institutional legitimacy are delegitimized by the claim that later verses abrogate earlier gender rulings. Exit is unthinkable because their authority derives from continuous transmission of the literal text and classical fiqh synthesis.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_ulema, payer,
    institutional, civilizational, identity_locked, global).

% Communities whose religious and social identity is fused with literal-hierarchical gender norms. Experience epistemic violence as foundational texts are reinterpreted to negate their family structure and gendered social roles. Exit fractures communal belonging and self-concept.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_communities, payer,
    organized, generational, identity_locked, global).

% Advance the abrogation reading within traditional institutions at high personal and professional cost. They set the hermeneutic agenda by publishing, teaching, and lobbying for reformist codification, but face ostracism, revoked credentials, and loss of scholarly community. Their exit options are constrained by career path dependence and identity fusion with the ulama class.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_scholars, agenda_setter,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, progressive_scholars, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, egalitarian legal framework for gender relations across Muslim jurisdictions by resolving Qur'anic textual multiplicity through the classical principle of naskh (abrogation), privileging later universal-dignity verses over earlier context-specific rulings.
% TRANSFER_FUNCTION: Moves legal authority, inheritance shares, testimony weight, and guardianship rights from traditional ulema and patriarchal family structures to women as autonomous legal subjects; transfers the burden of interpretive legitimacy from uninterrupted literal transmission to reformist scholarly institutions.
% ABSENT_VOICES: Traditional women who experience the literal-hierarchical framework as protective or identity-constitutive, and quietist or mystical scholars who reject both literalist and progressive legalistic framings in favor of spiritual non-juridical readings, are structurally excluded. The debate is framed as a binary between literal hierarchy and progressive equality, silencing alternative hermeneutical exit routes.
% DISAPPEARANCE_RATIONALE: If the progressive abrogation reading vanished overnight, reformist family codes in Tunisia, Morocco, and similar jurisdictions would lose their primary hermeneutical justification; traditional ulema would regain unchallenged authority over gender jurisprudence; women's legal parity in inheritance, testimony, and guardianship would revert to classical fiqh frameworks; and the scholarly field would lose its most structurally radical egalitarian tool.
% FOUNDING_PROBLEM: The apparent contradiction between Qur'anic verses asserting universal human dignity (49:13) and verses prescribing differentiated gender rights (4:11, 2:282, 4:34), which classical jurisprudence resolved in favor of hierarchy by treating the universal verses as general and the specific verses as operative exceptions.
% FOUNDING_PROBLEM_CORROBORATION: Progressive scholars (Amina Wadud, Kecia Ali, Fazlur Rahman) attest the problem is the unresolved tension between equality and patriarchal classical synthesis. Traditional ulema (Al-Azhar, Deoband) attest there is no contradiction because the specific verses are the final operative rulings. Independent historians of Islamic law (Wael Hallaq, Khaled Abou El Fadl) corroborate that the tension was recognized but resolved hierarchically in pre-modern jurisprudence, supporting the 'dead problem revived by modernity' reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.87, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.87) because the reading effects a complete normative reversal: it transfers legal authority, inheritance shares, testimony weight, and guardianship rights from men and ulema to women. Suppression is high (0.82) because the reading cannot prevail without actively suppressing the literal-hierarchical reading that dominates institutional Islamic law. Theater ratio is moderate (0.45) because while genuine legal reform occurs in some jurisdictions, much scholarly advocacy remains performative textual argument without state enforcement. Accessibility collapse is high (0.78) because once the abrogation frame is accepted, literal alternatives collapse in the legal domain. Resistance is high (0.80) due to entrenched traditional institutions. The measurement series runs on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The women/beneficiary seat and the traditional_ulema/payer seat experience diametrically opposed computed types: from the beneficiary seat the constraint is emancipatory coordination (rope-like), while from the payer seat it is comprehensive delegitimization and extraction (snare-like). The engine captures this divergence through directionality: the same structural arrangement yields negative effective extraction (subsidy) for women and extreme positive effective extraction for traditional authorities. Progressive scholars sit ambiguously: they are agenda-setters (low directionality for the constraint's operation) but also payers (high directionality for personal costs), warranting the dual role.
 *
 * DIRECTIONALITY LOGIC:
 *   Women are structural beneficiaries (directionality near the beneficiary end): the constraint subsidizes their legal capacity and removes subordination. Traditional ulema and literalist communities are structural targets (directionality near the target end): the constraint extracts their authority, social structure, and identity stability. Progressive scholars sit in the middle: they administer the reading's propagation but bear severe institutional costs, producing a mid-range effective extraction that reflects their dual structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the emancipatory outcome as pure extraction (snare) and mislabeling the delegitimization of traditional communities as pure coordination (rope). The genuine coordination functionâestablishing uniform legal parityâis inseparable from the asymmetric extraction from literalist identity structures. Mandatrophy is contested: traditionalists argue the founding problem (gender hierarchy as divine decree) was never live and the arrangement is pure hermeneutical violence; progressives argue the founding problem (unresolved textual contradiction) was suppressed by classical synthesis and is now revived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_instrumentalization,
    'Is the application of naskh to gender-specific verses a genuine retrieval of classical hermeneutical principle, or an instrumental construction using abrogation rhetoric to achieve modern egalitarian outcomes?',
    'Historical jurisprudential archaeology comparing classical naskh application domains to contemporary progressive extensions; sociological analysis of scholarly motivation and institutional pressure.',
    'If instrumental, the constraint''s authority_grounding shifts from lineage to extraction and its computed type may trend toward snare; if genuine retrieval, the lineage grounding holds and the coordination function carries more weight in the tangled-rope balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_instrumentalization, conceptual, 'Whether naskh on gender verses is classical retrieval or modern construction.').

omega_variable(
    epistemic_violence_ambiguity,
    'Is the delegitimization of literalist communities experienced as coercive epistemic violence or emancipatory normative reform?',
    'Ethnographic study of affected communities measuring identity fracture, voluntary adoption rates, and psychological markers of coercion versus emancipation; comparative analysis across reformist and conservative jurisdictions.',
    'If experienced as predominantly epistemic violence, effective suppression is higher than the structural measure suggests and the constraint trends toward snare; if experienced as liberation, the victim framing may overstate extraction for willing adopters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_ambiguity, empirical, 'Structural ambiguity between liberation and violence in delegitimization.').

omega_variable(
    state_enforcement_trajectory,
    'Will the progressive abrogation reading remain scholarly discourse, or will it be adopted by state legal systems as enforceable positive law?',
    'Comparative legal analysis of reformist Muslim-majority jurisdictions tracking codification of egalitarian family law and state confrontation with traditional institutions.',
    'State adoption dramatically raises suppression, extractiveness, and spatial scope; scholarly discourse alone remains lower on all three and may function more like scaffold than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_trajectory, empirical, 'Whether state enforcement replaces scholarly advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgvp_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.6).
narrative_ontology:measurement(qgvp_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(qgvp_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.5).
narrative_ontology:measurement(qgvp_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.45).
narrative_ontology:measurement(qgvp_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qgvp_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qgvp_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qgvp_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(qgvp_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(qgvp_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(qgvp_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(qgvp_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(qgvp_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qgvp_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(qgvp_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qgvp_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(qgvp_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(qgvp_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quranic_gender_verses kernel, decomposed per the Îµ-invariance principle because the literal, contextual, and progressive readings instantiate structurally distinct constraints with different Îµ values, beneficiary structures, and authority groundings. Each reading carries its own stable Îµ and classification; they are linked as a constraint family rather than collapsed into a single ambiguous label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
