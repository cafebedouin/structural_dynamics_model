% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Classical Fiqh Schools as Binding Authority via Taqlid
 *   domain: religious/legal_authority
 *
 * SUMMARY:
 *   This constraint story models the traditionalist_taqlid reading of the
 *   contested kernel quran_hadith_substrate. In this reading, classical fiqh
 *   schools (Hanafi, Maliki, Shafi'i, Hanbali) are held to represent a
 *   binding authoritative consensus (ijma), and contemporary Muslims are
 *   obligated to follow established madhhab rulings via taqlid. The
 *   arrangement coordinates legal and ritual uniformity across diverse
 *   communities but simultaneously extracts from progressive Muslims, women
 *   seeking gender equality, and religious minorities by suppressing
 *   alternative interpretive readings and institutionalizing subordinate
 *   legal statuses. The high extraction and suppression metrics reflect
 *   institutional enforcement in traditionalist-dominant contexts. The
 *   claimed type is tangled_rope â genuine coordination function plus
 *   asymmetric extraction â authored independently of the metrics.
 *
 * KEY AGENTS:
 *   - traditional_ulama: Primary agenda_setter (institutional/generational/constrained) â administers taqlid obligation, controls interpretive gatekeeping, collects authority and status.
 *   - madhhab_institutions: Primary beneficiary (institutional/generational/constrained) â preserves classical corpus, benefits from endowments and state recognition.
 *   - mosque_hierarchies: Secondary beneficiary (organized/generational/constrained) â propagates taqlid norms in communal life.
 *   - progressive_muslims: Primary payer (moderate/biographical/constrained) â bears cognitive and social costs of suppressed ijtihad.
 *   - women_seeking_equality: High-extraction payer (powerless/biographical/identity_locked) â subject to classical family-law disadvantages with exit blocked by identity fusion.
 *   - religious_minorities_dhimmi: High-extraction payer (powerless/generational/trapped) â subject to classical subordinate legal status.
 *   - reformist_scholars: Excluded analytical seat â would advocate for contextual ijtihad but are kept out of authoritative deliberation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.72).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.78).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Classical Fiqh Schools as Binding Authority via Taqlid").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '459717ee-af88-46a8-84fc-69fbb6104b96').
narrative_ontology:cs_kernel_codification('459717ee-af88-46a8-84fc-69fbb6104b96', fixed_text).
narrative_ontology:cs_authority_grounding('459717ee-af88-46a8-84fc-69fbb6104b96', lineage).
narrative_ontology:cs_interpretation_layer_present('459717ee-af88-46a8-84fc-69fbb6104b96').
narrative_ontology:cs_reading_relation('459717ee-af88-46a8-84fc-69fbb6104b96', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('459717ee-af88-46a8-84fc-69fbb6104b96', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('459717ee-af88-46a8-84fc-69fbb6104b96', foundational, taqlid_obligation_binding).
narrative_ontology:cs_axiom_status(taqlid_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('459717ee-af88-46a8-84fc-69fbb6104b96', taqlid_obligation_binding, theological).
narrative_ontology:cs_axiom('459717ee-af88-46a8-84fc-69fbb6104b96', foundational, classical_school_consensus_presumptive).
narrative_ontology:cs_axiom_status(classical_school_consensus_presumptive, holdable).
narrative_ontology:cs_axiom_grounding('459717ee-af88-46a8-84fc-69fbb6104b96', classical_school_consensus_presumptive, theological).
narrative_ontology:cs_reference_frame('459717ee-af88-46a8-84fc-69fbb6104b96', classical_fiqh_consensus_authority).
narrative_ontology:cs_drift_state('459717ee-af88-46a8-84fc-69fbb6104b96', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('459717ee-af88-46a8-84fc-69fbb6104b96', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_dhimmi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit classical fiqh rulings across generations; administer the obligation of taqlid by certifying who may issue fatwas and what sources are admissible. Their social status, institutional funding, and hermeneutical monopoly depend on the exclusivity of their interpretive role. Exit from the classical framework means abandoning the authority that defines their position.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, beneficiary).

% Preserve and propagate a specific madhhab's corpus through madrasa networks, canonical textbooks, and certification systems. Control access to advanced legal training and juridical titles. Benefit from religious endowments (awqaf), state recognition, and transnational donor support tied to their authoritative status.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, constrained, global).

% Deliver sermons, educational programs, and pastoral care aligned with classical rulings and taqlid norms. Reinforce doctrinal conformity in congregational life. Receive state or community support contingent on loyalty to established madhhab positions and on not platforming reformist voices.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, generational, constrained, national).

% Seek ethical and legal frameworks compatible with contemporary human rights, pluralism, and scientific knowledge. Face social ostracism, accusations of apostasy, and exclusion from communal leadership if they publicly reject taqlid or advocate ijtihad. Bear the cognitive cost of navigating between sincere faith and classical rulings they experience as unjust.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Subject to classical family-law rulings on marriage, divorce, child custody, and inheritance that afford them lesser rights than men. Advocacy for reform is delegitimized as Western cultural imperialism or apostasy. Exit is blocked by deep identity-fusion with Muslim family and community structures; leaving the framework is experienced as leaving the community itself.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality, payer,
    powerless, biographical, identity_locked, global).

% Live under classical dhimmi frameworks that institutionalize subordinate legal and social status, including restrictions on public religious expression, political office, and military service. In some jurisdictions, personal-status law is governed by fiqh-derived rules that cap their civic equality. Viable exit typically requires migration or conversion, both carrying extreme costs.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_dhimmi, payer,
    powerless, generational, trapped, regional).

% Would advocate for contextual ijtihad, quranic ethical trajectory, and gender-equal readings but are structurally excluded from authoritative fiqh deliberation, madrasa tenure, and state fatwa councils. Their exclusion is enforced by hermeneutical closure around classical texts and by social sanctions against deviance.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interpretive unity across diverse Muslim communities by establishing a stable, transmitted corpus of legal rulings (madhhab jurisprudence); prevents arbitrary individual interpretation from fragmenting communal ritual and legal practice by delegating authority to certified scholars.
% TRANSFER_FUNCTION: Moves authority to interpret and apply divine law from individual Muslims, women, and contemporary ethicists to certified madhhab scholars and their institutional heirs; simultaneously transfers subordinate legal and social status from women and religious minorities to the classical framework's beneficiaries.
% ABSENT_VOICES: Reformist scholars advocating contextual ijtihad, feminist Muslim jurists challenging patriarchal classical rulings, and religious minorities rejecting dhimmi subordination are structurally excluded from authoritative fiqh deliberation; their absence is enforced by hermeneutical closure and social sanctions.
% DISAPPEARANCE_RATIONALE: If classical fiqh schools lost their binding authoritative status and taqlid obligations vanished, family law, inheritance, and criminal adjudication in traditionalist communities would reorganize around state courts, reformist ijtihad, or secular frameworks; the institutional economy of madrasas, fatwa bodies, and mosque hierarchies would collapse or transform fundamentally.
% FOUNDING_PROBLEM: Post-prophetic Muslim communities faced widespread disagreements on legal and ritual matters; the classical schools consolidated interpretive methodologies and transmitted rulings to preserve unity and reduce error in applying revelation to new contexts.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Islamic law corroborate the historical consolidation but dispute its contemporary necessity. Reformist Muslim scholars and human rights organizations attest that textual literacy, democratic deliberation, and global human rights norms have superseded the need for madhhab gatekeeping; these sources sit outside the beneficiary set. Traditional ulama assert the problem is perennially live, which is self-serving.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint moves substantial interpretive and legal authority from lay Muslims, women, and minorities to classical scholars and their institutions, backed by social and sometimes state sanctions. Suppression (0.78) is high because the constraint's persistence depends on actively excluding reformist ijtihad, feminist jurisprudence, and secular legal alternatives; alternatives are structurally barred from authoritative status. Theater_ratio (0.55) is moderate-to-high because a growing share of madhhab institutional activity defends the authority structure itself (performative fatwas defending taqlid, heresy accusations) rather than solving new legal problems. Accessibility_collapse (0.70) is high because within the traditionalist epistemic framework, the classical schools appear as the only legitimate interpreters, and alternatives collapse into 'Western corruption' or 'apostasy.' Resistance (0.55) is moderate because reformist movements exist but are fragmented and penalized. Measurements are aligned on a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   The traditional_ulama seat experiences the constraint as necessary coordination preventing interpretive anarchy and preserving divine law; the progressive_muslims, women_seeking_equality, and religious_minorities_dhimmi seats experience it as enforced extraction that blocks their ethical and legal aspirations. The engine computes this divergence from the structural asymmetry in power, exit, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional_ulama, madhhab_institutions, mosque_hierarchies) derive low directionality from their structural position: the constraint subsidizes their authority, income, and social role. Victims (progressive_muslims, women_seeking_equality, religious_minorities_dhimmi) derive high directionality: they pay the costs of suppressed agency and subordinate status. Women are identity_locked, which amplifies effective extraction beyond what a purely structural measure would capture. The excluded reformist seat has no directionality because it is outside the constraint's operation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â legal unity after prophetic revelation â was genuine. However, the R5 genealogy flags a potential mandatrophy: the classical schools consolidated around the 10th century CE, and the contemporary globalized context presents structurally different coordination problems (mass literacy, codified state law, human rights discourse). The traditionalist reading treats the solution as permanent, not transitional. Because the constraint actively suppresses alternatives rather than merely persisting by inertia, it is classified as tangled_rope rather than piton; the theater ratio, while rising, does not yet indicate pure performance. If enforcement decayed and the constraint persisted only as cultural habit without institutional gatekeeping, it would drift toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taqlid_authority_source,
    'Does the obligation of taqlid derive from a binding divine commandment within the Qurano-hadithic substrate, or is it a historically constructed institutional mechanism for preserving scholarly authority?',
    'Historical-critical analysis of the emergence of taqlid obligations in post-formative-period legal theory; comparison with early Islamic practice where direct ijtihad was normative.',
    'If taqlid is constructed, the constraint''s extraction is institutional rather than theological, supporting reformist or state-hybrid reclassification; if divine, the high extractiveness is reclassified as necessary spiritual discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_authority_source, conceptual, 'Whether taqlid obligation is divine or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings achieved through structural enforcement (state-backed blasphemy laws, institutional excommunication, employment bans) or internalized self-policing (fear of divine displeasure, communal shame, family rupture)?',
    'Cross-jurisdictional comparison of reformist voice and women''s legal advocacy in contexts with versus without state-enforced apostasy penalties; post-exit trajectory of individuals who leave traditionalist communities.',
    'If suppression is primarily internalized, effective extraction exceeds structural measures because targets carry the constraint with them after any formal exit; if structural, legal reform could rapidly reclassify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_location,
    'This constraint is the traditionalist_taqlid reading of kernel quran_hadith_substrate; sibling readings are reformist_ijtihad and state_hybrid. The disagreement is located at the locus of legitimate interpretive authority. What would a sibling reading change structurally?',
    'Side-by-side comparison of axioms and beneficiary/victim structures across the three readings; identify which premises are contradictory and which are merely competitive.',
    'Adopting the reformist reading would empty the victim set of women and minorities and sharply lower extraction. Adopting the state-hybrid reading would shift the agenda_setter from ulama to state apparatus and invert the directionality of state actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Sibling reading structural deltas for kernel quran_hadith_substrate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 10, 0.35).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.4).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 30, 0.45).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.5).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel quran_hadith_substrate, decomposed from the colloquial label 'Islamic law' which conflates structurally distinct commitments: traditionalist taqlid (binding classical authority), reformist ijtihad (contextual reinterpretation), and state hybrid (political sovereignty over selective classical adoption). Each reading has a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
