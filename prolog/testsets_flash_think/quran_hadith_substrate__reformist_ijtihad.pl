% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Quranic Ethical Trajectory over Literal Hadith
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the active intellectual and social project of
 *   'reformist ijtihad' within Islamic jurisprudence. It mandates contextual
 *   interpretation when classical rulings conflict with contemporary ethics,
 *   human rights, or public interest, prioritizing the Quran's ethical
 *   trajectory over literalist hadith application. This is one reading of the
 *   'quran_hadith_substrate' kernel, actively challenging traditionalist
 *   interpretations. The constraint functions as a Tangled Rope, coordinating
 *   progressive Muslims and scholars while extracting legitimacy and
 *   authority from traditional religious establishments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.4).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Quranic Ethical Trajectory over Literal Hadith").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '5bb15a39-9f3d-4614-ae5d-48489a4e32d7').
narrative_ontology:cs_kernel_codification('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', fixed_text).
narrative_ontology:cs_authority_grounding('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', expertise).
narrative_ontology:cs_interpretation_layer_present('5bb15a39-9f3d-4614-ae5d-48489a4e32d7').
narrative_ontology:cs_reading_relation('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', foundational, quranic_ethical_primacy).
narrative_ontology:cs_axiom_status(quranic_ethical_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', quranic_ethical_primacy, deontological).
narrative_ontology:cs_axiom('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', foundational, maslaha_as_legal_source).
narrative_ontology:cs_axiom_status(maslaha_as_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', maslaha_as_legal_source, instrumental).
narrative_ontology:cs_reference_frame('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', early_islamic_ijtihad_tradition).
narrative_ontology:cs_drift_state('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5bb15a39-9f3d-4614-ae5d-48489a4e32d7', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reformist_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively develop and promote interpretive methodologies that prioritize the Quran's ethical trajectory and contemporary values. They bear the intellectual and social cost of challenging established norms but gain influence and legitimacy among progressive communities. Their identity is deeply tied to this reformist project.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    organized, generational, identity_locked, global).

% Find spiritual and ethical coherence in reformist interpretations that align with modern sensibilities and human rights. They benefit from a more inclusive and adaptable understanding of their faith, but may face social pressure or ostracization from traditionalist circles.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Are often marginalized or harmed by literalist interpretations of classical rulings. They are primary beneficiaries of reformist ijtihad, which seeks to re-evaluate these rulings through an ethical and human rights lens, offering greater inclusion and justice within their faith tradition. Their identity is often deeply intertwined with their religious belonging.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_religious_minorities, beneficiary,
    powerless, biographical, identity_locked, global).

% Represent established religious authority and often adhere to classical fiqh schools. They bear the cost of challenges to their interpretive monopoly and the erosion of their traditional legitimacy. They actively resist reformist interpretations, viewing them as deviations from established Islamic scholarship.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulama, payer,
    institutional, generational, constrained, global).

% Are often funded by states or private donors who support traditionalist interpretations. They face a challenge to their ideological and financial foundations as reformist ideas gain traction. They actively mobilize resources to counter reformist narratives and maintain the status quo.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_institutions, payer,
    institutional, generational, constrained, global).

% Monitor the evolution of Islamic jurisprudence, particularly its alignment with international human rights norms. They provide external critique and support for reformist efforts, but do not directly participate in the internal theological debate.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, secular_human_rights_advocates, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and ethically grounded interpretive framework for Muslims seeking to reconcile their faith with contemporary ethical challenges, human rights, and public interest (maslaha), fostering a shared understanding among progressive communities.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from rigid adherence to classical rulings and literalist hadith application towards a dynamic, ethically-driven engagement with primary Islamic sources. This shifts social capital and influence towards reformist scholars and their adherents, away from traditionalist institutions.
% ABSENT_VOICES: Extreme literalists and those who reject any form of ijtihad (independent reasoning) are structurally excluded from this discourse, as their premises are incompatible with the reformist project. They would argue for strict adherence to historical interpretations without contextual re-evaluation.
% DISAPPEARANCE_RATIONALE: If reformist ijtihad vanished, progressive Muslims would lose a vital framework for navigating modernity, potentially leading to increased secularization or a return to traditionalist interpretations that many find ethically untenable. The intellectual landscape of Islamic thought would become significantly less diverse and adaptable, and the struggle for human rights within Islamic contexts would lose a key internal driver.
% FOUNDING_PROBLEM: The perceived stagnation and ethical irrelevance of classical Islamic law in modern contexts, where literalist interpretations of hadith often conflict with contemporary ethics, human rights, and the public interest, leading to a crisis of faith for many Muslims.
% FOUNDING_PROBLEM_CORROBORATION: Progressive intellectuals, human rights organizations, and a growing segment of the global Muslim population attest to the ongoing relevance of this problem. Academic studies of Islamic thought and social trends also corroborate the need for interpretive renewal, from outside the immediate beneficiary groups.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the ongoing struggle for interpretive authority; while it challenges traditional power, it also provides a valuable framework for its beneficiaries. Suppression (0.40) is relatively low compared to traditionalist systems, as this reading aims to open interpretive space, but it still faces significant counter-mobilization and social pressure from traditionalists. Resistance (0.60) is high due to the active opposition from traditional ulama and institutions. Theater ratio (0.10) is low, as the movement is driven by genuine intellectual and ethical concerns, not performative maintenance of an atrophied function. Accessibility collapse (0.30) is moderate, as alternatives (traditionalist interpretations, secularism) are present but often unsatisfactory for the beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist scholars and progressive Muslims, this constraint is a necessary and beneficial framework for ethical and spiritual growth. From the perspective of traditional ulama and conservative institutions, it is a dangerous innovation that undermines established religious authority and tradition. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars, progressive Muslims, and marginalized groups (women, LGBTQ+, religious minorities) are beneficiaries, as this interpretive framework offers them greater inclusion, justice, and ethical coherence within their faith. Traditional ulama and conservative institutions are victims, as their interpretive monopoly and established authority are directly challenged and eroded by this constraint. The constraint actively enforces its interpretive mandate, requiring continuous intellectual and social effort.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_ijtihad_institutional_backing,
    'To what extent does reformist ijtihad gain institutional backing (e.g., from universities, state-sponsored religious bodies, or influential media platforms) versus remaining a grassroots intellectual movement?',
    'Empirical analysis of funding sources, curriculum changes in religious education, and media representation over time.',
    'Stronger institutional backing would increase its effective suppression of traditionalist alternatives and amplify its extractiveness from traditional authority, potentially shifting its classification towards a more entrenched Tangled Rope or even Snare if it gains coercive power. Lack of institutional backing would keep it vulnerable and its extractiveness limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_ijtihad_institutional_backing, empirical, 'Impact of institutionalization on reformist ijtihad''s power.').

omega_variable(
    quran_hadith_substrate_reading_traditionalist_taqlid_delta,
    'What would be the structural changes if the ''traditionalist_taqlid'' reading of the quran_hadith_substrate kernel were dominant?',
    'Comparative analysis of legal systems and social norms in contexts where traditionalist taqlid is institutionally enforced.',
    'The ''traditionalist_taqlid'' reading would entail significantly higher suppression of alternative interpretations, higher extractiveness from individuals seeking modern ethical frameworks, and a different set of beneficiaries (traditional ulama, conservative institutions) and victims (progressive Muslims, women, LGBTQ+ individuals).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quran_hadith_substrate_reading_traditionalist_taqlid_delta, conceptual, 'Structural delta of the traditionalist_taqlid sibling reading.').

omega_variable(
    quran_hadith_substrate_reading_state_hybrid_delta,
    'What would be the structural changes if the ''state_hybrid'' reading of the quran_hadith_substrate kernel were dominant?',
    'Comparative analysis of legal systems in states that selectively apply religious law alongside secular frameworks.',
    'The ''state_hybrid'' reading would likely result in a fragmented application of religious law, with high suppression in specific domains (e.g., family law) and lower suppression in others. Beneficiaries would include the state apparatus and potentially some citizens benefiting from secular protections, while victims would be those caught in the selectively applied religious codes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quran_hadith_substrate_reading_state_hybrid_delta, conceptual, 'Structural delta of the state_hybrid sibling reading.').

omega_variable(
    interpretive_disagreement_locus,
    'Is the core disagreement between reformist ijtihad and traditionalist taqlid primarily about the hierarchy of sources (Quran vs. Hadith), the role of human reason/ethics, or the methodology of interpretation?',
    'Detailed textual analysis of scholarly debates and fatwas from both camps, identifying explicit points of contention.',
    'If the disagreement is primarily methodological, resolution might be possible through shared scholarly criteria. If it''s about fundamental source hierarchy or the role of reason, the conflict is more foundational and less amenable to internal resolution, implying a more persistent and deeply entrenched Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_disagreement_locus, conceptual, 'Location of the core interpretive disagreement within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.11).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.1).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 30, 0.1).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.09).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, islamic_family_law_interpretations).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, human_rights_application_in_islamic_contexts).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, islamic_finance_ethics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
