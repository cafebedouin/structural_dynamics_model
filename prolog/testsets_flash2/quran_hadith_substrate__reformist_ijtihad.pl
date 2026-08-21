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
 *   human_readable: Reformist Ijtihad: Contextual Interpretation Prioritizing Quranic Ethics
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the 'reformist ijtihad' reading of the
 *   Quran-Hadith substrate in Islamic jurisprudence. It mandates contextual
 *   interpretation when classical rulings conflict with contemporary ethics,
 *   human rights, or public interest (maslaha), prioritizing the Quran's
 *   ethical trajectory over literalist hadith application. This approach aims
 *   to make Islamic law relevant and just in the modern world, but it faces
 *   significant resistance from traditional authority structures. The
 *   constraint is classified as a Tangled Rope because it genuinely
 *   coordinates a response to a collective action problem (reconciling faith
 *   with modernity) but also involves asymmetric extraction from traditional
 *   authorities whose interpretive monopoly is challenged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.6).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Interpretation Prioritizing Quranic Ethics").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '311d0538-c97e-41a0-88f3-e3d375a65433').
narrative_ontology:cs_kernel_codification('311d0538-c97e-41a0-88f3-e3d375a65433', fixed_text).
narrative_ontology:cs_authority_grounding('311d0538-c97e-41a0-88f3-e3d375a65433', lineage).
narrative_ontology:cs_interpretation_layer_present('311d0538-c97e-41a0-88f3-e3d375a65433').
narrative_ontology:cs_reading_relation('311d0538-c97e-41a0-88f3-e3d375a65433', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('311d0538-c97e-41a0-88f3-e3d375a65433', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('311d0538-c97e-41a0-88f3-e3d375a65433', foundational, quranic_ethics_supersede_literalism).
narrative_ontology:cs_axiom_status(quranic_ethics_supersede_literalism, holdable).
narrative_ontology:cs_axiom_grounding('311d0538-c97e-41a0-88f3-e3d375a65433', quranic_ethics_supersede_literalism, deontological).
narrative_ontology:cs_axiom('311d0538-c97e-41a0-88f3-e3d375a65433', foundational, maslaha_as_primary_legal_source).
narrative_ontology:cs_axiom_status(maslaha_as_primary_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('311d0538-c97e-41a0-88f3-e3d375a65433', maslaha_as_primary_legal_source, conventional).
narrative_ontology:cs_reference_frame('311d0538-c97e-41a0-88f3-e3d375a65433', ethical_quranic_trajectory).
narrative_ontology:cs_drift_state('311d0538-c97e-41a0-88f3-e3d375a65433', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('311d0538-c97e-41a0-88f3-e3d375a65433', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_individuals).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from interpretations that align with contemporary ethical standards and human rights, offering a path to reconcile faith with modern life. They advocate for this approach but face social and institutional resistance.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Are primary beneficiaries of reformist interpretations that challenge patriarchal or discriminatory classical rulings. Their ability to exit the religious framework is often identity-locked, making internal reform crucial for their well-being and belonging.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_individuals, beneficiary,
    powerless, generational, identity_locked, global).

% Benefit from interpretations that emphasize universal ethical principles and pluralism, potentially leading to greater acceptance and protection within Muslim-majority societies. They have limited direct influence on interpretive authority but gain from its shift.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    powerless, generational, constrained, national).

% Bear the cost of diminished authority and relevance as their classical rulings are challenged. Their legitimacy is often tied to maintaining interpretive monopoly and adherence to established schools of thought. Exit means abandoning their professional identity and institutional power.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulama, payer,
    institutional, generational, trapped, global).

% Experience a challenge to their institutional power and funding as reformist interpretations gain traction. They actively resist this approach through educational, media, and political channels, as their existence depends on upholding traditional authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_institutions, payer,
    institutional, generational, constrained, national).

% Observe and sometimes support reformist efforts within Islamic jurisprudence, seeing them as aligned with universal human rights principles. They do not directly participate in religious interpretation but influence the external pressure for reform.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Muslims to navigate the tension between inherited religious tradition and evolving modern ethical sensibilities, allowing for a coherent and ethically grounded practice of Islam in contemporary contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority from rigid adherence to classical rulings and literalist hadith application towards a dynamic engagement with the Quran's ethical trajectory and contemporary public interest (maslaha). This shifts legitimacy and influence within religious discourse.
% ABSENT_VOICES: Extremist literalist groups are entirely excluded from this discourse, as their interpretive methodology is fundamentally rejected. They would argue for strict adherence to a literalist reading of all texts, regardless of ethical implications.
% DISAPPEARANCE_RATIONALE: If this reformist approach vanished, many progressive Muslims would face a crisis of faith, potentially leading to disaffiliation or internal conflict. The space for ethical engagement with Islamic texts would shrink, reinforcing traditionalist hegemony and increasing the pressure on marginalized groups within Muslim communities.
% FOUNDING_PROBLEM: The perceived irreconcilability of certain classical Islamic rulings with contemporary ethical standards, human rights, and the public interest, leading to alienation for many Muslims and a crisis of relevance for Islamic law.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of Islamic ethics, human rights organizations, and a significant segment of the global Muslim population (especially youth and women) corroborate the ongoing nature of this problem, citing specific examples of legal and social injustices rooted in traditional interpretations. This corroboration comes from outside the immediate beneficiaries of the reformist movement.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45) reflects the cost borne by traditional authority structures in terms of diminished influence and challenged legitimacy. Suppression (0.6) is moderate because while reformist voices are often marginalized or condemned, they are not entirely silenced and continue to gain traction, especially in diaspora communities and online. Resistance (0.7) is high, indicating the active pushback from traditionalist forces. The theater ratio (0.2) is low, as the reformist movement is genuinely engaged in substantive reinterpretation rather than mere performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive Muslims and marginalized groups, this constraint is a liberating force, reducing the 'extraction' of their agency and dignity by traditional rulings. For traditional ulama and conservative institutions, it is an extractive force, undermining their established authority and the stability of their interpretive frameworks. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Muslims, women, LGBTQ+ individuals, and religious minorities are beneficiaries (low d) as the constraint empowers their agency and aligns religious practice with their ethical values. Traditional ulama and conservative institutions are victims (high d) as their interpretive monopoly and institutional power are challenged. The constraint actively enforces a shift in interpretive methodology, coordinating a new approach while extracting from those who resist this shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint addresses a live problem of reconciling faith with modernity, so mandatrophy is not resolved. Its persistence is driven by the ongoing need for ethical and relevant religious guidance, rather than inertia. The classification as a Tangled Rope prevents mislabeling it as pure extraction, acknowledging its genuine coordination function while highlighting the costs imposed on traditional structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_adoption_rate,
    'To what extent is reformist ijtihad being institutionally adopted (e.g., in fatwa councils, educational curricula, state legal systems) versus remaining a fringe or academic discourse?',
    'Empirical study of fatwa issuance, curriculum changes in religious seminaries, and legal reforms in Muslim-majority states over time.',
    'Higher institutional adoption would lower the effective suppression on reformist voices and increase the extractiveness from traditional authorities, potentially shifting the constraint towards a more established Rope or even a Mountain (if universally accepted). Lower adoption would indicate continued marginalization, keeping suppression high and extractiveness localized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_adoption_rate, empirical, 'Measures the real-world impact and acceptance of reformist interpretations beyond academic circles.').

omega_variable(
    legitimacy_of_maslaha,
    'Is ''maslaha'' (public interest) genuinely accepted as a primary source of Islamic law, or is its application still viewed as secondary and subordinate to classical textual interpretations by a majority of scholars?',
    'Content analysis of contemporary fiqh literature, surveys of leading Islamic jurists, and analysis of judicial reasoning in Islamic courts.',
    'If maslaha is widely accepted as primary, the reformist ijtihad gains stronger internal legitimacy, reducing its perceived extractiveness from traditionalists (as they would be seen as resisting a valid source). If it remains secondary, the reformist approach continues to be seen as an external imposition, increasing the perceived extraction from traditionalists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_maslaha, conceptual, 'Examines the internal jurisprudential debate over the hierarchy of legal sources, particularly the role of public interest.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of reformist voices primarily structural (e.g., state censorship, institutional exclusion) or internalized (e.g., self-censorship due to social pressure, fear of ostracism)?',
    'Post-exit suppression trajectory: if reformist scholars who leave traditional institutions continue to face significant social and professional barriers, it suggests a strong internalized component. If their influence grows rapidly outside traditional structures, structural suppression is dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — reformist agents carry the suppression with them even in less restrictive environments. This would make the path to widespread adoption more challenging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for reformist voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1980, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(qura_tr_t1990, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(qura_tr_t2010, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(qura_be_t1990, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(qura_be_t2010, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(qura_su_t1990, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(qura_su_t2010, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Quran-Hadith Substrate' kernel. It represents the reformist ijtihad approach, which prioritizes ethical trajectory and contextual interpretation. It is linked to sibling readings 'traditionalist_taqlid' and 'state_hybrid', which offer different interpretive methodologies and authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
