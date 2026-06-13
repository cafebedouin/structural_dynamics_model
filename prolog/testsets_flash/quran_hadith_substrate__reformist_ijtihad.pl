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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Contextual Interpretation of Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the practice of reformist ijtihad within
 *   Islamic jurisprudence, where classical rulings are re-evaluated and
 *   potentially superseded when they conflict with contemporary ethics, human
 *   rights, or public interest (maslaha), with a prioritization of the
 *   Quran's ethical trajectory over literalist hadith application. It is one
 *   reading of the broader 'quran_hadith_substrate' kernel, which governs the
 *   foundational sources of Islamic law. This reading aims to make Islamic
 *   law more relevant and just in modern contexts, but it faces significant
 *   resistance from traditional authority structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.3).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Interpretation of Islamic Law").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '11f0bdf0-6216-4520-97a4-ec81d62af5c9').
narrative_ontology:cs_kernel_codification('11f0bdf0-6216-4520-97a4-ec81d62af5c9', fixed_text).
narrative_ontology:cs_authority_grounding('11f0bdf0-6216-4520-97a4-ec81d62af5c9', expertise).
narrative_ontology:cs_interpretation_layer_present('11f0bdf0-6216-4520-97a4-ec81d62af5c9').
narrative_ontology:cs_reading_relation('11f0bdf0-6216-4520-97a4-ec81d62af5c9', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('11f0bdf0-6216-4520-97a4-ec81d62af5c9', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('11f0bdf0-6216-4520-97a4-ec81d62af5c9', foundational, quranic_ethics_supersede_literalist_hadith).
narrative_ontology:cs_axiom_status(quranic_ethics_supersede_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('11f0bdf0-6216-4520-97a4-ec81d62af5c9', quranic_ethics_supersede_literalist_hadith, deontological).
narrative_ontology:cs_axiom('11f0bdf0-6216-4520-97a4-ec81d62af5c9', foundational, maslaha_and_human_rights_are_valid_ijtihad_sources).
narrative_ontology:cs_axiom_status(maslaha_and_human_rights_are_valid_ijtihad_sources, holdable).
narrative_ontology:cs_axiom_grounding('11f0bdf0-6216-4520-97a4-ec81d62af5c9', maslaha_and_human_rights_are_valid_ijtihad_sources, conventional).
narrative_ontology:cs_reference_frame('11f0bdf0-6216-4520-97a4-ec81d62af5c9', quranic_ethical_trajectory).
narrative_ontology:cs_drift_state('11f0bdf0-6216-4520-97a4-ec81d62af5c9', contemporary_global_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('11f0bdf0-6216-4520-97a4-ec81d62af5c9', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_and_lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_muslim_majority_states).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and apply contextual ijtihad, prioritizing the Quran's ethical trajectory and contemporary values. They face resistance from traditionalists but gain traction in academic and progressive Muslim communities. Their authority is epistemic and moral, not institutional in many contexts.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    organized, generational, constrained, global).

% Find their ethical and social concerns validated by this interpretive approach, leading to a more inclusive and relevant understanding of Islam. They benefit from the intellectual space created for modern ethical considerations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, mobile, global).

% Experience liberation from classical rulings that often marginalize or oppress them. This reading offers a path to religious belonging without compromising their identity or rights, but they remain vulnerable to traditionalist backlash.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_and_lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, global).

% Benefit from a more inclusive and rights-oriented interpretation of Islamic law, potentially leading to greater protections and reduced discrimination in legal and social spheres. Their situation is highly dependent on the state's adoption of such interpretations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_muslim_majority_states, beneficiary,
    powerless, generational, constrained, national).

% See their interpretive monopoly and established authority challenged by this approach. They bear the cost of losing influence and legitimacy as alternative readings gain ground, often responding with counter-mobilization and accusations of heresy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulama, payer,
    institutional, generational, trapped, global).

% Their power structures and funding often depend on maintaining classical interpretations. They bear the cost of having their foundational premises questioned and their social control eroded, leading to active resistance against reformist efforts.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_institutions, payer,
    institutional, generational, trapped, global).

% Monitor the application of Islamic law and advocate for interpretations consistent with international human rights norms. They provide external pressure and validation for reformist efforts, but do not directly participate in the internal interpretive process.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for reconciling Islamic legal tradition with contemporary ethical challenges and human rights, allowing Muslims to navigate modernity while remaining faithful to their religious heritage.
% TRANSFER_FUNCTION: Transfers interpretive authority from rigid adherence to classical rulings towards a dynamic engagement with primary sources (Quran) and contemporary ethical considerations. It shifts the burden of proof for ethical relevance onto traditional rulings.
% ABSENT_VOICES: Extremist groups who reject any form of ijtihad that deviates from their literalist interpretations are excluded. They would condemn this approach as apostasy and actively work to suppress it, but are not part of the scholarly discourse this constraint describes.
% DISAPPEARANCE_RATIONALE: If reformist ijtihad vanished, the intellectual and ethical space for progressive Muslims would collapse, leading to increased internal conflict, alienation from religious institutions, and a resurgence of unchallenged traditionalist authority. The global discourse on Islam and modernity would be fundamentally altered.
% FOUNDING_PROBLEM: The growing disconnect between classical Islamic legal rulings and contemporary ethical standards, human rights, and public interest (maslaha), leading to a crisis of relevance and legitimacy for many Muslims in the modern world.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, surveys of Muslim youth, and statements from international human rights organizations corroborate the ongoing tension between traditional interpretations and modern values. Progressive Muslim organizations and interfaith dialogues also attest to the live nature of this problem, from outside the immediate beneficiary set of reformist scholars.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the integration of Islamic tradition with modern values (beneficiaries) but also extracts from traditional authority structures (victims) whose interpretive monopoly is challenged. It requires active enforcement by reformist scholars and institutions to gain acceptance against conservative pushback. Extractiveness is moderate (0.45) as it challenges established norms, but not fully coercive. Suppression is relatively low (0.30) because this reading actively seeks to lower the suppression of alternative interpretations, though it still faces suppression from traditionalists. The decreasing suppression_requirement over time reflects the growing intellectual space for reformist thought, while extractiveness has slightly increased as the challenge to traditional authority becomes more pronounced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist scholars and beneficiaries, this is a necessary and just coordination mechanism for Islamic law in the modern era. From the perspective of traditional ulama and conservative institutions, it is an illegitimate deviation that undermines the integrity of the tradition and their authority, thus experiencing it as extractive. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars act as agenda-setters, actively shaping and promoting this interpretive framework. Progressive Muslims, women, LGBTQ+ Muslims, and religious minorities are beneficiaries, as this reading offers them a more inclusive and just religious experience. Traditional ulama and conservative institutions are victims, as their authority and established interpretations are directly challenged and eroded by this approach. Human rights advocates serve as external observers, validating the ethical trajectory of this ijtihad.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_adoption_rate,
    'To what extent is reformist ijtihad being adopted and institutionalized by official religious bodies and state legal systems in Muslim-majority countries?',
    'Tracking changes in fatwa councils'' methodologies, curriculum reforms in Islamic universities, and amendments to family or criminal laws in states that claim Islamic legal grounding.',
    'Higher institutional adoption would increase the effective power and scope of this constraint, potentially shifting its classification towards a more established ''Rope'' or ''Tangled Rope'' with broader impact, and further eroding the ''traditionalist_taqlid'' reading''s influence. Low adoption would keep it as a ''Rope'' or ''Scaffold'' primarily within academic and activist circles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_adoption_rate, empirical, 'Measures the real-world impact and reach of reformist interpretations beyond academic discourse.').

omega_variable(
    legitimacy_of_ethical_sources,
    'Is the grounding of ''contemporary ethics'' and ''human rights norms'' as valid sources for ijtihad universally accepted within the reformist framework, or is there internal contestation?',
    'Content analysis of reformist scholarly debates, identifying explicit methodological statements on the epistemic status of secular ethical frameworks versus internal Islamic ethical reasoning.',
    'If internal contestation is significant, the coherence and stability of this reading as a ''Rope'' or ''Tangled Rope'' would be weakened, as its foundational premises are not fully settled even among its proponents. If universally accepted, its internal consistency is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_ethical_sources, conceptual, 'Examines the internal coherence of the reformist methodology regarding external ethical sources.').

omega_variable(
    traditionalist_counter_mobilization_effectiveness,
    'How effective is traditionalist counter-mobilization in suppressing the spread and acceptance of reformist ijtihad, particularly in contexts where traditional institutions hold state power?',
    'Analyzing legal challenges against reformist scholars, censorship of reformist publications, and public campaigns by traditionalist bodies to discredit reformist interpretations. Measuring the impact on reformist scholars'' ability to publish, teach, and influence public opinion.',
    'High effectiveness of counter-mobilization would increase the ''suppression'' metric for this constraint and could push its classification towards a ''Snare'' for reformist scholars, or a ''Piton'' if the reformist efforts become purely performative under pressure. Low effectiveness would allow the ''Tangled Rope'' to function more openly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(traditionalist_counter_mobilization_effectiveness, empirical, 'Assesses the real-world power dynamics between reformist and traditionalist forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1980, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(qura_tr_t1990, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(qura_tr_t2010, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(qura_tr_t2020, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(qura_be_t1990, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(qura_be_t2010, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(qura_be_t2020, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(qura_su_t1990, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(qura_su_t2010, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(qura_su_t2020, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel. It directly challenges the 'traditionalist_taqlid' reading and influences the 'state_hybrid' reading by providing an alternative interpretive framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
