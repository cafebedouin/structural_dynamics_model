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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Mandate in Islamic Jurisprudence
 *   domain: Islamic Jurisprudence / Legal Theory / Religious Authority
 *
 * SUMMARY:
 *   This constraint describes the mandate for reformist ijtihad within
 *   Islamic jurisprudence, which requires contextual interpretation of
 *   classical rulings when they conflict with contemporary ethics, human
 *   rights, or public interest (maslaha), prioritizing the Quran's ethical
 *   trajectory over literalist hadith application. It is a reading of the
 *   broader 'quran_hadith_substrate' kernel, actively promoted by reformist
 *   scholars and movements to modernize Islamic law and make it more relevant
 *   and just in the 21st century. The constraint functions as a Tangled Rope,
 *   coordinating progressive interpretations while extracting legitimacy and
 *   power from traditional authority structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.45).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.55).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Mandate in Islamic Jurisprudence").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "Islamic Jurisprudence / Legal Theory / Religious Authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, 'bd0959eb-b4cd-44b5-9be5-48106c215ad8').
narrative_ontology:cs_kernel_codification('bd0959eb-b4cd-44b5-9be5-48106c215ad8', formalized).
narrative_ontology:cs_authority_grounding('bd0959eb-b4cd-44b5-9be5-48106c215ad8', expertise).
narrative_ontology:cs_interpretation_layer_present('bd0959eb-b4cd-44b5-9be5-48106c215ad8').
narrative_ontology:cs_reading_relation('bd0959eb-b4cd-44b5-9be5-48106c215ad8', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('bd0959eb-b4cd-44b5-9be5-48106c215ad8', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('bd0959eb-b4cd-44b5-9be5-48106c215ad8', foundational, quranic_ethical_trajectory_supremacy).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bd0959eb-b4cd-44b5-9be5-48106c215ad8', quranic_ethical_trajectory_supremacy, deontological).
narrative_ontology:cs_axiom('bd0959eb-b4cd-44b5-9be5-48106c215ad8', foundational, maslaha_over_literalism).
narrative_ontology:cs_axiom_status(maslaha_over_literalism, holdable).
narrative_ontology:cs_axiom_grounding('bd0959eb-b4cd-44b5-9be5-48106c215ad8', maslaha_over_literalism, conventional).
narrative_ontology:cs_reference_frame('bd0959eb-b4cd-44b5-9be5-48106c215ad8', ethical_quranic_primacy).
narrative_ontology:cs_drift_state('bd0959eb-b4cd-44b5-9be5-48106c215ad8', contemporary_islamic_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('bd0959eb-b4cd-44b5-9be5-48106c215ad8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and practice contextual ijtihad, prioritizing the Quran's ethical trajectory. They seek to re-legitimize Islamic law in modern contexts and challenge traditional interpretive monopolies. They gain influence and intellectual authority from this mandate.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    organized, biographical, mobile, global).

% Benefit from interpretations that align Islamic teachings with contemporary ethics and human rights, finding greater relevance and ethical coherence in their faith. They support reformist scholars and movements.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Are primary beneficiaries of interpretations that challenge patriarchal or discriminatory classical rulings, leading to greater rights and inclusion within Muslim communities. Their identity is often deeply tied to their faith, making exit difficult.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women_lgbtq_minorities, beneficiary,
    powerless, biographical, identity_locked, global).

% Bear the cost of diminished authority and legitimacy as their literalist interpretations are challenged and potentially superseded. They resist the reformist mandate, viewing it as an erosion of established tradition and religious authenticity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulama, payer,
    institutional, generational, constrained, global).

% Religious universities, fatwa councils, and state-backed religious bodies that derive their power from upholding classical fiqh. They face pressure to adapt or risk losing relevance, but their institutional inertia makes change difficult and costly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_institutions, payer,
    institutional, generational, constrained, global).

% Monitor the discourse and impact of reformist ijtihad, often supporting its aims from a secular perspective. They provide external pressure and validation for interpretations that align with universal human rights.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, secular_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, reformist_scholars).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate Islamic legal interpretation by mandating contextual ijtihad, ensuring rulings are consistent with contemporary ethics, human rights, and public interest (maslaha), and prioritizing the Quran's ethical trajectory.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from traditional, literalist readings of classical fiqh and hadith to contextual, ethical-trajectory-based readings. This shifts influence from traditional religious authorities and institutions to reformist scholars and progressive Muslim communities.
% ABSENT_VOICES: Literalists, Salafists, and those who believe classical rulings are immutable and universally applicable. They are often marginalized or excluded from the reformist discourse, and would argue that this mandate compromises religious authenticity and divine law.
% DISAPPEARANCE_RATIONALE: If this mandate for reformist ijtihad vanished, the default interpretive framework would revert to traditionalist approaches. This would lead to continued and intensified conflict between religious rulings and modern values, further alienating many Muslims and hindering efforts to reconcile Islam with universal human rights. The progressive voices would lose their primary interpretive tool and platform.
% FOUNDING_PROBLEM: The growing irrelevance, ethical conflicts, and social injustices arising from strict, literalist adherence to classical fiqh rulings in contemporary contexts, leading to the alienation of many Muslims and a perceived incompatibility of Islam with universal human rights and modern ethical standards.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Muslim intellectuals, human rights organizations, and some international legal bodies attest to the ongoing conflict and the urgent need for reform. Academic studies in Islamic law and sociology of religion also corroborate the challenges faced by Muslims in reconciling faith with modernity.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) because while it benefits many, it also imposes a cost on traditionalists by de-legitimizing their interpretive monopoly. Suppression (0.55) is present as traditionalist counter-mobilization attempts to suppress reformist voices, but it's not absolute due to the growing support for reform. Resistance (0.7) is high, reflecting the ongoing struggle against deeply entrenched traditional views. Theater ratio is low (0.15) because the mandate is an active, functional challenge to existing norms, not a performative maintenance of an atrophied function. The metrics show a slight increase in extractiveness and suppression over time, reflecting the intensifying contestation as reformist ideas gain traction and traditionalists push back harder.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist scholars and beneficiaries, this mandate is a necessary and just coordination mechanism for the evolution of Islamic law. From the perspective of traditional ulama and conservative institutions, it is an illegitimate extraction of their inherited authority and a dangerous deviation from established religious principles. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars are the agenda-setters, actively shaping and enforcing this interpretive mandate. Progressive Muslims, women, LGBTQ+ individuals, and religious minorities are the primary beneficiaries, gaining greater inclusion and ethical coherence. Traditional ulama and conservative institutions are the victims, as their authority and interpretive monopoly are challenged and eroded. Secular human rights advocates act as observers, often aligning with the reformist aims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''quran_hadith_substrate'' kernel, or merely a variant within a broader interpretive tradition?',
    'Analysis of core axiomatic differences and their logical implications for other readings. If the core premises are mutually exclusive within a single framework, it''s a distinct reading.',
    'If a distinct reading, it strengthens the case for a kernel-based analysis of Islamic jurisprudence. If a variant, the ''quran_hadith_substrate'' kernel might need further decomposition or a different framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a specific reading of the kernel.').

omega_variable(
    reformist_impact_on_traditional_authority,
    'To what extent does the ''mandate'' for contextual ijtihad actually diminish the power and legitimacy of traditional authority structures in practice?',
    'Empirical studies tracking fatwa issuance, judicial rulings, public opinion shifts, and institutional funding for traditional vs. reformist bodies over time.',
    'If the impact is substantial, the extractiveness and suppression metrics are accurate or even understated. If the impact is minimal, the constraint might be more performative (higher theater_ratio) than truly transformative, suggesting a weaker Tangled Rope or even a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_impact_on_traditional_authority, empirical, 'Measures the real-world effect of reformist ijtihad on traditional power.').

omega_variable(
    universality_of_contemporary_ethics,
    'Are ''contemporary ethics'' and ''human rights norms'' universally agreed upon and stable, or are they themselves contested and subject to cultural relativism, potentially making the reformist mandate''s grounding unstable?',
    'Philosophical and sociological analysis of the cross-cultural reception and contestation of these norms. Examination of internal debates within human rights discourse.',
    'If these norms are highly contested, the reformist mandate''s foundation is weaker, potentially increasing its internal resistance and making its coordination function more fragile. This could shift its classification towards a more contested or even Snare-like dynamic if the ''ethics'' become a tool for a new form of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_of_contemporary_ethics, conceptual, 'Examines the stability and universality of the ethical foundations of reformist ijtihad.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(qura_tr_t2005, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(qura_tr_t2010, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2010, 0.13).
narrative_ontology:measurement(qura_tr_t2015, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(qura_tr_t2020, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(qura_tr_t2025, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(qura_tr_t2030, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2030, 0.09).

% Extraction over time
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(qura_be_t2005, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(qura_be_t2010, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(qura_be_t2015, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(qura_be_t2020, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(qura_be_t2025, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2025, 0.47).
narrative_ontology:measurement(qura_be_t2030, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2030, 0.49).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(qura_su_t2005, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(qura_su_t2010, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(qura_su_t2015, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement(qura_su_t2020, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(qura_su_t2025, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement(qura_su_t2030, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2030, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
