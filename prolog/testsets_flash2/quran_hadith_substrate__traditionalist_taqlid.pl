% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid: Obligation to Follow Classical Fiqh Schools
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This constraint describes the traditionalist reading of Islamic
 *   jurisprudence, where classical fiqh schools (madhahib) are considered to
 *   represent authoritative consensus (ijma), and contemporary Muslims are
 *   obligated to follow their established rulings via taqlid (emulation).
 *   This reading emphasizes stability and continuity, but it faces increasing
 *   contestation from reformist movements and state actors. The constraint is
 *   framed as a tangled_rope due to its genuine coordination function
 *   (coherence of law) coupled with significant extraction from those who
 *   seek alternative interpretations or whose rights are curtailed by
 *   classical rulings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.75).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.88).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.75).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid: Obligation to Follow Classical Fiqh Schools").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal/social").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'e6113200-5f4e-4a58-aec9-dcd193c909fa').
narrative_ontology:cs_kernel_codification('e6113200-5f4e-4a58-aec9-dcd193c909fa', formalized).
narrative_ontology:cs_authority_grounding('e6113200-5f4e-4a58-aec9-dcd193c909fa', lineage).
narrative_ontology:cs_interpretation_layer_present('e6113200-5f4e-4a58-aec9-dcd193c909fa').
narrative_ontology:cs_reading_relation('e6113200-5f4e-4a58-aec9-dcd193c909fa', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('e6113200-5f4e-4a58-aec9-dcd193c909fa', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('e6113200-5f4e-4a58-aec9-dcd193c909fa', foundational, ijma_is_binding_and_infallible).
narrative_ontology:cs_axiom_status(ijma_is_binding_and_infallible, holdable).
narrative_ontology:cs_axiom_grounding('e6113200-5f4e-4a58-aec9-dcd193c909fa', ijma_is_binding_and_infallible, theological).
narrative_ontology:cs_axiom('e6113200-5f4e-4a58-aec9-dcd193c909fa', foundational, taqlid_is_obligatory_for_non_mujtahid).
narrative_ontology:cs_axiom_status(taqlid_is_obligatory_for_non_mujtahid, holdable).
narrative_ontology:cs_axiom_grounding('e6113200-5f4e-4a58-aec9-dcd193c909fa', taqlid_is_obligatory_for_non_mujtahid, conventional).
narrative_ontology:cs_reference_frame('e6113200-5f4e-4a58-aec9-dcd193c909fa', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('e6113200-5f4e-4a58-aec9-dcd193c909fa', contemporary_globalized_islam, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e6113200-5f4e-4a58-aec9-dcd193c909fa', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, conservative_muslim_communities).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, independent_islamic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The traditional religious scholars who interpret and transmit classical fiqh. Their authority and social standing are directly tied to the preservation and enforcement of taqlid. They benefit from the stability and predictability of established rulings, and from their role as gatekeepers of religious knowledge.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% The formal schools of Islamic law (madhahib) and their associated educational and legal bodies. They benefit from the continued adherence to their established methodologies and rulings, which ensures their institutional relevance and funding.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    organized, generational, constrained, global).

% Communities that find comfort and stability in following established religious norms and rulings. They benefit from the clear guidance and social cohesion provided by taqlid, avoiding the perceived chaos of individual interpretation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, conservative_muslim_communities, beneficiary,
    organized, biographical, constrained, local).

% Individuals who seek to reinterpret Islamic texts in light of modern ethical considerations, human rights, or scientific advancements. They bear the cost of social ostracization, academic marginalization, and legal challenges when their views conflict with established taqlid rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, identity_locked, global).

% Women whose legal and social status is often circumscribed by classical fiqh rulings on family law, inheritance, and public roles. They bear the direct costs of these rulings and face significant barriers to advocating for reform within traditionalist frameworks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status, payer,
    powerless, generational, trapped, local).

% Non-Muslims living in traditionalist-dominant contexts, whose rights and protections are defined by classical fiqh's dhimmi status. They bear the costs of legal and social discrimination, with little to no recourse for challenging these established norms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, generational, trapped, local).

% Scholars who advocate for renewed ijtihad (independent reasoning) and challenge the strict adherence to taqlid. They face professional marginalization, accusations of heresy, and difficulty securing institutional support for their research and teaching.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, independent_islamic_scholars, payer,
    moderate, biographical, constrained, global).

% Legal systems in some Muslim-majority countries that operate outside or alongside traditional fiqh, particularly in commercial or administrative law. They are excluded from the internal doctrinal debates of taqlid but may influence its application through state policy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, secular_legal_systems, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, coherent, and widely accepted framework for Islamic law and ethics, ensuring consistency across diverse Muslim communities and preventing fragmentation into countless individual interpretations.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual Muslims and contemporary scholars to the established classical schools and their contemporary representatives (ulama), along with the social capital and institutional power that accompanies this authority.
% ABSENT_VOICES: Reformist scholars and activists, particularly those advocating for gender equality or minority rights, are often marginalized or silenced within traditionalist discourse. Their arguments for contextual ijtihad are excluded from the authoritative consensus-building process.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid vanished overnight, it would lead to a profound reordering of religious authority, legal practice, and social norms within traditionalist Muslim communities. There would be a surge in individual interpretation (ijtihad), fragmentation of legal rulings, and a significant challenge to the authority of the traditional ulama and madhhab institutions.
% FOUNDING_PROBLEM: The early Muslim community faced the challenge of maintaining legal and ethical coherence across a rapidly expanding empire, preventing arbitrary rulings and ensuring fidelity to the Quran and Sunnah after the Prophet's death.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist ulama assert the problem is still live, arguing that abandoning taqlid would lead to chaos and heresy. Reformist scholars and independent academics, from outside the benefiting parties, argue that the original problem of coherence has been superseded by new challenges (e.g., human rights, modern statecraft) that require renewed ijtihad, rendering strict taqlid an obstacle rather than a solution.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.75) because the system transfers significant interpretive authority and social capital to traditional institutions, often at the expense of individual agency and the rights of marginalized groups. Suppression (0.88) is very high, reflecting the institutionalized mechanisms (religious education, fatwa councils, social pressure) that enforce adherence to taqlid and marginalize dissenting voices. The theater ratio (0.20) is relatively low, as the system is actively maintained and its functions (both coordination and extraction) are genuinely performed, not merely theatrical. Accessibility collapse is high (0.70) because for many, particularly in traditionalist-dominant contexts, the social and religious cost of seeking alternatives to taqlid is substantial.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional ulama, taqlid is a necessary rope for religious coherence and preservation. From the perspective of progressive Muslims or women's rights advocates, it operates as a snare, extracting agency and perpetuating inequality. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama and madhhab institutions are clear beneficiaries, as their authority and existence are predicated on taqlid. Conservative Muslim communities also benefit from the perceived stability and clarity. Progressive Muslims, women seeking equal status, religious minorities, and independent scholars are victims, bearing the costs of restricted interpretation, curtailed rights, and social marginalization. The directionality for beneficiaries is low (subsidized by the constraint), while for victims it is high (targeted by the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring coherence and fidelity to foundational texts) is contested. While traditionalists argue it's still live, reformists argue it's 'dead' in its original form, having become a tool for institutional power rather than a genuine solution to contemporary challenges. The classification as tangled_rope, rather than a pure rope, prevents mislabeling by acknowledging the asymmetric extraction inherent in its operation, even if a coordination function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_scope_ambiguity,
    'Is the classical consensus (ijma) truly binding for all time and contexts, or is its scope limited to specific historical periods and issues?',
    'Historical-critical analysis of the formation of ijma and its application in different eras, alongside theological arguments regarding its epistemological limits.',
    'If ijma''s scope is limited, the constraint''s suppression of contemporary ijtihad would be less justified, potentially lowering its effective extractiveness for victims seeking reform. If universally binding, the traditionalist position is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_scope_ambiguity, conceptual, 'Ambiguity regarding the universal applicability of classical ijma.').

omega_variable(
    taqlid_necessity_ambiguity,
    'Is taqlid (emulation) a necessary safeguard against religious fragmentation and error, or an obstacle to intellectual and ethical progress in Islam?',
    'Empirical study of communities that practice different degrees of ijtihad vs. taqlid, assessing their social cohesion, ethical development, and intellectual vitality. Theological arguments on the role of reason in Islamic law.',
    'If taqlid is shown to be an obstacle, the constraint''s legitimacy would erode, leading to increased resistance and potentially lower suppression. If shown to be necessary, its coordination function would be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taqlid_necessity_ambiguity, preference, 'Whether taqlid is a necessary good or an impediment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional barriers, legal codes) or internalized (social pressure, fear of ostracization, self-censorship)?',
    'Post-exit suppression trajectory: if suppression persists after formal legal/institutional barriers are removed (e.g., in diaspora communities), reclassify as partially internalized. Sociological studies on community pressure and individual belief formation.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective. If purely structural, removing external barriers would be sufficient for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 10, 0.17).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.18).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 30, 0.19).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.2).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 50, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel. This 'traditionalist_taqlid' reading emphasizes adherence to classical fiqh, contrasting with 'reformist_ijtihad' (contextual interpretation) and 'state_hybrid' (selective state application). Each reading represents a distinct constraint with different beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
