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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid (Adherence to Classical Fiqh Schools)
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the traditionalist reading of Islamic
 *   jurisprudence, where classical fiqh schools (madhhabs) are considered to
 *   represent an authoritative consensus (ijma), and contemporary Muslims are
 *   obligated to follow their rulings via taqlid (adherence). This reading
 *   emphasizes continuity with tradition and minimizes independent reasoning
 *   (ijtihad). It is one reading of the 'quran_hadith_substrate' kernel,
 *   which concerns the foundational sources of Islamic law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.7).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.85).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, snare).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid (Adherence to Classical Fiqh Schools)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'f21d3082-101e-4a73-a870-9ebb5bb53f63').
narrative_ontology:cs_kernel_codification('f21d3082-101e-4a73-a870-9ebb5bb53f63', fixed_text).
narrative_ontology:cs_authority_grounding('f21d3082-101e-4a73-a870-9ebb5bb53f63', lineage).
narrative_ontology:cs_interpretation_layer_present('f21d3082-101e-4a73-a870-9ebb5bb53f63').
narrative_ontology:cs_reading_relation('f21d3082-101e-4a73-a870-9ebb5bb53f63', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('f21d3082-101e-4a73-a870-9ebb5bb53f63', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('f21d3082-101e-4a73-a870-9ebb5bb53f63', foundational, ijma_of_madhhabs_is_binding).
narrative_ontology:cs_axiom_status(ijma_of_madhhabs_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('f21d3082-101e-4a73-a870-9ebb5bb53f63', ijma_of_madhhabs_is_binding, conventional).
narrative_ontology:cs_axiom('f21d3082-101e-4a73-a870-9ebb5bb53f63', foundational, taqlid_is_obligatory_for_laymen).
narrative_ontology:cs_axiom_status(taqlid_is_obligatory_for_laymen, holdable).
narrative_ontology:cs_axiom_grounding('f21d3082-101e-4a73-a870-9ebb5bb53f63', taqlid_is_obligatory_for_laymen, deontological).
narrative_ontology:cs_reference_frame('f21d3082-101e-4a73-a870-9ebb5bb53f63', classical_madhhab_supremacy).
narrative_ontology:cs_drift_state('f21d3082-101e-4a73-a870-9ebb5bb53f63', contemporary_globalized_islam, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f21d3082-101e-4a73-a870-9ebb5bb53f63', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, independent_ijtihad_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of classical fiqh, benefiting from the authority derived from established madhhabs. They maintain the intellectual and social infrastructure that legitimizes taqlid and actively suppress alternative interpretations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, arbitrage, global).

% The formal and informal structures (seminaries, endowments, scholarly networks) that perpetuate the teachings and authority of specific classical schools of law. They receive financial and social capital from the adherence to taqlid.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Local religious authorities who derive their legitimacy and influence from their adherence to and propagation of traditional madhhab rulings. They benefit from a stable, predictable interpretive framework that minimizes internal dissent.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, biographical, constrained, local).

% Individuals who seek to reconcile Islamic teachings with modern ethical standards, human rights, and social justice. They face social ostracism, intellectual marginalization, and sometimes legal penalties for challenging established rulings, yet remain committed to their Muslim identity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    powerless, biographical, identity_locked, global).

% Women advocating for gender equality within Islamic legal frameworks, often finding classical fiqh rulings to be discriminatory. They bear the social and legal costs of challenging patriarchal interpretations, with their identity as Muslims making exit from the framework unthinkable.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status, payer,
    powerless, generational, identity_locked, global).

% Non-Muslim communities living in traditionalist-dominant contexts, whose legal and social status is often defined by classical dhimmi (protected non-Muslim) rulings that grant them limited rights and impose specific obligations. They are structurally trapped by the prevailing legal system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, generational, trapped, national).

% Scholars who attempt independent reasoning (ijtihad) outside the strictures of classical madhhabs, seeking to derive rulings directly from the Quran and Sunnah. They face accusations of innovation (bid'ah) and lack institutional support, limiting their influence and career prospects.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, independent_ijtihad_scholars, payer,
    moderate, biographical, constrained, global).

% Advocates for a contextualized approach to Islamic law, prioritizing the Quran's ethical trajectory and contemporary public interest. They are actively marginalized by traditionalist institutions and their arguments are often dismissed as illegitimate or uninformed.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, coherent, and widely accepted body of Islamic law (fiqh) that guides the religious and social lives of Muslims, minimizing interpretive chaos and ensuring continuity with historical tradition.
% TRANSFER_FUNCTION: Transfers interpretive authority and social capital from individual Muslims and independent scholars to established classical fiqh schools and their contemporary representatives (ulama, madhhab institutions), in exchange for perceived religious certainty and communal cohesion.
% ABSENT_VOICES: Reformist scholars and progressive Muslim movements, particularly those advocating for gender equality or minority rights, are systematically excluded from authoritative interpretive circles. Their voices are dismissed as lacking traditional credentials or being influenced by 'Western' ideas, preventing their challenges to taqlid from gaining traction within traditionalist discourse.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid and the authority of classical fiqh schools vanished overnight, the entire structure of Islamic legal and religious authority would collapse. Muslims would be forced to engage in independent reasoning, leading to immense interpretive diversity, potential fragmentation of communities, and a radical re-evaluation of religious practice and social norms. The traditional ulama and madhhab institutions would lose their primary source of legitimacy and power.
% FOUNDING_PROBLEM: The early Muslim community faced the challenge of deriving consistent legal rulings from the Quran and Sunnah across diverse geographical and cultural contexts, leading to interpretive divergence and potential chaos. The development of madhhabs and the principle of taqlid aimed to provide stability, coherence, and a mechanism for legal certainty.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist ulama assert the problem of interpretive chaos remains live, necessitating taqlid for religious cohesion. Reformist scholars and progressive Muslim groups, however, argue that the founding problem of legal consistency has been superseded by the problem of ethical stagnation and social injustice, and that taqlid now serves to maintain an outdated legal system rather than genuinely solve contemporary challenges. Independent academic analyses of Islamic legal history corroborate the historical function of taqlid but also document its contemporary contestation.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).

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
 *   The extractiveness (0.7) is high because adherence to taqlid transfers significant interpretive authority and social capital to traditional religious institutions, often at the expense of individual agency and the ability to address contemporary ethical challenges. Suppression (0.85) is very high due to the institutionalized mechanisms (educational systems, fatwa councils, social pressure) that actively marginalize and delegitimize alternative readings like reformist ijtihad. The theater ratio (0.2) is low, indicating that the constraint's primary function is still genuinely about maintaining doctrinal authority, though some performative aspects exist in defending its 'naturalness' against modern critiques. The historical measurements show a gradual increase in both extractiveness and suppression as traditionalist institutions consolidated their authority over centuries and faced modern challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional ulama and madhhab institutions, this constraint is a necessary Rope, ensuring religious cohesion and doctrinal purity. They experience it as a coordination mechanism that preserves the integrity of Islamic law. However, from the perspective of progressive Muslims, women seeking equal status, and independent scholars, it operates as a Snare, extracting their interpretive agency and imposing rulings that may conflict with their ethical sensibilities, with high social and intellectual costs for dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama and madhhab institutions are clear beneficiaries (d near 0.0) as they gain authority and resources from the system. Progressive Muslims, women, religious minorities, and independent scholars are targets (d near 1.0) as they bear the costs of conformity or dissent. The 'identity_locked' exit option for progressive Muslims and women reflects their deep commitment to their Muslim identity, making a complete exit from the framework unthinkable despite its extractive nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was to provide legal certainty and cohesion. While traditionalists argue this problem is still live, reformists contend that the constraint has outlived its original function and now primarily serves to maintain the power structures of traditional religious authority, thus exhibiting mandatrophy. The high and increasing suppression, coupled with the contested status of the founding problem, suggests a drift towards a Snare, where the coordination story is increasingly a cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_scope_ambiguity,
    'Is the ''ijma'' (consensus) of classical fiqh schools truly universal and binding for all times, or is it a historical consensus specific to its context?',
    'Historical-critical analysis of the formation of ijma and its application across different eras, examining whether its scope was ever intended to be trans-historical and trans-cultural.',
    'If ijma is found to be historically contingent, the foundational claim of traditionalist taqlid weakens, potentially reclassifying it from a Snare (claiming universal authority) to a Tangled Rope (a historical coordination mechanism with contemporary extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_scope_ambiguity, conceptual, 'Ambiguity regarding the universal and binding nature of classical ijma.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings structural (institutional barriers, legal penalties) or internalized (cognitive patterns, fear of social ostracism)?',
    'Post-exit suppression trajectory: if scholars or communities who leave traditionalist institutions continue to self-censor or face internal conflict, it suggests internalized suppression. If only external barriers are removed, reclassify as primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making genuine intellectual freedom harder to achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative Islamic legal interpretations.').

omega_variable(
    taqlid_ijtihad_boundary,
    'At what point does ''guided taqlid'' (following a madhhab with some understanding) transition into ''independent ijtihad'' (original legal reasoning), and is this boundary genuinely permeable or institutionally policed?',
    'Empirical study of scholarly careers and fatwa issuance: if scholars can genuinely transition from taqlid to ijtihad within traditional institutions without significant penalty, the boundary is permeable. If not, it is institutionally policed.',
    'If the boundary is permeable, the constraint''s suppression is lower, as it allows for internal evolution. If policed, it reinforces the Snare classification by demonstrating active suppression of intellectual mobility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taqlid_ijtihad_boundary, empirical, 'Permeability of the boundary between taqlid and ijtihad.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''traditionalist_taqlid'' the most appropriate framing for this constraint, or would a ''reformist_ijtihad'' or ''state_hybrid'' framing better capture its structural dynamics?',
    'Analysis of the dominant discourse and institutional power in a given context: if traditionalist institutions hold sway, this framing is accurate. If reformist voices are gaining institutional power or state actors are the primary enforcers, alternative framings might be more appropriate.',
    'Adopting a ''reformist_ijtihad'' framing would likely lower extractiveness and suppression, reclassifying the constraint as a Rope or Tangled Rope. A ''state_hybrid'' framing would shift beneficiaries to state actors and highlight political rather than purely doctrinal extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the ''quran_hadith_substrate'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1000, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(qura_tr_t1300, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(qura_tr_t1600, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(qura_tr_t1850, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1850, 0.18).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t1000, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(qura_be_t1300, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1300, 0.5).
narrative_ontology:measurement(qura_be_t1600, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(qura_be_t1850, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1000, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(qura_su_t1300, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1300, 0.6).
narrative_ontology:measurement(qura_su_t1600, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(qura_su_t1850, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, islamic_family_law_codes).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, islamic_finance_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel, which concerns the foundational sources of Islamic law. The other readings are 'reformist_ijtihad' and 'state_hybrid', each representing a distinct structural claim about interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
