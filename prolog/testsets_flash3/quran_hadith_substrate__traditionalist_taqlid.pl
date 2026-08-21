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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   obligated to follow their established rulings through taqlid (emulation).
 *   This reading emphasizes continuity, stability, and the preservation of
 *   religious authority structures. The metrics reflect a high degree of
 *   extraction and suppression, as this framework often limits individual
 *   interpretation and adaptation to modern contexts, particularly impacting
 *   progressive Muslims, women, and religious minorities. The claimed type is
 *   'tangled_rope' because it provides a genuine coordination function (legal
 *   stability) but also involves significant asymmetric extraction and
 *   requires active enforcement to maintain its authority against reformist
 *   challenges.
 *
 * KEY AGENTS:
 *   - traditional_ulama: Primary agenda_setter (institutional/identity_locked) — benefits from constraint
 *   - madhhab_institutions: Beneficiary (organized/constrained) — benefits from constraint
 *   - conservative_muslim_communities: Beneficiary (organized/identity_locked) — benefits from constraint
 *   - progressive_muslims: Primary payer (moderate/constrained) — bears extraction
 *   - women_seeking_equal_status: Primary payer (powerless/trapped) — bears extraction
 *   - religious_minorities: Primary payer (powerless/trapped) — bears extraction
 *   - reformist_scholars: Excluded (powerful/constrained) — would object but is not in the conversation
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
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '7c44fa34-f84f-4a58-9b9d-07a36208933c').
narrative_ontology:cs_kernel_codification('7c44fa34-f84f-4a58-9b9d-07a36208933c', fixed_text).
narrative_ontology:cs_authority_grounding('7c44fa34-f84f-4a58-9b9d-07a36208933c', lineage).
narrative_ontology:cs_interpretation_layer_present('7c44fa34-f84f-4a58-9b9d-07a36208933c').
narrative_ontology:cs_reading_relation('7c44fa34-f84f-4a58-9b9d-07a36208933c', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('7c44fa34-f84f-4a58-9b9d-07a36208933c', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('7c44fa34-f84f-4a58-9b9d-07a36208933c', foundational, ijma_is_binding_for_all_generations).
narrative_ontology:cs_axiom_status(ijma_is_binding_for_all_generations, holdable).
narrative_ontology:cs_axiom_grounding('7c44fa34-f84f-4a58-9b9d-07a36208933c', ijma_is_binding_for_all_generations, theological).
narrative_ontology:cs_axiom('7c44fa34-f84f-4a58-9b9d-07a36208933c', foundational, taqlid_is_obligatory_for_non_mujtahids).
narrative_ontology:cs_axiom_status(taqlid_is_obligatory_for_non_mujtahids, holdable).
narrative_ontology:cs_axiom_grounding('7c44fa34-f84f-4a58-9b9d-07a36208933c', taqlid_is_obligatory_for_non_mujtahids, conventional).
narrative_ontology:cs_reference_frame('7c44fa34-f84f-4a58-9b9d-07a36208933c', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('7c44fa34-f84f-4a58-9b9d-07a36208933c', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7c44fa34-f84f-4a58-9b9d-07a36208933c', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, conservative_muslim_communities).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The traditional religious scholars who interpret and transmit classical fiqh, asserting the authority of ijma (consensus) and the necessity of taqlid (following established schools). Their authority and social standing are directly tied to the maintenance of this interpretive framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% The formal legal schools (madhahib) and their associated educational and judicial bodies. They benefit from the continued adherence to their established rulings, which ensures their relevance and funding. Deviating from taqlid would undermine their foundational premise.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    organized, generational, constrained, global).

% Communities that find stability and clarity in adhering to established rulings, viewing taqlid as a safeguard against error and innovation. They benefit from the social cohesion and clear guidance provided by this framework, often enforcing it through social pressure.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, conservative_muslim_communities, beneficiary,
    organized, biographical, identity_locked, local).

% Individuals and groups who seek to reinterpret Islamic law in light of contemporary ethics, human rights, and public interest (maslaha). They bear the cost of social ostracism, accusations of heresy, and lack of institutional support when challenging traditional rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Women whose legal and social status is often circumscribed by classical fiqh rulings (e.g., in family law, testimony, inheritance). They face significant barriers to achieving equal rights within this framework and have limited avenues for redress or reform.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_status, payer,
    powerless, generational, trapped, national).

% Non-Muslims living in contexts where classical fiqh (e.g., dhimmi status) informs legal and social norms. They bear the cost of unequal legal standing and social marginalization, with little to no ability to influence the interpretive framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, generational, trapped, national).

% Scholars who advocate for ijtihad (independent reasoning) and contextual reinterpretation of Islamic law. They are often excluded from mainstream religious institutions and face significant resistance from traditionalist authorities, limiting their ability to influence policy or public discourse.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, coherent, and historically grounded framework for Islamic legal and ethical guidance, ensuring continuity with past generations of scholars and preventing fragmentation of religious authority.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from individual contemporary Muslims to established classical fiqh schools and their contemporary custodians (ulama), in exchange for clear, pre-determined legal rulings.
% ABSENT_VOICES: Progressive Muslim intellectuals, feminist theologians, and human rights advocates who argue for a re-evaluation of classical rulings based on the Quran's ethical trajectory and contemporary values are largely marginalized or excluded from traditional interpretive circles. Their voices are suppressed by the very mechanisms that uphold taqlid.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid vanished overnight, it would lead to a profound crisis of authority within traditional Islamic institutions. Individual Muslims would be compelled to engage in ijtihad, leading to diverse interpretations, potential fragmentation of communities, and a significant reordering of religious leadership and legal systems.
% FOUNDING_PROBLEM: To prevent fragmentation and innovation (bid'ah) in Islamic law after the early generations, ensuring the preservation of the Prophet's sunnah and the consensus (ijma) of early scholars.
% FOUNDING_PROBLEM_CORROBORATION: Traditional ulama and madhhab institutions assert the problem is still live, citing the dangers of unqualified individual interpretation. Reformist scholars and human rights advocates argue that the problem of fragmentation has been superseded by the need for ethical and contextual engagement with modern challenges, and that taqlid now hinders progress; their arguments are supported by independent sociological and legal analyses of Muslim societies.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the constraint imposes significant costs on those seeking alternative interpretations or legal reforms, limiting their agency and often their rights. Suppression is very high due to institutionalized religious authority, social pressure, and in some contexts, state enforcement of classical rulings. The theater ratio is relatively low, as the institutions genuinely believe in and actively maintain the framework, though some of its justifications may be performative in modern contexts. Accessibility collapse is substantial as alternatives are actively suppressed. Resistance is moderate-high, reflecting ongoing challenges from reformist movements and human rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   Traditional ulama and madhhab institutions experience this as a necessary 'rope' for preserving religious integrity and community cohesion, with minimal extraction. Progressive Muslims, women, and minorities experience it as a 'snare' that limits their rights and agency, with high extraction and suppression. The engine's classification will reflect this divergence based on their structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional ulama and madhhab institutions are clear beneficiaries, as their authority and existence are predicated on this framework (low d). Conservative communities also benefit from the stability and clear guidance, reinforcing the constraint (low d). Progressive Muslims, women, and minorities are targets, bearing the costs of limited agency, social pressure, and legal disadvantages (high d). Reformist scholars are excluded, their attempts to offer alternatives actively suppressed, placing them at the target end of the spectrum.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling it as a pure 'rope' (which would ignore the significant extraction) or a pure 'snare' (which would ignore its genuine coordination function in providing legal stability and historical continuity for many adherents). The contest over its 'founding_problem_status' (contested) and 'disappearance_verdict' (world_rearranges) highlights the ongoing debate about whether its mandate has atrophied or remains vital, preventing a premature 'piton' classification. The constraint is actively defended and benefits identifiable parties, making it far from inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_scope_ambiguity,
    'Is the classical ijma (consensus) truly binding for all future generations, or is its scope limited to specific historical contexts and issues?',
    'Historical-critical analysis of the formation of ijma and its application in different eras, alongside theological arguments for the possibility of re-evaluating past consensus.',
    'If ijma is found to be historically contingent, it would significantly weaken the authority of taqlid, potentially reclassifying the constraint as a ''snare'' for those seeking reform, as its coordination function would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_scope_ambiguity, conceptual, 'Ambiguity regarding the universal and eternal binding nature of classical ijma.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional barriers, legal codes) or internalized (cognitive patterns, social norms that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals leaving traditional communities still self-censor), reclassify as partially internalized. Sociological studies on the impact of religious education and community socialization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making genuine reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in adherence to taqlid.').

omega_variable(
    taqlid_ijtihad_framing_contest,
    'Is the relationship between taqlid and ijtihad one of strict opposition, or can they be integrated within a single framework (e.g., ''guided ijtihad'' within madhhab principles)?',
    'Analysis of contemporary fatwas and scholarly debates that attempt to bridge the gap, and their acceptance within mainstream institutions. Conceptual analysis of the logical compatibility of the two approaches.',
    'If integration is possible, the ''reformist_ijtihad'' reading might shift from ''coexists_with'' to ''influences'' or even ''forecloses'' the strict traditionalist view, leading to a re-evaluation of the constraint''s rigidity and potential for internal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_ijtihad_framing_contest, conceptual, 'Contest over the conceptual framing of taqlid and ijtihad as mutually exclusive or potentially reconcilable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 10, 0.23).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.22).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 30, 0.21).
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
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_hadith_substrate' kernel. It represents the traditionalist interpretation of Islamic legal authority, emphasizing taqlid (emulation) of classical fiqh schools. It is linked to sibling readings (reformist_ijtihad, state_hybrid) which offer alternative approaches to interpreting the Quran and Hadith.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
