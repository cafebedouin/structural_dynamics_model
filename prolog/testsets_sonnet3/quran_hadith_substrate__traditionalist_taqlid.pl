% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Traditionalist Taqlid: Obligatory Adherence to Classical Madhhab Consensus
 *   domain: religious/legal
 *
 * SUMMARY:
 *   In jurisdictions and communities where traditionalist taqlid
 *   predominates, contemporary Muslims are expected to resolve legal and
 *   ritual questions by reference to the established rulings of a recognized
 *   madhhab rather than through independent reasoning from Quran and hadith.
 *   This constraint models that specific reading: ijma as historically
 *   settled and binding, and taqlid as the required posture of the ordinary
 *   believer toward it. The coordination function is genuine (stable,
 *   learnable, transmissible practice across a vast and diverse global
 *   community) but the arrangement also concentrates interpretive authority
 *   in a scholarly class whose institutional position depends on taqlid's
 *   continuation, while imposing disproportionate costs on women, religious
 *   minorities, and dissenting or minority-sexuality believers who have no
 *   standing in the interpretive process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.79).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid: Obligatory Adherence to Classical Madhhab Consensus").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '8dcfaae7-03eb-499f-b11a-40b076594cf3').
narrative_ontology:cs_kernel_codification('8dcfaae7-03eb-499f-b11a-40b076594cf3', fixed_text).
narrative_ontology:cs_authority_grounding('8dcfaae7-03eb-499f-b11a-40b076594cf3', lineage).
narrative_ontology:cs_interpretation_layer_present('8dcfaae7-03eb-499f-b11a-40b076594cf3').
narrative_ontology:cs_reading_relation('8dcfaae7-03eb-499f-b11a-40b076594cf3', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('8dcfaae7-03eb-499f-b11a-40b076594cf3', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('8dcfaae7-03eb-499f-b11a-40b076594cf3', foundational, ijma_constitutes_closed_binding_consensus).
narrative_ontology:cs_axiom_status(ijma_constitutes_closed_binding_consensus, holdable).
narrative_ontology:cs_axiom_grounding('8dcfaae7-03eb-499f-b11a-40b076594cf3', ijma_constitutes_closed_binding_consensus, conventional).
narrative_ontology:cs_axiom('8dcfaae7-03eb-499f-b11a-40b076594cf3', foundational, independent_ijtihad_by_laypeople_illegitimate).
narrative_ontology:cs_axiom_status(independent_ijtihad_by_laypeople_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('8dcfaae7-03eb-499f-b11a-40b076594cf3', independent_ijtihad_by_laypeople_illegitimate, conventional).
narrative_ontology:cs_axiom('8dcfaae7-03eb-499f-b11a-40b076594cf3', secondary, classical_rulings_reflect_immutable_divine_intent).
narrative_ontology:cs_axiom_status(classical_rulings_reflect_immutable_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('8dcfaae7-03eb-499f-b11a-40b076594cf3', classical_rulings_reflect_immutable_divine_intent, theological).
narrative_ontology:cs_reference_frame('8dcfaae7-03eb-499f-b11a-40b076594cf3', classical_madhhab_consolidation_period).
narrative_ontology:cs_drift_state('8dcfaae7-03eb-499f-b11a-40b076594cf3', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8dcfaae7-03eb-499f-b11a-40b076594cf3', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, religious_court_officials).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, muslims_in_interfaith_marriages).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lgbtq_muslims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, lay_muslim_worshippers).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_court_officials).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_muslim_worshippers).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, ijma_as_binding_consensus).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, continuity_of_transmitted_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained scholars who issue fatwas, staff religious courts, and administer madrasas within a specific madhhab lineage. They determine which contemporary questions receive new rulings and which are foreclosed by settled consensus. Their social status, income, and institutional position depend on the continued authority of taqlid; they retain personal interpretive latitude (ijtihad) unavailable to laypeople.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_scholarly_class, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Formal schools of jurisprudence (Hanafi, Maliki, Shafi'i, Hanbali, and Shia equivalents) that certify scholars, publish authoritative texts, and coordinate with state religious ministries. They receive funding, waqf endowments, and political patronage tied to their role as gatekeepers of orthodox practice.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, agenda_setter).

% Judges (qadis) in family and personal-status courts who apply madhhab rulings to divorce, inheritance, custody, and marriage cases. They benefit from institutional legitimacy and state salary, but are also bound by the same taqlid framework and face professional and social sanction if they issue independent rulings that depart from the established school.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_court_officials, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, religious_court_officials, payer).

% Practicing Muslims who accept core Islamic obligations but wish to apply contextual reasoning to inheritance shares, testimony weight, or ritual practice. Publicly departing from madhhab rulings risks social ostracism, accusations of apostasy, and exclusion from community religious life; formal exit from the religious community carries severe social and sometimes legal costs.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, national).

% Bear the direct weight of classical rulings on unequal inheritance shares, unilateral male divorce (talaq), guardianship requirements for marriage, and testimony discounting in religious courts. In jurisdictions where family law is state-enforced through madhhab-derived codes, there is no legal exit within the religious-legal system itself; civil alternatives, where they exist, may not be recognized by the community or extended family.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status, payer,
    powerless, biographical, trapped, national).

% Non-Muslim residents in jurisdictions retaining classical dhimmi-derived frameworks (special taxation, restrictions on testimony, proselytizing bans, or unequal legal standing). They have no voice in the jurisprudential process that defines their status and no jurisdictional alternative where the state enforces classical rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_status, payer,
    powerless, generational, trapped, national).

% Face nullification or non-recognition of marriages that fall outside classical permissibility rules (e.g., Muslim women marrying non-Muslim men), with direct consequences for inheritance, child custody, and legal residency in jurisdictions applying madhhab family law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, muslims_in_interfaith_marriages, payer,
    powerless, biographical, trapped, national).

% Subject to classical criminal and moral rulings treated as settled and non-revisable under taqlid. Exposed to social exclusion, family repudiation, and in some jurisdictions criminal prosecution under state-adopted classical penal frameworks; no standing within the jurisprudential process to contest the underlying rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, lgbtq_muslims, payer,
    powerless, biographical, trapped, national).

% Muslim jurists and intellectuals who argue for renewed ijtihad on the grounds that classical rulings reflect the social conditions of their formation rather than immutable divine command. They are frequently denied recognition as qualified interpreters, excluded from official fatwa councils, and in some contexts face charges of heresy for publishing reinterpretations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, generational, constrained, global).

% Most ordinary practitioners benefit from taqlid's coordination function: a settled, learnable body of ritual and ethical practice that does not require each person to independently derive rulings from primary sources. This genuinely lowers the cognitive and social cost of religious practice, but locks them out of ritual questions where classical answers conflict with their own ethical intuitions on gender, family, or minority relations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, lay_muslim_worshippers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, lay_muslim_worshippers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Taqlid solves a genuine and difficult coordination problem: without a settled body of interpretation, hundreds of millions of practitioners would each need specialist training to independently derive rulings on prayer, fasting, marriage, inheritance, and commerce from the Quran and hadith corpus. The madhhab system provides a stable, transmissible, community-legible standard that lowers the interpretive burden on ordinary believers and prevents chaotic proliferation of contradictory individual rulings.
% TRANSFER_FUNCTION: Interpretive authority and its downstream social, legal, and economic consequences (inheritance shares, marital exit rights, testimony weight, custody outcomes, minority legal status) are moved from the individual conscience and from potential reformist reinterpretation toward the certified scholarly class and the historical rulings they administer. Religious legitimacy, funding, and institutional power flow toward ulama and madhhab institutions; the cost of foreclosed reinterpretation is borne disproportionately by women, minorities, and dissenting believers.
% ABSENT_VOICES: Progressive and reformist scholars are formally excluded from fatwa-issuing councils in traditionalist-dominant states; women affected by inheritance and divorce rulings have no formal role in the jurisprudential process that produced those rulings; religious minorities under dhimmi-derived frameworks have no representation in the Islamic legal tradition that defines their status at all.
% DISAPPEARANCE_RATIONALE: If the obligation of taqlid to classical madhhab rulings vanished overnight, religious courts would lose their doctrinal basis for enforcing current inheritance, divorce, and testimony rules; scholars would lose their gatekeeping monopoly over ritual and legal interpretation; and a wave of contested, decentralized reinterpretation would follow — precisely the outcome the reformist_ijtihad reading treats as legitimate and the traditionalist reading treats as illegitimate fragmentation.
% FOUNDING_PROBLEM: In the first centuries after the Prophet's death, as the Muslim community expanded rapidly across diverse regions with no living authoritative interpreter, competing and sometimes contradictory legal opinions threatened communal unity and legal predictability. The madhhab system and the ijma consensus it claimed to represent were built to stabilize practice, preserve transmitted knowledge accurately, and prevent doctrinal fragmentation across a geographically vast, rapidly Islamizing population.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary comparative-religion scholars and historians of Islamic law (writing from academic institutions outside the ulama establishment) broadly corroborate that the founding problem — preventing fragmentation in a period without a living Prophet or centralized authority — was real and substantially resolved by the classical period. Reformist scholars within the tradition, along with human-rights-oriented Muslim jurists, attest that the founding problem is now largely solved (transmission and textual preservation are secure) and that continued mandatory taqlid persists for institutional and social-control reasons rather than to solve the original transmission problem. The traditionalist ulama themselves are the primary source asserting the founding problem remains fully live; independent corroboration from outside the beneficiary class supports the 'largely dead, function has shifted' reading rather than the 'still live' reading.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.68 at interval end, reflecting institutionalized enforcement of classical rulings in family courts, inheritance law, and status determinations for minorities — mechanisms with direct, measurable material consequences (inheritance shares, custody outcomes, criminal exposure) rather than merely doctrinal disagreement. Suppression (0.79) is higher than extractiveness because the constraint's persistence depends heavily on foreclosing alternative interpretation — social sanction, apostasy accusations, and exclusion from fatwa councils for reformist voices — independent of any single extraction event. Theater ratio is comparatively low (0.28) because the coordination function (a genuinely usable, transmissible ritual and legal framework for hundreds of millions of practitioners) remains substantially functional; this is not a purely performative arrangement. Accessibility collapse (0.62) reflects that once a believer accepts taqlid's premise, alternative interpretations become socially and often legally unavailable, though not as completely foreclosed as a natural law. Resistance (0.58) reflects real and growing reformist, feminist, and minority-rights contestation from within and adjacent to the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ulama, madhhab institutions), the arrangement reads as fidelity to sound transmitted consensus protecting the community from doctrinal chaos. From the payer seats (women seeking equal status, religious minorities, progressive and LGBTQ Muslims), the same structure reads as an enforced foreclosure of interpretation that happens to concentrate authority and material benefit in the hands of those administering it. The engine computes this divergence from the declared power, exit, and beneficiary/victim structure; the claimed_type (tangled_rope) is authored independently of these seat-level metrics and the divergence itself is the analytical signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Ulama, madhhab institutions, and religious court officials sit near the beneficiary end: they administer the system, derive status and material support from its continuation, and retain personal interpretive latitude denied to laypeople. Progressive Muslims, women, religious minorities, interfaith-married Muslims, and LGBTQ Muslims sit near the target end: they bear concrete legal and social costs from rulings they had no role in producing and cannot exit without severe social or legal consequence. Ordinary lay worshippers are more symmetric — the arrangement lowers their interpretive burden but locks them out of revision on issues touching their own lives, hence the dual beneficiary/payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the fact that taqlid solves a real coordination problem — a decentralized global religious community genuinely benefits from a stable, learnable common standard rather than atomized individual reinterpretation. Collapsing this into a pure snare would erase that function and mischaracterize the experience of the many lay believers for whom the system is not experienced as extraction. Equally, treating it as a pure rope would erase the asymmetric, non-consensual cost borne by women, minorities, and dissenters who never agreed to and cannot exit the arrangement. The tangled_rope classification requires both the genuine coordination function and the identifiable victim class to be true simultaneously, which is the structural claim this story makes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_scope_of_consensus,
    'Was classical ijma ever the comprehensive, settled consensus the traditionalist reading claims, or was it always a retrospectively constructed unity papering over genuine historical disagreement among early jurists?',
    'Historical-critical analysis of early fiqh disagreement records (ikhtilaf literature) comparing the actual diversity of early juristic opinion against later claims of unanimous consensus.',
    'If ijma was always more contested and constructed than claimed, the traditionalist reading''s core legitimating premise weakens substantially, supporting the reformist reading''s claim that renewed ijtihad is methodologically consistent with, not a break from, the early tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_scope_of_consensus, empirical, 'Whether classical ijma represents genuine historical unanimity or retrospective institutional consolidation.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination benefit of a stable, learnable jurisprudential standard separable from the specific asymmetric costs imposed on women, minorities, and dissenters, or are those costs intrinsic to the content of the classical rulings themselves?',
    'Comparative analysis of reformed madhhab-adjacent legal systems (e.g., codified family law reforms in Tunisia, Morocco) that retain doctrinal continuity while revising specific gender and minority provisions, assessing whether communal stability and legitimacy survive such revision.',
    'If separable, the extraction is not intrinsic to taqlid as coordination but to specific unrevised content — supporting a scaffold-style reform path. If inseparable, the tangled_rope classification understates how tightly extraction is bound to the coordination mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether taqlid''s coordination function can be preserved while revising its extractive content.').

omega_variable(
    committer_framing_selection,
    'Is the traditionalist_taqlid reading correctly modeled as the ''standing arrangement under contest'' baseline, given that in many majority-Muslim states it is the state_hybrid reading, not pure traditionalist taqlid, that is actually enforced through law?',
    'Jurisdiction-by-jurisdiction mapping of which reading (traditionalist_taqlid, reformist_ijtihad, state_hybrid) actually governs enforced family and criminal law versus which governs informal social and religious authority.',
    'Where state_hybrid is the operative enforced arrangement, this traditionalist_taqlid story''s ε should be understood as describing informal community and religious-court authority rather than the full state legal apparatus; conflating the two would overstate this reading''s direct legal enforcement power in hybrid jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Alternative framing: whether this reading describes de facto religious authority or de jure state enforcement, and how that choice affects ε attribution across the kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.15).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.18).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.21).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.25).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(qura_be_t100, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_hadith_substrate kernel. traditionalist_taqlid (this story) authors high suppression and institutionalized enforcement, treating classical madhhab consensus as binding. reformist_ijtihad authors substantially lower suppression and different beneficiary/victim sets, treating contextual reinterpretation as legitimate and mandated. state_hybrid authors a mixed structure where state sovereignty rather than doctrinal fidelity grounds legitimacy, selectively adopting classical rulings in some domains and secular/reformist frameworks in others. Each story carries its own stable ε assessed by its own reading's lights; none averages across the others. The three are linked via affects_constraints because institutional and political pressure in one reading's jurisdiction (e.g., traditionalist enforcement hardening) structurally affects resource availability and legitimacy conditions for reformist and hybrid readings operating in adjacent or overlapping populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
