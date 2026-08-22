% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Traditionalist Taqlid Obligation to Classical Madhhab Consensus
 *   domain: religious/legal-authority
 *
 * SUMMARY:
 *   In traditionalist-dominant Muslim contexts, the established rulings of
 *   the classical fiqh schools are held to constitute binding consensus
 *   (ijma), and contemporary Muslims are obligated to follow those rulings
 *   through taqlid rather than derive law independently. The arrangement
 *   solves a real problem (who may interpret the scriptural corpus, and how a
 *   dispersed community keeps a shared practice) while simultaneously
 *   concentrating interpretive authority in a credentialed scholarly class,
 *   transmitting classical personal-status and minority-status rules into the
 *   present unmodified, and treating fresh derivation as presumption or
 *   transgression. Enforcement runs through credential gatekeeping,
 *   congregational and familial sanction, and, in traditionalist-dominant
 *   jurisdictions, state religious apparatuses. This file instantiates the
 *   traditionalist reading of the Quran-Hadith kernel as one clean,
 *   epsilon-invariant constraint; the kernel family and sibling readings are
 *   documented in kernel_context and the omega variables, not averaged into
 *   this story. Claim and metrics are authored independently: tangled_rope is
 *   my structural judgment (genuine coordination plus asymmetric extraction
 *   under active enforcement), and the metric values describe the
 *   arrangement's observed operation. The interval maps t=0 to the mid-1950s
 *   post-colonial constitutional moment and t=60 to the present.
 *
 * KEY AGENTS:
 *   - classical_fiqh_ulama: Agenda-setter and principal beneficiary (institutional / identity_locked) — administers the deference obligation, certifies who may interpret, collects the authority rents
 *   - madhhab_institutional_networks: Beneficiary (institutional / identity_locked) — collects students, endowments, publishing flows, and prestige
 *   - mosque_hierarchies: Beneficiary (organized / constrained) — collects posts and congregational standing by staffing the schools' positions
 *   - traditionalist_lay_communities: Beneficiary with payer costs (moderate / constrained) — receives coordination and continuity; forgoes independent derivation and, for women among them, equal standing under classical family rules
 *   - progressive_muslim_reformers: Payer (organized / constrained) — bears sanction for practicing or advocating fresh derivation
 *   - women_in_traditionalist_jurisdictions: Payer (powerless / trapped) — live under classical personal-status positions with the reform path closed
 *   - religious_minorities_under_dhimmi_frameworks: Payer (powerless / trapped) — hold subordinate status where classical minority rules are applied or invoked
 *   - reformist_ijtihad_scholars: Excluded (powerful / mobile) — credentialed jurists denied a seat in the traditionalist conversation
 *   - state_religious_establishments: Agenda-setter with a beneficiary position (institutional / constrained) — runs the official enforcement apparatus in traditionalist-dominant jurisdictions
 *   - academic_islamic_law_scholars: Analytical observer (analytical / analytical) — maps the structure without holding ruling authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.78).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid Obligation to Classical Madhhab Consensus").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal-authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '394d9abc-fcb5-49f7-97e2-45eec0b78052').
narrative_ontology:cs_kernel_codification('394d9abc-fcb5-49f7-97e2-45eec0b78052', fixed_text).
narrative_ontology:cs_authority_grounding('394d9abc-fcb5-49f7-97e2-45eec0b78052', lineage).
narrative_ontology:cs_interpretation_layer_present('394d9abc-fcb5-49f7-97e2-45eec0b78052').
narrative_ontology:cs_reading_relation('394d9abc-fcb5-49f7-97e2-45eec0b78052', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('394d9abc-fcb5-49f7-97e2-45eec0b78052', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('394d9abc-fcb5-49f7-97e2-45eec0b78052', foundational, madhhab_rulings_constitute_binding_ijma).
narrative_ontology:cs_axiom_status(madhhab_rulings_constitute_binding_ijma, holdable).
narrative_ontology:cs_axiom_grounding('394d9abc-fcb5-49f7-97e2-45eec0b78052', madhhab_rulings_constitute_binding_ijma, theological).
narrative_ontology:cs_axiom('394d9abc-fcb5-49f7-97e2-45eec0b78052', foundational, ijtihad_gate_closed_contemporary).
narrative_ontology:cs_axiom_status(ijtihad_gate_closed_contemporary, holdable).
narrative_ontology:cs_axiom_grounding('394d9abc-fcb5-49f7-97e2-45eec0b78052', ijtihad_gate_closed_contemporary, conventional).
narrative_ontology:cs_reference_frame('394d9abc-fcb5-49f7-97e2-45eec0b78052', closed_ijtihad_madhhab_consensus).
narrative_ontology:cs_drift_state('394d9abc-fcb5-49f7-97e2-45eec0b78052', contemporary_reformist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('394d9abc-fcb5-49f7-97e2-45eec0b78052', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, classical_fiqh_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutional_networks).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditionalist_lay_communities).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslim_reformers).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_in_traditionalist_jurisdictions).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, state_religious_establishments).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, traditionalist_lay_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurists trained through ijaza chains in one of the recognized schools. They write commentaries and supercommentaries on the school's corpus, examine candidates for teaching licenses, answer submitted questions with rulings attributed to the school, and decide which questions are settled and which remain open. Their livelihood, standing, and sense of vocation are bound to the corpus they transmit; stepping outside it would mean forfeiting the credentials that make their voice carry. They experience the deference directed at them as the proper order of religious life.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, classical_fiqh_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% Seminaries, endowments, publishing houses, and Sufi orders organized around each school's corpus. They enroll students, pay teachers, print the standard texts, and maintain the chains of transmission that certify authority. Their enrollment, endowment income, and prestige depend on the corpus remaining the reference point for religious questions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutional_networks, beneficiary,
    institutional, generational, identity_locked, global).

% Imams, preachers, and clerical associations who staff congregational life. They preach from the school's positions, officiate marriages and funerals under its rules, and draw the content of weekly guidance from the established corpus. Their posts and congregational standing are appointed and reviewed through networks that expect fidelity to the school.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, generational, constrained, regional).

% Observant Muslims who pray, fast, marry, contract, and bury under the school's guidance. They receive clear answers to religious questions and a shared practice that binds them to parents, neighbors, and ancestors. What they forgo is deriving rulings for themselves: the adab they are formed in treats questioning the school's settled positions as presumption, and the practical costs of dissent include marriage exclusion and loss of congregational standing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditionalist_lay_communities, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, traditionalist_lay_communities, payer).

% Scholars and lay movements who argue that scripture's ethical aims require fresh engagement where classical positions conflict with contemporary moral knowledge. They publish rereadings, run study circles, and seek pulpits and councils. What comes back: accusations of innovation or worse, denial of teaching posts and mosque platforms, and in some jurisdictions legal jeopardy. Their institutional bases sit outside the traditionalist networks, in universities, advocacy organizations, and diaspora communities.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslim_reformers, payer,
    organized, biographical, constrained, global).

% Women whose marriages, inheritance shares, testimony, and freedom of movement are governed by classical personal-status positions applied by courts or enforced by family and community. The corpus assigns them half shares in inheritance, discounted testimony in some schools, and male guardianship in marriage. Where the corpus is applied, rederiving these rules from the sources is precisely what the deference obligation forecloses. The exits available are emigration, family rupture, or leaving the community, and all carry severe costs.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_in_traditionalist_jurisdictions, payer,
    powerless, biographical, trapped, regional).

% Non-Muslims and minority sects living where classical rulings on non-Muslim status are applied or invoked: places of worship, family law across religious lines, eligibility for public office, and in some contexts testimony and tax provisions drawn from the classical corpus. Their equal-citizenship claims run against positions the deference obligation holds settled. Emigration or concealment are the exits available to those who can afford them.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, biographical, trapped, regional).

% Jurists with classical training who argue the qualifications for ijtihad are met today and that fresh derivation is a duty where circumstances have changed. They hold chairs, write widely read works, and command followings, but the traditionalist networks do not recognize their licenses, do not seat them on their councils, and do not transmit their rulings through their institutions. Their authority operates in a parallel economy of universities, publishers, and movements.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad_scholars, excluded,
    powerful, generational, mobile, global).

% Official muftiates, fatwa councils, ministries of religious affairs, and personal-status courts in jurisdictions where the state administers the schools' positions. They appoint preachers, certify curricula, and apply classical family law through the courts. They collect administrative legitimacy and employment from running this apparatus, their personnel depend on the corpus staying authoritative, and several also answer to political principals who find the apparatus useful.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_religious_establishments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, state_religious_establishments, beneficiary).

% Historians and theorists of Islamic law in universities who map how the schools formed, how consensus claims were constructed, and how authority is transmitted and contested. They hold no ruling authority in any community and answer to disciplinary norms rather than to the schools.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, academic_islamic_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interpretive-authority problem for a scriptural legal tradition: it fixes who may derive binding rulings from the Quran and Sunnah (credentialed jurists within the recognized schools), keeps a dispersed global community's practice synchronized across worship, contracts, marriage, and burial, and transmits the juristic corpus across generations through master-student chains so the tradition's answers remain available without each generation rederiving them.
% TRANSFER_FUNCTION: Moves deference and interpretive authority from lay Muslims to the credentialed scholarly class; moves status, employment, endowment income, and publishing flows to madhhab institutions and mosque hierarchies; moves the costs of the transmitted corpus, including unequal inheritance shares, discounted testimony in some schools, and dhimmi-derived minority status, onto women, dissenters, and minorities; and moves dissent itself into the sanction machinery of denied platforms, denied posts, and in some jurisdictions denied legal standing.
% ABSENT_VOICES: Reformist ijtihad jurists would object that the closure is neither historically accurate nor theologically required; they are excluded from the schools' councils, pulpits, and curricula and operate in parallel institutions. Women's advocacy coalitions would object to the personal-status positions the obligation holds settled; they are outside the councils that certify positions as consensus. Minority-sect representatives would object to the minority-status rules; they are not seated anywhere in the conversation. All three objections exist and are audible only outside the traditionalist structure.
% DISAPPEARANCE_RATIONALE: If the taqlid obligation vanished overnight, the scholarly class's interpretive monopoly would dissolve, since deference is the currency of its authority, and the madhhab institutions would lose their claim on students and endowments. Personal-status law in traditionalist-dominant jurisdictions would face immediate contestation, because the classical positions' immunity from rederivation is precisely what the obligation supplies. Religious authority would rearrange around competing interpretive claims from state councils, universities, individual scholars, and lay reading movements rather than remain with the schools. The world rearranges because the arrangement is load-bearing for religious authority, not because the scriptural corpus would change.
% FOUNDING_PROBLEM: After the Prophet's death, the community needed to determine God's law from the Quran and the Sunnah: who possesses the competence to derive rulings, how unqualified derivation is prevented, and how the community keeps one law rather than fragmenting into private interpretation. The madhhab consolidation answered this by institutionalizing method and credential; the taqlid obligation answered it by binding later generations to the schools' settled answers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: academic historians and theorists of Islamic law, working from the usul al-fiqh literature (the Hallaq and Weiss line of scholarship), attest that the interpretive-authority problem was real at consolidation and remains structurally present for any scriptural legal tradition; reformist jurists, themselves outside the benefiting structure, attest the problem is live while disputing that classical taqlid is the required answer. No party attests that the problem is dead; the dispute is over the solution, not the problem.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.68 at interval end) because the arrangement converts the scriptural corpus's interpretive openness into a closed monopoly: the deference economy routes authority, employment, and endowment income to the scholarly class, while the costs of the transmitted corpus (unequal inheritance shares, discounted testimony, dhimmi-derived minority status) land on those with the least exit. Suppression is higher still (0.78) because persistence depends on actively closing the alternative: credential gatekeeping decides who may derive law, congregational and familial sanction punishes those who try, takfir-adjacent accusation raises the stakes of dissent, and state apparatuses criminalize or delegitimize open ijtihad in traditionalist-dominant jurisdictions. The suppression figure is a raw structural measure of the machinery, unscaled by power or scope (only extraction is scaled, by directionality and scope, in the engine's computation); the internalized share of deference is handled by omega rather than folded into the scalar. Theater is moderate (0.30): teaching, adjudication, and fatwa work are real functions performed daily, but a growing share of the arrangement's activity is performative maintenance — invoking consensus where the schools' ikhtilaf is documented in their own literatures, ritualized citation of long-dead authorities, deference performed as piety. Accessibility collapse is moderate (0.55): the alternative of reopened ijtihad remains demonstrably available (reformist institutions, diaspora communities, and several states already operate outside full taqlid), but within traditionalist-dominant contexts the alternative is heavily foreclosed socially and sometimes legally. Resistance is substantial (0.58): reformist jurisprudence, feminist rereadings from Musa through Wadud to the Musawah coalition, and state-level family-law reform constitute sustained organized resistance — the coalition vector is the main reform path available to the powerless seats, and it is met by the arrangement's sanction machinery. The measurement series share one grid; the suppression_requirement series is authored because enforcement capacity is the dynamic this story traces — hardening through the mid-century Islamist turn, partially contested since, plateauing at a high level. Identity lock is central to enforcement capacity: for the ulama the fusion is institutional (the madhhab has become the scholar's authority; exit is self-dissolution) and professional (an ijaza career path of decades); for lay traditionalists it is ideological and relational (piety constituted through the adab of deference). If the ulama's identity frame broke, the constraint's enforcement capacity would drop sharply, because much of the enforcement is identity-carried rather than externally imposed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the ulama seat, the arrangement is the preservation of divine law and the community's unity: deference reads as piety, closure as fidelity, and the extraction is invisible from inside the identity that benefits from it. From the women's and minority seats, the same arrangement is the mechanism that keeps classical personal-status and dhimmi rules operative and forecloses the rederivation that would revise them — the coordination they experience is subordination. From the lay traditionalist seat it is genuinely both: real guidance and community, purchased with a reasoning license they have been formed never to want. The engine computes these per-seat types from the structural data; the divergence is the measurement the corpus exists to take, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations put the ulama, the madhhab networks, and the mosque hierarchies near the beneficiary end (low d): they collect authority, income, and standing and bear little of the corpus's cost. Identity lock cuts both ways for them — it cements the benefit position by making exit self-dissolving. Women and religious minorities are declared victims with powerless power and trapped exit: they sit near the full-target end, and the engine amplifies their effective extraction accordingly. Progressive reformers are organized but constrained — high directionality toward the target end, moderated only by their parallel institutional bases. Traditionalist lay communities are net beneficiaries with real payer costs (foreclosed derivation, and for the women among them the classical family rules), placing them mid-low rather than at either pole. State religious establishments add a second agenda-setting seat holding a beneficiary position: enforcement personnel who also collect from running the apparatus. The obligation's claimed scope is the global community, and the engine applies the scope modifier — a universal claim verified by a gatekeeping class amplifies what each trapped seat experiences. No directionality overrides are authored: the derivation from beneficiary/victim declarations plus power and exit produces the right relationships, and the dual-positioned seats (lay communities, state establishments) are expressed through secondary roles rather than override entries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who may derive binding law from the scriptural corpus, and how a community prevents unqualified derivation — is live, not dead: every reading of the kernel, including both siblings, is an answer to it, and the corroboration record attests the problem from outside the beneficiary set. So no mandatrophy is declared and none should be: the madhhab apparatus still performs its function daily (teaching, adjudication, fatwa), which is why theater stays moderate and the piton reading fails. The founding_problem_status x disappearance_verdict pair (live x world_rearranges) correctly produces no zombie flag. The tangled_rope classification prevents two opposite misreadings. Reading the arrangement as a pure snare would erase the coordination that hundreds of millions genuinely receive — shared practice, legal continuity, transmission of a civilization's juristic inheritance — and would mispredict the arrangement's resilience, which draws on real attachment and not only coercion. Reading it as a pure rope would erase the documented asymmetry: the same structure that coordinates the majority's practice fixes women's inheritance shares and minority status and treats the rederivation that would revise them as transgression. The hybrid classification keeps both facts on the books and lets the per-seat computation express who is coordinated and who pays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_locus,
    'This constraint is one reading of kernel quran_hadith_substrate (the traditionalist_taqlid reading). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compare the compiled sibling files (quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid) on victim sets, beneficiary sets, suppression, and epsilon. The disagreement''s location is fixed by the axioms: the binding force of established madhhab consensus, and the locus of interpretive authority.',
    'Under the reformist sibling the victim set shrinks (rederivation is licensed rather than sanctioned), the beneficiary set disperses (no interpretive monopoly), and epsilon falls. Under the state_hybrid sibling, enforcement re-grounds in political sovereignty and the scope partitions by legal domain. Classification of this file is unaffected — it is the traditionalist reading only — but cross-reading comparison inherits the delta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_locus, conceptual, 'Committer-frame omega: one of three readings of the Quran-Hadith kernel; records the structural delta the siblings would introduce and where the readings disagree.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of independent derivation structural (credential gatekeeping, congregational and state sanction) or internalized (deference formed as piety, so that dissent is experienced as presumption — ''who am I to judge the imams'')?',
    'Compare suppression trajectories across contexts where structural enforcement relaxed: diaspora communities, states that reformed family law, post-authority-collapse settings. If independent derivation flourishes where the machinery is removed, suppression was structural; if deference persists, a substantial internalized share remains.',
    'If internalized, effective suppression exceeds the structural 0.78 — targets carry the closure with them after exit, and reformist institutions underperform relative to their formal availability. If mostly structural, removing the machinery releases derivation quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression in the deference formation of lay communities.').

omega_variable(
    ijma_historical_reality,
    'Was the consensus the arrangement invokes historically real — did the classical schools actually agree on the corpus of rulings attributed to them as binding ijma — or is the consensus substantially constructed after the fact from school positions that diverged?',
    'Documentary comparison of the schools'' positive law across the classical period: where the madhhabs diverged (and the ikhtilaf literature records extensive divergence), trace how later authority literature re-presented the divergence as settled school consensus.',
    'If consensus is substantially constructed, the coordination claim weakens and the performative share rises — the current theater_ratio understates the arrangement, and the extraction share of the measured epsilon increases because the invoked consensus functions as authority theater rather than transmitted agreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_historical_reality, empirical, 'Historical status of the ijma claim that grounds the taqlid obligation.').

omega_variable(
    enforcement_context_variance,
    'The authored metrics average over radically different enforcement contexts — state-enforced traditionalist jurisdictions, socially enforced communities, diaspora settings where enforcement is familial only, and contested contexts. What is the arrangement''s structure in the modal traditionalist context versus the tails?',
    'Stratified measurement: compile separate epsilon and suppression estimates for the state-enforced, socially enforced, and diaspora strata rather than a single global figure, so per-seat computation can read stratum-specific directionality.',
    'Epsilon and suppression could swing roughly plus or minus 0.15 across strata; a state-enforced stratum computes nearer the snare boundary while a diaspora stratum computes nearer rope. The single-story figure is the communal average the manifest''s decomposition requested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_context_variance, empirical, 'Cross-context variance in enforcement intensity behind the averaged metrics.').

omega_variable(
    content_harm_vs_authority_extraction,
    'Does this arrangement''s epsilon include the downstream harms of the rulings it transmits (unequal inheritance, discounted testimony, dhimmi-derived minority status), or only the extraction of interpretive authority itself? The transmitted corpus''s content is arguably a separate constraint this arrangement protects.',
    'Counterfactual decomposition: where the madhhab corpus''s content has been revised by state family-law reform while the deference structure remained, measure what extraction remains — that residue is the authority-extraction component; the difference from the current figure is the content-harm component.',
    'If content harms dominate epsilon, the binding force of the taqlid obligation is the load-bearing element and reform should target closure; if authority extraction dominates, the gatekeeping structure itself is the primary object. Either way the family decomposition (content constraint versus authority constraint) should be made explicit in the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_harm_vs_authority_extraction, conceptual, 'Decomposition of epsilon into transmitted-content harms versus interpretive-authority rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 10, 0.22).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.24).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 30, 0.26).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.28).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 50, 0.29).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 50, 0.77).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal authority' covers three structurally distinct arrangements sharing one kernel (the Quran-Hadith substrate as binding legal source) but differing in epsilon, victim sets, and enforcement structure. This file is the traditionalist_taqlid member (epsilon 0.68; victims: reformers, women under classical personal status, minorities under dhimmi frameworks; enforcement: credential gatekeeping plus state apparatus). The reformist_ijtihad member carries lower epsilon (open derivation, dispersed authority) and the state_hybrid member carries a partitioned structure (classical rules where the state adopts them, sovereign legitimacy as the enforcement ground). The upstream/downstream structure runs through the corpus itself: the traditionalist reading maintains the classical corpus's authority that the state hybrid selectively draws on, while the reformist reading contests the closure the traditionalist reading enforces. Each family file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
