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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Traditionalist Taqlid: Mandatory Adherence to Classical Madhhab Rulings via Ijma Authority
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This constraint instantiates the traditionalist reading of a contested
 *   kernel about the authority of the Quran-hadith substrate for contemporary
 *   Islamic legal practice. On this reading, the classical fiqh schools'
 *   consensus (ijma) is treated as closed and authoritative, and contemporary
 *   Muslims are obligated to follow established madhhab rulings through
 *   taqlid rather than independent reasoning. The story is authored strictly
 *   from within this reading: the ε, beneficiary, and victim structure
 *   describe how this specific commitment operates where it holds
 *   institutional force (traditionalist-dominant jurisdictions,
 *   madhhab-administered family courts, mosque hierarchies enforcing school
 *   conformity), not an average across readings and not the sibling readings'
 *   alternative arrangements.
 *
 * KEY AGENTS:
 *   - madhhab_institutions: agenda_setter (institutional/arbitrage) — administers and enforces the closed-consensus doctrine
 *   - ulama_class: beneficiary/agenda_setter (organized/identity_locked) — professional and identity stake in taqlid's continuation
 *   - women_seeking_equal_legal_status: payer (powerless/trapped) — bears inheritance, guardianship, divorce asymmetries
 *   - religious_minorities_under_dhimmi_frameworks: payer (powerless/trapped) — classified without standing to contest
 *   - reformist_scholars: excluded (moderate/constrained) — structurally denied platform within the framework
 *   - state_religious_affairs_ministries: observer/agenda_setter (institutional/analytical) — referees which reading gets state force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.79).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid: Mandatory Adherence to Classical Madhhab Rulings via Ijma Authority").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb').
narrative_ontology:cs_kernel_codification('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', fixed_text).
narrative_ontology:cs_authority_grounding('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', lineage).
narrative_ontology:cs_interpretation_layer_present('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb').
narrative_ontology:cs_reading_relation('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', foundational, ijma_closure_is_binding).
narrative_ontology:cs_axiom_status(ijma_closure_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', ijma_closure_is_binding, conventional).
narrative_ontology:cs_axiom('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', foundational, taqlid_obligatory_absent_scholarly_qualification).
narrative_ontology:cs_axiom_status(taqlid_obligatory_absent_scholarly_qualification, holdable).
narrative_ontology:cs_axiom_grounding('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', taqlid_obligatory_absent_scholarly_qualification, conventional).
narrative_ontology:cs_reference_frame('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', classical_ijma_closure).
narrative_ontology:cs_drift_state('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a901a4cd-ab5e-499d-8984-b2b6ffb5e1cb', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_class).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, religious_court_functionaries).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, muslims_in_cross_madhhab_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the canon of accepted rulings within a given school (Hanafi, Maliki, Shafi'i, Hanbali), trains and certifies jurists, and adjudicates which interpretations count as valid ijtihad versus impermissible deviation. Draws legitimacy, tuition revenue, and institutional continuity from the claim that classical consensus is closed and binding.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Scholars whose social standing, income, and interpretive monopoly depend on the taqlid framework remaining authoritative. They issue fatwas, staff religious courts, and act as gatekeepers to who may speak with legal authority on Islamic law. Their professional identity is constituted by mastery of and fidelity to the classical schools; independent ijtihad by non-credentialed actors threatens the value of that credential.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_class, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, ulama_class, agenda_setter).

% Local religious institutions that implement madhhab rulings in sermons, community adjudication, and social pressure. They benefit from a stable, unquestioned doctrinal package that requires no local theological labor to defend and that reinforces their role as the community's interpretive authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, generational, constrained, regional).

% Seek to apply contextual reasoning to inheritance, family law, or ritual practice in ways that depart from classical rulings. Face social excommunication, accusations of heresy or apostasy, denial of religious burial rites, and exclusion from mosque leadership if they act on reformist conclusions. Formal exit (leaving the faith community) carries severe social and sometimes legal costs; remaining while dissenting invites ongoing sanction.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, national).

% Bear the direct weight of classical rulings on inheritance shares, testimony weighting, guardianship (wilayah) in marriage, unilateral male divorce (talaq), and polygyny permissions where these are enforced through state-backed religious courts or communal pressure. In jurisdictions where family law is delegated to religious authorities, they have no secular forum to appeal to and often lack the economic independence to exit the marriage or community entirely.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status, payer,
    powerless, biographical, trapped, national).

% In jurisdictions or communities where classical dhimmi doctrine is invoked, bear differential legal status, taxation, or restrictions historically justified by classical consensus. Have no standing within the taqlid framework itself to contest the rulings that classify them, since the framework by design does not treat their objection as a source of law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi_frameworks, payer,
    powerless, generational, trapped, national).

% Navigate conflicting rulings when spouses or communities follow different schools, particularly on divorce validity, inheritance division, or ritual obligations. The taqlid requirement to follow one's own school rigidly, rather than adjudicate on the merits of the specific case, produces friction that falls disproportionately on the more legally vulnerable spouse, typically the wife.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, muslims_in_cross_madhhab_marriages, payer,
    moderate, biographical, constrained, regional).

% Argue that ijtihad should remain open and that classical rulings reflect the social conditions of their formation rather than eternal consensus. Are frequently denied platforms within mosque and madrasa institutions, labeled deviant by ulama boards, and in some jurisdictions face legal or extralegal sanction for public advocacy of reinterpretation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, biographical, constrained, national).

% Governments that certify which madhhab rulings apply in state-administered family courts, license preachers, and referee disputes between traditionalist and reformist religious actors. They observe the contest between readings but frequently have their own reasons (political legitimacy, minority-community management) for reinforcing or relaxing taqlid's binding force.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_religious_affairs_ministries, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, state_religious_affairs_ministries, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, widely recognized body of legal reasoning (fiqh) so that ordinary believers, judges, and communities do not need to independently derive rulings on marriage, inheritance, worship, and commerce from scripture each time a question arises; reduces interpretive chaos and forum-shopping within a given school.
% TRANSFER_FUNCTION: Moves interpretive authority and the social/legal capital that comes with it from individual believers and rival interpretive communities to certified madhhab scholars and the institutions that train and credential them; moves economic and legal certainty away from women, religious minorities, and reformist dissenters and toward those whose status depends on classical rulings remaining binding.
% ABSENT_VOICES: Reformist scholars and progressive Muslims are present but structurally out-voted within taqlid institutions since the framework does not recognize contemporary ijtihad as a legitimate source of authority; women affected by guardianship and inheritance rulings are rarely seated on the juristic councils that interpret those rulings; religious minorities classified under classical dhimmi doctrine have no standing to contest their own classification from within the framework.
% DISAPPEARANCE_RATIONALE: If taqlid's binding force disappeared overnight, religious courts relying on classical madhhab rulings for family law would lose their doctrinal warrant, ulama credentialing would lose its exclusivity value, and millions of individual legal outcomes (inheritance division, divorce validity, guardianship) currently fixed by school affiliation would become open to renegotiation through either state civil law or individual ijtihad — a substantial rearrangement of religious, legal, and social institutions in traditionalist-majority contexts.
% FOUNDING_PROBLEM: In the first few centuries after the Prophet's death, jurists faced the problem of preventing interpretive chaos and unqualified individuals from issuing authoritative religious-legal rulings; taqlid and the madhhab system solved this by consolidating accumulated juristic reasoning into transmissible, teachable schools with clear chains of authority.
% FOUNDING_PROBLEM_CORROBORATION: Ulama and madhhab institutions attest the founding problem (interpretive chaos, unqualified rulings) remains live and that taqlid still prevents it. Independent historians of Islamic law and reformist scholars from outside the beneficiary class attest that the original problem of establishing baseline scholarly competence was substantially solved by the classical period itself, and that continued mandatory taqlid now functions primarily to preserve institutional authority rather than to prevent a live epistemic emergency; some state religious-affairs bodies corroborate the reformist reading when it serves administrative convenience, but do not do so consistently.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored high (0.68 by interval end) because the doctrinal claim of closed consensus functions to transfer interpretive authority and its attendant social/legal capital toward madhhab institutions and the ulama, at direct cost to those governed by rulings on inheritance, guardianship, divorce, and minority status who have no standing within the framework to contest them. Suppression is authored higher still (0.79) because the constraint's persistence in traditionalist-dominant contexts depends on active exclusion of ijtihad-based alternatives — denial of platforms to reformist scholars, social and sometimes legal sanction for dissent — not on voluntary preference alone. Theater is moderate (0.32) and rising: a meaningful share of ulama activity is genuine legal-educational function, but a growing share defends doctrinal closure itself against reformist pressure rather than serving disputants' substantive needs. This rising trajectory reflects intensifying reformist challenge over the twentieth and twenty-first centuries met with hardening institutional defense, not a stable equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Madhhab institutions and the ulama sit near the full-beneficiary end: they set the terms, are credentialed and paid through the framework, and have mobile or arbitrage-level exit (a scholar can move between institutional roles without losing standing). Women seeking equal legal status and religious minorities sit near the full-target end: trapped exit options, no standing within the framework's own logic to contest their treatment, and the extraction (unequal inheritance shares, guardianship subordination, differential minority status) flows directly from the doctrinal claim being enforced as binding. Progressive Muslims and cross-madhhab spouses sit in between — constrained but not fully trapped, since some avenues (state civil courts, emigration, quiet non-compliance) exist, at real social cost. Reformist scholars are structurally excluded rather than coordinated: their objection is not weighed inside the framework's decision procedure at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing interpretive chaos and unqualified legal rulings in the early Islamic centuries — was plausibly solved by the maturation of the classical schools themselves centuries ago. This reading nonetheless treats the closure of ijtihad as still binding today, which is exactly the founding-problem-status mismatch (status: contested, tending toward dead outside the beneficiary class, with disappearance_verdict: world_rearranges) that flags a live mandatrophy candidate: an arrangement whose original coordination problem has been substantially resolved but whose authority structure persists and continues to bind third parties who had no part in resolving the original problem. Classifying this as tangled_rope rather than pure snare preserves the real coordination value the schools provide (legal predictability, trained expertise, protection against unqualified fatwa-issuing) while still registering the asymmetric extraction falling on women, minorities, and dissenters — collapsing it to snare would erase the genuine coordination function; collapsing it to rope would erase the documented victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closed_ijma_vs_reformable_consensus,
    'Is the classical ijma genuinely closed and binding for all time, as the traditionalist reading holds, or was ''closure'' itself a later juristic construction contingent on historical conditions that no longer obtain?',
    'Historical-critical scholarship on the actual practice of ijtihad in the first several Islamic centuries, compared against later claims of consensus closure; comparative study of how other legal traditions handle stare decisis versus periodic re-derivation.',
    'If closure is itself a historically contingent construction rather than an inherent feature of the sources, the traditionalist reading''s core legitimating claim weakens substantially, supporting reclassification of this constraint''s coordination component as smaller relative to its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closed_ijma_vs_reformable_consensus, conceptual, 'Whether ijma closure is an inherent feature of the sources or a later institutional construction.').

omega_variable(
    reading_selection_and_committer_structure,
    'This story deliberately instantiates only the traditionalist_taqlid reading of the quran_hadith_substrate kernel; the reformist_ijtihad and state_hybrid readings are separate constraints with their own ε and party structures. Which reading a given Muslim-majority jurisdiction or community actually operates under is itself contested and can shift.',
    'Track which reading has de facto institutional force in a given jurisdiction over time (e.g., codification of family law, composition of religious court benches, state licensing of preachers) as a way of determining which constraint file is empirically operative for that population at that time.',
    'A jurisdiction moving from traditionalist_taqlid toward state_hybrid or reformist_ijtihad would see this constraint''s real-world extraction fall toward the sibling constraints'' lower ε values without any change to this file''s own authored ε — because this file is a fixed reading, not a moving average.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_and_committer_structure, conceptual, 'Committer-structure note: this constraint is one fixed reading among three; which reading applies is an empirical, shifting fact about a given context.').

omega_variable(
    ulama_capture_vs_genuine_expertise,
    'How much of the ulama class''s defense of mandatory taqlid reflects genuine concern about interpretive competence versus self-interested defense of professional and institutional monopoly?',
    'Compare ulama positions on ijtihad across cases where their institutional interests do and do not align with the outcome (e.g., rulings that would reduce clerical court fees or credentialing requirements versus rulings that would not).',
    'A high self-interest component would support weighting the beneficiary/extraction reading more heavily; a low component would support crediting more of the theater_ratio to genuine, non-extractive doctrinal caution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ulama_capture_vs_genuine_expertise, empirical, 'Whether ulama resistance to reformist ijtihad is primarily principled or primarily self-interested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.14).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.19).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.24).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.28).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 100, 0.32).

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
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'contested authority of the Quran-hadith substrate for contemporary Islamic law' per the epsilon-invariance principle. traditionalist_taqlid (this file, ε=0.68, tangled_rope) claims closed classical consensus is binding via taqlid. reformist_ijtihad claims contextual reinterpretation is obligatory when classical rulings conflict with contemporary ethics or maslaha. state_hybrid claims the state selectively adopts classical rulings in some domains (family, criminal) while applying secular/reformist frameworks elsewhere, grounded in political sovereignty. Each carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because measuring the kernel by 'is mandatory taqlid binding' versus 'is contextual reinterpretation obligatory' versus 'does the state hybridize sources' yields structurally different extraction profiles, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
