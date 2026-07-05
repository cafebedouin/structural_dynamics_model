% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the traditionalist_taqlid reading of the
 *   Quran-hadith substrate kernel: the claim that classical fiqh schools'
 *   consensus (ijma) is authoritative and binding, such that contemporary
 *   Muslims are religiously obligated to follow established madhhab rulings
 *   via taqlid rather than engaging in independent legal reasoning (ijtihad).
 *   This is presented within the tradition as fidelity to settled,
 *   correctly-derived religious law protecting the community from
 *   interpretive chaos and heterodoxy. Structurally, it functions as a
 *   hybrid: it does solve a real coordination problem (a common
 *   legal-religious vocabulary across a vast and otherwise fragmented
 *   community, reducing doctrinal conflict and providing predictable rulings
 *   for personal status, inheritance, and ritual practice) while
 *   simultaneously channeling interpretive authority, seminary credentialing,
 *   and enforcement capacity toward incumbent jurist hierarchies, at the
 *   direct expense of progressive reformers, women seeking revised family-law
 *   status, religious minorities governed under classical dhimmi frameworks,
 *   and independent jurists who lack institutional standing within a
 *   recognized madhhab.
 *
 * KEY AGENTS:
 *   - madhhab_jurist_hierarchies: Primary agenda-setter and beneficiary (institutional/arbitrage) — administers taqlid obligation, credentials interpreters, collects institutional authority and resources
 *   - mosque_institutional_networks: Secondary beneficiary (institutional/constrained) — enforces compliance at the community level, delivers rulings, depends on madhhab legitimacy for standing
 *   - state_religious_affairs_ministries: Institutional actor (institutional/mobile) — in many traditionalist-dominant states, codifies madhhab rulings into enforceable family and personal-status law
 *   - progressive_reformist_muslims: Primary target (moderate/constrained) — bears doctrinal exclusion, risk of takfir accusation, and loss of standing for advocating ijtihad-based reform
 *   - women_seeking_equal_family_law_status: Primary target (powerless/trapped) — bears concrete legal costs in inheritance, divorce, guardianship, and testimony rulings derived from classical fiqh
 *   - religious_minorities_under_classical_dhimmi_rules: Primary target (powerless/trapped) — bears reduced legal status where classical dhimmi frameworks remain codified
 *   - independent_jurists_outside_recognized_schools: Secondary target (moderate/constrained) — excluded from recognized interpretive authority regardless of scholarly merit
 *   - comparative_religious_historians: Analytical observer (analytical/analytical) — examines the historicity of ijma and the closure-of-ijtihad claim from outside all three contesting readings
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
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'f8a3f35e-3d59-434f-9115-f3183019d9b0').
narrative_ontology:cs_kernel_codification('f8a3f35e-3d59-434f-9115-f3183019d9b0', fixed_text).
narrative_ontology:cs_authority_grounding('f8a3f35e-3d59-434f-9115-f3183019d9b0', lineage).
narrative_ontology:cs_interpretation_layer_present('f8a3f35e-3d59-434f-9115-f3183019d9b0').
narrative_ontology:cs_reading_relation('f8a3f35e-3d59-434f-9115-f3183019d9b0', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('f8a3f35e-3d59-434f-9115-f3183019d9b0', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('f8a3f35e-3d59-434f-9115-f3183019d9b0', foundational, classical_ijma_permanently_binding).
narrative_ontology:cs_axiom_status(classical_ijma_permanently_binding, holdable).
narrative_ontology:cs_axiom_grounding('f8a3f35e-3d59-434f-9115-f3183019d9b0', classical_ijma_permanently_binding, conventional).
narrative_ontology:cs_axiom('f8a3f35e-3d59-434f-9115-f3183019d9b0', foundational, gates_of_independent_ijtihad_closed).
narrative_ontology:cs_axiom_status(gates_of_independent_ijtihad_closed, holdable).
narrative_ontology:cs_axiom_grounding('f8a3f35e-3d59-434f-9115-f3183019d9b0', gates_of_independent_ijtihad_closed, conventional).
narrative_ontology:cs_reference_frame('f8a3f35e-3d59-434f-9115-f3183019d9b0', classical_madhhab_consensus_settled).
narrative_ontology:cs_drift_state('f8a3f35e-3d59-434f-9115-f3183019d9b0', contemporary_pluralist_muslim_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f8a3f35e-3d59-434f-9115-f3183019d9b0', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_jurist_hierarchies).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_institutional_networks).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, state_religious_affairs_ministries).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, azhar_style_seminary_establishments).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_reformist_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_family_law_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_classical_dhimmi_rules).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, independent_jurists_outside_recognized_schools).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, ijma_of_classical_schools_is_binding).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__traditionalist_taqlid, gates_of_ijtihad_closed_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains, credentials, and ordains jurists within recognized schools; issues authoritative fatwas; determines who counts as a legitimate interpreter of the law. Their institutional standing, income, and social authority derive directly from the premise that classical consensus is binding and taqlid is obligatory. They can move between jurisdictions and adapt rhetorically without losing standing, giving them the most arbitrage-grade position of any seat.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_jurist_hierarchies, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Delivers rulings and religious instruction at the community level, drawing legitimacy from affiliation with a recognized madhhab. Depends on the taqlid framework for its own authority to adjudicate local disputes and issue guidance; would face a legitimacy vacuum if the binding-consensus premise were widely rejected.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_institutional_networks, beneficiary,
    institutional, generational, constrained, national).

% In many traditionalist-dominant states, codifies specific madhhab rulings into enforceable family, inheritance, and personal-status law, using taqlid's authority claim to legitimate state legal codes. Benefits from administrative simplicity and religious legitimacy; can shift emphasis between madhhabs or toward more state-centered legal reform if politically expedient.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_religious_affairs_ministries, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, state_religious_affairs_ministries, agenda_setter).

% Advocate contextual reinterpretation of classical rulings in light of contemporary ethics and human rights norms. Face social exclusion, accusations of deviance or apostasy, and professional risk within religious institutions for challenging the binding-consensus premise. Exit is possible (secular life, alternative religious communities, emigration) but costly to identity and community standing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_reformist_muslims, payer,
    moderate, biographical, constrained, global).

% Live under inheritance, divorce, guardianship, and testimony rulings derived from classical fiqh and often codified into state family law. Where classical rulings are legally binding, exit requires either emigration, informal circumvention at social risk, or waiting for state-level reform they cannot themselves initiate.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_family_law_status, payer,
    powerless, biographical, trapped, national).

% In jurisdictions where classical dhimmi frameworks remain codified or socially operative, bear reduced legal status, restricted worship or civic participation, and dependence on the majority community's tolerance. Have essentially no leverage to alter the doctrinal premise producing their status.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_classical_dhimmi_rules, payer,
    powerless, biographical, trapped, national).

% Possess scholarly training but lack affiliation with a recognized madhhab or seminary lineage; their independent legal reasoning is denied authoritative standing regardless of merit. Can publish or teach outside official channels but without institutional recognition or enforceable authority.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, independent_jurists_outside_recognized_schools, payer,
    moderate, biographical, constrained, regional).

% Study the historical formation of ijma claims and the timing of the 'closing of the gates of ijtihad' narrative relative to madhhab institutional consolidation, without a stake in any reading's doctrinal victory.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, comparative_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, transmissible legal-religious vocabulary across a geographically and culturally vast community, reducing doctrinal fragmentation and giving adjudicators, communities, and states a predictable reference point for personal-status, inheritance, ritual, and contractual law without requiring each generation or locality to re-derive rulings from first principles.
% TRANSFER_FUNCTION: Moves interpretive authority, credentialing power, and the capacity to define binding legal outcomes toward incumbent madhhab jurist hierarchies, mosque networks, and allied state religious ministries, and away from reformist scholars, independent jurists, and — most concretely — toward diminished legal standing for women and religious minorities in jurisdictions where classical rulings are codified into enforceable law.
% ABSENT_VOICES: Reformist scholars advocating ijtihad, women's rights advocates seeking revised inheritance and family law, and representatives of religious minorities living under dhimmi-derived legal status would object to the binding-consensus premise, but are structurally outside the credentialing and adjudicative bodies that determine which readings count as authoritative — their objections circulate in reformist scholarship and human-rights forums rather than within the recognized fiqh apparatus itself.
% DISAPPEARANCE_RATIONALE: If obligatory taqlid to classical madhhab consensus were no longer treated as binding, state family-law codes drawing on classical rulings would face immediate legitimacy pressure to reform, seminary credentialing systems would lose their exclusive claim to interpretive authority, independent and reformist jurists would gain standing they currently lack, and millions of women and religious minorities living under classically-derived personal-status law would gain grounds to contest their legal status. The rearrangement would be gradual rather than instantaneous given entrenched institutions, but the doctrinal foundation legitimating current arrangements would be gone.
% FOUNDING_PROBLEM: In the early centuries after the Prophet's death, the Muslim community faced genuine interpretive fragmentation as it expanded across diverse regions and cultures; a mechanism was needed to prevent proliferating, mutually contradictory individual legal rulings from destabilizing communal cohesion and religious authority.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist jurists and seminary institutions attest the founding problem remains live and the classical consensus remains the correct, permanently binding solution. Comparative religious historians outside the beneficiary set (drawing on textual and institutional-history evidence) attest that the 'closing of the gates of ijtihad' was substantially a later institutional consolidation narrative rather than an early substrate-mandated closure, and that the original coordination problem has since been transformed by conditions the classical jurists could not have anticipated — supporting a 'dead as originally posed, contested as currently invoked' reading from outside the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) reflects substantial but not maximal extraction: the coordination function (shared legal vocabulary, reduced doctrinal fragmentation) is genuine and non-trivial, which is why this is authored as tangled_rope rather than snare — but the extraction riding on that coordination (exclusion of reformist voices, codified inferior legal status for women and minorities in dhimmi-derived rulings) is substantial and institutionalized. Suppression (0.79) is high and rising across the measured interval because enforcement mechanisms — takfir/apostasy accusation risk, seminary gatekeeping, state codification of madhhab family law — have hardened rather than relaxed in many traditionalist-dominant jurisdictions over the modern period, even as global Muslim discourse has diversified. Theater ratio is comparatively low (0.28) because the doctrinal and institutional machinery (fatwa councils, madhhab-specific courts, seminary credentialing) performs real adjudicative work, not merely symbolic function — though a rising trajectory suggests some ritualization as the underlying consensus claim is increasingly contested by reformist scholarship.
 *
 * DIRECTIONALITY LOGIC:
 *   Madhhab jurist hierarchies sit at the full-beneficiary end: they administer the taqlid obligation, control credentialing, and their institutional authority is the direct product of the constraint's persistence. Mosque networks and state religious ministries benefit secondarily by deriving legitimacy and administrative simplicity from a settled doctrinal reference point. Women seeking equal family-law status and religious minorities under classical dhimmi rules sit near the full-target end: they are trapped by identity (born into communities where classical rulings are codified as personal-status law) and bear concrete, non-symbolic costs — inheritance shares, divorce asymmetries, testimony weighting, legal status tiers. Progressive reformist Muslims and independent jurists are constrained rather than trapped: some exit is possible (emigration to jurisdictions with reformist or secular family law, informal non-compliance) but at real social and sometimes legal cost, including apostasy risk in some jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing interpretive chaos and preserving a coherent, transmissible legal-religious tradition amid the early community's geographic and cultural expansion — was genuinely live in the classical period. Whether it remains live today is exactly the site of the kernel contest: the traditionalist reading holds the problem is permanently solved by settled consensus and remains binding; the reformist reading holds the problem has been transformed (the challenge is now reconciling inherited rulings with contemporary ethical and empirical realities the classical jurists could not have anticipated) such that the original solution has become an obstacle. This story does not adjudicate that dispute — it names the traditionalist reading's own position honestly: that ijma is treated as permanently settled, not as a historically contingent solution to a since-evolved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_traditionalist_vs_reformist,
    'Is the Quran-hadith substrate correctly read through obligatory taqlid to classical madhhab consensus, or does the same substrate support (or mandate) contextual ijtihad that revises classical rulings against contemporary ethics and maslaha?',
    'No empirical resolution exists within the tradition itself; the dispute is adjudicated by which interpretive community''s authority claim is accepted. Comparative jurisprudential history (whether ''closing the gates of ijtihad'' was itself a post-formative institutional consolidation rather than a substrate-mandated closure) is the主要 evidentiary terrain cited by both sides.',
    'If the reformist reading is correct, the traditionalist taqlid reading''s claim to represent the substrate''s authoritative meaning is itself the extractive move — obligatory taqlid becomes a jurisdictional capture dressed as fidelity. If the traditionalist reading is correct, reformist ijtihad is the deviation and this constraint is closer to a genuine coordination mechanism preserving doctrinal integrity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_traditionalist_vs_reformist, conceptual, 'The kernel is read three incompatible ways by three live parties; this omega records that this story is ONE such reading (traditionalist_taqlid), not an adjudication between them.').

omega_variable(
    closure_of_ijtihad_historicity,
    'Was the ''closing of the gates of ijtihad'' a genuine early consensus event, or a later institutional narrative retrofitted to consolidate madhhab authority once independent jurisprudence became inconvenient to established schools?',
    'Historical-critical analysis of the timing and authorship of closure claims relative to the institutional consolidation of the four Sunni madhhabs; cross-referencing with recorded ijtihad activity that continued well after the claimed closure date.',
    'If closure was retrofitted, obligatory taqlid''s foundational premise (that consensus already settled the law, foreclosing new interpretation) is itself constructed rather than discovered — strengthening the false-summit reading of this constraint''s mountain-adjacent claims to permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_of_ijtihad_historicity, empirical, 'Whether the doctrinal basis for mandatory taqlid rests on a historically accurate or a constructed consensus narrative.').

omega_variable(
    beneficiary_capture_vs_genuine_coordination,
    'Does obligatory taqlid solve a genuine coordination problem (preventing legal chaos from uncredentialed individual interpretation) that happens to also benefit incumbent authorities, or is the coordination function primarily cover for jurisdictional rent extraction by seminary and mosque hierarchies?',
    'Compare legal outcomes and doctrinal stability in communities with strong taqlid enforcement versus communities with recognized ijtihad practice — if coordination benefits (consistency, dispute reduction) are marginal relative to the exclusion of independent and reformist voices, extraction dominates.',
    'Determines whether this constraint is better modeled as tangled_rope (coordination + extraction coexisting) or should be reclassified toward snare if coordination benefit proves negligible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_coordination, empirical, 'Whether obligatory madhhab adherence''s coordination claim is substantive or primarily a cover story for institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.14).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 20, 0.17).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.2).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 60, 0.23).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.26).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(qura_be_t100, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'Quran-hadith substrate' claim per the ε-invariance principle. traditionalist_taqlid, reformist_ijtihad, and state_hybrid are structurally distinct readings of the same underlying textual kernel, each with a different authority-grounding claim, different beneficiary/victim structure, and a different ε. They are linked bidirectionally: this story's authority claim (settled classical consensus) directly forecloses/pressures the legitimacy conditions the sibling readings must argue against or route around.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
