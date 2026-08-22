% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Reading of the Quran-Hadith Interpretive Substrate
 *   domain: islamic_jurisprudence_legal_theory_religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the reformist ijtihad reading of the
 *   contested Quran-hadith interpretive kernel: the claim that classical
 *   rulings which conflict with contemporary ethics, human rights norms, or
 *   maslaha must be revisited through contextual ijtihad, prioritizing the
 *   Quran's broader ethical trajectory over literalist application of
 *   hadith-derived rulings. Where this reading gains institutional backing
 *   (sympathetic courts, reform-aligned muftis, transnational NGO advocacy),
 *   it functions as a genuine coordination mechanism letting observant
 *   Muslims reconcile faith commitments with contemporary norms — but it also
 *   displaces the authority base of madhhab-aligned clergy, fatwa councils,
 *   and seminary networks whose legitimacy depends on an interpretive
 *   monopoly the reformist reading structurally denies. This is not the same
 *   constraint as the traditionalist taqlid reading (which authors near-zero
 *   extraction against classical authority, since taqlid IS its own
 *   legitimacy standard) or the state-hybrid reading (which authors
 *   extraction differently, indexed to selective political adoption). Each
 *   reading has its own epsilon, its own beneficiary/victim structure, and
 *   its own type; they are linked here as siblings in the kernel network, not
 *   merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.38).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Reading of the Quran-Hadith Interpretive Substrate").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence_legal_theory_religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, 'ea6d9394-1903-4788-85ad-e0b93c6194c6').
narrative_ontology:cs_kernel_codification('ea6d9394-1903-4788-85ad-e0b93c6194c6', distributed).
narrative_ontology:cs_authority_grounding('ea6d9394-1903-4788-85ad-e0b93c6194c6', expertise).
narrative_ontology:cs_interpretation_layer_present('ea6d9394-1903-4788-85ad-e0b93c6194c6').
narrative_ontology:cs_reading_relation('ea6d9394-1903-4788-85ad-e0b93c6194c6', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('ea6d9394-1903-4788-85ad-e0b93c6194c6', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('ea6d9394-1903-4788-85ad-e0b93c6194c6', foundational, contextual_reinterpretation_obligatory_on_conflict).
narrative_ontology:cs_axiom_status(contextual_reinterpretation_obligatory_on_conflict, holdable).
narrative_ontology:cs_axiom_grounding('ea6d9394-1903-4788-85ad-e0b93c6194c6', contextual_reinterpretation_obligatory_on_conflict, instrumental).
narrative_ontology:cs_axiom('ea6d9394-1903-4788-85ad-e0b93c6194c6', foundational, quranic_ethical_trajectory_supersedes_literalist_hadith_application).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_literalist_hadith_application, holdable).
narrative_ontology:cs_axiom_grounding('ea6d9394-1903-4788-85ad-e0b93c6194c6', quranic_ethical_trajectory_supersedes_literalist_hadith_application, conventional).
narrative_ontology:cs_reference_frame('ea6d9394-1903-4788-85ad-e0b93c6194c6', classical_ijma_as_provisional_scholarly_consensus).
narrative_ontology:cs_drift_state('ea6d9394-1903-4788-85ad-e0b93c6194c6', post_human_rights_era_globalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea6d9394-1903-4788-85ad-e0b93c6194c6', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_family_law_reform).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reform_oriented_islamic_ngos).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, madhhab_aligned_clergy).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_fatwa_councils).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_seminary_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce new fiqh reasoning that reopens questions classical schools treated as closed, arguing the Quran's ethical trajectory (justice, mercy, human dignity) should override literalist hadith application where the two conflict. They administer the interpretive method itself — deciding what counts as valid maslaha reasoning and which classical rulings are eligible for revision. Their exit from the broader Islamic tradition is not real; they seek legitimacy within it, not outside it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, beneficiary).

% Rely on reformist rulings to contest classical positions on divorce initiation, guardianship, inheritance, and testimony weight. Where reformist ijtihad has institutional backing (a sympathetic court, a reform-aligned mufti), they gain real legal remedies; where traditionalist structures still control family courts, the reformist reading remains aspirational. Exit to secular courts is often unavailable or socially costly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_family_law_reform, beneficiary,
    moderate, biographical, constrained, national).

% Depend on reformist reinterpretation to argue that classical criminalizing rulings reflect historically contingent hadith application rather than binding Quranic ethics. Benefit exists mostly at the level of theological argument and diaspora community formation; in states where traditionalist or state-hybrid readings control criminal codes, the reformist reading offers no enforceable protection and exit from the underlying religious community is often not psychologically or socially available.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, trapped, national).

% Benefit indirectly when reformist maslaha-based reasoning is used to argue for pluralistic citizenship rights over classical dhimmi-status jurisprudence. Their standing depends entirely on which reading the state or the dominant clerical establishment adopts; they have no direct voice in the ijtihad debate itself.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_under_muslim_majority_states, beneficiary,
    powerless, biographical, constrained, national).

% Their scholarly authority and institutional position rest on the claim that classical school rulings, transmitted via taqlid, represent settled consensus not subject to individual reinterpretation. Reformist ijtihad directly threatens this by asserting that any qualified reasoner may override established rulings when they conflict with contemporary ethics. Their identity and career are fused to the interpretive monopoly; conceding the reformist premise dissolves the basis of their authority, not merely its application.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, madhhab_aligned_clergy, payer,
    institutional, generational, identity_locked, national).

% Issue binding or quasi-binding rulings under the assumption that classical consensus settles most contested questions. Reformist ijtihad's spread erodes their gatekeeping function over what counts as authoritative Islamic law, and where state or civil society actors defer to reformist scholars instead, the councils lose adjudicative relevance and revenue from fatwa-issuing services.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_fatwa_councils, payer,
    institutional, generational, constrained, national).

% Train the next generation of jurists in classical methodology and depend on taqlid's authority for curriculum legitimacy and student enrollment. Reformist ijtihad's institutional gains threaten the pipeline that sustains these networks financially and reputationally; they respond with counter-mobilization, fatwas against reformist scholars, and lobbying for state suppression of reformist curricula.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_seminary_networks, payer,
    organized, generational, constrained, regional).

% Fund, publish, and internationally circulate reformist scholarship, and lobby international human rights bodies to cite reformist readings as authoritative Islamic positions on gender and minority rights. They can shift jurisdictions and platforms more freely than local scholars, giving them a form of interpretive arbitrage the domestic actors lack.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reform_oriented_islamic_ngos, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, reform_oriented_islamic_ngos, agenda_setter).

% Decide how much institutional space to grant reformist ijtihad relative to traditionalist and state-hybrid readings, often instrumentally — adopting reformist positions where they serve modernization or international legitimacy goals, and traditionalist positions where they serve alliance with clerical establishments. Not committed to any reading on doctrinal grounds.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_religious_affairs_ministries, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_religious_affairs_ministries, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a method for resolving genuine tension between classical ruling texts (many produced under premodern social conditions) and contemporary ethical commitments (human rights norms, gender equality, pluralism) without requiring wholesale abandonment of the Quran-hadith textual tradition — it lets observant Muslims retain religious identity while revising specific applications.
% TRANSFER_FUNCTION: Moves interpretive authority from madhhab-credentialed clergy and fatwa councils toward individually or institutionally validated reformist scholars, and moves practical legal and social outcomes (custody rulings, criminalization exposure, minority legal standing) toward outcomes more aligned with contemporary human-rights framing, away from classical-ruling-derived outcomes.
% ABSENT_VOICES: Ordinary lay Muslims who neither identify as reformist nor traditionalist, and who experience the ijtihad contest mainly as instability in what authoritative religious guidance says, are largely absent from the scholarly debate itself. Rural and non-elite communities without access to reformist scholarship networks are effectively excluded from benefiting even where reformist rulings exist on paper.
% DISAPPEARANCE_RATIONALE: If the mandate for contextual ijtihad vanished and traditionalist taqlid became uncontested, family courts operating under reformist precedent would revert, reform-oriented NGOs would lose their doctrinal basis for advocacy, progressive scholars would lose institutional standing, and LGBTQ and minority communities would lose even the aspirational theological cover the reformist reading currently provides. Conservative seminary networks and fatwa councils would regain uncontested authority.
% FOUNDING_PROBLEM: Classical fiqh rulings, many formulated in 8th-14th century social and political contexts, increasingly conflict with contemporary human rights norms, gender equality commitments, and pluralistic citizenship models that many Muslims — especially in diaspora and reform-minded majority-Muslim contexts — hold as binding ethical commitments alongside their faith.
% FOUNDING_PROBLEM_CORROBORATION: Human rights bodies, comparative legal scholars outside the Islamic scholarly tradition, and dissenting voices within conservative seminaries (some of whom privately acknowledge the tension even while publicly defending taqlid) corroborate that the underlying conflict between classical rulings and contemporary norms is real and unresolved, not manufactured by reformist advocates. However, traditionalist scholars dispute that ijtihad is the correct or permissible resolution mechanism, arguing the tension should be resolved through contextualized taqlid rather than individual reinterpretation.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42, moderate band) reflects that reformist ijtihad genuinely displaces material authority, revenue, and legitimacy from traditional clergy and fatwa councils — this is a real transfer, not merely rhetorical. It rises modestly over the interval (0.28 to 0.42) as reformist scholarship gains institutional footholds in courts and diaspora communities. Suppression is comparatively low and falling (0.50 to 0.38) because reformist ijtihad's own doctrine explicitly rejects suppressing alternative readings — its coordination premise depends on interpretive pluralism, so as it gains ground it structurally cannot close off traditionalist argument without contradicting itself. Resistance is high (0.72) because conservative seminary networks and fatwa councils actively counter-mobilize; this is the expected dynamic for a reading vulnerable to traditionalist backlash. Accessibility collapse is moderate (0.40): where reformist readings have institutional backing, alternatives narrow for those actors, but classical taqlid remains fully available and practiced elsewhere.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars, women seeking family law reform, LGBTQ Muslims, and religious minorities are declared beneficiaries because the reformist reading's entire coordination function is to produce outcomes more favorable to them than classical literalist application would. Madhhab-aligned clergy, fatwa councils, and seminary networks are declared victims because their institutional legitimacy is directly constituted by the interpretive monopoly the reformist reading denies — this is a genuine structural cost, not incidental discomfort. State religious affairs ministries are treated as an observer/agenda-setter hybrid rather than a committed beneficiary or victim: their adoption of the reformist reading is instrumental to modernization or legitimacy goals, not doctrinal conviction, so their directionality is closer to symmetric/analytical than either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure rope or pure snare prevents two mislabelings. First, it prevents treating reformist ijtihad as pure benign coordination (rope) when it demonstrably displaces real institutional power and revenue from traditional authorities — that displacement is a genuine cost borne by an identifiable party, not a victimless efficiency gain. Second, it prevents treating it as pure extraction (snare) when it does solve a real and independently corroborated coordination problem: the live tension between premodern ruling texts and contemporary ethical commitments that many observant Muslims hold simultaneously. Both the coordination function and the asymmetric cost to traditional authority are real and coexist in the same structure, which is exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijtihad_qualification_gatekeeping,
    'Who is authorized to perform valid contextual ijtihad, and does the reformist reading itself reconstitute a new interpretive gatekeeping structure (credentialed reformist scholars, reform-aligned institutions) rather than genuinely democratizing interpretation?',
    'Track whether reformist ijtihad outcomes remain accessible to lay reasoning or become concentrated in a small credentialed reformist elite whose institutional position mirrors the traditionalist structure it displaces.',
    'If reformist authority concentrates similarly to traditional taqlid authority, the reading''s own extraction profile would need re-assessment upward, converging structurally with the tangled_rope pattern it currently only partially exhibits via displaced traditional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijtihad_qualification_gatekeeping, empirical, 'Whether reformist ijtihad reconstitutes rather than dissolves interpretive gatekeeping.').

omega_variable(
    sibling_reading_coexistence_or_erosion,
    'Does the spread of reformist ijtihad in some jurisdictions structurally erode the traditionalist_taqlid reading''s viability elsewhere (via transnational scholarly networks, diaspora influence, international human rights citation), or do the readings persist as genuinely independent, geographically bounded practices?',
    'Compare traditionalist fatwa council authority and enrollment trends in jurisdictions with strong reformist institutional presence versus jurisdictions without it, controlling for other modernization variables.',
    'If reformist gains genuinely erode traditionalist legitimacy even where traditionalist institutions remain formally dominant, the reading_relations edge to traditionalist_taqlid may warrant reclassification from coexists_with toward influences with stronger downstream pressure than currently modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_or_erosion, empirical, 'Whether reformist and traditionalist readings are structurally independent or one erodes the other.').

omega_variable(
    maslaha_reasoning_boundary_ambiguity,
    'Is there a principled, contestable-in-good-faith boundary on what counts as valid maslaha (public interest) reasoning, or does the concept''s flexibility allow the reformist reading to justify essentially any outcome a reform-minded scholar prefers, undermining its claim to be a disciplined interpretive method rather than a proxy for external secular-liberal commitments?',
    'Examine whether reformist scholars ever produce maslaha-based rulings that cut against progressive preferences (a genuine test of the method''s independence from outcome-driven reasoning), versus a pattern where maslaha invariably tracks contemporary liberal-rights positions.',
    'If maslaha reasoning shows no independent constraining power, the coordination-function claim weakens and the reading looks more like an extraction/legitimation vehicle for externally-derived commitments dressed in Islamic legal vocabulary, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_reasoning_boundary_ambiguity, conceptual, 'Whether maslaha functions as a disciplined method or as post-hoc justification for predetermined outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 8, 0.13).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 16, 0.16).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 32, 0.38).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept of 'how Islamic law relates to classical rulings' into structurally distinct constraints sharing one kernel (quran_hadith_substrate). reformist_ijtihad (this file) authors moderate extraction (~0.42) with falling suppression, reflecting a reading whose coordination function is real but whose spread displaces traditional clerical authority. traditionalist_taqlid authors near-zero extraction from its own vantage (taqlid IS the legitimacy standard it measures itself against) but would author high extraction from a reformist observer's vantage — which is exactly why it is a separate story rather than a parameter on this one. state_hybrid authors extraction indexed to selective political adoption rather than doctrinal conviction, with legitimacy grounded in sovereignty rather than either pure doctrinal position. All three link via affects_constraints; none averages or is measured 'the other way' inside a single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
