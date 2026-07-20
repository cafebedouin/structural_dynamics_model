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
 *   human_readable: Classical Madhhab Authority via Obligatory Taqlid
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This constraint instantiates the traditionalist_taqlid reading of the
 *   quran_hadith_substrate kernel. It treats the classical fiqh schools
 *   (Hanafi, Maliki, Shafi'i, Hanbali) as embodying authoritative consensus
 *   (ijma) and obligates contemporary Muslims who are not qualified mujtahids
 *   to follow established madhhab rulings via taqlid. The constraint
 *   coordinates a global community around a shared legal framework, but it
 *   also asymmetrically extracts interpretive autonomy from lay believers,
 *   women, and minorities while concentrating authority, status, and material
 *   support in traditional ulama and madhhab institutions. The high
 *   suppression score reflects the institutional marginalization of reformist
 *   ijtihad and the theological delegitimation of progressive readings.
 *
 * KEY AGENTS:
 *   - Traditional ulama: Primary agenda-setter (institutional/arbitrage) â certifies legitimate opinion and captures the deference and funding that flow to recognized authority.
 *   - Madhhab institutions: Primary beneficiary (institutional/constrained) â maintains the classical corpus and certification pipelines that sustain the authority structure.
 *   - Mosque hierarchies: Secondary beneficiary (organized/constrained) â propagates taqlid obligation and enforces conformity through community-level social pressure.
 *   - Lay Muslims: Payer (moderate/constrained) â surrenders interpretive autonomy in exchange for coherence; bears the constraint's primary coordination cost.
 *   - Women seekers of equal status: Payer (powerless/identity_locked) â bears the asymmetric cost of classical gender rulings; exit blocked by identity fusion.
 *   - Religious minorities: Payer (powerless/trapped) â subject to classical dhimmi frameworks with no viable alternative legal authority.
 *   - Reformist scholars: Excluded (moderate/constrained) â advocates ijtihad but structurally barred from authoritative interpretive venues.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.72).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.8).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.72).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Classical Madhhab Authority via Obligatory Taqlid").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'eb9e4024-5cd9-4ea5-819f-1b26dff039f8').
narrative_ontology:cs_kernel_codification('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', fixed_text).
narrative_ontology:cs_authority_grounding('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', lineage).
narrative_ontology:cs_interpretation_layer_present('eb9e4024-5cd9-4ea5-819f-1b26dff039f8').
narrative_ontology:cs_reading_relation('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', foundational, classical_madhhab_binding_authority).
narrative_ontology:cs_axiom_status(classical_madhhab_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', classical_madhhab_binding_authority, theological).
narrative_ontology:cs_axiom('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', foundational, taqlid_obligatory_for_non_mujtahids).
narrative_ontology:cs_axiom_status(taqlid_obligatory_for_non_mujtahids, holdable).
narrative_ontology:cs_axiom_grounding('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', taqlid_obligatory_for_non_mujtahids, theological).
narrative_ontology:cs_reference_frame('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', classical_madhhab_consensus).
narrative_ontology:cs_drift_state('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb9e4024-5cd9-4ea5-819f-1b26dff039f8', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seekers_equal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certify, transmit, and interpret classical fiqh rulings; adjudicate legitimacy of legal opinions; train jurists in madhhab methodology; receive social deference, institutional funding, and state recognition as the authorized religious voice.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, traditional_ulama, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Maintain and publish authoritative editions of classical texts; operate certification programs for muftis and qadis; manage endowments and international funding streams that depend on the perception of unbroken madhhab continuity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Propagate the duty of taqlid through Friday sermons, religious education, and community counseling; enforce conformity through social pressure and by filtering which scholars and texts are accessible to congregants.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, generational, constrained, national).

% Obligated to follow qualified jurists without independent investigation of scripture; surrender personal interpretive autonomy in exchange for a coherent religious legal framework and communal belonging; bear the social cost of deviation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, lay_muslims, payer,
    moderate, biographical, constrained, local).

% Seek to reconcile Islamic identity with contemporary ethics and human rights norms; marginalized by institutional gatekeeping; labeled deviant when their conclusions depart from classical madhhab positions on gender, governance, or interfaith relations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, national).

% Subject to classical fiqh rulings on marriage, divorce, inheritance, and testimony that deny equal legal agency; identity fusion between religious commitment and gendered community membership makes exit psychologically and socially prohibitive.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seekers_equal_status, payer,
    powerless, biographical, identity_locked, national).

% Governed by classical dhimmi frameworks derived from traditional fiqh; bear second-class legal status in family and civil matters in traditionalist-dominated jurisdictions; lack recourse to alternative legal authorities.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Advocate contextual ijtihad and direct engagement with scripture outside madhhab boundaries; systematically excluded from prestigious seminaries, state fatwa councils, and mainstream mosque platforms; treated as illegitimate by traditionalist gatekeepers.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_scholars, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of interpreting divine law for a global, linguistically diverse, religiously committed community without requiring every believer to master classical Arabic, usul al-fiqh, and the massive hadith corpus; provides a stable, predictable legal framework that preserves communal coherence across time and space.
% TRANSFER_FUNCTION: Moves interpretive authority and legal-determination power from individual believers and local communities to certified ulama and madhhab institutions; moves social deference, institutional funding, and state recognition from lay communities and states to the religious hierarchy in exchange for rendered fatwas and legitimacy.
% ABSENT_VOICES: Reformist scholars advocating contextual ijtihad, women legal theorists contesting classical gender rulings, religious minorities seeking full legal equality, and secular Muslims who reject taqlid obligation but remain within the broader community are structurally excluded from authoritative interpretive venues.
% DISAPPEARANCE_RATIONALE: If the obligation to follow established madhhab rulings via taqlid dissolved, individual Muslims would reclaim or redirect interpretive agency, the traditional ulama would lose their monopoly on legitimate legal opinion, madhhab institutions would face funding and enrollment collapse, and the social architecture of deference that sustains mosque hierarchies would fragment.
% FOUNDING_PROBLEM: The early Muslim community faced interpretive chaos (ikhtilaf) as divine scripture encountered unprecedented local contexts; without authorized interpreters, the community risked sectarian fragmentation, inconsistent practice, and lay error in matters of divine obligation.
% FOUNDING_PROBLEM_CORROBORATION: Traditional ulama attest the problem remains live due to lay incapacity and the danger of unguided interpretation. Academic historians and reformist scholars attest the founding problem was real but has been superseded by mass literacy, textual availability, and legal education; they argue the institutional solution has ossified into power preservation. Modern Muslim-majority states with hybrid or code-law systems provide empirical evidence that reduced taqlid does not necessarily cause communal chaos.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.72) is high because the constraint moves substantial interpretive authority and legal control from the many to the few; suppression (0.80) is higher because the arrangement's persistence depends on actively excluding rival interpretive methodologies (reformist ijtihad, individual reasoning) and maintaining gatekeeping institutions. Theater ratio (0.50) is moderate-high: classical fiqh contains genuine legal content, but a growing share of institutional activity is performative maintenance of authority rather than responsive legal reasoning. Accessibility collapse (0.70) reflects that once inside the traditionalist epistemic framework, alternatives appear theologically illegitimate. Resistance (0.45) is moderate: reformist movements are persistent but institutionally marginalized. The measurement series show a rising trajectory over two centuries as colonial disruption gave way to post-colonial state co-optation, petrodollar institutional funding, and digital consolidation of fatwa authority.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (traditional ulama) experiences the constraint as a sacred trust and necessary bulwark against interpretive chaos; the engine will compute this seat near the beneficiary end. The payer seats (women, minorities, reformists) experience the same structure as an enforced hierarchy that denies them standing and legal equality; the engine will compute these seats near the target end. Lay Muslims sit closer to symmetric: they receive genuine coordination (stable legal framework) but pay with surrendered autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (traditional_ulama, madhhab_institutions, mosque_hierarchies) drive directionality toward the subsidy end for those agents. Victim declarations (progressive_muslims, women_seekers_equal_status, religious_minorities) drive directionality toward the full-target end. The divergence is structural: the same text and institution that coordinates the community also concentrates interpretive power. Exit options amplify the split: ulama have arbitrage-grade exit (can relocate to favorable jurisdictions), while religious minorities are trapped and women are identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâinterpretive chaos in the early communityâwas a genuine coordination failure. A mandatrophy analysis prevents mislabeling the entire structure as pure extraction (snare) by preserving the coordination function in the classification: the world would rearrange if the constraint disappeared because arrangements genuinely depend on the legal coherence it provides. However, the temporal measurements show rising theater and extraction over centuries, indicating that the mandate has atrophied in part: institutional maintenance now consumes resources beyond what coordination requires, and the original problem context (low literacy, textual scarcity) has shifted while the institutional solution has hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How would the classification change if the quran_hadith_substrate kernel were read through reformist_ijtihad or state_hybrid rather than traditionalist_taqlid?',
    'Compare the compiled structural data of sibling constraints in this kernel family; the traditionalist reading concentrates extraction on progressive Muslims, women, and minorities while reformist readings would redistribute directionalities toward traditional institutions.',
    'A reformist reading would likely lower base_extractiveness for women and minorities while raising it for classical institutions; a state-hybrid reading would split directionality by policy domain, reducing the global scope of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel reading location and sibling structural delta').

omega_variable(
    authority_naturalness_ambiguity,
    'Is the ulama''s interpretive authority a natural feature of textual complexity and religious expertise, or a constructed institutional mechanism that concentrates power?',
    'Cross-cultural comparison with textual communities that operate without obligatory taqlid (e.g., Protestant Christianity, rabbinic voluntarism) and historical correlation analysis of literacy rates versus taqlid obligation strength.',
    'If natural, the constraint''s extraction is largely coordination cost and the type remains tangled_rope; if constructed, base_extractiveness exceeds coordination-justified levels and the type tends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_naturalness_ambiguity, conceptual, 'Whether religious authority is natural or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of reformist and progressive voices structural (institutional gatekeeping, funding control, state-enforced fatwa monopolies) or internalized (lay Muslims believe taqlid is a religious virtue and self-police against deviation)?',
    'Measure reformist voice prevalence in environments where institutional gatekeeping is weak (decentralized online discourse) versus environments with strong traditional certification; observe whether suppression persists after institutional barriers are removed.',
    'If internalized, effective suppression exceeds the structural measure, the constraint persists against institutional reform, and the directionality for lay Muslims shifts toward full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 40, 0.35).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 80, 0.4).
narrative_ontology:measurement(qura_tr_t120, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 120, 0.45).
narrative_ontology:measurement(qura_tr_t160, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 160, 0.48).
narrative_ontology:measurement(qura_tr_t200, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 200, 0.5).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(qura_be_t120, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 120, 0.65).
narrative_ontology:measurement(qura_be_t160, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 160, 0.7).
narrative_ontology:measurement(qura_be_t200, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 200, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(qura_su_t120, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 120, 0.75).
narrative_ontology:measurement(qura_su_t160, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 160, 0.78).
narrative_ontology:measurement(qura_su_t200, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 200, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, state_hybrid).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three structurally distinct constraints because the same scriptural corpus supports radically different authority structures. Traditionalist_taqlid concentrates authority in classical lineage; reformist_ijtihad distributes it toward contemporary ethics and individual reason; state_hybrid fragments it across policy domains. Their epsilon values, victim sets, and enforcement mechanisms differ widely enough that they cannot be treated as one constraint with measurement parameters. They form a constraint family linked by structural influence edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
