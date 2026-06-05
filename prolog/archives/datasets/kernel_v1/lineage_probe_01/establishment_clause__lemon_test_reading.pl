% ============================================================================
% CONSTRAINT STORY: establishment_clause__lemon_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_establishment_clause__lemon_test_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: establishment_clause__lemon_test_reading
 *   human_readable: Establishment Clause via Lemon Test
 *   domain: constitutional_law/religious_liberty
 *
 * SUMMARY:
 *   The Lemon test (Lemon v. Kurtzman, 1971) represents one reading of the
 *   Establishment Clause kernel: government action is permissible under the
 *   Clause only if it (1) has a secular purpose, (2) has a primary effect
 *   that neither advances nor inhibits religion, and (3) does not foster
 *   excessive entanglement between government and religion. This reading
 *   instantiates a separationist hermeneutic — it positions neutrality as
 *   requiring government to exclude religious considerations from law-making
 *   and to refrain from subsidizing religious activity. The reading competes
 *   with three alternative doctrinal interpretations: the coercion test
 *   (which asks only whether government coerces religious participation), the
 *   endorsement test (which asks whether a reasonable observer perceives
 *   government endorsement of religion), and the history-traditions reading
 *   (which asks whether the practice aligns with founding-era understandings
 *   and unbroken tradition). All four readings are live positions in
 *   contemporary constitutional jurisprudence, held by different judicial
 *   coalitions and advocated by different legal communities. The Lemon test
 *   has become increasingly dominant in lower courts and in separationist
 *   advocacy but faces structural pressure from originalist and conservative
 *   jurisprudential movements that favor history-traditions and coercion
 *   approaches. The constraint exhibits Tangled Rope classification: it
 *   provides genuine coordination for separationist plaintiffs and
 *   legislatures seeking a binding rule, but it also extracts from
 *   faith-based institutions that must navigate unpredictable three-pronged
 *   scrutiny. The suppression mechanism operates through doctrinal breadth
 *   (all three prongs must be satisfied) combined with judicial discretion in
 *   applying the prongs (secondary effects can be read broadly or narrowly,
 *   entanglement is fact-intensive). Theater ratio reflects that courts have
 *   applied Lemon inconsistently across ideological lines — the test's formal
 *   structure obscures result-oriented jurisprudence.
 *
 * KEY AGENTS:
 *   - Faith-Based Institutions: Primary victim (powerless/trapped) — seek public funding or program participation; face three-pronged doctrinal barrier with unpredictable judicial application
 *   - Separationist Plaintiffs / Civil Liberties Advocates: Primary beneficiary (institutional/arbitrage) — use Lemon's broad three prongs to challenge accommodationist government action; have institutional standing to litigate
 *   - State Legislatures: Secondary actor (organized/constrained) — must design education and social services policy to satisfy Lemon scrutiny; experience mixed coordination (binding rule) and extraction (invalidation risk)
 *   - Federal Judiciary: Institutional actor (institutional/arbitrage) — applies and interprets Lemon doctrine; has performative and genuine enforcement roles
 *   - Originalist / History-Traditions Coalition: Organized reform agent (organized/constrained) — seeks to replace Lemon with history-traditions analysis; perceives Lemon as doctrinal error requiring replacement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the Lemon test as the inevitable logical form of Establishment Clause reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(establishment_clause__lemon_test_reading, 0.58).
domain_priors:suppression_score(establishment_clause__lemon_test_reading, 0.72).
domain_priors:theater_ratio(establishment_clause__lemon_test_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(establishment_clause__lemon_test_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(establishment_clause__lemon_test_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(establishment_clause__lemon_test_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(establishment_clause__lemon_test_reading, tangled_rope).
narrative_ontology:human_readable(establishment_clause__lemon_test_reading, "Establishment Clause via Lemon Test").
narrative_ontology:topic_domain(establishment_clause__lemon_test_reading, "constitutional_law/religious_liberty").

domain_priors:requires_active_enforcement(establishment_clause__lemon_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(establishment_clause__lemon_test_reading, '73e6f8f8-1603-48b8-969f-b6be17b1badb').
narrative_ontology:cs_kernel_codification('73e6f8f8-1603-48b8-969f-b6be17b1badb', fixed_text).
narrative_ontology:cs_authority_grounding('73e6f8f8-1603-48b8-969f-b6be17b1badb', lineage).
narrative_ontology:cs_interpretation_layer_present('73e6f8f8-1603-48b8-969f-b6be17b1badb').
narrative_ontology:cs_reading_relation('73e6f8f8-1603-48b8-969f-b6be17b1badb', establishment_clause__coercion_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('73e6f8f8-1603-48b8-969f-b6be17b1badb', establishment_clause__endorsement_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('73e6f8f8-1603-48b8-969f-b6be17b1badb', establishment_clause__history_tradition_reading, coexists_with).
narrative_ontology:cs_axiom('73e6f8f8-1603-48b8-969f-b6be17b1badb', foundational, secular_purpose_secular_effects_required).
narrative_ontology:cs_axiom_status(secular_purpose_secular_effects_required, holdable).
narrative_ontology:cs_axiom_grounding('73e6f8f8-1603-48b8-969f-b6be17b1badb', secular_purpose_secular_effects_required, deontological).
narrative_ontology:cs_axiom('73e6f8f8-1603-48b8-969f-b6be17b1badb', foundational, excessive_entanglement_prohibited).
narrative_ontology:cs_axiom_status(excessive_entanglement_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('73e6f8f8-1603-48b8-969f-b6be17b1badb', excessive_entanglement_prohibited, deontological).
narrative_ontology:cs_reference_frame('73e6f8f8-1603-48b8-969f-b6be17b1badb', separationist_constitutional_principle).
narrative_ontology:cs_drift_state('73e6f8f8-1603-48b8-969f-b6be17b1badb', contemporary_originalist_ascendancy, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('73e6f8f8-1603-48b8-969f-b6be17b1badb', '').
narrative_ontology:cs_kernel_id(establishment_clause__lemon_test_reading, establishment_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(establishment_clause__lemon_test_reading, separationist_plaintiffs).
narrative_ontology:constraint_beneficiary(establishment_clause__lemon_test_reading, secular_advocacy_groups).
narrative_ontology:constraint_victim(establishment_clause__lemon_test_reading, accommodationist_religious_programs).
narrative_ontology:constraint_victim(establishment_clause__lemon_test_reading, faith_based_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAITH-BASED INSTITUTION (SNARE) — A religious organization wishing to receive public funding or participate in government programs faces an unpredictable three-pronged test with high invalidation risk. The institution cannot exit the legal framework; it is trapped within constitutional constraints it did not choose. The Lemon test's broad suppression (secondary effects prong, entanglement prong) creates substantial barriers to accessing benefits available to secular programs. Maximum experienced extraction — the institution bears full cost of doctrinal scrutiny with no offsetting coordination benefit.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGISLATURE (TANGLED ROPE) — A state legislature implementing educational funding or social services must coordinate across religious and secular constituencies while satisfying the Lemon test. The legislature benefits from the test's clarity as a binding rule (coordination function) but also faces extraction: the three-pronged scrutiny invalidates programs that would pass simpler tests. Constrained exit — the legislature cannot avoid federal constitutional review. Mixed extraction and coordination.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SEPARATIONIST PLAINTIFF (ROPE) — A secular citizen or civil liberties organization challenging government support for religion experiences the Lemon test as coordination: the three-pronged test reliably enables them to challenge accommodationist programs and enforce constitutional limits. Net beneficiary. The test has arbitrage qualities — it enables strategic litigation to reshape policy. Rope classification reflects that the constraint's primary benefit (institutional standing to litigate) flows toward this agent.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL JUDICIARY (PITON) — The courts apply Lemon as a formal doctrinal standard (emergence from Lemon v. Kurtzman, 1971). However, the test has become substantially performative over 50+ years: courts routinely ignore, reinterpret, or apply the prongs inconsistently depending on ideological position (see divergent outcomes in Ten Commandments cases, school prayer cases, and voucher cases). The judiciary maintains Lemon through institutional inertia — it is the canonical test name in casebooks — despite widespread acknowledgment that the test fails to predict outcomes in contested cases. Theater ratio elevated because judicial invocation of Lemon is often ritualistic coverage for result-oriented reasoning.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST COALITION (SCAFFOLD) — A coalition of constitutional scholars and originalist jurists views Lemon as a temporary doctrinal compromise that obscures the founding-era understanding of Establishment. This perspective sees Lemon's sunset as imminent or necessary: the recent historical-traditions turn in SCOTUS jurisprudence (Carson v. Makin, Kennedy v. Bremerton School District, 2024-2025) treats Lemon as phasing out in favor of historical-traditions analysis. Low effective extraction for this coalition because they perceive structural exit pathways (doctrinal reformulation) and have organizational capacity to push them. Sunset clause is institutional/jurisprudential rather than statutory.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational timescale, the three-pronged test appears as an immutable logical consequence of constitutional conflict: any attempt to draw lines between permissible and impermissible government involvement with religion must employ some version of a test like Lemon. The analytic might see it as capturing a natural law of constitutional design — the irreducible complexity of religious neutrality. However, this reading is vulnerable to false-summit detection: the Lemon test is not inevitable given the Establishment Clause text, nor is it the only possible doctrinal reading (history-traditions, coercion, and endorsement tests are alternative doctrinal constructions). The mountain view naturalizes a contingent judicial choice made in 1971.
constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(establishment_clause__lemon_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(establishment_clause__lemon_test_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(establishment_clause__lemon_test_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(establishment_clause__lemon_test_reading, TR),
    TR >= 0.70.

:- end_tests(establishment_clause__lemon_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Lemon test enables separationist plaintiffs to challenge a broad range of government support for religion (three independent prongs, each sufficient to invalidate). Faith-based institutions experience substantial extraction: they face doctrinal uncertainty and invalidation risk when seeking public benefits. However, extraction is not maximal because the test provides some predictability (the three prongs are formally specified) and because legislative compliance is possible (many programs satisfy Lemon scrutiny). The measurement trajectory (0.42 → 0.58 over 10 periods) reflects increasing extractiveness as courts have become more willing to invoke the secondary-effects and entanglement prongs broadly, expanding the scope of invalidation. Suppression (0.72): High. The three-pronged test creates substantial barriers to faith-based program participation: secular-purpose requirement is difficult to satisfy when the program's origin involves religious motivation; secondary-effects prong captures programs whose foreseeable consequence is religious advancement; entanglement prong captures ongoing monitoring relationships. These barriers are structural (written into doctrine) rather than merely enforced through discretion. Theater ratio (0.65): Moderate-high. Lemon invocation is partly performative: courts have applied the three prongs inconsistently across factually similar cases, with ideological rather than doctrinal factors predicting outcomes. However, the test is not purely performative — some cases are predictable from Lemon logic (e.g., direct government subsidy to sectarian school easily fails prongs 1 or 2). The theater_ratio trajectory reflects increasing performative content as the doctrine has aged and ideological divides have widened (1971–2025).
 *
 * PERSPECTIVAL GAP:
 *   The Lemon reading produces dramatically different classifications across observer positions. Faith-based institutions (powerless/trapped) perceive Snare: high doctrinal suppression with no exit and no offsetting benefit. Separationist plaintiffs (institutional/arbitrage) perceive Rope: the doctrine enables strategic litigation with low cost of exit (they can simply not litigate, though institutional advocacy groups are committed to doing so). State legislatures (organized/constrained) perceive Tangled Rope: they benefit from a binding rule that clarifies their obligations but suffer extraction when programs are invalidated. The judiciary (institutional/arbitrage) perceives Piton: the test has become primarily performative, with inconsistent application masking result-oriented reasoning. The originalist coalition (organized/constrained) perceives Scaffold with a sunset: Lemon is a temporary doctrinal position that should be replaced by history-traditions analysis; they see the sunset as philosophically necessary and institutionally achievable through judicial appointments. The civilizational analytical observer risks perceiving Mountain: the three-pronged test appears as the logical form of religious neutrality itself. This perspectival gap reveals that the Lemon test is not a discovery of constitutional law but a contingent institutional choice whose legitimacy depends on the observer's structural position and commitment to separationism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the constraint. Faith-based institutions are victims seeking benefits; they have trapped exit options; d ≈ 0.95, producing high f(d) and high χ (experienced extraction). Separationist plaintiffs are beneficiaries with arbitrage options (they can litigate or refrain); d ≈ 0.05, producing negative f(d) and negative χ (coordination benefit, not extraction). State legislatures are both beneficiaries (they gain a binding rule) and victims (programs are invalidated); they have constrained exit (cannot ignore federal constitutional requirement); d ≈ 0.50, moderate f(d), producing balanced experienced extraction. The judiciary has arbitrage-like exit (can reinterpret Lemon) but is institutionally committed to its application; d ≈ 0.30, producing low-moderate f(d). The originalist coalition perceives Lemon as a constraint on their preferred doctrine; they have some organized exit capacity (judicial appointments, legislative constitutional amendment) but face resistance from separationist constituencies; d ≈ 0.55, moderate f(d). The analytical observer is not a structural participant; d ≈ 0.72, canonical analytical derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lemon reading resolves mandatrophy by acknowledging that multiple classification types can be structurally valid depending on observer position. The apparent contradiction (Snare vs. Rope vs. Mountain) is not an inconsistency in the framework but an accurate representation of how the Lemon doctrine functions differently for different institutional actors. The separationist sees genuine coordination (Rope); the faith-based institution sees pure extraction (Snare); the legislature sees mixed coordination-extraction (Tangled Rope). No type is universally correct — the presheaf over observer positions IS the complete picture. The false-summit risk (Mountain) arises when natural-law framing treats the Lemon test as the inevitable logical form of Establishment reasoning rather than as one doctrinal choice among competing alternatives (coercion, endorsement, history-traditions). Mandatrophy is fully resolved by the commitment-system frame: the Lemon test is one reading of the Establishment Clause kernel; its legitimacy depends on accepting separationist rather than accommodationist values; the kernel permits multiple readings; the doctrinal contest is not resolvable by appeal to the constitutional text alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lemon_predictive_power,
    'Does the Lemon test''s three-pronged structure actually predict Supreme Court outcomes in contested Establishment cases, or does the test provide ex-post justification for ideologically driven results?',
    'Empirical analysis: compare predicted outcomes from strict Lemon application versus actual SCOTUS voting patterns across 30-year case sample (e.g., school prayer, vouchers, religious display cases). If prediction accuracy < 60%, the test lacks epistemic weight and serves primarily as doctrinal theater.',
    'If high prediction accuracy: Lemon has genuine coordination function (beneficiary''s rope classification strengthens). If low accuracy: theater ratio is higher than 0.65; piton classification is stronger; tangled-rope classification for legislatures weakens (uncertainty eliminates coordination value).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lemon_predictive_power, empirical, 'Whether Lemon test prongs predict SCOTUS outcomes').

omega_variable(
    alternative_doctrinal_foreclosure,
    'Does the Lemon test''s three-pronged framework logically foreclose the coercion test or endorsement test, or are these alternative readings that coexist within constitutional jurisprudence?',
    'Doctrinal analysis: identify a case where Lemon and coercion test produce opposite holdings on the same facts. If such cases exist and are not reconciled, the readings coexist. If coercion test is presented as superseding Lemon''s logic, identify whether SCOTUS majority views them as foreclosing or compatible.',
    'If foreclosed: reading_relation is ''forecloses'' (rare, high confidence in doctrinal boundaries). If coexist: reading_relation is ''coexists_with'' (expected — different judicial coalitions hold different readings). If Lemon influences coercion test''s applicability without ruling it out: ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_doctrinal_foreclosure, conceptual, 'Logical relationship between Lemon and alternative doctrinal readings').

omega_variable(
    originalist_empirical_claim,
    'What did the founding generation''s understanding of ''establishment'' actually prohibit? Does historical practice (post-1789) show consistent support for secular-purpose-and-effects reasoning, or did founding practice permit what Lemon would invalidate?',
    'Historical scholarship: detailed analysis of founding-era government support for religion (chaplaincy, tax exemptions, Sunday closing laws). If founding practice shows sectarian aid that would fail Lemon, then Lemon''s axiom (secular_purpose_secular_effects_required) overrides founding-era understanding. If founding practice is consistent with Lemon''s prongs, the axiom is grounded in original understandings.',
    'If Lemon overrides founding practice: the axiom is vulnerable to history-traditions reading and faces ''axiom_overriding'' drift. If Lemon aligns with founding understanding, the secular_purpose_secular_effects_required axiom is empirically grounded in original understanding (shifts grounding_type).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_empirical_claim, empirical, 'Whether founding-era practice aligns with Lemon''s secular-purpose requirement').

omega_variable(
    separationist_litigation_capacity,
    'Does the Lemon test''s broad three-pronged scrutiny genuinely enable separationist plaintiffs to challenge accommodationist programs, or has procedural doctrine (standing, ripeness, mootness) eliminated the practical enforceability of the test?',
    'Procedural analysis: count successful separationist challenges under each prong of Lemon over past 20 years. Compare to failed challenges due to standing/ripeness barriers. If standing barriers prevent meritorious Lemon claims from being heard, the test''s suppression is procedurally nullified and its beneficiary status is compromised.',
    'If high enforcement rate: beneficiary_separationist_plaintiffs status is solid (rope classification confirmed). If enforcement is procedurally blocked: beneficiary status is illusory, and the suppression metric should reflect actual legal barriers rather than doctrinal breadth. This would reframe suppression as ''formal breadth with practical inaccessibility'' — shifting theater_ratio upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separationist_litigation_capacity, empirical, 'Whether Lemon test is procedurally enforceable for separationist plaintiffs').

omega_variable(
    kernel_contest_framing,
    'Is the Establishment Clause itself a kernel with multiple readings (Lemon, coercion, endorsement, history-traditions), or is Lemon a false reading that should be replaced by the correct original understanding?',
    'Jurisprudential assessment: if SCOTUS majority opinion explicitly acknowledges multiple live interpretive traditions and treats Lemon as one reading among several, the kernel framing applies. If SCOTUS treats Lemon as a mistaken doctrine that must be replaced by the correct reading, the kernel contest dissolves and only one correct reading remains.',
    'If kernel contest is accurate: this story is one of four sibling readings; all four are live; the constraint family persists. If one reading is displaced as incorrect: the decomposition structure collapses; only the correct reading instantiates an Establishment Clause constraint; others become historical-artifact stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether Establishment Clause is a contested kernel or permits one correct reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(establishment_clause__lemon_test_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esta_tr_t0, establishment_clause__lemon_test_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(esta_tr_t5, establishment_clause__lemon_test_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(esta_tr_t10, establishment_clause__lemon_test_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(esta_be_t0, establishment_clause__lemon_test_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(esta_be_t5, establishment_clause__lemon_test_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(esta_be_t10, establishment_clause__lemon_test_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(esta_su_t0, establishment_clause__lemon_test_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(esta_su_t5, establishment_clause__lemon_test_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(esta_su_t10, establishment_clause__lemon_test_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(establishment_clause__lemon_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(establishment_clause__lemon_test_reading, establishment_clause__coercion_test_reading).
narrative_ontology:affects_constraint(establishment_clause__lemon_test_reading, establishment_clause__endorsement_test_reading).
narrative_ontology:affects_constraint(establishment_clause__lemon_test_reading, establishment_clause__history_tradition_reading).

% DUAL FORMULATION NOTE:
% The Establishment Clause constraint family (4 stories) decomposes by doctrinal reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and suppression profiles. The Lemon reading has ε ≈ 0.58 (moderate-high extraction) and suppression ≈ 0.72 (high). The coercion reading has lower suppression and lower extraction (narrower invalidation reach). The endorsement reading has moderate suppression and intermediate extraction (observer-perception basis). The history-traditions reading has lower suppression and lower extraction (aligned with founding practice where more accommodation occurred). All four readings are linked by network.affects_constraints because the judicial choice of one reading affects the scope and severity of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(establishment_clause__lemon_test_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
