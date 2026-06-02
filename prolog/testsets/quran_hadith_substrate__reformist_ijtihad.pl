% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Contextual Ijtihad Mandated When Classical Rulings Conflict with Contemporary Ethics, Human Rights, and Maslaha
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   The reformist_ijtihad reading of the quran_hadith_substrate kernel
 *   instantiates a contemporary Islamic jurisprudential mandate: when
 *   classical hadith-derived rulings (on women's testimony, apostasy laws,
 *   LGBTQ+ status, non-Muslim rights) conflict with contemporary human rights
 *   ethics and the Quran's broader ethical trajectory toward justice (adl)
 *   and mercy (rahma), jurists must undertake contextual ijtihad to
 *   reinterpret classical rulings in light of maslaha (public interest) and
 *   contemporary circumstances. This reading emerged forcefully in the late
 *   20th century through reformist scholars like Abdullah bin Bayyah, Tariq
 *   Ramadan, and institutional initiatives like Morocco's 2004 Moudawana
 *   reforms. The constraint exhibits tangled rope structure: it contains
 *   genuine coordination function (enabling Muslims to practice Islam
 *   coherently in pluralistic societies without abandoning textual authority)
 *   alongside asymmetric extraction (redistributing interpretive authority
 *   from traditionalist schools to progressive scholars and secular states).
 *   The suppression is declining over the measurement interval (0.62 → 0.48)
 *   as reformist readings gain institutional backing in some Muslim-majority
 *   states, but remains elevated because traditionalist authority structures
 *   resist and some regions actively suppress reformist jurisprudence. The
 *   theater ratio is declining (0.62 → 0.55) as reformist methodology becomes
 *   more formalized and less dependent on ad-hoc appeals to maslaha; the
 *   readings are crystallizing into coherent schools rather than remaining
 *   scattered reinterpretations.
 *
 * KEY AGENTS:
 *   - Progressive Muslim scholars: Primary beneficiary (organized/arbitrage) — gain authority and legitimacy; can claim to recover authentic Islam while implementing modern ethics
 *   - Women, LGBTQ+ individuals, religious minorities: Primary beneficiary (powerless → organized/constrained via coalition) — gain expanded legal protections and interpretive validation of their existence within Islamic frameworks
 *   - Trapped believer (identity-locked): Primary victim (powerless/identity_locked) — experiences cognitive bind between faith tradition and ethical intuition; cannot exit without existential dissolution
 *   - Traditionalist authority structures: Secondary victim (institutional/constrained) — lose interpretive monopoly and institutional legitimacy; experience reformist mandates as extraction of their authority
 *   - Progressive nation-states: Secondary beneficiary (institutional/arbitrage) — can claim Islamic legitimacy while implementing human rights frameworks; use reformist ijtihad as cover for secular governance
 *   - Ordinary believers in pluralistic contexts: Mixed (moderate/constrained) — benefit from reduced cognitive conflict but face pressure to navigate multiple authority systems and community fragmentation
 *   - Formal Islamic legal system: Inert observer (institutional/constrained) — persists through theater of applying classical jurisprudence while selectively implementing reformist readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.48).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Contextual Ijtihad Mandated When Classical Rulings Conflict with Contemporary Ethics, Human Rights, and Maslaha").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, 'c9bbdac1-82d2-4f62-8cb3-992e9490be01').
narrative_ontology:cs_kernel_codification('c9bbdac1-82d2-4f62-8cb3-992e9490be01', fixed_text).
narrative_ontology:cs_authority_grounding('c9bbdac1-82d2-4f62-8cb3-992e9490be01', lineage).
narrative_ontology:cs_interpretation_layer_present('c9bbdac1-82d2-4f62-8cb3-992e9490be01').
narrative_ontology:cs_reading_relation('c9bbdac1-82d2-4f62-8cb3-992e9490be01', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('c9bbdac1-82d2-4f62-8cb3-992e9490be01', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('c9bbdac1-82d2-4f62-8cb3-992e9490be01', foundational, quranic_ethical_trajectory_authority).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_authority, holdable).
narrative_ontology:cs_axiom_grounding('c9bbdac1-82d2-4f62-8cb3-992e9490be01', quranic_ethical_trajectory_authority, deontological).
narrative_ontology:cs_axiom('c9bbdac1-82d2-4f62-8cb3-992e9490be01', foundational, maslaha_as_interpretive_principle).
narrative_ontology:cs_axiom_status(maslaha_as_interpretive_principle, holdable).
narrative_ontology:cs_axiom_grounding('c9bbdac1-82d2-4f62-8cb3-992e9490be01', maslaha_as_interpretive_principle, conventional).
narrative_ontology:cs_reference_frame('c9bbdac1-82d2-4f62-8cb3-992e9490be01', quranic_authority_with_contextual_transmission).
narrative_ontology:cs_drift_state('c9bbdac1-82d2-4f62-8cb3-992e9490be01', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c9bbdac1-82d2-4f62-8cb3-992e9490be01', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_individuals).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, contemporary_muslims_facing_ethical_contradiction).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_authority_structures).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_interpretive_monopoly).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, literalist_hadith_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED BELIEVER (SNARE) — Muslims seeking to follow Islamic law but whose identity is constituted through faith traditions face an impossible cognitive bind: classical hadith rulings on women's testimony, apostasy, LGBTQ+ individuals, and non-Muslim rights directly contradict ethical intuitions shaped by contemporary human rights norms and Quranic ethical trajectory. The believer is identity-locked (cannot exit faith without existential dissolution) yet experiences the literalist constraint as a trap — they internalize the conflict as personal moral failure rather than structural contradiction. Maximum experienced extraction because exit would require abandoning their identity entirely.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORMIST COALITION (TANGLED ROPE) — Progressive Muslim scholars and activists advocating contextual ijtihad experience this as a genuine coordination mechanism: they are solving the problem of how Islam can remain a living ethical tradition in modernity without abandoning textual authority or theological coherence. But they also face suppression: traditionalist institutions deny them authority, their interpretations are accused of Western infiltration, and legal systems dominated by classical jurisprudence actively marginalize their readings. They benefit from the opening they have created (expanded interpretive space) but bear costs (delegitimization, career risk, violent opposition). Active enforcement of reformist readings requires institutional backing they often lack.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE NATION-STATE (ROPE) — Some Muslim-majority states (Morocco's reforms to personal status law, Tunisia's constitutional secularism) benefit from reformist ijtihad as a coordination mechanism: they can claim Islamic legitimacy while implementing human rights frameworks. The constraint solves a genuine coordination problem: how to maintain religious legitimacy while governing a pluralistic society. The state experiences the constraint as beneficial (low extraction) because arbitrage options exist — reformist readings provide intellectual cover for secular governance without requiring abandonment of Islamic framing. This perspective sees reformist ijtihad primarily as coordination, not extraction.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONALIST AUTHORITY STRUCTURE (SNARE) — Classical jurisprudential schools and traditional Islamic institutions experience reformist ijtihad as an extraction mechanism that threatens their monopoly on legitimate interpretation. From this perspective, the constraint extracts their authority (unambiguously — victims declare their own loss). Traditionalist institutions have arbitrage options (they can accommodate some reforms or retreat to insular communities) but experience the reformist mandate as a direct assault on their structural legitimacy. The extraction flow is clear: reformist mandates redirect authority from traditionalist schools to progressive scholars, judges, and secular legal frameworks.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORDINARY BELIEVER / NAVIGATING PLURALISM (TANGLED ROPE) — Muslims in pluralistic societies face mixed coordination and extraction: the reformist mandate enables them to practice Islam without cognitive dissonance (coordination function), but they also face pressure to conform to state legal frameworks, risk social fragmentation along traditionalist/reformist lines, and bear psychological costs of navigating multiple authority systems. The constraint both enables their religious practice and constrains its full expression in traditionalist forms. Theater content is moderate — some reformist framing is performative (claiming to recover 'true Islam' when actually constructing new readings), but the underlying coordination work is genuine.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL ISLAMIC LEGAL SYSTEM (PITON) — Classical Islamic jurisprudence as a unified formal system is largely inert at the civilizational level: scholars invoke classical precedents while making decisions driven by contemporary politics, state law, and social pressure. The high theater ratio (0.55) reflects that much formal adherence to classical methodology is performative — judges and muftis claim to apply classical principles while substantially reconstructing them. The system persists through institutional inertia and because no unified alternative authority structure has fully replaced it. The constraint appears to be applying classical jurisprudence while actually enacting selective modernization.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TEMPORARY SYNTHESIS (SCAFFOLD) — From a civilizational analytical view, contextual ijtihad represents a temporary institutional form designed to manage the transition from classical jurisprudence to a fully reconstructed Islamic ethics for contemporary contexts. The constraint has built-in sunset logic: as reformist readings accumulate sufficient coherence and institutional backing, they will crystallize into their own schools and formal methodologies, at which point the 'mandate for contextual ijtihad' dissolves into standard jurisprudential practice. The constraint is scaffolding — temporary support for the transition from literalism to living interpretation. Low effective extraction because the analytical perspective sees genuine structural innovation and knows the form is temporary.
constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_hadith_substrate__reformist_ijtihad, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, TR),
    TR >= 0.70.

:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate, rising. At t0 (early period of reformist emergence, before institutional consolidation), extractiveness was low (0.18) because reformist readings were marginalized and posed minimal threat to traditionalist dominance — they were intellectual positions without enforcement power. As reformist readings gain institutional backing in some states and universities (t10: 0.32), extractiveness rises because authority is actually being redistributed from traditionalist to reformist structures. By t20 (contemporary period with some legal reforms and significant institutional presence), extractiveness reaches 0.42 as reformist ijtihad becomes partially mandated in some jurisdictions, creating real constraints on traditionalist authority. The trajectory shows that reformist ijtihad is NOT a natural law or pure coordination mechanism, but rather a contingent institutional power shift that increases extraction as it gains force. Suppression (0.48): Moderate, declining. Early suppression was high (0.62) because reformist scholars faced accusations of Western infiltration, institutional marginalization, and violent opposition from traditionalist actors. As some states provide legal backing for reformist readings (Morocco, Tunisia to varying degrees), suppression declines (0.54 → 0.48) because the coercive barriers to reformist interpretation are partly dismantled. However, suppression remains above rope threshold because traditionalist networks maintain parallel authority structures and can still constrain reformist scholars in many contexts. Theater ratio (0.55): Moderate, declining. Early reformist writings had higher theater (0.62) because they invoked maslaha and Quranic ethical trajectory in ways that felt ad-hoc, and because state implementation of reforms was often performative (symbolic commitment to progressivism without functional legal change). As reformist jurisprudence matures, the methodology becomes more rigorous and institutionalized (0.55), moving from theatrical appeals to structured reinterpretation. The declining theater ratio reflects professionalization of the reformist reading, not reduction of its contingency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full classical perspectival gap: the ordinary believer caught in identity-locked constraint (Snare) perceives literal hadith rulings as unchangeable and experiences reformist reinterpretation as external pressure on their faith. The reformist scholar perceives the same rulings as contextual and experiences the mandate as recovery of Quranic authentic intent (Tangled Rope). The traditionalist institution perceives the mandate as a direct attack on their authority (Snare — experiencing extraction). The progressive state perceives reformist ijtihad as a coordination mechanism for pluralistic governance (Rope). The piton perspective observes that formal Islamic jurisprudence maintains the theater of classical methodology while substantially reconstructing it. The analytical observer sees the whole process as temporary scaffolding — institutional forms designed to manage the transition from literalism to living interpretation, with built-in sunset logic. No perspective is 'wrong'; each observes a real structural aspect of the constraint. The critical gap: whether the trapped believer's experience (identity-locked, Snare) indicates genuine structural immobility or cognitive capture that could be dissolved by identity-frame shift. The reformist reading assumes the latter; the traditionalist reading assumes the former.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim structural positions and exit options. Progressive scholars (beneficiary/arbitrage) experience d ≈ 0.15 (low d → negative f(d) → low or negative effective extraction chi). Traditionalist institutions (victim/constrained-but-resourced) experience d ≈ 0.55-0.65 (medium-high d → positive f(d) → moderate chi). The trapped believer (victim/identity_locked) experiences d ≈ 0.85-0.89 (high d → high f(d) → high chi), derived from the fact that they are a victim structurally (bear the psychological cost of cognitive dissonance) with exit severely constrained by identity fusion (cannot leave faith without existential dissolution). The identity_locked exit option captures the specific mechanism: the believer is not trapped by external barriers (money, law, family pressure alone) but by internalized identity commitment. At biographical time horizon, identity_locked returns Rope because the agent perceives the constraint as potentially changeable in principle if their identity frame shifted, even though they cannot actually shift it from within their current epistemic position. This distinguishes identity_locked from trapped (immutable from any perspective) and constrained (high-cost exit but perceivable). The engine's directionality derivation automatically computes these values from beneficiary/victim declarations and exit_options; commentary documents the reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by clarifying that reformist ijtihad is NOT a pure coordination mechanism (Rope) and NOT pure extraction (Snare), but a genuinely mixed hybrid (Tangled Rope) with real coordination function and real asymmetric extraction. The coordination function is real: believers genuinely benefit from being able to practice Islam coherently in modern contexts, and the ijtihad process does solve the problem of how to maintain textual fidelity while adapting to contemporary ethics. The extraction is also real: the process redistributes interpretive authority from traditionalist schools (who lose monopoly legitimacy) to progressive scholars and secular-influenced institutions (who gain authority to reinterpret the tradition). The mandatrophy dissolves when we recognize that both elements are structural, not one being a mask for the other. Some observers (especially within traditionalist frameworks) see the extraction and deny the coordination function, perceiving pure Snare. Some observers (especially within progressive frameworks) see the coordination function and minimize the extraction, perceiving pure Rope. The tangled rope classification captures both: this is a coordination mechanism (genuine ethical and practical benefit) that is delivered through extraction (redistribution of interpretive authority). The declining suppression and extractiveness over the measurement interval suggest that as reformist readings become more institutionalized, they move toward genuine coordination — the extraction is front-loaded during the transition, but the constraint could stabilize as a Rope if reformist readings fully crystallize into their own schools with broad acceptance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maslaha_commensurability,
    'Is maslaha (public interest) sufficiently defined and bounded to serve as a legitimate override criterion for hadith-derived rulings, or does it function as an undefined escape hatch that collapses interpretive constraints?',
    'Comparative analysis of maslaha applications across different reformist schools; identification of consistent principles vs. ad-hoc invocations; historical cases where maslaha was invoked to justify opposite conclusions',
    'If bounded: reformist ijtihad is a coherent jurisprudential method (Rope or Tangled Rope with genuine coordination function). If undefined: maslaha is performative cover for unrestricted preference-aggregation (Snare with theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_commensurability, conceptual, 'Whether maslaha provides bounded constraint or functions as escape hatch').

omega_variable(
    quranic_ethical_trajectory_ambiguity,
    'Is there a univocal Quranic ethical trajectory regarding women''s rights, religious freedom, and LGBTQ+ inclusion, or does the Quran contain multiple ethical voices with genuinely contradictory implications?',
    'Detailed Quranic linguistic and contextual analysis; comparison of verses by theme and period; examination of whether reformist and traditionalist scholars are reading the same text or imposing incompatible structures onto it',
    'If univocal trajectory: reformist readings are recovering authentic Quranic intent (Rope — genuine coordination with textual fidelity). If contradictory: both reformist and traditionalist selections are defensible; the constraint is choosing which voices to amplify (Tangled Rope or Snare depending on whose choice is enforced).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quranic_ethical_trajectory_ambiguity, conceptual, 'Whether Quranic ethical trajectory is univocal or contains genuine contradictions').

omega_variable(
    authority_substrate_contestation,
    'Is this constraint primarily about interpretive methodology (how to read classical rulings in light of contemporary ethics) or about authority redistribution (whose voices count as legitimate Islamic interpretation)?',
    'Track whether reformist mandates succeed through methodological persuasion (scholars convince each other via arguments about hermeneutics) or institutional power (states enforce reformist readings, traditionalist scholars are marginalized, reformist institutions receive funding and legitimacy). Examine whether traditionalist scholars accept reformist methodology or resist on authority grounds.',
    'If methodological: the constraint is primarily about jurisprudential technique (Rope). If authority-based: the constraint is primarily about extracting power from traditionalist structures (Tangled Rope or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_substrate_contestation, empirical, 'Whether constraint operates through interpretive methodology or authority redistribution').

omega_variable(
    reading_kernel_ambiguity,
    'Is the quran_hadith_substrate a single contested kernel admitting multiple readings, or are reformist and traditionalist positions incompatible enough to constitute separate kernels with incommensurable authority structures?',
    'Examine whether reformist and traditionalist scholars invoke the same textual corpus (Quran, hadith, consensus) as authority source, even while disagreeing on priority and interpretation. If they appeal to wholly different authority sources, the kernel is distributed or implicit. If they appeal to the same texts with different interpretive frameworks, the kernel is fixed or formalized.',
    'If single kernel: all readings (reformist, traditionalist, state_hybrid) are legitimately interpreting the same foundational commitment. If separate kernels: the readings are not commensurable, and the constraint structure is misspecified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether reformist and traditionalist positions share a single contested kernel or constitute incommensurable authority structures').

omega_variable(
    performative_vs_functional_enforcement,
    'When reformist ijtihad is mandated by states or elite reformist institutions, is the enforcement primarily performative (symbolic claim to progressive legitimacy) or functionally altering believers'' lived experience of Islamic law?',
    'Track implementation fidelity: compare formal legal code changes with actual court decisions, fatwa issuances, and community practice; examine whether traditionalist scholars continue to dominate fatwas despite formal reformist mandates; assess whether believers actually follow reformist rulings or revert to traditionalist guidance from trusted sources',
    'If performative: suppression is lower than theater ratio suggests (believers and traditionalist scholars can ignore mandates); ε may be overstated. If functional: suppression is real (believers feel genuine conflict, traditionalist authority is genuinely constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_vs_functional_enforcement, empirical, 'Whether state/elite enforcement of reformist ijtihad is performative or functionally altering practice').

omega_variable(
    sibling_reading_incommensurability,
    'Can a single Muslim''s faith framework coherently hold both the reformist_ijtihad reading and the traditionalist_taqlid reading, or do these readings foreclose each other at the foundational level?',
    'Examine whether individual believers, scholars, or communities adopt both readings sequentially or hold them simultaneously (coexistence); assess whether the readings appeal to incompatible authority criteria or merely prioritize them differently; investigate cases where individual scholars have shifted from one reading to another',
    'If foreclose: reformist and traditionalist are fundamentally opposed (this reading forecloses traditionalist). If coexist: the readings compete but neither eliminates the other (genuine pluralism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_incommensurability, conceptual, 'Whether reformist and traditionalist readings foreclose or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhsr_theater_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.62).
narrative_ontology:measurement(qhsr_theater_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.58).
narrative_ontology:measurement(qhsr_theater_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(qhsr_extract_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qhsr_extract_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(qhsr_extract_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qhsr_suppress_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(qhsr_suppress_t10, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(qhsr_suppress_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate is a contested kernel admitting multiple readings with structurally distinct extractiveness values. This story (reformist_ijtihad, ε=0.42) models contextual ijtihad as a hybrid coordination-extraction mechanism. Sibling reading traditionalist_taqlid (ε lower, Rope-dominant) models classical jurisprudence as coordination without contested extraction. Sibling reading state_hybrid (ε higher, Snare-vulnerable) models state-enforced hybrid readings as extraction-prone without genuine coordination. Network links capture that these readings compete for institutional dominance and influence each other's viability. All three readings appeal to the same foundational kernel (Quran/hadith authority) but construct incommensurable interpretive structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
