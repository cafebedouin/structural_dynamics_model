% ============================================================================
% CONSTRAINT STORY: muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_muslim_shariat_reading, []).

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
 *   constraint_id: muslim_shariat_reading
 *   human_readable: Marriage as Civil Contract under Muslim Shariat Law
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   Muslim Shariat law governs marriage as a civil contract (nikah) between
 *   consenting parties, grounded in Quranic injunctions and hadith
 *   transmitted through centuries of jurisprudential tradition. The
 *   constraint coordinates household formation, dissolution, inheritance, and
 *   child custody within a religiously embedded legal framework. It solves
 *   genuine problems: formalizing mutual obligations, providing culturally
 *   legitimate dispute resolution, preserving religious autonomy under Indian
 *   constitutional provisions (Articles 25-26). Simultaneously, it embeds
 *   structural extraction: unilateral male divorce access (talaq), polygyny
 *   permission, gender-asymmetric inheritance (daughters receive half of
 *   sons' shares under Quranic prescription), testimonial devaluation (two
 *   female witnesses equal one male in some contexts), and custody allocation
 *   favoring patrilineal lineage. The 2019 criminalization of instant triple
 *   talaq represents a partial sunset on the most extractive dissolution
 *   mechanism, but gender asymmetry in divorce initiation (khul requires
 *   husband's consent or judicial intervention; talaq does not) persists. The
 *   constraint operates at the intersection of religious authority, state
 *   legal pluralism, and gendered power asymmetry. This reading is one of
 *   five sibling readings of the contested kernel 'family_law_authority' —
 *   the others being Hindu Dharmashastra, Christian canonical, Parsi
 *   Zoroastrian, and secular contractual readings. Each reading instantiates
 *   a structurally distinct constraint with different beneficiary/victim
 *   configurations and different extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Husband Contractors: Primary beneficiary (moderate/constrained) — unilateral talaq access, polygyny permission, superior inheritance and testimonial weight; also coordinated through mahr obligation and formalized mutual duties
 *   - Wife Contractors: Primary victim (powerless/identity_locked) — asymmetric divorce access, inferior inheritance and testimonial weight, custody vulnerability; identity fusion with religious community makes exit unthinkable from within the frame despite structural mobility
 *   - Religious Judicial Authorities (Qazi): Institutional beneficiary (institutional/arbitrage) — authority monopoly over marital disputes within Muslim community; sees constraint as pure coordination
 *   - Extended Patrilineal Family: Secondary beneficiary (moderate/constrained) — custody and inheritance allocation favors patrilineal lineage; benefits from household stability under religiously legitimate framework
 *   - Children in Polygynous Households: Secondary victim (powerless/trapped) — resource dilution, household instability, maternal competition; no exit capacity
 *   - Muslim Women's Rights Reform Coalition: Organized agents (organized/constrained) — sees constraint as scaffold with sunset logic; 2019 triple talaq ban is partial victory; advocates for further reform
 *   - All India Muslim Personal Law Board: Institutional actor (institutional/constrained) — defends Shariat autonomy; benefits from authority monopoly; resists reform that would reduce institutional control; also provides genuine coordination function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (marital contract formalization, culturally embedded dispute resolution) and structural extraction (gender-asymmetric exit, inheritance, testimony)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(muslim_shariat_reading, 0.48).
domain_priors:suppression_score(muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(muslim_shariat_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(muslim_shariat_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(muslim_shariat_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(muslim_shariat_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(muslim_shariat_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(muslim_shariat_reading, "Marriage as Civil Contract under Muslim Shariat Law").
narrative_ontology:topic_domain(muslim_shariat_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(muslim_shariat_reading, '60af9561-3af4-4c18-a1e0-cd4bf0ad9608').
narrative_ontology:cs_kernel_codification('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', fixed_text).
narrative_ontology:cs_authority_grounding('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', lineage).
narrative_ontology:cs_interpretation_layer_present('60af9561-3af4-4c18-a1e0-cd4bf0ad9608').
narrative_ontology:cs_reading_relation('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', muslim_shariat_reading__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', muslim_shariat_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', muslim_shariat_reading__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', muslim_shariat_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', foundational, divine_law_gender_asymmetry).
narrative_ontology:cs_axiom_status(divine_law_gender_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', divine_law_gender_asymmetry, theological).
narrative_ontology:cs_axiom('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', foundational, male_unilateral_dissolution_authority).
narrative_ontology:cs_axiom_status(male_unilateral_dissolution_authority, overridden).
narrative_ontology:cs_axiom_grounding('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', male_unilateral_dissolution_authority, theological).
narrative_ontology:cs_axiom('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', secondary, patrilineal_custody_presumption).
narrative_ontology:cs_axiom_status(patrilineal_custody_presumption, holdable).
narrative_ontology:cs_axiom_grounding('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', patrilineal_custody_presumption, conventional).
narrative_ontology:cs_reference_frame('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', quranic_gender_complementarity).
narrative_ontology:cs_drift_state('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', post_2019_triple_talaq_ban, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60af9561-3af4-4c18-a1e0-cd4bf0ad9608', '').
narrative_ontology:cs_kernel_id(muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, husband_contractors).
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, religious_judicial_authorities).
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, extended_patrilineal_family).
narrative_ontology:constraint_victim(muslim_shariat_reading, wife_contractors).
narrative_ontology:constraint_victim(muslim_shariat_reading, women_under_unilateral_divorce).
narrative_ontology:constraint_victim(muslim_shariat_reading, children_in_polygynous_households).
narrative_ontology:constraint_vindicates(muslim_shariat_reading, divine_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(muslim_shariat_reading, gender_complementarity_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WIFE UNDER UNILATERAL TALAQ (SNARE) — Identity-locked exit: dissolution of marriage would require abandoning religious community identity and extended family network. Pre-2019 triple talaq structure gave husband unilateral dissolution power with minimal procedural constraint. Wife bears extraction through asymmetric exit rights, inferior inheritance shares, testimonial devaluation, and custody vulnerability. Post-2019 criminalization of instant triple talaq reduces some extraction but gender-asymmetric khul (wife-initiated dissolution requiring husband's consent or judicial intervention) remains. Maximum experienced extraction — structural mobility exists but identity fusion with religious community makes exit unthinkable from within the frame.
constraint_indexing:constraint_classification(muslim_shariat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: HUSBAND CONTRACTOR (TANGLED ROPE) — Benefits from unilateral talaq access, polygyny permission, superior inheritance shares, and testimonial weight. Also coordinated: mahr obligation creates dower security; nikah formalizes mutual obligations; witnesses and documentation reduce ambiguity. Mixed position: the constraint coordinates household formation and dissolution while extracting from the wife's structural position. Constrained exit because religious community membership and family honor are at stake, but exit is possible at a cost.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QAZI / SHARIAT COURT (ROPE) — Experiences the constraint as pure coordination: adjudicating marital disputes according to a stable canonical framework. Benefits from institutional authority and fee collection, but sees the structure as solving the genuine problem of providing religiously legitimate family governance. Arbitrage exit — authority can shift between Shariat courts, secular courts, or alternative dispute resolution without loss of livelihood or status.
constraint_indexing:constraint_classification(muslim_shariat_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized advocacy groups (All India Muslim Women's Personal Law Board, BMMA) see the gender-asymmetric structure as temporary and correctable through internal reform or legislative intervention. The 2019 triple talaq criminalization represents a partial sunset. Coalition has constrained exit (members remain within the religious community) but sees a clear path to reform: codification of gender-equal divorce procedures, judicial oversight of talaq, abolition of polygyny, inheritance equalization. Estimated sunset: 15-30 years for substantial internal reform or state-level uniform civil code adoption.
constraint_indexing:constraint_classification(muslim_shariat_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AIMPLB (TANGLED ROPE) — Institutional body defending Shariat autonomy against state encroachment. Benefits from authority monopoly and community legitimacy. Also constrained: genuine coordination function exists (providing culturally embedded dispute resolution, preserving religious autonomy under Article 25/26). Mixed extraction: the body resists reform that would reduce gender asymmetry because reform threatens institutional authority, yet also provides real adjudication services. Constrained exit — cannot abandon Shariat jurisdiction without dissolving institutional purpose.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint solves genuine coordination problems: formalizing marriage contracts, defining mutual obligations, providing dissolution procedures, clarifying inheritance. Simultaneously embeds asymmetric extraction: unilateral male divorce access, polygyny permission, testimonial and inheritance devaluation of women, custody allocation favoring patrilineal lineage. Both functions are structural, not merely claimed. The analytical classification is tangled_rope because both coordination and extraction are present and neither is pretextual.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(muslim_shariat_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(muslim_shariat_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(muslim_shariat_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Gender-asymmetric divorce access (pre-2019 instant triple talaq, post-2019 persistent khul/talaq asymmetry), polygyny permission, inheritance devaluation (daughters receive half), testimonial devaluation, and custody allocation favoring patrilineal lineage create substantial extraction toward wives and children in polygynous households. The 2019 triple talaq criminalization reduces extraction modestly (from 0.52 to 0.48) but does not eliminate asymmetry. The extraction is not as severe as pure coercive extraction (debt bondage, trafficking) because genuine coordination functions exist (mahr provides some economic security, nikah formalizes mutual obligations, Shariat courts provide culturally embedded dispute resolution). Suppression (0.62): Moderate-high. Wife's exit capacity is suppressed through identity lock (religious community membership, extended family network), material dependency (lower inheritance shares, limited labor market access, custody loss risk), and social ostracism. Husband's exit is less suppressed (unilateral talaq access, superior economic position) but still constrained by religious community norms and family honor. Suppression has increased over the interval (0.55 → 0.62 → 0.65 → 0.62) as urbanization and economic change have intensified material dependency for women with lower traditional family support, then decreased slightly post-2019 as triple talaq criminalization provided symbolic state backing for wife's complaints. Theater ratio (0.35): Low-moderate. The constraint has genuine functional content: marital contracts are formed, obligations are adjudicated, disputes are resolved. Theater exists in the gap between formal Shariat court procedures and actual enforcement (mahr claims often go unenforced, polygyny permission is rarely prosecuted when conditions are violated, instant triple talaq persisted despite Islamic scholarly consensus that it was invalid). Theater has increased modestly over the interval (0.25 → 0.30 → 0.35) as Shariat courts have become more procedurally formal without corresponding increases in enforcement capacity. The 2019 Act adds theater (criminalization with low conviction rate) but the overall ratio remains below 0.50 — most of the constraint's activity is functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The husband contractor sees tangled rope (benefits from unilateral exit and inheritance superiority, also coordinated through mahr and formalized obligations). The wife under unilateral talaq sees snare (identity-locked exit, asymmetric extraction through divorce access, inheritance, testimony, custody). The Qazi sees rope (pure coordination — adjudicating disputes according to stable canonical framework). The reform coalition sees scaffold (temporary problem with sunset — 2019 triple talaq ban is partial victory, further reform is structurally possible). The AIMPLB sees tangled rope (genuine coordination function in providing culturally embedded dispute resolution, also extracting through authority monopoly and resistance to gender-equal reform). The analytical observer sees tangled rope (both genuine coordination and structural extraction are present and neither is pretextual). The perspectival gaps are NOT measurement error — they reflect the agents' different structural positions. The wife experiences high extraction because she is the target; the husband experiences low extraction because he is the beneficiary; the Qazi experiences zero extraction because he is the institutional administrator. The scaffold perspective is real because organized advocacy has achieved partial reform and sees a path to further sunset; the snare perspective is real because the powerless/identity_locked agent cannot exit despite that reform pathway.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived by the engine from beneficiary/victim declarations, power level, and exit options. Husband contractors are declared beneficiaries with moderate power and constrained exit → low d → low effective extraction (they experience the constraint as mostly coordination). Wife contractors are declared victims with powerless position and identity_locked exit → high d → high effective extraction (they bear the asymmetry). Religious judicial authorities are declared beneficiaries with institutional power and arbitrage exit → very low d → negative effective extraction (they collect from the constraint's operation). Children in polygynous households are declared victims with powerless position and trapped exit → maximum d → maximum effective extraction. The reform coalition has organized power and constrained exit with no beneficiary/victim declaration → moderate d derived from power fallback → moderate extraction (they experience the constraint as a problem to be solved, not as pure extraction or pure coordination). The AIMPLB is a declared beneficiary with institutional power and constrained exit → low-moderate d → mixed experience (they benefit from authority but are also structurally locked into defending the framework).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled rope classification at the analytical level is structurally accurate: both genuine coordination and asymmetric extraction are present. The coordination function is not pretextual cover — nikah formalizes mutual obligations, mahr creates dower security, Shariat courts provide culturally embedded dispute resolution, witnesses and documentation reduce contractual ambiguity. The extraction is also not imaginary — unilateral male divorce access, polygyny permission, inheritance devaluation, testimonial devaluation, and custody allocation favoring patrilineal lineage create measurable asymmetry. The mandate (provide religiously legitimate family governance) has NOT outlived its function — the religious community still values culturally embedded dispute resolution over secular courts for marital matters. But the mandate has become extractive — the authority structure resists reform that would reduce gender asymmetry because reform threatens institutional control. The constraint is NOT a piton (theater ratio is only 0.35, and functional adjudication continues). It is NOT a pure snare (genuine coordination exists). It is NOT a pure rope (asymmetric extraction exists). It is tangled rope — the coordination and extraction are intertwined and both are structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the Muslim Shariat reading of family law authority structurally distinct from sibling readings (Hindu Dharmashastra, Christian canonical, Parsi, secular contractual), or do all personal law systems instantiate the same constraint with surface variation?',
    'Cross-reading comparison: measure ε, suppression, and beneficiary/victim structure across sibling readings. If structural metrics cluster tightly, readings are surface variants. If metrics diverge substantially (e.g., Shariat shows higher gender-asymmetric extraction than secular contractual, or Christian canonical shows higher dissolution suppression), readings are distinct constraints.',
    'If distinct: each reading is a separate constraint story with its own classification. If convergent: the kernel itself is the constraint and readings are observer framings rather than structural differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether sibling readings of family law authority are structurally distinct constraints or framings of one constraint').

omega_variable(
    triple_talaq_ban_sunset,
    'Does the 2019 Muslim Women (Protection of Rights on Marriage) Act represent a genuine sunset on unilateral male divorce extraction, or a symbolic criminalization with low enforcement?',
    'Longitudinal measurement: conviction rates under the Act, divorce initiation patterns post-2019, women''s reported exit capacity in surveys. If conviction rate > 20% and women''s divorce-initiation rate converges toward men''s, the sunset is real. If conviction rate < 5% and asymmetry persists, the Act is performative.',
    'If genuine sunset: extractiveness declines over time and the constraint shifts toward symmetric coordination (rope from more perspectives). If performative: extractiveness remains high and the Act adds theater_ratio rather than reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triple_talaq_ban_sunset, empirical, 'Whether the 2019 triple talaq ban is a functional sunset or performative legislation').

omega_variable(
    mahr_protection_sufficiency,
    'Does the mahr (dower) obligation provide genuine economic protection to wives, or does it function as symbolic compensation that leaves wives economically vulnerable post-divorce?',
    'Empirical analysis: average mahr amounts as percentage of household wealth, enforcement rates of mahr claims in Shariat courts, post-divorce economic outcomes for women with vs without mahr recovery. If mahr > 30% of household wealth and enforcement rate > 70%, protection is genuine. If mahr < 10% and enforcement < 40%, it is symbolic.',
    'If genuine protection: reduces effective extraction toward wives (lowers chi for powerless/identity_locked perspective). If symbolic: mahr is performative and extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_protection_sufficiency, empirical, 'Whether mahr provides real economic protection or symbolic compensation').

omega_variable(
    internal_vs_external_reform,
    'Will substantive reform of gender-asymmetric Shariat provisions come through internal religious reinterpretation (ijtihad, feminist tafsir) or through external state intervention (uniform civil code, legislative override)?',
    'Historical trajectory analysis: compare reform pathways in other Muslim-majority jurisdictions (Tunisia''s 1956 Code of Personal Status abolished polygyny through legislative reform; Morocco''s 2004 Mudawana reformed through state-guided religious reinterpretation). Track advocacy coalition strategies and institutional resistance patterns in India.',
    'If internal reform: scaffold sunset proceeds through reinterpretation of Quranic injunctions and hadith; religious authority structure adapts. If external intervention: state override triggers legitimacy crisis and institutional resistance; reform is imposed rather than integrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_vs_external_reform, preference, 'Whether reform proceeds through internal religious reinterpretation or external state intervention').

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the wife''s low exit capacity primarily identity-locked (internalized religious framing makes exit unthinkable) or structurally trapped (economic dependency, custody loss, social ostracism create material barriers)?',
    'Distinguish mechanisms through post-exit trajectory analysis: if women who leave Muslim marriages under secular law report that the primary barrier was cognitive (could not imagine exit while inside the frame) rather than material (faced high costs but could imagine exit), the lock is identity-based. If material costs dominate exit narratives, the trap is structural.',
    'If identity-locked: the constraint''s suppression is internalized and persists even after structural barriers are removed (e.g., women who gain economic independence still feel unable to exit). If structurally trapped: reducing material barriers (economic support, custody reform, anti-discrimination enforcement) directly increases exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether wife''s low exit is identity-locked or structurally trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(muslim_shariat_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msr_theater_1950, muslim_shariat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(msr_theater_1990, muslim_shariat_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(msr_theater_2019, muslim_shariat_reading, theater_ratio, 69, 0.35).
narrative_ontology:measurement(msr_theater_2026, muslim_shariat_reading, theater_ratio, 76, 0.35).

% Extraction over time
narrative_ontology:measurement(msr_extract_1950, muslim_shariat_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(msr_extract_1970, muslim_shariat_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(msr_extract_1990, muslim_shariat_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(msr_extract_2000, muslim_shariat_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(msr_extract_2019, muslim_shariat_reading, base_extractiveness, 69, 0.48).
narrative_ontology:measurement(msr_extract_2026, muslim_shariat_reading, base_extractiveness, 76, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(msr_suppress_1950, muslim_shariat_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(msr_suppress_1990, muslim_shariat_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(msr_suppress_2019, muslim_shariat_reading, suppression_requirement, 69, 0.65).
narrative_ontology:measurement(msr_suppress_2026, muslim_shariat_reading, suppression_requirement, 76, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(muslim_shariat_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the 'family_law_authority' kernel. Each reading is a structurally distinct constraint with different ε values. The Muslim Shariat reading shows moderate-high extraction (0.48) due to gender-asymmetric divorce, inheritance, and custody allocation. The secular contractual reading (Special Marriage Act 1954) shows lower extraction due to gender-neutral provisions. The Hindu Dharmashastra reading shows different extraction mechanisms (dowry system, restitution of conjugal rights) with different victim profiles. The readings are linked through the kernel but are not the same constraint observed from different angles — they have different beneficiary/victim structures and different temporal trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(muslim_shariat_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
