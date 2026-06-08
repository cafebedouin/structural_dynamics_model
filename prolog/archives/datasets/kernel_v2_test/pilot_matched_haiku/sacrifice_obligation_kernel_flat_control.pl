% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel_flat_control, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel_flat_control
 *   human_readable: Halakhic Obligation to Perform Temple Sacrifice
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   The halakhic obligation to perform Temple sacrifice as commanded in Torah
 *   represents a constraint that exhibits all six Deferential Realism types
 *   from different structural positions. The constraint operates across two
 *   distinct historical periods: the Second Temple period (516 BCE – 70 CE),
 *   when sacrifice was functionally performable and institutionally central,
 *   and the post-destruction period (70 CE onward), when the obligation
 *   persisted in halakhic theory despite functional impossibility. In the
 *   Temple period, the constraint coordinates genuine religious functions
 *   (atonement, purity maintenance, collective worship) while extracting
 *   material and institutional benefits for the priestly class and Temple
 *   authority. In the post-destruction period, the constraint becomes largely
 *   theatrical: extensive halakhic elaboration of sacrifice law, liturgical
 *   commemoration, and theoretical mastery of an impossible practice. The
 *   constraint demonstrates how a single structural phenomenon can be
 *   classified as pure coordination (rope) from the beneficiary's
 *   perspective, mixed coordination-extraction (tangled rope) from the
 *   institutional authority's perspective, pure extraction (snare) from the
 *   obligated lay population's perspective, and degraded performance (piton)
 *   from the post-destruction analytical perspective. The analytical observer
 *   risks naturalizing this institutional arrangement as an immutable feature
 *   of Torah law (mountain), but the structural data reveals identifiable
 *   beneficiaries, active enforcement requirements, and high theater in the
 *   post-destruction period — all indicators of a false summit.
 *
 * KEY AGENTS:
 *   - Lay Jewish Population: Primary victim (powerless/identity_locked) — obligated to perform sacrifices; bears material costs (animals, time, travel) and opportunity costs; benefits minimally from the coordination function; identity-locked through covenantal identity fusion
 *   - Priestly Class (Kohanim): Primary beneficiary (institutional/arbitrage) — collects material benefits (meat portions, hides, incense fees); monopolizes sacred performance; experiences the constraint as pure coordination
 *   - Temple Institutional Authority: Secondary beneficiary (institutional/constrained) — controls resource allocation, adjudicates disputes, maintains ritual purity standards; requires active enforcement; benefits institutionally and materially
 *   - Halakhic Coherence: Primary victim (abstract/trapped) — the constraint's post-destruction persistence creates theoretical incoherence (obligation to perform impossible acts); cannot exit or organize
 *   - Rabbinic Reform Movement: Organized agent (organized/mobile) — sees the obligation as transitional; declares sunset through reinterpretation; has agency and exit path
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable Torah law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel_flat_control, 0.35).
domain_priors:suppression_score(sacrifice_obligation_kernel_flat_control, 0.62).
domain_priors:theater_ratio(sacrifice_obligation_kernel_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel_flat_control, "Halakhic Obligation to Perform Temple Sacrifice").
narrative_ontology:topic_domain(sacrifice_obligation_kernel_flat_control, "religious_law/halakhic_authority").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel_flat_control, '091f81b0-dfc3-43d6-84ab-3bdc6d0d86ad').
narrative_ontology:cs_kernel_codification('091f81b0-dfc3-43d6-84ab-3bdc6d0d86ad', fixed_text).
narrative_ontology:cs_authority_grounding('091f81b0-dfc3-43d6-84ab-3bdc6d0d86ad', lineage).
narrative_ontology:cs_interpretation_layer_present('091f81b0-dfc3-43d6-84ab-3bdc6d0d86ad').
narrative_ontology:cs_created_at('091f81b0-dfc3-43d6-84ab-3bdc6d0d86ad', '2026-02-26T00:00:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(sacrifice_obligation_kernel_flat_control, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel_flat_control, priestly_class).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel_flat_control, temple_institutional_authority).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel_flat_control, lay_jewish_population).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel_flat_control, halakhic_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated to perform sacrifices at the Temple, bearing material costs (animals, time, travel) and opportunity costs. Identity-locked through covenantal identity fusion — rejecting the obligation means rejecting Jewish identity itself. Experiences ritual impurity penalties and social ostracism for non-compliance. Benefits minimally from the coordination function (atonement, purity maintenance) because the priestly class collects most material benefits.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, lay_jewish_population, payer,
    powerless, biographical, identity_locked, regional).

% Monopolizes sacred performance and collects material benefits: meat portions from sacrifices, hides, incense fees, and other Temple revenues. Maintains institutional authority through control of purity adjudication and ritual performance. Can refuse to perform sacrifices, can migrate, can reinterpret halakha. Experiences the constraint as pure coordination — the sacrifice system solves the genuine problem of maintaining ritual purity and collective atonement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, priestly_class, beneficiary,
    institutional, immediate, arbitrage, regional).

% Controls Temple operations, resource allocation, and purity adjudication. Enforces the sacrifice obligation through mandatory pilgrimage, mandatory sacrifice at key festivals, and mandatory payment of temple taxes. Benefits institutionally (power, authority) and materially (Temple revenues). Constrained by the need to maintain the system's legitimacy and enforce compliance across a large population.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, temple_institutional_authority, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel_flat_control, temple_institutional_authority, beneficiary).

% The abstract collective good of halakhic coherence bears the cost of the post-destruction obligation's persistence. The constraint creates theoretical incoherence: an obligation to perform impossible acts (Temple sacrifice without a Temple). Cannot exit or organize. The constraint's persistence requires elaborate theoretical justifications (suspension vs transformation, eventual Temple restoration vs permanent reinterpretation) that strain halakhic logic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, halakhic_coherence, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel_flat_control, halakhic_coherence).

% Organized agents (Reform and Conservative rabbinical authorities) see the sacrifice obligation as a temporary historical arrangement with a sunset. Declare the obligation superseded by modern ethical monotheism and alternative mechanisms (prayer, ethical action, spiritual practice). Have substantial agency and see a clear exit path. Actively reinterpret halakha to replace the obligation with alternative practices.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, rabbinic_reform_movement, observer,
    organized, generational, mobile, global).

% Views the constraint from a civilizational perspective without structural stake in its operation. Risks naturalizing the sacrifice obligation as an immutable feature of Torah law — divinely commanded, textually fixed, logically necessary. The structural data (identifiable beneficiaries, active enforcement, high theater post-destruction) contradicts this naturalization, revealing it as a false summit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The sacrifice system solves the genuine problem of how to maintain ritual purity, achieve collective atonement, and sustain a relationship with the divine in Second Temple Judaism. The system provides mechanisms for purification after impurity, atonement for sins, and collective worship at key festivals. These are real coordination problems that require institutional solutions.
% TRANSFER_FUNCTION: The constraint transfers material resources (animals, incense, other offerings) from the lay population to the priestly class and Temple authority. It also transfers institutional power and authority from the lay population to the Temple institutional structure. The lay population bears costs (animals, time, travel, opportunity costs); the priestly class and Temple authority collect benefits (meat portions, hides, incense fees, institutional power).
% ABSENT_VOICES: The voices of those who rejected the sacrifice obligation (heterodox Jewish groups, Qumran community, early Christian Jews) are absent from the halakhic record. The constraint's legitimacy is established through the voices of those who benefited from it (priestly class, Temple authority) and those who accepted it (lay population with strong covenantal identity). Dissenting voices are marginalized or excluded from the halakhic conversation.
% DISAPPEARANCE_RATIONALE: If the sacrifice obligation disappeared, the world would rearrange itself substantially in the Temple period: atonement mechanisms would need replacement, purity maintenance would require alternative systems, collective worship would need new institutional forms. Post-destruction, the world has already rearranged itself — the obligation's disappearance would merely formalize what has already occurred (the shift to prayer, ethical action, and spiritual practice as alternatives). The contestation arises because different Jewish movements dispute whether the obligation's functional necessity has truly ended or whether it remains binding in principle pending Temple restoration.
% FOUNDING_PROBLEM: The founding problem was the need to maintain ritual purity, achieve atonement for sins, and sustain collective worship in Second Temple Judaism. The sacrifice system was built to solve these problems through institutional mechanisms: the Temple as the locus of purity, the priestly class as the mediators of atonement, and mandatory sacrifice as the mechanism of collective worship.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by: (1) the Temple's destruction in 70 CE, which made actual sacrifice performance impossible; (2) the development of alternative atonement mechanisms (prayer, repentance, ethical action) in rabbinic Judaism; (3) the successful functioning of Jewish communities without Temple sacrifice for nearly 2,000 years; (4) the explicit reinterpretation of the obligation by Reform and Conservative movements as superseded by alternative practices. The founding problem is dead in practice, though contested in principle by Orthodox movements that maintain the obligation pending Temple restoration.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY JEWISH POPULATION (SNARE) — Structurally mobile (could refuse sacrifice) but identity-locked through covenantal identity fusion. The obligation is experienced as inescapable because rejecting it means rejecting Jewish identity itself. Suppression is high: social ostracism, ritual impurity status, exclusion from community. The constraint extracts labor, resources, and time without proportional benefit to the obligated party. The priestly class collects the material benefits (meat portions, hides, incense fees).
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRIESTLY CLASS (ROPE) — Institutional beneficiary with high exit optionality (can refuse to perform sacrifices, can migrate, can reinterpret halakha). Experiences the constraint as pure coordination: the sacrifice system solves the genuine problem of how to maintain ritual purity, collective atonement, and divine relationship. The priestly class benefits materially (meat portions, hides, incense fees) and institutionally (monopoly on sacred performance). From this seat, the constraint is coordination with no experienced extraction.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPLE INSTITUTIONAL AUTHORITY (TANGLED ROPE) — Coordinates genuine religious function (atonement, purity maintenance, collective worship) while extracting institutional power and resource control. The authority requires active enforcement: mandatory pilgrimage, mandatory sacrifice at key festivals, mandatory payment of temple taxes. Suppression is substantial (religious law, social enforcement, ritual impurity penalties). But the authority also genuinely solves coordination problems: how to maintain collective ritual practice, how to allocate sacred resources, how to adjudicate purity disputes. The constraint is both coordination and extraction.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: POST-DESTRUCTION HALAKHIC SYSTEM (PITON) — After the Temple's destruction in 70 CE, the sacrifice obligation persists in halakhic theory and liturgical memory despite the functional impossibility of performance. The constraint becomes largely theatrical: Talmudic discussions of sacrifice law, liturgical recitations of sacrifice procedures, theoretical elaborations of sacrifice rules. The original coordination function (actual atonement, actual purity maintenance) is gone, but the obligation persists through institutional inertia and textual authority. Theater ratio is high: extensive halakhic literature on sacrifices that cannot be performed, detailed liturgical commemoration, theoretical mastery of an impossible practice.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the sacrifice obligation might appear as an immutable feature of Torah law: divinely commanded, textually fixed, logically necessary for atonement and purity. The obligation emerges naturally from the Torah's own logic and requires no external enforcement — it is self-evident to those who accept Torah authority. However, the structural data contradicts this: the constraint has identifiable beneficiaries (priestly class, temple authority), requires active enforcement, and exhibits high theater in the post-destruction period. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: RABBINIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (Reform and Conservative rabbinical authorities) see the sacrifice obligation as a temporary historical arrangement with a sunset: the obligation was appropriate for the Temple period but is superseded by modern ethical monotheism and rabbinic reinterpretation. The constraint is experienced as transitional — the obligation is being replaced by prayer, ethical action, and spiritual practice. The sunset is declared explicitly: sacrifice is no longer obligatory; its function is fulfilled through alternative means. This perspective has substantial agency and sees a clear exit path.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sacrifice_obligation_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(sacrifice_obligation_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts material resources (animals, time, travel costs) and institutional power from the lay population to the priestly class and Temple authority. However, the extraction is not maximal because the constraint also provides genuine coordination benefits: atonement mechanisms, purity maintenance, collective worship. The lay population experiences both coordination benefit and extraction cost. The measurement trajectory shows declining extractiveness over time (0.42 → 0.35) as post-destruction alternatives (prayer, repentance, ethical action) reduce the constraint's functional necessity. Suppression (0.62): Moderate-high. The constraint is maintained through multiple suppression mechanisms: religious law (Torah and rabbinic authority), social enforcement (community norm, ritual impurity status), and institutional power (Temple control of purity adjudication). However, suppression is not total — some dissent exists, and post-destruction suppression mechanisms weaken substantially. The measurement trajectory shows declining suppression (0.75 → 0.45) as external enforcement mechanisms become impossible and identity-lock becomes the primary binding mechanism. Theater ratio (0.68): High in post-destruction period, low in Temple period. In the Temple period, the constraint is functionally performable and theater is minimal (0.15) — sacrifices are actually performed, not merely discussed. Post-destruction, theater rises dramatically (0.15 → 0.72) as the constraint becomes largely theoretical: Talmudic discussions of sacrifice law, liturgical recitations, theoretical elaborations of impossible practices. The rising theater trajectory indicates the constraint's functional degradation and transition to piton classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The priestly class sees pure coordination (rope) — the sacrifice system solves the genuine problem of maintaining ritual purity and collective atonement. The Temple authority sees mixed coordination-extraction (tangled rope) — the system both solves coordination problems and concentrates institutional power. The lay population sees pure extraction (snare) — they bear costs without proportional benefit, and their identity-lock prevents exit. The post-destruction halakhic system sees degraded performance (piton) — the obligation persists through textual authority and institutional inertia despite functional impossibility. The analytical observer risks seeing immutable natural law (mountain) — the obligation appears divinely commanded and self-evident to Torah believers. The Reform movement sees a transitional arrangement (scaffold) — the obligation is being replaced by alternative mechanisms with an explicit sunset. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position: their power level, exit options, time horizon, and relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. The lay population has high d (0.75–0.85): they are trapped or identity-locked, have no arbitrage options, and bear costs without collecting benefits. The priestly class has low d (0.10–0.20): they are institutional beneficiaries with arbitrage options, collecting material and institutional benefits. The Temple authority has moderate d (0.40–0.50): they are institutional beneficiaries but constrained by the need to maintain the system's legitimacy and enforce compliance. The post-destruction halakhic system has moderate-high d (0.55–0.65): the obligation persists despite functional impossibility, indicating that the constraint's binding mechanism has shifted from external enforcement to internalized authority. The analytical observer has neutral d (0.50): they see the constraint from a civilizational perspective without structural stake in its operation. The engine computes effective extraction (χ) from d using the sigmoid f(d), amplifying extraction for high-d agents (trapped, identity-locked) and damping it for low-d agents (beneficiaries with arbitrage). The perspectival gap in d values (0.10 for beneficiaries vs 0.80 for victims) produces the maximum perspectival divergence in classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The sacrifice obligation's mandate (atonement, purity maintenance, collective worship) has outlived its functional necessity post-70 CE. The Temple's destruction made actual sacrifice performance impossible, yet the obligation persists in halakhic theory and liturgical practice. The constraint exhibits classic mandatrophy symptoms: (1) the founding problem (need for Temple-based atonement) is dead; (2) the arrangement persists through institutional inertia (textual authority, rabbinic elaboration); (3) the theater ratio rises dramatically (0.15 → 0.72) as the constraint becomes increasingly performative; (4) suppression mechanisms shift from external (Temple enforcement) to internal (identity-lock, textual authority). The resolution is visible in the Reform movement's explicit sunset: the obligation is reinterpreted as superseded by prayer, ethical action, and spiritual practice. The constraint demonstrates how mandatrophy manifests in religious law: the obligation persists not because it solves the founding problem (which is impossible post-destruction) but because the textual authority and institutional structure that created it remain in place. The piton classification captures this degradation: the constraint is maintained theatrically through halakhic elaboration and liturgical commemoration, not through functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the sacrifice obligation a divinely commanded natural law (immutable, self-evident to Torah believers) or a constructed institutional arrangement that benefits identifiable parties?',
    'Historical-textual analysis: does the obligation appear in all Torah strata or emerge in later priestly redaction? Comparative religious analysis: do parallel sacrifice systems in ancient Near Eastern religions show similar institutional beneficiary patterns? Halakhic analysis: does the obligation''s scope and enforcement change across rabbinic periods in ways consistent with institutional interests rather than divine command?',
    'If natural law: mountain classification is correct; beneficiaries are incidental. If constructed: false summit classification is correct; the constraint is tangled rope with priestly extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, empirical, 'Whether sacrifice obligation is divinely immutable or institutionally constructed').

omega_variable(
    identity_lock_mechanism_strength,
    'How much of the lay Jewish population''s compliance with sacrifice obligation derives from identity fusion (covenantal identity) versus external suppression (social penalty, ritual impurity status)?',
    'Historical evidence of voluntary compliance vs forced compliance; analysis of dissent and resistance movements; examination of whether identity-locked agents could exit if suppression were removed; study of post-destruction period when suppression mechanisms weakened but identity-lock persisted.',
    'If primarily identity-locked: the constraint''s binding mechanism is cognitive rather than structural; exit would require identity transformation. If primarily suppression-based: the constraint is structurally extractive and would collapse if enforcement ceased.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Relative strength of identity-lock versus external suppression in compliance').

omega_variable(
    priestly_material_benefit_quantification,
    'What proportion of priestly income and status derived from the sacrifice system versus other Temple functions (teaching, blessing, purity adjudication)?',
    'Textual analysis of priestly portions in Torah and Talmud; economic reconstruction of Temple-period Jewish communities; comparison with non-sacrificial priestly functions in post-destruction period.',
    'If sacrifice system provided majority of priestly benefit: constraint is substantially extractive (high ε). If sacrifice was one function among many: extraction is moderate (medium ε).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_material_benefit_quantification, empirical, 'Quantification of priestly material benefit from sacrifice system').

omega_variable(
    coordination_function_necessity,
    'Was the sacrifice system the only available mechanism for achieving atonement, purity maintenance, and collective worship in Second Temple Judaism, or were alternative mechanisms available?',
    'Textual evidence of alternative atonement mechanisms (repentance, prayer, ethical action); historical evidence of non-sacrificial Jewish communities (Qumran, Diaspora); rabbinic discussions of sacrifice substitutes.',
    'If sacrifice was necessary: coordination function is genuine and extraction is justified (rope or tangled rope). If alternatives existed: the constraint is more purely extractive (snare or tangled rope with higher ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether sacrifice system was the only available coordination mechanism').

omega_variable(
    post_destruction_mandate_obsolescence,
    'Does the persistence of sacrifice obligation in post-destruction halakha represent genuine theological commitment to eventual Temple restoration, or institutional inertia and textual authority maintenance?',
    'Analysis of rabbinic discussions of sacrifice law post-70 CE; examination of whether the obligation is treated as temporarily suspended or permanently transformed; study of how different Jewish movements (Orthodox, Conservative, Reform) justify or reject the obligation.',
    'If genuine theological commitment: the obligation remains structurally binding (mountain or rope). If institutional inertia: the constraint is piton (degraded function maintained theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_destruction_mandate_obsolescence, conceptual, 'Whether post-destruction sacrifice obligation reflects theology or inertia').

omega_variable(
    suppression_mechanism_internalization,
    'In the post-destruction period, when external suppression mechanisms (Temple enforcement, ritual impurity penalties) became impossible, did the suppression persist through internalization (identity-lock, textual authority, community norm) or did it decay?',
    'Historical evidence of continued compliance with sacrifice-related laws (e.g., Passover sacrifice substitutes, fast-day observances) after Temple destruction; analysis of whether compliance persisted in communities with weak enforcement capacity; study of how suppression mechanisms transformed from external to internal.',
    'If suppression internalized: the constraint''s binding mechanism shifted from external to cognitive; identity-lock became the primary suppression mechanism. If suppression decayed: the constraint''s force weakened substantially post-destruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression mechanisms internalized or decayed post-destruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel_flat_control, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_obl_theater_t0, sacrifice_obligation_kernel_flat_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sac_obl_theater_t50, sacrifice_obligation_kernel_flat_control, theater_ratio, 50, 0.35).
narrative_ontology:measurement(sac_obl_theater_t100, sacrifice_obligation_kernel_flat_control, theater_ratio, 100, 0.68).
narrative_ontology:measurement(sac_obl_theater_t150, sacrifice_obligation_kernel_flat_control, theater_ratio, 150, 0.72).

% Extraction over time
narrative_ontology:measurement(sac_obl_extract_t0, sacrifice_obligation_kernel_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sac_obl_extract_t50, sacrifice_obligation_kernel_flat_control, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(sac_obl_extract_t100, sacrifice_obligation_kernel_flat_control, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(sac_obl_extract_t150, sacrifice_obligation_kernel_flat_control, base_extractiveness, 150, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sac_obl_supp_t0, sacrifice_obligation_kernel_flat_control, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sac_obl_supp_t50, sacrifice_obligation_kernel_flat_control, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(sac_obl_supp_t100, sacrifice_obligation_kernel_flat_control, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(sac_obl_supp_t150, sacrifice_obligation_kernel_flat_control, suppression_requirement, 150, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel_flat_control, temple_purity_system).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel_flat_control, priestly_authority_legitimacy).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel_flat_control, jewish_identity_covenantal_binding).

% DUAL FORMULATION NOTE:
% The sacrifice obligation is downstream of the broader Temple institutional system and upstream of post-destruction halakhic authority. The constraint's extractiveness and suppression values are interdependent with the purity system (which defines who can perform sacrifices and who is obligated) and with covenantal identity binding (which locks the lay population into compliance despite extraction). Separate constraint stories for the purity system and identity-binding mechanism would show how the sacrifice obligation is embedded in a larger institutional ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
