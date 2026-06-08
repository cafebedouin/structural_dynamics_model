% ============================================================================
% CONSTRAINT STORY: liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_reading, []).

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
 *   constraint_id: liturgical_reading
 *   human_readable: Liturgical Reading: Ritual Preservation as Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The liturgical reading instantiates one position in a contested kernel
 *   about Hebrew language vitality. This reading restricts the vitality claim
 *   to the ritual preservation domain: unbroken liturgical use across
 *   centuries constitutes evidence of Hebrew's continuous transmission,
 *   regardless of daily vernacular status. Rabbinic authorities, liturgical
 *   communities, and ritual preservation institutions benefit from this
 *   framing — it validates their custodial role and treats ritual continuity
 *   as sufficient for vitality claims. The constraint operates within a
 *   narrow domain (synagogue liturgy, ritual texts, ceremonial contexts) and
 *   makes no claim about daily conversational use or native speaker
 *   populations. Extractiveness is low (0.18) because the coordination
 *   function is genuine within its domain: liturgical Hebrew does preserve
 *   textual stability, enable cross-community worship, and maintain
 *   connection to historical practice. The constraint becomes extractive only
 *   when this narrow ritual domain is claimed as sufficient evidence for
 *   broader linguistic vitality — but that totalizing move belongs to a
 *   different reading (hybrid_continuity_reading) or to dynamics not captured
 *   in this constraint (vernacular suppression, which would be a separate
 *   story with different beneficiaries and victims). Theater ratio (0.42)
 *   reflects moderate performative content: liturgical practice maintains
 *   symbolic continuity even when comprehension is minimal, but the ritual
 *   does perform real coordination functions (communal synchronization,
 *   textual preservation, cross-generational transmission). The measurements
 *   show slight upward drift in both theater and extractiveness over the
 *   century-scale interval, reflecting increasing distance between liturgical
 *   Hebrew and vernacular language competence in diaspora communities.
 *
 * KEY AGENTS:
 *   - Rabbinic Authorities: Primary beneficiary (institutional/mobile) — institutional authority grounded in mastery of liturgical tradition; coordination function is real but authority structure benefits from Hebrew liturgical monopoly
 *   - Liturgical Communities: Organized beneficiaries (organized/constrained) — observant communities experience Hebrew liturgy as coordination enabling textual accuracy and cross-community coherence
 *   - Ritual Preservation Institutions: Institutional beneficiaries (institutional/mobile) — yeshivot, seminaries, and liturgical publishing houses benefit from maintaining Hebrew as ritual standard
 *   - Individual Worshipers: Moderate participants (moderate/mobile) — can exit or choose communities with more vernacular integration; experience liturgical Hebrew as enabling coordination rather than barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_reading, 0.18).
domain_priors:suppression_score(liturgical_reading, 0.25).
domain_priors:theater_ratio(liturgical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(liturgical_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(liturgical_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_reading, rope).
narrative_ontology:human_readable(liturgical_reading, "Liturgical Reading: Ritual Preservation as Hebrew Vitality").
narrative_ontology:topic_domain(liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_reading, '94eac047-7972-43ca-8612-7363aaeb84d5').
narrative_ontology:cs_kernel_codification('94eac047-7972-43ca-8612-7363aaeb84d5', fixed_text).
narrative_ontology:cs_authority_grounding('94eac047-7972-43ca-8612-7363aaeb84d5', lineage).
narrative_ontology:cs_interpretation_layer_present('94eac047-7972-43ca-8612-7363aaeb84d5').
narrative_ontology:cs_reading_relation('94eac047-7972-43ca-8612-7363aaeb84d5', liturgical_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('94eac047-7972-43ca-8612-7363aaeb84d5', liturgical_reading__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('94eac047-7972-43ca-8612-7363aaeb84d5', foundational, ritual_transmission_suffices_for_vitality).
narrative_ontology:cs_axiom_status(ritual_transmission_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('94eac047-7972-43ca-8612-7363aaeb84d5', ritual_transmission_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('94eac047-7972-43ca-8612-7363aaeb84d5', secondary, comprehension_not_required_for_continuity).
narrative_ontology:cs_axiom_status(comprehension_not_required_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('94eac047-7972-43ca-8612-7363aaeb84d5', comprehension_not_required_for_continuity, conventional).
narrative_ontology:cs_reference_frame('94eac047-7972-43ca-8612-7363aaeb84d5', second_temple_liturgical_hebrew).
narrative_ontology:cs_drift_state('94eac047-7972-43ca-8612-7363aaeb84d5', contemporary_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94eac047-7972-43ca-8612-7363aaeb84d5', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(liturgical_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(liturgical_reading, ritual_preservation_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liturgical_reading, individual_worshipers).
narrative_ontology:constraint_vindicates(liturgical_reading, ritual_continuity_doctrine).
narrative_ontology:constraint_vindicates(liturgical_reading, liturgical_hebrew_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic authorities set liturgical standards, adjudicate textual disputes via responsa, and transmit interpretive traditions. They benefit from institutional authority grounded in mastery of liturgical Hebrew. Could theoretically adopt vernacular liturgy but maintain Hebrew because it performs real coordination functions (textual stability, cross-community coherence) and because Hebrew liturgy is constitutive of rabbinic authority.
narrative_ontology:constraint_stakeholder(liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(liturgical_reading, rabbinic_authorities, beneficiary).

% Observant Jewish communities worldwide use liturgical Hebrew in synagogue worship, ritual practice, and ceremonial contexts. They benefit from textual continuity, cross-community worship coherence, and connection to historical practice. Constrained exit: switching to vernacular would fragment communal coherence and disrupt centuries of ritual continuity. Experience the constraint as coordination rather than extraction.
narrative_ontology:constraint_stakeholder(liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, constrained, regional).

% Yeshivot, rabbinic seminaries, liturgical publishing houses, and textual preservation organizations benefit from maintaining Hebrew as the ritual standard. Their institutional role depends on transmitting and adjudicating liturgical Hebrew texts. Could support vernacular integration but maintain Hebrew standard because it is constitutive of their mission.
narrative_ontology:constraint_stakeholder(liturgical_reading, ritual_preservation_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% Individual participants in liturgical practice engage with Hebrew prayers through varying levels of comprehension (native literacy, acquired study, translation aids, or rote familiarity). Benefit from participation in communal ritual and connection to textual tradition. Mobile exit: can choose communities with more vernacular integration, use translations and guides, or opt out of ritual practice entirely. Experience liturgical Hebrew as enabling coordination (access to tradition, communal synchronization) rather than as barrier.
narrative_ontology:constraint_stakeholder(liturgical_reading, individual_worshipers, beneficiary,
    moderate, biographical, mobile, local).

% UNESCO language vitality frameworks and sociolinguistic consensus criteria for assessing language endangerment. This is a NON-AGENT analytical framework, not a party that benefits or pays. Included for narrative completeness to document the contested definitional boundary: does ritual-only preservation count as vitality, or does vitality require daily vernacular use across domains? The framework itself collects no rents from this constraint.
narrative_ontology:constraint_stakeholder(liturgical_reading, sociolinguistic_vitality_frameworks, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(liturgical_reading, sociolinguistic_vitality_frameworks).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining stable, accurate transmission of liturgical texts across centuries and geographies; enabling cross-community worship coherence; preserving connection to historical practice and textual tradition.
% TRANSFER_FUNCTION: Institutional authority and interpretive legitimacy flow to rabbinic authorities and ritual preservation institutions who hold mastery of liturgical Hebrew. Participation and communal belonging flow to liturgical communities. Individual worshipers transfer deference (to rabbinic interpretation) and effort (acquiring Hebrew literacy or relying on translations).
% ABSENT_VOICES: Non-observant Jews, secular Hebrew speakers, and advocates for vernacular liturgical integration are not in the liturgical community's conversation. If present, they would argue that liturgical-only Hebrew preserves ritual at the cost of accessibility, that comprehension matters more than textual continuity, or that vitality requires daily use beyond the ritual domain. Their absence from the liturgical framing allows ritual preservation to be treated as sufficient for vitality claims.
% DISAPPEARANCE_RATIONALE: If liturgical Hebrew disappeared overnight, rabbinic authority structures would lose a key legitimacy pillar (mastery of sacred texts in original language), cross-community worship coherence would fragment (vernacular liturgy would diverge by region and language), and the chain of textual transmission stretching back centuries would break. Ritual preservation institutions would lose their mission. Individual worshipers would lose access to historical practice. Arrangements depend on this constraint.
% FOUNDING_PROBLEM: Hebrew ceased to be a daily vernacular language for most Jewish communities after the Bar Kokhba revolt (135 CE) and the consolidation of diaspora communities speaking Aramaic, Greek, Arabic, and later European vernaculars. The founding problem was how to maintain textual continuity of Torah, Talmud, and liturgical tradition across geographic dispersion and linguistic fragmentation. Unbroken liturgical use in Hebrew (synagogue worship, ritual practice, textual study) solved this coordination problem by preserving a stable linguistic kernel for sacred texts even as vernacular languages shifted.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguistic scholarship (Sáenz-Badillos, Fellman, Wexler) documents that Hebrew was not a native vernacular for most diaspora Jews from the 2nd century CE through the 19th century, yet liturgical and textual Hebrew remained stable across that period. Rabbinic responsa literature (Teshuvot) across centuries engages with liturgical Hebrew as a living legal and ritual language even while acknowledging vernacular diversity. Liturgical communities themselves attest that ritual Hebrew enables cross-community coherence (a Yemenite Jew and an Ashkenazi Jew can pray together despite different vernacular languages). The founding problem is corroborated by both scholarly consensus and by the sustained practice of liturgical communities, not merely by self-interested claims of rabbinic authorities.
narrative_ontology:disappearance_verdict(liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(liturgical_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RABBINIC AUTHORITIES (ROPE) — Experience liturgical Hebrew preservation as pure coordination. Unbroken ritual transmission across centuries solves the genuine problem of maintaining textual continuity and communal practice standards. Benefits from institutional authority grounded in mastery of liturgical tradition. Mobile exit options — could theoretically adopt vernacular liturgy but choose not to because the coordination function is real.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: LITURGICAL COMMUNITIES (ROPE) — Observant communities experience ritual Hebrew as coordination that preserves textual accuracy, enables participation across geographic boundaries, and maintains connection to historical practice. Constrained exit (switching to vernacular would fragment communal coherence and disrupt cross-community worship), but this is experienced as legitimate coordination cost rather than extraction. No identified victim set.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDIVIDUAL WORSHIPER (ROPE) — Individual participants in liturgical practice experience Hebrew as a coordination standard. Can comprehend prayers through translation, participation guides, or acquired literacy. Mobile exit — can choose communities with more vernacular integration or non-participation. The ritual framework is experienced as enabling rather than extractive.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, liturgical Hebrew preservation solves the coordination problem of maintaining stable ritual texts across centuries and geographies. The reading occupies a narrow domain (ritual practice) and makes no totalizing claim about daily linguistic vitality. Low extractiveness reflects genuine coordination function. The constraint becomes extractive only when the liturgical domain is claimed as sufficient evidence for broader vitality — but that move belongs to a different reading (the hybrid_continuity_reading or vernacular suppression dynamics), not this one.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_reading_tests).
:- end_tests(liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Liturgical Hebrew preservation solves genuine coordination problems within its narrow domain. Rabbinic authorities benefit from institutional authority grounded in liturgical mastery, but the coordination function (textual stability, cross-community worship, historical continuity) is structurally real. The constraint would become more extractive if ritual preservation were claimed as sufficient for broader vitality or if liturgical Hebrew were used to suppress vernacular alternatives — but those dynamics belong to sibling readings or separate constraints. Suppression (0.25): Low-moderate. Limited suppression within the ritual domain: liturgical communities could theoretically adopt more vernacular integration but choose not to because the coordination function is valued. No systematic suppression of alternatives within the constraint's domain. Theater ratio (0.42): Moderate. Liturgical practice maintains symbolic continuity and performs real coordination functions (communal synchronization, textual preservation), but comprehension is often minimal and participation can be ritualistic rather than communicative. The ritual has genuine function, but the function is partly performative (symbolic continuity, identity maintenance) rather than purely instrumental (textual comprehension, semantic engagement).
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as Rope because the constraint operates within a narrow, voluntary domain where the coordination function is genuine and the extraction is minimal. Rabbinic authorities benefit from institutional authority but also perform real coordination functions (textual adjudication, cross-community standards). Liturgical communities experience constrained exit (switching to vernacular would fragment coherence) but this is legitimate coordination cost. Individual worshipers have mobile exit and experience the constraint as enabling. The analytical observer sees genuine coordination within the ritual domain. The perspectival convergence (all Rope) reflects that this reading does not make totalizing claims about vitality — it restricts the claim to ritual preservation, where the coordination function is real and the extraction is low. Perspectival divergence appears in sibling readings: native_daily_reading (where vernacular suppression produces victims) and hybrid_continuity_reading (where Modern Hebrew revitalization creates new beneficiary/victim structures).
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (rabbinic authorities, liturgical communities, ritual preservation institutions) experience low directionality toward the constraint — they benefit from the coordination function and from the institutional authority it enables. No victim set is declared because the constraint operates within a voluntary ritual domain where participants choose to engage with liturgical Hebrew. The constraint imposes no cost on agents outside this domain. Individual worshipers have mobile exit options — they can choose communities with more vernacular integration or opt out of ritual participation entirely. The low extractiveness and absence of victims reflect that this reading occupies a narrow, domain-specific claim: ritual preservation constitutes vitality within the liturgical context. The totalizing move (claiming ritual preservation as sufficient for general linguistic vitality) is not present in this reading — that move would produce different beneficiary/victim structures and belongs to the hybrid_continuity_reading or to separate constraints about vernacular suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by restricting the vitality claim to a narrow domain where the coordination function is genuine. Liturgical Hebrew does preserve textual stability and enable cross-community ritual coherence — those are real coordination problems, and ritual preservation is a legitimate solution. The mandate has not outlived its function within the ritual domain. Mandatrophy would appear if this narrow ritual coordination were claimed as sufficient evidence for broader linguistic vitality (naturalizing ritual authority as general vitality) or if liturgical Hebrew were used to suppress vernacular alternatives outside the ritual domain. Those dynamics belong to sibling readings or separate constraints. Within its domain, this constraint is coordination with low extraction, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'This constraint is one reading of the contested kernel ''hebrew_vitality''. This reading (liturgical_reading) restricts the vitality claim to ritual preservation in liturgical contexts. Sibling readings native_daily_reading (native speakers in daily use) and hybrid_continuity_reading (Modern Hebrew revitalization incorporating liturgical corpus) occupy the same kernel but instantiate different constraints with different beneficiary structures and ε values. Is the liturgical domain sufficient to ground a vitality claim, or does vitality require daily vernacular use?',
    'Sociolinguistic definition of language vitality: consensus criteria from UNESCO vitality framework (intergenerational transmission, absolute number of speakers, domain of use). Liturgical-only use scores low on most vitality metrics. Resolution depends on whether ''vitality'' is defined narrowly (unbroken transmission) or broadly (active daily use across domains).',
    'If vitality requires daily use: this reading''s beneficiaries are overstating the claim, and the constraint becomes a false summit (naturalizing ritual authority as linguistic vitality). If vitality includes ritual-only preservation: this reading is descriptively accurate and remains low-extraction coordination. The sibling readings instantiate the alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Kernel reading disambiguation: is ritual preservation sufficient for vitality claim?').

omega_variable(
    historical_counterfactual_vernacular_shift,
    'Would rabbinic authorities have resisted a historical shift to vernacular liturgy if such a shift had been proposed within traditional frameworks (e.g., Sephardic communities adopting Ladino prayers, Ashkenazi communities adopting Yiddish)? If yes, does that resistance indicate extraction (preserving Hebrew liturgy to maintain institutional authority) rather than pure coordination?',
    'Historical analysis of documented liturgical language debates: Hasidic vernacular sermon traditions, Reform movement vernacular liturgy adoption in 19th century, Sephardic responsa on Ladino prayers. Did rabbinic authorities defend Hebrew liturgy on coordination grounds (textual stability, cross-community coherence) or authority grounds (sacred language doctrine, preservation of clerical interpretive monopoly)?',
    'If resistance was coordination-based: supports low ε (0.18). If resistance was authority-based: suggests higher ε (textual monopoly maintained through linguistic barrier) and potential victim set (congregants excluded by language barrier).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_counterfactual_vernacular_shift, empirical, 'Would vernacular liturgy shift have been resisted for coordination or authority reasons?').

omega_variable(
    literacy_barrier_as_hidden_extraction,
    'Does the liturgical Hebrew requirement create a structural barrier to participation for non-literate or less-educated community members, effectively extracting deference to rabbinic authorities who hold interpretive monopoly? If synagogue participation requires Hebrew literacy (or reliance on rabbinic interpretation), is the constraint coordination (enabling textual stability) or extraction (maintaining clerical authority through linguistic gatekeeping)?',
    'Ethnographic data on synagogue participation patterns: correlation between Hebrew literacy and active participation vs passive attendance. Historical comparison: did communities with higher vernacular liturgy integration (Reform, Reconstructionist) show broader lay participation in ritual leadership? Textual analysis: are Hebrew prayers accompanied by translations and participation aids, or is comprehension assumed/required?',
    'If literacy barrier is minimal (translations widely available, comprehension not required for participation): supports low ε coordination reading. If literacy barrier is substantial (comprehension assumed, rabbinic interpretation required): suggests victim set (non-literate congregants) and higher ε (extraction through linguistic gatekeeping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_barrier_as_hidden_extraction, empirical, 'Does liturgical Hebrew create hidden extraction via literacy barrier?').

omega_variable(
    cs_framing_underspecification,
    'This constraint is framed with kernel=''liturgical_corpus'' (the prayer texts) and authority=''rabbinic_tradition'' (the interpretive lineage that transmits and adjudicates liturgical practice). An alternative framing takes kernel=''hebrew_language_itself'' and authority=''ritual_use_community'' (the distributed practice of liturgical Hebrew by observant Jews worldwide, where authority is diffuse rather than rabbinic-hierarchical). The first framing foregrounds institutional authority and produces cs_pattern extraction; the second foregrounds distributed coordination and produces cs_pattern practice. Both are coherent. What structural signals guide the choice?',
    'Institutional analysis: who adjudicates liturgical disputes — rabbinic authorities via responsa and halakhic rulings, or distributed community practice via normative convergence? If adjudication is hierarchical (rabbinic), the first framing is structurally accurate. If adjudication is distributed (community norms), the second framing is accurate. Historical data: how have liturgical innovations (e.g., adding prayers for State of Israel, gender-neutral language) been adopted — top-down rabbinic rulings or bottom-up community practice?',
    'First framing (liturgical_corpus + rabbinic_tradition) highlights potential extraction via interpretive monopoly and aligns with lineage authority_grounding. Second framing (hebrew_language + ritual_use_community) highlights coordination via distributed practice and aligns with practice authority_grounding. Choice affects cs_pattern classification and whether the constraint routes to extraction or coordination in the commitment system typology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underspecification, conceptual, 'CS framing under-determination: institutional vs distributed authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liturgical_theater_t0, liturgical_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liturgical_theater_t50, liturgical_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(liturgical_theater_t100, liturgical_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(liturgical_extract_t0, liturgical_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(liturgical_extract_t50, liturgical_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(liturgical_extract_t100, liturgical_reading, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(liturgical_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The liturgical_reading is one of three constraint stories decomposed from the colloquial concept 'Hebrew vitality'. Each reading has its own ε value reflecting different domains and beneficiary structures. This reading (liturgical ritual preservation) has ε=0.18; native_daily_reading (Modern Hebrew vernacular in Israel) likely has higher ε due to vernacular suppression dynamics; hybrid_continuity_reading (revitalization drawing on liturgical corpus) has complex ε reflecting intersection of religious and secular nationalisms. The readings are linked via network.affects_constraints to model that they instantiate competing framings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
