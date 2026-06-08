% ============================================================================
% CONSTRAINT STORY: hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hindu_dharmashastra_reading, []).

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
 *   constraint_id: hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Reading of Family Law Authority
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The dharmashastra reading of family law authority grounds marriage
 *   regulation in textual prescriptions from Manusmriti, Yajnavalkya Smriti,
 *   and later commentaries, mediated through regional interpretive schools
 *   (Mitakshara joint family system dominant in North India, Dayabhaga
 *   individual inheritance system in Bengal). Pre-1955, this reading operated
 *   as religious personal law governing Hindu marriage, divorce (effectively
 *   unavailable), succession, and family property. The constraint's core
 *   features: marriage as sacramental samskara (indissoluble ritual bond),
 *   caste endogamy requirements enforced through social ostracism and ritual
 *   exclusion, wife as ritual participant in husband's dharmic duties without
 *   independent legal personality, joint family property system concentrating
 *   control in senior male (karta). Post-1947 constitutional reform created
 *   parallel statutory framework (Hindu Marriage Act 1955, Hindu Succession
 *   Act 1956, Hindu Minority and Guardianship Act 1956) that partially
 *   displaced dharmashastra constraints in formal legal domain while
 *   customary practice persisted in parallel. The interval spans 1947-2018
 *   (Independence to contemporary period), capturing the statutory reform's
 *   impact on the constraint's operation.
 *
 * KEY AGENTS:
 *   - Upper-Caste Male Householders: Primary beneficiaries (institutional/arbitrage) — capture property control, ritual authority, social capital through arranged endogamous marriage system
 *   - Joint Family Property Holders: Primary beneficiaries (institutional/arbitrage) — benefit from property consolidation and intergenerational wealth transfer mechanisms
 *   - Ritual Purity Gatekeepers: Secondary beneficiaries (institutional/arbitrage in traditional contexts, piton classification in urban contexts) — maintain caste boundaries through marriage control; function has atrophied in urban middle class
 *   - Married Women Without Separate Property: Primary victims (powerless/trapped pre-1955, powerless/constrained post-1955) — trapped by sacramental indissolubility, no independent property rights, no legal personality separate from husband pre-reform; constrained by customary practice persistence post-reform
 *   - Inter-Caste Couples: Secondary victims (moderate/constrained post-1955) — face social ostracism and caste panchayat sanctions despite statutory protection; benefit from legal recognition but bear customary enforcement costs
 *   - Partners Seeking Dissolution: Tertiary victims (powerless/trapped pre-1955, moderate/constrained post-1955) — no exit mechanism under sacramental doctrine pre-reform; costly divorce process and social stigma post-reform
 *   - Reformist Legislative Coalition: Organized agents (organized/mobile) — Nehru's Congress reformers, women's organizations, Ambedkar's constitutional drafters building statutory alternative with sunset logic
 *   - Analytical Observer: Civilizational perspective recognizing both genuine coordination function (kinship and property coordination for propertied families) and asymmetric extraction (indissolubility entrapping women, caste endogamy excluding lower castes, property rules concentrating male control)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hindu_dharmashastra_reading, 0.58).
domain_priors:suppression_score(hindu_dharmashastra_reading, 0.72).
domain_priors:theater_ratio(hindu_dharmashastra_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hindu_dharmashastra_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hindu_dharmashastra_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hindu_dharmashastra_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hindu_dharmashastra_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hindu_dharmashastra_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(hindu_dharmashastra_reading, "Hindu Dharmashastra Reading of Family Law Authority").
narrative_ontology:topic_domain(hindu_dharmashastra_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hindu_dharmashastra_reading, '4f3414dd-3ccb-42d1-bed8-851432d71818').
narrative_ontology:cs_kernel_codification('4f3414dd-3ccb-42d1-bed8-851432d71818', fixed_text).
narrative_ontology:cs_authority_grounding('4f3414dd-3ccb-42d1-bed8-851432d71818', lineage).
narrative_ontology:cs_interpretation_layer_present('4f3414dd-3ccb-42d1-bed8-851432d71818').
narrative_ontology:cs_reading_relation('4f3414dd-3ccb-42d1-bed8-851432d71818', hindu_dharmashastra_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f3414dd-3ccb-42d1-bed8-851432d71818', hindu_dharmashastra_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f3414dd-3ccb-42d1-bed8-851432d71818', hindu_dharmashastra_reading__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f3414dd-3ccb-42d1-bed8-851432d71818', hindu_dharmashastra_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('4f3414dd-3ccb-42d1-bed8-851432d71818', foundational, marriage_as_sacramental_samskara).
narrative_ontology:cs_axiom_status(marriage_as_sacramental_samskara, holdable).
narrative_ontology:cs_axiom_grounding('4f3414dd-3ccb-42d1-bed8-851432d71818', marriage_as_sacramental_samskara, theological).
narrative_ontology:cs_axiom('4f3414dd-3ccb-42d1-bed8-851432d71818', foundational, caste_endogamy_as_dharmic_obligation).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharmic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4f3414dd-3ccb-42d1-bed8-851432d71818', caste_endogamy_as_dharmic_obligation, theological).
narrative_ontology:cs_axiom('4f3414dd-3ccb-42d1-bed8-851432d71818', secondary, wife_ritual_participant_not_autonomous_agent).
narrative_ontology:cs_axiom_status(wife_ritual_participant_not_autonomous_agent, overridden).
narrative_ontology:cs_axiom_grounding('4f3414dd-3ccb-42d1-bed8-851432d71818', wife_ritual_participant_not_autonomous_agent, conventional).
narrative_ontology:cs_reference_frame('4f3414dd-3ccb-42d1-bed8-851432d71818', classical_dharmashastra_commentaries).
narrative_ontology:cs_drift_state('4f3414dd-3ccb-42d1-bed8-851432d71818', post_constitutional_reform_1950_2018, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f3414dd-3ccb-42d1-bed8-851432d71818', '2026-06-08T14:23:45Z').
narrative_ontology:cs_kernel_id(hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hindu_dharmashastra_reading, upper_caste_male_householders).
narrative_ontology:constraint_beneficiary(hindu_dharmashastra_reading, joint_family_property_holders).
narrative_ontology:constraint_beneficiary(hindu_dharmashastra_reading, ritual_purity_gatekeepers).
narrative_ontology:constraint_victim(hindu_dharmashastra_reading, married_women_without_separate_property).
narrative_ontology:constraint_victim(hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(hindu_dharmashastra_reading, partners_seeking_dissolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED WIFE PRE-1955 (SNARE) — Before Hindu Marriage Act 1955, sacramental indissolubility doctrine made exit legally and ritually impossible. Bears maximum extraction: no property rights, no legal personality separate from husband, no dissolution mechanism. The sacramental frame presented structural entrapment as religious duty.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTER-CASTE COUPLE (TANGLED ROPE) — Faces social ostracism and ritual exclusion but can legally marry post-1955. Benefits from some coordination (legal recognition, property rights under reformed law) while bearing extraction (caste panchayat sanctions, family disinheritance, violence risk). Mixed coordination-extraction where reformed statutory framework provides exit from worst dharmashastra constraints but customary enforcement persists.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UPPER-CASTE JOINT FAMILY (ROPE) — Benefits from property consolidation through arranged endogamous marriage, ritual authority, and social capital. Experiences the constraint as coordination: dharmashastra marriage norms stabilize intergenerational wealth transfer, caste network cohesion, and ritual standing. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMIST COALITION POST-1947 (SCAFFOLD) — Organized agents (Nehru's Congress reformers, women's organizations, Ambedkar's constitutional drafters) saw dharmashastra marriage as transitional constraint requiring legislative sunset. Hindu Marriage Act 1955, Hindu Succession Act 1956, and subsequent amendments created statutory alternative with divorce provisions, women's property rights, inter-caste marriage protection. Sunset logic: dharmashastra constraints would attenuate as statutory framework matured and urbanization reduced joint family prevalence. Estimated timeline: 2-3 generations for statutory norms to dominate in urban contexts; longer in rural areas.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RITUAL PURITY GATEKEEPERS (PITON) — Caste endogamy and ritual purity maintenance through marriage control has atrophied in urban middle-class contexts where economic mobility, geographic dispersion, and statutory protections have eroded enforcement capacity. What remains is largely performative: caste verification rituals, gothra checks, and matrimonial advertisements maintain the theater of purity boundaries while inter-caste marriage rates rise and legal sanctions fail. The gatekeepers see their own function as degraded — maintained through social inertia and wedding ritual theater, not through effective control.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine coordination function (marriage as property and kinship coordination mechanism across generations) layered with asymmetric extraction (sacramental indissolubility entrapping women, caste endogamy excluding lower castes, joint family rules concentrating male property control). Both functions are structurally real: arranged endogamous marriage does solve coordination problems for propertied families, AND it extracts from women and excluded castes. The reformed statutory framework (post-1955) partially unbundles these functions but customary practice persists in parallel, creating legal pluralism.
constraint_indexing:constraint_classification(hindu_dharmashastra_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hindu_dharmashastra_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hindu_dharmashastra_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hindu_dharmashastra_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hindu_dharmashastra_reading, TR),
    TR >= 0.70.

:- end_tests(hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Pre-1955 extractiveness was severe (0.75) due to sacramental indissolubility trapping women in abusive marriages, zero independent property rights, and caste endogamy enforced through ostracism. Statutory reform reduced extractiveness to 0.58 by introducing divorce provisions, women's succession rights, and inter-caste marriage protection, but customary practice persistence maintains substantial extraction in rural contexts and traditional communities. The current value represents national average across urban/rural and reform/customary practice contexts. Suppression (0.72): High. Pre-1955 suppression was near-total (0.88): no legal exit from marriage, no property alternatives, caste panchayat enforcement of endogamy through violence threat. Post-reform suppression decreased to 0.72 as statutory framework provided formal alternatives, but informal enforcement persists through social ostracism, family pressure, economic dependency (especially for women without separate income), and caste panchayat jurisdiction in rural areas. The gap between formal legal rights and practical enforcement capacity maintains high suppression. Theater ratio (0.35): Moderate-low. The constraint has relatively low theater because dharmashastra prescriptions, while contested, are functionally operational: arranged marriages do coordinate kinship networks and property transfer; caste endogamy norms, while weakening in urban contexts, remain enforced in rural areas; joint family property rules continue to structure inheritance. Theater has increased slightly over the interval (0.25→0.35) as ritual purity gatekeeping has become more performative in urban middle-class contexts (piton classification), but core coordination functions persist. The theater is lower than many religious constraints because the dharmashastra reading remained legally operative until 1955 and continues as customary practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is diagnostic. Upper-caste male householders experience pure coordination (rope): arranged endogamous marriage solves legitimate problems of property consolidation, ritual continuity, and kinship network stability. They are net beneficiaries with arbitrage exit options (can opt into statutory framework selectively). Trapped wives pre-1955 experienced pure extraction (snare): sacramental indissolubility with no property rights and no exit created maximum extraction. Post-reform, women in traditional contexts experience constrained exit rather than trapped, but still classify as victims bearing substantial extraction. Inter-caste couples experience mixed coordination-extraction (tangled rope): benefit from statutory legal recognition while bearing customary enforcement costs (ostracism, disinheritance, violence risk). The reformist coalition saw temporary problem requiring legislative sunset (scaffold): statutory framework would displace dharmashastra constraints as urbanization and legal consciousness increased. The analytical observer sees both genuine coordination function and asymmetric extraction operating simultaneously (tangled rope at analytical level), with different intensities across urban/rural and reform/customary contexts. The constraint is NOT a mountain despite sacramental framing — the false summit detector would flag this as naturalization of a constructed constraint benefiting upper-caste male property holders.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper-caste male householders are primary beneficiaries with institutional power and arbitrage exit options. The engine derives low d (near 0.0-0.2) → negative or low chi → rope classification. Their structural position: they set marriage terms, control property, capture ritual authority, and can selectively adopt statutory provisions when advantageous while maintaining customary practice when beneficial. Married women without separate property are primary victims with powerless status and trapped exit options pre-1955 (constrained post-1955). Engine derives high d (near 0.8-1.0) → high chi → snare classification pre-reform. Post-reform, exit_options shift to constrained (costly but possible divorce, improved but incomplete property rights) → moderate d (0.5-0.7) → still substantial chi but not maximal. Inter-caste couples are secondary victims with moderate power and constrained exit options. Engine derives moderate d (0.4-0.6) → moderate chi → tangled rope classification. They benefit from statutory recognition (coordination function) while bearing customary sanctions (extraction). Joint family property holders are beneficiaries with institutional power and arbitrage exit options. Low d → rope classification. Ritual purity gatekeepers had beneficiary status with institutional power and arbitrage options in traditional contexts (low d → rope), but their function has atrophied in urban contexts where enforcement capacity eroded (piton classification from their own degraded perspective, not from high chi). The reformist coalition has organized power and mobile exit options (low d due to agency) → scaffold classification with sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that sacramental marriage is simultaneously a genuine coordination mechanism (for propertied families) AND an extraction mechanism (for women and excluded castes). The coordination function is structurally real: arranged endogamous marriage does solve intergenerational property transfer problems, does maintain kinship network cohesion, does coordinate ritual obligations. The extraction is also structurally real: sacramental indissolubility did entrap women in abusive marriages, caste endogamy does exclude and punish inter-caste couples, joint family property rules do concentrate male control. The tangled rope classification at analytical level captures both functions operating through the same institutional structure. The constraint is not mislabeled coordination (it coordinates) and not mislabeled extraction (it extracts). Both are present. The perspectival gap shows how beneficiaries experience only coordination while victims experience primarily extraction from the same structural arrangement. The false summit risk is that the sacramental frame naturalizes what is actually a constructed constraint serving identifiable beneficiaries — the 'divine mandate' story obscures the property consolidation function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a reading of the dharmashastra textual kernel, or is it the kernel itself? Do the Manusmriti, Yajnavalkya Smriti, and later commentaries constitute the constraint, or do competing interpretive traditions within Hindu law (Mitakshara vs Dayabhaga schools) represent different readings of a deeper kernel?',
    'Historical legal analysis of whether different regional Hindu law schools (Mitakshara''s joint family emphasis vs Dayabhaga''s individual inheritance) derive from textual interpretation or represent distinct customary systems later rationalized through textual authority. If the former, the dharmashastra texts are the kernel and this story should be further decomposed into regional reading variants. If the latter, customary practice is the kernel and textual authority is the legitimating layer.',
    'If texts are kernel: this story requires regional decomposition (Mitakshara/Dayabhaga readings as siblings). If custom is kernel: dharmashastra reading is already the correct grain, and statutory reform represents a competing kernel rather than a sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this represents kernel or reading in Hindu law interpretive structure').

omega_variable(
    sacramental_naturalization,
    'Is sacramental indissolubility a genuine theological constraint (mountain from the believer''s perspective) or a constructed doctrine that benefits identifiable agents (false summit: presented as divine mandate but serving upper-caste male property consolidation)?',
    'Cross-tradition comparison: early Vedic literature shows widow remarriage and divorce practices; later dharmashastra texts progressively restrict these. Textual archaeology can determine whether indissolubility doctrine emerged to solve theological problems or property consolidation problems. Correlation with property law development is diagnostic.',
    'If genuine theological constraint: victims experience mountain (sacramental bond is metaphysically real from within the frame). If constructed doctrine: snare classification is accurate even from believer''s perspective once the property-consolidation function is visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_naturalization, empirical, 'Whether sacramental indissolubility is theological necessity or constructed constraint').

omega_variable(
    statutory_sunset_effectiveness,
    'Has the statutory reform framework (Hindu Marriage Act 1955 onward) actually functioned as a scaffold with sunset logic, or has legal pluralism created permanent parallel systems where dharmashastra constraints persist indefinitely in customary practice despite statutory alternatives?',
    'Longitudinal data on: inter-caste marriage rates over time, women''s property claim success rates, divorce petition outcomes, caste panchayat jurisdiction persistence. If statutory framework is displacing customary practice, trends should show monotonic movement toward statutory norms. If parallel systems are stable, customary constraint persistence indicates scaffold perspective was aspirational rather than structural.',
    'If sunset is real: scaffold classification is structurally correct and dharmashastra constraints will attenuate to piton status across contexts. If parallel systems are stable: tangled rope or snare classifications are more accurate long-term, and the ''reform'' merely added a second constraint without removing the first.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_sunset_effectiveness, empirical, 'Whether statutory reform created genuine sunset or parallel legal pluralism').

omega_variable(
    urban_rural_decomposition,
    'Do urban and rural contexts represent different constraint stories requiring decomposition (different epsilon values for enforcement effectiveness, different beneficiary sets, different exit options), or do they represent the same constraint observed at different spatial scopes?',
    'Comparison of extractiveness and suppression metrics across urban/rural contexts. If metrics differ substantially (e.g., urban extractiveness < 0.3, rural > 0.7), the contexts should be modeled as separate stories linked by network.affects_constraints. If metrics are similar and only enforcement mechanisms differ, single story with spatial_scope variation is appropriate.',
    'If decomposition needed: current story overclaims generality by averaging across contexts with structurally different dynamics. If single story is appropriate: current epsilon (0.58) and suppression (0.72) represent genuine national-level averages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urban_rural_decomposition, empirical, 'Whether urban/rural contexts require separate constraint stories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hindu_dharmashastra_reading, 0, 71).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pre_1955_theater, hindu_dharmashastra_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(post_1955_theater, hindu_dharmashastra_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(post_1985_theater, hindu_dharmashastra_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(pre_1955_extractiveness, hindu_dharmashastra_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(post_1955_extractiveness, hindu_dharmashastra_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(post_1985_extractiveness, hindu_dharmashastra_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pre_1955_suppression, hindu_dharmashastra_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(post_1955_suppression, hindu_dharmashastra_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(post_1985_suppression, hindu_dharmashastra_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(hindu_dharmashastra_reading, secular_contractual_reading).
narrative_ontology:affects_constraint(hindu_dharmashastra_reading, hindu_succession_reform_1956).

% DUAL FORMULATION NOTE:
% The dharmashastra reading is one of five sibling readings of the family_law_authority kernel in Indian legal pluralism. Each reading governs a different religious community's personal law. The secular_contractual_reading (uniform civil code proposals) represents an alternative kernel framework rather than a sibling reading of the dharmashastra kernel — it rejects religious authority entirely and grounds family law in state sovereignty and contractual autonomy. Network edges capture both sibling coexistence (this reading affects how Muslim and Christian personal laws are interpreted through comparative reference) and kernel-level tension (this reading's persistence affects viability of secular uniform civil code proposals). The hindu_succession_reform_1956 edge captures the statutory reform's partial displacement of dharmashastra inheritance rules while marriage and divorce provisions remained under customary practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
