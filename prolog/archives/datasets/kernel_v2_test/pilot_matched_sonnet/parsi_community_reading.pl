% ============================================================================
% CONSTRAINT STORY: parsi_community_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parsi_community_reading, []).

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
 *   constraint_id: parsi_community_reading
 *   human_readable: Parsi Community Marriage Authority (Zoroastrian Codified Reading)
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The Parsi community reading of the marriage authority kernel instantiates
 *   a codified religious personal law regime governing a small,
 *   demographically declining Zoroastrian diaspora community in India
 *   (approximately 60,000 members as of 2021). The Parsi Marriage and Divorce
 *   Act (1936) codified community norms into statutory law, creating a hybrid
 *   structure: religious doctrine operationalized through state-enforced
 *   legal mechanisms. The constraint exhibits tangled rope structure at the
 *   analytical level because the coordination function (preserving
 *   Zoroastrian community identity and religious continuity) is inseparable
 *   from asymmetric extraction (endogamy enforcement that disproportionately
 *   affects women, exclusion of children of mixed marriages from community
 *   institutions, loss of religious status for intermarried members). The
 *   reading coexists with four sibling readings (Hindu codified, Muslim
 *   shariat, Christian colonial, secular contractual) in India's legal
 *   pluralism framework, but demographic decline has intensified enforcement
 *   of endogamy rules, increasing suppression over the interval. Theater
 *   ratio is low (0.25) because the constraint's enforcement is functional
 *   rather than performative: community institutions actually exclude
 *   intermarried members, and the judiciary actually enforces the statutory
 *   provisions. The constraint is not a piton — the exclusionary mechanism
 *   operates as designed.
 *
 * KEY AGENTS:
 *   - Intermarried Parsi Women: Primary victims (powerless/identity_locked) — face community excommunication and loss of religious status; identity-locked by Zoroastrian belonging but structurally mobile to secular regime
 *   - Endogamous Parsi Families: Mixed position (moderate/constrained) — benefit from community institutions and religious continuity but constrained by endogamy enforcement and limited partner choice
 *   - Parsi Community Institutions: Primary beneficiaries (institutional/arbitrage) — Bombay Parsi Punchayet, Zoroastrian Trust Funds, fire temples; collect legitimacy and authority from codified personal law status
 *   - Parsi Reform Movement: Organized opposition (organized/mobile) — Association for Revival of Zoroastrianism, women's rights groups; advocate for gender-equal recognition and see endogamy as temporary demographic response
 *   - Indian Judiciary: Institutional enforcer (institutional/constrained) — constrained by constitutional commitment to personal law autonomy (Article 25-26) and fundamental rights enforcement (Article 14-15); cannot exit the tension
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination-extraction coupling that defines tangled rope at the analytical level
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parsi_community_reading, 0.32).
domain_priors:suppression_score(parsi_community_reading, 0.48).
domain_priors:theater_ratio(parsi_community_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parsi_community_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(parsi_community_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(parsi_community_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(parsi_community_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(parsi_community_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parsi_community_reading, tangled_rope).
narrative_ontology:human_readable(parsi_community_reading, "Parsi Community Marriage Authority (Zoroastrian Codified Reading)").
narrative_ontology:topic_domain(parsi_community_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(parsi_community_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parsi_community_reading, '0b44235c-e892-43dd-8d7a-3bc3ed459d8c').
narrative_ontology:cs_kernel_codification('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', formalized).
narrative_ontology:cs_authority_grounding('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', lineage).
narrative_ontology:cs_interpretation_layer_present('0b44235c-e892-43dd-8d7a-3bc3ed459d8c').
narrative_ontology:cs_reading_relation('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', parsi_community_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', parsi_community_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', parsi_community_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', parsi_community_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', foundational, zoroastrian_identity_through_endogamy).
narrative_ontology:cs_axiom_status(zoroastrian_identity_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', zoroastrian_identity_through_endogamy, conventional).
narrative_ontology:cs_axiom('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', foundational, community_boundary_maintenance_necessity).
narrative_ontology:cs_axiom_status(community_boundary_maintenance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', community_boundary_maintenance_necessity, instrumental).
narrative_ontology:cs_axiom('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', secondary, statutory_codification_legitimacy).
narrative_ontology:cs_axiom_status(statutory_codification_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', statutory_codification_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', zoroastrian_endogamous_continuity).
narrative_ontology:cs_drift_state('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', contemporary_demographic_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b44235c-e892-43dd-8d7a-3bc3ed459d8c', '').
narrative_ontology:cs_kernel_id(parsi_community_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parsi_community_reading, parsi_community_institutions).
narrative_ontology:constraint_beneficiary(parsi_community_reading, endogamous_parsi_families).
narrative_ontology:constraint_beneficiary(parsi_community_reading, parsi_religious_authorities).
narrative_ontology:constraint_victim(parsi_community_reading, intermarried_parsi_women).
narrative_ontology:constraint_victim(parsi_community_reading, children_of_mixed_marriages).
narrative_ontology:constraint_victim(parsi_community_reading, parsi_men_with_non_parsi_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(parsi_community_reading, endogamous_parsi_families).
narrative_ontology:constraint_victim(parsi_community_reading, indian_judiciary).
narrative_ontology:constraint_vindicates(parsi_community_reading, community_autonomy_doctrine).
narrative_ontology:constraint_vindicates(parsi_community_reading, religious_personal_law_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parsi women who marry non-Parsi men face community excommunication, loss of fire temple access, exclusion from community institutions, and denial of religious status to their children. They are structurally mobile (can access secular marriage regime under Special Marriage Act) but identity-locked: exiting to secular regime requires abandoning Zoroastrian religious identity and community belonging. The identity lock is cognitive rather than material — the barrier is internal (self-concept constituted through community membership) rather than external (legal prohibition).
narrative_ontology:constraint_stakeholder(parsi_community_reading, intermarried_parsi_women, payer,
    powerless, biographical, identity_locked, national).

% Parsi families who maintain endogamy benefit from community institutions (fire temples, religious ceremonies, Zoroastrian Trust Funds, community dispute resolution) and religious continuity. They also bear costs: pressure to maintain endogamy limits partner choice, community authority over personal status decisions, and social obligation to enforce exclusionary norms against intermarried relatives. Mixed position: genuine coordination alongside coercive overhead.
narrative_ontology:constraint_stakeholder(parsi_community_reading, endogamous_parsi_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(parsi_community_reading, endogamous_parsi_families, payer).

% Bombay Parsi Punchayet, Zoroastrian Trust Funds, fire temples, and community religious authorities administer the codified personal law regime. They set the agenda (interpret statutory provisions, enforce endogamy rules, control access to community resources) and collect legitimacy and authority from the arrangement. They can engage with the secular legal system when advantageous (arbitrage exit) and experience the constraint as coordination solving the problem of minority community preservation.
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_community_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Association for Revival of Zoroastrianism, Parsi women's rights groups, and reformist religious scholars advocate for gender-equal recognition of intermarried members and their children. They are excluded from agenda-setting authority (community institutions control interpretation) but organized enough to build alternative pathways. They see the endogamy enforcement as a temporary demographic crisis response that should sunset as alternative identity-maintenance mechanisms mature.
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_reform_movement, excluded,
    organized, generational, mobile, national).

% Indian courts enforce the Parsi Marriage and Divorce Act (1936) and adjudicate disputes under the personal law regime. They are constrained by constitutional commitment to religious personal law autonomy (Article 25-26) and fundamental rights enforcement (Article 14-15). They cannot exit this tension without constitutional amendment. They experience the constraint as mixed coordination (personal law pluralism governs religious diversity) and extraction (must enforce community norms that conflict with constitutional equality guarantees).
narrative_ontology:constraint_stakeholder(parsi_community_reading, indian_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(parsi_community_reading, indian_judiciary, payer).

% Children of Parsi-non-Parsi marriages are denied Zoroastrian religious status, fire temple access, and community institution participation regardless of their own religious practice or identity. They are trapped (cannot access the community regime their Parsi parent belongs to) and bear extraction without having chosen the constraint. They are the clearest victims of the endogamy enforcement mechanism.
narrative_ontology:constraint_stakeholder(parsi_community_reading, children_of_mixed_marriages, payer,
    powerless, biographical, trapped, national).

% Parsi men who marry non-Parsi women face formal community exclusion rules similar to intermarried Parsi women, but empirical evidence suggests asymmetric enforcement: men retain more community access than women. They are constrained (face social costs and some institutional exclusion) but not identity-locked (the cognitive barrier is weaker for men). The gender asymmetry raises extractiveness and validates the snare perspective for women.
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_men_with_non_parsi_spouses, payer,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Parsi community reading solves the coordination problem of preserving Zoroastrian religious identity and community continuity for a small, demographically declining diaspora minority (approximately 60,000 members in India as of 2021). The codified personal law regime provides: religious marriage ceremonies, community dispute resolution, inheritance frameworks, fire temple access protocols, and institutional mechanisms for transmitting Zoroastrian religious practice across generations.
% TRANSFER_FUNCTION: The arrangement transfers legitimacy and authority from the state to Parsi community institutions (Bombay Parsi Punchayet, religious authorities, fire temples). It transfers community status and religious access from intermarried members to endogamous members. It transfers agenda-setting power from individual Parsis to community institutions. It transfers the cost of demographic decline from the community as a whole to intermarried members and their children, who are excluded from community institutions.
% ABSENT_VOICES: Intermarried Parsi women and children of mixed marriages are structurally excluded from agenda-setting authority. Community institutions control interpretation of the statutory provisions, and reform movements have not displaced this authority. The excluded voices would object to: gender-asymmetric enforcement of endogamy rules, denial of religious status to children of mixed marriages, loss of fire temple access for intermarried members, and the framing of endogamy as theological necessity rather than demographic strategy. They are excluded because community institutions hold interpretive authority under the lineage-grounded commitment system, and the Indian judiciary defers to community interpretation under constitutional personal law autonomy (Article 25-26).
% DISAPPEARANCE_RATIONALE: If the Parsi community reading disappeared overnight (statutory repeal or judicial invalidation), the world would rearrange: intermarried Parsis would seek recognition under alternative regimes (secular contractual reading, Hindu codified reading if applicable), community institutions would lose statutory enforcement authority, fire temple access protocols would become purely voluntary rather than legally enforceable, inheritance disputes would migrate to secular courts, and the demographic decline trajectory would accelerate (no legal mechanism to enforce endogamy). The constraint organizes real arrangements — it is not a natural fact.
% FOUNDING_PROBLEM: The Parsi Marriage and Divorce Act (1936) was enacted to address two founding problems: (1) legal uncertainty about Parsi marriage validity and inheritance rights under British colonial law, which did not recognize Zoroastrian religious marriages; (2) demographic anxiety about community decline and assimilation pressure in the Indian independence movement context. The Act codified community norms into statutory law to secure legal recognition and enforce endogamy as a community preservation strategy.
% FOUNDING_PROBLEM_CORROBORATION: The legal uncertainty problem (founding problem 1) is DEAD: Parsi marriages are now legally recognized, and inheritance frameworks are established. The demographic decline problem (founding problem 2) is LIVE but contested: the community is still declining (60,000 members in 2021 vs 114,000 in 1941), but reformers argue that endogamy enforcement accelerates decline by excluding intermarried members and their children rather than preventing it. Corroboration: demographic data from Census of India (decline documented by state statistical authority, outside beneficiary set); legal recognition confirmed by Indian judiciary (secular authority); reform movement assessment (Association for Revival of Zoroastrianism, outside traditional beneficiary institutions). The founding problem status is contested because community institutions claim demographic decline justifies continued enforcement, while reformers claim enforcement causes decline.
narrative_ontology:disappearance_verdict(parsi_community_reading, world_rearranges).
narrative_ontology:founding_problem_status(parsi_community_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERMARRIED PARSI WOMAN (SNARE) — Identity-locked by Zoroastrian religious identity and community belonging, but structurally mobile (can access secular marriage regime). Faces community excommunication and loss of religious status upon marrying outside the community. The coordination story (preserving Zoroastrian endogamy) is cover for asymmetric extraction: men who marry out retain more community access than women who marry out. The identity lock is cognitive — exit would require abandoning Zoroastrian identity, not just paying a material cost.
constraint_indexing:constraint_classification(parsi_community_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: ENDOGAMOUS PARSI FAMILY (TANGLED ROPE) — Constrained by community norms but benefits from institutional recognition and religious continuity. Experiences genuine coordination (community marriage institutions provide religious ceremonies, dispute resolution, inheritance frameworks) alongside extraction (must accept community authority over personal status, limited choice in partner selection, pressure to maintain endogamy). Mixed experience: coordination function is real but comes with coercive overhead.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARSI COMMUNITY INSTITUTIONS (ROPE) — Primary beneficiary with arbitrage exit options (can engage with secular legal system when advantageous). Experiences the constraint as coordination: codified community norms solve the genuine problem of maintaining Zoroastrian religious identity and community continuity in a minority diaspora context. Low effective extraction because the institution collects legitimacy and authority from the arrangement.
constraint_indexing:constraint_classification(parsi_community_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARSI REFORM MOVEMENT (SCAFFOLD) — Organized reformers (Association for Revival of Zoroastrianism, Parsi women's rights groups) see the endogamy enforcement as a temporary demographic crisis response that should sunset as the community stabilizes alternative identity-maintenance mechanisms. They advocate for gender-equal recognition of intermarried members and children of mixed marriages. The constraint is transitional: its justification is demographic survival, not permanent theological necessity.
constraint_indexing:constraint_classification(parsi_community_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INDIAN JUDICIARY (TANGLED ROPE) — Constrained by constitutional commitment to religious personal law autonomy (Article 25-26) but also tasked with enforcing fundamental rights (Article 14-15). Experiences genuine coordination (personal law pluralism solves the problem of governing a religiously diverse polity) alongside extraction (must enforce community norms that conflict with constitutional equality guarantees). The judiciary cannot exit this tension without constitutional amendment.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the Parsi community reading instantiates a genuine coordination function (minority community preservation through codified religious norms) that is inseparable from asymmetric extraction (gender-asymmetric endogamy enforcement, exclusion of intermarried members and their children). The coordination and extraction are structurally coupled: the same institutional mechanism that preserves community identity also enforces exclusionary boundaries. This is the definitional case of tangled rope at the analytical level.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parsi_community_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parsi_community_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parsi_community_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(parsi_community_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The Parsi community reading extracts from intermarried members (loss of community status, religious access, inheritance rights) and disproportionately from women (gender-asymmetric enforcement documented in empirical studies). However, extraction is not as severe as pure snare because: (1) exit to secular marriage regime is legally available with moderate cost, (2) the coordination function (community preservation) is genuine for endogamous members, (3) enforcement is transparent and codified rather than arbitrary. The value has increased over the interval (0.20 → 0.32) as demographic decline intensified endogamy enforcement. Suppression (0.48): Moderate-high. Significant barriers to exit include identity-lock (Zoroastrian religious identity is constituted through community belonging), social ostracism, loss of access to fire temples and religious ceremonies, exclusion from community institutions, and inheritance complications. Suppression has increased over the interval (0.35 → 0.48) as demographic anxiety tightened enforcement. Theater ratio (0.25): Low. The constraint's enforcement is functional: community institutions actually exclude intermarried members, fire temples actually deny access, the judiciary actually enforces statutory provisions. The low theater distinguishes this from a piton — the mechanism operates as designed, not as performance. Theater has increased modestly (0.15 → 0.25) as some enforcement has become ritualized, but remains substantially functional.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across power and exit dimensions. Parsi community institutions (institutional/arbitrage) see rope — they experience the constraint as coordination solving the genuine problem of minority community preservation. Endogamous Parsi families (moderate/constrained) see tangled rope — genuine coordination (religious continuity, community institutions) inseparable from extraction (endogamy enforcement, limited choice). Intermarried Parsi women (powerless/identity_locked) see snare — the coordination story is cover for exclusionary extraction, and the identity lock prevents exit despite legal availability of secular alternatives. The Parsi reform movement (organized/mobile) sees scaffold — the endogamy enforcement is a temporary demographic crisis response that should sunset as alternative identity-maintenance mechanisms mature. The Indian judiciary (institutional/constrained) sees tangled rope — constitutional pluralism creates genuine coordination (governing religious diversity) inseparable from extraction (enforcing norms that conflict with equality guarantees). The analytical observer sees tangled rope at the civilizational level — the coordination and extraction are structurally coupled in the mechanism itself, not merely in different agents' experiences of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Parsi community institutions are primary beneficiaries with arbitrage exit (can engage secular legal system when advantageous) → low d → low/negative chi. Endogamous Parsi families are mixed (beneficiaries of coordination, constrained by enforcement) with constrained exit → moderate d → moderate chi. Intermarried Parsi women are primary victims with identity_locked exit (structurally mobile but cognitively trapped by religious identity fusion) → high d → high chi. The identity lock is the critical structural feature: a Parsi woman who marries a non-Parsi man can legally access the secular marriage regime (Special Marriage Act, 1954) but cannot do so without abandoning her Zoroastrian religious identity and community belonging. The barrier is internal (identity constitution) rather than external (legal prohibition). Parsi men who marry out face similar formal rules but empirical evidence suggests asymmetric enforcement — men retain more community access than women, raising extractiveness. The Indian judiciary is constrained (cannot exit the personal law / fundamental rights tension without constitutional amendment) and experiences the constraint as mixed coordination-extraction → moderate d → moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a transitional state between rope and snare but a stable structural configuration where coordination and extraction are inseparable. The Parsi community reading cannot be decomposed into 'the coordination part' (community preservation) and 'the extraction part' (endogamy enforcement) because the same institutional mechanism performs both functions simultaneously. Removing the extraction (allowing intermarried members full community status) would dissolve the coordination function (endogamous community preservation). The constraint is not mandatrophy — it is functioning exactly as designed. The reform movement's scaffold perspective represents a genuine alternative pathway (gender-equal recognition, alternative identity mechanisms) but has not yet displaced the tangled rope structure. The constraint will resolve to scaffold only if the reform movement succeeds in building alternative coordination mechanisms that preserve Zoroastrian identity without exclusionary enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the marriage authority kernel grounded in religious doctrine (making this reading a theological commitment) or in community survival strategy (making it a demographic policy)?',
    'Historical analysis of Parsi Matrimonial Disputes Act (1936) legislative debates and community discourse; comparison with pre-codification religious texts vs post-codification judicial interpretations',
    'If theological: the reading is a lineage-grounded commitment system with low mutability. If demographic: the reading is an instrumental policy with higher mutability as demographic conditions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether marriage authority is theological commitment or demographic strategy').

omega_variable(
    gender_asymmetry_measurement,
    'Does the Parsi community reading enforce gender-symmetric endogamy rules, or do men who marry out retain more community access than women who marry out?',
    'Empirical survey of community status outcomes for intermarried men vs women; analysis of fire temple access, inheritance rights, and community institution participation by gender',
    'If symmetric: extractiveness is lower (0.20-0.25 range) and the coordination function is stronger. If asymmetric: extractiveness is higher (0.35-0.45 range) and the snare perspective is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_asymmetry_measurement, empirical, 'Whether endogamy enforcement is gender-symmetric or asymmetric').

omega_variable(
    secular_exit_cost,
    'What is the actual cost for a Parsi individual to exit to the secular marriage regime while retaining Zoroastrian religious practice?',
    'Interviews with intermarried Parsis who attempted to maintain religious practice; documentation of fire temple access policies, navjote (initiation) availability for children of mixed marriages, and community institution participation rules',
    'If low cost: exit_options should be ''mobile'' rather than ''identity_locked'' for the powerless perspective, reducing effective extraction. If high cost: identity_locked is correct and extraction is substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_exit_cost, empirical, 'Cost of exiting to secular regime while maintaining religious practice').

omega_variable(
    sibling_reading_coexistence,
    'Do the five sibling readings (Hindu codified, Muslim shariat, Christian colonial, secular contractual, Parsi community) coexist as parallel legal regimes, or does one reading''s institutional dominance create structural pressure on the others?',
    'Analysis of Indian Supreme Court personal law jurisprudence; measurement of cross-regime migration rates; identification of institutional resource asymmetries (state funding, judicial deference, legislative attention)',
    'If parallel coexistence: all readings are equally stable. If hierarchical: the dominant reading (likely Hindu codified or secular contractual) influences the others through legitimacy pressure and resource allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether sibling readings coexist symmetrically or hierarchically').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parsi_community_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_mar_theater_1936, parsi_community_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(parsi_mar_theater_1966, parsi_community_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(parsi_mar_theater_1986, parsi_community_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(parsi_mar_theater_2006, parsi_community_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(parsi_mar_extract_1936, parsi_community_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(parsi_mar_extract_1966, parsi_community_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(parsi_mar_extract_1986, parsi_community_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(parsi_mar_extract_2006, parsi_community_reading, base_extractiveness, 70, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(parsi_mar_suppress_1936, parsi_community_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(parsi_mar_suppress_1966, parsi_community_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(parsi_mar_suppress_1986, parsi_community_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(parsi_mar_suppress_2006, parsi_community_reading, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parsi_community_reading, identity_coordination).
narrative_ontology:affects_constraint(parsi_community_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(parsi_community_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(parsi_community_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into five structurally distinct readings (Hindu codified, Muslim shariat, Christian colonial, secular contractual, Parsi community), each with different ε values reflecting different coordination/extraction balances. The Parsi reading is linked to sibling readings through India's legal pluralism framework: all five coexist as parallel personal law regimes, and institutional dynamics in one reading (e.g., reform pressure, judicial interpretation, demographic change) create structural pressure on the others. The readings are not independent constraints — they form a constraint family within the constitutional pluralism structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
