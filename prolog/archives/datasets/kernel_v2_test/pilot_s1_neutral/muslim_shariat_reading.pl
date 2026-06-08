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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Islamic Shariat Marriage Authority (Uncodified Juristic Reading)
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents one reading of the marriage authority kernel —
 *   the Islamic shariat interpretation that grounds marriage legitimacy in
 *   divine revelation (Quran/Hadith) as interpreted through classical Islamic
 *   jurisprudence (fiqh), with minimal state legislative interference in
 *   religious personal law. This reading has structured family law in
 *   Muslim-majority states and Muslim minority communities for over 1,400
 *   years. The constraint exhibits all classic features of tangled rope: it
 *   coordinates a genuine function (enabling religious pluralism by
 *   permitting non-state family law authority; protecting religious
 *   communities from secular state overreach) while simultaneously extracting
 *   from specific populations, primarily women at marriage dissolution and
 *   religious minorities excluded from coverage. The reading's core claim is
 *   that family authority derives from divine revelation, not human contract
 *   or state legislation — a theological assertion that appears as natural
 *   law from within the Islamic framework but reveals itself as a false
 *   summit when examined structurally: identifiable institutional
 *   beneficiaries (male household heads, juristic authorities, community
 *   elders) exist, enforcement is active (community pressure, religious court
 *   machinery, threat of social/spiritual exclusion), and the arrangement
 *   serves specific interests in maintaining patriarchal household authority
 *   and juristic institutional power.
 *
 * KEY AGENTS:
 *   - Married women (especially at dissolution): Primary victims (powerless/identity_locked) — structurally mobile in many contemporary contexts but identity-fused with Islamic faith and community; face severe exit costs including dowry loss, child custody disadvantage, economic insecurity, and spiritual exclusion from community
 *   - Male household heads: Primary beneficiaries (institutional/arbitrage) — benefit from unilateral or asymmetric divorce rights, polygamy permission, financial control, witness authority; experience the arrangement as natural religious law
 *   - Traditional Islamic juristic authority (Hanafi, Maliki, Shafi'i, Hanbali schools): Institutional beneficiaries (institutional/arbitrage) — maintain interpretive monopoly and deference from community; experience the constraint as coordination of legitimate religious authority without state interference
 *   - Reformist juristic community (modern Islamic scholars): Secondary actors (organized/constrained) — advocate revision (abolishing unilateral talaq, strengthening women's rights, limiting polygamy); experience the reading as constraining their interpretive innovation; benefit from enhanced legitimacy in modern states but face institutional resistance
 *   - Women's rights advocacy networks (transnational): Challengers (organized/constrained) — organize transnationally to reform extractive family law; coordinate accountability function while bearing suppression costs; constrained but gaining leverage in global human rights discourse
 *   - Religious minorities in Muslim-majority states (Hindu, Christian, Parsi, atheist): Secondary victims (powerless/trapped) — excluded from personal law coverage or subject to discriminatory parallel regimes; experience the constraint as legal marginalization enforced by state non-interference with shariat authority
 *   - Secularist state apparatus: Institutional performer (institutional/arbitrage) — maintains formal constitutional secularism while exempting religious family law from secular jurisdiction; benefits from avoidance of religious conflict but performs neutrality theatrically
 *   - Analytical observer: Sees false summit structure — the appearance of theological necessity masks contingent institutional arrangements that benefit identifiable actors and require active enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(muslim_shariat_reading, 0.68).
domain_priors:suppression_score(muslim_shariat_reading, 0.72).
domain_priors:theater_ratio(muslim_shariat_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(muslim_shariat_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(muslim_shariat_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(muslim_shariat_reading, "Islamic Shariat Marriage Authority (Uncodified Juristic Reading)").
narrative_ontology:topic_domain(muslim_shariat_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(muslim_shariat_reading, '2149dce5-faba-4437-9fa5-5aaa867a2e63').
narrative_ontology:cs_kernel_codification('2149dce5-faba-4437-9fa5-5aaa867a2e63', distributed).
narrative_ontology:cs_authority_grounding('2149dce5-faba-4437-9fa5-5aaa867a2e63', extraction).
narrative_ontology:cs_interpretation_layer_present('2149dce5-faba-4437-9fa5-5aaa867a2e63').
narrative_ontology:cs_reading_relation('2149dce5-faba-4437-9fa5-5aaa867a2e63', muslim_shariat_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('2149dce5-faba-4437-9fa5-5aaa867a2e63', muslim_shariat_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('2149dce5-faba-4437-9fa5-5aaa867a2e63', muslim_shariat_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('2149dce5-faba-4437-9fa5-5aaa867a2e63', muslim_shariat_reading__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('2149dce5-faba-4437-9fa5-5aaa867a2e63', foundational, divine_revelation_binding_authority).
narrative_ontology:cs_axiom_status(divine_revelation_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('2149dce5-faba-4437-9fa5-5aaa867a2e63', divine_revelation_binding_authority, deontological).
narrative_ontology:cs_axiom('2149dce5-faba-4437-9fa5-5aaa867a2e63', foundational, juristic_interpretation_as_legitimate_adaptation).
narrative_ontology:cs_axiom_status(juristic_interpretation_as_legitimate_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('2149dce5-faba-4437-9fa5-5aaa867a2e63', juristic_interpretation_as_legitimate_adaptation, conventional).
narrative_ontology:cs_axiom('2149dce5-faba-4437-9fa5-5aaa867a2e63', secondary, gender_asymmetric_rights_as_divinely_ordained).
narrative_ontology:cs_axiom_status(gender_asymmetric_rights_as_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('2149dce5-faba-4437-9fa5-5aaa867a2e63', gender_asymmetric_rights_as_divinely_ordained, empirically_contingent).
narrative_ontology:cs_axiom('2149dce5-faba-4437-9fa5-5aaa867a2e63', secondary, minimal_state_interference_in_religious_law).
narrative_ontology:cs_axiom_status(minimal_state_interference_in_religious_law, overridden).
narrative_ontology:cs_axiom_grounding('2149dce5-faba-4437-9fa5-5aaa867a2e63', minimal_state_interference_in_religious_law, instrumental).
narrative_ontology:cs_reference_frame('2149dce5-faba-4437-9fa5-5aaa867a2e63', divine_revelation_as_binding_family_law_authority).
narrative_ontology:cs_drift_state('2149dce5-faba-4437-9fa5-5aaa867a2e63', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2149dce5-faba-4437-9fa5-5aaa867a2e63', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, islamic_juristic_authority).
narrative_ontology:constraint_beneficiary(muslim_shariat_reading, community_elders).
narrative_ontology:constraint_victim(muslim_shariat_reading, women_at_marriage_dissolution).
narrative_ontology:constraint_victim(muslim_shariat_reading, women_post_divorce_economic_security).
narrative_ontology:constraint_victim(muslim_shariat_reading, religious_minorities_in_muslim_majority_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARRIED WOMAN AT DISSOLUTION (SNARE) — Identity fused with marriage and religious community membership. Structural mobility exists (can legally leave in most contemporary jurisdictions) but cognitive/identity frame makes exit unthinkable from within — abandoning marriage means abandoning Islamic identity framework, community belonging, and familial recognition. Triple talaq pre-2017 gave husband unilateral exit; woman faced severe exit costs (economic, social, spiritual) with no reciprocal right. Maximum experienced extraction. Exit costs include loss of dowry, child custody disputes weighted against mother, economic dependency, and religious community status.
constraint_indexing:constraint_classification(muslim_shariat_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORMIST JURISTIC COMMUNITY (TANGLED ROPE) — Modern Islamic jurists advocating reform (e.g., abolishing unilateral talaq, strengthening women's divorce rights, limiting polygamy) experience the constraint as mixed: they coordinate a genuine function (adapting divine revelation to contemporary contexts) while also bearing extraction costs (resistance from conservative scholars, institutional pressure, career risk from violating established school doctrines). They benefit from enhanced legitimacy in modern states but face suppression from traditional authorities. Constrained exit: reforming the reading requires surviving institutional and theological challenge.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRADITIONAL JURISTIC AUTHORITY (ROPE) — Classical Islamic jurisprudence (Hanafi, Maliki, Shafi'i, Hanbali schools) experiences the constraint as coordination: it solves the genuine problem of legitimizing marriage authority within a religious framework without state codification. The juristic tradition benefits from this arrangement (institutional prestige, community deference, control over family law), experiences it as natural coordination, and has exit options (can shift interpretation within established schools or rely on state non-interference). Arbitrage: can maneuver between traditional and modern readings depending on political context.
constraint_indexing:constraint_classification(muslim_shariat_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: WOMEN'S RIGHTS ADVOCACY NETWORKS (TANGLED ROPE) — Transnational networks (Human Rights Watch, Amnesty International, Muslim women's organizations) coordinate across borders to challenge extractive family law while respecting religious pluralism. They coordinate genuine function (accountability for gender-based harm) while bearing extraction costs (state retaliation, accusations of cultural imperialism, funding restrictions in conservative contexts). Constrained but organized — they have some capacity to build alternative narratives and political pressure, but face institutional barriers.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULARIST STATE LAW (PITON) — In nominally secular constitutional states with large Muslim populations (India, Indonesia, Egypt pre-2017), state law formally recognizes Islamic personal law but maintains a performative separation between 'religion' and 'law'. The state preserves formal constitutional secularism through theater while actual family law is administered by religious courts or community authorities. The theater ratio is high (0.38 suggests moderate performance, but the state's role is substantially theatrical — codifying secular principles while exempting religious family law from application). Exit from this arrangement is structurally degraded — the state has abandoned substantive governance of family law but performs secular authority; reformers must navigate both state institutions and religious courts.
constraint_indexing:constraint_classification(muslim_shariat_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THEOLOGICAL NECESSITY / NATURAL LAW VIEW (MOUNTAIN) — From within the Islamic theological framework, marriage authority deriving from divine revelation (Quran/Hadith) appears as immutable natural law: God has revealed the principles; human juristic interpretation applies them to circumstances. No alternative exists that maintains the religious framework — rejecting divine revelation as source of authority dissolves the Islamic commitment. However, the structural data reveals this as a false summit: identifiable beneficiaries (male household heads, juristic authority structures) exist; enforcement is active (community pressure, religious court authority); the arrangement serves specific institutional interests. The 'necessity' is theological only within the chosen framework; from outside the framework, the arrangement is contingent and extractive.
constraint_indexing:constraint_classification(muslim_shariat_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: COMPARATIVE PLURALISM OBSERVER (TANGLED ROPE) — From a comparative constitutional law perspective, the arrangement coordinates a genuine function (enabling religious pluralism by permitting non-state family law authority) while extracting from specific populations (women with fewer exit options than men, religious minorities excluded from personal law coverage). The constraint exhibits genuine hybrid character: coordination of religious autonomy + asymmetric extraction along gender and religious lines. Neither pure rope nor pure snare — the mixed character is structural, not perspectival.
constraint_indexing:constraint_classification(muslim_shariat_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
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

test(piton_threshold) :-
    domain_priors:theater_ratio(muslim_shariat_reading, TR),
    TR >= 0.70.

:- end_tests(muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderately high. The constraint extracts significantly from women at marriage dissolution through asymmetric rights (unilateral talaq pre-2017, unequal inheritance, disadvantaged witness authority, child custody bias). It extracts from religious minorities through discriminatory personal law regimes or exclusion. However, the extraction is not maximal (0.9+) because: (1) the arrangement does coordinate a genuine function (religious pluralism), (2) some reform pathways exist (reformist juristic reinterpretation, state-level legal change like India's triple talaq ban 2017, community pressure from women's organizations), and (3) contemporary juristic voices argue for more equitable implementations while maintaining the theological commitment. The trajectory shows increasing extractiveness from t=0 (0.52) to t=6 (0.68) as educational access and women's economic participation have increased the gap between formal rights and lived experience, then plateau at t=10 suggesting stabilization as reform movements reach institutional recognition (India's talaq ban, Egypt's khul' reforms, Malaysia's joint talaq rights). Suppression (0.72): Moderately high. Significant barriers to exit and reform include: (1) identity fusion — exiting marriage means exiting Islamic community identity for many women; (2) economic dependency — women's inheritance disadvantage and restricted guardianship over financial decisions; (3) legal barriers — religious courts' enforcement machinery and state non-interference doctrine; (4) social barriers — honor, shame, family pressure, threat of custody loss; (5) institutional resistance — conservative juristic authorities defending traditional interpretations; (6) theological framing as divine necessity rather than human arrangement. Suppression is slightly declining at t=10 (0.65) due to rising state regulation (formal tallaq registration, mandatory counseling, joint talaq rights in some jurisdictions) and transnational advocacy capacity. Theater ratio (0.38): Relatively low, indicating high functional content. This constraint does actually organize authority structures and enforce rules — it is not primarily performative. The theater is lower here than in many institutional constraints because the enforcement mechanisms (community pressure, religious court authority, family honor dynamics) are substantive rather than merely symbolic. The slight rise from t=0 (0.25) to t=6 (0.38) reflects increasing state ceremonialism (formal recognition ceremonies, state-registered religious courts, constitutional provisions) as classical juristic authority increasingly relies on state machinery for enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme and structural. The married woman at marriage dissolution (identity_locked exit) sees a snare: she is trapped by identity fusion with the Islamic faith and community, facing severe exit costs despite nominal structural mobility. The traditional juristic authority (arbitrage exit) sees rope: they coordinate a legitimate religious function, benefit from community deference, and have maneuvering room to adapt interpretations. The male household head sees rope: he benefits from the arrangement, experiences it as religiously sanctioned, and has exit options (can initiate talaq, can take additional wives, can reallocate resources). The reformist juristic community sees tangled rope: they coordinate genuine theological work (adapting revelation to modern contexts) while being suppressed by conservative institutions and facing career risk. Women's rights advocates see tangled rope: they coordinate accountability function while being constrained by state non-interference doctrine and institutional resistance. The secular state apparatus sees piton: they perform neutrality while the actual authority structures (religious courts, community enforcement) operate in the gaps. The theological observer sees mountain (false summit): divine revelation appears as natural law until structural analysis reveals beneficiaries and enforcement machinery. The comparative pluralism observer sees tangled rope: the arrangement genuinely coordinates religious autonomy while asymmetrically extracting from women and minorities. No single perspective produces the same classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary dramatically across stakeholder groups. Male household heads experience d ≈ 0.15 (strongly beneficiary, arbitrage exit) — the constraint subsidizes their authority and they have flexibility. Female spouses at dissolution experience d ≈ 0.85 (strongly target, identity_locked exit) — maximum extraction because they bear costs while identity fusion prevents exit. Reformist jurists experience d ≈ 0.55 (near symmetric, constrained exit) — they coordinate but are constrained by institutional pressure. Traditional juristic authority experiences d ≈ 0.10 (beneficiary, arbitrage exit) — they collect institutional prestige and deference. Women's rights advocates experience d ≈ 0.70 (high target, constrained exit) — they bear institutional suppression costs. Religious minorities experience d ≈ 0.75 (target, trapped exit) — excluded from protection or subject to discriminatory regimes. The engine derives these from the beneficiary/victim declarations and exit modulation, producing a highly differentiated field where effective extraction chi varies by more than an order of magnitude across actor positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The mandate of this constraint is 'enable religious pluralism and protect religious communities from secular state overreach in family law.' This mandate is LIVE — it remains functionally justified by the need for religious autonomy in plural societies. However, the constraint exhibits severe mandate obsolescence in one direction: it protects religious authority from state interference while simultaneously failing to protect women and minorities from religious authority. The original mandate (protection from secular overreach) has been achieved in most contemporary contexts — secular states generally do not legislate religious family law — but the constraint persists beyond its original justification, now functioning primarily as protection of patriarchal authority from gender-equality oversight. The constraint is not a piton (which would show high theater) — it is a true tangled rope showing mandate partial obsolescence. The 'coordination of religious pluralism' mandate is satisfied; the 'protection of vulnerable populations from patriarchal extraction' mandate is not satisfied. This asymmetric mandate obsolescence (one function remains live, another is dead) is the diagnostic signature of a snare masquerading as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_foreclosure_vs_coexistence,
    'Does the shariat reading''s core premise (divine revelation as binding authority for family law) logically foreclose the secular contractual reading, or do both remain live readings that coexist across different parties?',
    'Test whether a single legal framework could simultaneously hold (a) divine revelation as binding authority for family law, and (b) marriage as a secular contract revocable by mutual consent. If logically impossible in one framework: foreclosure relation. If live in different parties'' frameworks: coexistence.',
    'Foreclosure: the two readings cannot be unified — legal pluralism requires separate jurisdictional domains. Coexistence: both readings remain live as competing claims within the same state, producing legal fragmentation and access inequality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_foreclosure_vs_coexistence, conceptual, 'Whether shariat and secular contractual readings logically foreclose each other').

omega_variable(
    triple_talaq_axiom_status,
    'Is unilateral male talaq (pre-2017 form) a foundational axiom of the shariat reading, or a secondary implication that the reading can abandon while maintaining its core commitment to divine revelation as authority?',
    'Historical analysis of juristic debate: did classical jurists argue talaq rights flow directly from Quranic verse, or derive them as secondary implications? Post-2017 India: did shariat scholars who accept the ban argue it violates the core reading, or that it clarifies/corrects secondary practices?',
    'If foundational: banning unilateral talaq forecloses the shariat reading (inconsistent with core premise). If secondary: the reading can adapt (status=overridden), maintaining theological commitment while changing family law implementation. This determines whether reform-responsive versions of the shariat reading remain coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triple_talaq_axiom_status, empirical, 'Whether unilateral talaq is foundational or secondary within the shariat axiom set').

omega_variable(
    state_enforcement_contradiction,
    'Can the ''minimal state interference'' axiom coexist with the ''requires active enforcement'' structural requirement in a modern state context?',
    'Empirical mapping: which enforcement mechanisms actually operate (state courts, religious courts, community pressure)? Historical trajectory: as states increased enforcement (codification of Islamic law, state recognition of religious courts, formalized registration), did the reading''s adherents defend ''minimal state'' principle or abandon it?',
    'If state enforcement is necessary for the reading''s operation in modern contexts, the ''minimal state'' axiom is overridden by contemporary reality. This reveals an inconsistency within the reading itself: it cannot simultaneously claim minimal state interference while relying on state institutions for enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_contradiction, empirical, 'Contradiction between minimal state interference axiom and actual enforcement mechanisms').

omega_variable(
    gender_asymmetry_theological_necessity,
    'Is the gender asymmetry in talaq rights, inheritance, and witness authority a direct scriptural requirement or a juristic interpretation subject to reconsideration in light of contemporary context?',
    'Quranic verse analysis and juristic commentary lineages: do classical scholars treat these as explicit divine commands or as contextual interpretations? Do contemporary reformist scholars identify alternative interpretive traditions that yield more symmetric rights?',
    'If scriptural requirement: gender asymmetry is foundational to the shariat reading and cannot be revised without abandoning the reading. If juristic interpretation: it can be reconsidered (status=holdable but subject to reinterpretation). This determines whether the reading is compatible with gender-equal family law structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_theological_necessity, conceptual, 'Whether gender asymmetries are scriptural necessities or juristic interpretations').

omega_variable(
    divine_authority_falsifiability,
    'What would constitute evidence that this reading''s grounding claim (divine revelation as authority source) is false? Is there any conceivable empirical or logical evidence that would lead adherents to abandon the reading?',
    'Philosophical analysis of the axiom''s grounding type: is it empirically contingent (subject to falsification), deontological (not falsifiable), or self-reinforcing (non-falsifiable by design)? Interview adherents: what evidence would change their view?',
    'Non-falsifiable readings are structurally protected from refutation but also protected from verification — they are stable but insulated from reality-testing. This affects classification: a non-falsifiable mountain may be analytically indistinguishable from a false summit. High non-falsifiability supports the false summit interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_authority_falsifiability, conceptual, 'Falsifiability status of divine revelation as binding authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(muslim_shariat_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msr_tr_t0, muslim_shariat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(msr_tr_t3, muslim_shariat_reading, theater_ratio, 3, 0.33).
narrative_ontology:measurement(msr_tr_t6, muslim_shariat_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(msr_tr_t10, muslim_shariat_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(msr_be_t0, muslim_shariat_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(msr_be_t3, muslim_shariat_reading, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(msr_be_t6, muslim_shariat_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(msr_be_t10, muslim_shariat_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(msr_su_t0, muslim_shariat_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(msr_su_t3, muslim_shariat_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(msr_su_t6, muslim_shariat_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(msr_su_t10, muslim_shariat_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, secular_contractual_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, parsi_community_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, christian_colonial_reading).
narrative_ontology:affects_constraint(muslim_shariat_reading, legal_pluralism_enforcement_gap).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into five distinct constraints, each representing a different reading's authority structure with different ε values. The shariat reading (this story) has ε=0.68 reflecting substantial gender asymmetry and extraction from women. The secular contractual reading (separate story) has lower ε reflecting equality assumptions (though often fails in implementation). The network links show mutual influence: the shariat reading's existence depends on state non-interference (affects legal_pluralism_enforcement_gap); reformist movements within the shariat reading influence the secular reading by challenging assumptions about what constitutes religious necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(muslim_shariat_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
