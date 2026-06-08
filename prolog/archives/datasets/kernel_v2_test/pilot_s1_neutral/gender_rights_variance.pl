% ============================================================================
% CONSTRAINT STORY: gender_rights_variance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gender_rights_variance, []).

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
 *   constraint_id: gender_rights_variance
 *   human_readable: Gender Rights Variance in India's Parallel Personal-Law Regime
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   India's post-independence constitutional order has maintained five
 *   separate personal-law regimes (Hindu, Muslim, Christian, Parsi, and
 *   secular) governing marriage, divorce, inheritance, and guardianship —
 *   creating a fragmented legal landscape where identical family situations
 *   receive radically different legal treatment depending on the parties'
 *   religion. This constraint is the interaction between two competing
 *   constitutional commitments: Articles 14 and 15 guarantee gender equality;
 *   Articles 25-28 guarantee religious autonomy and minority cultural
 *   protection. The kernel contest is whether 'marriage and family law
 *   authority' is a single commitment read multiple ways (different religious
 *   communities reading one constitutional mandate differently) or multiple
 *   incoherent kernels coexisting without synthesis. The constraint's
 *   classification as Tangled Rope reflects that it simultaneously
 *   coordinates genuine religious minority protection AND enables systematic
 *   gender discrimination through the same mechanism. Women in restrictive
 *   jurisdictions experience identity-lock: they are structurally mobile
 *   (could migrate to secular jurisdiction, convert, flee the family) but
 *   identity-bound to the community system through religious identity, family
 *   networks, and sense of self constituted within community frameworks. The
 *   extractiveness metric shows accumulation: post-independence frameworks
 *   began with moderate extraction (0.35) as constitutional ideals were newly
 *   minted; extraction has accumulated to 0.62 by contemporary period as the
 *   gap between constitutional promises and actual implementation widened,
 *   and as patriarchal gatekeepers learned to use 'religious autonomy'
 *   framing to shield discriminatory rules from accountability. Suppression
 *   requirement has declined slightly (from 0.72 to 0.58) as alternative
 *   legal pathways have opened (women's rights organizations, constitutional
 *   petitions, some legislative reform), but suppression remains substantial
 *   because the core mechanisms (family dependence, social ostracism threat,
 *   identity-lock) persist. Theater ratio has risen sharply (0.42 to 0.68),
 *   indicating that the legitimacy maintenance of the parallel system has
 *   become increasingly performative: constitutional framers presented legal
 *   pluralism as principled minority accommodation; contemporary defenders
 *   frame it the same way, even as evidence accumulates that parallel systems
 *   primarily enable patriarchal gatekeeping rather than minority protection.
 *
 * KEY AGENTS:
 *   - Women in Restrictive Jurisdictions: Primary victim (powerless/identity_locked) — bears full extraction through unequal rights; cannot exit without abandoning identity and community belonging
 *   - Religious Community Authorities: Primary beneficiary (institutional/arbitrage) — define 'authentic' religious practice through law; maintain patriarchal control; extract through legal gatekeeping
 *   - Religious Minority Groups: Ambiguous stakeholder (moderate/mobile) — receive genuine legal protection against majoritarian homogenization, but protection mechanisms simultaneously enable internal gender oppression
 *   - Gender Rights Coalition: Secondary beneficiary (organized/mobile) — organizing for reform; has multiple strategic pathways (legislation, constitutional petition, international advocacy)
 *   - Secular State & Judiciary: Institutional actor (institutional/constrained) — constitutionally committed to both gender equality and religious autonomy; manages the tension; extracts through power to define scope of 'personal matters'
 *   - Constitutional Pluralism Doctrine: Vindicated proposition (non-agent) — the idea that multicultural states must accommodate religious legal autonomy; benefits from the constraint's persistence without collecting rents itself
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the parallel system as inherent to multicultural constitutionalism, missing that it is a contingent choice with identifiable beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_rights_variance, 0.62).
domain_priors:suppression_score(gender_rights_variance, 0.58).
domain_priors:theater_ratio(gender_rights_variance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_rights_variance, extractiveness, 0.62).
narrative_ontology:constraint_metric(gender_rights_variance, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gender_rights_variance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_rights_variance, tangled_rope).
narrative_ontology:human_readable(gender_rights_variance, "Gender Rights Variance in India's Parallel Personal-Law Regime").
narrative_ontology:topic_domain(gender_rights_variance, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(gender_rights_variance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gender_rights_variance, '59306f70-cf1b-4cc5-a537-400a39192c0f').
narrative_ontology:cs_kernel_codification('59306f70-cf1b-4cc5-a537-400a39192c0f', formalized).
narrative_ontology:cs_authority_grounding('59306f70-cf1b-4cc5-a537-400a39192c0f', lineage).
narrative_ontology:cs_interpretation_layer_present('59306f70-cf1b-4cc5-a537-400a39192c0f').
narrative_ontology:cs_reading_relation('59306f70-cf1b-4cc5-a537-400a39192c0f', gender_rights_variance__uniform_civil_code_reading, coexists_with).
narrative_ontology:cs_reading_relation('59306f70-cf1b-4cc5-a537-400a39192c0f', gender_rights_variance__reformed_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('59306f70-cf1b-4cc5-a537-400a39192c0f', gender_rights_variance__intersectional_reading, influences).
narrative_ontology:cs_axiom('59306f70-cf1b-4cc5-a537-400a39192c0f', foundational, religious_autonomy_foundational).
narrative_ontology:cs_axiom_status(religious_autonomy_foundational, holdable).
narrative_ontology:cs_axiom_grounding('59306f70-cf1b-4cc5-a537-400a39192c0f', religious_autonomy_foundational, deontological).
narrative_ontology:cs_axiom('59306f70-cf1b-4cc5-a537-400a39192c0f', foundational, gender_equality_foundational).
narrative_ontology:cs_axiom_status(gender_equality_foundational, holdable).
narrative_ontology:cs_axiom_grounding('59306f70-cf1b-4cc5-a537-400a39192c0f', gender_equality_foundational, deontological).
narrative_ontology:cs_axiom('59306f70-cf1b-4cc5-a537-400a39192c0f', secondary, authentic_doctrine_claim).
narrative_ontology:cs_axiom_status(authentic_doctrine_claim, overridden).
narrative_ontology:cs_axiom_grounding('59306f70-cf1b-4cc5-a537-400a39192c0f', authentic_doctrine_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('59306f70-cf1b-4cc5-a537-400a39192c0f', constitutional_pluralism_framework).
narrative_ontology:cs_drift_state('59306f70-cf1b-4cc5-a537-400a39192c0f', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('59306f70-cf1b-4cc5-a537-400a39192c0f', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_rights_variance, religious_community_authorities).
narrative_ontology:constraint_beneficiary(gender_rights_variance, male_family_heads).
narrative_ontology:constraint_beneficiary(gender_rights_variance, constitutional_pluralism_doctrine).
narrative_ontology:constraint_victim(gender_rights_variance, women_in_restrictive_jurisdictions).
narrative_ontology:constraint_victim(gender_rights_variance, religious_minorities).
narrative_ontology:constraint_victim(gender_rights_variance, marriage_equality_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN IN RESTRICTIVE JURISDICTION (SNARE) — Identity-locked through community membership, religious identity, and family networks. Structurally mobile (could physically migrate) but identity-bound to the community legal system. Cannot exercise exit without abandoning family identity, community belonging, and social standing. Experiences full extraction: unequal inheritance, limited divorce rights, guardianship restrictions, child custody inequality. Suppression is both structural (legal barriers, family economic dependency) and internalized (identity fusion with community norms).
constraint_indexing:constraint_classification(gender_rights_variance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORMIST MINORITY WOMAN (TANGLED ROPE) — Constrained by resource requirements, social ostracism risk, and coordination barriers with reform movements. Experiences mixed extraction and coordination benefit: the legal pluralism framework theoretically protects religious freedom (coordination function) but simultaneously enables gender discrimination within that framework (extraction mechanism). Exit costs are substantial (family rupture, community expulsion) but not insurmountable. Some capacity for collective action through women's rights organizations, but power asymmetry remains.
constraint_indexing:constraint_classification(gender_rights_variance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY AUTHORITY (ROPE) — Institutional actor with arbitrage options (can leverage competing constitutional interpretations, can frame gender rules as doctrinal authenticity rather than oppressive policy). Experiences the constraint as genuine coordination: managing religious identity preservation, maintaining community cohesion through law, protecting against secular homogenization. Extraction toward this agent flows from the ability to define 'authentic' religious practice and enforce it as law. Benefits from legal pluralism doctrine without bearing its gender-inequality costs.
constraint_indexing:constraint_classification(gender_rights_variance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GENDER RIGHTS COALITION (TANGLED ROPE) — Organized actors (women's rights NGOs, feminist legal scholars, secular civil society) see both coordination and extraction. The constraint has genuine coordination function (protecting religious autonomy for minorities) but is being weaponized as extraction (using religious autonomy to shield gender oppression). Coalition has mobile exit options (can work through legislative reform, constitutional amendment, international law advocacy) and some power through organized action. Extraction is substantial but not absolute because the coalition has multiple strategic pathways.
constraint_indexing:constraint_classification(gender_rights_variance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY & SECULAR STATE (TANGLED ROPE) — Institutional actor constrained by constitutional commitment to both gender equality (Part III) and minority religious rights (Article 25-28). Experiences genuine coordination problem: how to protect both rights simultaneously? Also extracts through management of the tension: judicial deference to religious law on 'personal matters' while retaining power to decide what counts as 'personal.' Active enforcement through Article 32 petitions, but enforcement capacity is limited by federalism, resource constraints, and political will. The state sees coordination (legitimate values conflict) and extraction (state power to define scope of 'personal law').
constraint_indexing:constraint_classification(gender_rights_variance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PARALLEL LEGAL STRUCTURE (PITON) — The institutional apparatus of parallel personal-law systems (separate courts, community judges, statutory codes for each religion) persists substantially through inertia and theatrical legitimacy maintenance rather than functional necessity. Colonial-era legal pluralism was built for administrative convenience; post-independence, it has been sustained as an expression of 'minority protection' and 'secular accommodation,' but the functional verification of whether parallel systems actually protect minorities or primarily enable gender extraction is contested. Theater ratio is high: constitutional framers presented legal pluralism as principled minority accommodation; actual operation is often patriarchal gatekeeping dressed in religious authenticity language.
constraint_indexing:constraint_classification(gender_rights_variance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the view that 'diverse societies must accommodate religious legal autonomy as an irreducible constitutional principle' risks appearing as natural law — a necessary feature of multicultural governance. However, this naturalizes what is actually a contingent institutional choice (India could have adopted uniform civil code; other nations have taken different paths). The mountain classification is a false summit candidate: the 'irreducibility' of religious pluralism is presented as natural law, but beneficiaries (religious authorities) exist and extract from the arrangement. Engine false-summit detection will reveal the naturalization.
constraint_indexing:constraint_classification(gender_rights_variance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_rights_variance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_rights_variance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_rights_variance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gender_rights_variance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_rights_variance, TR),
    TR >= 0.70.

:- end_tests(gender_rights_variance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial and accumulated. The parallel personal-law system extracts from women in restrictive jurisdictions through unequal inheritance (Muslim daughters receive half of sons' shares in some interpretations), unequal divorce rights (Muslim women have limited talaq rights; Hindu women historically had no divorce), unequal guardianship (women often denied guardianship of children), and unequal marital property rights. The extraction is not as severe as pure slavery or forced labor (which would be 0.8-0.95) because limited exit pathways exist (conversion, migration, secular Hindu code available), but it is substantial because identity-lock prevents most women from exercising exits. The accumulation from 0.35 to 0.62 reflects that post-independence frameworks created constitutional promises of equality that remain unfulfilled; the gap itself becomes extractive as women recognize unequal treatment but cannot escape it. Suppression (0.58): Substantial. Multiple layered mechanisms: structural barriers (legal prohibition, economic dependency on family, resource constraints), social barriers (community ostracism threat, family pressure, alliance threats), and internalized barriers (identity fusion with community norms, cognitive capture through religious framing as 'authentic' rather than oppressive). The slight decline from 0.72 to 0.58 reflects that women's rights organizations, constitutional petitions (Shah Bano case, Shayara Bano case), and some legislative reforms (Dissolution of Muslim Marriages Act 1939, post-Shah Bano amendments) have created modest alternative pathways and reduced the total suppression load, but the core mechanisms remain potent. Theater ratio (0.68): High and rising. Constitutional framers presented legal pluralism as principled accommodation of religious minorities and secular ideology coexisting — a genuine coordination solution to the problem of diverse religions in one state. But contemporary defenders of parallel systems appeal to the same framing (minority protection, secularism, religious autonomy) while empirical evidence increasingly reveals that the systems primarily enable patriarchal gatekeeping. The rise from 0.42 to 0.68 reflects that the gap between legitimacy claim and actual function has widened; the apparatus requires more theatrical performance to maintain its legitimacy as evidence of gender harm accumulates. This is diagnostic of piton-stage degradation: the system was built for a functional reason (colonial administrative pluralism); post-independence it was theoretically justified as minority protection; now it persists mainly through repeated assertion that minorities need protection while actual evidence of gender cost accumulates unaddressed.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are maximal because different observers see fundamentally different phenomena through the same institutional structure. For the woman subject to restrictive personal law, the structure is an unchangeable oppressive regime (Snare). For the religious authority, it is legitimate community self-governance (Rope). For the judicial system, it is a genuine constitutional contradiction (Tangled Rope). For the reform coalition, it is a mixed coordination-extraction problem with multiple resolution pathways (Tangled Rope). For the institutional apparatus itself as a set of mechanisms, it is increasingly theatrical — performing minority protection while evidence of gender harm accumulates (Piton). For the analytical observer, there is a risk of naturalizing the whole thing as inherent to multicultural constitutionalism (Mountain / false summit). These gaps are NOT measurement errors — they reflect real structural differences in how the constraint operates across different positions. The engine's perspectival array captures the full conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options through the pipeline (beneficiary + exit → low d; victim + trapped exit → high d). The woman in restrictive jurisdiction occupies the highest-extraction position: she is declared a victim (bears costs of unequal rights), her exit is identity_locked (structurally mobile but cognitively captured), so the engine derives d ≈ 1.0 (full target). The religious authority occupies the lowest-extraction position: beneficiary status + arbitrage exit → d ≈ 0.0 (full beneficiary). The reformist minority woman (victim + constrained exit) occupies intermediate extraction: d ≈ 0.65. The gender rights coalition (beneficiary of reform potential + mobile exit) occupies d ≈ 0.25. The judiciary (neither pure beneficiary nor victim, constrained by constitutional duty) occupies d ≈ 0.50. The variation in d across perspectives drives the perspectival gap: different agents with the same structural data experience radically different effective extraction because their structural positions differ. The piton classification for the institutional apparatus reflects high theater ratio (0.68), not from high chi — the apparatus extracts only moderately (through state power to define scope) but is maintained largely through performance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the parallel personal-law system was mandated to solve a genuine coordination problem (how can a multicultural secular state protect both religious autonomy and gender equality?) but the mandate has been co-opted by gatekeepers to solve a different problem (how can patriarchal religious authorities maintain control over women while claiming to protect minority rights?). The original mandate — minority religious protection — is live and defensible. The actual operation has drifted toward gatekeeping maintenance. The divergence between mandated function and actual operation is the symptom of resolved mandatrophy: the constraint persists not because the original coordination problem requires it, but because institutional gatekeepers benefit from the status quo and frame it in mandated language. The machinery for addressing this (constitutional petitions, legislative reform, international law) is active but slow, producing the tangled-rope classification rather than pure snare: the system both coordinates genuine minority protection and enables extraction, and reform pathways exist but are constrained by power asymmetries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_autonomy_vs_gender_equality,
    'Is the conflict between religious autonomy and gender equality a genuine irresolvable tension, or a false dichotomy constructed to protect patriarchal gatekeeping?',
    'Comparative constitutional analysis: examine democracies that have adopted uniform civil codes (Turkey, Tunisia) and assess whether religious autonomy and gender equality are genuinely incompatible or whether the incompatibility is specific to particular religious interpretations maintained by institutional gatekeepers. Analyze reform movements within each faith tradition claiming doctrinal support for gender equality.',
    'If genuinely irresolvable: mountain classification becomes more defensible (constraints on human rights become features, not bugs). If false dichotomy: tangled-rope classification confirmed; the ''autonomy'' framing is extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_vs_gender_equality, conceptual, 'Whether religious autonomy and gender equality are genuinely incompatible or a false dichotomy').

omega_variable(
    identity_lock_stability,
    'For women identity-locked to restrictive personal-law systems, how stable is the lock across generation change? Do daughters exit at higher rates than mothers?',
    'Longitudinal demographic data on inter-generational exit rates, particularly comparing women in restrictive vs. progressive jurisdictions; survey data on identity-fusion strength (do second-generation women see the legal rules as ''theirs'' or as imposed?); analysis of conversion/exit patterns post-legal reform.',
    'If lock weakens inter-generationally: identity_locked exit option becomes identity_locked → constrained → mobile trajectory. If stable: the binding mechanism is sustained cognitive capture, not just structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_stability, empirical, 'Stability of identity-lock across generations').

omega_variable(
    minority_protection_actualization,
    'Do parallel personal-law systems actually protect minority religious communities from majoritarian law, or do they primarily protect patriarchal gatekeepers from accountability?',
    'Case-by-case analysis: do protection claims cluster around gender/family matters (where gatekeepers have strongest interest) or across full personal law domains? Compare protection outcomes for religious minorities vs. within-religion outcomes for women. Examine whether communities requesting legal autonomy are doing so to protect authentic doctrinal practice or to avoid scrutiny of discriminatory rules.',
    'If actually protective of minority interests: coordination function is genuine (Rope classification more defensible). If primarily gatekeeping: pure extraction mechanism revealed (Snare from beneficiary side, Tangled Rope from state side).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_actualization, empirical, 'Whether parallel law actually protects minorities or primarily enables patriarchal gatekeeping').

omega_variable(
    uniform_civil_code_feasibility,
    'Is uniform civil code politically and socially feasible in India without becoming a secular majoritarian override of minority autonomy?',
    'Analysis of successful uniform code transitions (Turkey, Tunisia, Egypt); examination of failed or delayed Indian reform efforts (Shah Bano case fallout); assessment of whether gender-equality-preserving pluralism is a genuine third option vs. a contradiction in terms.',
    'If feasible: the current parallel system is a contingent choice, not a natural law necessity (undermines mountain classification). If infeasible: some version of gender inequality may be the structural cost of accommodating religious pluralism in deeply multicultural states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_civil_code_feasibility, conceptual, 'Feasibility of uniform civil code in multicultural context').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) primarily structural (legal barriers, economic dependency, resource constraints) or internalized (identity fusion, cognitive capture through religious framing)?',
    'Post-exit analysis: track women who have exited restrictive personal-law regimes and measure whether suppression persists (internalized) or dissolves (was primarily structural). Interview data on source of perceived constraint: is the barrier experienced as ''the law prevents this'' or ''my identity is constituted through this practice''?',
    'If primarily structural: suppression decreases as material barriers are removed. If primarily internalized: suppression persists after exit; the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    authentic_religious_doctrine_claim,
    'When religious authorities claim that gender-unequal rules are ''authentic religious doctrine,'' are they reporting discovered doctrine or constructing doctrine to maintain authority?',
    'Textual and historical analysis: examine whether gender rules in personal-law systems map to doctrinal texts or to contemporary gatekeepers'' interpretations. Analyze how doctrinal interpretations have changed over time and whether change correlates with external pressure (reform movements, legal challenges) or with community-internal doctrinal development.',
    'If discovering doctrine: religious autonomy claim has higher legitimacy (supports Rope perspective from authority side). If constructing doctrine: authenticity claim is performative (supports Piton classification of the authority structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_religious_doctrine_claim, conceptual, 'Whether gender rules are discovered doctrine or constructed authority maintenance').

omega_variable(
    false_summit_beneficiary_structure,
    'Is the classification of gender-rights variance as a natural feature of multicultural constitutionalism a false summit — naturalizing a contingent arrangement that benefits identifiable gatekeepers?',
    'Trace who uses ''legal pluralism is natural'' framing and what material benefits flow to them (religious authorities maintain power, state outsources controversial decisions, patriarchal structures persist unchallenged). Compare to societies that rejected this framing and examine whether gender equality was achievable without abandoning religious diversity.',
    'If false summit confirmed: the mountain classification is reclassified to tangled_rope or snare. The ''naturalness'' framing is revealed as extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, conceptual, 'Whether multicultural legal pluralism is natural law or false-summit naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_rights_variance, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grv_theater_1947, gender_rights_variance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(grv_theater_1972, gender_rights_variance, theater_ratio, 25, 0.58).
narrative_ontology:measurement(grv_theater_1997, gender_rights_variance, theater_ratio, 50, 0.65).
narrative_ontology:measurement(grv_theater_2022, gender_rights_variance, theater_ratio, 75, 0.68).

% Extraction over time
narrative_ontology:measurement(grv_extract_1947, gender_rights_variance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(grv_extract_1957, gender_rights_variance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(grv_extract_1972, gender_rights_variance, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(grv_extract_1987, gender_rights_variance, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(grv_extract_2022, gender_rights_variance, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(grv_suppress_1947, gender_rights_variance, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(grv_suppress_1972, gender_rights_variance, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(grv_suppress_1997, gender_rights_variance, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(grv_suppress_2022, gender_rights_variance, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_rights_variance, identity_coordination).
narrative_ontology:boltzmann_floor_override(gender_rights_variance, 0.12).
narrative_ontology:affects_constraint(gender_rights_variance, uniform_civil_code_feasibility).
narrative_ontology:affects_constraint(gender_rights_variance, talaq_polygamy_doctrine).
narrative_ontology:affects_constraint(gender_rights_variance, hindu_succession_inheritance_variance).

% DUAL FORMULATION NOTE:
% The gender-rights variance is downstream of the constitutional commitment to religious autonomy (Articles 25-28) and its interaction with the equality commitment (Articles 14-15). This story models the constraint at the level of the parallel system's operation. Upstream constraints include the constitutional kernel contest (what does religious autonomy mean in a secular state?) and specific doctrinal interpretations (what makes a marriage valid, how is divorce defined). Downstream constraints include specific gender-outcome variations (inheritance inequality, divorce inequality, guardianship inequality) and the political economy of reform (what reform is politically feasible?). The network reflects that changes in any upstream or downstream constraint cascade through the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gender_rights_variance, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
