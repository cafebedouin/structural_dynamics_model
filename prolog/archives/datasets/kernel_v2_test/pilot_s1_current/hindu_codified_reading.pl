% ============================================================================
% CONSTRAINT STORY: hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hindu_codified_reading, []).

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
 *   constraint_id: hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority (Civil Court Reading)
 *   domain: constitutional_pluralism/family_law/religious_governance
 *
 * SUMMARY:
 *   The Hindu Marriage Act (HMA) of 1955 codified Hindu marriage law in
 *   post-independence India, replacing the interpretive sphere of śāstric
 *   jurisprudence with state-administered uniform rules. This constraint
 *   instantiates one reading of the marriage authority kernel: the reading
 *   that grounds Hindu marriage validity in a codified statute as interpreted
 *   by civil courts. This reading coexists with four sibling readings (Muslim
 *   Shariah, Christian canonical, Parsi communal, and secular civil) in the
 *   contested multi-faith personal law system. The Hindu codified reading
 *   presents itself as coordination — providing uniform rules that replaced
 *   contested interpretations. However, the structural analysis reveals a
 *   tangled rope: the codification embedded brahmanical marriage norms
 *   (monogamy, patrilineal succession, male authority) into state law,
 *   simultaneously solving the coordination problem of what rules to apply
 *   while extracting authority from non-brahmanical communities and
 *   distributing gender-asymmetric property transmission. The measurement
 *   trajectory shows modest increase in extractiveness (0.28 → 0.37 over 70
 *   years) alongside declining suppression (0.48 → 0.39) — interpreted as
 *   increasing visibility of the extraction mechanism and generational
 *   friction from gender equity reform.
 *
 * KEY AGENTS:
 *   - Hindu Woman: powerless/trapped — navigates fault-based divorce, male-biased inheritance; experiences both coordination gains (legal marriage recognition) and extraction (property asymmetry)
 *   - Civil Court Judiciary: institutional/arbitrage — displaces pandits and community adjudicators; gains authority and applicability clarity from codification; net beneficiary
 *   - Lower-Caste Hindu Couple: powerless/identity_locked — pre-codification marriage forms are erased; state recognition requires adoption of brahmanical structure; identity cannot exit the frame
 *   - Interfaith Couple (Hindu + Other): powerless/identity_locked — not eligible for HMA recognition without conversion or resort to Special Marriage Act; religious identity is non-negotiable; locked out of coordination mechanism
 *   - Gender Equity Reform Coalition: organized/constrained — sees sunset potential through reinterpretation; constrained by political and judicial conservatism; generational progress visible
 *   - Upper-Caste Hindu Male Householder: powerful/arbitrage — benefits from codification of patrilineal property transmission disguised as gender-neutral law; net beneficiary
 *   - Hindu Personal Law Institution: institutional/arbitrage — piton: institutional performance of marriage adjudication persists despite function atrophy; legitimacy decoupled from actual authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hindu_codified_reading, 0.35).
domain_priors:suppression_score(hindu_codified_reading, 0.42).
domain_priors:theater_ratio(hindu_codified_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hindu_codified_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hindu_codified_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hindu_codified_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(hindu_codified_reading, "Hindu Codified Marriage Authority (Civil Court Reading)").
narrative_ontology:topic_domain(hindu_codified_reading, "constitutional_pluralism/family_law/religious_governance").

domain_priors:requires_active_enforcement(hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hindu_codified_reading, '73111bcd-4c67-4148-88de-52e1a8cf5b7e').
narrative_ontology:cs_kernel_codification('73111bcd-4c67-4148-88de-52e1a8cf5b7e', formalized).
narrative_ontology:cs_authority_grounding('73111bcd-4c67-4148-88de-52e1a8cf5b7e', extraction).
narrative_ontology:cs_interpretation_layer_present('73111bcd-4c67-4148-88de-52e1a8cf5b7e').
narrative_ontology:cs_reading_relation('73111bcd-4c67-4148-88de-52e1a8cf5b7e', hindu_codified_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('73111bcd-4c67-4148-88de-52e1a8cf5b7e', hindu_codified_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('73111bcd-4c67-4148-88de-52e1a8cf5b7e', hindu_codified_reading__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('73111bcd-4c67-4148-88de-52e1a8cf5b7e', hindu_codified_reading__secular_civil_reading, influences).
narrative_ontology:cs_axiom('73111bcd-4c67-4148-88de-52e1a8cf5b7e', foundational, codified_brahmanical_norm_as_hindu_universal).
narrative_ontology:cs_axiom_status(codified_brahmanical_norm_as_hindu_universal, holdable).
narrative_ontology:cs_axiom_grounding('73111bcd-4c67-4148-88de-52e1a8cf5b7e', codified_brahmanical_norm_as_hindu_universal, conventional).
narrative_ontology:cs_axiom('73111bcd-4c67-4148-88de-52e1a8cf5b7e', foundational, state_court_monopoly_marriage_adjudication).
narrative_ontology:cs_axiom_status(state_court_monopoly_marriage_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('73111bcd-4c67-4148-88de-52e1a8cf5b7e', state_court_monopoly_marriage_adjudication, instrumental).
narrative_ontology:cs_axiom('73111bcd-4c67-4148-88de-52e1a8cf5b7e', secondary, patrilineal_property_transmission_as_coordination).
narrative_ontology:cs_axiom_status(patrilineal_property_transmission_as_coordination, holdable).
narrative_ontology:cs_axiom_grounding('73111bcd-4c67-4148-88de-52e1a8cf5b7e', patrilineal_property_transmission_as_coordination, deontological).
narrative_ontology:cs_reference_frame('73111bcd-4c67-4148-88de-52e1a8cf5b7e', brahmanical_personal_law_codified).
narrative_ontology:cs_drift_state('73111bcd-4c67-4148-88de-52e1a8cf5b7e', contemporary_constitutional_pluralism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('73111bcd-4c67-4148-88de-52e1a8cf5b7e', '').
narrative_ontology:cs_kernel_id(hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hindu_codified_reading, state_judicial_apparatus).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, brahmanical_property_transmission).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, upper_caste_male_authority).
narrative_ontology:constraint_victim(hindu_codified_reading, women_marital_mobility).
narrative_ontology:constraint_victim(hindu_codified_reading, lower_caste_marriage_claims).
narrative_ontology:constraint_victim(hindu_codified_reading, interfaith_couple_recognition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HINDU WOMAN (TANGLED ROPE) — Experiences the codified statute as both coordination (marriage is now registered, property claims have legal standing) and extraction (property inheritance still favors males; divorce requires proving fault; remarriage carries social enforcement of marital status). Trapped by exit costs: exit requires either abandonment of children or costly litigation. The codification provides some equality gains over pre-codification custom but preserves substantial extraction through gendered fault doctrine and inheritance asymmetry.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL COURT JUDICIARY (ROPE) — Perceives codification as a coordination mechanism that resolves the problem of adjudicating Hindu marriage disputes uniformly. The courts benefit from arbitrage: they can apply statutory rules instead of navigating contested interpretations of smṛti and śāstra. The judiciary experiences the HMA 1955 as delegitimizing and clarifying their authority — codification transferred interpretive power from pundits and community arbiters to the state apparatus. Net beneficiary position.
constraint_indexing:constraint_classification(hindu_codified_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LOWER-CASTE COUPLE (SNARE) — Trapped by multiple layers: (a) legal requirement to register under HMA 1955 displaces pre-codification community recognition; (b) codified statute embeds brahmanical marriage norms (monogamy, inheritance through male line) that contradict some lower-caste ritual practices; (c) exit requires either denying caste identity (rejecting community-recognized marriage forms) or accepting state-imposed extraction (non-recognition as valid marriage under state law). Identity-locked because the recognition of marriage identity is constituted through the state's codified frame — rejecting the statute means losing legal marital status and accompanying property/succession rights. Pure extraction: the statute provides no benefit to lower-caste marriage forms while imposing brahmanical structure.
constraint_indexing:constraint_classification(hindu_codified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERFAITH COUPLE (SNARE) — Doubly trapped: HMA 1955 applies only when both parties are Hindu; if one partner is Muslim/Christian/Other, the couple must either (a) register under Special Marriage Act (1954), which imposes secular ceremonies and waiting periods incompatible with some faith traditions, or (b) forgo state recognition entirely. For Hindu+non-Hindu couples who wish to preserve both faith identities, the codified statute provides no coordination pathway — it is pure extraction (forced choice between state recognition and religious identity). Identity-locked because religious identity is non-negotiable for many parties; exit would require renouncing faith commitment to access the legal frame. The statute extracts recognition-capture.
constraint_indexing:constraint_classification(hindu_codified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: GENDER EQUITY REFORM COALITION (SCAFFOLD) — Organized agents (women's legal rights groups, constitutional scholars, reform judges) see codification as a transitional scaffolding toward gender-equal marriage law. The HMA 1955, while preserving male bias in inheritance and fault doctrine, created the legal substrate from which equality claims could be launched. Reform judges have reinterpreted statutory provisions to reduce fault requirements and expand marital property sharing. The coalition experiences the HMA as having a sunset: as gender equality norms harden through case law and constitutional interpretation, the statute's male-biased provisions are hollowed out. Extractiveness is declining generationally as interpretation shifts toward equality. Exit path is visible (judicial reinterpretation; statutory amendment) but constrained by political resistance from traditional interests.
constraint_indexing:constraint_classification(hindu_codified_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BRAHMANICAL UPPER-CASTE MALE HOUSEHOLDER (ROPE) — Perceives codification as a coordination mechanism that preserves patrilineal property transmission and male authority while formalizing it as 'uniform law.' The HMA 1955 codified brahmanical marriage norms (monogamy, male succession, widow chastity ideals) and embedded them in state authority, making them appear gender-neutral rather than caste-specific. This agent benefits from arbitrage: the statute disguises extractive inheritance rules as gender-neutral coordination. Net beneficiary.
constraint_indexing:constraint_classification(hindu_codified_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: HINDU PERSONAL LAW AS INSTITUTIONAL PRACTICE (PITON) — The codified HMA 1955 is a degraded performance of a prior function. Pre-codification, Hindu marriage was adjudicated through śāstric interpretation by community-embedded pandits with local legitimacy. Post-codification, the same function (marriage adjudication, property assignment, succession) is performed by civil courts applying written statute — the institution persists but its primary function has atrophied. Courts now apply standardized rules rather than contextual śāstric reasoning; pandits have been displaced by lawyers; legitimacy flows from state authority rather than interpretive scholarship. Theater ratio is moderate (0.38) because the codified statute retains some residual legitimacy through appeals to scriptural continuity and Hindu tradition, but the actual adjudication is fully state-centric. The piton persists through institutional inertia and appeals to tradition rather than functional necessity.
constraint_indexing:constraint_classification(hindu_codified_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STATUTORY NATURALIZATION VIEW (MOUNTAIN) — From a civilizational vantage, the codification of Hindu marriage law can appear as a natural consequence of the logic of modern state formation: all complex legal traditions eventually codify; written law is more transparent and consistent than interpretive custom; uniform rules are more equitable than context-dependent adjudication. This perspective sees the HMA 1955 as an inevitable stage in the rationalization of governance. However, this naturalizes a contingent institutional choice: codification in favor of brahmanical norms, with state monopoly on marriage recognition, is NOT inevitable — other pathways (pluralist recognition of multiple marriage forms, non-state adjudication, community-embedded reforms) were possible. The engine should flag this as a false summit: the 'natural law of modernization' framing obscures the extraction of brahmanical authority and state monopoly.
constraint_indexing:constraint_classification(hindu_codified_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hindu_codified_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hindu_codified_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hindu_codified_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hindu_codified_reading, TR),
    TR >= 0.70.

:- end_tests(hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The codified HMA 1955 provides genuine coordination gains (uniform rules, legal registration, property claims) alongside genuine extraction (patrilineal property transmission, fault-based divorce, denial of non-brahmanical and interfaith marriage recognition). The score reflects that the constraint is mixed — the coordination problem (what rules apply to Hindu marriages?) is solved, but the solution redistributes authority toward the state and toward brahmanical norms. The 0.28 → 0.37 trajectory reflects increasing visibility of the extraction mechanisms as gender equity movements highlight asymmetries. Suppression (0.42): Moderate. Exit costs are significant (women risk children and property; lower-caste couples risk legal non-recognition; interfaith couples must forgo faith identity) but not total — legal aid, divorce access, and constitutional protections provide some exit routes. The declining trajectory (0.48 → 0.39) reflects improving access to courts and legal remedies. Theater ratio (0.38): Moderate-low. The statute retains legitimacy appeals to Hindu tradition and represents real codification work, but institutional performance has shifted: courts now apply written rules rather than engage śāstric interpretation; the appeal to 'Hindu tradition' is partly theatrical while the actual adjudication is fully state-centric. The rising trajectory (0.32 → 0.41) reflects increasing resort to rhetoric of tradition as the statute's brahmanical embeddings become more visible and contested.
 *
 * PERSPECTIVAL GAP:
 *   The entire constraint exhibits radical perspectival divergence. The civil court judiciary sees coordination (Rope) — codification solved the problem of applying Hindu law uniformly. Upper-caste males see coordination (Rope) — codification preserved patrilineal property transmission under modern state authority. The gender equity coalition sees scaffold (Rope in transition) — judicial reinterpretation is reducing extraction. But the lower-caste couple sees pure extraction (Snare) — their marriage forms are erased. The interfaith couple sees pure extraction (Snare) — they are locked out of the mechanism entirely. The Hindu woman sees mixed coordination and extraction (Tangled Rope) — the statute enables her property claims but encodes male succession. The institution sees its own degradation (Piton) — the formal rule-application persists through inertia rather than function. The civilizational observer risks seeing immutable law (Mountain) — codification as inevitable modernization. The perspectival structure reveals that the constraint's type is not singular but distributed across the observation landscape — the same structural facts produce Rope, Snare, Tangled Rope, Scaffold, and Piton simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The four-tuple context (power, time_horizon, exit_options, spatial_scope) determines each perspective's directionality. Beneficiaries occupy high-power/arbitrage/immediate positions (civil courts, upper-caste males) and derive d toward 0.0 (low or negative effective extraction). Victims occupy powerless/trapped positions (women, lower-caste couples, interfaith couples) and derive d toward 1.0 (maximum experienced extraction). The identity_locked exit option for lower-caste and interfaith couples indicates that their trapping mechanism is cognitive (identity constituted through state recognition) rather than material (legal prohibition) — they have structural mobility (other jurisdictions, non-recognition) but cannot exercise it without abandoning identity. The piton perspective derives from high theater ratio combined with degraded function (institutional performance persists despite atrophy of interpretive role). The scaffold perspective derives from organized power combined with constrained exit and visible sunset (judicial reinterpretation pathway visible but difficult). The analytical mountain is likely a false summit: it naturalizes state monopoly and brahmanical specificity as inevitable features of modernization rather than contingent institutional choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by this reading's explicit documentation as a READING of the marriage authority kernel, not as a claim about universal Hindu marriage law. The constraint's mandate is to determine who adjudicates Hindu marriage claims with state backing (the procedural question); the mandate does NOT extend to determining what Hindu marriage 'really is' (the substantive question resolved across sibling readings). This reading's mandate is live and functional: state courts do adjudicate Hindu marriage disputes under HMA 1955 in contemporary India. The mandatrophy arises only if the reading claims to be the single valid understanding of Hindu marriage across all caste and regional contexts — a claim this reading does NOT make. The reading's own classification as Tangled Rope (not pure coordination) acknowledges that the statute's extraction mechanisms coexist with its coordination function. If the reading were classified as pure Rope, the mandatrophy would emerge (as implicit claim: codification benefits everyone equally). The Tangled Rope classification prevents the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_brahmanical_specificity,
    'Is the Hindu codified reading a politically neutral encoding of Hindu tradition, or does codification privilege brahmanical forms of marriage (monogamy, male succession, widow ideals) and suppress non-brahmanical marriage forms?',
    'Historical analysis of pre-codification diversity in Hindu marriage practices across caste and region; comparison of codified rules against actual śāstric diversity; empirical survey of marriage practice displacement post-codification.',
    'If neutral encoding: the reading is coordination (Rope) for all Hindu communities. If brahmanical-privileging: the reading is Snare for non-brahmanical communities and extraction mechanism for lower castes. The entire classification shifts based on whether codification homogenized or merely formalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_brahmanical_specificity, empirical, 'Whether codification is neutral encoding or brahmanical privileging').

omega_variable(
    state_monopoly_inevitability,
    'Is the state''s monopoly on marriage recognition and adjudication a necessary feature of codification, or could a codified statute coexist with non-state marriage recognition and community adjudication (as in some pluralist frameworks)?',
    'Comparative legal analysis of pluralist jurisdictions (Malaysia, Nigeria, Lebanon); examination of theoretical possibility spaces for codified law with concurrent jurisdiction; exploration of whether the 1955 Indian choice of state monopoly was structurally inevitable or politically contingent.',
    'If monopoly is inevitable: the constraint is structurally determined by modernization. If contingent: the choice to vest adjudication power in state courts is a distribution of political authority that could have been different, making the codified reading''s beneficiary structure (state apparatus, brahmanical norms) more clearly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_inevitability, conceptual, 'Whether state monopoly on marriage recognition is structurally necessary or contingent').

omega_variable(
    gender_equality_trajectory,
    'Is the generational trend toward gender equality in case law (reducing fault bias, expanding marital property) evidence that the scaffold sunset (toward gender-equal marriage law) is real, or are we observing rhetorical modernization without structural change in property transmission and succession?',
    'Empirical data on marriage property division outcomes; inheritance outcome analysis by gender; longitudinal tracking of widow remarriage rates and legal recognition; comparative case law sentiment analysis.',
    'If real trend: scaffold classification is correct, and the HMA is transitioning toward Rope (coordination). If rhetorical: the constraint remains Tangled Rope or Snare, with gender equality invoked theatrically while substantive extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_trajectory, empirical, 'Whether gender equality trajectory is real structural change or rhetorical modernization').

omega_variable(
    interfaith_marriage_recognition,
    'Is the denial of HMA 1955 recognition to interfaith couples a feature of Hindu codification specifically, or a necessary consequence of adopting religious-community-specific personal law systems in multi-faith states?',
    'Comparison with other multi-faith states'' treatment of interfaith marriage; analysis of whether alternative codifications (e.g., a secular civil code applicable to all) would resolve the gap; examination of whether the HMA could be amended to recognize interfaith Hindu marriages without losing its Hindu-specific character.',
    'If Hindu-specific flaw: the reading''s extraction of interfaith couples is a contingent design choice (Snare for interfaith couples is engineered). If inevitable feature: the reading''s Snare character for interfaith couples is a structural property of religious personal law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_marriage_recognition, empirical, 'Whether interfaith marriage denial is specific to Hindu codification or inevitable in religious personal law').

omega_variable(
    kernel_reading_authority_distinction,
    'Is this ''Hindu codified reading'' a reading OF the marriage authority kernel (how marriage legitimacy is grounded), or a reading OF Hindu tradition itself? In other words, is the kernel ''who decides what is a valid marriage'' or ''what IS a valid Hindu marriage''?',
    'Conceptual clarification and structural mapping of the four-element CS tuple (kernel_codification, authority_grounding, interpretation_layer, codified_content). Determine whether the Hindu marriage kernel is substantive (what Hindu marriage is) or procedural (who adjudicates marriage).',
    'If kernel is procedural: the reading''s alternatives are secular state adjudication, community adjudication, religious adjudication — reading_relations should reflect these as foreclosing relationships. If kernel is substantive: the reading''s alternatives are different understandings of Hindu marriage itself — coexists_with or influences relationships. The classification path hinges on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authority_distinction, conceptual, 'Whether the kernel is procedural (who decides) or substantive (what is valid)').

omega_variable(
    false_summit_modernization_narrative,
    'Does the mountain classification from the analytical observer''s civilizational perspective naturalize a contingent institutional choice (state monopoly on marriage, brahmanical norm encoding) as an inevitable feature of modernization and rationalization?',
    'Examination of pre-codification alternatives; comparison with other modern legal systems that achieved codification without state monopoly or brahmanical specificity; analysis of whether the ''natural law of modernization'' framing serves to legitimize extraction.',
    'If yes: the mountain is a false summit, and the constraint should be reclassified as Tangled Rope or Snare. The ''inevitability'' framing itself is an extraction mechanism — it prevents questioning of the institutional design choices embedded in the HMA.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_modernization_narrative, conceptual, 'Whether modernization narrative naturalizes contingent institutional choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcr_tr_t0, hindu_codified_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hcr_tr_t15, hindu_codified_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(hcr_tr_t30, hindu_codified_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(hcr_tr_t50, hindu_codified_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(hcr_be_t0, hindu_codified_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hcr_be_t15, hindu_codified_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(hcr_be_t30, hindu_codified_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(hcr_be_t50, hindu_codified_reading, base_extractiveness, 50, 0.37).

% Suppression requirement over time
narrative_ontology:measurement(hcr_su_t0, hindu_codified_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(hcr_su_t15, hindu_codified_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(hcr_su_t30, hindu_codified_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(hcr_su_t50, hindu_codified_reading, suppression_requirement, 50, 0.39).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hindu_codified_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, secular_civil_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, uniform_civil_code_aspiration).

% DUAL FORMULATION NOTE:
% The Hindu codified reading is one member of a constraint family bound by the marriage authority kernel. Each reading is a structurally distinct constraint with different extractiveness values, beneficiary structures, and victim sets. The Hindu codified reading (ε=0.35) is moderate-extraction Tangled Rope; the secular civil reading would be lower-extraction Rope (uniform application); the Muslim Shariah reading (as a separate constraint story) would carry different gender and caste extraction profiles. The family members are linked through network.affects_constraints because the authority struggle in Indian personal law is a zero-sum competition for state backing — if one reading gains enforcement power, others lose it. However, each story must be authored independently with its own ε and perspectives; the ε-invariance principle applies across the kernel family: if measuring the Hindu reading through the 'degree to which it privileges brahmanical norms' changes ε, the author is measuring a different constraint (e.g., brahmanical norm entrenchment vs. marriage authority codification). See `docs/kernel_decomposition.md` for constraint family methodology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hindu_codified_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
