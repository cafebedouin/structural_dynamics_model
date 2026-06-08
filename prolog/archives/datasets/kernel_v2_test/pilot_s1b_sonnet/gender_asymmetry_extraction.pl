% ============================================================================
% CONSTRAINT STORY: gender_asymmetry_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gender_asymmetry_extraction, []).

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
 *   constraint_id: gender_asymmetry_extraction
 *   human_readable: Gender Asymmetry Extraction via Parallel Personal Law Systems
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   India's parallel personal-law system allows Hindu, Muslim, Christian,
 *   Parsi, and secular legal codes to govern marriage, divorce, inheritance,
 *   and adoption simultaneously within one state. Established during British
 *   colonial rule and retained after 1947 independence, the system was
 *   justified as necessary accommodation of religious pluralism and as
 *   bulwark against communal violence. Seven decades later, it persists
 *   without zero-sum displacement: no single code has conquered the others,
 *   and the state has repeatedly declined to impose a Uniform Civil Code
 *   despite constitutional directive. This constraint exhibits the core
 *   tangled_rope structure: genuine coordination function (managing religious
 *   diversity, preventing legal uniformity from triggering communal conflict)
 *   inseparably bundled with substantial extraction (gender-asymmetric
 *   inheritance, unequal divorce rights, maintenance conditional on religious
 *   obedience). The system's most severe extraction falls on women whose
 *   identity is constituted through religious community membership—exit from
 *   traditional personal law requires either religious conversion or adoption
 *   of secular marriage under the Special Marriage Act, both of which rupture
 *   family ties and community belonging. The theater ratio (0.38) is
 *   moderate, not high: religious authorities genuinely adjudicate family law
 *   rather than merely performing adjudication. But extractiveness (0.68) and
 *   suppression (0.72) are both high, reflecting substantial gender asymmetry
 *   and identity-based legal inequality. Measurements show gradual
 *   intensification over 75 years: extractiveness rising from 0.55 to 0.68,
 *   suppression from 0.65 to 0.72, theater ratio from 0.25 to 0.38. The
 *   increase in theater reflects growing gap between pluralism rhetoric and
 *   coordination function as partition-era communal violence risk has
 *   diminished but political fear of testing that empirically remains.
 *
 * KEY AGENTS:
 *   - Women Under Traditional Codes: Primary victims (powerless/identity_locked) — bear gender-asymmetric extraction, cannot exit without community rupture and identity transformation
 *   - Religious Institutional Authorities: Primary beneficiaries (institutional/arbitrage) — extract legitimacy and control from personal law autonomy, can arbitrage between state and religious authority
 *   - Male Heads of Household: Secondary beneficiaries (moderate/constrained) — benefit from gender-asymmetric inheritance and divorce rules, but constrained by community enforcement and religious authority supervision
 *   - Interfaith Couples: Mixed position (moderate/constrained) — benefit from Special Marriage Act coordination function but bear social ostracism and must exit religious personal law entirely
 *   - Secular Feminist Coalition: Organized reformers (organized/mobile) — advocate Uniform Civil Code, see parallel system as temporary scaffold that has outlived sunset
 *   - Reform Advocates Within Communities: Internal reformers (moderate/constrained) — benefit from pluralism's space for internal debate, but bear extraction from institutional resistance to change
 *   - Postcolonial State Legal System: Institutional actor (institutional/constrained) — maintains system through inertia, constrained by fear of communal violence if reform attempted
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled_rope: real coordination bundled with substantial extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_asymmetry_extraction, 0.68).
domain_priors:suppression_score(gender_asymmetry_extraction, 0.72).
domain_priors:theater_ratio(gender_asymmetry_extraction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_asymmetry_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(gender_asymmetry_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gender_asymmetry_extraction, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_asymmetry_extraction, tangled_rope).
narrative_ontology:human_readable(gender_asymmetry_extraction, "Gender Asymmetry Extraction via Parallel Personal Law Systems").
narrative_ontology:topic_domain(gender_asymmetry_extraction, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(gender_asymmetry_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gender_asymmetry_extraction, '2dbf1647-61cf-4f7b-aa70-459db9f14364').
narrative_ontology:cs_kernel_codification('2dbf1647-61cf-4f7b-aa70-459db9f14364', distributed).
narrative_ontology:cs_authority_grounding('2dbf1647-61cf-4f7b-aa70-459db9f14364', lineage).
narrative_ontology:cs_interpretation_layer_present('2dbf1647-61cf-4f7b-aa70-459db9f14364').
narrative_ontology:cs_reading_relation('2dbf1647-61cf-4f7b-aa70-459db9f14364', gender_asymmetry_extraction__uniform_civil_code_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dbf1647-61cf-4f7b-aa70-459db9f14364', gender_asymmetry_extraction__internal_reform_reading, coexists_with).
narrative_ontology:cs_axiom('2dbf1647-61cf-4f7b-aa70-459db9f14364', foundational, religious_autonomy_over_family_law).
narrative_ontology:cs_axiom_status(religious_autonomy_over_family_law, holdable).
narrative_ontology:cs_axiom_grounding('2dbf1647-61cf-4f7b-aa70-459db9f14364', religious_autonomy_over_family_law, deontological).
narrative_ontology:cs_axiom('2dbf1647-61cf-4f7b-aa70-459db9f14364', foundational, communal_violence_prevention_via_legal_pluralism).
narrative_ontology:cs_axiom_status(communal_violence_prevention_via_legal_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('2dbf1647-61cf-4f7b-aa70-459db9f14364', communal_violence_prevention_via_legal_pluralism, empirically_contingent).
narrative_ontology:cs_reference_frame('2dbf1647-61cf-4f7b-aa70-459db9f14364', partition_era_pluralism_necessity).
narrative_ontology:cs_drift_state('2dbf1647-61cf-4f7b-aa70-459db9f14364', contemporary_post_communal_violence_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2dbf1647-61cf-4f7b-aa70-459db9f14364', '2025-01-09T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_asymmetry_extraction, religious_institutional_authorities).
narrative_ontology:constraint_beneficiary(gender_asymmetry_extraction, male_heads_of_household).
narrative_ontology:constraint_victim(gender_asymmetry_extraction, women_under_traditional_codes).
narrative_ontology:constraint_victim(gender_asymmetry_extraction, interfaith_couples).
narrative_ontology:constraint_victim(gender_asymmetry_extraction, religious_minorities_within_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gender_asymmetry_extraction, interfaith_couples).
narrative_ontology:constraint_beneficiary(gender_asymmetry_extraction, reform_advocates_within_communities).
narrative_ontology:constraint_victim(gender_asymmetry_extraction, reform_advocates_within_communities).
narrative_ontology:constraint_vindicates(gender_asymmetry_extraction, cultural_pluralism_doctrine).
narrative_ontology:constraint_vindicates(gender_asymmetry_extraction, religious_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women whose marriages are governed by traditional Hindu, Muslim, or Christian personal law codes. Face unequal inheritance rights, divorce barriers, and maintenance conditional on religious obedience. Identity-locked: exiting personal law system requires either religious conversion or Special Marriage Act adoption, both causing community rupture and family estrangement. The personal law codes constitute their legal identity within family structures—cannot exit without becoming a different kind of person in their community's eyes.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, women_under_traditional_codes, payer,
    powerless, biographical, identity_locked, national).

% Muslim Personal Law Boards, Hindu customary authorities, Christian ecclesiastical courts. Set and adjudicate family law for their communities. Arbitrage between religious and state authority: invoke religious autonomy to resist state interference, invoke state enforcement machinery to maintain internal discipline. Extract legitimacy and institutional power from personal law autonomy.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, religious_institutional_authorities, agenda_setter,
    institutional, immediate, arbitrage, national).

% Men whose family arrangements are governed by personal law codes. Benefit from gender-asymmetric inheritance (sons receive more than daughters), easier divorce access, and authority over household maintenance. Constrained exit: cannot easily leave religious community without social costs, and religious authorities supervise family law compliance.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, male_heads_of_household, beneficiary,
    moderate, biographical, constrained, national).

% Couples marrying across religious boundaries. Benefit from Special Marriage Act coordination: provides legal recognition when religious codes forbid union. But pay through extraction: must renounce religious personal law entirely to access secular marriage, face social ostracism from both communities, navigate bureaucratic obstacles. Dual role: coordination beneficiaries and social penalty payers.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, interfaith_couples, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gender_asymmetry_extraction, interfaith_couples, beneficiary).

% Internal reform movements within religious communities advocating gender equality. Benefit from pluralism's space for internal debate: religious autonomy creates room for reform arguments within traditions. But pay through institutional resistance: religious authorities extract legitimacy from tradition while resisting change. Constrained by identity loyalty—cannot exit community to pursue reform from outside.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, reform_advocates_within_communities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gender_asymmetry_extraction, reform_advocates_within_communities, beneficiary).

% Organized advocacy groups pushing for Uniform Civil Code. Operate outside religious frameworks, mobile exit from personal law system. See parallel system as temporary scaffold that should sunset as gender equality norms mature. Neither collecting from nor paying into the personal law structure directly—advocating for its replacement.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, secular_feminist_coalition, observer,
    organized, generational, mobile, national).

% Indian state legal system maintaining parallel personal law framework. Sets overall structure: which codes are recognized, how conflicts between codes are resolved, when state courts intervene. Constrained exit: dismantling requires constitutional amendment and communal consensus the state fears to pursue. Maintains system through inertia, invoking pluralism doctrine to avoid politically costly reform.
narrative_ontology:constraint_stakeholder(gender_asymmetry_extraction, postcolonial_state_legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The parallel personal law system coordinates religious pluralism by enabling each community to govern family law according to its own traditions, preventing legal uniformity from erasing religious differences and avoiding zero-sum conflict over whose family law becomes state law.
% TRANSFER_FUNCTION: The system transfers legal authority over family matters from the state to religious institutional authorities, and within families transfers resources and autonomy from women to male household heads via gender-asymmetric inheritance, divorce, and maintenance rules.
% ABSENT_VOICES: Women who have exited traditional personal law (via Special Marriage Act or conversion) are structurally excluded from reform debates within religious communities—their exit disqualifies their voice. Religious minorities within minorities (e.g., Dalit women within Hindu law, Shia women within Sunni-dominated Muslim Personal Law Board, Protestant women in Catholic-majority Christian codes) are excluded from agenda-setting within their traditions.
% DISAPPEARANCE_RATIONALE: If parallel personal law disappeared overnight (replaced by Uniform Civil Code), family arrangements would rearrange substantially: interfaith marriages would become legally simpler, women's inheritance and divorce rights would change (direction depending on UCC content), religious institutional authorities would lose adjudication power, and community identity boundaries would be redrawn. The system's persistence shows stakeholders exist whose arrangements depend on it.
% FOUNDING_PROBLEM: Managing religious diversity during partition and immediate post-independence period (1947-1960s) to prevent communal violence. Partition caused 1-2 million deaths and 10-15 million displacements in massive Hindu-Muslim communal violence. Parallel personal law system was retained to avoid legal uniformity triggering further violence by erasing religious community autonomy over family law.
% FOUNDING_PROBLEM_CORROBORATION: Status is contested between: (1) Religious institutional authorities claim founding problem remains live—Uniform Civil Code would trigger communal conflict. (2) Secular feminist coalition claims founding problem is dead—partition-era violence risk has diminished; UCC opposition is now institutional self-preservation, not genuine pluralism defense. (3) State has not empirically tested this by attempting UCC adoption, so both claims remain unverified. Corroboration sources: Historical scholarship on partition (Gyanendra Pandey, Urvashi Butalia) documents founding violence. Contemporary political science (Rochana Bajpai, Rajeev Bhargava) documents current contestation. No corroboration exists outside the interested parties because the state has avoided the test.
narrative_ontology:disappearance_verdict(gender_asymmetry_extraction, world_rearranges).
narrative_ontology:founding_problem_status(gender_asymmetry_extraction, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER TRADITIONAL CODES (SNARE) — Identity-locked by religious community membership that constitutes family, social network, and self-concept. Exit from the personal law system requires either converting religion (losing community) or adopting secular marriage under Special Marriage Act (social stigma, family rupture). High extractiveness: unequal inheritance, divorce barriers, maintenance rights dependent on religious obedience. The coordination story (preserving cultural identity) is cover; the structure extracts from women to maintain patriarchal family arrangements.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERFAITH COUPLES (TANGLED ROPE) — Constrained by dual-community social pressure and legal complexity. Benefit from genuine coordination function: Special Marriage Act provides legal recognition when religious codes would forbid union. But also bear extraction: must renounce religious personal law to access secular marriage, face social ostracism, navigate bureaucratic obstacles. Mixed experience: coordination exists but comes bundled with asymmetric costs.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTIONAL AUTHORITIES (ROPE) — Primary beneficiaries. Arbitrage between religious and state authority: can invoke religious autonomy to resist state interference while invoking state enforcement machinery to maintain internal discipline. Experience the system as pure coordination: the parallel structure enables religious community self-governance. Low effective extraction because extraction flows toward this agent.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM ADVOCATES (TANGLED ROPE) — Moderate power through community organizing, constrained by identity loyalty and institutional resistance. Benefit from the system's pluralism: religious autonomy creates space for internal reform arguments. But bear extraction: reform efforts require navigating religious authority structures that extract legitimacy from tradition while resisting change. Generational time horizon: see the possibility of evolution within religious frameworks.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SECULAR FEMINIST COALITION (SCAFFOLD) — Organized agents advocating for Uniform Civil Code see the parallel personal law system as temporary accommodation of religious pluralism that should sunset as gender equality norms mature. Mobile exit: can operate outside religious frameworks. See the structure as transitional coordination mechanism that has outlived its justification. Lower extraction because organizing capacity creates alternatives.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE LEGAL SYSTEM (PITON) — The parallel personal law structure is institutional residue from colonial-era divide-and-rule strategy. Original coordination function (managing religious diversity during partition crisis) has atrophied; what persists is performative pluralism. The state maintains the structure through inertia despite rising theater ratio: religious autonomy doctrine invoked to avoid politically costly reform, not because it solves a live coordination problem. Constrained exit: dismantling requires constitutional amendment and communal consensus the state fears to pursue.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, the system exhibits genuine coordination function (manages religious pluralism, prevents zero-sum legal uniformity) bundled with substantial extraction (gender asymmetry, identity-based legal inequality). The persistence without displacement suggests stable equilibrium of competing legitimacy claims, not natural law. This is the authoritative classification: real coordination exists but enforcement of gender-asymmetric codes makes the structure extractive. Theater ratio is moderate (0.38) not high: religious authorities genuinely govern family law, not merely perform governance.
constraint_indexing:constraint_classification(gender_asymmetry_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_asymmetry_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_asymmetry_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_asymmetry_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gender_asymmetry_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_asymmetry_extraction, TR),
    TR >= 0.70.

:- end_tests(gender_asymmetry_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Gender asymmetry in inheritance (Muslim personal law: daughters receive half of sons' share; Hindu personal law: married daughters historically excluded from agricultural property), divorce (Muslim triple talaq until 2019 reform; Christian personal law: no divorce except judicial separation), and maintenance (Hindu law: maintenance conditional on wifely obedience; Muslim law: temporary alimony only) creates substantial extraction from women. The extraction is not total because some women benefit from protective provisions (maintenance rights, stridhan property) and because internal reform movements have achieved incremental changes. But the overall structure channels resources and autonomy from women to male family heads and religious institutional authorities. Suppression (0.72): High. Exit barriers include identity fusion with religious community, family rupture, social ostracism, and bureaucratic obstacles to accessing Special Marriage Act. Women who exit report sustained community exclusion and family estrangement. But suppression is not maximal: Special Marriage Act provides legal exit path, interfaith couples do exit successfully, and some women navigate between systems strategically. Theater ratio (0.38): Moderate. Religious authorities genuinely adjudicate family law—Muslim Personal Law Boards issue fatwas, Christian ecclesiastical courts grant annulments, Hindu customary authorities resolve inheritance disputes. This is not merely performative. But theater is rising: the pluralism justification (preventing communal violence) is increasingly rhetorical rather than functional as partition-era violence risk has diminished. State invokes religious autonomy doctrine to avoid politically costly reform, not because the coordination function remains as strong as in 1947.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap is between women under traditional codes (snare: pure extraction, no coordination benefit visible) and religious institutional authorities (rope: pure coordination, extraction invisible or justified as legitimate differentiation). Women experience identity-locked entrapment in gender-asymmetric legal codes; religious authorities experience voluntary coordination enabling community self-governance. The gap reveals that 'coordination' and 'extraction' are not objective properties but structural relationships: the same mechanism that coordinates religious diversity extracts from women within each tradition. Interfaith couples and reform advocates occupy the middle ground (tangled_rope): they see both the coordination function and the extraction, experiencing mixed benefits and costs. The secular feminist coalition sees scaffold: temporary accommodation that should sunset. The postcolonial state sees piton: atrophied function maintained by inertia. The analytical observer sees tangled_rope as the authoritative classification: genuine coordination exists but is inseparably bundled with substantial extraction, requiring active enforcement to maintain gender-asymmetric codes.
 *
 * DIRECTIONALITY LOGIC:
 *   Women under traditional codes are victims with identity_locked exit, producing high directionality toward full target. The engine derives d from victim status + identity_locked exit + powerless agent power, yielding high d → high f(d) → high effective extraction. Religious institutional authorities are beneficiaries with arbitrage exit options, producing low directionality toward full beneficiary. The engine derives d from beneficiary status + arbitrage exit + institutional power, yielding low d → negative f(d) → negative or near-zero effective extraction (they experience subsidy, not cost). Male heads of household are beneficiaries but with constrained exit (cannot easily leave religious community), producing moderate directionality. Interfaith couples are in mixed position: beneficiaries of Special Marriage Act coordination but victims of social ostracism, with constrained exit. Reform advocates within communities are also mixed: beneficiaries of pluralism's space for debate but victims of institutional resistance. The perspectival gap is widest between women under traditional codes (snare) and religious institutional authorities (rope), reflecting maximum asymmetry in experienced extraction. No override is needed—the structural declarations drive accurate directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The parallel personal law system was designed to manage religious diversity and prevent communal violence during partition and immediate post-independence period (1947-1960s). That mandate may have been legitimate coordination given partition's massive communal violence (1-2 million deaths, 10-15 million displaced). But seven decades later, the founding problem has either diminished or transformed: communal violence risk today is qualitatively different from partition-era risk, and the state has not empirically tested whether Uniform Civil Code adoption would trigger violence comparable to partition. The system persists through political fear of testing this counterfactual, not through demonstrated ongoing necessity. This suggests mandatrophy: the mandate has outlived its function, but the structure persists because dismantling requires confronting religious institutional authorities whose power now depends on personal law autonomy. The theater ratio increase from 0.25 to 0.38 over 75 years reflects this: pluralism rhetoric is increasingly performative as the original coordination function atrophies. However, mandatrophy is not resolved because the genuine coordination function (managing religious diversity) has not been fully superseded—the system still prevents legal uniformity from erasing religious differences, which many communities value. The omega variable 'postcolonial_mandatrophy' captures this irreducible uncertainty: has the founding problem dissolved, or does it persist in transformed form?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_decomposition,
    'Can the gender-asymmetric extraction be cleanly separated from the religious pluralism coordination, or are they structurally inseparable?',
    'Comparative analysis of parallel personal law systems that have achieved internal gender equality reform without state displacement (e.g., Jewish religious courts in Israel post-rabbinical court reforms). If gender equality can be achieved within religious autonomy framework, extraction and coordination are separable constraints. If every attempt at internal reform fails or triggers system collapse, they are inseparable.',
    'If separable: two distinct constraints (one rope, one snare) linked by network.affects_constraints, and reform can target extraction without dismantling coordination. If inseparable: single tangled_rope, and any reform requires renegotiating the entire structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, empirical, 'Whether gender extraction can be separated from pluralism coordination').

omega_variable(
    identity_lock_strength,
    'Is women''s identity-locked exit from personal law codes a structural feature of religious community membership, or a contingent feature of current social enforcement?',
    'Longitudinal study of women who exit traditional personal law (via Special Marriage Act or conversion) measuring: sustained community ties, family relationships, employment outcomes, self-reported identity continuity. If identity remains stable post-exit, lock is social enforcement (suppression). If identity ruptures, lock is structural (identity fusion).',
    'If structural: identity_locked classification is correct and exit requires identity transformation. If contingent: should reclassify to constrained (high-cost but surmountable exit) and focus reform on reducing social penalties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Whether identity lock is structural or socially enforced').

omega_variable(
    postcolonial_mandatrophy,
    'Has the parallel personal law system''s original mandate (preventing communal violence during partition, managing religious diversity in newly independent India) been superseded by changed circumstances?',
    'Historical analysis: was partition-era communal violence risk the founding problem? Has that risk diminished or transformed? Counterfactual: would Uniform Civil Code adoption trigger communal violence comparable to partition, or is that threat now performative political rhetoric?',
    'If mandate is obsolete: system is piton from more perspectives (atrophied function maintained by inertia and political fear). If mandate remains live: system is rope or tangled_rope (genuine ongoing coordination function). State''s reluctance to test this empirically is itself diagnostic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(postcolonial_mandatrophy, conceptual, 'Whether partition-era coordination mandate remains live').

omega_variable(
    uniform_code_false_summit,
    'Would a Uniform Civil Code eliminate gender extraction or merely centralize it under state authority?',
    'Comparative analysis of states that implemented uniform civil codes (Turkey, Tunisia): did gender equality improve, or did patriarchal norms migrate into state law? Analysis of proposed UCC drafts in India: do they embed Hindu majoritarian norms while claiming neutrality?',
    'If UCC eliminates extraction: secular feminist scaffold perspective is structurally correct. If UCC centralizes extraction: both current system and proposed UCC are snares, differing only in which institutional authority extracts. The ''reform'' narrative becomes false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_code_false_summit, empirical, 'Whether uniform civil code would eliminate or centralize extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_asymmetry_extraction, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1947, gender_asymmetry_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_1972, gender_asymmetry_extraction, theater_ratio, 25, 0.3).
narrative_ontology:measurement(theater_1997, gender_asymmetry_extraction, theater_ratio, 50, 0.35).
narrative_ontology:measurement(theater_2022, gender_asymmetry_extraction, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(extract_1947, gender_asymmetry_extraction, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(extract_1972, gender_asymmetry_extraction, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(extract_1997, gender_asymmetry_extraction, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(extract_2022, gender_asymmetry_extraction, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1947, gender_asymmetry_extraction, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(suppress_1972, gender_asymmetry_extraction, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(suppress_1997, gender_asymmetry_extraction, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(suppress_2022, gender_asymmetry_extraction, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_asymmetry_extraction, identity_coordination).
narrative_ontology:affects_constraint(gender_asymmetry_extraction, triple_talaq_instant_divorce).
narrative_ontology:affects_constraint(gender_asymmetry_extraction, hindu_succession_act_gender_parity).
narrative_ontology:affects_constraint(gender_asymmetry_extraction, interfaith_marriage_legal_recognition).

% DUAL FORMULATION NOTE:
% The gender asymmetry extraction is one constraint within a larger family of personal law constraints. Each religious tradition's specific rules (triple talaq in Muslim law, Hindu Succession Act amendments, Christian divorce prohibition) are distinct constraints with their own extractiveness values. This story models the overarching parallel system structure that enables those specific extractions. Network decomposition: the system-level constraint (this story) has moderate extractiveness (0.68) reflecting aggregate gender asymmetry across traditions; specific rule-level constraints have higher or lower extractiveness depending on severity of particular asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
