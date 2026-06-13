% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract (Secular Contractual Reading)
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   The secular contractual reading of family law authority treats marriage
 *   as a civil contract between autonomous individuals, valid by state
 *   registration alone, dissoluble by the individuals through law, and
 *   gender-symmetric in rights. This reading has become institutionalized in
 *   secular nation-states (France, Germany, India, much of the Anglophone
 *   world) but remains contested where religious law traditions retain
 *   institutional authority (much of the Muslim world, parts of Christian
 *   Europe, Hindu-majority India, Zoroastrian communities). The constraint
 *   embodies a fundamental claim about the SOURCE OF AUTHORITY over kinship:
 *   the state, derived from democratic and constitutional principles, versus
 *   religious and customary authority derived from sacred texts, interpretive
 *   tradition, and community practice. This story instantiates ONLY the
 *   secular contractual reading and its internal logic; the sibling readings
 *   (Christian canonical, Hindu dharmashastra, Muslim shariat, Parsi
 *   Zoroastrian) are separate constraint stories with their own ε values and
 *   structural data.
 *
 * KEY AGENTS:
 *   - autonomous_individuals: the beneficiary seat; treated as self-governing agents; can exit by choosing not to marry, divorcing, or marrying across religious boundaries
 *   - state_law_authority: the agenda-setter; holds the exclusive authority to validate and dissolve marriage within territory; derives legitimacy from democratic legislation and constitutional equality principles
 *   - religious_authorities: the excluded seat; their traditional authority is structurally displaced by this reading; they would dispute the exclusion and assert their authority is not subordinate
 *   - traditionally_governed_communities: the payer seat; must navigate dual legal systems and bear the cost of legal pluralism; also gain state-law protections unavailable in traditional law
 *   - religious_minorities: a beneficiary seat; protected from majoritarian religious law; interfaith marriage becomes possible
 *   - women_and_gender_minorities: a beneficiary seat; granted equal contractual rights in principle (though enforcement varies)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.38).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract (Secular Contractual Reading)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/political/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '080f4083-cc7e-407f-8f6d-d1d86c7f6ca7').
narrative_ontology:cs_kernel_codification('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', formalized).
narrative_ontology:cs_authority_grounding('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', extraction).
narrative_ontology:cs_interpretation_layer_present('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7').
narrative_ontology:cs_reading_relation('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', family_law_authority__christian_canonical_reading, forecloses).
narrative_ontology:cs_reading_relation('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', family_law_authority__hindu_dharmashastra_reading, forecloses).
narrative_ontology:cs_reading_relation('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', foundational, individual_autonomy_supremacy).
narrative_ontology:cs_axiom_status(individual_autonomy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', individual_autonomy_supremacy, deontological).
narrative_ontology:cs_axiom('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', foundational, secular_state_authority_over_kinship).
narrative_ontology:cs_axiom_status(secular_state_authority_over_kinship, holdable).
narrative_ontology:cs_axiom_grounding('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', secular_state_authority_over_kinship, conventional).
narrative_ontology:cs_axiom('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', secondary, gender_equality_in_contract).
narrative_ontology:cs_axiom_status(gender_equality_in_contract, holdable).
narrative_ontology:cs_axiom_grounding('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', gender_equality_in_contract, deontological).
narrative_ontology:cs_reference_frame('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', democratic_secular_state_authority).
narrative_ontology:cs_drift_state('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', contemporary_religious_resurgence, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('080f4083-cc7e-407f-8f6d-d1d86c7f6ca7', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, autonomous_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_law_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, traditionally_governed_communities).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, women_and_gender_minorities).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, traditionally_governed_communities).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, individual_autonomy_supremacy).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, secular_state_authority_over_kinship).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, gender_equality_in_contract).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treated as self-governing agents whose consent (and only consent) constitutes valid marriage. May marry across religious/caste/ethnic lines without religious authority approval. Dissolution is available through divorce on stated grounds, without requiring religious approval. Exit is available: they can choose not to marry, dissolve the marriage through law, or marry someone of different faith. The constraint treats them as juridical equals regardless of gender.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, autonomous_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Establishes and enforces the contractual framework for marriage: registration requirements, validity criteria (age, consent, no impediments), divorce provisions, property rights, succession. Derives legitimacy from democratic legislation and constitutional principles of equality. Adjudicates disputes and enforces the contract through courts. The state holds the exclusive authority to declare what constitutes a valid marriage within its territory.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_law_authority, agenda_setter,
    institutional, generational, analytical, national).

% Are not recognized as having authority to validate or dissolve marriage under this reading. They may perform ceremonies and offer blessings, but only the state registration confers legal marriage status. Their traditional role as arbiters of family law is structurally displaced. They would dispute this exclusion and argue their authority is not subordinate but parallel or superior.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities, excluded,
    organized, generational, constrained, national).

% Face a constraint that overrides customary or religious marriage law where they conflict with state law. They bear the cost of legal pluralism: they must navigate both state registration and their community's traditions, or choose between them. They also benefit from state-law protections (property rights, succession clarity, divorce exit) that traditional law may not provide, particularly for women and minorities within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, traditionally_governed_communities, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, traditionally_governed_communities, beneficiary).

% Are protected from majoritarian religious marriage law under this reading. Those whose religion is minority in the nation-state cannot be compelled into marriage forms dictated by the majority religion. Interfaith marriage becomes legally possible. Dissolution is available regardless of religious opposition. The secular framework provides exit from religiously-enforced family structures.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_minorities, beneficiary,
    moderate, biographical, mobile, national).

% Are granted equal contractual rights regardless of gender under this reading (in principle — enforcement varies). Spousal consent is symmetric; divorce rights are equal; property and succession rights do not depend on gender. Traditional religious law often grants asymmetric rights; the secular framework legally overrides those distinctions, though custom and enforcement gaps may persist in practice.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, women_and_gender_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Examines how this constraint competes with, displaces, or coexists alongside religious family law frameworks in pluralistic societies. Documents the structural logic of each reading and the points of institutional conflict.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, observer_comparative_legal_analysis, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_law_authority).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, legally intelligible marriage framework that permits autonomous individuals to create binding family arrangements recognized across jurisdiction, with transparent exit conditions (divorce) and clear property/succession rules. Reduces transaction costs of family formation by creating a single legal pathway that works across religious and ethnic boundaries.
% TRANSFER_FUNCTION: Transfers the authority to define marriage validity from religious/customary institutions to the state. Transfers the power to authorize or dissolve marriage from religious authorities to state courts. Transfers family law decision-making power from elders/patriarchs to the married couple themselves (in principle). Does not transfer goods or money directly; it reallocates decision-making authority.
% ABSENT_VOICES: Religious authorities and their constituencies who would assert that marriage is not a secular contract but a sacred/dharmic/quranic institution whose authority cannot be transferred to the state. Communities whose customary law differs from state law and who are structurally subordinated by this reading. Those who hold that religious authority is superior to state authority in matters of kinship would dispute this reading's entire premise.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and state authority over marriage law dissolved, the world would reorganize around religious and customary family law frameworks. Interfaith marriage would become impossible in many jurisdictions; women's property and divorce rights would depend on the applicable religious law; the state would lose its ability to enforce uniform family law across population boundaries. The administrative and property consequences would be substantial.
% FOUNDING_PROBLEM: Religious and customary family law frameworks often conflict with each other and with principles of individual autonomy and gender equality. In pluralistic societies, no single religious framework commands universal assent. Marriage law that tracks one religion privileges that religion's members and marginalizes others. A secular, state-administered framework solves the coordination problem of plural authority systems.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for secular family law (legal scholars, women's rights organizations, minority-religion representatives) attest the founding problem is live and serious. Religious authorities dispute that the problem exists, asserting their authority is legitimate and the state's secular framework is the actual violation. Some pragmatists (judges, legislators) acknowledge the problem but debate whether secular law or religious accommodation is the better solution. Comparative legal analysis of jurisdictions with legal pluralism documents the practical conflicts.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.38) because the constraint does solve a genuine coordination problem (plural authority systems) and beneficiaries do collect something real (marriage legal recognition, equal rights, exit through divorce). It is not zero because the constraint's persistence depends on the state exercising monopoly authority over family law — religious authorities and their constituents bear the cost of having their authority displaced, and some individuals bear the cost of legal pluralism (having to navigate both state and customary systems). Suppression is low (0.22) and falling over the 1970-2026 interval because enforcement is increasingly normalized — individuals accept state authority as legitimate and enforcement machinery is not visibly coercive; legitimacy is high. Theater is low and stable (0.18) because the constraint's function (coordinate plural systems, protect minorities, enable exit) is genuine; performative activity is minimal. Accessibility collapse is moderate (0.45) because alternatives to civil marriage do exist (religious marriage, cohabitation, customary union) and are not completely suppressed — the state does not police the absence of marriage — though state registration is the only legally recognized form. Resistance is moderate (0.52) because the constraint meets real resistance from religious authorities and their constituencies who dispute state jurisdiction over family law. The measurement series shows extractiveness and suppression rising through the early decades (as state enforcement machinery was built and religious authority was actively displaced) and then stabilizing as the new framework became institutionalized and legitimized.
 *
 * PERSPECTIVAL GAP:
 *   The secular state and beneficiary individuals experience this constraint as coordinate rope: a real problem solved (plural authority), benefits collected (marriage recognition, equal rights, divorce), alternatives suppressed but not coercively so. Religious authorities and their constituents experience it as tangled rope or snare: their authority is extracted from them (displaced), they bear costs (loss of jurisdictional control), and they are excluded from the conversation. The directionality divergence is structural: the state and autonomous individuals have low d (beneficiaries), while religious authorities have high d (targets of authority displacement). The constraint does NOT compute the same type from every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous individuals are beneficiaries (d near 0.0): the constraint protects their autonomy, grants them equal rights, enables exit through divorce. The state is a partial beneficiary (d near 0.2): it gains tax revenue from marriage registration, gains jurisdictional control over family law, but also bears administration costs. Religious authorities are the target (d near 0.9): their traditional authority is structurally displaced; they bear costs (loss of jurisdictional control, reduced constituency loyalty, conflict with followers who choose secular marriage) and have constrained exit (they cannot opt out of the secular framework's dominion without leaving the territorial jurisdiction). Traditionally-governed communities sit near symmetric (d near 0.5): they gain state-law protections but bear coordination costs (having to navigate dual systems). No override is needed; the beneficiary/victim declarations and exit options produce the right directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — coordination among plural authority systems — remains LIVE and substantially UNSOLVED in pluralistic societies. Religious and secular marriage frameworks continue to contest jurisdiction. The constraint persists not because the founding problem is dead but because the secular state has sufficient institutional power to enforce its reading. This is not mandatrophy; it is active competition between authority systems, with the secular state currently ascendant. However, in societies where religious law retains strong institutional backing (parts of the Muslim world, Hindu-majority India), the constraint does NOT cleanly establish secular state authority — it is contested and partially displaced. The mandatrophy risk is inverted: if the founding problem (plural authority systems) were solved by true integration or consensus, the secular constraint would no longer need to assert monopoly authority so actively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_authority_displacement_legitimacy,
    'Is the displacement of religious authority from family law legitimate derived-from-secular-principles (democratic majority, constitutional equality) or is it an imposition on communities that would prefer religious governance?',
    'Empirical: survey communities under secular family law frameworks asking whether they accept the legitimacy of state authority or view it as imposed. Normative: philosophical debate about whether individual autonomy rights can override communal religious governance preferences.',
    'If displacement is perceived as imposed (not legitimate), the constraint may compute as snare/tangled_rope rather than rope from the religious-authority and traditional-community seats. If displacement is accepted as legitimate-derivation, it remains rope. The extraction level does not change; the certification does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_displacement_legitimacy, preference, 'Whether the constraint''s authority displacement is legitimate pluralism or illegitimate majoritarian imposition.').

omega_variable(
    founding_problem_scope_contest,
    'Does the founding problem — coordination among plural authority systems — require a monopoly solution (one authority only) or can it be solved by a genuine plural-but-equal framework?',
    'Examine jurisdictions with legal pluralism (India, Malaysia, Nigeria) where state and religious law coexist without monopoly. Document whether this produces coordination or chaos. Can individuals choose which framework governs their marriage?',
    'If genuine pluralism can coordinate the plural systems, then the secular constraint''s assertion of MONOPOLY authority is extraction beyond coordination need. If monopoly is the only stable solution, the extraction is justified by coordination function. This would affect the type from the religious-authority and traditional-community seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_scope_contest, empirical, 'Whether coordination requires monopoly authority or permits genuine pluralism.').

omega_variable(
    gender_equality_enforcement_gap,
    'The constraint CLAIMS gender-symmetric rights, but does enforcement track the claim? Do women achieve equal divorce rights, property rights, and succession rights in practice under this reading?',
    'Comparative empirical analysis of actual divorce proceedings, property disputes, and succession cases in jurisdictions using this reading. Document gap between authored symmetric rights and actual outcomes.',
    'A large enforcement gap would suggest the constraint is a false summit (natural law/coordinate reading that in fact extracts asymmetrically). The authored ε might be optimistic; actual ε might be higher if gender hierarchy persists within the framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equality_enforcement_gap, empirical, 'Whether gender-symmetric rights are enforced in practice or persistently violated.').

omega_variable(
    secular_state_authority_scope_claim,
    'Is the state''s authority over family law a legitimate derived-from-contract-and-consent (individuals agree to state jurisdiction) or is it asserted unilaterally as a monopoly?',
    'Examine historical origins of state marriage law: did individuals consent (democratic process) or did the state impose it on pre-existing religious systems? Can individuals opt out?',
    'If authority is derived-from-consent, the constraint is rope. If it is monopoly-asserted, the constraint may compute as snare from the non-consenting religious-authority seat. The question is not empirically resolvable in the past tense (history is written by victors) but is normatively decisive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_state_authority_scope_claim, conceptual, 'Whether state authority over family law is legitimately derived or monopolistically asserted.').

omega_variable(
    secular_reading_vs_religious_readings_foreclosure,
    'Does the secular contractual reading logically foreclose the religious readings (can a single party hold both simultaneously), or do they merely coexist as different parties'' commitments?',
    'Philosophical/logical analysis: does the axiom ''individual autonomy supremacy overrides religious authority'' necessarily rule out ''religious authority is superior''? Can these coexist in one person''s framework without contradiction?',
    'If foreclosure holds, the readings are in logical conflict; a person cannot hold both. If coexistence holds, individuals can (and in pluralistic societies do) navigate both frameworks pragmatically. This affects the type of institutional conflict and the engine''s treatment of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_reading_vs_religious_readings_foreclosure, conceptual, 'Whether the secular and religious readings logically foreclose each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1970, family_law_authority__secular_contractual_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(fami_tr_t1970, projected).
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__secular_contractual_reading, theater_ratio, 1985, 0.14).
narrative_ontology:measurement_basis(fami_tr_t1985, observed).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__secular_contractual_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(fami_tr_t2000, observed).
narrative_ontology:measurement(fami_tr_t2013, family_law_authority__secular_contractual_reading, theater_ratio, 2013, 0.17).
narrative_ontology:measurement_basis(fami_tr_t2013, observed).
narrative_ontology:measurement(fami_tr_t2020, family_law_authority__secular_contractual_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement_basis(fami_tr_t2020, observed).
narrative_ontology:measurement(fami_tr_t2026, family_law_authority__secular_contractual_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(fami_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t1970, family_law_authority__secular_contractual_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(fami_be_t1970, projected).
narrative_ontology:measurement(fami_be_t1985, family_law_authority__secular_contractual_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement_basis(fami_be_t1985, observed).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__secular_contractual_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement_basis(fami_be_t2000, observed).
narrative_ontology:measurement(fami_be_t2013, family_law_authority__secular_contractual_reading, base_extractiveness, 2013, 0.37).
narrative_ontology:measurement_basis(fami_be_t2013, observed).
narrative_ontology:measurement(fami_be_t2020, family_law_authority__secular_contractual_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement_basis(fami_be_t2020, observed).
narrative_ontology:measurement(fami_be_t2026, family_law_authority__secular_contractual_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(fami_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1970, family_law_authority__secular_contractual_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement_basis(fami_su_t1970, projected).
narrative_ontology:measurement(fami_su_t1985, family_law_authority__secular_contractual_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement_basis(fami_su_t1985, observed).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__secular_contractual_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement_basis(fami_su_t2000, observed).
narrative_ontology:measurement(fami_su_t2013, family_law_authority__secular_contractual_reading, suppression_requirement, 2013, 0.23).
narrative_ontology:measurement_basis(fami_su_t2013, observed).
narrative_ontology:measurement(fami_su_t2020, family_law_authority__secular_contractual_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement_basis(fami_su_t2020, observed).
narrative_ontology:measurement(fami_su_t2026, family_law_authority__secular_contractual_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(fami_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel. The secular_contractual_reading asserts state authority, gender symmetry, and registration-only validity. Sibling readings (Christian canonical, Hindu dharmashastra, Muslim shariat, Parsi Zoroastrian) instantiate different authority structures with different ε values. All five are linked via the kernel network. The decomposition is motivated by ε-invariance: the founding problem is identical (plural authority systems), but each reading's solution produces a different extraction profile because the authority MECHANISM and BENEFICIARY STRUCTURE differ across readings. A unified 'family law authority' constraint would collapse these distinctions and hide the structural conflict; five stories make the divergence explicit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
