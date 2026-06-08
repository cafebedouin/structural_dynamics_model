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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Gender Rights Variance Across Personal Law Regimes in India
 *   domain: constitutional_law/legal_pluralism/gender_justice
 *
 * SUMMARY:
 *   India's legal pluralism permits different personal law regimes (Hindu,
 *   Muslim, Christian, Parsi, Secular) to govern marriage, divorce,
 *   maintenance, custody, and inheritance. This creates a structural
 *   constraint where women's rights vary dramatically depending on which
 *   regime applies. Women under uncodified Shariat face triple talaq (until
 *   2017), unequal inheritance, polygamy, and maintenance denial enforced by
 *   community qazis with minimal judicial review. Women under codified Hindu
 *   law face statutory protections but with embedded gender asymmetries
 *   (unequal guardianship, maintenance gaps). Women under secular law
 *   (Special Marriage Act) have gender-neutral rules but face social stigma
 *   and community exit costs. The constraint exhibits snare characteristics:
 *   patriarchal community authorities benefit from legal pluralism that
 *   preserves their control; women in uncodified regimes are trapped by
 *   religious identity, community enforcement, and lack of codified
 *   alternatives; the regime persists through active enforcement (qazi
 *   authority, community sanctions) and suppression of alternatives (social
 *   cost of inter-community marriage, identity lock). The constraint also
 *   exhibits false-summit characteristics: the 'inherent pluralism' framing
 *   naturalizes what is actually a contingent institutional choice that
 *   privileges patriarchal interpretations. The analytical observer risks
 *   seeing legal pluralism as an immutable feature of multi-religious
 *   democracy, when the structural data reveals it as a power distribution
 *   that benefits patriarchal authorities.
 *
 * KEY AGENTS:
 *   - Women in Uncodified Shariat Regimes: Primary victims (powerless/trapped) — face triple talaq, unequal inheritance, polygamy, maintenance denial enforced by community qazis
 *   - Women Locked in Community Identity: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with religious/community membership; exit requires apostasy or inter-community marriage
 *   - Women in Codified Hindu Regime: Secondary victims (moderate/constrained) — benefit from statutory protections but face embedded gender asymmetries; exit to secular regime carries social cost
 *   - Patriarchal Community Authorities: Primary beneficiaries (institutional/arbitrage) — benefit from legal pluralism that preserves community control; can shift between community enforcement and statutory appeal
 *   - Feminist Legal Reform Movements: Organized agents (organized/constrained) — pushing for UCC and gender-justice interventions; constrained by constitutional federalism and political economy
 *   - Constitutional Pluralism Framework: Institutional actor (institutional/arbitrage) — maintains performative pluralism; Articles 25-28 protect religious freedom while Article 44 UCC directive remains aspirational
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power distribution as inherent feature of multi-religious democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_rights_variance, 0.68).
domain_priors:suppression_score(gender_rights_variance, 0.72).
domain_priors:theater_ratio(gender_rights_variance, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_rights_variance, extractiveness, 0.68).
narrative_ontology:constraint_metric(gender_rights_variance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gender_rights_variance, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_rights_variance, snare).
narrative_ontology:human_readable(gender_rights_variance, "Gender Rights Variance Across Personal Law Regimes in India").
narrative_ontology:topic_domain(gender_rights_variance, "constitutional_law/legal_pluralism/gender_justice").

domain_priors:requires_active_enforcement(gender_rights_variance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gender_rights_variance, '05426c41-6f2e-47ec-baee-6dfa09a06d77').
narrative_ontology:cs_kernel_codification('05426c41-6f2e-47ec-baee-6dfa09a06d77', distributed).
narrative_ontology:cs_authority_grounding('05426c41-6f2e-47ec-baee-6dfa09a06d77', extraction).
narrative_ontology:cs_reading_relation('05426c41-6f2e-47ec-baee-6dfa09a06d77', gender_rights_variance__hindu_codified_reading, forecloses).
narrative_ontology:cs_reading_relation('05426c41-6f2e-47ec-baee-6dfa09a06d77', gender_rights_variance__secular_contractual_reading, forecloses).
narrative_ontology:cs_reading_relation('05426c41-6f2e-47ec-baee-6dfa09a06d77', gender_rights_variance__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('05426c41-6f2e-47ec-baee-6dfa09a06d77', gender_rights_variance__parsi_community_reading, coexists_with).
narrative_ontology:cs_axiom('05426c41-6f2e-47ec-baee-6dfa09a06d77', foundational, divine_law_non_negotiable).
narrative_ontology:cs_axiom_status(divine_law_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('05426c41-6f2e-47ec-baee-6dfa09a06d77', divine_law_non_negotiable, theological).
narrative_ontology:cs_axiom('05426c41-6f2e-47ec-baee-6dfa09a06d77', foundational, state_authority_limited_to_non_religious_matters).
narrative_ontology:cs_axiom_status(state_authority_limited_to_non_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('05426c41-6f2e-47ec-baee-6dfa09a06d77', state_authority_limited_to_non_religious_matters, deontological).
narrative_ontology:cs_axiom('05426c41-6f2e-47ec-baee-6dfa09a06d77', secondary, community_qazi_authority_supreme_in_personal_law).
narrative_ontology:cs_axiom_status(community_qazi_authority_supreme_in_personal_law, overridden).
narrative_ontology:cs_axiom_grounding('05426c41-6f2e-47ec-baee-6dfa09a06d77', community_qazi_authority_supreme_in_personal_law, conventional).
narrative_ontology:cs_axiom('05426c41-6f2e-47ec-baee-6dfa09a06d77', secondary, patriarchal_family_structure_divinely_ordained).
narrative_ontology:cs_axiom_status(patriarchal_family_structure_divinely_ordained, overridden).
narrative_ontology:cs_axiom_grounding('05426c41-6f2e-47ec-baee-6dfa09a06d77', patriarchal_family_structure_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('05426c41-6f2e-47ec-baee-6dfa09a06d77', divine_law_supremacy_with_community_enforcement).
narrative_ontology:cs_drift_state('05426c41-6f2e-47ec-baee-6dfa09a06d77', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05426c41-6f2e-47ec-baee-6dfa09a06d77', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_rights_variance, patriarchal_community_authorities).
narrative_ontology:constraint_victim(gender_rights_variance, women_in_uncodified_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gender_rights_variance, women_in_hindu_regime).
narrative_ontology:constraint_victim(gender_rights_variance, women_under_shariat).
narrative_ontology:constraint_victim(gender_rights_variance, women_in_hindu_regime).
narrative_ontology:constraint_victim(gender_rights_variance, feminist_legal_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to uncodified Shariat rules enforced by community qazis. Face triple talaq (until 2017), unequal inheritance (daughters receive half of sons' share), polygamy (husbands can take up to four wives), maintenance denial (husbands can refuse maintenance without legal consequence), and custody loss (fathers have automatic guardianship). Exit requires apostasy or inter-community marriage with severe social cost and family rupture. No codified rules to appeal to; enforcement is through community sanctions (social ostracism, family pressure, economic exclusion).
narrative_ontology:constraint_stakeholder(gender_rights_variance, women_under_shariat, payer,
    powerless, biographical, trapped, national).

% Subject to Hindu Marriage Act 1955 (codified). Benefit from statutory protections: divorce grounds expanded (1976), maintenance rights codified, guardianship reformed. But face embedded gender asymmetries: unequal guardianship (father has preference), maintenance gaps (limited duration, amount), property rights limitations (limited inheritance rights in joint family property). Exit to secular regime (Special Marriage Act) is legally available but carries social cost (community disapproval, family pressure). Can appeal to courts for enforcement; codified rules provide some leverage.
narrative_ontology:constraint_stakeholder(gender_rights_variance, women_in_hindu_regime, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gender_rights_variance, women_in_hindu_regime, beneficiary).

% Qazis, ulema, community leaders, and patriarchal family heads benefit from legal pluralism that preserves their control over marriage, divorce, and inheritance. Uncodified Shariat regimes enable qazi authority without judicial review or written rules. Codified regimes (Hindu, Christian, Parsi) allow community interpretation within statutory framework. Can shift between community enforcement (when it serves patriarchal interest) and statutory appeal (when it serves patriarchal interest). Arbitrage option: can choose which regime applies to which dispute depending on outcome preference. Benefit from lack of codification (uncodified Shariat) and from community enforcement mechanisms (social sanctions, family pressure).
narrative_ontology:constraint_stakeholder(gender_rights_variance, patriarchal_community_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gender_rights_variance, patriarchal_community_authorities, beneficiary).

% Women's rights NGOs, constitutional lawyers, Supreme Court judges, and feminist activists see the constraint as a coordination failure with extractive overlay. Pushing for UCC (Uniform Civil Code) as sunset mechanism and for gender-justice interventions (triple talaq ban, maintenance rulings, custody reforms). Constrained by constitutional federalism (Articles 25-28 protect religious freedom), political economy of community mobilization (patriarchal authorities have strong community support), and institutional inertia (UCC has been aspirational for 75 years). Have achieved some victories (triple talaq ban 2017, maintenance rulings) but face ongoing resistance from patriarchal authorities and conservative political movements.
narrative_ontology:constraint_stakeholder(gender_rights_variance, feminist_legal_reformers, payer,
    organized, generational, constrained, national).

% The Articles 25-28 framework (religious freedom + UCC directive) is a non-agent entity (a doctrine, not an actor) but shapes the constraint's operation. Articles 25-28 protect religious freedom and community autonomy; Article 44 directs the state to work toward a UCC. The framework is performative: it maintains legal pluralism while the UCC directive remains aspirational. Constitutional courts issue gender-justice interventions (triple talaq ban, maintenance rulings) that are theatrically framed as 'protecting religious freedom' while actually constraining it. The framework persists through institutional inertia — it satisfies neither gender justice advocates nor religious conservatives, but neither coalition has sufficient power to replace it.
narrative_ontology:constraint_stakeholder(gender_rights_variance, constitutional_pluralism_framework, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(gender_rights_variance, constitutional_pluralism_framework).

% Supreme Court has issued gender-justice interventions: triple talaq ban (2017), maintenance rulings (Danial Latifi, Shayara Bano), custody reforms. These interventions shift authority grounding from religious textualism to constitutional gender justice. But interventions are constrained by constitutional federalism (cannot fully override Articles 25-28) and by political economy (patriarchal authorities have strong community support). Interventions are pushing for a reading shift (from Shariat reading to gender-justice reading) but face ongoing resistance and legislative backlash (Muslim Women (Protection of Rights on Divorce) Act 2019 codified triple talaq ban but with limited maintenance provisions).
narrative_ontology:constraint_stakeholder(gender_rights_variance, supreme_court_interventions, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adjudicate the validity of marriage, terms of divorce, maintenance rights, custody allocation, and inheritance distribution across multiple religious communities with different legitimacy claims (scriptural, statutory, customary, contractual). The coordination problem is real: a multi-religious society must have some mechanism for resolving marriage disputes, and different communities have different legitimacy claims about what that mechanism should be.
% TRANSFER_FUNCTION: The constraint transfers rights and obligations from women to patriarchal authorities. In uncodified Shariat regimes: triple talaq (divorce right transfers from women to husbands), maintenance denial (financial obligation transfers from husbands to women's families), unequal inheritance (property transfers from daughters to sons), polygamy (marital rights transfer from wives to husbands). In codified regimes: similar transfers but with statutory constraints. The transfer is gendered: men retain control over marriage, divorce, and inheritance; women bear the costs.
% ABSENT_VOICES: Women in uncodified Shariat regimes are partially absent from the constitutional conversation: they are not represented in qazi councils, have limited access to courts, and face community sanctions for appealing to secular authorities. Inter-community women (those in inter-faith marriages) are absent from community authority structures. Egalitarian Islamic law scholars (those advocating gender-egalitarian interpretations of Shariat) are marginalized by conservative ulema. Secular women (those who have opted into Special Marriage Act) are absent from religious community authority structures. The absence is structural: the regime does not include these voices in authority adjudication.
% DISAPPEARANCE_RATIONALE: If the gender_rights_variance constraint disappeared overnight (i.e., if legal pluralism were replaced by a gender-egalitarian UCC), the world would rearrange substantially. Patriarchal community authorities would lose control over marriage, divorce, and inheritance. Women would gain uniform gender-egalitarian rights across all communities. Inter-community marriages would become legally straightforward. Community enforcement mechanisms (qazi authority, social sanctions) would be replaced by secular judicial enforcement. Religious communities would lose the ability to enforce patriarchal rules through legal pluralism. The rearrangement would be significant: the constraint is not a natural law but a contingent institutional choice that enables patriarchal authority.
% FOUNDING_PROBLEM: The founding problem was protecting minority religious communities from majoritarian state imposition. When India adopted the Constitution (1950), the Articles 25-28 framework was designed to protect Hindu, Muslim, Christian, Parsi, and other religious minorities from a Hindu-majoritarian state imposing Hindu law on all communities. The framework allowed each community to maintain its own personal law (marriage, divorce, inheritance) according to its own religious traditions. This was a genuine coordination problem: how to protect religious freedom while maintaining a unified state?
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from feminist legal scholars (Flavia Agnes, Ratna Kapur) who argue that legal pluralism has been captured by patriarchal authorities. Corroboration from Supreme Court interventions (triple talaq ban, maintenance rulings) that implicitly acknowledge the founding problem is dead and that gender justice is now the priority. Corroboration from inter-community women and women's rights organizations who argue that the framework enables patriarchal extraction. No corroboration from patriarchal community authorities, who continue to argue that the framework protects religious freedom. The corroboration is asymmetric: gender justice advocates agree the founding problem is dead; patriarchal authorities deny it.
narrative_ontology:disappearance_verdict(gender_rights_variance, world_rearranges).
narrative_ontology:founding_problem_status(gender_rights_variance, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER UNCODIFIED SHARIAT (SNARE) — Trapped by religious identity, community enforcement, and lack of codified alternatives. Triple talaq (pre-2017), unequal inheritance, polygamy, and maintenance denial are enforced through community qazis with minimal judicial review. Exit requires apostasy or inter-community marriage with severe social cost. Maximum extraction with minimal coordination function.
constraint_indexing:constraint_classification(gender_rights_variance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN LOCKED IN COMMUNITY IDENTITY (SNARE) — Structurally mobile (could legally opt into Special Marriage Act) but identity-fused with religious/community membership. Exit would require abandoning family, faith identity, and social belonging. The binding mechanism is cognitive (identity constituted through community) rather than purely material. Experiences the constraint as unchangeable from within the identity frame, though structural mobility exists.
constraint_indexing:constraint_classification(gender_rights_variance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN IN CODIFIED HINDU REGIME (TANGLED ROPE) — Genuine coordination function (Hindu Marriage Act 1955 codifies marriage, divorce, maintenance, custody) with embedded asymmetric extraction (unequal guardianship, maintenance gaps, property rights limitations). Exit to secular regime carries career and social costs but is legally available. Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(gender_rights_variance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PATRIARCHAL COMMUNITY AUTHORITIES (ROPE) — Benefit from legal pluralism that preserves community control over marriage, divorce, and inheritance. Uncodified Shariat regimes enable qazi authority without judicial review. Codified regimes (Hindu, Christian, Parsi) allow community interpretation within statutory framework. Arbitrage option: can shift between community enforcement and statutory appeal depending on outcome preference. Experiences constraint as coordination mechanism preserving community autonomy.
constraint_indexing:constraint_classification(gender_rights_variance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEMINIST LEGAL REFORM MOVEMENTS (TANGLED ROPE) — Organized agents (women's rights NGOs, constitutional lawyers, Supreme Court interventions) see the constraint as a coordination failure with extractive overlay. Genuine coordination problem: how to adjudicate marriage/divorce/inheritance across communities? Extractive overlay: current solution preserves patriarchal authority. Constrained by constitutional federalism (Articles 25-28 protect religious freedom) and political economy of community mobilization. Pushing for UCC (Uniform Civil Code) as sunset mechanism.
constraint_indexing:constraint_classification(gender_rights_variance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL PLURALISM FRAMEWORK (PITON) — The Articles 25-28 framework (religious freedom + UCC directive) is largely performative. The UCC directive (Article 44) has been aspirational for 75 years without implementation. Constitutional courts issue gender-justice interventions (triple talaq ban, maintenance rulings) that are theatrically framed as 'protecting religious freedom' while actually constraining it. The pluralism framework persists through institutional inertia — it satisfies neither gender justice advocates nor religious conservatives, but neither coalition has sufficient power to replace it. Theater ratio reflects the gap between constitutional aspiration (UCC) and institutional reality (legal pluralism maintained).
constraint_indexing:constraint_classification(gender_rights_variance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some legal pluralism is inherent to multi-religious societies: no single legal regime can satisfy all communities' legitimacy claims simultaneously. The tension between religious freedom (Articles 25-28) and gender justice (Articles 14-15) is a structural feature of constitutional democracy, not a contingent institutional arrangement. However, the structural data contradicts this: identifiable beneficiaries (patriarchal authorities) exist, suppression is high, and the constraint persists through active enforcement. The engine will compute this as a false summit, revealing that 'inherent pluralism' naturalizes what is actually a contingent power distribution.
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
 *   Extractiveness (0.68): High. Patriarchal community authorities extract substantial benefit from legal pluralism: they maintain control over marriage, divorce, and inheritance without state codification or judicial review. The extraction is not maximal (0.72) because some women have exit options (Special Marriage Act, codified regimes) and because Supreme Court interventions (triple talaq ban, maintenance rulings) have constrained the most egregious practices. The upward trajectory (0.52 → 0.68 over 30 years) reflects accumulating extraction as community authorities have hardened enforcement mechanisms in response to feminist legal challenges. Suppression (0.72): High and stable. Suppression operates through multiple mechanisms: religious identity (women cannot exit without apostasy), community enforcement (social sanctions, family pressure), lack of codification (uncodified Shariat has no written rules women can appeal to), and identity lock (women's self-concept is constituted through community membership). The stable trajectory reflects that suppression mechanisms have not weakened despite legal reforms — they have adapted. Theater ratio (0.38): Moderate-low. The constraint has genuine coordination function (adjudicating marriage/divorce/inheritance across communities) alongside extraction. The theater is not high because the coordination problem is real and the regime does solve it (albeit in a patriarchal way). The slight upward trajectory reflects increasing performativity of constitutional pluralism framework (Articles 25-28 framed as protecting religious freedom while actually constraining gender justice).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Women under uncodified Shariat see pure extraction (Snare) — the regime offers no coordination benefit, only patriarchal control. Women in codified Hindu regime see mixed coordination and extraction (Tangled Rope) — the regime solves the coordination problem but with embedded gender asymmetry. Patriarchal authorities see coordination (Rope) — the regime preserves community autonomy and solves the problem of adjudicating marriage across communities. Feminist reformers see a coordination failure with extractive overlay (Tangled Rope) — the problem is real but the solution privileges patriarchal authority. The constitutional framework sees itself as pluralism (Piton) — performatively maintaining Articles 25-28 while the UCC directive remains aspirational. The civilizational observer risks seeing natural law (Mountain) — legal pluralism is inherent to multi-religious democracy — but the structural data reveals this as a false summit: identifiable beneficiaries exist, suppression is high, and the constraint persists through active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Women in uncodified Shariat have d ≈ 1.0 (full targets): they are powerless, trapped, and bear maximum extraction. Women in codified Hindu regime have d ≈ 0.65 (partial targets): they are moderate power, constrained exit, and bear significant but not maximal extraction. Patriarchal authorities have d ≈ 0.1 (full beneficiaries): they are institutional power, arbitrage exit, and collect from the constraint. Feminist reformers have d ≈ 0.55 (partial targets): they are organized power, constrained exit, and bear extraction through blocked reform agenda. The constitutional framework has d ≈ 0.2 (beneficiary): it is institutional power, arbitrage exit, and benefits from maintaining the status quo. The engine derives these values from beneficiary/victim declarations and exit modulation; the commentary reflects the structural reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the original mandate (adjudicate marriage/divorce/inheritance across communities while respecting religious freedom) has outlived its function. The Articles 25-28 framework was designed to protect minority religious communities from majoritarian state imposition. But it has become a mechanism for preserving patriarchal authority within those communities, particularly in uncodified Shariat regimes. The UCC directive (Article 44) was meant to resolve this by creating a gender-egalitarian secular alternative. But 75 years later, the UCC remains aspirational while legal pluralism persists. The constraint persists not because the original mandate is still live, but because patriarchal authorities benefit from the status quo and have sufficient power to block reform. The feminist legal reform movements are pushing for a sunset (UCC implementation or gender-justice interventions that constrain patriarchal authority). The constitutional framework is maintaining performative pluralism (Articles 25-28 framed as protecting religious freedom) while the actual function (preserving patriarchal authority) has become extractive. This is the classic mandatrophy pattern: the original coordination function (protecting religious freedom) has been captured by patriarchal authorities and weaponized against gender justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_freedom_vs_gender_justice,
    'Is the gender rights variance a necessary feature of protecting religious freedom, or a contingent institutional choice that privileges patriarchal interpretations?',
    'Comparative analysis: jurisdictions with religious freedom protections (Canada, South Africa, Germany) and their gender-justice outcomes. Examination of whether gender-egalitarian interpretations of Islamic, Hindu, Christian law are structurally possible or foreclosed by the current regime.',
    'If necessary: mountain classification confirmed — pluralism is inherent to multi-religious democracy. If contingent: snare classification confirmed — the regime privileges patriarchal readings and forecloses egalitarian alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_freedom_vs_gender_justice, conceptual, 'Whether gender variance is inherent to religious pluralism or a contingent power choice').

omega_variable(
    exit_cost_measurement,
    'What are the actual material, social, and identity costs for women exiting from uncodified Shariat to secular regime? Are these costs structural (material barriers) or internalized (identity fusion)?',
    'Ethnographic study of women who have opted into Special Marriage Act; measurement of post-exit suppression (does it persist after legal exit?); tracking of family/community reintegration costs.',
    'If costs are primarily structural: trapped classification confirmed. If costs are primarily internalized: identity_locked classification confirmed. If mixed: both mechanisms operate and suppression is higher than structural barriers alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Composition of exit costs: structural vs internalized').

omega_variable(
    egalitarian_interpretation_foreclosure,
    'Are gender-egalitarian interpretations of Islamic law (e.g., Shariat-compliant divorce rights for women, equal inheritance) structurally possible within the current regime, or does the regime actively foreclose them?',
    'Analysis of qazi rulings and community enforcement: do egalitarian interpretations exist in practice? Are they suppressed by community authorities or by lack of codification? Comparison with jurisdictions (Malaysia, Egypt) where egalitarian Islamic law reforms have been implemented.',
    'If egalitarian interpretations are possible but suppressed: snare classification confirmed with active foreclosure. If egalitarian interpretations are structurally impossible: mountain classification (inherent to uncodified law). If egalitarian interpretations exist but are marginalized: tangled rope classification (coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(egalitarian_interpretation_foreclosure, empirical, 'Whether egalitarian Islamic law interpretations are foreclosed or suppressed').

omega_variable(
    false_summit_natural_law_claim,
    'Is the ''inherent pluralism'' framing a genuine natural law (multi-religious societies necessarily have legal variance) or a false summit that naturalizes a contingent power distribution?',
    'Examination of whether the current gender-rights variance is the only possible pluralism outcome, or whether alternative pluralisms (gender-egalitarian across all regimes) are structurally possible. Comparison with jurisdictions that have achieved pluralism + gender equality.',
    'If false summit: the mountain classification is a cover story for snare. The constraint persists because patriarchal authorities benefit, not because pluralism is inherent. Reclassification to snare or tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether ''inherent pluralism'' is natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_rights_variance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grv_tr_t0, gender_rights_variance, theater_ratio, 0, 0.32).
narrative_ontology:measurement(grv_tr_t10, gender_rights_variance, theater_ratio, 10, 0.35).
narrative_ontology:measurement(grv_tr_t20, gender_rights_variance, theater_ratio, 20, 0.38).
narrative_ontology:measurement(grv_tr_t30, gender_rights_variance, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(grv_be_t0, gender_rights_variance, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(grv_be_t10, gender_rights_variance, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(grv_be_t20, gender_rights_variance, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(grv_be_t30, gender_rights_variance, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(grv_su_t0, gender_rights_variance, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(grv_su_t10, gender_rights_variance, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(grv_su_t20, gender_rights_variance, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(grv_su_t30, gender_rights_variance, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_rights_variance, enforcement_mechanism).
narrative_ontology:affects_constraint(gender_rights_variance, triple_talaq_enforcement).
narrative_ontology:affects_constraint(gender_rights_variance, maintenance_denial_mechanism).
narrative_ontology:affects_constraint(gender_rights_variance, unequal_inheritance_distribution).
narrative_ontology:affects_constraint(gender_rights_variance, inter_community_marriage_stigma).

% DUAL FORMULATION NOTE:
% The gender_rights_variance constraint is a family of related constraints, each with different ε values. Triple talaq enforcement (pre-2017) had ε ≈ 0.85 (pure extraction, minimal coordination). Maintenance denial has ε ≈ 0.72 (extraction with some coordination function). Unequal inheritance has ε ≈ 0.65 (coordination + extraction). Inter-community marriage stigma has ε ≈ 0.58 (coordination + extraction + identity lock). The family is linked by the shared kernel (marriage authority) and the shared beneficiary (patriarchal authorities). Each constraint has its own perspectives and measurements; the family structure enables analysis of how different mechanisms within legal pluralism produce different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gender_rights_variance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
