% ============================================================================
% CONSTRAINT STORY: family_law_authority_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority_flat_control, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority_flat_control
 *   human_readable: Authority to Define Valid Family Formation and Dissolution
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The authority to define valid family formation and dissolution — who can
 *   marry, under what conditions marriages dissolve, who inherits, who gets
 *   custody — is among the most contested domains of state and religious
 *   power. This constraint operates simultaneously as a coordination
 *   mechanism (societies need predictable rules for inheritance disputes and
 *   custody allocation) and an extraction mechanism (the specific boundaries
 *   of recognition concentrate benefits on those whose relationships align
 *   with state or religious orthodoxy). The constraint has cycled through
 *   phases of increasing extraction (1800-1950: expanding state monopoly,
 *   criminalizing common-law marriage, excluding same-sex and interracial
 *   unions) and partial liberalization (1975-2000: marriage equality
 *   movements, gender-neutral custody defaults, recognition of non-biological
 *   parental bonds). The analytical challenge is that the coordination and
 *   extraction functions are structurally inseparable: the same mechanism
 *   that provides legal certainty also draws and enforces exclusionary
 *   boundaries. This is a diagnostic case for tangled rope at the analytical
 *   level — not a rope that could be made less extractive through reform, but
 *   a structure where coordination and extraction are woven together.
 *
 * KEY AGENTS:
 *   - Non-Recognized Unions: Primary victims (powerless/trapped) — relationships outside formal framework face legal invisibility, inheritance exclusion, custody denial
 *   - State Legal Apparatus: Primary beneficiary (institutional/constrained) — monopoly on family definition, concentration of authority, filing fees and court costs
 *   - Religious Institutional Authorities: Secondary beneficiary (institutional/constrained) — where religious law governs family formation, religious authorities hold parallel monopoly
 *   - Propertied Families Under Current Regime: Tertiary beneficiary (powerful/arbitrage) — those whose structures align with formal recognition experience pure coordination
 *   - Marriage Equality Coalitions: Organized reformers (organized/mobile) — expanding recognition boundaries, building alternative frameworks, pursuing scaffold sunset logic
 *   - Contested Custody Claimants: Secondary victims (moderate/constrained) — face high costs navigating system but also benefit when it grants standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority_flat_control, 0.48).
domain_priors:suppression_score(family_law_authority_flat_control, 0.62).
domain_priors:theater_ratio(family_law_authority_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(family_law_authority_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority_flat_control, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority_flat_control, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(family_law_authority_flat_control, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority_flat_control, tangled_rope).
narrative_ontology:human_readable(family_law_authority_flat_control, "Authority to Define Valid Family Formation and Dissolution").
narrative_ontology:topic_domain(family_law_authority_flat_control, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority_flat_control, '33f0e13b-1c72-43c8-926a-65cf1aa1d656').
narrative_ontology:cs_kernel_codification('33f0e13b-1c72-43c8-926a-65cf1aa1d656', formalized).
narrative_ontology:cs_authority_grounding('33f0e13b-1c72-43c8-926a-65cf1aa1d656', extraction).
narrative_ontology:cs_interpretation_layer_present('33f0e13b-1c72-43c8-926a-65cf1aa1d656').
narrative_ontology:cs_created_at('33f0e13b-1c72-43c8-926a-65cf1aa1d656', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(family_law_authority_flat_control, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority_flat_control, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(family_law_authority_flat_control, religious_institutional_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority_flat_control, property_inheritors_under_current_regime).
narrative_ontology:constraint_victim(family_law_authority_flat_control, non_recognized_unions).
narrative_ontology:constraint_victim(family_law_authority_flat_control, excluded_claimants_to_inheritance).
narrative_ontology:constraint_victim(family_law_authority_flat_control, custody_losers_under_formal_rules).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority_flat_control, propertied_families_under_current_regime).
narrative_ontology:constraint_victim(family_law_authority_flat_control, contested_custody_claimants).
narrative_ontology:constraint_victim(family_law_authority_flat_control, excluded_inheritance_claimants).
narrative_ontology:constraint_vindicates(family_law_authority_flat_control, marriage_as_state_institution).
narrative_ontology:constraint_vindicates(family_law_authority_flat_control, inheritance_rule_naturalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Relationships outside the formal recognition framework — same-sex couples in jurisdictions without marriage equality, polyamorous families, cohabiting partners without formal marriage, cross-border unions not recognized domestically. Face legal invisibility: no inheritance rights without specific legal instruments, custody claims denied or heavily burdened, property claims unenforceable. Cannot exit: all legal flows (inheritance, custody, survivorship benefits, tax treatment) run through the formal system. No realistic exit option — alternative arrangements (private contracts, religious-only ceremonies) lack enforceability when challenged.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, non_recognized_unions, payer,
    powerless, biographical, trapped, national).

% The institutional structure that administers family law: courts, civil registries, family law bureaucracies. Sets the formal rules for valid marriage, divorce, inheritance, and custody. Benefits from monopoly on recognition (filing fees, court costs, concentration of authority in state bureaucracy) but also provides genuine coordination services (resolves disputes predictably, prevents violence over inheritance, establishes custody without blood feuds). Could delegate more authority to religious or community institutions but faces political costs from secular coalitions (who see religious authority as extractive) and from beneficiaries of current regime (whose arrangements are protected by existing rules).
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, state_legal_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority_flat_control, state_legal_apparatus, beneficiary).

% In jurisdictions with religious family law systems (Islamic family law in many Muslim-majority states, Orthodox Jewish family courts in Israel, canonical marriage law in some Catholic-majority contexts), religious authorities set and administer family law rules for their communities. Benefit from authority monopoly within their domain but face contestation from secular state systems and from excluded groups within their communities (interfaith couples, same-sex couples, divorced women in systems with gender-asymmetric divorce rules). Could accept pluralism but face internal legitimacy costs. Where religious and state systems overlap, religious authorities experience state monopoly as extraction of their traditional domain.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, religious_institutional_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority_flat_control, religious_institutional_authorities, beneficiary).

% Families whose structures align with formal recognition: opposite-sex marriages (in all jurisdictions), biological parent-child bonds, formal adoptions. Benefit from predictable inheritance rules, custody defaults that favor them, tax and survivorship benefits. Have arbitrage exit options: can jurisdiction-shop for favorable divorce or custody law, use prenuptial agreements and trusts to optimize within the system, relocate to minimize tax burden. The system protects their arrangements and they have resources to navigate it effectively.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, propertied_families_under_current_regime, beneficiary,
    powerful, immediate, arbitrage, national).

% Agents with legitimate relational bonds who face disadvantage under formal custody rules: non-custodial parents (especially fathers in jurisdictions with maternal custody defaults, mothers in jurisdictions with paternal defaults), same-sex partners in jurisdictions with partial recognition, extended family members (grandparents, aunts/uncles) seeking custody or visitation, stepparents without formal adoption. Can navigate the system but at high cost (litigation expense, emotional toll, time). Constrained exit: some can forum-shop across jurisdictions, some can pursue alternative legal strategies (adoption, guardianship, private agreements), but exit is costly and uncertain. Experience both coordination (the system does resolve custody disputes through rules rather than violence) and extraction (the rules systematically favor certain claimants based on formal status rather than actual caregiving relationships).
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, contested_custody_claimants, payer,
    moderate, biographical, constrained, national).

% Those who benefit from current inheritance rules: surviving spouses in formally recognized marriages, biological or adopted children, designated heirs in jurisdictions with testamentary freedom. Inheritance flows to them by default or by will. Have arbitrage options: can structure estates to minimize tax, can jurisdiction-shop for favorable inheritance law, can use trusts and other instruments to optimize. The system's coordination function (predictable inheritance allocation) operates in their favor.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, property_inheritors_under_current_regime, beneficiary,
    powerful, immediate, arbitrage, national).

% Those excluded from inheritance by formal rules: surviving partners in non-recognized unions (no intestacy rights), non-biological caregivers without formal adoption (no inheritance standing), children from dissolved marriages in jurisdictions with disinheritance rules. Trapped: cannot exit the system (all inheritance flows through it) and have no default claim. Must rely on specific legal instruments (wills, trusts, beneficiary designations) that the decedent may not have executed or that may be challenged by formal heirs.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, excluded_inheritance_claimants, payer,
    powerless, biographical, trapped, national).

% Organized movements that expanded formal recognition: marriage equality advocates who won same-sex marriage legalization, gender-neutral custody reform movements, coalitions seeking recognition of non-biological parental bonds (stepparent adoption, de facto parent doctrine, co-parent recognition). Can organize across jurisdictions, forum-shop, build parallel recognition systems (religious ceremonies without state recognition, community-based custody agreements). See the current authority structure as a temporary coordination mechanism whose exclusionary boundaries are being dismantled. Mobile exit options: can build alternatives while pursuing reform from within. The constraint has a sunset from their perspective: as recognition expands and alternative frameworks proliferate, the state monopoly loses force.
narrative_ontology:constraint_stakeholder(family_law_authority_flat_control, marriage_equality_coalitions, observer,
    organized, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Family law authority structures coordinate inheritance allocation (preventing blood feuds and providing predictable property transfer at death), custody resolution (allocating parental rights and obligations without violence), and legal status recognition (establishing who counts as family for purposes of survivorship benefits, hospital visitation, tax treatment, immigration sponsorship). The coordination problem is real: societies need mechanisms to resolve these questions, and the alternative (private negotiation backed by threat of violence) is worse.
% TRANSFER_FUNCTION: The arrangement transfers legal standing, property rights, and parental authority. FROM: relationships outside the formal recognition framework, non-biological caregivers, excluded inheritance claimants, non-custodial parents in gender-biased systems. TO: the state legal apparatus (filing fees, court costs, authority concentration), religious institutional authorities (where they hold family law jurisdiction), propertied families whose structures align with formal rules (inheritance defaults, custody presumptions, survivorship benefits). The transfer mechanism is boundary enforcement: those inside the recognition framework collect benefits, those outside bear costs.
% ABSENT_VOICES: Same-sex couples were excluded from the marriage equality conversation until very recently (1990s-2000s in most jurisdictions; still excluded in many). Polyamorous families are almost entirely outside the conversation — no jurisdiction recognizes multi-partner unions and the discourse treats polyamory as inherently non-coordinatable. Children in custody disputes have representation in theory (guardian ad litem, best-interest-of-child standards) but limited voice in practice. Non-Western family structures (extended family residential patterns, communal childrearing, non-nuclear household forms) were often excluded from the rule-setting conversation when colonial legal systems imposed Western marriage and custody norms. Religious minorities are sometimes excluded in jurisdictions where majority-religion family law is imposed on all (e.g., Hindu or Muslim family law applied to religious minorities in some postcolonial states).
% DISAPPEARANCE_RATIONALE: If formal family law authority disappeared overnight, inheritance disputes would be resolved through private negotiation backed by threat of violence (or through alternative authorities like religious courts or community elders), custody would be contested through physical control and social pressure rather than court orders, and legal status would fragment into multiple overlapping recognitions (employer benefits, insurance designation, hospital visitation) with no coherent framework. Propertied families would seek alternative coordination mechanisms immediately (private contracts, religious authorities, community norms). Non-recognized unions would gain some freedom (no longer face legal invisibility) but also lose some protection (could not invoke state authority when convenient). The world would rearrange itself around alternative authorities and private ordering. The constraint is not a natural fact — it is an institutional arrangement that structures behavior.
% FOUNDING_PROBLEM: The founding problem family law authority structures were built to solve: prevent blood feuds over inheritance (medieval European context: feuding kin groups contesting property transfers at death), protect dependent wives and children in a context of gender-based economic dependency (19th-century context: women had limited property rights and economic opportunities; formal marriage and custody defaults provided some security), establish legal certainty about family status for purposes of property transfer and succession (eliminate ambiguity about who is legitimate heir, who has parental authority, whose marriages count for property law purposes). These were real coordination problems in their historical contexts.
% FOUNDING_PROBLEM_CORROBORATION: The problem's status is contested between seats. Proponents of current authority structures (state legal apparatus, religious authorities, propertied families aligned with current regime) claim inheritance disputes and custody conflicts still require centralized authority and formal boundaries — THEIR corroboration is self-interested. Critics (marriage equality coalitions, non-recognized unions, excluded claimants) argue much of the coordination function could be provided through alternative mechanisms (private contracts with state backstop, plural recognition systems, community-based custody mediation) and that the current authority structure persists primarily to benefit those who hold monopoly power. Disinterested corroboration is limited: legal scholars who study alternative family law regimes (Mary Anne Case, Martha Fineman, Nancy Polikoff) document that recognition boundaries have shifted dramatically over time and across jurisdictions WITHOUT collapse of coordination, suggesting the specific boundaries are more about power consolidation than coordination necessity. But no large-scale natural experiments exist to test whether pluralistic alternatives could fully replace centralized authority.
narrative_ontology:disappearance_verdict(family_law_authority_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-RECOGNIZED UNIONS (SNARE) — Relationships outside the formal recognition framework face structural exclusion from inheritance rights, custody protections, and property claims. Cannot exit the system (all property, custody, and survivorship flows through it) and bear maximum extraction through denial of legal standing. Experience pure extraction: the coordination story ('we're just organizing families') is cover for a mechanism that concentrates benefits on recognized unions and extracts from everyone else through legal invisibility.
constraint_indexing:constraint_classification(family_law_authority_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTESTED CUSTODY CLAIMANTS (TANGLED ROPE) — Agents with legitimate relational bonds (non-custodial parents, extended family, same-sex partners in jurisdictions with partial recognition) who face high costs navigating the formal system but also benefit from its existence when it does grant standing. Constrained exit: can sometimes shift jurisdictions or pursue alternative legal strategies, but at significant cost. Experience both coordination (the system does resolve custody disputes through rules rather than violence) and extraction (the rules systematically favor certain claimants over others based on formal status rather than actual caregiving relationships).
constraint_indexing:constraint_classification(family_law_authority_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTIED FAMILIES UNDER CURRENT REGIME (ROPE) — Those whose family structures align with the formal recognition framework experience the constraint as pure coordination: inheritance flows predictably, custody defaults to recognized parents, property rights transfer cleanly. Arbitrage exit options via jurisdiction shopping, prenuptial agreements, trusts. Net beneficiaries: the system protects their arrangements and they have the resources to optimize within it.
constraint_indexing:constraint_classification(family_law_authority_flat_control, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGAL APPARATUS (TANGLED ROPE) — The institutional structure that administers family law both coordinates (resolves disputes, provides predictability, prevents blood feuds over inheritance) and extracts (maintains monopoly on legitimate family definition, collects filing fees and court costs, concentrates authority in state bureaucracy). Constrained exit: the state could delegate more authority to religious or community institutions but faces political costs. Benefits from the monopoly on recognition but also genuinely provides coordination services.
constraint_indexing:constraint_classification(family_law_authority_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTIONAL AUTHORITIES (TANGLED ROPE) — In jurisdictions with religious family law systems (Islamic family law in many Muslim-majority states, Orthodox Jewish family courts in Israel, canonical marriage law in some Catholic-majority states), religious authorities both coordinate (provide culturally legitimate dispute resolution within their communities) and extract (maintain monopoly on recognition within their domain, exclude interfaith or same-sex unions, concentrate authority in religious hierarchy). Constrained exit: religious institutions could accept pluralism but face internal legitimacy costs. Where religious and state systems compete or overlap, religious authorities experience the state system as extraction and their own system as coordination.
constraint_indexing:constraint_classification(family_law_authority_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MARRIAGE EQUALITY COALITIONS (SCAFFOLD) — Organized movements that expanded the recognition framework (same-sex marriage legalization, gender-neutral custody defaults, recognition of non-biological parental bonds) see the current authority structure as a temporary coordination mechanism whose exclusionary boundaries are being dismantled. Mobile exit options: can organize across jurisdictions, forum-shop, build parallel recognition systems (religious ceremonies without state recognition, community-based custody agreements). The constraint has a sunset: as formal recognition expands and alternative frameworks proliferate, the state monopoly on family definition loses force. The coordination function (predictable inheritance, custody resolution) can persist without the exclusionary extraction (non-recognition of non-traditional families).
constraint_indexing:constraint_classification(family_law_authority_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global analytical view, family law authority structures exhibit both genuine coordination (societies need mechanisms to resolve inheritance disputes, allocate custody, and provide legal certainty about family status) and substantial extraction (the specific boundaries of recognition concentrate benefits on those whose relationships align with state or religious orthodoxy and systematically exclude others). The coordination function is real but the extraction is not incidental — it is built into the authority structure. Tangled rope at the analytical level because the extraction and coordination are structurally inseparable: the same mechanism that provides legal certainty (coordination) also draws and enforces exclusionary boundaries (extraction). The analytical perspective does not reduce to rope or mountain.
constraint_indexing:constraint_classification(family_law_authority_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_law_authority_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_law_authority_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint concentrates substantial benefits on recognized unions and formal inheritors while excluding others, but it also provides genuine coordination services (predictable inheritance, custody resolution without violence). The moderate value reflects that roughly half the system's operation is coordination and half is extractive boundary enforcement. The historical trajectory shows accumulation (rising to 0.58 by 1950 as state monopoly expanded) followed by partial liberalization (falling to 0.48 by 2000 as marriage equality and custody reforms reduced some exclusions). Suppression (0.62): Moderate-high. Significant barriers to exit: all property and custody flows through the formal system, alternative arrangements lack legal enforceability in most jurisdictions, and penalties for non-compliance (inheritance denial, custody loss) are severe. But suppression is not total — some jurisdictions allow religious-only marriages, covenant marriage opt-ins provide some pluralism, and cross-border forum shopping provides constrained exit for those with resources. Theater ratio (0.35): Moderate-low. Family law authority structures are substantially functional, not primarily performative. Courts do resolve custody disputes, inheritance law does allocate property, and marriage recognition does provide legal certainty. The theater component (bureaucratic rituals, formal ceremonies, licensing fees that fund other state functions) is present but not dominant. The modest increase during 1800-1950 reflects bureaucratization; the modest decrease 1975-2000 reflects streamlining reforms.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those inside and outside the formal recognition framework. Propertied families whose relationships align with state or religious orthodoxy experience pure coordination: the system protects their arrangements, inheritance flows predictably, custody defaults to them. Non-recognized unions experience pure extraction: the same system that coordinates for others excludes them through legal invisibility. The analytical challenge is that this is not a bug that can be fixed without changing the system's fundamental structure — the coordination and extraction are woven together. Expanding recognition boundaries (marriage equality, non-biological parental bonds) reduces extraction for previously excluded groups but does not eliminate the structural dynamic: there will always be relationships outside the current boundary, and the authority structure will always concentrate benefits on those inside and extract from those outside. The analytical perspective classifies this as tangled rope rather than rope because the extraction is not incidental to coordination — it is built into the mechanism of drawing and enforcing boundaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-recognized unions are full victims (d → 1.0): trapped exit, no benefits from the system, bear maximum extraction through legal invisibility. Contested custody claimants are partial victims with some agency (d → 0.6-0.7): constrained exit, experience both coordination (when the system grants them standing) and extraction (when it favors others based on formal status). Propertied families aligned with current regime are full beneficiaries (d → 0.1-0.2): arbitrage exit options, net benefit from predictable inheritance and custody defaults. State legal apparatus is primary beneficiary (d → 0.2): constrained exit (could delegate authority but faces political costs), benefits from monopoly on recognition but also provides coordination services. Religious institutional authorities are beneficiaries in jurisdictions where they hold authority (d → 0.2-0.3), victims in jurisdictions where state monopoly excludes religious law (d → 0.7). Marriage equality coalitions are organized reformers with mobile exit (d → 0.4): experience current boundaries as extraction but are building alternatives and expanding recognition. The directionality spread produces the perspectival gap: beneficiaries see rope, trapped victims see snare, constrained agents see tangled rope, organized reformers see scaffold, analytical observers see tangled rope at civilizational scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy dynamics across multiple historical phases. The original mandate (prevent blood feuds over inheritance, provide legal certainty about family status, protect dependent children and spouses) was genuine and the early coordination function was real. But as state authority expanded (1800-1950), the system accumulated extraction: criminalization of common-law marriage eliminated informal arrangements that had previously been recognized, antimiscegenation laws excluded interracial unions, and gender-based custody defaults concentrated parental rights on fathers. By mid-20th century, much of the system's operation was boundary enforcement rather than coordination. Marriage equality movements (1975-2000) partially resolved the mandatrophy by expanding recognition, but the fundamental structure persists: the system still draws exclusionary boundaries, and those boundaries still concentrate benefits and impose costs. The analytical question (omega: coordination_floor_ambiguity) is whether the current level of authority concentration is necessary for coordination or whether a more pluralistic system could provide the same services with lower extraction. If the coordination floor is low, the constraint is a resolved mandatrophy that persists through institutional inertia and beneficiary capture. If the coordination floor is high, the extraction is necessary cost rather than obsolete mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_floor_ambiguity,
    'How much authority concentration is inherent to family law coordination versus extractive monopoly? Could a more pluralistic recognition system (multiple overlapping authorities, private ordering with state backstop) provide the same coordination services with lower extraction?',
    'Comparative analysis of jurisdictions with different recognition regimes: centralized state monopoly vs religious pluralism vs covenant marriage opt-ins vs common-law recognition. Measure inheritance dispute rates, custody litigation costs, and satisfaction of non-traditional families across regimes.',
    'If coordination floor is high: current extraction is necessary cost (closer to rope from more perspectives). If coordination floor is low: much current extraction is unnecessary monopoly rent (closer to snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_ambiguity, empirical, 'Whether authority concentration is coordination necessity or extractive monopoly').

omega_variable(
    recognition_boundary_naturalization,
    'Are the specific boundaries of legitimate family formation (opposite-sex marriage, biological parentage, formal adoption) natural categories that authority structures discover, or constructed categories that authority structures create and then naturalize?',
    'Historical analysis of boundary shifts (common-law marriage recognition and later abolition, same-sex marriage legalization, recognition of non-biological parental bonds). If boundaries are natural, they should be stable across jurisdictions and resistant to political pressure. If constructed, they should vary with political coalitions and cultural norms.',
    'If natural: mountain claim has force; some non-recognition is genuinely coordinate rather than extractive. If constructed: much of what presents as coordination is actually extraction through naturalized exclusion (false summit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_boundary_naturalization, conceptual, 'Whether recognition boundaries are discovered or constructed').

omega_variable(
    exit_option_threshold,
    'At what point do alternative recognition systems (religious ceremonies without state recognition, community custody agreements, cross-jurisdictional forum shopping) constitute real exit versus merely constrained navigation within the dominant system?',
    'Measure enforceability of alternative arrangements: do religious-only marriages confer inheritance rights? Do community custody agreements hold up in state courts when challenged? Do cross-border unions survive return to restrictive jurisdictions?',
    'If alternatives are enforceable: exit is more real, effective extraction is lower, scaffold perspective gains force. If alternatives collapse under state authority: exit is illusory, effective extraction is higher, snare perspective gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_threshold, empirical, 'Whether alternative recognition systems constitute real exit').

omega_variable(
    religious_state_boundary_contestation,
    'In jurisdictions where religious and state family law systems overlap or compete, is the contestation genuine pluralism (both systems coordinate different communities) or jurisdictional extraction (both systems compete to control the same population)?',
    'Analysis of jurisdictions with parallel religious and civil family law (Israel, India, Lebanon, Malaysia). Measure: Can individuals opt between systems? Are boundaries enforced coercively or through voluntary affiliation? Do systems cooperate or compete for jurisdiction?',
    'If genuine pluralism: both systems are closer to rope (coordination for their respective communities). If jurisdictional competition: both systems are closer to tangled rope or snare (extraction through monopoly capture).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_state_boundary_contestation, empirical, 'Whether religious/state overlap is pluralism or jurisdictional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority_flat_control, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(famlaw_theater_1800, family_law_authority_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(famlaw_theater_1850, family_law_authority_flat_control, theater_ratio, 50, 0.32).
narrative_ontology:measurement(famlaw_theater_1900, family_law_authority_flat_control, theater_ratio, 100, 0.38).
narrative_ontology:measurement(famlaw_theater_1950, family_law_authority_flat_control, theater_ratio, 150, 0.41).
narrative_ontology:measurement(famlaw_theater_1975, family_law_authority_flat_control, theater_ratio, 175, 0.37).
narrative_ontology:measurement(famlaw_theater_2000, family_law_authority_flat_control, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(famlaw_extract_1800, family_law_authority_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(famlaw_extract_1850, family_law_authority_flat_control, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(famlaw_extract_1900, family_law_authority_flat_control, base_extractiveness, 100, 0.51).
narrative_ontology:measurement(famlaw_extract_1950, family_law_authority_flat_control, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(famlaw_extract_1975, family_law_authority_flat_control, base_extractiveness, 175, 0.54).
narrative_ontology:measurement(famlaw_extract_2000, family_law_authority_flat_control, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(famlaw_suppress_1800, family_law_authority_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(famlaw_suppress_1900, family_law_authority_flat_control, suppression_requirement, 100, 0.64).
narrative_ontology:measurement(famlaw_suppress_1950, family_law_authority_flat_control, suppression_requirement, 150, 0.71).
narrative_ontology:measurement(famlaw_suppress_2000, family_law_authority_flat_control, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This flat construction models family law authority as a single constraint with contested boundaries. A kernel-reading decomposition would split this into multiple readings (civil-legal reading, religious-doctrinal reading, common-law reading, abolitionist reading) each with different beneficiary/victim structures and different claims about what constitutes legitimate family formation. The flat construction captures the contestation as perspectival disagreement and omega variables rather than as separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
