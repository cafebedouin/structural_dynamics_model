% ============================================================================
% CONSTRAINT STORY: renaissance_marriage_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renaissance_marriage_market, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: renaissance_marriage_market
 *   human_readable: Renaissance Marriage Market
 *   domain: social/economic/gender
 *
 * SUMMARY:
 *   The Renaissance marriage market operated as a mechanism for dynastic
 *   wealth consolidation, political alliance-building, and patrimonial
 *   control disguised as natural social order and coordinating mechanism. The
 *   constraint operated at the intersection of economics (dowry extraction,
 *   property consolidation), gender (female legal subordination and
 *   reproductive control), and power (patriarchal authority and noble
 *   privilege). The marriage market was simultaneously a coordination
 *   mechanism solving the legitimate problem of family alliance and wealth
 *   perpetuation AND an extraction mechanism concentrating reproductive and
 *   economic power in the hands of male patriarchs and male heirs. The
 *   constraint's extractiveness increased over the Renaissance interval as
 *   urbanization and commercial expansion created new wealth to extract,
 *   while formal suppression remained high (legal prohibition on female
 *   property ownership, mandatory obedience, marital rape immunity). Theater
 *   increased slightly as humanist rhetoric about marriage companionship
 *   emerged, masking rather than reducing the underlying asymmetry. The
 *   constraint demonstrates how a single institutional arrangement can
 *   legitimately coordinate real family and dynastic interests while
 *   simultaneously extracting from a population (unmarried daughters, female
 *   economic autonomy) that has no exit option and no voice in the system's
 *   design.
 *
 * KEY AGENTS:
 *   - Unmarried Daughters: Primary victims (powerless/trapped) — lack legal status, property rights, economic mobility; entire life trajectory controlled by father's strategic needs
 *   - Patriarchs/Family Heads: Primary beneficiaries (institutional/arbitrage) — design and control the marriage system to consolidate wealth and power; have full exit options but benefit enormously from participation
 *   - Male Heirs: Secondary beneficiaries (moderate/constrained) — benefit from dynastic consolidation enabled by strategic marriages; constrained by father's primacy and own marriage prospects dependent on family strategy
 *   - Merchant Families: Secondary actors (powerful/mobile) — benefit from marriage-mediated access to noble status and connections; experience asymmetric extraction through dowry and bride value extraction by nobles
 *   - Church/Convent System: Institutional actor (institutional/arbitrage) — provides alternative for unmarriageable daughters; operates as piton (performative spirituality masking economic necessity)
 *   - Guild Families and Urban Merchants: Emerging organized actors (organized/constrained) — developing alternative coordination through guild regulation and merchant endogamy; represent incipient scaffold with sunset trajectory
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (dowry, female legal disability) as immutable features of human reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renaissance_marriage_market, 0.58).
domain_priors:suppression_score(renaissance_marriage_market, 0.72).
domain_priors:theater_ratio(renaissance_marriage_market, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renaissance_marriage_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(renaissance_marriage_market, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(renaissance_marriage_market, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renaissance_marriage_market, tangled_rope).
narrative_ontology:human_readable(renaissance_marriage_market, "Renaissance Marriage Market").
narrative_ontology:topic_domain(renaissance_marriage_market, "social/economic/gender").

domain_priors:requires_active_enforcement(renaissance_marriage_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renaissance_marriage_market, patriarchal_families).
narrative_ontology:constraint_beneficiary(renaissance_marriage_market, male_heirs).
narrative_ontology:constraint_beneficiary(renaissance_marriage_market, dynastic_consolidation).
narrative_ontology:constraint_victim(renaissance_marriage_market, unmarried_women).
narrative_ontology:constraint_victim(renaissance_marriage_market, dependent_daughters).
narrative_ontology:constraint_victim(renaissance_marriage_market, female_economic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMARRIED DAUGHTER (SNARE) — Has no legal status, no property rights, no economic mobility, and no exit option. Economic dependency on father/family is absolute. Marriage is the only socially acceptable exit from daughterhood, yet the marriage itself is arranged by the father to serve dynastic interests. The daughter bears the full extraction: her labor, reproductive capacity, and allegiance serve the father's wealth consolidation. No alternative path exists — remaining unmarried means lifelong dependent status or entry into convent (loss of family resources). Maximum suppression: legal prohibition on independent property holding, social stigma against unmarriage, economic necessities that preclude any autonomy.
constraint_indexing:constraint_classification(renaissance_marriage_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: YOUNGER SON (TANGLED ROPE) — Benefits from the marriage market's coordination function: dynastic consolidation through strategic marriages creates family alliances, pooled resources, and economic stability from which younger sons derive security and opportunity. Yet also bears extraction: excluded from primary inheritance, dependent on the patriarch's choice to support him, subject to the constraint that his own marriage prospects depend on the family's strategic needs. Not powerless, but constrained by patriarchal precedent and resource dependency. Exit options exist (military service, priesthood, merchant career) but at significant cost to status and family ties. Mixed experience: genuine coordination (family alliance) + extraction (subordination to father's will).
constraint_indexing:constraint_classification(renaissance_marriage_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATRIARCH/FAMILY HEAD (ROPE) — Experiences the marriage market as pure coordination: arranging marriages is how families solve the collective action problem of wealth consolidation, alliance-building, and dynastic perpetuation. The patriarch has significant exit options (can choose not to participate in the market, can arrange marriages differently), yet participates because the coordination function genuinely serves the family's interests. The marriage market solves real problems: how to secure advantageous alliances, how to distribute daughters and sons strategically, how to maintain family wealth across generations. Net beneficiary with high agency — the constraint serves the patriarch's interests because he designed the system.
constraint_indexing:constraint_classification(renaissance_marriage_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT/WEALTHY NON-NOBLE FAMILY (TANGLED ROPE) — Powerful agents with genuine economic resources and market alternatives. Benefit from marriage market coordination: can leverage daughters' marriages to gain noble connections, increase social status, secure contracts through alliance. Yet also experience extraction: noble families extract dowries and bride value while potentially granting minimal reciprocal advantage; social hierarchy means even wealthy merchants cannot achieve true parity. Mobile exit options exist (invest in land and titles directly, accumulate wealth through commerce without marriage alliances), but the marriage market offers faster status conversion. Mixed experience of both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(renaissance_marriage_market, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CHURCH AND CONVENT SYSTEM (PITON) — Formally provides an alternative to marriage by offering the convent as a respectable exit for unmarriageable daughters (those without dowry, those with disabilities, those disfavored by family politics). Theater ratio is high (0.55 baseline, likely higher for this perspective): the rhetoric emphasizes spiritual calling and voluntary commitment, yet the structural reality is that most convent entrants lack real choice — they are sent by families who cannot afford to marry them or cannot get them married strategically. The church's actual coordination function (providing shelter, education, social position for women without husbands) is obscured by the performative narrative of religious vocation. The system persists through institutional inertia and because alternative institutions don't exist, not because convents genuinely solve the unmarriageable-daughter problem in alignment with women's interests.
constraint_indexing:constraint_classification(renaissance_marriage_market, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: URBAN GUILD FAMILIES AND MERCHANT ASSOCIATIONS (SCAFFOLD) — Emerging organized actors in Renaissance cities (Venice, Florence, Bruges) who are developing alternative coordination mechanisms through guild regulation and merchant family networks. These actors experience the feudal/noble marriage market as a temporary bottleneck being circumvented through: merchant endogamy (marrying within the merchant class rather than purchasing noble connections), property regulation by guild rules that enable daughters to maintain limited control of assets, and urban legal systems that recognize some female economic agency in widowhood and property inheritance. Sunset clause is incipient: as urban commerce expands and guild power grows, the noble-dominated marriage market loses monopoly on dynastic legitimacy. Over 2-3 generations (late Renaissance), merchant family networks construct parallel marriage coordination systems with lower extraction rates. Theater is moderate and declining: guild regulations are enforced through transparent apprenticeship and property rules, not through supernatural or mystical framing.
constraint_indexing:constraint_classification(renaissance_marriage_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL REPRODUCTION VIEW (MOUNTAIN) — From the civilizational/universal frame, some constraint on female mobility and reproductive partnership choice is inherent to biological reproduction in pre-contraceptive societies: fertility management and paternity certainty create immutable structural tensions between individual reproductive autonomy and collective kinship stability. This perspective naturalizes the constraint as a timeless feature of human social organization. However, the structural data contradicts this: the extractiveness (0.58) and suppression (0.72) metrics reflect contingent institutional choices (dowry systems, legal prohibition on female property ownership, male-only inheritance), not biological necessities. The mountain classification signals a false summit — the 'natural law' framing obscures that societies with different reproductive regimes (Islamic inheritance law granting daughters partial shares, widow-led households in widowhood, merchant family property practices) achieve fertility coordination without the same extraction levels.
constraint_indexing:constraint_classification(renaissance_marriage_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renaissance_marriage_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renaissance_marriage_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renaissance_marriage_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(renaissance_marriage_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(renaissance_marriage_market, TR),
    TR >= 0.70.

:- end_tests(renaissance_marriage_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The market extracts daughters' reproductive capacity, labor potential, and legally-held assets (dowry) to serve patriarchal dynastic interests. Yet not maximal (0.72+) because: (1) families do invest in daughters' welfare and material security through marriage; (2) coordination function is genuine — strategic marriage alliances create real family benefits; (3) emergence of merchant alternatives shows the market is not the only viable coordination mechanism. Suppression (0.72): High. Layered barriers prevent daughters from exiting: legal prohibition on independent property ownership, mandatory obedience to father then husband, no socially acceptable unmarried status except convent, economic dependency precluding autonomous choice, social stigma against female sexual or economic autonomy. Theater ratio (0.55): Moderate. The marriage market's narrative emphasizes dynastic honor, family duty, and providential ordering of society — performative elements masking economic extraction. Yet enforcement is substantially material (legal prohibition, economic dependency, physical restriction) rather than purely theatrical, so the ratio is not extreme. The increase over the interval (0.50 → 0.58) reflects humanist rhetorical flourishes about marriage companionship becoming more prominent even as legal subordination intensified.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the central analytical feature of this constraint. The patriarch's Rope classification and the unmarried daughter's Snare classification both derive from the same ε=0.58 and σ(regional)=0.9, but produce different effective extractiveness (χ) through directionality. This is not inconsistency — it is the system operating as designed. The constraint is tangled rope at the analytical level because it genuinely coordinates (family alliance) while genuinely extracting (from daughters). The perspectives diverge because the beneficiaries experience the coordination function (patriarch sees Rope) while the victims experience the extraction (daughter sees Snare). The scaffold perspective (guild merchants) represents a structural alternative emerging within the same temporal and geographic domain, showing that extraction rates are not immutable — different coordination mechanisms produce different suppression and extractiveness profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's classification is derived from combining (P,T,E,S) with the binary directionality function (beneficiary/victim status and exit options). The engine computes d from: (1) is this agent a beneficiary or victim? (2) what are their exit options? The daughter is a victim with trapped exit → high d → high experienced extractiveness. The patriarch is a beneficiary with arbitrage exit → low d → negative or minimal experienced extractiveness (he benefits from the constraint, so it doesn't feel extractive to him). The younger son is a victim (secondary) with constrained exit → moderate-high d → moderate experienced extractiveness. The merchant is a beneficiary (alliance gain) and victim (dowry loss) with mobile exit → balanced d ≈ 0.50 → moderate experienced extractiveness. Suppression is unscaled by directionality: all agents experience the same 0.72 suppression (legal and economic barriers are universal), but directionality determines whether they experience the constraint as extraction or coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy (misclassifying extraction as pure coordination or vice versa) because the beneficiary/victim structure is unambiguous: patriarchs and male heirs benefit; unmarried daughters are victims. The Rope perspective (patriarch) is legitimate because patriarchs genuinely solve a coordination problem through the marriage market (how to maintain family wealth and alliances across generations). The Snare perspective (daughter) is legitimate because the daughter bears extraction with no exit option. The Tangled Rope claim at the analytical level correctly identifies that the system is hybrid: it coordinates family interests while extracting from daughters. The suppression metric (0.72) is high enough that the system cannot be pure Rope (which would require suppression ≤ 0.40). The theater metric (0.55) is moderate (not extreme), which rules out Piton except for the church's role. The extractiveness (0.58) is high enough for Snare from some perspectives but not universal enough (there is genuine coordination happening) to be Snare at the analytical level. The Tangled Rope classification at the claimed_type level is justified because: (1) genuine coordination function (marriage alliances serve real family interests), (2) asymmetric extraction (from daughters to patriarchs), (3) active enforcement (legal, social, and economic coercion), all three gates satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dowry_mechanism_function,
    'Does the dowry system function primarily as bride price (extraction from the bride''s father) or as inheritance prepayment (coordination mechanism enabling daughters to participate in family wealth)?',
    'Comparative analysis of dowry law across Renaissance states; correlation between dowry size and daughter''s post-marital property control; examination of widow remarriage rights and dowry recovery',
    'If extraction-dominant: increases snare classification prevalence. If inheritance-coordination: more instances justify tangled_rope rather than snare. This determines whether the constraint is primarily extractive or a coordination mechanism with asymmetric terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dowry_mechanism_function, empirical, 'Whether dowry functions as bride price or inheritance prepayment').

omega_variable(
    female_agency_in_negotiation,
    'To what degree did Renaissance daughters exercise influence over their own marriage arrangements despite formal legal subordination?',
    'Letters, diaries, and marriage contracts documenting resistance, negotiation, and refusal; analysis of remarriage patterns showing daughter preferences post-widowhood; comparison of negotiation space across regions and classes',
    'If daughters had significant hidden agency: perspectives should be upgraded from trapped to constrained or mobile in some cases, lowering experienced extractiveness. If agency was minimal: suppression metric should increase, raising snare prevalence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_agency_in_negotiation, empirical, 'Extent of female agency in marriage negotiations').

omega_variable(
    convent_voluntariness_ambiguity,
    'Were convent entrants genuinely making vocational choices or were they primarily responding to family economic necessity?',
    'Analysis of convent demographic patterns; comparison of voluntarily-applying daughters vs. those with family pressure; examination of convent life narratives distinguishing spiritual commitment from pragmatic shelter',
    'If genuinely voluntary for most: convent provides legitimate exit option, lowering powerless/trapped classifications. If primarily economic: convent is a degraded form of the marriage constraint (piton status confirmed), not an escape. Affects assessment of whether unmarriageable daughters are truly trapped or constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convent_voluntariness_ambiguity, empirical, 'Whether convent entrants made genuinely voluntary choices').

omega_variable(
    merchant_family_alternative_trajectory,
    'Did merchant families'' divergent marriage practices (guild endogamy, property transmission to daughters, widow agency) represent a genuine structural alternative or merely a slower version of noble extraction patterns?',
    'Longitudinal tracking of merchant family property transmission across generations; comparison of merchant daughters'' autonomy vs. noble daughters''; analysis of whether merchant endogamy reduced dowry extraction or shifted it to different mechanisms',
    'If genuine alternative: scaffold perspective is well-founded and the constraint is declining in scope. If merchants replicated noble extraction patterns: the market is less bifurcating than theory suggests, and scaffold''s sunset clause is aspirational rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merchant_family_alternative_trajectory, empirical, 'Whether merchant families created genuine alternative coordination').

omega_variable(
    identity_lock_vs_forced_compliance,
    'Did the constraint operate primarily through internalized norms and identity fusion (daughters perceived daughterhood and obedience as natural identity) or through external coercion and legal prohibition?',
    'Textual analysis of daughters'' narratives for language of internal conviction vs. resigned compliance; examination of rare refusals and the daughter''s framing of resistance; comparison of suppression mechanisms across literacy levels (literate women leaving records vs. illiterate women whose compliance we infer)',
    'If identity-locked dominant: constraint operates through cognitive capture even when external barriers are removed (widow remarriage patterns would show persistent compliance to family will). If external coercion dominant: constraint relaxes immediately when legal prohibition is removed (widow agency would show high autonomy). Affects interpretation of which exit option best characterizes trapped vs. identity_locked for unmarried daughters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_forced_compliance, empirical, 'Whether constraint operates through identity fusion or external coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renaissance_marriage_market, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renm_tr_t0, renaissance_marriage_market, theater_ratio, 0, 0.5).
narrative_ontology:measurement(renm_tr_t50, renaissance_marriage_market, theater_ratio, 50, 0.55).
narrative_ontology:measurement(renm_tr_t100, renaissance_marriage_market, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(renm_be_t0, renaissance_marriage_market, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(renm_be_t50, renaissance_marriage_market, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(renm_be_t100, renaissance_marriage_market, base_extractiveness, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renaissance_marriage_market, resource_allocation).
narrative_ontology:affects_constraint(renaissance_marriage_market, female_property_rights_medieval_law).
narrative_ontology:affects_constraint(renaissance_marriage_market, patrilineal_inheritance_system).
narrative_ontology:affects_constraint(renaissance_marriage_market, dowry_extraction_mechanism).

% DUAL FORMULATION NOTE:
% The Renaissance marriage market decomposes into multiple structurally distinct constraints: (1) dowry extraction (ε ≈ 0.65, Snare at victim perspective) — pure transfer of bride's family wealth to groom's family with minimal coordination function; (2) reproductive control (ε ≈ 0.62, Tangled Rope) — coordination of legitimate family lineage management with extraction of female reproductive autonomy and bodily control; (3) property transmission (ε ≈ 0.45, Rope to Tangled Rope) — coordination of intergenerational wealth distribution with gender-based asset exclusion. The current story focuses on the integrated system at the level of marriage-market participation. The dowry and reproductive control stories are downstream constraints with their own ε values and perspectival structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renaissance_marriage_market, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
