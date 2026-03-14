% ============================================================================
% CONSTRAINT STORY: germanic_honor_code
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_germanic_honor_code, []).

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
 *   constraint_id: germanic_honor_code
 *   human_readable: Germanic Honor Code System
 *   domain: social/cultural/institutional
 *
 * SUMMARY:
 *   The Germanic honor code represents a system of constraint that
 *   coordinates warrior elite competition while extracting obedience and
 *   labor from subordinates and women. Spanning from Early Germanic tribal
 *   structures through the High Medieval period, this constraint exhibits the
 *   full range of perspectival classification: the warrior elite experience
 *   coordination (Rope), organized clans experience mixed coordination and
 *   extraction (Tangled Rope), non-elite males and women experience pure or
 *   near-pure extraction (Snare), and by the High Medieval period the code
 *   degrades into performative ritual maintained by institutional inertia
 *   (Piton). The constraint's extractiveness increases from ~0.35 (early
 *   period, when honor code was primarily competitive coordination among
 *   rough equals) to ~0.58 (late period, when it had become a stratified tool
 *   for elite dominance). Theater ratio rises correspondingly: the code's
 *   functional enforcement capacity declined as feudal legal structures
 *   formalized, but the performative theater (heraldry, knightly ceremony,
 *   literary romance) increased, creating an inversion where appearance of
 *   honor superseded actual enforcement. The suppression (0.68) reflects
 *   substantial barriers to exit and participation: legal status hierarchies,
 *   kinship obligation structures, and internalized identity fusion make the
 *   code difficult to escape even for those with theoretical alternatives.
 *
 * KEY AGENTS:
 *   - Warrior Elite (Jarls, Kings): Primary beneficiaries (powerful/mobile) — extract status, wealth, and political power through honor competition; see code as coordination mechanism enabling peer competition
 *   - Freeborn Non-Elite Males: Moderate victims (moderate/constrained) — participate in honor system but lack elite status; constrained by vendetta obligations and military service while receiving limited benefits
 *   - Women (All Social Ranks): Severe victims (powerless/trapped or identity_locked) — structurally excluded from honor-code decision-making; trapped by kinship structures and reproductive obligations; may be identity_locked through cultural internalization of honor values
 *   - Thralls/Unfree Males: Extreme victims (powerless/trapped) — legally excluded from honor code participation; obligated to serve without rights; cannot defend honor
 *   - Clan/Family Structures: Organized enforcers (organized/constrained) — coordinate around honor norms; both benefit from collective defense capacity and bear extraction through vendetta obligation cycles
 *   - Institutional Continuity (Church, Medieval Nobility): Vestigial beneficiaries (institutional/arbitrage) — maintain honor-code language for legitimacy after functional enforcement declines; use performative theater as institutional substrate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germanic_honor_code, 0.58).
domain_priors:suppression_score(germanic_honor_code, 0.68).
domain_priors:theater_ratio(germanic_honor_code, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germanic_honor_code, extractiveness, 0.58).
narrative_ontology:constraint_metric(germanic_honor_code, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(germanic_honor_code, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(germanic_honor_code, tangled_rope).
narrative_ontology:human_readable(germanic_honor_code, "Germanic Honor Code System").
narrative_ontology:topic_domain(germanic_honor_code, "social/cultural/institutional").

domain_priors:requires_active_enforcement(germanic_honor_code).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(germanic_honor_code, male_warrior_elite).
narrative_ontology:constraint_beneficiary(germanic_honor_code, tribal_leadership).
narrative_ontology:constraint_victim(germanic_honor_code, women).
narrative_ontology:constraint_victim(germanic_honor_code, lower_status_males).
narrative_ontology:constraint_victim(germanic_honor_code, outsiders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN IN TRIBAL CONTEXT (SNARE) — No exit from honor obligations that constrain sexuality, autonomy, and economic participation. Trapped by kinship structures, property dependency, and social exclusion from decision-making. Bears extraction through reproductive labor obligations and restricted agency while receiving no reciprocal protection in practice.
constraint_indexing:constraint_classification(germanic_honor_code, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THRALL/UNFREE MALE (SNARE) — Legally excluded from honor code entirely; cannot defend honor through ritual because unworthy. Trapped by legal status. Experiences pure extraction: obligated to serve honor-code bearers without reciprocal rights or standing. Cannot exit through rebellion without death.
constraint_indexing:constraint_classification(germanic_honor_code, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: FREEBORN NON-ELITE MALE (TANGLED ROPE) — Partial membership in honor system: can defend personal honor through combat but lacks elite status and political voice. Constrained by both obligations (vendetta cycles, military service) and limited benefits (lower social standing, reduced wealth). Experiences genuine coordination function (feud resolution, alliance-building) alongside asymmetric extraction favoring elites.
constraint_indexing:constraint_classification(germanic_honor_code, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: WARRIOR ELITE (ROPE) — Primary beneficiary experiencing honor code as coordination mechanism for consolidating power and managing competition among peers. Can negotiate, arbitrage between tribal groups, or relocate. Sees the constraint as enabling rather than restrictive: shared honor standards create predictable competition, allow reputation-building, and facilitate alliance formation. Low experienced extraction.
constraint_indexing:constraint_classification(germanic_honor_code, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZED CLAN STRUCTURES (TANGLED ROPE) — Coordinated around honor defense, resource protection, and alliance maintenance. Clan enforcement of honor norms creates internal solidarity but also extracts from clan members (especially subordinates) through vendetta obligations and property claims. Coordination function genuine (protection, resource pooling); extraction function also genuine (obligation cycles, limited autonomy).
constraint_indexing:constraint_classification(germanic_honor_code, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INSTITUTIONAL CONTINUITY (PITON) — By High Medieval period, honor code has degraded into mostly performative ritual (heraldry, knightly ceremonies, courtly literature celebrating honor) with declining functional enforcement. Theater ratio elevated as actual enforcement capacity declined. Institutions (nobility, church) maintain honor language for legitimacy while material extraction mechanisms shift to formalized feudal-legal structures. The honor code persists through institutional inertia, not functional necessity.
constraint_indexing:constraint_classification(germanic_honor_code, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risks framing honor codes as inevitable features of warrior societies, treating as natural law what is actually a contingent institutional arrangement designed to concentrate power. Risk of naturalizing the extraction as 'inherent to male status competition' rather than recognizing it as a specific historical construction.
constraint_indexing:constraint_classification(germanic_honor_code, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germanic_honor_code_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(germanic_honor_code, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germanic_honor_code, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(germanic_honor_code, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(germanic_honor_code, TR),
    TR >= 0.70.

:- end_tests(germanic_honor_code_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The honor code extracts substantial labor (military service, feud participation, resource claims), reproductive labor (women's obligation to produce heirs), and psychological compliance (internalized identity through honor). However, it is not a pure extraction mechanism (snare): genuine coordination functions exist among elites and within clans. The extractiveness trajectory (0.35 → 0.58 over 10 time periods) reflects increasing stratification as the system evolved: early Germanic tribes had more horizontal honor competition; by the High Medieval period, honor code had become formalized as a justification for hierarchical feudal extraction. Suppression (0.68): High. Significant barriers to exit and non-participation include legal status (slave/thrall/serf/free hierarchy), kinship obligation structures that make exit costly, cultural internalization of honor as identity, and credible violence for those who refuse participation. The suppression is not total — organized groups can negotiate vendetta settlements, and some mobility is possible — but significant. Theater ratio (0.65): Moderate-high, increasing over time. Early period had lower theater: honor enforcement relied on actual feud/combat capacity and reputation within local networks. By High Medieval period, theater had expanded significantly: heraldic systems, courtly literature, ceremonial knighthood, and church blessing became primary honor signals, while actual enforcement capacity declined. Theater ratio increase suggests the code's shift from functional enforcement to institutional legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional arrangement can be perceived as radically different depending on structural position. From the warrior elite perspective, the honor code is Rope: it solves the coordination problem of peer competition and alliance formation in warrior societies with no central authority. From the woman's perspective, it is Snare: a system that extracts reproductive labor, restricts mobility and economic participation, and offers no reciprocal protection or voice. From the clan perspective, it is Tangled Rope: genuinely coordinates collective defense and resource management while extracting from clan members through vendetta obligation cycles and subordination. From the thrall's perspective, it is an exclusionary system (Snare at extreme) — the code doesn't even deign to include thralls as potential honor-bearers, treating them as sub-human and extracting their labor without reciprocal standing. From the institutional perspective at high temporal scale, it is Piton: the code's performative theater (heraldry, ceremony, literary romance) has replaced functional enforcement as actual feudal-legal structures took over constraint management. The analytical observer risks seeing this as Mountain (natural feature of warrior societies) when it is actually a historically contingent institutional construction designed to concentrate power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to extraction flow. Elite warriors experience low d (beneficiaries with mobile exit) → low/negative f(d) → experience low chi; they control honor-code rules and can arbitrage between communities. Organized clans experience moderate d (both coordinators and enforcers) → moderate f(d) → experience moderate chi reflecting mixed function. Non-elite males experience high d (partial victims with constrained exit) → high f(d) → experience high chi; they can theoretically leave but face severe costs. Women experience maximum d (trapped victims) → maximum f(d) → experience maximum chi; they have no voice in rule-setting and structural exit barriers are absolute. Thralls experience maximum d plus legal non-personhood → maximum extraction without even the dignity of honor-code participation. The directionality pipeline reveals that suppression mechanisms are not uniform: elite suppression is low (they set the rules), organized-group suppression is moderate (internal enforcement + external threat), non-elite suppression is high (external enforcement + identity capture), thrall suppression is maximal (legal status + violence).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that high extractiveness (0.58) is compatible with genuine coordination function because the extraction is highly asymmetric and directed at disempowered groups. The honor code is NOT a pure snare masquerading as rope (the mandatrophy risk) — it genuinely coordinates warrior elite competition and clan collective action. However, this coordination is purchased through severe extraction from women, thralls, and non-elites. The Tangled Rope classification correctly captures this: the code has both a real coordination function (peer competition, alliance formation, vendetta settlement) AND asymmetric extraction (from those excluded from rule-setting). The increasing extractiveness trajectory (0.35 → 0.58) reflects growing stratification: as the code evolved from horizontal peer coordination to vertical justification for feudal hierarchy, the coordination benefit shifted upward (only elites benefit) while extraction shifted downward (women and non-elites bear increasing costs). The piton-stage perspectives in the High Medieval period correctly identify that performative theater has replaced functional enforcement — the code persists through institutional legitimacy and identity internalization, not because it still solves coordination problems efficiently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_function_vs_extraction_ratio,
    'What proportion of honor code enforcement actually coordinates legitimate dispute resolution versus extracting advantage for elites?',
    'Historical analysis of feud resolution outcomes, property transfers, and status changes; comparison of honor code claims vs actual enforcement patterns in sources',
    'If coordination > 60%: classification shifts toward Rope/Scaffold from multiple perspectives. If extraction > 70%: classification shifts toward Snare/Tangled Rope. Current estimate (58% extractiveness) suggests near-equilibrium but reflects uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_code_function_vs_extraction_ratio, empirical, 'Proportion of honor code function devoted to coordination vs extraction').

omega_variable(
    female_exit_option_categorization,
    'Are women truly trapped (no material exit options) or identity_locked (structurally mobile but identity-constituted through kinship/marriage system)?',
    'Historical evidence of women''s exit paths: remarriage options after widowhood, convent entry, divorce mechanisms, property control. If genuine alternatives exist but women don''t exercise them due to identity/social shame: identity_locked. If no alternatives exist: trapped.',
    'If identity_locked: reveals that suppression mechanism is partly internalized (identity-frame prevents seeing exits). Changes measurement of suppression over lifecycle — post-patriarchy, suppression should decline to near-zero if merely internalized, persist if structural. If trapped: suppression is intrinsic to system design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_exit_option_categorization, empirical, 'Whether female exit constraints are structural or identity-constituted').

omega_variable(
    thrall_honor_code_participation,
    'Could thralls have theoretically participated in honor code if freed, or was the code constitutively dependent on slavery?',
    'Historical cases of freed thralls gaining social status; linguistic/legal analysis of whether honor code explicitly excluded unfree or only practically (via power asymmetry). Examine whether Roman slave systems showed parallel constraints.',
    'If honor code theoretically open to freed: extraction is power-dependent, not system-inherent. If honor code explicitly excludes unfree: slavery is structural prerequisite, tangled rope becomes snare from thrall perspective. Current analysis assumes the latter; evidence otherwise would upgrade thrall to constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thrall_honor_code_participation, empirical, 'Whether honor code was constitutively dependent on slavery or contingently implemented via power').

omega_variable(
    theater_ratio_inflation_over_time,
    'Does theater_ratio increase as honor code loses enforcement capacity and shifts to performative ritual?',
    'Measurement trajectory from Early Germanic period (high enforcement, low theater) through High Medieval (declining enforcement, increasing heraldic/literary theater). Proxy: ratio of honor-system-related material goods to actual documented feud/vendetta incidents.',
    'If theater increases sharply: supports piton classification for late period. If constant: honor code maintains functional enforcement longer than current model suggests. Current estimate (0.65) reflects mixed period; precise dating would clarify lifecycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_inflation_over_time, empirical, 'Whether theater_ratio increases as honor code degrades from functional to performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(germanic_honor_code, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_germanic, germanic_honor_code, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_migration_period, germanic_honor_code, theater_ratio, 5, 0.5).
narrative_ontology:measurement(theater_high_medieval, germanic_honor_code, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_early_germanic, germanic_honor_code, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extractiveness_migration_period, germanic_honor_code, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(extractiveness_high_medieval, germanic_honor_code, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(germanic_honor_code, enforcement_mechanism).
narrative_ontology:affects_constraint(germanic_honor_code, feudal_obligation_hierarchy).
narrative_ontology:affects_constraint(germanic_honor_code, tribal_kinship_system).

% DUAL FORMULATION NOTE:
% The Germanic honor code is structurally upstream of feudal obligation hierarchies and kinship-based property systems. Its extractiveness and suppression mechanisms feed directly into these dependent constraints. However, the honor code itself can be decomposed: the elite peer-coordination function (pure Rope at early stages) and the extraction-toward-women/thralls function (Snare) are structurally distinct and could be written as separate stories with different ε values. The unified story captures the constraint's evolution from higher-coordination (early period) to higher-extraction (late period).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(germanic_honor_code, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
