% ============================================================================
% CONSTRAINT STORY: german_board_gender_quota
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_board_gender_quota, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: german_board_gender_quota
 *   human_readable: German Gender Quota for Corporate Boards (FüPoG II)
 *   domain: economic/regulatory
 *
 * SUMMARY:
 *   Germany's FüPoG II (Führungspositionen-Gesetz II), effective August 2021,
 *   mandates 40% gender representation on corporate boards of publicly listed
 *   and large companies. This constraint exhibits Tangled Rope structure at
 *   the institutional level: it combines genuine coordination benefit
 *   (solving a market failure in talent allocation and organizational
 *   effectiveness) with asymmetric extraction (imposing career costs on
 *   excluded male candidates and compliance costs on firms). The constraint's
 *   perspectival range spans from pure extraction (Snare) when viewed from
 *   excluded male candidates' position, to pure coordination (Rope) when
 *   viewed from women beneficiaries, to temporary scaffolding (Scaffold) when
 *   viewed by gender parity advocates, to degraded ritual (Piton) in the
 *   vestigial corporate governance tradition. The theater ratio (0.55)
 *   reflects ongoing invocation of merit-based selection narratives even
 *   though the selection mechanism is now mandate-driven — this performative
 *   layer is gradually declining as norms shift and quota become internalized
 *   practice.
 *
 * KEY AGENTS:
 *   - Women Board Candidates: Primary beneficiary (institutional/arbitrage) — gain board access previously blocked by informal gatekeeping; experience constraint as pure coordination benefit
 *   - Male Candidates in Management Pipeline: Primary victim (powerless/trapped) — face systematic exclusion from board advancement during quota window; constrained to German jurisdiction or international relocation
 *   - DAX/MDAX Listed Corporations: Secondary actor (organized/constrained) — bear compliance costs and board nomination disruption but benefit from diversity-driven governance effectiveness; constrained exit via regulatory/tax barriers
 *   - German Regulatory Authority (BMAS): Enforcer/beneficiary (institutional/constrained) — implements quota mandate and monitors compliance; benefits from EU alignment and demographic efficiency goals
 *   - Works Councils & Union Structures: Organized beneficiary (organized/constrained) — support quota as workforce equity mechanism; constrained by labor law framework
 *   - Corporate Governance Establishment: Vestigial actor (institutional/arbitrage) — maintains merit-based selection rhetoric while operating under mandate; low functional necessity (Piton perspective)
 *   - Gender Parity Advocacy Coalition: Organized beneficiary (organized/constrained) — views quota as temporary scaffolding with 2031 sunset review; sees suppression declining as norms internalize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_board_gender_quota, 0.38).
domain_priors:suppression_score(german_board_gender_quota, 0.48).
domain_priors:theater_ratio(german_board_gender_quota, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_board_gender_quota, extractiveness, 0.38).
narrative_ontology:constraint_metric(german_board_gender_quota, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(german_board_gender_quota, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_board_gender_quota, tangled_rope).
narrative_ontology:human_readable(german_board_gender_quota, "German Gender Quota for Corporate Boards (FüPoG II)").
narrative_ontology:topic_domain(german_board_gender_quota, "economic/regulatory").

domain_priors:requires_active_enforcement(german_board_gender_quota).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_board_gender_quota, women_in_executive_positions).
narrative_ontology:constraint_beneficiary(german_board_gender_quota, workforce_diversity_advocates).
narrative_ontology:constraint_victim(german_board_gender_quota, male_candidates_for_board_seats).
narrative_ontology:constraint_victim(german_board_gender_quota, boards_facing_compliance_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MALE CANDIDATES (SNARE) — Male candidates in high-potential management tiers face systematic exclusion from board advancement during the quota implementation window. The constraint operates through mandate-driven selection rather than merit competition. No exit option exists within the German corporate system; candidates must either accept reduced advancement prospects or relocate to non-quota jurisdictions. Trapped exit + victim status → maximum experienced extraction.
constraint_indexing:constraint_classification(german_board_gender_quota, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CORPORATIONS (TANGLED ROPE) — Firms experience both coordination benefit (diversity-driven board effectiveness, reduced legal/reputational risk, access to wider talent pool) and enforcement extraction (compliance costs, board nomination process disruption, potential reduction in candidate experience levels during transition). Constrained exit: firms can relocate headquarters but face legal, tax, and operational barriers. Active enforcement regime required by law.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN CANDIDATES (ROPE) — Primary beneficiary. Quota mechanism enables board access that was previously blocked by informal gatekeeping and network effects. The constraint solves a coordination problem: firms lacked incentive to seek women candidates; quota removes that decision burden by mandating inclusion. Women candidates experience this as pure coordination benefit with minimal coercive content — they gain access without bearing significant cost. Arbitrage exit available (international board opportunities).
constraint_indexing:constraint_classification(german_board_gender_quota, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GENDER PARITY COALITION (SCAFFOLD) — Organized actors (works councils, feminist organizations, regulatory bodies) view the quota as a temporary scaffolding structure with explicit sunset logic embedded in the law. FüPoG II contains a built-in review clause at 10 years (2031) with expectation that market mechanisms and cultural shifts will make the quota unnecessary. The coalition sees enforcement suppression declining over the interval as norms internalize and alternative selection mechanisms mature.
constraint_indexing:constraint_classification(german_board_gender_quota, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CORPORATE GOVERNANCE TRADITION (PITON) — The old selection mechanism (cooptation, informal networks, tenure-track sponsorship) persists as vestigial practice even after quota implementation. Boards maintain ceremonial commitment to 'merit' while de facto using quota mandates. Theater ratio elevated (0.55) reflects performative invocation of continuity-with-merit narratives obscuring the fact that selection mechanism has fundamentally shifted. The tradition is inert — maintained through institutional habit rather than functional necessity.
constraint_indexing:constraint_classification(german_board_gender_quota, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GERMAN STATE (TANGLED ROPE) — Regulatory authority experiences the constraint as both enforcer and beneficiary. Coordination benefit: quota mechanism advances long-term labor market efficiency and gender equity goals that align with EU directives and broader demographic shifts. Extraction dynamic: enforcement requires monitoring, penalty assessment, and reputational management. Constrained exit: Germany cannot unilaterally withdraw from EU-aligned standards without trade/diplomatic cost. Active enforcement mechanism explicit in law (fines, mandatory reporting).
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical position, one could argue that gender-balanced organizational performance is a natural property of human capital optimization — excluding half the population from leadership was always suboptimal, and the quota merely corrects a persistent market failure. Under this reading, the constraint is a law-like correction of deviation, not an imposition. However, this perspective risks naturalizing a contingent policy choice as inherent organizational logic. Engine will flag as false summit.
constraint_indexing:constraint_classification(german_board_gender_quota, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_board_gender_quota_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_board_gender_quota, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_board_gender_quota, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(german_board_gender_quota, TR),
    TR >= 0.70.

:- end_tests(german_board_gender_quota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint imposes real costs on excluded male candidates (career advancement blocked) and firms (compliance overhead, nomination process disruption, potential reduction in board experience levels during transition). However, extractiveness is not severe (not Snare-level) because: (1) alternative management roles with comparable compensation exist for excluded candidates; (2) the 40% target is not total exclusion, leaving 60% of board seats available; (3) firms gain offsetting benefits from diversity-driven governance effectiveness and reduced legal/reputational risk; (4) the enforcement mechanism has a built-in sunset clause (10-year review) with expectation of eventual internalization. Suppression (0.48): Moderate. Significant barriers to exit exist for male candidates within German corporate system and for firms within EU regulatory framework, but alternatives exist (international relocation for candidates, compliance pathways for firms). Not total suppression. Theater ratio (0.55): Moderate-high. Corporate governance discourse continues to invoke merit-based selection and continuity narratives even though the selection mechanism has fundamentally shifted to mandate-driven parity. This performative layer (theater) is gradually declining as quota selection becomes internalized and norms shift. The trajectory shows theater declining from 0.38→0.55 initially (increase in performative gap as mandate takes effect and old rhetoric collides with new practice), suggesting eventual decline as internalization completes.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is substantial and structurally rooted. Women candidates see pure coordination (Rope) — the quota removes a barrier that was previously blocking access. Male candidates see pure extraction (Snare) — they are excluded with no exit option. Corporations see mixed dynamics (Tangled Rope) — they benefit from governance diversity but face disruption costs. The advocacy coalition sees a temporary structure with a sunset (Scaffold) — enforcement suppression is expected to decline as norms internalize. The corporate governance tradition sees itself as degraded (Piton) — merit-based rhetoric persists but the actual mechanism is mandate-driven. The analytical observer risks seeing a natural law (Mountain) — gender parity as optimal organizational structure — but this perspective naturalizes a contingent policy choice. The perspectival gaps reflect genuine structural asymmetries: different agents occupy different positions relative to the extraction flow (who benefits, who bears cost, what exit options exist). No single type is correct; all six readings are legitimate observations of different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically across perspectives. Women candidates (beneficiary + arbitrage exit) derive d ≈ 0.15, experiencing negative/negligible extraction. Male candidates in management (victim + trapped exit) derive d ≈ 0.95, experiencing high extraction. Corporations (mixed beneficiary/victim + constrained exit) derive d ≈ 0.50, experiencing moderate mixed extraction. German state (enforcer + constrained exit) derives d ≈ 0.45, experiencing moderate extraction offset by coordination benefit. The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid: it solves a coordination problem (firms had no incentive to seek women board members despite efficiency gains) while creating asymmetric extraction (male candidates bear cost). Both functions are real and structurally necessary — the constraint could not achieve the coordination benefit without the extraction mechanism, which makes it Tangled Rope rather than pure Rope or pure Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Tangled Rope classification is correct and unavoidable. The constraint CANNOT be pure Rope because the coordination benefit (diversity-driven governance) necessarily requires extraction from male candidates (removing them from candidacy pool during quota implementation). Conversely, it CANNOT be pure Snare because genuine coordination benefit accrues to firms and to the broader economy (market failure correction, efficiency gain). The constraint is hybrid by structural necessity: achieving the coordination function requires the extraction mechanism. Attempting to classify it as either pure Rope or pure Snare separately would miss the integrated structure. The active enforcement mechanism (mandatory reporting, fines for non-compliance) confirms the Tangled Rope gate requirement. The beneficiaries (women candidates, firms via governance effectiveness, state via EU alignment) and victims (male candidates, firms via compliance cost) are both present and both essential to the constraint's function. The perspectival range (Snare/Rope/Tangled Rope/Scaffold/Piton/Mountain) shows that the constraint is observed differently depending on the agent's position, but all perspectives converge on acknowledging the hybrid structure: pure extraction (Snare) plus pure coordination (Rope) plus enforcement overlay (Tangled Rope). The mandatrophy is fully resolved: this is a legitimate Tangled Rope, not a mislabeled Snare pretending to be Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_quality_metric,
    'Does board composition selected under quota mandate optimize for governance effectiveness as well as demographic representation?',
    'Comparative analysis of board performance metrics (ROI, strategic decision quality, risk management) for quota-selected vs pre-quota boards; longitudinal tracking of firm outcomes post-2021',
    'If yes: quota represents efficient correction of market failure (Mountain perspective gains credibility; extraction classification overstated). If no: quota is enforced allocation with real cost to selection quality (Snare/Tangled Rope perspectives confirmed; extraction magnitude increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_quality_metric, empirical, 'Whether quota selection optimizes governance effectiveness').

omega_variable(
    alternative_pathway_sufficiency,
    'Would organic market-driven selection of women to boards have eventually reached comparable parity levels without legal mandate?',
    'Comparative analysis of gender representation trends in non-German EU firms; analysis of firms that exceeded quota voluntarily pre-2021; counterfactual modeling of market trajectory absent quota',
    'If alternative pathway sufficient: quota is unnecessary intervention (Scaffold sunset logic confirmed early; suppression level overestimated). If alternative pathway stalled: quota was essential structural correction (Snare and Tangled Rope severity confirmed; extraction is unavoidable cost of market correction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, conceptual, 'Whether market-driven selection would have achieved similar parity').

omega_variable(
    male_candidate_reallocation,
    'Do excluded male candidates permanently exit German corporate leadership or reallocate to non-board management roles with similar compensation?',
    'Longitudinal career tracking of male candidates blocked from board advancement 2021-2026; analysis of compensation, title, and authority reallocation; international relocation rates',
    'If permanent exit: extraction is severe (careers truncated). If reallocation to equivalent roles: extraction is redistributive not destructive (Tangled Rope classification supported). If international relocation: extraction is externalized (German corporations benefit at cost to global male candidate mobility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_candidate_reallocation, empirical, 'Reallocation patterns for excluded male candidates').

omega_variable(
    compliance_cost_asymmetry,
    'Are compliance costs distributed fairly across firm size, or do small/medium firms bear disproportionate enforcement burden?',
    'Cost analysis by firm size (DAX vs MDAX vs Mittelstand); analysis of penalty assessment patterns; comparison of compliance infrastructure investments',
    'If symmetric: extraction is legitimate regulatory cost (Tangled Rope classification confirmed). If asymmetric to SME: quota extracts disproportionately from smaller firms (Snare classification for SME cohort).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_asymmetry, empirical, 'Distribution of compliance costs across firm size').

omega_variable(
    internalization_horizon,
    'How long until gender-balanced board selection becomes internal organizational norm rather than mandate-driven practice?',
    'Survey of corporate culture shifts; analysis of board nomination process rhetoric; tracking of voluntary diversity commitments that exceed quota; generational turnover analysis',
    'If internalization < 5 years: Scaffold sunset logic robust; suppression declining rapidly (theater ratio will drop significantly). If internalization > 15 years: Scaffold is aspirational; enforcement suppression may remain high (Tangled Rope persists longer than expected).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_horizon, preference, 'Timeline for norm internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_board_gender_quota, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbgq_tr_t0, german_board_gender_quota, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gbgq_tr_t5, german_board_gender_quota, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gbgq_tr_t10, german_board_gender_quota, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(gbgq_be_t0, german_board_gender_quota, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gbgq_be_t5, german_board_gender_quota, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(gbgq_be_t10, german_board_gender_quota, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_board_gender_quota, enforcement_mechanism).
narrative_ontology:affects_constraint(german_board_gender_quota, eu_directive_2022_2464_board_diversity).
narrative_ontology:affects_constraint(german_board_gender_quota, german_equal_opportunity_law).

% DUAL FORMULATION NOTE:
% FüPoG II is downstream of EU regulatory harmonization (2022/2464) and reflects German implementation of broader EU gender parity directives. The national constraint has its own extractiveness profile reflecting specific German corporate structure and labor law; the EU directive has its own extractiveness reflecting cross-national regulatory imposition. Linked via network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
