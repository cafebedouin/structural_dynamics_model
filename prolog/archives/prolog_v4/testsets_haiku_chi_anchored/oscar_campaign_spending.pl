% ============================================================================
% CONSTRAINT STORY: oscar_campaign_spending
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oscar_campaign_spending, []).

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
 *   constraint_id: oscar_campaign_spending
 *   human_readable: Oscar Campaign Spending Limits
 *   domain: social/entertainment/cultural_politics
 *
 * SUMMARY:
 *   The Oscar campaign spending limit is an informal but powerful constraint
 *   on how films compete for awards. Studios and filmmakers understand that
 *   campaigns exceeding approximately $15 million in total spending (print
 *   advertising, digital marketing, screenings, guild events, critic dinners,
 *   travel, and other voter outreach) trigger backlash and diminishing
 *   returns. The constraint operates through no formal rules, only through
 *   cultural consensus and industry reputation effects. This creates a hybrid
 *   structure: the spending limit genuinely solves a coordination problem
 *   (preventing spending wars that would make awards inaccessible), but it
 *   also preserves structural advantages for major studios who can leverage
 *   their distribution networks, existing critic relationships, and in-kind
 *   support to run more efficient campaigns. Independent producers and
 *   filmmakers from underrepresented backgrounds face higher per-dollar
 *   campaign costs because they lack these institutional relationships and
 *   must spend directly on visibility. The constraint exhibits all the
 *   hallmarks of a tangled rope: it has a real coordination function
 *   (preventing arms races), active enforcement (social pressure, critic
 *   backlash, voter resentment), beneficiaries (major studios who set the
 *   standard), victims (independents and diverse storytelling), and high
 *   suppression (limited exit from awards consideration without withdrawing
 *   from consideration entirely). The theater ratio (0.65) reflects that the
 *   constraint is increasingly maintained through narrative and cultural
 *   messaging rather than institutional mechanism — the Academy has no formal
 *   spending limit despite decades of discussion, relying instead on industry
 *   understanding and reputation effects to enforce the ceiling.
 *
 * KEY AGENTS:
 *   - Major Studios: Primary beneficiary (institutional/arbitrage) — absorb campaign costs into distribution budgets, leverage existing relationships, experience the constraint as coordination mechanism
 *   - Independent Producers: Primary victim (powerless/trapped) — must spend directly and cannot access studio financing; face barrier to awards consideration without equivalent resources
 *   - Diversity Equity Coalition: Secondary victim (organized/constrained) — organized advocacy groups benefit from visibility but face suppressed campaign resources and cultural gatekeeping
 *   - Academy Governance: Institutional actor (institutional/arbitrage) — maintains informal limit through social pressure; resists formalizing rules to avoid appearing to restrict speech
 *   - Reform Movement: Organized agents (organized/constrained) — advocate for transparent spending caps and formalized limits; see pathway to sunset through institutionalization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as both coordination mechanism and asymmetric extraction mechanism operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oscar_campaign_spending, 0.38).
domain_priors:suppression_score(oscar_campaign_spending, 0.48).
domain_priors:theater_ratio(oscar_campaign_spending, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oscar_campaign_spending, extractiveness, 0.38).
narrative_ontology:constraint_metric(oscar_campaign_spending, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(oscar_campaign_spending, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oscar_campaign_spending, tangled_rope).
narrative_ontology:human_readable(oscar_campaign_spending, "Oscar Campaign Spending Limits").
narrative_ontology:topic_domain(oscar_campaign_spending, "social/entertainment/cultural_politics").

domain_priors:requires_active_enforcement(oscar_campaign_spending).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, major_studios).
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, awards_voting_establishment).
narrative_ontology:constraint_victim(oscar_campaign_spending, independent_producers).
narrative_ontology:constraint_victim(oscar_campaign_spending, diverse_storytelling_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT PRODUCER (SNARE) — Trapped by resource disparity. Campaign spending for international festivals, screenings, critic dinners, and guild events requires $500K-$2M minimum. Independent producers cannot access studio financing or amortize costs across multiple films. No exit without withdrawing from awards consideration entirely. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(oscar_campaign_spending, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIVERSITY EQUITY COALITION (TANGLED ROPE) — Organized groups (filmmakers of color, women directors, disability advocates) benefit from awards visibility and industry validation (coordination function), but face structural barriers to mobilizing equivalent campaign resources. High suppression (limited studio backing, cultural gatekeeping) combined with genuine coordination need (visibility enables future funding). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR STUDIOS (ROPE) — Experience the spending limit as pure coordination mechanism. The informal ceiling (roughly $15M total across print, digital, events, and studio support) allows studios to plan budgets predictably while ensuring winners retain legitimacy. Exceeding the ceiling triggers voter backlash and industry criticism, degrading the awards' reputational value studios rely on. d≈0.10, f(d)≈0.00, σ=1.0 → χ≈0.00. Negative extraction — studios benefit from the constraint's coordination function.
constraint_indexing:constraint_classification(oscar_campaign_spending, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMY GOVERNANCE (PITON) — The Academy has no formal spending limit, only an informal 'understanding' that campaigns exceeding $15M draw criticism. This informal limit is maintained through theater: Academy statements about 'respecting the spirit of the awards,' editorial criticism, and voter resentment operate as performative enforcement rather than structural barriers. The governance system is degraded — it relies on social pressure instead of clear rules, yet the rules cannot be formalized without appearing to restrict speech. theater_ratio=0.65 reflects this: the limit is mostly enforced through narrative and shame, not mechanism. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(oscar_campaign_spending, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM MOVEMENT / SUNSET VIEW (SCAFFOLD) — Advocates for formalized spending caps see this as a temporary coordination problem with a sunset pathway. If the Academy formalizes a hard cap ($10M baseline, adjusted for inflation) with transparent reporting, the informal suppression mechanism degrades over time. Reform groups (organizations tracking campaign spending, criticism campaigns against excessive budgets) have constrained but growing agency. d≈0.45, f(d)≈0.55, σ=1.0 → χ≈0.21. Scaffold gate satisfied if sunset implementation assumed within 5-10 years.
constraint_indexing:constraint_classification(oscar_campaign_spending, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a long-view analytical position, the spending limit is both coordination mechanism (solves the collective action problem of awards inflation) and extraction mechanism (preserves studio leverage over storytelling and narrative control). The constraint prevents spending wars that would make awards inaccessible, but also prevents independent and diverse voices from competing equally. ε=0.38 and suppression=0.48 indicate hybrid structure. d≈0.70, f(d)≈1.10, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oscar_campaign_spending_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(oscar_campaign_spending, TR),
    TR >= 0.70.

:- end_tests(oscar_campaign_spending_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The spending limit does create asymmetric outcomes — major studios run more efficient campaigns and get better ROI per dollar. But the extraction is not severe because independent films do win (frequently) and the constraint does prevent total spending wars. The value reflects that the limit partially solves the coordination problem while preserving studio advantage. Suppression (0.48): Moderate. Significant barriers include resource disparity, limited access to critic networks, lack of studio in-kind support, and structural bias in awards voter composition. But suppression is not total — independent pathways exist (festival circuits, streaming platforms, grassroots campaigns) even though they are resource-intensive. Theater ratio (0.65): Moderate-high. The enforcement mechanism is increasingly theater — Academy statements about 'respecting the spirit,' critic commentary, and voter resentment operate as performative pressure rather than formal rules. The theater has increased over the interval (years 0-30) as the spending gap has widened while the Academy has resisted formalizing rules. The constraint maintains itself through narrative ('this is what the awards community expects') rather than mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Major studios perceive pure coordination (Rope) — the spending limit allows predictable budgeting and maintains awards credibility. Independent producers perceive pure extraction (Snare) — they are locked into a system where they cannot compete on equal terms and cannot exit without giving up awards consideration. The diversity coalition perceives hybrid structure (Tangled Rope) — they benefit from the awards' reputational validation but face suppression in campaign resources and cultural gatekeeping. The Academy governance sees itself as managing informal norms (Piton) — the institution acknowledges the constraint exists but refuses to formalize it, maintaining plausible deniability about enforcement. Reform advocates see a temporary problem with a solution path (Scaffold) — formalizing spending caps would establish clear boundaries and reduce theater. The analytical observer sees the full tangled rope structure: genuine coordination function (preventing arms races) plus asymmetric extraction (preserving studio leverage).
 *
 * DIRECTIONALITY LOGIC:
 *   Major studios: Beneficiary + arbitrage → d≈0.10, f(d)≈0.00. Net beneficiary. Studios benefit from the constraint's coordination function without bearing significant costs. Independent producers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit awards consideration without forfeiting career opportunities; bear full cost of resource disparity. Diversity coalition: Victim + constrained → d≈0.65, f(d)≈0.95. Significant extraction; organized but constrained by systemic factors outside their control. Academy governance: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Slightly benefits from maintaining the status quo (avoiding rule formalization). Reform movement: Organized + constrained → d≈0.45, f(d)≈0.55. Have growing agency but constrained by Academy resistance to formalizing rules. Analytical observer: Derives d from structural position as neutral observer → d≈0.70, f(d)≈1.10. Sees asymmetric extraction layered onto coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the same structural phenomenon operates as both coordination and extraction depending on observer position. The studios' rope perspective is genuine — they experience pure coordination benefit. The independents' snare perspective is also genuine — they experience pure extraction cost. The constraint is not 'really' one or the other; it is a presheaf that looks like rope from one structural position and snare from another. The tangled rope classification for the analytical observer captures the full structure: the constraint has a real coordination function (prevents spending wars, maintains awards legitimacy) AND produces asymmetric extraction (preserves studio advantage, suppresses diverse voices). The question 'which type is correct?' is malformed. The correct answer is 'all of them, from their respective positions.' The mandatrophy is resolved by recognizing that the constraint's legitimacy depends on whether the coordination benefits (preventing arms races) outweigh the extraction harms (suppressing diverse storytelling). This is a value question, not a classification question, and is properly handled as an omega variable (whether the informal limit is truly enforceable and fair).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spending_cap_threshold_definition,
    'Where is the actual spending ceiling below which backlash is minimal? Is it $15M, $10M, or is the threshold observer-dependent (different for studios vs independents)?',
    'Historical analysis of campaigns exceeding claimed limits; correlation between spending levels and voter backlash, critic commentary volume, and industry response',
    'If threshold is objective and universal (~$15M): constraint is coordination mechanism (Rope/Scaffold views validated). If threshold varies by studio backing and filmmaking identity: constraint is extraction mechanism enforcing asymmetric suppression (Snare/Tangled Rope views validated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spending_cap_threshold_definition, empirical, 'Where the actual spending ceiling lies and whether it is uniform').

omega_variable(
    studio_subsidy_opacity,
    'How much of campaign spending is absorbed by studios and not disclosed publicly? Is the $15M ceiling measured as direct campaign spending or including in-kind studio support?',
    'Investigative accounting of campaign budgets (Academy data if disclosed, industry reporting, producer interviews); comparison of declared vs estimated total spending',
    'If studio subsidies are large and hidden: effective spending cap for independents is much lower than for studio films, confirming snare structure. If studio subsidies are transparent and limited: constraint operates more symmetrically, supporting rope or tangled rope views.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(studio_subsidy_opacity, empirical, 'Extent of studio campaign subsidy absorption').

omega_variable(
    informal_limit_enforceability,
    'Is the spending limit actually enforced through Academy action (disqualification, rules), or only through social pressure and industry reputation mechanisms?',
    'Review of Academy rules and official statements; documentation of enforcement actions (disqualifications, public criticism) tied to spending; analysis of whether exceeding the limit has structural consequences or only reputational ones',
    'If enforced through rules: constraint is coordination mechanism with clear boundaries (Rope/Scaffold). If enforced through social pressure alone: constraint is extraction disguised as cultural norm (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_limit_enforceability, empirical, 'Whether the spending limit is formally enforced or purely social').

omega_variable(
    diversity_campaign_resource_gap,
    'Do films with diverse casts and female directors receive equivalent campaign support from their studios compared to traditional prestige films?',
    'Comparative analysis of campaign budgets by film genre and director demographics; studio spending decisions across portfolio; correlation between director identity and campaign resource allocation',
    'If gap exists (diverse-backed films receive less campaign support): suppression term includes structural bias, confirming high-suppression tangled rope and snare perspectives. If no gap: constraint affects all producers symmetrically, supporting rope interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_campaign_resource_gap, empirical, 'Campaign resource disparity by director demographics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oscar_campaign_spending, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oscar_tr_t0, oscar_campaign_spending, theater_ratio, 0, 0.48).
narrative_ontology:measurement(oscar_tr_t15, oscar_campaign_spending, theater_ratio, 15, 0.58).
narrative_ontology:measurement(oscar_tr_t30, oscar_campaign_spending, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(oscar_be_t0, oscar_campaign_spending, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(oscar_be_t15, oscar_campaign_spending, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(oscar_be_t30, oscar_campaign_spending, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oscar_campaign_spending, information_standard).
narrative_ontology:affects_constraint(oscar_campaign_spending, streaming_awards_legitimacy).
narrative_ontology:affects_constraint(oscar_campaign_spending, diverse_filmmaker_industry_access).

% DUAL FORMULATION NOTE:
% The spending limit constraint decomposes into two distinct structural claims: (1) whether the limit prevents spending arms races (coordination problem, ε≈0.15), and (2) whether the limit preserves studio leverage over storytelling (extraction problem, ε≈0.55). These are typically conflated in industry discourse. The reported ε=0.38 averages the two, but the two-story decomposition would clarify whether reforms should focus on formalizing the coordination mechanism or on removing barriers to independent campaign access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(oscar_campaign_spending, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
