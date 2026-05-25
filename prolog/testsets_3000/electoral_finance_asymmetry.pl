% ============================================================================
% CONSTRAINT STORY: electoral_finance_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electoral_finance_asymmetry, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electoral_finance_asymmetry
 *   human_readable: Electoral Finance Asymmetry and Campaign Extraction
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Electoral finance asymmetry creates a structural constraint on democratic
 *   competition: wealth and access to donor networks determine electoral
 *   viability in ways that correlate weakly with constituent preference or
 *   policy competence. The constraint generates six distinct classifications
 *   depending on observer position, each capturing real structural facts
 *   about how campaign finance asymmetry operates. Nonwealthy candidates
 *   experience it as a snare (trapped exclusion), grassroots movements
 *   experience it as tangled rope (coordination + extraction), incumbents
 *   experience it as rope (pure coordination), reform movements experience it
 *   as scaffold (temporary with sunset), enforcement systems as piton
 *   (degraded ritual), and civilizational analysis risks naturalizing it as
 *   mountain (immutable law). The constraint's theater ratio (0.55) reflects
 *   that campaign finance disclosure and regulation are moderately
 *   performative: formal compliance is maintained while dark money, bundling,
 *   and post-election quid pro quo operate around regulatory edges. The
 *   extractiveness has risen from 0.35 to 0.58 over the measurement interval
 *   as campaign costs have increased and small-donor scalability remains
 *   unproven.
 *
 * KEY AGENTS:
 *   - Nonwealthy Candidates: Primary victim (powerless/trapped) — face structural exclusion from competitive races; no exit option without abandoning political voice
 *   - Wealthy Donors and Corporate Interests: Primary beneficiaries (institutional/arbitrage) — gain asymmetric political influence through contribution leverage and post-election access
 *   - Incumbent Politicians: Secondary beneficiary (institutional/arbitrage) — fundraising advantage compounds reelection advantage; can arbitrage to lobbying or corporate board positions
 *   - Grassroots Movements: Secondary victim (moderate/constrained) — benefit from campaign finance infrastructure for message amplification but face cost asymmetries and donor fatigue
 *   - Campaign Finance Reform Movement: Organized agents (organized/constrained) — perceive finance asymmetry as solvable through public financing, contribution limits, and disclosure; building sunset pathways
 *   - Federal Election Commission and State Finance Systems: Institutional actor (institutional/arbitrage) — maintain performative enforcement; inadequate resources and weak penalties allow extraction mechanisms to persist
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as universal law of politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electoral_finance_asymmetry, 0.58).
domain_priors:suppression_score(electoral_finance_asymmetry, 0.65).
domain_priors:theater_ratio(electoral_finance_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electoral_finance_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(electoral_finance_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(electoral_finance_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electoral_finance_asymmetry, tangled_rope).
narrative_ontology:human_readable(electoral_finance_asymmetry, "Electoral Finance Asymmetry and Campaign Extraction").
narrative_ontology:topic_domain(electoral_finance_asymmetry, "political_economy/governance").

domain_priors:requires_active_enforcement(electoral_finance_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electoral_finance_asymmetry, wealthy_donors).
narrative_ontology:constraint_beneficiary(electoral_finance_asymmetry, incumbent_politicians).
narrative_ontology:constraint_beneficiary(electoral_finance_asymmetry, corporate_interests).
narrative_ontology:constraint_victim(electoral_finance_asymmetry, nonwealthy_candidates).
narrative_ontology:constraint_victim(electoral_finance_asymmetry, electoral_competitiveness).
narrative_ontology:constraint_victim(electoral_finance_asymmetry, democratic_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NONWEALTHY CANDIDATE (SNARE) — Faces near-total exclusion from competitive races without access to donor networks or personal wealth. Cannot exit electoral system without abandoning political voice. Suppression is structural: campaign financing requirements create material barriers to candidacy. Maximum experienced extraction — the constraint extracts political opportunity from this agent with minimal coordination function.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRASSROOTS MOVEMENT (TANGLED ROPE) — Experiences both genuine coordination function (campaign finance systems enable message amplification, organization at scale) and significant extraction (cost asymmetries favor wealthy messages, small-donor fatigue). Exit is costly but possible: movements can build outside electoral channels (direct action, mutual aid, community organizing) but face barriers of legitimacy and resource allocation. Mixed experience reflects real coordination benefit alongside real asymmetric extraction.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT POLITICIAN (ROPE) — Experiences campaign finance as pure coordination: funds enable message delivery, staff recruitment, voter contact infrastructure. Incumbent advantage (fundraising incumbent vs challenger asymmetry) is viewed as normal political reality. Can arbitrage between electoral and post-electoral leverage (corporate board positions, lobbying). Net beneficiary — extraction flows toward this agent, but they perceive the mechanism as legitimate coordination of electoral competition.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAMPAIGN FINANCE REFORM MOVEMENT (SCAFFOLD) — Organized agents (Common Cause, Fix Our Senate, public financing advocates) perceive the finance asymmetry as a temporary coordination failure with a clear sunset: publicly funded elections, contribution limits, disclosure requirements, and small-donor matching systems are structured as temporary experiments that would replace the asymmetric extraction mechanism. Low experienced extraction because the movement has agency and clear exit pathways (legislative reform, constitutional amendment, state-level experiments). Constraint has built-in sunset logic: if public financing reaches critical adoption, private donor asymmetry loses force.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAMPAIGN FINANCE ENFORCEMENT SYSTEM (PITON) — Federal Election Commission and state finance disclosure systems are substantially degraded: enforcement is underfunded, penalties are nominal relative to benefit of violation, and dark money structures (501c4 organizations, super PACs, shell donation chains) render formal regulations performative. The system persists through institutional inertia and the narrative that 'money is speech' (constitutional protection) rather than because enforcement actually constrains extraction. Theater ratio reflects that regulatory compliance is largely theatrical — agents calculate violation benefit against weak penalties rather than viewing rules as binding constraints.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, campaign finance asymmetry appears as an immutable property of democratic systems: wealth always correlates with political influence, information asymmetries always advantage the organized, and coordination costs always scale with electorate size. This perspective sees the constraint as reflecting fundamental laws of political economy rather than contingent institutional arrangements. However, the structural data contradicts mountain classification — empirical variation across democracies in finance structures, effectiveness of public financing systems, and donor diversity suggests the asymmetry is contingent, not universal.
constraint_indexing:constraint_classification(electoral_finance_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electoral_finance_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electoral_finance_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electoral_finance_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electoral_finance_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electoral_finance_asymmetry, TR),
    TR >= 0.70.

:- end_tests(electoral_finance_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Campaign finance asymmetry extracts political opportunity from nonwealthy candidates and reduces electoral competitiveness, but the extraction is not total — candidates can still compete through organizational advantage, volunteer mobilization, and small-donor campaigns (though at higher cost and lower success rates). The 0.58 value reflects genuine asymmetric extraction alongside partial coordination function (campaign finance systems do enable message delivery at scale). The upward trend from 0.35 to 0.58 across the measurement interval reflects rising campaign costs, declining small-donor scalability, and increasing donor concentration. Suppression (0.65): High. Structural barriers include candidate personal fundraising requirements, minimum viable campaign budgets that scale with electorate size, donor network access concentrated among wealthy/connected agents, and implicit career gating (working capital in finance/law/consulting provides donor network access). These barriers are material and difficult to circumvent, though not insurmountable. Theater ratio (0.55): Moderate-high. Campaign finance disclosure rules, contribution limits, and FEC reporting are substantively maintained, but dark money structures (501c4 nonprofits, shell donation chains, post-election quid pro quo arrangements) operate in regulatory gaps. Enforcement is underfunded relative to violation benefit, making nominal compliance compatible with actual circumvention. Theater has increased from 0.42 to 0.55 as sophisticated workarounds have proliferated.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct constraint types emerge from the same structural data because observers occupy different positions relative to extraction flow. The nonwealthy candidate's perspective (Snare) is the maximal extraction view: trapped agent, high cost, no exit. The incumbent's perspective (Rope) is the minimal extraction view: beneficiary, low cost, pure coordination benefit. The grassroots movement (Tangled Rope) and reform movement (Scaffold) perspectives reveal that extraction mechanisms are not monolithic — some agents experience genuine coordination function alongside extraction, and some perceive viable exit pathways. The FEC perspective (Piton) reveals that formal regulatory systems can become degraded rituals. The analytical perspective risks (Mountain) naturalizes what is actually a contingent institutional arrangement. The perspectival gap shows that campaign finance asymmetry is not a single unified constraint but a family of structural inequalities operating at different timescales and through different mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extraction (chi) flows from their structural position: beneficiaries (wealthy donors, incumbents) have low d (full benefit) → low/negative chi; victims (nonwealthy candidates) have high d (full target) → high chi; mixed agents (grassroots movements) have moderate d → moderate chi. The constraint's directionality is asymmetric: it concentrates extraction toward powerless agents with trapped/constrained exit, while benefiting institutional agents with arbitrage options. Wealthy donors can exit electoral funding markets and maintain political influence through other channels (corporate boards, lobbying, think tanks); nonwealthy candidates cannot exit electoral competition without losing political voice. This asymmetry is the core structural fact driving Snare classification from the victim perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that campaign finance asymmetry genuinely exhibits coordination function (infrastructure for message amplification, voter contact, campaign organization) while simultaneously extracting political opportunity asymmetrically. The Tangled Rope classification captures both: beneficiaries (wealthy donors, incumbents) gain political influence through legitimate coordination of electoral competition, while victims (nonwealthy candidates, electoral competitiveness itself) bear extraction costs with limited coordination benefit. The mandatrophy arises from confusing 'campaign finance enables coordination' (true) with 'campaign finance asymmetry is pure coordination with no extraction' (false). The constraint coordinates electoral messaging while extracting differential political power — the coordination function does not eliminate the extraction, it hides it behind legitimate organizational necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    donor_influence_causality,
    'Does campaign finance asymmetry CAUSE political responsiveness asymmetry, or does it merely correlate with pre-existing wealth-based political interest?',
    'Natural experiments from campaign finance reform; comparison of policy responsiveness before/after contribution limits; analysis of politician behavior when facing donors vs non-donors after controlling for constituent preferences',
    'If causality high: constraint is extractive (donors extract policy responsiveness). If causality low: constraint is primarily informational (donors amplify voices they already agree with). Classification shifts from Snare to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_influence_causality, empirical, 'Whether finance asymmetry causes or merely correlates with policy responsiveness asymmetry').

omega_variable(
    small_donor_scalability,
    'Can small-donor fundraising (used successfully in some campaigns) scale to replace large-donor dependence system-wide, or does it remain a niche strategy dependent on candidate charisma and pre-existing visibility?',
    'Longitudinal analysis of small-donor campaign success rates across candidate types (charismatic vs organizational, celebrity vs local); cost-per-dollar comparison of small-donor vs large-donor fundraising at scale; replication studies of successful small-donor models across different electoral contexts',
    'If scalable: scaffold perspective is accurate — alternative pathways are structurally viable. If niche: scaffold is aspirational rather than structural, and the constraint persists as tangled rope or snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_donor_scalability, empirical, 'Whether small-donor models can scale to replace large-donor dependence').

omega_variable(
    public_financing_effectiveness,
    'Do public financing systems (Maine, Arizona, New York models) actually equalize candidate competitiveness and reduce donor influence, or do they shift extraction mechanisms rather than eliminate them (bundled grassroots organizing substitutes for bundled donations)?',
    'Comparison of candidate competitiveness metrics (win margin distributions, challenger funding ratios) before/after public financing adoption; analysis of whether non-monetary barriers to entry (name recognition, endorsement networks, volunteer mobilization) replace monetary barriers; tracking of post-campaign career leveraging by publicly-financed winners',
    'If effective: public financing removes extraction mechanism, and constraint disappears at state level. If mechanism-shifting: constraint persists with different extraction vector, suggesting deeper structural inequality not addressable through finance rules alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_financing_effectiveness, empirical, 'Whether public financing systems actually reduce donor influence or shift extraction mechanisms').

omega_variable(
    suppression_internalization,
    'Is the suppression in the nonwealthy candidate perspective primarily structural (material barriers to fundraising) or internalized (candidates believe they cannot compete and self-select out before facing actual barriers)?',
    'Analysis of candidate entry patterns: do nonwealthy candidates avoid races before or after attempting fundraising? Survey data on candidate perceptions of fundraising barriers vs actual fundraising success rates. Comparison of entry rates when fundraising barriers are reduced (public financing, contribution limits) to test whether perception or material barriers drive exclusion.',
    'If primarily structural: constraint classification remains snare with high actual cost. If primarily internalized: constraint operates through cognitive capture, and identity_locked exit option may be more accurate; classification may shift toward rope from nonwealthy candidate perspective if psychological barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression in candidate exclusion is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electoral_finance_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_fin_tr_t0, electoral_finance_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(elec_fin_tr_t20, electoral_finance_asymmetry, theater_ratio, 20, 0.48).
narrative_ontology:measurement(elec_fin_tr_t40, electoral_finance_asymmetry, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(elec_fin_be_t0, electoral_finance_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(elec_fin_be_t20, electoral_finance_asymmetry, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(elec_fin_be_t40, electoral_finance_asymmetry, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electoral_finance_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(electoral_finance_asymmetry, 0.12).
narrative_ontology:affects_constraint(electoral_finance_asymmetry, political_incumbent_advantage).
narrative_ontology:affects_constraint(electoral_finance_asymmetry, dark_money_opacity).
narrative_ontology:affects_constraint(electoral_finance_asymmetry, corporate_political_capture).

% DUAL FORMULATION NOTE:
% Electoral finance asymmetry is upstream of specific mechanisms of political influence extraction (lobbying effectiveness, corporate campaign leverage, incumbent reelection advantage). The finance asymmetry creates the structural condition enabling downstream constraints; decomposition would separate finance access (this story) from influence conversion (downstream stories with different epsilon values reflecting empirical specificity of influence mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electoral_finance_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
