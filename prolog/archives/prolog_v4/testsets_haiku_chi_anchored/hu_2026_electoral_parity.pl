% ============================================================================
% CONSTRAINT STORY: hu_2026_electoral_parity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hu_2026_electoral_parity, []).

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
 *   constraint_id: hu_2026_electoral_parity
 *   human_readable: The 2026 Hungarian Mixed-Member Majoritarian 'Inertia'
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   The 2026 Hungarian electoral system embodies a structural tension between
 *   its stated coordination function (combining local constituency
 *   representation with national party mandates via mixed-member design) and
 *   its actual extraction mechanism (winner-compensation rule that
 *   systematically advantages the governing coalition and disadvantages
 *   smaller opposition parties and independents). The 199-seat parliament
 *   uses a winner-compensated model where votes from constituency victors are
 *   added back to national lists, creating an asymmetry: when
 *   incumbent-aligned candidates win districts, their votes are counted twice
 *   (constituency victory + national list contribution); when opposition or
 *   independent candidates win districts, the same votes flow to national
 *   lists controlled by the incumbent coalition, negating the opposition
 *   victory. The constraint has been in place since 2011 under Fidesz
 *   supermajority control and has intensified through successive
 *   constitutional amendments. Extractiveness has risen from 0.38 (2014,
 *   initial design phase) to 0.52 (2026, after consolidation through legal
 *   weaponization), reflecting accumulated institutional accumulation of
 *   extraction. Theater ratio has risen from 0.42 to 0.58, indicating
 *   increasing reliance on performative constitutional defense narratives
 *   ('local representation protection') to maintain the rule against growing
 *   opposition and EU pressure.
 *
 * KEY AGENTS:
 *   - Incumbent Governing Coalition (Fidesz & allies): Primary beneficiary (institutional/arbitrage) — controls supermajority, designed the rule, benefits from winner-compensation amplification
 *   - Minor Opposition Parties (Democratic Coalition, Socialist Party, Jobbik splinters): Primary victim (powerless/trapped) — 8-12% national support cannot coordinate into list placement; constituency wins are swept into incumbent lists
 *   - Independent Candidates: Secondary victim (moderate/constrained) — can run without party overhead but lose constituency victories to compensation rule
 *   - Organized Democratic Reform Coalition (United Opposition, civil society, EU pressure): Organized agent (organized/mobile) — sees the rule as extractive and contestable; has mobile options (legislative reversal if they win supermajority, EU-level diplomatic pressure)
 *   - Electoral Commission & Constitutional Court: Institutional actor (institutional/arbitrage) — administers and defends the rule through constitutional narrative maintenance; theater is high because proportionality claims conflict with actual design
 *   - EU Democratic Standards Coalition: Continental observer (organized/constrained) — applies structural pressure via directives on transparency and proportionality; constrained by enforcement limitations but creating generational sunset incentive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hu_2026_electoral_parity, 0.52).
domain_priors:suppression_score(hu_2026_electoral_parity, 0.68).
domain_priors:theater_ratio(hu_2026_electoral_parity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hu_2026_electoral_parity, extractiveness, 0.52).
narrative_ontology:constraint_metric(hu_2026_electoral_parity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hu_2026_electoral_parity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hu_2026_electoral_parity, tangled_rope).
narrative_ontology:human_readable(hu_2026_electoral_parity, "The 2026 Hungarian Mixed-Member Majoritarian 'Inertia'").
narrative_ontology:topic_domain(hu_2026_electoral_parity, "political/electoral_systems").

domain_priors:requires_active_enforcement(hu_2026_electoral_parity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hu_2026_electoral_parity, incumbent_governing_coalition).
narrative_ontology:constraint_beneficiary(hu_2026_electoral_parity, major_party_apparatus).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, smaller_opposition_parties).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, independent_candidates).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, proportional_representation_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINOR OPPOSITION PARTY (SNARE) — Trapped within the electoral system. A party with 8-12% national support cannot coordinate into list placement due to constituency fragmentation. The mixed-member model compounds this: their constituency votes are swept back to national lists controlled by major parties. No exit option exists except to merge with larger parties (exit from political autonomy). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT CANDIDATES (TANGLED ROPE) — The system offers coordination benefit: independent candidates can run in constituencies without party overhead. But extraction is severe: winning an independent constituency seat triggers winner-compensation — the vote is counted again in national lists, typically benefiting the incumbent majority that shaped the rule. Constrained because independence is difficult to maintain across 199 constituencies; organized independents face coordination problems. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT GOVERNING COALITION (ROPE) — Experiences the mixed-member system as pure coordination: the winner-compensation rule amplifies their constituency victories into national list presence, converting local wins into parliament seats without additional campaigning. They can arbitrage between district strategy and national messaging. The rule was designed by their legal architects; they experience it as enabling their preferred coordination structure. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (TANGLED ROPE) — Organized actors within both government and opposition recognize the system as a hybrid: it does coordinate parliamentary representation (solve the 'what does a mixed system do?' problem), but the specific implementation (winner-compensation favoring incumbents) extracts from challengers. Reform movements in Hungary and EU-level observers see the rule as enforceable but contested — it requires active defense (constitutional amendments, parliament supermajorities) to maintain. Some major opposition parties have mobile options (threat to withdraw cooperation, coalitional restructuring), giving them exit capacity that minor parties lack. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL COMMISSION & CONSTITUTIONAL COURT (PITON) — The institutional machinery that administers the rule experiences it as largely performative theater. The mixed-member system's stated purpose (combine local representation with proportionality) is undermined by the winner-compensation rule, which breaks proportionality. The courts defend the rule's constitutionality, but the defense is theater: the rule is sustained by institutional inertia and supermajority control, not by genuine functional necessity. If the governing coalition lost its supermajority, the rule would be vulnerable to legal challenge (asymmetric institutional capture). theater_ratio=0.58 reflects that institutional defense requires constant constitutional narrative maintenance (speeches about 'local representation') despite the proportionality dysfunction. d≈0.05, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EU DEMOCRATIC STANDARDS COALITION (SCAFFOLD) — European Commission, Parliament, and civil society actors see the Hungarian electoral rule as a temporary constraint subject to external pressure and generational turnover. The rule persists because the governing coalition has supermajority control, but EU directives on electoral transparency, campaign financing, and proportional representation create structural incentives for sunset. The constraint has an implicit sunset: either democratic reversal (opposition wins 2/3 majority and repeals it) or EU structural reform (harmonization pressure). Constrained because EU institutions can apply diplomatic pressure but lack direct enforcement; organized because coalitional action (Denmark, Slovakia, Czech Republic) creates veto threats. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.24.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — A civilizational-scope analytical view might argue that all mixed-member systems face the 'compensation vs. proportionality tradeoff' as an inherent mathematical constraint: you cannot perfectly combine single-member districts with proportional outcomes without some tension. From this view, the winner-compensation rule is a natural law of electoral geometry, not a contingent institutional choice. However, the structural data contradicts this: extractiveness=0.52, suppression=0.68 indicate deliberate asymmetry, not mathematical inevitability. Comparative analysis of German mixed-member systems (negative compensation) or New Zealand's system (proportionality-weighted) shows the rule is chosen, not forced. This perspective risks false summit: naturalizing a contestable institutional design.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hu_2026_electoral_parity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hu_2026_electoral_parity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hu_2026_electoral_parity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hu_2026_electoral_parity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hu_2026_electoral_parity, TR),
    TR >= 0.70.

:- end_tests(hu_2026_electoral_parity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The winner-compensation rule creates asymmetric amplification: incumbent-aligned wins count twice, opposition wins are redistributed. Extractiveness is not at snare levels (0.66+) because opposition coalitions in 2022 and 2026 elections demonstrated partial mitigation through vote-pooling, and because EU pressure creates a realistic sunset pathway. The value reflects the rule's structural bite: it systematically redistributes representation from opposition to government. Suppression (0.68): High. Multiple barriers enforce the rule: (1) constitutional entrenchment (requires 2/3 supermajority to change, held by beneficiaries), (2) electoral commission interpretation of compensation logic, (3) legal defense through constitutional court, (4) campaign finance advantages that flow from incumbent list control. Opposition suppression is real but not total — civil society mobilization and EU directives create countervailing pressure. Theater ratio (0.58): Moderate-high. The institutional defense of the rule relies substantially on performative constitutional narratives ('protecting local representation,' 'preventing party oligopoly') that mask the extraction mechanism. But the theater is not maximal (0.70+) because the rule's extraction function is widely understood; legal scholars, opposition parties, and EU analysts openly describe it as a compensation mechanism that advantages the incumbent. Suppression and theater are rising over the interval, indicating institutional capture consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits three distinct structural readings from different agent positions. The incumbent coalition sees a coordination mechanism (Rope) — they experience the rule as enabling their preferred electoral strategy without additional effort. Minor opposition parties see pure extraction (Snare) — they cannot organize their 8-12% support into parliamentary representation because the rule systematically redistributes their votes. Organized opposition and EU observers see a temporary hybrid (Tangled Rope / Scaffold) — they recognize both the coordination function (mixed-member design does solve a real problem: balancing district and national representation) and the extraction mechanism (winner-compensation tilts the balance), and they see realistic pathways to reversal (electoral supermajority loss, EU structural pressure, generational turnover). The analytical observer risks a false summit (Mountain) — naturalizing the rule as an inherent feature of mixed-member systems — but comparative analysis of German and New Zealand systems falsifies this: winner-compensation is a design choice, not a mathematical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent governing coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; low effective extraction from their perspective. Minor opposition parties: Victim + trapped → d≈0.92, f(d)≈1.38. Highest extraction; cannot exit without merging (surrender autonomy). Independent candidates: Victim + constrained → d≈0.80, f(d)≈1.14. High extraction; constrained exit (can run but victories are redistributed). Organized opposition/Democratic reform: Victim + mobile (due to coalition capacity and EU leverage) → d≈0.55, f(d)≈0.75. Moderate extraction; mobile because supermajority reversal and EU pressure represent credible exit pathways. Electoral Commission: Institutional + arbitrage → d≈0.05, f(d)≈-0.11. Neutral beneficiary; administers the rule. EU coalition: Organized + constrained → d≈0.42, f(d)≈0.42. Constrained by enforcement limits but mobile through diplomatic pressure; lower effective extraction because they are applying pressure, not experiencing it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through a family of six structurally distinct readings. The incumbent coalition's Rope is genuine: they are solving a real coordination problem (how to translate district and national support into proportional parliamentary presence) via their preferred mechanism. The minor party's Snare is genuine: the same rule extracts from their representation. The organized opposition's Tangled Rope is genuine: they experience both coordination benefit (the mixed-member framework works) and extraction (the winner-compensation tilt). The Scaffold perspective is genuine: EU pressure and electoral reversibility create a realistic sunset pathway. The Electoral Commission's Piton is genuine: institutional defense has become largely performative because the rule's extraction is widely understood and contested. The analytical observer's false summit is genuine: naturalizing the rule as inevitable when it is actually contingent. The mandatrophy is NOT 'which type is correct?' but rather 'what level of analysis are we at?' The ruling coalition experiences coordination; the opposition experiences extraction; EU observers experience a temporary institutional arrangement subject to external pressure. All six readings are structurally valid from their respective vantage points. The classification system's job is to disambiguate these readings, not to collapse them into a single 'true' type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winner_compensation_necessity,
    'Is winner-compensation mathematically necessary for mixed-member coherence, or is it a contingent design choice that could be replaced by negative compensation or proportionality weighting?',
    'Comparative analysis: examine German (negative compensation), New Zealand (proportionality-weighted), and Japanese (parallel) mixed systems. If alternative designs are mathematically equivalent and empirically stable, then winner-compensation is not necessary — it is an extraction mechanism disguised as coordination.',
    'If necessary: mountain classification gains credibility; the rule is a structural limit. If contingent: snare and tangled-rope classifications are confirmed; the rule is designed extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(winner_compensation_necessity, empirical, 'Whether winner-compensation is a mathematical necessity or a design choice').

omega_variable(
    supermajority_indefinite_control,
    'Will the Fidesz-led governing coalition retain supermajority control indefinitely, or is a parliamentary reversal plausible within a 10-20 year horizon?',
    'Historical trend analysis: voter preference drift, generational turnover, coalition stability. Comparison to other post-communist democracies that reversed supermajority control (Poland 2015, Czech Republic 2021).',
    'If supermajority is indefinite: the constraint persists as a structural feature (snare for minors, tangled-rope for organized opposition). If reversal is plausible: the constraint is temporary (scaffold classification becomes dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supermajority_indefinite_control, empirical, 'Duration of governing supermajority control and reversal probability').

omega_variable(
    eu_structural_incentive_force,
    'Do EU directives on electoral transparency and proportionality represent genuine structural pressure that will force Hungarian reform, or are they performative diplomatic pressure without enforcement teeth?',
    'Track EU sanctions, budget conditionality, and legal proceedings against Hungary. Observe whether other post-communist democracies respond to similar pressure. Identify the cost to Hungary of non-compliance (EU fund access, diplomatic standing) vs. the cost of rule reversal (supermajority loss of advantage).',
    'If structural pressure is real: EU-level scaffold perspective becomes dominant; sunset is plausible within 10 years. If performative: EU pressure is theater; the constraint persists indefinitely as snare/tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_structural_incentive_force, empirical, 'Effectiveness and enforcement credibility of EU democratic standards pressure').

omega_variable(
    minor_party_coalition_capacity,
    'Can minor parties and independents organize into coordinated coalitions (pre-electoral blocs, vote-pooling agreements) that overcome the winner-compensation disadvantage?',
    'Historical analysis: did opposition coalitions in 2022, 2026, 2030 elections successfully mitigate the rule''s extraction? Measure whether opposition vote-pooling (e.g., Fidesz vs. United Opposition) changed the rule''s effective extractiveness.',
    'If coalition capacity is high: minor parties and organized opposition have a constrained exit option; classification shifts from snare/tangled-rope to scaffold or lighter rope. If coalition capacity is low: the rule remains snare for fragmented opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minor_party_coalition_capacity, empirical, 'Minor party coalitional organizing capacity under the electoral rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hu_2026_electoral_parity, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hu_2026_tr_t0, hu_2026_electoral_parity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hu_2026_tr_t5, hu_2026_electoral_parity, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hu_2026_tr_t10, hu_2026_electoral_parity, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hu_2026_be_t0, hu_2026_electoral_parity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hu_2026_be_t5, hu_2026_electoral_parity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(hu_2026_be_t10, hu_2026_electoral_parity, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hu_2026_electoral_parity, enforcement_mechanism).
narrative_ontology:affects_constraint(hu_2026_electoral_parity, hungarian_constitutional_court_capture).
narrative_ontology:affects_constraint(hu_2026_electoral_parity, post_communist_party_system_fragmentation).

% DUAL FORMULATION NOTE:
% The Hungarian electoral rule is downstream of the broader Hungarian constitutional capture mechanism (2011 supermajority) and upstream of party system fragmentation (2022-2026 opposition coalitional responses). It represents a specific implementation of constitutional control within the post-communist institutional context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hu_2026_electoral_parity, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
