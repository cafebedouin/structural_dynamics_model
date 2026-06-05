% ============================================================================
% CONSTRAINT STORY: portugal_government_stability_ad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portugal_government_stability_ad, []).

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
 *   constraint_id: portugal_government_stability_ad
 *   human_readable: The AD Minority Government Stability (The "Presidential" Scaffold)
 *   domain: political/parliamentary_governance
 *
 * SUMMARY:
 *   The Aliança Democrática (AD) minority government in Portugal (formed
 *   January 2024 following the October 2023 elections) represents a classic
 *   parliamentary scaffold: a temporary governance arrangement with a
 *   structural exit mechanism (presidential dissolution and new elections).
 *   The AD coalition (CDS-PP, AD proper, and Liberal Initiative) controls 79
 *   seats in a 230-seat parliament, requiring external support from either
 *   the center-left Socialist Party (PS) or the centrist Democratic Party or
 *   other independents to pass legislation. This creates a fundamentally
 *   unstable equilibrium where the government's survival depends on
 *   opposition abstention or selective support rather than formal coalition
 *   membership. The constraint exhibits tension between the government's need
 *   to govern (coordination function) and the opposition's incentive to
 *   demonstrate government incompetence (extraction via legislative
 *   obstruction). The presidency provides the ultimate escape valve: if the
 *   minority arrangement becomes unmanageable, the president can dissolve
 *   parliament and reset the game board. This makes the minority government a
 *   temporary structure with a predetermined sunset condition.
 *
 * KEY AGENTS:
 *   - Aliança Democrática Coalition (AD/CDS-PP/Liberal Initiative): Primary beneficiary (institutional/arbitrage) — captures ministerial positions, agenda-setting power, and legislative priority; benefits from parliamentary leadership despite minority status
 *   - Leftist Opposition (Socialist Party, Bloco de Esquerda, Livre, Communists): Primary victim (powerless/trapped) — excluded from government and coalition despite electoral strength; unable to overturn minority without major defections
 *   - Independent Deputies and Regional Parties (Chega, regional independence parties): Secondary actors (moderate/constrained) — hold leverage through swing votes; can extract policy concessions or face retaliation through legislative defeats
 *   - Portuguese Presidency: Structural stabilizer (organized/constrained) — holds dissolution power; serves as sunset mechanism that converts indefinite minority government into time-bounded scaffold
 *   - Parliamentary Opposition Parties (PS particularly): Forced coalition members (moderate/constrained) — may provide selective support to prevent worse outcomes while preserving ability to challenge government
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing electoral arithmetic as immutable law rather than contingent arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portugal_government_stability_ad, 0.28).
domain_priors:suppression_score(portugal_government_stability_ad, 0.52).
domain_priors:theater_ratio(portugal_government_stability_ad, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portugal_government_stability_ad, extractiveness, 0.28).
narrative_ontology:constraint_metric(portugal_government_stability_ad, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(portugal_government_stability_ad, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portugal_government_stability_ad, scaffold).
narrative_ontology:human_readable(portugal_government_stability_ad, "The AD Minority Government Stability (The \"Presidential\" Scaffold)").
narrative_ontology:topic_domain(portugal_government_stability_ad, "political/parliamentary_governance").

domain_priors:requires_active_enforcement(portugal_government_stability_ad).
narrative_ontology:has_sunset_clause(portugal_government_stability_ad).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portugal_government_stability_ad, alianca_democratica).
narrative_ontology:constraint_beneficiary(portugal_government_stability_ad, center_right_coalition).
narrative_ontology:constraint_victim(portugal_government_stability_ad, leftist_parliamentary_opposition).
narrative_ontology:constraint_victim(portugal_government_stability_ad, legislative_agenda_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEFTIST OPPOSITION (SNARE) — Cannot exit the parliamentary arithmetic; constrained to reactive opposition. The PS, Bloco, and Livre have no structural path to overturn AD minority government without coalition defection. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(portugal_government_stability_ad, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENTS / REGIONAL PARTIES (TANGLED ROPE) — Constrained by coalition requirements but also benefit from agenda-setting leverage. These actors (including regional independence parties) can influence legislation but face retaliation risk if they withdraw support. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(portugal_government_stability_ad, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AD COALITION LEADERSHIP (ROPE) — Experiences constraint as coordination problem: minority government requires constant negotiation and coalition discipline. Benefits from first-mover legislative agenda and ministerial appointments. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.03. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(portugal_government_stability_ad, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRESIDENTIAL STABILIZATION (SCAFFOLD) — The Portuguese presidency (Marcelo Rebelo de Sousa or successor) acts as a sunset provider: presidential intervention to dissolve parliament and call early elections is the structural escape valve. The government's stability is explicitly temporary — the presidency can reset the board if the minority becomes ungovernable. This is scaffolding with active enforcement and a clear sunset clause: the minority arrangement is intended as a 2-4 year interregnum, not a permanent state. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.13.
constraint_indexing:constraint_classification(portugal_government_stability_ad, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE PROCEDURE (PITON) — Parliamentary procedure in minority government becomes substantially performative: extensive debate, committee hearings, and negotiation sessions occur knowing outcomes are predetermined by coalition arithmetic. Much legislative activity is theater signaling that the minority government respects parliamentary process while maintaining executive power. theater_ratio=0.65 reflects this degradation. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(portugal_government_stability_ad, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, parliamentary arithmetic is an immutable constraint: if no coalition commands a majority, minority government is a structural necessity. The bottleneck appears as a natural law of parliamentary mathematics. However, base properties (ε=0.28, suppression=0.52, theater=0.65) contradict mountain gates — this is a false summit. The constraint is contingent on electoral results and coalition strategies, not immutable.
constraint_indexing:constraint_classification(portugal_government_stability_ad, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_government_stability_ad_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portugal_government_stability_ad, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_government_stability_ad, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(portugal_government_stability_ad, TR),
    TR >= 0.70.

:- end_tests(portugal_government_stability_ad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The AD minority government does extract legislative agenda priority and ministerial control from the 79-seat position, but the extraction is constrained by the requirement for opposition support on contested votes. Unlike a snare, the minority government cannot ignore opposition or repress alternative agendas — it must negotiate. The value reflects that extraction exists (first-mover advantage, agenda control) but is substantially limited by parliamentary arithmetic. Suppression (0.52): Moderate. The minority status itself acts as suppression: opposition parties have limited capacity to force government agenda changes without toppling the government (risking new elections where outcomes are uncertain). But suppression is not extreme — opposition can obstruct legislation, force compromises, and use parliamentary procedure to slow government. Theater ratio (0.65): Moderate-high. Parliamentary debate and procedure become partly performative under minority government: much legislative discussion occurs knowing outcomes are predetermined by coalition arithmetic or prior negotiation. However, genuine negotiation and floor dynamics remain possible (unlike pure Piton with theater >0.85). The value reflects that theater has risen from baseline as the minority arithmetic has become visible.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is stark: the AD leadership sees coordination (Rope) — they are solving the legitimate problem of governing without a majority. The leftist opposition sees snare-like entrapment (Snare) — they are excluded despite electoral strength with no structural path to overturn the arrangement. The presidency sees temporary stabilization with a sunset (Scaffold) — the minority is explicitly intended as a 2-4 year interregnum before new elections reset the board. Parliamentary procedure sees its own degradation (Piton) — extensive debate happens knowing outcomes are determined by coalition math. Independent deputies see mixed extraction and leverage (Tangled Rope) — they can extract policy concessions but face retaliation if they defect. The civilizational observer risks seeing parliamentary arithmetic as immutable (Mountain) — but the structural data reveals this as contingent on electoral results, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   AD Coalition Leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction. Leftist Opposition: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction; no structural exit. Independent Deputies: Victim/Beneficiary hybrid + constrained → d≈0.68, f(d)≈1.05. Can extract concessions but also constrained by coalition dynamics. Presidential Mechanism: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction; coalition has structural exit (dissolution). Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification rejected by empirical data.
 *
 * MANDATROPHY ANALYSIS:
 *   SCAFFOLD DIAGNOSIS: The mandatrophy is resolved through the presidential sunset mechanism. This constraint avoids conflation of temporary minority government (which requires coordination and has an exit valve) with permanent extraction (which would require sustained suppression and lack of alternatives). The scaffold is genuine because: (1) beneficiaries (AD coalition) experience coordination pressure, not unconstrained extraction; (2) has_sunset_clause=true and the presidential dissolution power is a real structural escape valve, not merely rhetorical; (3) theater_ratio=0.65 indicates degradation but not complete inertia; (4) extractiveness=0.28 is moderate, not the 0.66+ that would indicate snare. The constraint would degrade into snare if the presidential dissolution power were eliminated or if the opposition's structural trap became permanent. The constraint would upgrade to rope if minority status were irrelevant to stability (i.e., if the AD coalition had sufficient voluntary support). The current classification (scaffold) reflects the genuine time-bounded nature and the real exit mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_intervention_threshold,
    'What threshold of government dysfunction triggers presidential dissolution and early elections?',
    'Historical precedent analysis of Portuguese presidential dissolutions; interview data from presidential advisors on decision criteria; comparison with other parliamentary democracies (Belgium, Netherlands)',
    'If threshold is very high (government near-collapse): scaffold functions as extended minority government (shifts toward piton). If threshold is low (routine dysfunction): scaffold becomes rapid turnover (shifts toward rope/coordination problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_intervention_threshold, empirical, 'Presidential dissolution threshold for minority government dysfunction').

omega_variable(
    coalition_defection_dynamics,
    'Do coalition partners defect in response to extraction (agenda exclusion, legislative defeats), or is coalition discipline stable despite minority status?',
    'Voting record analysis of coalition partners on contested legislation; timeline of coalition demands and budget negotiations; comparison of defection rates under minority vs majority conditions',
    'If defection is frequent: scaffold fails rapidly (shifts to snare/chaos). If defection is rare: minority arrangement is structurally stable (scaffold is genuine, not contingent on luck).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_defection_dynamics, empirical, 'Whether coalition partners remain disciplined under minority government conditions').

omega_variable(
    opposition_strategic_calculus,
    'Do opposition parties use minority government dysfunction to advance their own electoral prospects, or do they cooperate with government on essential legislation?',
    'Analysis of opposition voting patterns on EU compliance, budget, and essential legislation; opinion polling on opposition confidence in post-election viability; interview data on strategic intent',
    'If opposition cooperates: scaffolding is genuine (broader stabilization). If opposition weaponizes dysfunction: scaffold is unstable (extraction mechanism dominates; shifts toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_strategic_calculus, empirical, 'Whether opposition parties cooperate or defect under minority government').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_government_stability_ad, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ad_stab_tr_t0, portugal_government_stability_ad, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ad_stab_tr_t2, portugal_government_stability_ad, theater_ratio, 2, 0.55).
narrative_ontology:measurement(ad_stab_tr_t4, portugal_government_stability_ad, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ad_stab_be_t0, portugal_government_stability_ad, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ad_stab_be_t2, portugal_government_stability_ad, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(ad_stab_be_t4, portugal_government_stability_ad, base_extractiveness, 4, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portugal_government_stability_ad, enforcement_mechanism).
narrative_ontology:affects_constraint(portugal_government_stability_ad, portuguese_electoral_system_proportionality).
narrative_ontology:affects_constraint(portugal_government_stability_ad, eu_fiscal_compliance_framework_pt).

% DUAL FORMULATION NOTE:
% The AD minority government stability is downstream of the Portuguese electoral system (which produced no clear majority in 2023) and the institutional structure of the presidency (which retained dissolution power). These upstream constraints have their own ε values; the minority government scaffold has ε=0.28 reflecting the specific coalition dynamics and parliamentary arithmetic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
