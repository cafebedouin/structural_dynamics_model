% ============================================================================
% CONSTRAINT STORY: thai_senate_veto_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_senate_veto_2026, []).

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
 *   constraint_id: thai_senate_veto_2026
 *   human_readable: The Senate Constitutional Veto (Post-Transitory Residual)
 *   domain: political/constitutional_governance
 *
 * SUMMARY:
 *   The Thai Senate constitutional veto represents a post-transitory
 *   institutional residual — a power structure designed for a temporary
 *   junta-legitimizing role that persists after its original function has
 *   expired. The five-year transitory provision allowing junta-appointed
 *   senators to vote for Prime Minister ended, but Article 256's one-third
 *   veto power over constitutional amendments remains. This creates a
 *   structural asymmetry: the elected House can command 67% supermajority
 *   support for constitutional reform, but the unelected Senate retains 34%
 *   blocking power. The constraint exhibits mixed coordination and
 *   extraction: some extraction occurs (reform agenda is blocked without
 *   negotiation), but the veto also functions as a check on radical
 *   constitutional change. The core tension is between democratic legitimacy
 *   (elected House majority) and constitutional entrenchment (Senate veto).
 *   Theater has increased over the measurement interval as the Senate
 *   increasingly justifies the veto through stability rhetoric rather than
 *   substantive negotiation. The constraint is neither pure coordination
 *   (genuine negotiation mechanism) nor pure extraction (unbreakable
 *   monopoly), but a degrading hybrid where performative legitimacy masks
 *   structural bias toward the ancien régime.
 *
 * KEY AGENTS:
 *   - Junta-Legacy Senate Bloc: Primary beneficiary (institutional/arbitrage) — retains institutional veto power without electoral accountability; can enforce blocking power unilaterally
 *   - Elected House Democratic Coalition: Primary victim (powerless/trapped) — cannot exit constitutional process; 67% supermajority insufficient to overcome Senate veto; faces perpetual negotiation asymmetry
 *   - Pro-Democracy Reform Coalition: Secondary victim (moderate/constrained) — benefits from voice but blocked from implementing reform agenda; constrained by constitutional thresholds
 *   - Civil Society Pressure Bloc: Organized actors (organized/constrained) — external pressure on Senate veto to collapse through delegitimation; represents exit pathway via public pressure accumulation
 *   - Constitutional Court: Institutional interpreter (institutional/constrained) — enforces formal veto power but constrained by legitimacy questions; doctrine increasingly questioned
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the veto as a constitutional principle when it is actually a transitory residual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_senate_veto_2026, 0.52).
domain_priors:suppression_score(thai_senate_veto_2026, 0.68).
domain_priors:theater_ratio(thai_senate_veto_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_senate_veto_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(thai_senate_veto_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(thai_senate_veto_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_senate_veto_2026, tangled_rope).
narrative_ontology:human_readable(thai_senate_veto_2026, "The Senate Constitutional Veto (Post-Transitory Residual)").
narrative_ontology:topic_domain(thai_senate_veto_2026, "political/constitutional_governance").

domain_priors:requires_active_enforcement(thai_senate_veto_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, junta_institutional_legacy).
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, conservative_establishment).
narrative_ontology:constraint_victim(thai_senate_veto_2026, elected_house_reform_agenda).
narrative_ontology:constraint_victim(thai_senate_veto_2026, democratic_supermajority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTED HOUSE DEMOCRATIC COALITION (SNARE) — Cannot exit constitutional process; 67% supermajority in House insufficient to overcome 34% Senate veto. Structural target of extraction: Senate veto power blocks reform without negotiation pathway. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRO-DEMOCRACY REFORM COALITION (TANGLED ROPE) — Constrained by constitutional thresholds but benefits from institutional voice and veto over non-reform amendments. Mixed: experiences both extraction (blocked amendments) and coordination (check on revanchist amendments). d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(thai_senate_veto_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUNTA-LEGACY SENATE BLOC (ROPE) — Institutional beneficiary (arbitrage exit: can withdraw from coalitions without cost). Experiences veto power as coordination mechanism: preserves constitutional stability against radical amendments. Asymmetry favors this bloc through unilateral veto. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(thai_senate_veto_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY PRESSURE BLOC (SCAFFOLD) — Organized agents (labor unions, pro-democracy NGOs, youth movements) see the veto as a temporary institutional constraint with sunset logic: continued Senate obstruction of reform drives public pressure and increases coalition leverage for eventual constitutional renegotiation. Theater manifests as Senate legitimacy performance (framing veto as 'stability') that degrades as public support for reform grows. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.23.
constraint_indexing:constraint_classification(thai_senate_veto_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT DOCTRINE (PITON) — The veto power is constitutionally enshrined but functionally atrophied: its primary function was to preserve junta influence during the five-year transitory period. With that period expired, the veto persists through institutional inertia — the Court has ruled that Senate participation in amendment voting is not discretionary despite the loss of legitimacy. Theater_ratio=0.65 reflects that constitutional proceduralism masks extraction. The doctrine is maintained not because it serves its original function but because constitutional change requires the very majority it blocks.
constraint_indexing:constraint_classification(thai_senate_veto_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL STRUCTURE VIEW (MOUNTAIN) — From a civilizational/universal perspective, some constitutional entrenchment is inherent to governance: supermajority requirements prevent tyranny of the majority and provide stability. The veto power is structural to bicameral constitutional systems. However, structural data (ε=0.52, suppression=0.68, theater=0.65) contradicts mountain classification — the engine flags this as a false summit. The naturalizing frame ignores that the Senate's legitimacy is derivative (junta-appointed, not elected), making the entrenchment asymmetric, not neutral.
constraint_indexing:constraint_classification(thai_senate_veto_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_senate_veto_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_senate_veto_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_senate_veto_2026, TR),
    TR >= 0.70.

:- end_tests(thai_senate_veto_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Senate veto power blocks constitutional reform without a clear negotiation pathway, representing meaningful extraction. However, it is not maximal (0.70+) because some path exists through public pressure and coalition dynamics — the constraint is not airtight. The measurement trajectory (0.38→0.52) reflects that extractiveness increased as the Senate's legitimacy degraded post-transitory period, forcing it to rely more heavily on pure veto rather than negotiated constitutional change. Suppression (0.68): High. The constraint operates through constitutional entrenchment, making exit extremely difficult. The elected majority has no legal pathway to override the veto without constitutional amendment — which requires the veto-holder's cooperation. This circularity creates severe suppression: those bearing the cost (reform coalition) cannot exit or bypass the constraint. Theater ratio (0.65): Moderately high, increasing over time. The Senate increasingly frames the veto as necessary for 'constitutional stability' and 'preventing radical change,' but this is performative legitimacy rather than substantive constitutional theory. The rise from 0.42→0.65 reflects growing gap between formal justification and actual function (institutional bias preservation). The theater is not total (0.70+) because some genuine constitutional deliberation occurs, but the veto's blocking function overshadows negotiation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a striking gap between beneficiary and victim experience. The junta-legacy Senate bloc sees a coordination mechanism — the veto preserves constitutional stability against radical amendments and ensures consultative input on major constitutional changes. The elected House democratic coalition sees a snare — they are structurally unable to implement democratic mandates without Senate permission. The reform coalition sees a tangled rope — extraction (blocked reforms) mixed with coordination benefits (check on revanchist amendments). The civil society pressure bloc sees a temporary problem with a sunset mechanism — continued obstruction drives public delegitimation and creates leverage for eventual renegotiation. The Constitutional Court doctrine sees itself as preserving stability (piton perspective — the doctrine is maintained through institutional inertia despite degraded function). The analytical observer risks naturalizing the veto as a constitutional principle (mountain) but the structural data reveals it as a contingent transitory residual.
 *
 * DIRECTIONALITY LOGIC:
 *   Junta-legacy Senate bloc: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with unilateral exit option. Elected House coalition: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — cannot exit constitutional process. Reform coalition: Victim + constrained → d≈0.70, f(d)≈1.08. Significant extraction but constrained by their own institutional position (can organize but cannot unilaterally break the veto). Civil society: Organized + constrained → d≈0.45, f(d)≈0.45. Low-to-moderate effective extraction; external pressure has leverage over time. Constitutional Court: Institutional + constrained → d≈0.20, f(d)≈0.05. Doctrine maintains veto but constrained by legitimacy pressure; piton classification emerges from theater gate, not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes constraint as structural); engine's false summit detector identifies this.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT TYPE RESOLUTION: This constraint resolves the mandatrophy by disambiguating the Senate veto's function across different time horizons and institutional positions. The junta-legacy bloc genuinely experiences it as coordination (rope) — from their position of institutional stability, the veto is a mechanism for ensuring consultative input. The elected House coalition genuinely experiences it as extraction (snare) — from their position of blocked democratic mandate, the veto is a unilateral blocking mechanism. The reform coalition genuinely experiences it as tangled rope — mixed coordination (constitutional checks) and extraction (blocked amendments) from a constrained institutional position. The civil society bloc genuinely experiences it as a temporary constraint with sunset logic (scaffold) — their outside pressure creates a pathway that degrades the veto's sustainability. The mandatrophy dissolves when we recognize that the same structural fact (Senate veto under Article 256) is experienced as different constraint types depending on the agent's structural relationship to the mechanism. The false summit (analytical naturalization) is detected by the engine because the structural data (ε=0.52, suppression=0.68) contradicts the mountain claim — if the veto were a constitutional principle, suppression would be much lower (≤0.15) and extractiveness would be negligible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senate_legitimacy_collapse,
    'At what threshold of public support for reform does the Senate''s veto power become politically unsustainable, forcing constitutional renegotiation?',
    'Public opinion polling on constitutional reform support; coalition size measurements; opposition party supermajority confirmation; street protest scale metrics',
    'If threshold < 60% public support: veto becomes unenforceable within 5 years (scaffold thesis). If threshold > 80%: junta legacy can maintain veto indefinitely despite public opposition (snare thesis).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(senate_legitimacy_collapse, empirical, 'Political sustainability threshold for Senate veto under public pressure').

omega_variable(
    defection_mechanism,
    'Will elected senators defect from the junta coalition to gain political legitimacy as democracy supporters, creating a voting majority for reform without constitutional renegotiation?',
    'Tracking of senator defections; comparative advantage analysis for individual senators switching coalitions; inter-election survey data on voter preference for defecting senators',
    'If defection > 20%: veto becomes supermajority-breakable (effective end of constraint). If defection < 5%: veto remains structurally durable (snare thesis confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defection_mechanism, empirical, 'Whether individual senator defections can dissolve the junta coalition').

omega_variable(
    precedent_amendment_sequencing,
    'Can reformers pass incremental constitutional amendments that reduce the Senate veto threshold incrementally, or does the veto block all self-modifying amendments?',
    'Historical analysis of Supreme Constitutional Court rulings on amendment precedent; test cases of threshold-reduction amendments; political feasibility studies for 5/6 or 4/5 supermajority strategies',
    'If incremental reduction viable: tangled rope thesis (mixed coordination/extraction). If veto blocks all self-modification: snare thesis confirmed (extraction with no exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_amendment_sequencing, conceptual, 'Whether veto power can be incrementally reduced through constitutional amendments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_senate_veto_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_sen_tr_t0, thai_senate_veto_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(thai_sen_tr_t2, thai_senate_veto_2026, theater_ratio, 2, 0.55).
narrative_ontology:measurement(thai_sen_tr_t4, thai_senate_veto_2026, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(thai_sen_be_t0, thai_senate_veto_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(thai_sen_be_t2, thai_senate_veto_2026, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(thai_sen_be_t4, thai_senate_veto_2026, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_senate_veto_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_senate_veto_2026, thai_house_senate_electoral_asymmetry).
narrative_ontology:affects_constraint(thai_senate_veto_2026, thai_constitutional_amendment_supermajority).

% DUAL FORMULATION NOTE:
% The Senate veto is downstream of the broader Thai constitutional framework and upstream of specific amendment attempts. The electoral asymmetry (appointed vs elected) is the upstream constraint generating the veto's extractive potential; the supermajority requirement is the downstream constraint defining what amendments the veto blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_senate_veto_2026, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
