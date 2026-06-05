% ============================================================================
% CONSTRAINT STORY: political_dissident_containment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_dissident_containment, []).

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
 *   constraint_id: political_dissident_containment
 *   human_readable: The state carceral system for neutralizing political opposition
 *   domain: political/authoritarian_control
 *
 * SUMMARY:
 *   The political dissident containment system represents a state apparatus
 *   for neutralizing organized opposition through legal instruments and
 *   carceral enforcement. The system uses formal legal procedures (criminal
 *   charges, trials, appeals) to provide a theatrical veneer of rule of law
 *   while functionally serving regime security interests. Dissidents are
 *   detained on charges of extremism, treason, or breach of political
 *   restrictions that are retroactively applied to opposition activity.
 *   Incarceration in remote facilities isolates detainees from family,
 *   counsel, and public observation. Deaths in custody remain uninvestigated.
 *   The constraint exhibits high extractiveness (ε=0.78) because it serves
 *   clear regime interests: neutralizing political opposition, consolidating
 *   ruling party monopoly, and concentrating power in the security apparatus.
 *   High suppression (0.88) reflects that dissidents lack meaningful exit
 *   options — they cannot publicly advocate, cannot emigrate safely, face
 *   legal liability, family harassment, and death risk. Theater ratio is high
 *   (0.82) because the system maintains extensive formal legal machinery
 *   (courts, trials, appeals, legal codes) that performs rule of law while
 *   delivering predetermined outcomes. The case of Alexei Navalny —
 *   imprisoned on contested charges, transferred to remote penal colony, died
 *   under unclear circumstances — exemplifies the system's structure: formal
 *   legality masking political extraction.
 *
 * KEY AGENTS:
 *   - Political Dissidents: Primary victims (powerless/trapped) — face imprisonment, torture, death with no meaningful legal recourse or exit option
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains expanded powers, budgets, institutional autonomy, and regime-security function through dissident containment
 *   - Ruling Party Monopoly: Secondary beneficiary (institutional/arbitrage) — consolidates power by eliminating organized opposition, maintains electoral theater without genuine competition
 *   - Formal Judicial System: Degraded institutional actor (institutional/constrained) — maintains appearance of independent law while functionally serving regime interests (piton perspective)
 *   - Broader Opposition Movement: Victim (moderate/trapped) — faces systematic pressure on leadership, family members, supporters; cannot exit without abandoning political voice
 *   - International Human Rights Community: Organized witness (organized/constrained) — can document violations but cannot enforce accountability; moral authority without enforcement capacity (snare perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_dissident_containment, 0.78).
domain_priors:suppression_score(political_dissident_containment, 0.88).
domain_priors:theater_ratio(political_dissident_containment, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_dissident_containment, extractiveness, 0.78).
narrative_ontology:constraint_metric(political_dissident_containment, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(political_dissident_containment, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_dissident_containment, snare).
narrative_ontology:human_readable(political_dissident_containment, "The state carceral system for neutralizing political opposition").
narrative_ontology:topic_domain(political_dissident_containment, "political/authoritarian_control").

domain_priors:requires_active_enforcement(political_dissident_containment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_dissident_containment, state_security_apparatus).
narrative_ontology:constraint_beneficiary(political_dissident_containment, ruling_party_monopoly).
narrative_ontology:constraint_victim(political_dissident_containment, political_dissidents).
narrative_ontology:constraint_victim(political_dissident_containment, civil_society_autonomy).
narrative_ontology:constraint_victim(political_dissident_containment, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE POLITICAL DISSIDENT (SNARE) — No meaningful exit options. Criminal charges are retroactively applied to opposition activity. Trial procedures lack genuine independence. Incarceration in remote facilities isolates from family, legal counsel, and public scrutiny. Death in custody remains uninvestigated. d≈0.98, f(d)≈1.48, σ=1.0 → χ≈1.15.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BROADER OPPOSITION MOVEMENT (SNARE) — Faces systematic pressure: imprisonment of leaders, asset seizure, harassment of family members, legal liability for association. Exit options severely constrained by regime's extraterritorial enforcement (poisoning attempts, intimidation abroad). Movement cannot dissolve without abandoning political voice. d≈0.92, f(d)≈1.42, σ=1.0 → χ≈1.11.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE SECURITY APPARATUS (ROPE) — Experiences the constraint as coordination mechanism for maintaining regime stability. Security apparatus benefits from expanded powers, budgets, and institutional autonomy. Exit options are arbitrage: security officials can leave state service, negotiate transfers, or defect. The carceral system solves the collective action problem of regime preservation. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(political_dissident_containment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE FORMAL JUDICIAL SYSTEM (PITON) — Maintains appearance of rule of law: courts exist, trials occur, convictions follow legal procedure. But the functional independence has atrophied — judges are appointed by regime, verdicts are predetermined, appeal processes are performative. Theater ratio = 0.82 (courts, lawyers, trials, appeals are performed; substantive judgment is absent). χ≈0.19, f(d)≈0.35, σ=1.0. The judicial system is degraded apparatus maintained through institutional inertia.
constraint_indexing:constraint_classification(political_dissident_containment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS COMMUNITY (SNARE) — Organized but trapped. Can document violations but cannot prevent them without political/military intervention. Moral authority exists but enforcement capacity does not. Regime's carceral system extracts from the international order's credibility (every unresponded violation weakens global human rights norms). d≈0.85, f(d)≈1.25, σ=1.2 → χ≈1.17.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL ORDER (PITON) — Maintains formal structures (International Criminal Court, Convention Against Torture, UN Human Rights Council) that ritually condemn state repression while remaining functionally unable to enforce. Theater ratio = 0.82 (resolutions, investigations, statements performed; enforcement absent). The international system's degraded capacity to prevent state violence against dissidents persists through institutional inertia — alternatives (humanitarian intervention, international prosecution) exist but face coordination barriers. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.91.
constraint_indexing:constraint_classification(political_dissident_containment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_dissident_containment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_dissident_containment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_dissident_containment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_dissident_containment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_dissident_containment, TR),
    TR >= 0.70.

:- end_tests(political_dissident_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): The system produces clear extraction. Regime security apparatus gains power and budgets. Ruling party eliminates electoral opposition. Dissidents lose freedom, property, and life. The extraction is not maximal (0.78 not 0.95) because the regime's objectives are partly security-maintenance (coordination function) rather than pure rent extraction — the regime genuinely views opposition containment as necessary for survival, not merely profitable. However, the system exhibits predatory characteristics (seeking wealth through asset seizure, exploiting legal gray zones) that justify the high extractiveness value. Suppression (0.88): Extreme. Dissidents have no genuine exit options — they cannot safely protest, emigrate, or appeal through independent courts. Family members face harassment. Regime uses extraterritorial enforcement (poisoning, intimidation abroad) to prevent exit. The suppression has increased over the measurement interval as regime consolidated power and became more willing to use violence. Theater ratio (0.82): High and increasing. The system maintains extensive formal judicial machinery — criminal codes, trials, appeals procedures, legal representation — that performs rule of law. Judges exist, trials occur, verdicts are delivered in formal proceedings. But the functional independence has atrophied: judges are appointed by regime, verdicts follow security directions, appeals are performative. The theater has increased as regime became more confident in its control and less concerned with maintaining plausibility.
 *
 * PERSPECTIVAL GAP:
 *   The dissident and regime security apparatus have fundamentally opposed perspectives. The dissident sees a Snare (pure extraction with no escape), while the security apparatus sees a Rope (solving the collective action problem of regime preservation). This gap reveals the fundamental asymmetry: what appears to the dissident as malicious extraction appears to the security apparatus as necessary coordination. The international human rights community sees a Snare from a different angle — they see extraction from the global human rights order's credibility (every unresponded violation weakens international norms), but lack enforcement capacity. The formal judicial system sees its own degradation (Piton) — the courts know they are performing rule of law rather than delivering it. The opposition movement sees the system as a Snare that selectively targets leadership, creating strategic dilemmas: resist and face imprisonment, capitulate and lose movement identity.
 *
 * DIRECTIONALITY LOGIC:
 *   Political Dissident: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction from the dissident's perspective. Cannot exit, bears full cost. State Security Apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Can exit state service (defect), gain resources/power from dissident containment. Ruling Party Monopoly: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Benefits from opposition neutralization, can reform or adapt policy without electoral constraint. Broader Opposition Movement: Victim + trapped → d≈0.92, f(d)≈1.42. High extraction. Movement leadership faces imprisonment; supporters face harassment; movement cannot dissolve without abandoning political purpose. Formal Judicial System: Degraded institutional actor, constrained exit → d≈0.60, f(d)≈0.75. Judicial system bears cost of complying with regime directives (institutional reputation damage) but cannot exit without being replaced. International Human Rights Community: Organized but functionally trapped → d≈0.85, f(d)≈1.25. Can advocate but cannot enforce; extracted from by regime's non-compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint is classified as Snare (pure extraction) despite having a coordination function (regime stability maintenance). The mandatrophy is resolved by distinguishing between the regime's internal rationale (coordination problem: regime survival requires opposition neutralization) and the structural function (extraction: dissident freedom, opposition autonomy, and judicial independence are extracted by the state security apparatus). The regime experiences the constraint as Rope (coordination mechanism for regime survival). The dissident experiences it as Snare (pure extraction with no coordination benefit). The mandatrophy asks: which is it? The answer is both — the constraint is extractive in its effects (dissidents bear cost) but coordination-motivated in its origins (regime sees it as necessary). However, the classification hierarchy prioritizes observable structural effects over internal motivation. Since ε=0.78, suppression=0.88, and the constraint systematically benefits security apparatus at cost to dissidents, the classification is Snare. The regime's internal experience of coordination does not change the structural facts: the system produces asymmetric extraction. The resolution is perspectival: from the regime's viewpoint, Rope; from the dissident's viewpoint, Snare; from the analytical view, Snare (because extraction dominates coordination logic).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_intent_vs_systemic_outcome,
    'Is the dissident containment system an intentional apparatus for neutralizing opposition, or a systemic outcome of combined authoritarianism, corruption, and institutional dysfunction?',
    'Documentary evidence (internal regime communications, security doctrine statements, official pronouncements); comparison with other authoritarian systems'' explicit vs implicit repression; analysis of whether contradictions between formal law and application follow predictable patterns or appear random',
    'If intentional apparatus: classification remains Snare (ε≈0.78) with clear extraction logic. If systemic byproduct: ε drops to 0.55-0.60 and classification shifts toward Tangled Rope (regime derives coordination benefits from order-maintenance that produces dissident extraction as side effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intent_vs_systemic_outcome, empirical, 'Whether dissident containment is intentional extraction or systemic byproduct').

omega_variable(
    extraction_sustainability_threshold,
    'At what level of dissident mortality or international response does the extraction mechanism cease functioning (regime loses legitimacy or faces military intervention)?',
    'Historical comparison: regimes that maintained carceral repression (USSR, East Germany, North Korea) vs those that collapsed from it (South Korea, Philippines, Brazil); analysis of international threshold responses; assessment of domestic elite coordination thresholds',
    'If sustainability horizon < 10 years: classification remains Snare but with temporal collapse omega (regime cannot sustain extraction). If > 30 years: Snare classification hardened (multi-generational extraction apparatus). Impacts mandatrophy resolution confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_sustainability_threshold, empirical, 'Duration threshold for carceral extraction apparatus sustainability').

omega_variable(
    alternative_opposition_containment,
    'Could the regime maintain monopoly power without carceral dissident containment, using instead cooptation, exile tolerance, or electoral theater?',
    'Comparative analysis of authoritarian regimes: those using carceral suppression vs those using softer control mechanisms; assessment of regime''s internal calculations regarding cost-benefit of imprisonment vs alternatives; counterfactual analysis of whether opposition could be tolerated as electoral minority without threatening regime',
    'If alternatives exist and are feasible: suppression and extractiveness values may be overestimated (regime chooses harsh method, not forced to). If carceral containment is genuinely necessary for regime stability: classification remains Snare with high suppression. Determines whether regime is rational extractor or fear-driven system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_opposition_containment, conceptual, 'Whether carceral containment is necessary or chosen method of control').

omega_variable(
    dissident_martyrdom_feedback_loop,
    'Does carceral containment increase dissident movement''s moral authority and international support, thereby self-undermining the extraction mechanism?',
    'Analysis of opposition movement''s fundraising, international support, and recruitment before and after high-profile imprisonment; comparison of movement trajectories in regimes with vs without martyr-producing carceral systems; assessment of whether imprisonment strengthens or weakens regime''s long-term stability',
    'If martyrdom increases opposition strength faster than carceral system can suppress: effective suppression < formal suppression, ε should be revised downward to 0.65-0.70. If regime can sustain extraction despite martyrdom effect: high suppression (0.88) is justified. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissident_martyrdom_feedback_loop, empirical, 'Whether carceral containment strengthens opposition movement via martyrdom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_dissident_containment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poldisc_tr_t0, political_dissident_containment, theater_ratio, 0, 0.62).
narrative_ontology:measurement(poldisc_tr_t10, political_dissident_containment, theater_ratio, 10, 0.75).
narrative_ontology:measurement(poldisc_tr_t20, political_dissident_containment, theater_ratio, 20, 0.82).

% Extraction over time
narrative_ontology:measurement(poldisc_be_t0, political_dissident_containment, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(poldisc_be_t10, political_dissident_containment, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(poldisc_be_t20, political_dissident_containment, base_extractiveness, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_dissident_containment, enforcement_mechanism).
narrative_ontology:affects_constraint(political_dissident_containment, authoritarian_electoral_theater).
narrative_ontology:affects_constraint(political_dissident_containment, state_security_apparatus_autonomy).
narrative_ontology:affects_constraint(political_dissident_containment, civil_society_suppression).

% DUAL FORMULATION NOTE:
% Political dissident containment is downstream of the regime's consolidation of power and upstream of broader civil society suppression. The constraint represents a specific instantiation of how authoritarian regimes maintain monopoly power through selective carceral enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_dissident_containment, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
