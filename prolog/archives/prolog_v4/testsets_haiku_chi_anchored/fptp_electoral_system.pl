% ============================================================================
% CONSTRAINT STORY: fptp_electoral_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fptp_electoral_system, []).

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
 *   constraint_id: fptp_electoral_system
 *   human_readable: First-Past-the-Post Electoral System
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   First-Past-the-Post (FPTP) is a winner-take-all electoral system in which
 *   the candidate with the most votes wins a district seat, regardless of
 *   achieving a majority. This constraint exhibits a hybrid
 *   coordination-extraction structure: it solves a real problem (how to
 *   aggregate diverse preferences into a representative selection mechanism)
 *   while simultaneously suppressing and extracting from certain voter
 *   classes. The system creates asymmetric incentive gradients: two major
 *   parties benefit enormously from the spoiler dynamic (third parties are
 *   structurally locked out), while voters whose preferences fall outside the
 *   binary choice face tactical voting pressure and representation loss. The
 *   constraint has intensified over time (ε increased from 0.35 to 0.52 over
 *   the interval) as political polarization has widened the suppression
 *   surface and as theater ratio has increased (candidates now claim to
 *   represent majority mandates with <40% of votes). This represents the
 *   classic lifecycle drift of a hybrid constraint: the coordination function
 *   remains constant, but the extraction rent has accumulated as parties
 *   optimize for the spoiler advantage.
 *
 * KEY AGENTS:
 *   - Marginalized Voters: Primary victims (powerless/trapped) — voters whose policy preferences diverge from the binary choice face suppression, tactical voting burden, and permanent voice loss
 *   - Third-Party Candidates: Secondary victims (powerless/trapped) — structurally unable to compete due to spoiler effect; participation becomes extraction mechanism
 *   - Swing Voters: Intermediate (moderate/constrained) — experience genuine coordination function in competitive districts but suppression in safe districts; mixed benefit and cost
 *   - Major Party Apparatus: Primary beneficiary (organized/arbitrage) — benefits from spoiler lock-in; achieves coalition aggregation and gatekeeper power with minimal competition
 *   - Incumbent Representatives: Secondary beneficiary (organized/constrained) — safe-seat incumbents benefit from districting + spoiler lock-in but lose mobility between cycles
 *   - Electoral Reform Coalition: Institutional actor (institutional/mobile) — proportional representation and ranked-choice voting advocates; see FPTP as temporary failure with clear alternative pathways
 *   - Electoral Law Institution: Inertial (institutional/constrained) — the formal statute persists through constitutional path-dependency; maintains legitimacy via 'one person one vote' rhetoric despite spoiler pathology
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design choice ('obviously the most votes wins') as inevitable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fptp_electoral_system, 0.52).
domain_priors:suppression_score(fptp_electoral_system, 0.68).
domain_priors:theater_ratio(fptp_electoral_system, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fptp_electoral_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(fptp_electoral_system, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fptp_electoral_system, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fptp_electoral_system, tangled_rope).
narrative_ontology:human_readable(fptp_electoral_system, "First-Past-the-Post Electoral System").
narrative_ontology:topic_domain(fptp_electoral_system, "political/electoral_systems").

domain_priors:requires_active_enforcement(fptp_electoral_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fptp_electoral_system, two_major_parties).
narrative_ontology:constraint_beneficiary(fptp_electoral_system, incumbent_representatives).
narrative_ontology:constraint_victim(fptp_electoral_system, third_party_candidates).
narrative_ontology:constraint_victim(fptp_electoral_system, voter_representation_fidelity).
narrative_ontology:constraint_victim(fptp_electoral_system, proportional_voice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED VOTER (SNARE) — Voters whose preferences diverge from the binary choice in their district face tactical voting pressure. No exit option. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73. The constraint suppresses honest voting and coerces strategic behavior.
constraint_indexing:constraint_classification(fptp_electoral_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD-PARTY CANDIDATES (SNARE) — Structurally unable to compete. Spoiler effect creates negative incentive gradient. No exit: participation itself becomes extraction mechanism. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74. Pure extraction via suppression of voice.
constraint_indexing:constraint_classification(fptp_electoral_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SWING VOTER (TANGLED ROPE) — Experiences genuine coordination function: their vote genuinely affects outcome in competitive districts. But suppression remains high (wasted votes in safe districts, tactical voting burden). Benefits from selection mechanism but constrained by spoiler logic. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PARTY APPARATUS (ROPE) — Pure coordination benefit with arbitrage exit. FPTP enables efficient coalition-building: aggregates diverse preferences into binary choice, reducing decision complexity. Parties see this as solving a collective action problem (how to select representatives from diverse electorate). d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Net beneficiary; system solves their coordination problem efficiently.
constraint_indexing:constraint_classification(fptp_electoral_system, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM COALITION (SCAFFOLD) — Institutional actors (proportional representation advocates, ranked-choice voting organizers, electoral commissions) see FPTP as a temporary coordination failure with a sunset. Many democracies have migrated or are piloting alternatives. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26. Low effective extraction because institutional actors see clear exit path and have agency to build alternative.
constraint_indexing:constraint_classification(fptp_electoral_system, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL LAW INSTITUTION (PITON) — The formal statute persists through constitutional inertia despite known pathologies. Theater ratio 0.58: substantial performative legitimation ('one person one vote') obscures the actual extraction mechanism (winner-take-all spoiler logic). The law maintains its authority via tradition and path dependency, not because it solves an active coordination problem. theater_ratio ≥ 0.70 gate is near but not satisfied; the piton is marginal — barely holds by institutional momentum.
constraint_indexing:constraint_classification(fptp_electoral_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational/universal angle, some might claim FPTP is a natural law of simple plurality: 'Someone has to win the election; the candidate with the most votes is the obvious choice.' But the structural data (ε=0.52, suppression=0.68, theater=0.58) contradict the mountain gate. This is a false summit: the 'inevitability' naturalizes a contingent design choice. Most democracies have adopted alternatives, proving the constraint is not a law of nature but a path-dependent institutional arrangement.
constraint_indexing:constraint_classification(fptp_electoral_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fptp_electoral_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fptp_electoral_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fptp_electoral_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fptp_electoral_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fptp_electoral_system, TR),
    TR >= 0.70.

:- end_tests(fptp_electoral_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The system does not extract wealth (it's not a rent-seeking oligarchy), but it extracts voice and representation. Voters whose preferences are outside the binary are forced to either abandon their preference or accept vote 'wastefulness.' The metric measures representation loss relative to vote share: a party with 25% nationally but zero seats has extractiveness approaching 1.0 from that group. The aggregate ε=0.52 reflects that roughly half the voting population experiences meaningful voice suppression in any given election (safe-district voters + third-party sympathizers + tactical voters). Suppression (0.68): High and structural. Barriers to voice include: (1) spoiler effect (third-party participation triggers strategic voting collapse), (2) wasted-vote burden (millions of votes for losing candidates generate zero representation), (3) tactical voting obligation (voters must vote against preference to avoid worse outcome), (4) safe-district lock-in (majority-party voters in safe districts face zero competitive incentive). These are not bugs but design features of FPTP — they are structural to the mechanism. Theater ratio (0.58): Moderate-high and increasing. Legitimation performance includes the 'one person one vote' rhetoric (obscures winner-take-all spoiler dynamics), mandate claims by sub-majority winners, and narrative of 'representing your constituency' (obscures that safe-district minorities have zero representation). The theater has increased because parties now claim mandates with smaller vote shares as polarization has widened. This is characteristic of Goodhart drift: as the constraint's extraction grows, the legitimation theater must also grow to maintain acceptance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence despite identical structural data. The major party apparatus sees pure coordination (Rope) — FPTP solves the problem of aggregating diverse voter preferences into binary choices. They experience the system as enabling coalition formation and efficient representation. Swing voters in competitive districts see tangled rope (mixed coordination and extraction) — their vote genuinely moves outcomes, but they're suppressed when district margins are safe. Third-party candidates and marginalized voters see pure snare (Snare) — the system suppresses their voice and offers no viable path forward. The electoral reform coalition sees a temporary problem with a sunset (Scaffold) — proportional representation and ranked-choice voting are available alternatives that democracies are adopting. The electoral law institution sees itself as a degraded piton (Piton) — formally legitimate but substantively performative, maintained through inertia. The analytical observer might naturalize the system as a law of plurality (Mountain: 'obviously the candidate with most votes wins'), but the structural data reveals this as a false summit — other democracies have solved the representation aggregation problem differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized voters: Victim + trapped → d≈0.93, f(d)≈1.40. Near-maximum extraction; no exit route. Third-party candidates: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; participation itself becomes the spoiler trap. Swing voters: Mixed + constrained → d≈0.68, f(d)≈1.08. Significant extraction but modulated by genuine coordination value in swing districts. Major party apparatus: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary; system solves their coalition problem with minimal cost. Electoral reform coalition: Institutional + mobile → d≈0.45, f(d)≈0.50. Moderate extraction because they're mobile (alternatives available) but constrained by path-dependent status quo. Electoral law institution: Institutional + constrained (piton) → d≈0.15, f(d)≈-0.01. Low directionality for the formal statute itself, but high theater (≥0.70 gate is marginal) indicates degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that FPTP is NOT a false natural law but a contingent institutional arrangement with measurable coordination benefits that are offset by extractive costs. The constraint prevents mislabeling it as pure snare (it does aggregate preferences and solve a real coordination problem) and prevents false mountain classification (other systems work). The mandatrophy is managed via the tangled_rope classification: the system has a genuine coordination function (major parties do solve the problem of aggregating diverse preferences), but it extracts from third parties and safe-district minorities to achieve this coordination. The classification holds that both properties are real: the coordination is not theater, and the extraction is not incidental. The remedy is not to eliminate FPTP (that would eliminate the coordination function) but to decompose it — replace FPTP with a system that provides equivalent coordination at lower extraction cost. Proportional representation and ranked-choice voting are empirically shown to provide similar or superior coordination at lower spoiler-driven suppression. The mandatrophy analysis confirms that FPTP is not a natural law (other systems work), not pure extraction (it does enable representation), but a suboptimal equilibrium where institutional inertia prevents migration to dominating alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spoiler_threshold_empirical,
    'Below what vote share does a third-party candidate trigger measurable spoiler effects (shifting winner identity in >5% of races)?',
    'Historical regression analysis: compare outcome probability (winner identity) when third-party vote share crosses 3%, 5%, 8%, 12% thresholds; identify causal threshold via discontinuity design',
    'If threshold is low (~3%): FPTP suppression is extreme; almost any alternative voice creates strategic voting collapse. If threshold is high (~15%+): spoiler effect is minor; FPTP is more rope than snare. Current evidence suggests 5-8% in competitive races.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spoiler_threshold_empirical, empirical, 'Spoiler effect threshold below which outcome probability shifts').

omega_variable(
    safe_district_representation_fidelity,
    'In safe districts (>15% margin), do voters in the losing supermajority experience the constraint as suppressive extraction or as legitimate minority status?',
    'Survey data: measure voter perception of representation quality; compare safe vs competitive districts; analyze exit-voice-loyalty patterns (do majority-party voters stay engaged despite guaranteed outcome?)',
    'If suppressive: FPTP is snare from more perspectives (high d). If legitimate: FPTP is rope or scaffold for safe-district minorities (moderate d). Current evidence suggests younger voters in safe districts experience high suppression (low engagement); older voters accept it as normal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safe_district_representation_fidelity, empirical, 'Voter perception of representation fidelity in safe districts').

omega_variable(
    alternative_system_stability_equivalence,
    'Do proportional representation or ranked-choice voting systems generate equivalent or lower total extraction/suppression when accounting for coalition negotiation costs and minority veto power?',
    'Comparative analysis: measure suppression (voice barriers), theater (legitimation performance), and extractiveness (rent-seeking by dominant coalition) across electoral systems in comparable democracies; control for confounds (institutional maturity, political culture, economic inequality)',
    'If alternatives are empirically equivalent: FPTP is a path-dependent Schelling point, not uniquely extractive. If alternatives are lower-extraction: FPTP is structurally worse than available alternatives. If alternatives are higher-extraction: FPTP''s simplicity is a genuine advantage (rent-seeking just takes different form).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_stability_equivalence, empirical, 'Comparative extraction/suppression across electoral systems').

omega_variable(
    mandate_legitimacy_paradox,
    'Does FPTP winner legitimacy (claiming a mandate) actually persist longer or stronger than PR-elected coalitions, despite lower vote share? Does it reduce defection pressure or increase elite accountability?',
    'Longitudinal legislative voting records: measure bill passage rates, defection rates, and coalition stability for FPTP-elected governments vs PR-elected governments; analyze executive accountability via confidence votes and parliamentary pushback',
    'If FPTP strengthens mandate: tangled_rope classification confirmed (coordination benefit is real). If FPTP produces equivalent or weaker legitimacy: extractiveness is not offset by meaningful coordination gain; reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_legitimacy_paradox, empirical, 'Whether FPTP mandates produce stronger governance legitimacy than alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fptp_electoral_system, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fptp_tr_t0, fptp_electoral_system, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fptp_tr_t50, fptp_electoral_system, theater_ratio, 50, 0.51).
narrative_ontology:measurement(fptp_tr_t100, fptp_electoral_system, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(fptp_be_t0, fptp_electoral_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fptp_be_t50, fptp_electoral_system, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(fptp_be_t100, fptp_electoral_system, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fptp_electoral_system, information_standard).
narrative_ontology:affects_constraint(fptp_electoral_system, two_party_duopoly).
narrative_ontology:affects_constraint(fptp_electoral_system, gerrymandering_district_design).
narrative_ontology:affects_constraint(fptp_electoral_system, voter_suppression_access).

% DUAL FORMULATION NOTE:
% FPTP is downstream of the basic representation aggregation problem (how to select representatives from diverse electorate) but represents a distinct constraint. The upstream problem has multiple solutions (FPTP, proportional, ranked-choice, etc.); FPTP is one design choice with its own ε and suppression profile. The downstream constraints (two-party duopoly, gerrymandering, voter suppression) are enabled by FPTP's structural features and would have different forms under alternative electoral systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
