% ============================================================================
% CONSTRAINT STORY: scale_ceiling_c0
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scale_ceiling_c0, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scale_ceiling_c0
 *   human_readable: Scale Ceiling on Economy-Wide Labor Coordination
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   The scale ceiling constraint describes a claimed natural limit on
 *   economy-wide labor coordination as polity size increases. The empirical
 *   pattern is uncontested: union density correlates with policy
 *   responsiveness in small polities (Nordic countries, ~5-10 million
 *   population) but not in large fragmented polities (United States, ~330
 *   million; European Union as a regulatory space, ~450 million). The
 *   contested question is whether this pattern reflects a natural
 *   coordination-cost scaling law (the mountain claim) or constructed
 *   institutional choices — regulatory fragmentation, capital mobility rules,
 *   jurisdictional competition — that could be redesigned (the
 *   constructed-constraint counterclaim). The constraint is claimed as
 *   mountain based on the cross-national gradient; it is authored with
 *   beneficiaries (capital holders, regulatory arbitrage firms) to trigger
 *   false-summit evaluation, with omegas documenting the irreducible
 *   natural-vs-constructed ambiguity. KEY AGENTS (by structural
 *   relationship): - Labor organizers in large polities: Primary targets
 *   (organized/constrained) — face superlinear coordination costs, cannot
 *   achieve economy-wide bargaining power - Labor organizers in small
 *   polities: Beneficiaries (organized/mobile) — operate below the scale
 *   threshold where economy-wide coordination remains feasible - Capital
 *   holders: Primary beneficiaries (powerful/arbitrage) — gain structural
 *   bargaining advantage from the coordination ceiling in large polities -
 *   Regulatory arbitrage firms: Secondary beneficiaries
 *   (institutional/arbitrage) — exploit fragmentation that the scale ceiling
 *   prevents labor from closing - Workers in large polities: Diffuse targets
 *   (powerless/trapped) — bear the policy-responsiveness consequences of
 *   failed coordination - Workers in small polities: Diffuse beneficiaries
 *   (moderate/constrained) — benefit from feasible economy-wide coordination
 *   - Comparative political economists: Analytical observers — document the
 *   gradient, contest the naturalness claim
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scale_ceiling_c0, 0.18).
domain_priors:suppression_score(scale_ceiling_c0, 0.12).
domain_priors:theater_ratio(scale_ceiling_c0, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scale_ceiling_c0, extractiveness, 0.18).
narrative_ontology:constraint_metric(scale_ceiling_c0, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(scale_ceiling_c0, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(scale_ceiling_c0, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(scale_ceiling_c0, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scale_ceiling_c0, mountain).
narrative_ontology:human_readable(scale_ceiling_c0, "Scale Ceiling on Economy-Wide Labor Coordination").
narrative_ontology:topic_domain(scale_ceiling_c0, "political_economy/democratic_theory/institutional_analysis").

domain_priors:emerges_naturally(scale_ceiling_c0).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scale_ceiling_c0, capital_holders).
narrative_ontology:constraint_beneficiary(scale_ceiling_c0, regulatory_arbitrage_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(scale_ceiling_c0, labor_organizers_small_polities).
narrative_ontology:constraint_beneficiary(scale_ceiling_c0, workers_small_polities).
narrative_ontology:constraint_victim(scale_ceiling_c0, labor_organizers_large_polities).
narrative_ontology:constraint_victim(scale_ceiling_c0, workers_large_polities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attempt to build economy-wide coordination in large fragmented polities with open capital accounts. Face coordination costs that scale superlinearly with polity size: more jurisdictions to align, more sectoral interests to reconcile, more exit points for capital to exploit. Can achieve sectoral coordination but economy-wide bargaining power remains structurally out of reach regardless of organizing effort.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, labor_organizers_large_polities, payer,
    organized, generational, constrained, national).

% Operate in small polities where coordination costs remain sublinear and economy-wide bargaining is structurally feasible. Achieve comprehensive labor coordination that translates to policy responsiveness. The same coordination technology that fails at large scale succeeds here not because of superior organizing but because the polity size sits below the structural threshold.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, labor_organizers_small_polities, beneficiary,
    organized, generational, mobile, national).

% Benefit from the coordination ceiling in large polities: fragmentation and scale prevent comprehensive labor coordination, leaving capital with structural bargaining advantage. In small polities where labor can coordinate economy-wide, capital faces symmetric bargaining power; in large polities the scale ceiling operates as a structural asymmetry favoring capital mobility over labor coordination.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Exploit regulatory fragmentation within large polities to route operations toward jurisdictions with weaker labor standards. The scale ceiling ensures no economy-wide labor coordination can close the arbitrage opportunity; sectoral coordination in one jurisdiction simply redirects capital to adjacent ones.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, regulatory_arbitrage_firms, beneficiary,
    institutional, biographical, arbitrage, continental).

% Bear the consequences of the coordination ceiling: sectoral gains are available but economy-wide bargaining power is structurally foreclosed. Policy responsiveness to labor interests remains low regardless of organizing effort because comprehensive coordination cannot form at scale. Geographic and occupational mobility is constrained; exit from the polity is prohibitively costly.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, workers_large_polities, payer,
    powerless, biographical, trapped, national).

% Benefit from living in polities below the scale threshold where economy-wide labor coordination is structurally feasible. Experience higher policy responsiveness and stronger labor standards as a result of comprehensive coordination that the scale ceiling prevents elsewhere.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, workers_small_polities, beneficiary,
    moderate, biographical, constrained, national).

% Document the cross-national pattern: union density correlates with policy responsiveness in small polities but not in large fragmented ones. Debate whether the pattern reflects a natural coordination-cost scaling law or constructed institutional choices that could be redesigned. Measure the gradient, model the threshold, and contest the naturalness claim.
narrative_ontology:constraint_stakeholder(scale_ceiling_c0, comparative_political_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None claimed at the constraint level — the constraint is the absence of a coordination mechanism that would work at large scale. The question is whether that absence is natural law or constructed institutional failure.
% TRANSFER_FUNCTION: The scale ceiling transfers bargaining power from labor to capital in large polities by preventing the formation of economy-wide coordination that would equalize negotiating leverage. No direct monetary transfer, but a structural shift in the terms of exchange.
% ABSENT_VOICES: Workers in large polities who would benefit from economy-wide coordination but cannot form it are present as payers, not excluded. Alternative institutional designers who would argue the ceiling is a choice, not a law, are present as observers. No structurally excluded seat identified.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if coordination costs did not scale superlinearly, or if institutional redesign made economy-wide coordination feasible in large polities — labor bargaining power would equalize with capital, policy responsiveness would rise, and regulatory arbitrage opportunities would compress. Capital holders and the mountain-claim defenders argue this is impossible (natural law); labor organizers and institutional reformers argue it is a matter of design.
% FOUNDING_PROBLEM: No founding problem — the constraint is claimed as a natural feature of coordination-cost scaling, not a designed solution to a problem.
% FOUNDING_PROBLEM_CORROBORATION: The mountain claim is corroborated by cross-national empirical patterns documented by comparative political economists outside any benefiting party. The constructed-constraint counterclaim is corroborated by institutional historians who document that regulatory fragmentation and capital mobility are policy choices, not natural constants.
narrative_ontology:disappearance_verdict(scale_ceiling_c0, contested).
narrative_ontology:founding_problem_status(scale_ceiling_c0, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(scale_ceiling_c0, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-12',
    'cohort_zero_regen', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'temperature=0.2').
narrative_ontology:story_seed(scale_ceiling_c0, 'scale_ceiling', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scale_ceiling_c0_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(scale_ceiling_c0, ExtMetricName, E),
    domain_priors:suppression_score(scale_ceiling_c0, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(scale_ceiling_c0),
    narrative_ontology:constraint_metric(scale_ceiling_c0, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(scale_ceiling_c0, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(scale_ceiling_c0_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the constraint operates as a structural limit rather than active rent collection — capital holders benefit from the coordination failure but do not directly extract from labor through the constraint itself. The modest upward drift reflects increasing capital mobility and regulatory fragmentation over the interval, which amplify the coordination ceiling's asymmetric effect. Suppression is very low (0.12) because the constraint does not actively prevent organizing — labor can coordinate sectorally at any scale; the ceiling operates on economy-wide coordination specifically. Theater ratio is near-zero (0.08) because there is no performative maintenance — the constraint persists whether or not anyone defends it. Accessibility collapse is high (0.82) because once the scaling relationship is understood, alternatives to the coordination ceiling (institutional redesign to reduce fragmentation, capital controls to reduce mobility asymmetry) are visible but require coordinated polity-level action that the constraint itself prevents. Resistance is low (0.15) because the mountain framing is widely accepted in policy discourse; resistance comes primarily from institutional reformers and labor movements contesting the naturalness claim.
 *
 * PERSPECTIVAL GAP:
 *   From the capital-holder and regulatory-arbitrage seats, the constraint is a natural feature of coordination-cost scaling — a mountain that no institutional redesign can move. From the labor-organizer and worker seats in large polities, the same structure operates as a constructed institutional failure: regulatory fragmentation and capital mobility are policy choices, and the coordination ceiling is the predictable result of those choices. The analytical seat documents the empirical gradient but contests whether it reflects natural law or institutional path dependence. The engine computes this divergence from the structural data; the authored mountain claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital holders and regulatory arbitrage firms are structural beneficiaries: the coordination ceiling in large polities creates a bargaining-power asymmetry favoring capital mobility over labor coordination. Their directionality sits near the beneficiary end (d ~ 0.1-0.2). Labor organizers in large polities and workers in large polities are the targets: they bear the coordination-cost burden and the policy-responsiveness consequences; their directionality sits near the target end (d ~ 0.7-0.9). Labor organizers and workers in small polities are beneficiaries of living below the threshold (d ~ 0.2-0.3). Comparative political economists are analytical observers (d = 0.5). The asymmetry is structural: the constraint operates differently at different scales, and agents cannot choose their polity size.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a mandatrophy case — it is not a coordination mechanism whose function has been captured. It is a claimed natural limit on coordination itself. The mandatrophy question does not apply because there is no original coordinating function to have outlived. The relevant analytical question is whether the limit is natural (mountain) or constructed (a snare or tangled rope masquerading as natural law via the false-summit mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_scaling,
    'Is the superlinear scaling of coordination costs with polity size a natural law of collective action, or is it the product of constructed institutional choices (regulatory fragmentation, capital mobility rules) that could be redesigned?',
    'Natural experiment from polities that reduce fragmentation or impose capital controls: if economy-wide coordination becomes feasible in large polities under alternative institutional arrangements, the scaling relationship is constructed, not natural. Alternatively, formal modeling of coordination-cost scaling under different institutional regimes.',
    'If natural, the constraint is a genuine mountain and the cross-national gradient reflects an irreducible limit. If constructed, the constraint is a false summit — a tangled rope or snare whose beneficiaries (capital holders, arbitrage firms) gain from the mountain framing that forecloses institutional redesign.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_scaling, conceptual, 'Whether coordination-cost scaling is natural law or institutional artifact.').

omega_variable(
    threshold_location,
    'Where is the scale threshold above which economy-wide coordination becomes structurally infeasible, and is that threshold fixed or does it shift with communication technology and institutional design?',
    'Cross-national comparison controlling for polity size, regulatory structure, and capital mobility; historical analysis of threshold shifts as communication costs fell and coordination technology improved.',
    'A fixed threshold would support the mountain claim; a threshold that shifts with institutional variables would support the constructed-constraint counterclaim. If the threshold has moved upward over time (larger polities can now coordinate economy-wide than could in prior eras), the ceiling is partly a function of available coordination technology, not purely natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_location, empirical, 'Whether the scale threshold is a natural constant or an institutional variable.').

omega_variable(
    sectoral_vs_economy_wide_distinction,
    'Is the distinction between sectoral coordination (feasible at any scale) and economy-wide coordination (infeasible above threshold) a natural feature of coordination-cost scaling, or is it an artifact of how labor law and collective bargaining are institutionally structured?',
    'Comparison of polities with different collective-bargaining institutional structures: if some large polities achieve economy-wide coordination through alternative institutional designs (e.g., sectoral bargaining with extension mechanisms that approximate economy-wide coverage), the distinction is institutional, not natural.',
    'If the sectoral/economy-wide distinction is institutional, the constraint is constructed and the mountain claim is false. If the distinction persists across all institutional designs, it supports the natural-scaling interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_vs_economy_wide_distinction, empirical, 'Whether the sectoral/economy-wide boundary is natural or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scale_ceiling_c0, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scal_tr_t0, scale_ceiling_c0, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(scal_tr_t0, observed).
narrative_ontology:measurement(scal_tr_t10, scale_ceiling_c0, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(scal_tr_t10, observed).
narrative_ontology:measurement(scal_tr_t20, scale_ceiling_c0, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(scal_tr_t20, observed).
narrative_ontology:measurement(scal_tr_t30, scale_ceiling_c0, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(scal_tr_t30, observed).
narrative_ontology:measurement(scal_tr_t40, scale_ceiling_c0, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(scal_tr_t40, observed).
narrative_ontology:measurement(scal_tr_t50, scale_ceiling_c0, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(scal_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(scal_be_t0, scale_ceiling_c0, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(scal_be_t0, observed).
narrative_ontology:measurement(scal_be_t10, scale_ceiling_c0, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(scal_be_t10, observed).
narrative_ontology:measurement(scal_be_t20, scale_ceiling_c0, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(scal_be_t20, observed).
narrative_ontology:measurement(scal_be_t30, scale_ceiling_c0, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(scal_be_t30, observed).
narrative_ontology:measurement(scal_be_t40, scale_ceiling_c0, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(scal_be_t40, observed).
narrative_ontology:measurement(scal_be_t50, scale_ceiling_c0, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(scal_be_t50, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(scale_ceiling_c0, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scale_ceiling_c0, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition under the epsilon-invariance principle. The cross-national responsiveness gradient could be measured via union density (low extraction, mountain-like) or via capital-mobility asymmetry (higher extraction, constructed). If future analysis establishes that these observables yield substantially different epsilon values, decompose into separate stories and link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
