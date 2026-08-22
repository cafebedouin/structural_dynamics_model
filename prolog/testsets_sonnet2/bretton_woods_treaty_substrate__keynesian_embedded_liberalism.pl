% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian Embedded Liberalism Reading)
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This story instantiates the keynesian_embedded_liberalism reading of the
 *   Bretton Woods treaty substrate kernel: the postwar system's capital
 *   account restrictions are read as legitimate, sanctioned constraint tools
 *   that protect national governments' capacity to pursue full-employment and
 *   welfare-state policy against the disciplining force of internationally
 *   mobile capital. From this reading's own lights, the party who pays the
 *   cost of the arrangement is international finance — barred from the yield
 *   arbitrage and disciplining leverage it would otherwise exercise over
 *   domestic policy. This is a distinct constraint from the sibling readings
 *   under the same kernel: the neoliberal_convertibility reading treats
 *   domestic government intervention as the constrained party and free
 *   capital markets as the beneficiary (inverting victim and beneficiary sets
 *   relative to this story); the sovereignty_defense reading treats external
 *   monetary discipline itself as the constrained object, foregrounding
 *   national monetary sovereignty rather than embedded-liberalism policy
 *   space as the value being protected. All three readings describe the same
 *   treaty architecture (Article VI capital controls, adjustable peg, IMF
 *   conditionality) but assign different beneficiary/victim structures and
 *   different normative weight to the coordination function, producing three
 *   separate ε values and three separate constraint stories linked by
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.55).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '428bbda9-7bb8-4abb-b544-8c1a00ae23cf').
narrative_ontology:cs_kernel_codification('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', formalized).
narrative_ontology:cs_authority_grounding('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', lineage).
narrative_ontology:cs_interpretation_layer_present('428bbda9-7bb8-4abb-b544-8c1a00ae23cf').
narrative_ontology:cs_reading_relation('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', foundational, domestic_full_employment_policy_takes_precedence_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_full_employment_policy_takes_precedence_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', domestic_full_employment_policy_takes_precedence_over_capital_mobility, instrumental).
narrative_ontology:cs_axiom('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', foundational, capital_controls_are_legitimate_treaty_sanctioned_tools_not_rights_violations).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_treaty_sanctioned_tools_not_rights_violations, holdable).
narrative_ontology:cs_axiom_grounding('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', capital_controls_are_legitimate_treaty_sanctioned_tools_not_rights_violations, conventional).
narrative_ontology:cs_reference_frame('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', keynesian_policy_space_settlement).
narrative_ontology:cs_drift_state('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', post_1973_float_and_liberalization_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('428bbda9-7bb8-4abb-b544-8c1a00ae23cf', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_and_welfare_constituencies).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, central_banks_pursuing_full_employment_policy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_portfolio_investors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporate_treasuries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and enforce the Bretton Woods architecture — fixed-but-adjustable exchange rates, IMF-sanctioned capital controls, current-account convertibility without full capital-account convertibility. Use the resulting insulation to run countercyclical fiscal policy, build welfare states, and target full employment without immediate speculative attack on the currency. Could in principle dismantle the controls unilaterally but doing so would reopen exposure to the capital flows the system is built to dampen.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary).

% Benefit from the policy space capital controls create: governments can run deficits, expand social insurance, and target employment without an immediate exchange-rate or capital-flight veto from international investors. Cannot exit the arrangement individually — their gains ride entirely on the government's continued willingness and ability to maintain controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_and_welfare_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Set domestic interest rates and credit conditions with reduced fear of triggering destabilizing capital flight, because the capital-account restrictions dampen the speed and volume of exit available to foreign and domestic holders of the currency. This autonomy is the entire point of the arrangement from this seat.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, central_banks_pursuing_full_employment_policy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, central_banks_pursuing_full_employment_policy, agenda_setter).

% Private banks, bondholders, and would-be currency speculators are legally barred or heavily taxed when moving capital across borders to arbitrage interest-rate differentials or take positions against a currency. From this reading's lights, this is a legitimate, sanctioned constraint on a form of activity that would otherwise override democratic macroeconomic choices — not an unjust taking, but it is nonetheless where the cost of the arrangement lands. Exit from the constraint itself is barred by treaty and domestic law; the only exit is via illegal capital flight or regulatory arbitrage in jurisdictions outside the system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance, payer,
    powerful, biographical, trapped, global).

% Face licensing requirements, dual exchange rates, or outright prohibitions on moving funds to chase yield differentials between countries. Some routing through offshore markets (proto-Eurodollar activity) provides partial exit, but the formal system is built to close this channel over time.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_portfolio_investors, payer,
    powerful, biographical, constrained, global).

% Firms with cross-border operations must navigate exchange controls when repatriating profits or financing subsidiaries, raising transaction costs and limiting treasury optimization relative to a world of free capital mobility. They lobby for liberalization but operate within the controls as a cost of doing business across borders.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporate_treasuries, payer,
    organized, biographical, constrained, global).

% Administer the par-value system, sanction the use of capital controls under Article VI, and provide balance-of-payments financing so governments do not have to choose between policy autonomy and external balance. Their institutional survival depends on the perceived legitimacy of the coordination function.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_and_bretton_woods_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Operate in the growing unregulated dollar-denominated market outside domestic jurisdiction, which functions as a slow-building escape valve from the controls. Their perspective — that capital mobility is a legitimate market function being suppressed — is not represented in the treaty negotiations; they simply route around it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, offshore_eurodollar_market_participants, excluded,
    organized, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, diffuse).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multilateral commitment among sovereign states to permit capital-account restrictions and current-account convertibility together, so that no single government's pursuit of full employment or welfare-state policy is punished by a speculative attack it cannot resist alone.
% TRANSFER_FUNCTION: Moves policy latitude and macroeconomic stability from footloose international capital toward national governments and their domestic constituencies; the corresponding cost is borne by financial actors as forgone arbitrage, forgone yield-chasing, and transaction friction on cross-border capital movement.
% ABSENT_VOICES: International financial firms and would-be currency speculators are formally sanctioned as the target of restriction by treaty design; they are present at Bretton Woods only as the problem to be solved, not as parties whose preferences are weighed. Offshore Eurodollar participants are entirely outside the negotiating room and simply build an exit route around the system rather than contesting it directly.
% DISAPPEARANCE_RATIONALE: If the capital-control architecture vanished overnight, governments would immediately face the discipline of open capital markets: fiscal deficits, employment targets, and credit policy would all become subject to real-time capital-flight veto, precipitating either policy retrenchment or currency crises — the entire postwar welfare-state settlement in many countries was built assuming this insulation existed.
% FOUNDING_PROBLEM: The interwar experience of beggar-thy-neighbor devaluations, hot-money flows, and the 1930s deflationary spiral driven by capital flight and gold-standard rigidity had discredited unrestricted capital mobility; the founding problem was to permit trade-facilitating currency convertibility while preventing destabilizing speculative capital flows from constraining domestic full-employment policy.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians studying the interwar collapse (outside any Bretton Woods beneficiary) corroborate that capital mobility restrictions addressed a real and previously catastrophic problem. Financial-sector economists and later IMF staff economists (from the 1970s onward) argue the problem was substantially solved by the 1960s and that continued restriction became rent-preserving policy insulation rather than crisis prevention — this is the core of the dispute the sibling readings (neoliberal_convertibility, sovereignty_defense) contest from opposite directions.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is authored moderate: the constraint imposes real, quantifiable costs on international capital (forgone arbitrage, transaction friction, prohibition on positions) but this reading holds those costs are the deliberate and legitimate price of a genuine coordination good, not rent extraction without offsetting function. Suppression (0.55) is authored higher than extraction because the system's persistence genuinely depends on active enforcement — capital controls require customs and exchange-control bureaucracies, currency licensing regimes, and IMF surveillance, and by the late 1960s the growing Eurodollar market shows the suppression apparatus visibly straining to hold as arbitrage channels multiply. Theater ratio rises modestly (0.08 to 0.20) as the system ages into the 1960s, tracking the growing gap between the formal capital-control architecture and the substantive erosion via offshore markets — enforcement increasingly performs continuity the underlying system can no longer fully deliver. All three tracked metrics share the same six-point time grid (1944, 1949, 1955, 1961, 1967, 1973).
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and the domestic constituencies whose policy space is protected sit near the beneficiary end of directionality: they receive the coordination good (insulated macroeconomic autonomy) and bear only the diffuse cost of administering controls. International finance and cross-border portfolio investors sit near the target end: they are the parties whose activity is directly and specifically restricted, and their exit options are legally foreclosed (trapped/constrained) rather than merely costly, which the derivation chain should read as amplifying rather than damping their effective extraction. Multinational treasuries sit closer to the middle — they pay real friction costs but retain more constrained (rather than trapped) exit via legitimate channels for trade-related conversion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored contested rather than resolved to either pole, because this reading's own internal logic holds the problem (interwar capital-flow instability undermining democratic macroeconomic choice) remained partially live throughout the Bretton Woods era even as the acute 1930s crisis conditions receded — the classification should not collapse into either 'this was always just extraction dressed as coordination' or 'this was purely functional coordination with zero extraction,' both of which would misdescribe a tangled_rope. The engine's tangled_rope classification (requiring both genuine coordination AND asymmetric extraction, both present here) is exactly the right frame to prevent this reading from being mislabeled as pure snare (which would ignore the real policy-space good delivered to domestic constituencies) or pure rope (which would ignore that international finance's exclusion is a lasting, identifiable, coercively maintained cost borne by a specific, nameable set of victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture_boundary,
    'Was the Bretton Woods capital-control regime a genuine, time-limited solution to a real interwar-era coordination failure, or did it become a durable extraction mechanism benefiting entrenched domestic political coalitions (organized labor, national industrial capital) at the expense of a financial sector whose mobility would have disciplined inefficient domestic policy?',
    'Comparative analysis of macroeconomic outcomes (growth, inflation, unemployment, investment efficiency) in capital-control regimes versus the post-1973 floating-rate, liberalized-capital era, controlling for other structural differences (Golden Age productivity catch-up, postwar reconstruction effects) that confound simple before/after comparison.',
    'If the controls primarily protected inefficient policy from deserved market discipline, this reading''s beneficiary framing (governments and domestic constituencies as beneficiaries) becomes harder to sustain and the constraint drifts toward snare from the international-finance seat''s perspective; if the controls primarily protected against genuinely destabilizing speculative flows unrelated to policy quality, the tangled_rope coordination function is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_boundary, empirical, 'Whether capital controls solved a real coordination failure or shielded domestic rent-seeking from external discipline.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Bretton Woods treaty substrate better understood as a single arrangement whose ε and beneficiary structure are genuinely indeterminate across observers, or as three structurally distinct constraints (this reading, neoliberal_convertibility, sovereignty_defense) that happen to share a textual and institutional substrate?',
    'Trace which specific treaty provisions (Article VI, par value adjustment clauses, IMF conditionality terms) each reading''s beneficiary/victim claims actually depend on; readings that depend on disjoint or contradictory provisions support the three-constraints-not-one interpretation this story adopts.',
    'If the three readings are shown to depend on overlapping, non-contradictory provisions, the ε-invariance principle would require further decomposition rather than three parallel stories; if the readings genuinely rest on distinct normative premises about the same provisions (as this story assumes), the current three-story decomposition is the correct resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether kernel decomposition into three sibling readings is the correct resolution or masks a deeper single-constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1949, 0.1).
narrative_ontology:measurement_basis(bret_tr_t1949, observed).
narrative_ontology:measurement(bret_tr_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1955, 0.14).
narrative_ontology:measurement_basis(bret_tr_t1955, observed).
narrative_ontology:measurement(bret_tr_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1961, 0.17).
narrative_ontology:measurement_basis(bret_tr_t1961, observed).
narrative_ontology:measurement(bret_tr_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(bret_tr_t1967, observed).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.2).
narrative_ontology:measurement_basis(bret_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.3).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1949, 0.33).
narrative_ontology:measurement_basis(bret_be_t1949, observed).
narrative_ontology:measurement(bret_be_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1955, 0.37).
narrative_ontology:measurement_basis(bret_be_t1955, observed).
narrative_ontology:measurement(bret_be_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1961, 0.4).
narrative_ontology:measurement_basis(bret_be_t1961, observed).
narrative_ontology:measurement(bret_be_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1967, 0.44).
narrative_ontology:measurement_basis(bret_be_t1967, observed).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement_basis(bret_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.45).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement_basis(bret_su_t1949, observed).
narrative_ontology:measurement(bret_su_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1955, 0.52).
narrative_ontology:measurement_basis(bret_su_t1955, observed).
narrative_ontology:measurement(bret_su_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1961, 0.53).
narrative_ontology:measurement_basis(bret_su_t1961, observed).
narrative_ontology:measurement(bret_su_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement_basis(bret_su_t1967, observed).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(bret_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, sovereignty_defense).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the bretton_woods_treaty_substrate kernel, each a separate constraint story with its own ε, beneficiary/victim structure, and claimed type. keynesian_embedded_liberalism (this story) treats international finance as the constrained/victim party and national governments as beneficiary; neoliberal_convertibility inverts this, treating government intervention as constrained and free capital markets as the beneficiary interest; sovereignty_defense treats external monetary discipline as the constrained object and frames the protected value as national monetary sovereignty rather than welfare-state policy space. All three describe the same treaty text and institutional machinery (IMF Article VI, adjustable peg, capital controls) but are ε-invariant, structurally distinct constraints, linked here via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
