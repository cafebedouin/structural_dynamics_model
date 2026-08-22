% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Credibility Paradox (Institutional Deterrence Regime)
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the credibility-paradox reading of the
 *   nuclear-impossibility kernel: the logical tension that deterrence
 *   requires a credible threat to use nuclear weapons, but any actual use
 *   guarantees mutual destruction, rendering the threat inherently
 *   incredible. The institutional response to this paradoxâperpetual
 *   modernization, counterforce innovation, tactical nuclear renaissance, and
 *   elaborate escalation doctrineâconstitutes the standing arrangement
 *   under contest. Rather than resolving the paradox, the regime manages it
 *   performatively while extracting massive resources and externalizing
 *   existential risk onto populations and non-nuclear states.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_establishment: Primary agenda-setter (institutional/arbitrage) â designs doctrine and captures budgets
 *   - great_power_executives: Primary beneficiary (powerful/constrained) â inherits strategic leverage
 *   - citizenries_of_nuclear_powers: Primary payer (powerless/trapped) â bears risk and cost without voice
 *   - nonnuclear_weapon_states: Secondary payer (moderate/trapped) â locked into NPT and extended deterrence crossfire
 *   - strategic_studies_community: Analytical observer (analytical/analytical) â supplies legitimating vocabulary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.68).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Credibility Paradox (Institutional Deterrence Regime)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, 'fa35015c-b06f-4b0f-bf31-80814b983a02').
narrative_ontology:cs_kernel_codification('fa35015c-b06f-4b0f-bf31-80814b983a02', formalized).
narrative_ontology:cs_authority_grounding('fa35015c-b06f-4b0f-bf31-80814b983a02', lineage).
narrative_ontology:cs_interpretation_layer_present('fa35015c-b06f-4b0f-bf31-80814b983a02').
narrative_ontology:cs_reading_relation('fa35015c-b06f-4b0f-bf31-80814b983a02', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa35015c-b06f-4b0f-bf31-80814b983a02', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('fa35015c-b06f-4b0f-bf31-80814b983a02', foundational, threatened_self_annihilation_inherently_incredible).
narrative_ontology:cs_axiom_status(threatened_self_annihilation_inherently_incredible, holdable).
narrative_ontology:cs_axiom_grounding('fa35015c-b06f-4b0f-bf31-80814b983a02', threatened_self_annihilation_inherently_incredible, empirically_contingent).
narrative_ontology:cs_axiom('fa35015c-b06f-4b0f-bf31-80814b983a02', foundational, escalation_management_preserves_war_reachability).
narrative_ontology:cs_axiom_status(escalation_management_preserves_war_reachability, holdable).
narrative_ontology:cs_axiom_grounding('fa35015c-b06f-4b0f-bf31-80814b983a02', escalation_management_preserves_war_reachability, instrumental).
narrative_ontology:cs_reference_frame('fa35015c-b06f-4b0f-bf31-80814b983a02', mutual_vulnerability_deterrence_stability).
narrative_ontology:cs_drift_state('fa35015c-b06f-4b0f-bf31-80814b983a02', multipolar_counterforce_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa35015c-b06f-4b0f-bf31-80814b983a02', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_establishment).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_executives).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, citizenries_of_nuclear_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, nonnuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, maintains, and doctrinalizes the nuclear arsenal and its employment theory. Justifies budgets and institutional survival through constant modernization, counterforce innovation, and the performative maintenance of credible threats. Could pivot to conventional defense analysis but carries significant identity lock as the nuclear priesthood.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Inherit strategic coercive leverage from nuclear status. Benefit from extended deterrence relationships and great-power bargaining weight. Cannot unilaterally renounce arsenals without ceding relative strategic position, but personally bear no direct cost of arsenal maintenance or targeting risk.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_executives, beneficiary,
    powerful, biographical, constrained, global).

% Bear the existential risk of being targeted by rival arsenals and the fiscal cost of maintaining those arsenals. Have negligible voice in nuclear doctrine or targeting policy. Cannot opt out of deterrence relationships except through emigration, which merely shifts them into another nuclear or non-nuclear zone.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, citizenries_of_nuclear_powers, payer,
    powerless, generational, trapped, global).

% Locked into the Nuclear Non-Proliferation Treaty regime that legally preserves the arsenals of nuclear powers while denying them equivalent weapons. Many are tethered to extended deterrence guarantees that place them in the escalation crossfire without giving them a seat in nuclear planning. Diplomatically marginalized when they demand disarmament timelines.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nonnuclear_weapon_states, payer,
    moderate, generational, trapped, global).

% Analyzes deterrence stability, credibility, and arms control. Provides the theoretical vocabulary that legitimates or critiques the regime. Maintains professional incentives tied to the continued relevance of nuclear strategy as a field of inquiry.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_studies_community, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing direct great-power nuclear war by making the anticipated costs of initiation exceed any conceivable benefit, thereby coordinating mutual restraint through the prospect of mutual destruction.
% TRANSFER_FUNCTION: Moves existential risk and tax revenue from citizenries and non-nuclear states to nuclear weapons establishments and great-power strategic leverage; moves diplomatic deference from non-nuclear states to nuclear powers.
% ABSENT_VOICES: Citizenries of nuclear powers are formally enfranchised but effectively excluded from nuclear targeting and employment policy; non-nuclear states are present at NNPT forums but structurally excluded from decisions about arsenal size, modernization, or doctrine. Anti-nuclear social movements are kept outside formal strategic planning.
% DISAPPEARANCE_RATIONALE: If the credibility paradox vanishedâif nuclear threats became automatically credible without paradox, or automatically incredible without consequenceâthe entire institutional architecture of deterrence theory, arms racing, and counterforce modernization would lose its organizing rationale. Alliances structured around extended deterrence would face renegotiation, and the strategic studies field would contract dramatically.
% FOUNDING_PROBLEM: Preventing recurrence of great-power total war after 1945 in a world where nuclear weapons had collapsed the cost-distance between attack and annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists (Schelling, Kahn, Wohlstetter) documented the credibility problem from outside the benefiting parties in the 1950sâ60s. Contemporary arms control analysts outside the nuclear establishment corroborate that the founding problem of great-power war prevention has been substantially solved by the terror balance, while the arrangement persists to serve institutional and coercive functions. The nuclear establishment itself attests the problem remains live, citing emerging threats.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the paradox justifies institutional capture far beyond the pure war-prevention minimum: counterforce arsenals, tactical renaissance, and alliance infrastructural costs extract wealth and risk from payers. Suppression (0.68) is high because disarmament advocacy and non-nuclear demands are systematically marginalized through NPT hierarchy, alliance discipline, and strategic discourse that treats abolition as naive. Theater_ratio (0.55) reflects the performative core of the entire enterpriseâmaking an incredible threat credible through ritualized modernization, signaling, and doctrinal innovation. Accessibility_collapse (0.40) is moderate: abolition and denuclearization remain conceptually available but are institutionally foreclosed. Resistance (0.38) is moderate: anti-nuclear movements and some non-nuclear coalitions mount real opposition but are excluded from decision fora.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the arrangement is genuine coordination that has prevented great-power nuclear war for eight decades. From the payer seats, the same arrangement operates as asymmetric risk imposition: citizenries bear annihilation risk while executives and establishments capture strategic and budgetary rents. The engine computes this divergence from the structural dataâbeneficiaries with constrained or arbitrage exit versus payers with trapped exitârather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear_weapons_establishment sits near the beneficiary end: it directly collects budgets, institutional prestige, and generational mission from the paradox. Great_power_executives sit slightly further toward symmetric but still in beneficiary territory: they harvest strategic leverage without personal cost. Citizenries_of_nuclear_powers and nonnuclear_weapon_states sit near the full-target end: they bear the existential and fiscal costs of a deterrence logic they did not author, with trapped exit options that amplify effective extraction. The strategic_studies_community sits near analytical center: neither collecting nor paying, but furnishing the interpretive layer that sustains the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two misreadings. First, it blocks a pure rope reading by naming the victimsâcitizenries and non-nuclear states who pay for the coordination through risk and exclusionâthereby preventing the regime from being certified as benign coordination. Second, it blocks a pure snare reading by acknowledging the genuine coordination function: the empirical record of eighty years without great-power nuclear war is real and must not be disappeared. The classification captures the hybrid: a coordination mechanism whose persistence requires active enforcement and whose costs fall asymmetrically on those outside the decision loop.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterforce_technology_paradox_resolution,
    'Will emerging counterforce capabilities (hypersonic delivery, AI-enhanced targeting, missile defense) resolve the credibility paradox by making limited nuclear victory structurally plausible, or will they merely deepen the theater?',
    'Empirical observation of doctrinal shifts and arms-control telemetry over the next two decades: if arsenals shift decisively toward counterforce without triggering crisis instability, the paradox is dissolving; if modernization cycles continue without altering the core incredibility of mass use, the paradox persists as justification.',
    'If resolvable, the constraint reclassifies toward snare (pure extraction via warfighting postures) or rope (genuine coordination through dominance); if irresolvable, the tangled_rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_technology_paradox_resolution, empirical, 'Whether technology can dissolve the credibility paradox or deepens performative management').

omega_variable(
    nuclear_taboo_autonomy,
    'Does the nuclear taboo operate as an independent internalized constraint that makes the credibility paradox moot, or is the taboo itself parasitic on the institutional deterrence regime?',
    'Comparative case analysis of non-use in near-miss crises: if taboo-driven restraint appears even when escalation dominance is plausible, the taboo is autonomous and supports a structural_contraction reading; if non-use tracks strategic cost-benefit alone, the taboo is derivative and the credibility paradox remains live.',
    'An autonomous taboo would reduce effective extraction by providing a genuine, low-cost coordination mechanism; a derivative taboo leaves the regime dependent on performative enforcement and institutional capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_taboo_autonomy, conceptual, 'Whether the nuclear taboo is an independent constraint or a product of the deterrence regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 40, 0.7).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 80, 0.55).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
