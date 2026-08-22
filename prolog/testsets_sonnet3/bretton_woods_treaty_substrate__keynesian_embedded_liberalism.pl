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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian Embedded Liberalism Reading)
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This story instantiates the Keynesian embedded-liberalism reading of the
 *   Bretton Woods treaty substrate: the arrangement of adjustable pegs,
 *   IMF-sanctioned capital controls (Article VI, Article XIV transitional
 *   provisions), and the scarce-currency clause is read as a deliberate
 *   structural shield protecting national democratic policy space against the
 *   disciplining power of mobile international capital. Under this reading,
 *   national governments and, through them, domestic labor and welfare-state
 *   constituencies are the beneficiaries; international finance and
 *   speculative capital are the parties whose historical freedom of action is
 *   constrained and who therefore occupy the victim/payer role. This is
 *   emphatically NOT the same constraint as the neoliberal_convertibility
 *   reading (which treats government intervention as the constrained party
 *   and free capital markets as the protected good) or the
 *   sovereignty_defense reading (which frames the constraint as protecting
 *   monetary sovereignty per se, a related but distinct beneficiary logic
 *   centered on the state as sovereign rather than the state as
 *   employment-policy vehicle). All three are separate constraint stories
 *   sharing one kernel — the Bretton Woods treaty substrate — and are linked
 *   via network.affects_constraints, not merged into one ambiguous ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.28).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.28).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'ab145ec1-6f51-4c79-a263-19065930c3be').
narrative_ontology:cs_kernel_codification('ab145ec1-6f51-4c79-a263-19065930c3be', formalized).
narrative_ontology:cs_authority_grounding('ab145ec1-6f51-4c79-a263-19065930c3be', lineage).
narrative_ontology:cs_interpretation_layer_present('ab145ec1-6f51-4c79-a263-19065930c3be').
narrative_ontology:cs_reading_relation('ab145ec1-6f51-4c79-a263-19065930c3be', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('ab145ec1-6f51-4c79-a263-19065930c3be', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('ab145ec1-6f51-4c79-a263-19065930c3be', foundational, domestic_full_employment_policy_takes_precedence_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_full_employment_policy_takes_precedence_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('ab145ec1-6f51-4c79-a263-19065930c3be', domestic_full_employment_policy_takes_precedence_over_capital_mobility, empirically_contingent).
narrative_ontology:cs_axiom('ab145ec1-6f51-4c79-a263-19065930c3be', secondary, capital_controls_are_legitimate_coordination_tools_not_market_distortions).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_coordination_tools_not_market_distortions, holdable).
narrative_ontology:cs_axiom_grounding('ab145ec1-6f51-4c79-a263-19065930c3be', capital_controls_are_legitimate_coordination_tools_not_market_distortions, instrumental).
narrative_ontology:cs_reference_frame('ab145ec1-6f51-4c79-a263-19065930c3be', keynesian_postwar_settlement).
narrative_ontology:cs_drift_state('ab145ec1-6f51-4c79-a263-19065930c3be', post_1973_liberalization_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ab145ec1-6f51-4c79-a263-19065930c3be', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_constituencies).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_reconstruction_planners).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_currency_traders).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_portfolio_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain the ability to run full-employment fiscal and monetary policy, manage exchange rates, and pursue welfare-state build-out without immediate capital flight punishing the attempt. They negotiated and enforce the Article VIII/XIV capital-control permissions and administer exchange controls domestically. Their exit from the arrangement would mean surrendering the policy autonomy the system exists to protect.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter).

% Benefit from governments' freedom to run counter-cyclical policy, maintain full employment targets, and build social insurance without hot money flows forcing austerity. Their leverage exists only because capital cannot easily discipline the government that answers to them; they have no independent exit and depend entirely on the national policy space the constraint preserves.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_constituencies, beneficiary,
    organized, generational, constrained, national).

% Designed the adjustable peg plus capital control system (White, Keynes, and delegates) explicitly to prevent the interwar pattern of speculative attacks forcing deflationary policy on democracies. They administer the IMF's Article VIII/XIV permissions and treat capital mobility as the problem to be solved, not the freedom to be protected.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_reconstruction_planners, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_reconstruction_planners, agenda_setter).

% Denied the ability to move capital freely across borders to arbitrage interest-rate and currency differentials; capital controls are erected specifically to block this activity. From this seat, legitimate portfolio and lending activity is treated as a systemic threat to be contained rather than as productive intermediation, and the controls are enforced through licensing, currency rationing, and criminal exchange-control statutes.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance, payer,
    powerful, biographical, trapped, global).

% Cannot execute the kind of destabilizing bets against pegged currencies that characterized the 1920s-30s gold-exchange collapses, because exchange control regimes and capital account restrictions foreclose the transactions. They bear the cost of a system explicitly designed to eliminate their historical function.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_currency_traders, payer,
    powerful, immediate, trapped, global).

% Face licensing requirements, currency conversion limits, and repatriation restrictions on capital they would otherwise deploy across borders. Some legitimate diversification and investment activity is caught in the same net erected against speculative flows, and there is no appeal mechanism within the treaty framework for distinguishing the two from the investor's side.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_portfolio_investors, payer,
    powerful, biographical, constrained, global).

% Nations running persistent surpluses (the analogue to postwar Switzerland or later Germany/Japan) would have preferred a system placing more adjustment burden on debtor deficit countries; the Keynesian reading's scarce-currency clause and symmetric-adjustment logic were negotiated against their preferences, and their objections were substantially overridden at Bretton Woods itself.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, gold_bloc_creditor_nations, excluded,
    institutional, generational, constrained, national).

% Study the interwar gold standard's deflationary collapse and the Bretton Woods architects' explicit intent to prevent capital mobility from re-imposing that discipline; they assess whether the embedded-liberalism compromise achieved sustained growth with policy autonomy or merely deferred the underlying tension until the system's 1971-73 collapse.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a system in which national governments retain sufficient policy autonomy — over interest rates, exchange rates, and fiscal stance — to pursue full employment and welfare-state construction, by using treaty-sanctioned capital controls to prevent speculative capital flows from disciplining those choices before democratic processes can validate or correct them.
% TRANSFER_FUNCTION: Moves the ABILITY TO MOVE CAPITAL FREELY from international finance and cross-border investors to national governments and, through them, to domestic labor constituencies who gain policy space; the transfer is of optionality and leverage, not of money directly — international finance loses the arbitrage and disciplining power it held under the interwar gold-exchange standard.
% ABSENT_VOICES: Colonial and newly-independent Global South states had minimal seats at Bretton Woods itself and are not modeled here as primary parties, though the embedded-liberalism compromise was designed around advanced-economy welfare states; gold-bloc creditor nations objected to the adjustment asymmetry and were substantially overridden. International finance's own preferred framing (free capital mobility as itself a coordination good) is excluded from this reading by construction — see the sovereignty_defense and neoliberal_convertibility sibling readings for how that objection is modeled.
% DISAPPEARANCE_RATIONALE: If the capital-control permissions vanished overnight (as effectively happened after 1971-73 and accelerated through the 1980s-90s liberalization), governments would face immediate hot-money discipline on fiscal and monetary choices; the welfare-state expansions and full-employment commitments of the 1945-71 period would become substantially harder to sustain, which is exactly what the embedded-liberalism literature argues happened after the system's collapse.
% FOUNDING_PROBLEM: The interwar gold standard's fixed exchange rates plus free capital mobility had forced deflationary austerity onto democracies during the Depression, as speculative capital flight punished any government that tried to reflate; Keynes and White sought a system preventing that specific external discipline from overriding domestic full-employment policy.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the negotiating parties (including scholars unsympathetic to Keynesian macroeconomics) broadly corroborate that capital mobility constrained interwar policy autonomy and that Bretton Woods architects designed controls specifically to prevent recurrence; whether the underlying problem remains live post-1973 (given subsequent capital account liberalization and its consequences) is exactly the subject of ongoing dispute between the sibling readings of this same kernel.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate (0.28-0.35, dipping in the high-growth 1950s-60s and rising again as the system strained after 1965) because, from this reading's own lights, the constraint on capital is a coordination cost imposed to protect a genuine collective good (policy autonomy for democratic welfare states), not a rent extracted for its own sake — but it is still a real cost borne by international finance, hence non-zero. Suppression is moderate-high (0.38-0.50) because capital controls require active enforcement (exchange licensing, currency rationing, criminal penalties for evasion) and did meet real resistance from finance throughout the period, particularly intensifying as Euromarket workarounds proliferated in the 1960s. Theater ratio rises sharply near the end of the interval (0.28 at 1971) reflecting the well-documented decay of enforcement credibility just before collapse — controls were increasingly performative as the dollar-gold link buckled, then the interval closes in 1973 at a lower value reflecting system termination rather than resolved function.
 *
 * PERSPECTIVAL GAP:
 *   From the national-government seat, this reads as rope: a coordination mechanism enabling a genuine collective good (stable full-employment democracies) that the interwar system had made impossible. From the international-finance seat, the same structural facts read as a tangled_rope or worse: a coordination story (financial stability) used as cover to permanently exclude them from otherwise-legitimate cross-border allocation activity. This reading authors the claim from the beneficiary seat's own logic (rope) while authoring metrics that are honest about the enforcement and resistance the arrangement required — the divergence between the two is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and domestic labor constituencies are declared beneficiaries because the coordination good (retained policy space) flows to them structurally, not incidentally; their d sits near the beneficiary end. International finance, speculative traders, and portfolio investors are declared victims/payers because the specific freedom the system exists to deny is theirs; their d sits near the target end, amplified by the trapped exit_options (capital account restrictions foreclosed exactly the arbitrage moves that would let them route around the constraint). Gold-bloc creditor nations are marked excluded rather than victim because their objection was to adjustment asymmetry within the beneficiary coalition, not to the capital-control function itself — a structurally distinct grievance from finance's exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than resolved-dead because whether interwar-style capital-flow discipline remains a live threat to democratic policy autonomy is precisely the question dividing this reading from its neoliberal sibling — this reading holds the problem (capital flight punishing full-employment policy) never disappeared and was merely unmasked by liberalization after 1973, while the sibling readings hold the constraint outlived any legitimate function well before its 1971-73 collapse. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is intentional: this reading asserts the arrangement's removal mattered in the real world (supporting its coordination claim) precisely because it denies the problem the arrangement solved is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_protectionism_boundary,
    'Was the capital-control regime a genuine solution to a real collective-action problem (preventing destabilizing hot-money flows from forcing deflationary policy on democracies), or was it protectionism for domestic political coalitions dressed in the language of financial stability?',
    'Comparative analysis of capital-control-era growth and volatility outcomes against the post-1973 liberalized-capital era, controlling for the oil shocks and other confounds; examination of whether controls were relaxed asymmetrically to favor incumbent domestic industries versus applied even-handedly to genuinely destabilizing flows.',
    'If the controls demonstrably suppressed productive cross-border investment alongside destabilizing speculation without a clean separation mechanism, the rope claim weakens toward tangled_rope even within this reading''s own framework; if the controls can be shown to have targeted destabilizing flows specifically, the rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_protectionism_boundary, empirical, 'Whether capital controls solved a genuine coordination problem or served as cover for protecting domestic political coalitions from competition.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is there a single fact about the Bretton Woods treaty substrate that would adjudicate between the keynesian_embedded_liberalism, neoliberal_convertibility, and sovereignty_defense readings, or are all three permanently underdetermined by the same treaty text and historical record?',
    'This is likely conceptual rather than empirical: the same Article VI/XIV provisions can be read as protecting policy space (this reading), protecting capital markets (sibling reading), or protecting sovereignty per se (sibling reading) depending on which party''s welfare function is taken as primary. No additional historical evidence resolves which welfare function the treaty ''really'' served, because the negotiators themselves (Keynes vs. White) held different theories of what was being protected.',
    'If irreducibly underdetermined, all three readings remain permanently live and the network edges between them document contest rather than convergence — consistent with the coexists_with relations declared in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel''s contested readings could ever be resolved to one canonical structural fact, or whether the underdetermination is permanent.').

omega_variable(
    beneficiary_coalition_internal_asymmetry,
    'Within the beneficiary coalition (national governments, domestic labor), did the policy-space gains actually flow to labor constituencies, or were they substantially captured by domestic industrial and financial elites who used the same capital controls for their own protection?',
    'Distributional analysis of postwar welfare-state expansion and wage-share data across Bretton Woods-era economies, disaggregated by whether capital-control regimes correlate with labor-share gains or with domestic elite rent extraction.',
    'If labor''s beneficiary status was substantially diluted by elite capture within the same national coalition, the beneficiary set authored here (national_governments, domestic_labor_constituencies) may need to be split into a further decomposed story distinguishing state-capture dynamics from genuine labor benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_coalition_internal_asymmetry, empirical, 'Whether the domestic beneficiaries of policy space were labor broadly or narrower domestic elite coalitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.15).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1958, 0.26).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1965, 0.27).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.3).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.44).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, sovereignty_defense).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the single natural-language label 'Bretton Woods creates constraints on international capital/government/monetary sovereignty' per the ε-invariance principle. keynesian_embedded_liberalism (this story) authors capital controls as coordination protecting domestic full-employment policy, with international finance as victim and national governments/labor as beneficiary, claimed_type rope. neoliberal_convertibility inverts the beneficiary/victim assignment, treating government intervention as the constrained party. sovereignty_defense centers national monetary sovereignty as the protected good rather than employment policy specifically. All three share the same treaty text and 1944-1973 interval but are structurally distinct constraints with different ε, different parties, and potentially different claimed_type — they are linked here rather than merged because measuring 'the same constraint' three different ways produced three different extraction values, which per the ε-invariance test means three constraints, not one with three observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
