% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Sovereignty-Defense Reading (Asymmetric Monetary Discipline)
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_defense reading of the
 *   contested bretton_woods_treaty_substrate kernel. It holds that the
 *   Bretton Woods conference created constraints on external monetary
 *   discipline to preserve national monetary sovereignty. The structural
 *   delta of this reading is asymmetric: non-reserve-currency states enter
 *   the victim set as bearers of adjustment discipline, the United States
 *   enters the beneficiary set via exorbitant privilege, and the gold anchor
 *   functions as a snare rather than a neutral stabilizer. The constraint is
 *   claimed as tangled_rope because a genuine coordination function (fixed
 *   exchange rates, trade payments clearing) is inseparable from the
 *   asymmetric extraction embedded in the reserve-currency hierarchy.
 *
 * KEY AGENTS:
 *   - us_reserve_currency_issuer (institutional/arbitrage): agenda-setter and primary beneficiary â sets the nominal anchor and collects seigniorage and deficit-financing exemption.
 *   - non_reserve_currency_states (organized/constrained): primary payer â bears balance-of-payments adjustment burdens and IMF conditionality.
 *   - imf_administration (institutional/constrained): secondary agenda-setter â administers and legitimates the asymmetric rules.
 *   - decolonizing_economies (powerless/trapped): excluded voice â subjected to the order without having shaped it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.72).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Sovereignty-Defense Reading (Asymmetric Monetary Discipline)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '78ee5ce7-ef01-435f-9607-36fe70499c61').
narrative_ontology:cs_kernel_codification('78ee5ce7-ef01-435f-9607-36fe70499c61', formalized).
narrative_ontology:cs_authority_grounding('78ee5ce7-ef01-435f-9607-36fe70499c61', lineage).
narrative_ontology:cs_interpretation_layer_present('78ee5ce7-ef01-435f-9607-36fe70499c61').
narrative_ontology:cs_reading_relation('78ee5ce7-ef01-435f-9607-36fe70499c61', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('78ee5ce7-ef01-435f-9607-36fe70499c61', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('78ee5ce7-ef01-435f-9607-36fe70499c61', foundational, reserve_currency_exemption_from_discipline).
narrative_ontology:cs_axiom_status(reserve_currency_exemption_from_discipline, holdable).
narrative_ontology:cs_axiom_grounding('78ee5ce7-ef01-435f-9607-36fe70499c61', reserve_currency_exemption_from_discipline, empirically_contingent).
narrative_ontology:cs_axiom('78ee5ce7-ef01-435f-9607-36fe70499c61', foundational, gold_anchor_non_reserve_snare).
narrative_ontology:cs_axiom_status(gold_anchor_non_reserve_snare, holdable).
narrative_ontology:cs_axiom_grounding('78ee5ce7-ef01-435f-9607-36fe70499c61', gold_anchor_non_reserve_snare, empirically_contingent).
narrative_ontology:cs_reference_frame('78ee5ce7-ef01-435f-9607-36fe70499c61', westphalian_monetary_equality).
narrative_ontology:cs_drift_state('78ee5ce7-ef01-435f-9607-36fe70499c61', bw_operational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78ee5ce7-ef01-435f-9607-36fe70499c61', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, dollar_gold_standard_legitimacy).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, imf_multilateral_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the supply of the primary reserve asset (dollar-gold peg) and sets the nominal anchor for the system. Can finance external deficits in its own currency without undertaking the austerity imposed on deficit states. Unilaterally suspended gold convertibility in 1971, exercising arbitrage-grade exit from the constraint's own discipline while others remained bound.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% Must maintain external monetary discipline (peg maintenance, reserve accumulation, IMF standby compliance) to access dollar liquidity and trade credit. Bear the full burden of balance-of-payments adjustment through deflation and austerity, while the reserve issuer expands deficits. Exit is constrained by the dollar shortage, trade dependence, and the absence of viable alternative reserve assets.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    organized, generational, constrained, global).

% Administers the Articles of Agreement, approves exchange-rate parities, and operationalizes conditionality on debtor states. Presents the arrangement as multilateral cooperation and technocratic neutrality, while enforcing the asymmetric adjustment rules that privilege the reserve-currency issuer.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_administration, agenda_setter,
    institutional, generational, constrained, global).

% Excluded from the 1944 Bretton Woods drafting table. Subjected to the monetary order's discipline through inherited colonial currency boards and post-independence IMF programs without having shaped the rules. Their monetary policy was managed by colonial offices or successor institutions that locked them into the dollar-gold nexus without sovereign central-bank capacity.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, decolonizing_economies, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multilateral fixed-but-adjustable exchange-rate system to enable postwar trade and investment by reducing currency uncertainty and providing a nominal anchor.
% TRANSFER_FUNCTION: Transfers the burden of external adjustment from the reserve-currency issuer to non-reserve states, while transferring seigniorage, deficit-financing capacity, and structural exemption from discipline to the reserve-currency issuer.
% ABSENT_VOICES: Decolonizing economies were excluded from the 1944 drafting. Later, deficit states facing IMF structural adjustment had limited voice relative to creditor states and the Fund bureaucracy. Rival monetary blocs and regional clearing-union advocates were sidelined.
% DISAPPEARANCE_RATIONALE: If the BW constraint on external monetary discipline vanished, fixed exchange-rate parities would dissolve, the dollar's exclusive reserve role would end, and non-reserve states would face different (not necessarily lighter) adjustment pressures. The postwar trade and payments system was organized around this architecture; its disappearance would force a global monetary reordering.
% FOUNDING_PROBLEM: Prevent a return to the competitive devaluations, currency blocs, and trade protectionism of the 1930s that destroyed international commerce and monetary stability.
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White attested the competitive-devaluation problem from the British and US drafting seats. Subsequent historical political economy from outside the US beneficiary seat â Helleiner, Eichengreen, and the dependencia school â contests whether the BW architecture solved this problem or merely relocated it into an asymmetric dollar-gold hierarchy that reproduced 1930s-style coercion against non-reserve peripheries.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the structural asymmetry where non-reserve states bore the full burden of external adjustment (deflation, austerity, IMF conditionality) while the reserve-currency issuer financed deficits without equivalent discipline. Suppression (0.68) reflects the IMF enforcement machinery, capital-control regimes that trapped states in the dollar-gold nexus, and the exclusion of alternative reserve or clearing arrangements. Theater ratio (0.42) captures the widening gap between multilateral-cooperation rhetoric and the actual asymmetric operation of the system. Accessibility collapse (0.65) records how viable alternatives (bilateral clearing, regional monetary unions, autarkic development) collapsed as the BW architecture became the sole legitimate global framework. Resistance (0.52) reflects French gold-recall challenges, UK sterling crises, and peripheral defaults, which were substantial but insufficient to reform the core asymmetry. The temporal series shows extraction and theater rising monotonically through the operational era as the dollar shortage transitioned to a dollar glut, while suppression peaked in the late 1960s and softened only with the system's collapse.
 *
 * PERSPECTIVAL GAP:
 *   The US Treasury/Federal Reserve seat experiences the constraint as a global public good that the US underwrites; from this position the system is coordination with incidental benefits. The non-reserve-currency state seat experiences the same rules as enforced extraction â they must deflate to maintain parity while the US does not. The IMF administrative seat experiences technocratic neutrality. These divergences are structural: the reserve issuer has arbitrage-grade exit (unilateral suspension of convertibility, as executed in 1971), while non-reserve states are constrained or identity-locked into the sovereign-debtor role.
 *
 * DIRECTIONALITY LOGIC:
 *   us_reserve_currency_issuer is declared in beneficiaries and controls rule-setting and reserve issuance, placing it near the full-beneficiary end (low d). non_reserve_currency_states are declared in victims and lack dollar-creation capacity, placing them near the full-target end (high d). imf_administration sits near symmetric: it coordinates and enforces but does not personally collect the seigniorage surplus. decolonizing_economies are excluded from the conversation entirely, with no directional relationship to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing 1930s-style competitive devaluation and monetary nationalism â was substantially solved by the late 1950s as trade and payments normalized. Yet the constraint persisted and intensified through the 1960s because the reserve-currency issuer captured significant rents (seigniorage, deficit financing) and the IMF bureaucracy had institutional stakes in the arrangement's continuity. The persistence after the founding problem's death, combined with rising theater and extraction, indicates mandatrophy: the constraint became a vehicle for extraction dressed in the language of its obsolete founding mission. The T17 abductive trigger would fire on the rising extraction trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Bretton Woods order best understood as a sovereignty-preserving coordination mechanism or as an asymmetric extraction structure privileging the reserve currency issuer?',
    'Comparative analysis of adjustment burdens across reserve and non-reserve states during the BW era; examination of the Triffin dilemma and US balance-of-payments behavior.',
    'If the asymmetry is structurally inherent, the sovereignty_defense reading is validated as tangled_rope; if the asymmetry is contingent on US policy choices, the kernel may be closer to the keynesian_embedded_liberalism reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether BW was symmetric coordination or asymmetric extraction').

omega_variable(
    gold_anchor_mechanism,
    'Did the gold-exchange standard function as a stabilizing nominal anchor or as a liquidity snare that forced non-reserve states into deflationary adjustment?',
    'Archival analysis of IMF standby arrangements and national central bank policies under BW; compare inflation/deflation outcomes across reserve and non-reserve states.',
    'If gold scarcity systematically forced austerity on non-reserve states while the reserve issuer expanded deficits, the constraint''s extractiveness is higher than the coordination framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_mechanism, empirical, 'Whether the gold anchor was stabilizer or asymmetric snare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_sovereignty_defense_tr_t0, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t5, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 5, 0.25).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t10, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 10, 0.3).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t15, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 15, 0.35).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t20, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 20, 0.4).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t25, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 25, 0.45).
narrative_ontology:measurement(bw_sovereignty_defense_tr_t30, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(bw_sovereignty_defense_be_t0, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bw_sovereignty_defense_be_t5, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bw_sovereignty_defense_be_t10, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(bw_sovereignty_defense_be_t15, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(bw_sovereignty_defense_be_t20, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(bw_sovereignty_defense_be_t25, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(bw_sovereignty_defense_be_t30, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bw_sovereignty_defense_su_t0, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bw_sovereignty_defense_su_t5, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(bw_sovereignty_defense_su_t10, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bw_sovereignty_defense_su_t15, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(bw_sovereignty_defense_su_t20, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(bw_sovereignty_defense_su_t25, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(bw_sovereignty_defense_su_t30, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bretton_woods_treaty_substrate kernel, decomposed per the epsilon-invariance principle. Sibling readings instantiate structurally distinct claims: keynesian_embedded_liberalism emphasizes domestic policy space; neoliberal_convertibility emphasizes market freedom; sovereignty_defense emphasizes asymmetric monetary discipline. The kernel label 'Bretton Woods' conflates these; they are modeled as separate constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
