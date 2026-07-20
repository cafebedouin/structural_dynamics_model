% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Technological Determinist Reading: Printing Press Caused Reformation
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinist reading of the
 *   press-Reformation kernel: the printing press functioned as an autonomous,
 *   mountain-like force that made censorship impossible and vernacular
 *   scripture inevitable. The reading treats technology as an exogenous
 *   upstream variable and reformers as passive beneficiaries of structural
 *   capacity. The Catholic Church is rendered a futile resistor against a
 *   natural-historical law. The story is authored as a mountain claim with
 *   declared beneficiaries and victims to test the false-summit detection
 *   apparatus: the TD narrative presents itself as natural law, but
 *   identifiable parties (reformers, print merchants, lay readers) benefit
 *   from its operation while others (the Church, manuscript scribes) bear
 *   concentrated costs.
 *
 * KEY AGENTS:
 *   - protestant_reformers (beneficiary - organized/mobile): Gains theological reach from mass reproducibility
 *   - catholic_hierarchy (payer - institutional/constrained): Loses information monopoly and territorial religious unity
 *   - vernacular_lay_readers (beneficiary - moderate/mobile): Gains direct scriptural access bypassing priestly mediation
 *   - print_merchants (beneficiary - moderate/mobile): Profits from demand for vernacular religious texts
 *   - manuscript_scribes (payer - powerless/trapped): Economic displacement from mechanical reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.55).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.3).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.55).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Technological Determinist Reading: Printing Press Caused Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history of technology / religious history / media studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '3d3afcae-5e9d-4071-82a6-d3329b6da0a3').
narrative_ontology:cs_kernel_codification('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', fixed_text).
narrative_ontology:cs_authority_grounding('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', expertise).
narrative_ontology:cs_interpretation_layer_present('3d3afcae-5e9d-4071-82a6-d3329b6da0a3').
narrative_ontology:cs_reading_relation('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', foundational, technological_autonomy_in_history).
narrative_ontology:cs_axiom_status(technological_autonomy_in_history, holdable).
narrative_ontology:cs_axiom_grounding('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', technological_autonomy_in_history, empirically_contingent).
narrative_ontology:cs_axiom('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', secondary, information_monopoly_collapse_inevitable).
narrative_ontology:cs_axiom_status(information_monopoly_collapse_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', information_monopoly_collapse_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', technological_imperative_governs_history).
narrative_ontology:cs_drift_state('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', post_social_history_empirical_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d3afcae-5e9d-4071-82a6-d3329b6da0a3', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_lay_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, print_merchants).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, manuscript_scribes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated as a decentralized religious movement exploiting the press's capacity for rapid vernacular pamphlet production, bypassing ecclesiastical gatekeeping to disseminate theological challenges across Europe.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Attempted to maintain doctrinal monopoly through censorship, the Index, and territorial control, but faced structurally escalating costs of suppression as print volume outpaced enforcement capacity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Gained direct access to scripture and polemical texts in local languages, reducing dependence on priestly mediation and enabling private religious interpretation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_lay_readers, beneficiary,
    moderate, biographical, mobile, regional).

% Profited from the commercial demand for Reformation pamphlets and vernacular Bibles, investing in presses and distribution networks to serve expanding literacy markets.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, print_merchants, beneficiary,
    moderate, biographical, mobile, regional).

% Saw their economic function in manual text reproduction collapse as mechanical printing undercut production costs and speed; lacked capital and training to transition into the new medium.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, manuscript_scribes, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Decentralized reproduction and distribution of uniform texts across a linguistically fragmented continent, solving the coordination problem of scalable information dissemination.
% TRANSFER_FUNCTION: Moves control over religious information from centralized ecclesiastical manuscript networks to decentralized print markets and lay readership.
% ABSENT_VOICES: Manuscript scribes and monastic copyists are structurally excluded from the historiographical narrative of progress; they would object that their displacement was economic and political, not technologically inevitable.
% DISAPPEARANCE_RATIONALE: If the press's capacity to mass-produce vernacular texts vanished, the Reformation's rapid diffusion would stall; the Church's information monopoly would reconstitute; and the economic and cultural base of print merchants and lay literacy would collapse.
% FOUNDING_PROBLEM: How to reproduce and distribute texts reliably, rapidly, and affordably across a continent with high linguistic fragmentation and low rates of institutional centralization.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book trade attest to pre-print scarcity and high marginal costs of manuscript production; the Catholic hierarchy attests that the problem was not scarcity but controlled distribution, arguing the press created the problem of uncontrolled doctrinal chaos.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is substantial because the press extracts from the Church's historical information monopoly and from scribal livelihoods, transferring capacity to reformers and commercial printers. Suppression (0.30) is moderate: the constraint suppresses the alternative of centralized manuscript control not by active coercion but by structural obsolescence and cost asymmetry. Accessibility collapse (0.88) is high because once the press is established, the manuscript alternative becomes economically non-viable at scale. Resistance (0.35) is non-trivial: the Church actively resisted through the Index and territorial censorship, signaling that the constraint is not a frictionless natural law. Theater ratio (0.25) captures the growing performative dimension of 'inevitability' rhetoric as the TD narrative solidifies. The metric/claim divergence is deliberate: the constraint claims mountain status but meets resistance and concentrates benefits, enabling FSM evaluation.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and lay-reader seats, the press appears as liberating coordination (rope-like), delivering scripture and challenging monopoly. From the Church and scribal seats, the same structure appears as extractive displacement (snare-like), destroying livelihoods and control without consent. The TD reading naturalizes the reformer seat's experience as historically inevitable, erasing the Church seat's resistance as futile. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers, vernacular readers, and print merchants are declared beneficiaries: they sit at low directionality because the constraint subsidizes their capacity and reach. The Catholic hierarchy and manuscript scribes are declared victims: they sit at high directionality because the constraint extracts from their institutional and economic position. The Church's institutional power and constrained exit (no alternative to territorial religious monopoly) amplify its effective extraction. Scribes' powerlessness and trapped exit (no skill transfer) place them near full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the genuine coordination function (text distribution) from the extraction layer (destruction of the Church's monopoly and scribal craft). The TD reading conflates these by treating both as inevitable byproducts of a mountain. The mandatrophy question asks whether the constraint's founding problem (text scarcity) is still live; by 1600 it is solved, yet the TD narrative persists as a naturalized account. The dead-problem plus world-rearranges mismatch flags potential mandatrophy: the arrangement has outlived its function but persists as explanatory ideology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'Is the printing press''s effect on the Reformation a genuine technological mountain (autonomous structural force), or a constructed narrative that benefits Protestant historiography and secular modernization theory while rendering Catholic agency invisible?',
    'Comparative historical analysis of Reformation success in regions with differential print adoption; if press density does not correlate with Reformation diffusion, the mountain claim is falsified and the constraint is a false summit.',
    'If falsified, the constraint reclassifies from mountain to tangled_rope or snare â a tool strategically exploited by reformers rather than an inevitable force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, empirical, 'Whether the press operated as autonomous cause or as instrument wielded by religious actors').

omega_variable(
    censorship_impossibility_claim,
    'Did the printing press make censorship structurally impossible, or did it merely raise the cost and scale of censorship while states and churches eventually developed effective print-control regimes?',
    'Trace subsequent history of press censorship (Index, licensing, copyright police) to determine if censorship was impossible or just temporarily disrupted.',
    'If censorship was not impossible but merely costly, the inevitability claim is overstated and the constraint''s extractiveness is lower than the TD reading suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(censorship_impossibility_claim, empirical, 'Whether censorship was rendered impossible or merely more expensive').

omega_variable(
    sibling_reading_structural_delta,
    'How would classifying this constraint under the strategic_deployment or mutual_shaping readings alter the beneficiary/victim structure and directionality assignments?',
    'Generate parallel constraint stories for sibling readings and compare directionality distributions; strategic_deployment would shift reformers from beneficiaries to agenda_setters and lower accessibility_collapse.',
    'Under alternative readings, the constraint likely classifies as tangled_rope or rope rather than mountain, revealing the TD reading as a naturalization narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural difference between technological determinism and agency-based readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__technological_determinism, theater_ratio, 20, 0.08).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.12).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causation__technological_determinism, theater_ratio, 60, 0.18).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causation__technological_determinism, theater_ratio, 80, 0.22).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__technological_determinism, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__technological_determinism, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(pres_be_t60, press_reformation_causation__technological_determinism, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(pres_be_t80, press_reformation_causation__technological_determinism, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__technological_determinism, base_extractiveness, 100, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
