% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 — Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the conditional_reciprocity_reading of the
 *   geneva_conventions_1949 kernel: the Conventions understood as a
 *   reciprocal bargain whose protections apply fully only against compliant
 *   adversaries, with Article 4 criteria gating combatant status and
 *   proportionality calculation mediating civilian immunity. Under this
 *   reading the arrangement imposes a moderate constraint on state violence
 *   between regular forces while channeling its costs onto actors who cannot
 *   meet the conditions of protection — irregular fighters whose viable
 *   tactics disqualify them, detainees classified outside prisoner-of-war
 *   status, and civilians whose immunity is narrowed by necessity balancing.
 *   The epsilon referent is the standing arrangement under contest: the
 *   conditional-protection regime as actually administered, assessed by this
 *   reading's own lights — the reading regards conditionality itself as
 *   legitimate bargain enforcement, and its epsilon reflects the extraction
 *   it concedes (status-stripped detention, proportionality residue) rather
 *   than the conditionality it endorses. The colloquial label 'the Geneva
 *   Conventions' decomposes into three structurally distinct constraints —
 *   this reading, the humanitarian_ceiling_reading (absolute unconditional
 *   minimums), and the security_maximization_reading (necessity-suspension) —
 *   linked through network.affects_constraints; each carries its own epsilon,
 *   victim set, and classification.
 *
 * KEY AGENTS:
 *   - - great_power_signatories: Primary agenda-setter and beneficiary (institutional/arbitrage) — draft, interpret, and administer the reciprocity conditions; collect discretion and legitimacy
 *   - - regular_state_armed_forces: Primary beneficiary (organized/constrained) — receive full POW protections and reciprocal restraint in exchange for disciplinary obligations
 *   - - irregular_forces: Primary target (moderate/trapped) — structurally unable to meet Article 4 criteria without abandoning viable tactics
 *   - - detained_unlawful_combatants: Acute target (powerless/trapped) — held outside protected status with no procedural standing
 *   - - civilians_in_conflict_zones: Diffuse target (powerless/trapped) — formal immunity subordinated to attacker-run proportionality balancing
 *   - - military_judge_advocates: Operational administrator (organized/constrained) — apply the reading case by case; their interpretations accumulate into doctrine
 *   - - icrc_delegates: Analytical observer (institutional/analytical) — monitor the full structure from inside under access mandates
 *   - - humanitarian_law_advocates: Excluded voice (organized/constrained) — contest conditionality from outside the state-dominated interpretation process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.52).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.57).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '619e7e10-c6bd-46d3-b889-c71217f8b488').
narrative_ontology:cs_kernel_codification('619e7e10-c6bd-46d3-b889-c71217f8b488', fixed_text).
narrative_ontology:cs_authority_grounding('619e7e10-c6bd-46d3-b889-c71217f8b488', lineage).
narrative_ontology:cs_interpretation_layer_present('619e7e10-c6bd-46d3-b889-c71217f8b488').
narrative_ontology:cs_reading_relation('619e7e10-c6bd-46d3-b889-c71217f8b488', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('619e7e10-c6bd-46d3-b889-c71217f8b488', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('619e7e10-c6bd-46d3-b889-c71217f8b488', foundational, protection_conditioned_on_adversary_compliance).
narrative_ontology:cs_axiom_status(protection_conditioned_on_adversary_compliance, holdable).
narrative_ontology:cs_axiom_grounding('619e7e10-c6bd-46d3-b889-c71217f8b488', protection_conditioned_on_adversary_compliance, conventional).
narrative_ontology:cs_axiom('619e7e10-c6bd-46d3-b889-c71217f8b488', secondary, combatant_status_requires_article_four_discipline).
narrative_ontology:cs_axiom_status(combatant_status_requires_article_four_discipline, holdable).
narrative_ontology:cs_axiom_grounding('619e7e10-c6bd-46d3-b889-c71217f8b488', combatant_status_requires_article_four_discipline, conventional).
narrative_ontology:cs_reference_frame('619e7e10-c6bd-46d3-b889-c71217f8b488', reciprocal_compliance_bargain).
narrative_ontology:cs_drift_state('619e7e10-c6bd-46d3-b889-c71217f8b488', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('619e7e10-c6bd-46d3-b889-c71217f8b488', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, great_power_signatories).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_armed_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, article_4_combatant_criteria).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, proportionality_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocity_incentive_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the 1949 Conventions and dominate their interpretation through military doctrine, reservations, and diplomatic practice. They drafted the Article 4 criteria that gate combatant status, operate the detention and prosecution machinery that applies them, and decide when adversary conduct triggers degraded application. Their own soldiers receive full prisoner-of-war protections in return; their compliance costs are real but are priced against discretion over how the rules bind weaker adversaries. Exit is effectively open: they can reinterpret, reserve against, or quietly depart from provisions without forfeiting the framework's benefits.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, great_power_signatories, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, great_power_signatories, beneficiary).

% Serve in uniformed, hierarchically commanded forces that meet the Article 4 criteria by construction. The framework guarantees them prisoner-of-war status if captured and reciprocal restraint from opposing regular forces; in return they accept disciplinary obligations under their own military justice systems. Their protection depends on the framework persisting, so exiting is not a realistic option even where compliance chafes.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_armed_forces, beneficiary,
    organized, biographical, constrained, global).

% Fight without uniforms or centralized command structures because concealment and dispersion are their only viable tactics against stronger conventional forces. Those same traits disqualify them from prisoner-of-war status under the Article 4 criteria, so capture exposes them to prosecution or indefinite detention rather than protected custody. Meeting the criteria would mean abandoning the tactics that keep them alive; there is no available path to protection that does not require fighting in ways they would lose.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces, payer,
    moderate, immediate, trapped, regional).

% Are in custody after capture, classified outside prisoner-of-war status. They face interrogation regimes, military commissions, or prolonged internment, with limited access to the visiting and registration mechanisms that protected-status detainees receive. Their situation is determined entirely by decisions made by the capturing state; they have no procedural standing to contest their classification in most venues.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_unlawful_combatants, payer,
    powerless, immediate, trapped, regional).

% Live where proportionality calculations are made about their neighborhoods. The reading preserves their formal immunity while subordinating it to military-necessity balancing performed by the attacking party; they bear the residual harm when the balance tips. They have no seat in the calculation, no exit from besieged areas in many campaigns, and their exposure compounds across generations through displacement and unexploded ordnance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones, payer,
    powerless, generational, trapped, regional).

% Advise commanders on targeting, detention, and status determinations, translating the conventions into rules of engagement and capture policies case by case. They hold real discretion over how conditionality is applied in practice, exercised within career structures that reward institutional loyalty. Their interpretations accumulate into the working meaning of the reading without ever passing through formal treaty revision.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, military_judge_advocates, agenda_setter,
    organized, biographical, constrained, national).

% Visit detention facilities, register prisoners, and broker compliance communications between belligerents under the conventions' access mandates. They see the full structure from inside — who is protected, who is not, and what degradation looks like on the ground — and report confidentially to the parties rather than publicly adjudicating.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, icrc_delegates, observer,
    institutional, generational, analytical, global).

% Organizations and scholars who argue that protections must not depend on adversary compliance and that the Article 4 gate strips fighters of guaranteed rights. They litigate, publish, and lobby treaty bodies, but hold no vote in the state-dominated interpretation process; their objections enter the record without entering the decision.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_law_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, great_power_signatories).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mutual restraint in war between disciplined forces: each side limits its violence knowing its own captured soldiers will receive protected custody, producing predictable treatment of prisoners and wounded, credible surrender incentives, and reduced escalation spirals between regular armies.
% TRANSFER_FUNCTION: Moves protection and legitimacy from irregular fighters and conflict-zone civilians (whose safeguards are narrowed by status conditionality and proportionality balancing) toward regular state forces and the states interpreting the rules; moves discretion over life-and-death determinations to the party running the compliance and proportionality assessments.
% ABSENT_VOICES: Irregular forces and conflict-zone civilians had no seat at the 1949 drafting conference, which was composed of states; contemporary humanitarian organizations and affected communities object to conditionality but sit outside the state-dominated treaty-interpretation process where the reading is maintained.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity structure vanished overnight, the entire architecture of status determination, detention regimes, prisoner exchanges, and proportionality review would lose its organizing vocabulary; surrender incentives between regular armies would weaken, and every ongoing asymmetric conflict would face an ungoverned vacuum in which neither degradation nor protection had a settled legal meaning.
% FOUNDING_PROBLEM: The mass abuse of prisoners, partisan reprisals, and denials of quarter documented in the Second World War demanded codified, mutually binding rules protecting captured and wounded combatants and civilian populations.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's official commentaries, the Nuremberg and subsequent tribunal records, and postwar diplomatic correspondence corroborate the founding problem from outside any single benefiting state. Humanitarian organizations corroborate that the problem remains live in ongoing conflicts while disputing that conditionality is a remedy for it; no corroboration exists for the claim that the founding problem is confined to the interstate wars the bargain was designed for.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.52 at interval end) because the arrangement's costs concentrate on actors with no route to protected status: the Article 4 gate converts tactical necessity into legal disability, and proportionality balancing leaves civilians bearing the residual of decisions they never participate in. Suppression (0.57) tracks the enforcement machinery the conditionality requires — status determination boards, interrogation and internment regimes, military commissions — which must actively hold the classification line against captured fighters claiming protection. Theater is moderate-low (0.31): compliance review, ICRC visitation, and military justice are substantially functional in interstate conflicts, but a growing share of activity consists of legitimating performance (tribunals and findings that dress degradation as adjudication). Accessibility collapse is 0.48: alternatives persist and are invoked — Common Article 3 floors, Additional Protocol I's relaxed criteria, the Martens Clause, customary-law arguments — but accepting the reciprocity frame makes unconditional protection look strategically naive, collapsing much of the practical alternative space. Resistance is 0.6: sustained doctrinal contestation by the ICRC, humanitarian organizations, and legal scholars, plus periodic state-level rejection. The temporal series run on one shared eight-point grid (1949–2024) across all three tracked metrics. The trajectory is cyclical rather than monotonic: wartime crises spike extractiveness and enforcement (Korea's POW repatriation disputes, Vietnam-era status controversies, the post-2001 detention expansion), followed by partial post-crisis relaxation (1977 Protocols, the 1991 high-compliance interlude, post-2014 normalization). The oscillation is partly the extraction mechanism itself — each crisis normalizes a degradation that persists into the relaxed phase, so the baseline ratchets upward (1949 trough 0.35, 2024 resting point 0.52, well above trough despite two relaxation cycles). Endpoint values match the base_properties scalars; the series ends in a post-peak relaxation phase, not at equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute divergent types from identical text. From the great-power and regular-forces seats the arrangement is a legitimate bargain they administer and profit from: restraint purchased at prices they set, applied against adversaries they assess. From the irregular, detainee, and civilian seats the same structure operates as enforced exclusion — a protection regime whose entry conditions they cannot meet and whose balancing decisions they cannot contest. Military judge advocates occupy a hinge position: they experience the reading as professional craft while manufacturing the discretionary applications the payer seats experience as extraction. The engine computes these per-seat classifications from the structural data; the divergence between seats is the measurement, not an artifact to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Regular state armed forces sit near the beneficiary pole (protected status, reciprocal restraint, constrained exit — they cannot leave the framework that shields them). Irregular forces, detained unlawful combatants, and civilians sit near the target pole, with detainees highest: trapped exit plus total dependence on captor-side classification maximizes their effective extraction. Civilians carry diffuse but compounding exposure across a generational horizon. One override is declared: great_power_signatories derive a near-beneficiary directionality from their beneficiary declaration, but as agenda-setters they also bear real compliance costs and troop risk, and their position is net-beneficiary rather than pure subsidy recipient — the override sets d to 0.22 to reflect that they collect the arrangement's gains while absorbing part of its price. Judge advocates and ICRC delegates hold no beneficiary or victim declaration; the former administers, the latter observes, and both fall to their power-atom fallbacks rather than being forced into the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — WWII-scale prisoner abuse and reprisal — remains live in some conflicts, but the conditionality mechanism now operates predominantly in asymmetric conflicts the 1949 bargain never contemplated, where the reciprocal-incentive logic that justified it barely engages (irregular groups were never positioned to offer reciprocal full compliance in the first place). Authoring founding_problem_status as contested rather than dead prevents two misclassifications: reading the arrangement as pure coordination ignores that its extraction half now dominates its operating environment; reading it as pure extraction discards the genuine interstate restraint that still functions when two regular armies meet. The status x disappearance mismatch consumer finds no zombie flag here (contested x world_rearranges), but the interstate_bargain_atrophy omega tracks the drift path: if the coordination half survives only in conflicts the reading barely governs, the arrangement is en route to piton — maintained theatrically by doctrine and commemoration while its operative content narrows to degradation administration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the conditional_reciprocity_reading of kernel geneva_conventions_1949; what structural differences would appear under the humanitarian_ceiling_reading instead?',
    'Comparative enumeration: determine which protections (POW status, civilian immunity, detention safeguards) survive adversary non-compliance under each reading and measure the resulting victim-set delta.',
    'Under the humanitarian ceiling reading the victim sets shrink sharply — irregulars retain floor protections unconditionally and detainees regain registered custody — and epsilon drops well below the value authored here. The two readings are different constraints, not one constraint measured two ways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which sibling reading of the Geneva kernel is instantiated determines the victim set and epsilon.').

omega_variable(
    reciprocity_verification_asymmetry,
    'Is adversary compliance — the trigger condition for full application — assessed symmetrically by the parties, or do states alone run the compliance assessments that degrade irregulars'' protections?',
    'Audit status-determination and reciprocity findings across post-1949 conflicts for who assesses compliance, on what evidence, subject to what independent review.',
    'If assessment is one-directional, the bargain degenerates toward one-way degradation: effective extraction on irregulars rises beyond the authored value and the constraint drifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_verification_asymmetry, empirical, 'Whether the reciprocity condition is administered symmetrically or as unilateral state discretion.').

omega_variable(
    proportionality_erosion_boundary,
    'Does proportionality calculation, as practiced under this reading, preserve meaningful civilian immunity or systematically widen acceptable collateral harm?',
    'Cross-campaign casualty-composition studies and targeting-board records compared against the collateral-damage estimates stated at the time of each operation.',
    'Systematic widening raises the civilian share of the extraction burden, raising epsilon and strengthening the extraction half of the tangled_rope classification; a stable boundary supports the reading''s claim that immunity is preserved though narrowed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_erosion_boundary, empirical, 'Whether proportionality functions as a real limit or a ratchet on civilian exposure.').

omega_variable(
    security_maximization_boundary,
    'Where does this reading''s permitted proportional degradation end and the security_maximization_reading''s necessity-suspension begin — is that boundary stable, or does each conditionality concession move it?',
    'Track doctrinal slippage across conflicts: catalog protections degraded under reciprocity claims versus protections suspended under necessity claims, and test whether the two categories remain distinct in operational practice.',
    'If the boundary erodes, this reading functions as the transmission mechanism to the security-maximization sibling and its downstream influence edge hardens into dependency; if stable, the two readings remain separable constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_maximization_boundary, conceptual, 'Stability of the line between conditional degradation and outright suspension of protections.').

omega_variable(
    interstate_bargain_atrophy,
    'Does the reciprocal bargain still bind great-power conduct in interstate war, or does it now operate mainly as a framework for degrading irregulars in asymmetric conflicts?',
    'Compare compliance behavior in interstate engagements versus asymmetric campaigns on prisoner treatment, surrender acceptance, and targeting restraint across the measurement interval.',
    'If the coordination half survives only in conflicts the reading barely governs, the arrangement''s mandate has outlived its operating environment — reinforcing the contested founding-problem status and marking a drift path toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_bargain_atrophy, empirical, 'Whether the coordination function retains force in the conflicts the bargain was built for.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_cond_recip_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t1949, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t1955, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t1955, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t1968, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t1968, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t1977, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t1977, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t1991, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t1991, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t2001, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t2014, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2014, 0.34).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t2014, observed).
narrative_ontology:measurement(geneva_cond_recip_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(geneva_cond_recip_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(geneva_cond_recip_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t1949, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t1955, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t1955, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t1968, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t1968, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t1977, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t1977, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t1991, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1991, 0.4).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t1991, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t2001, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t2014, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t2014, observed).
narrative_ontology:measurement(geneva_cond_recip_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(geneva_cond_recip_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_cond_recip_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t1949, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t1955, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1955, 0.38).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t1955, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t1968, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t1968, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t1977, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t1977, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t1991, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1991, 0.42).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t1991, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t2001, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t2014, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t2014, observed).
narrative_ontology:measurement(geneva_cond_recip_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.57).
narrative_ontology:measurement_basis(geneva_cond_recip_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Geneva Conventions' conflates three structurally distinct constraints instantiated by three readings of one kernel. The humanitarian_ceiling_reading is upstream: its unconditional-floor claims are the standard cited by critics of conditionality and supply the normative baseline against which this reading's degradations are measured. This conditional_reciprocity_reading exerts downstream influence on the security_maximization_reading: each accepted concession to conditionality lowers the legitimacy barrier for necessity-based suspension, supplying the conceptual bridge (non-compliance justifies degradation) that the security reading radicalizes. The three stories carry different epsilons, different victim sets, and different failure modes; they are linked through affects_constraints rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
