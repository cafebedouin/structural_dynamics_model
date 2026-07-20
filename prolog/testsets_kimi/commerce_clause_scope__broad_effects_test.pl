% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause â Broad Effects Test Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the broad_effects_test reading of the
 *   contested commerce_clause_scope kernel. Under this reading, Commerce
 *   includes any economic activity that substantially affects interstate
 *   commerce in the aggregate; regulate includes prohibition and
 *   comprehensive control; and federal power extends to intrastate activities
 *   with cumulative national economic impact. The reading produces an
 *   expansive victim set (virtually all local economic activity) and
 *   concentrates authority in federal regulators. It is structurally distinct
 *   from the narrow_originalist reading (trade crossing lines, regulate means
 *   make regular) and the intermediate_channels reading (substantial effects
 *   subject to limiting principles). Sibling readings are modeled as separate
 *   constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - federal_regulators: agenda_setter (institutional/analytical) â administers and defends the doctrine, benefits from jurisdictional expansion
 *   - national_interest_groups: beneficiary (organized/mobile) â lobbies for uniform national policy
 *   - civil_rights_enforcers: beneficiary (organized/mobile) â relies on broad jurisdictional hook
 *   - state_governments: payer (institutional/constrained) â loses police powers and experimental autonomy
 *   - intrastate_producers: payer (moderate/constrained) â local businesses subjected to federal regulation via aggregation
 *   - state_sovereignty_advocates: excluded (organized/constrained) â argues for hard limits, structurally excluded from controlling doctrine
 *   - constitutional_originalists: observer (analytical/analytical) â documents and critiques doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.78).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.78).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.78).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause â Broad Effects Test Reading").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'd15850c5-c7c0-4122-9964-42804d0d9f1b').
narrative_ontology:cs_kernel_codification('d15850c5-c7c0-4122-9964-42804d0d9f1b', fixed_text).
narrative_ontology:cs_authority_grounding('d15850c5-c7c0-4122-9964-42804d0d9f1b', lineage).
narrative_ontology:cs_interpretation_layer_present('d15850c5-c7c0-4122-9964-42804d0d9f1b').
narrative_ontology:cs_reading_relation('d15850c5-c7c0-4122-9964-42804d0d9f1b', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('d15850c5-c7c0-4122-9964-42804d0d9f1b', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('d15850c5-c7c0-4122-9964-42804d0d9f1b', foundational, aggregate_economic_effects_suffice).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_suffice, holdable).
narrative_ontology:cs_axiom_grounding('d15850c5-c7c0-4122-9964-42804d0d9f1b', aggregate_economic_effects_suffice, conventional).
narrative_ontology:cs_axiom('d15850c5-c7c0-4122-9964-42804d0d9f1b', foundational, federal_preemption_of_state_economic_power).
narrative_ontology:cs_axiom_status(federal_preemption_of_state_economic_power, holdable).
narrative_ontology:cs_axiom_grounding('d15850c5-c7c0-4122-9964-42804d0d9f1b', federal_preemption_of_state_economic_power, conventional).
narrative_ontology:cs_reference_frame('d15850c5-c7c0-4122-9964-42804d0d9f1b', integrated_national_economy).
narrative_ontology:cs_drift_state('d15850c5-c7c0-4122-9964-42804d0d9f1b', post_sebelius_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d15850c5-c7c0-4122-9964-42804d0d9f1b', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, intrastate_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Commerce Clause to extend federal regulatory authority over intrastate economic activity through the aggregation doctrine. Defend the scope in litigation and rulemaking. Benefit from expanded jurisdictional reach, enlarged budgets, and administrative autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Lobby for uniform national standards in environmental, labor, and consumer policy that preempt state variation. Benefit from avoiding a regulatory race-to-the-bottom and from reduced compliance fragmentation across fifty state regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Rely on broad Commerce Clause authority to sustain federal civil rights statutes and anti-discrimination regimes against state resistance. Benefit from a jurisdictional hook that reaches private and state conduct nationwide without needing separate state-by-state political victories.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcers, beneficiary,
    organized, generational, mobile, national).

% Lose policy autonomy and traditional police powers to federal preemption. Cannot experiment with regulatory regimes that deviate from federal standards if economic effects might aggregate nationally. Litigate against expansion with intermittent success.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Local businesses and producers whose activities are purely intrastate become subject to federal wage, hour, environmental, and health regulation based on cumulative national economic effects. Must comply or face federal penalties; no opt-out available.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, intrastate_producers, payer,
    moderate, biographical, constrained, local).

% Advocate for a hard constitutional limit on federal commerce power and the restoration of state police powers. Structurally excluded from controlling judicial doctrine; their arguments appear in dissent and academic critique but do not command majority support.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% Argue that the broad effects test contradicts the original meaning of Commerce and regulate. Their views are cited in dissent but do not control current doctrine. They observe and document the drift from textual and structural limits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_originalists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulators).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national regulation of economic activity that individual states cannot achieve alone; solves collective-action problems and race-to-bottom dynamics across state lines; provides a jurisdictional foundation for federal civil rights and labor standards enforcement.
% TRANSFER_FUNCTION: Moves regulatory authority and policy autonomy from state governments and local economic actors to federal regulators and national policy frameworks, via judicially enforced federal preemption.
% ABSENT_VOICES: State sovereignty advocates and decentralized communities seeking regulatory diversity outside economic frameworks are present in dissent but structurally excluded from controlling doctrine; local actors without multi-state footprint are assumed to be reachable through aggregation.
% DISAPPEARANCE_RATIONALE: If the broad effects test vanished overnight, federal statutes predicated on it would face immediate constitutional challenge, state regulatory regimes would reassert across labor, environmental, and health domains, and the national uniformity currently enforced would fragment into competing state standards.
% FOUNDING_PROBLEM: State-level trade barriers, conflicting commercial regulations, and collective-action failures among states in the early republic; later, state resistance to civil rights and labor standards that could not be addressed without a national jurisdictional hook.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulators and national interest groups attest the problem remains live. State governments and originalist scholars attest the founding problem has shifted or been solved and the doctrine now operates as federal overreach. Judicial dissents in United States v. Lopez, United States v. Morrison, and NFIB v. Sebelius provide corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the aggregation doctrine allows federal regulators to claim authority over virtually all economic activity, decoupling federal reach from genuine interstate channels and extracting sovereignty from states. Suppression is equally high (0.78) because state alternatives are actively preempted and the legal framework closes off state-level regulatory divergence. Theater is moderate (0.32): the doctrine performs genuine coordination (civil rights, national market uniformity) but an increasing share of its maintenance consists of precedent defense and jurisdictional boundary policing rather than solving new collective-action problems. Accessibility collapse is high (0.82) because once the doctrine is accepted, states have no viable legal alternative to federal preemption in the economic domain. Resistance is substantial (0.70) due to persistent state litigation, originalist judicial dissents, and political federalism movements.
 *
 * PERSPECTIVAL GAP:
 *   The federal regulator seat experiences the constraint as necessary coordination machinery for a unified national economy; the state-government seat experiences the same structure as sovereign extraction. The engine computes this divergence from the structural data â the same doctrine reads as coordination from the beneficiary side and as extraction from the payer side.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators and national interest groups are structural beneficiaries: the constraint subsidizes their authority and policy goals (low d). State governments and intrastate producers are structural targets: they bear the costs of displaced sovereignty and compliance burdens (high d). Civil rights enforcers occupy a beneficiary position because the jurisdictional hook enables their mission (low d). State sovereignty advocates are excluded from the conversation entirely, receiving neither coordination benefit nor direct extraction cost but denied voice (directionality analytically indeterminate, treated as excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve interstate collective-action failures and state-level trade barriers. Those original problems are partially solved, yet the doctrine has expanded far beyond them to reach local economic conduct with attenuated interstate links. The persistence of the broad effects test is partly justified by live coordination needs (civil rights, national environmental standards), but its scope now exceeds those justifications. This creates mandatrophy risk: the constraint is not yet a piton because beneficiaries still profit from it, but its founding ratio of coordination-to-extraction has shifted toward extraction over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_limit,
    'Does the broad effects test retain meaningful limiting principles that exclude non-economic intrastate activity, or has the economic/noneconomic distinction collapsed in practice?',
    'Jurisprudential tracking of cases post-Lopez, Morrison, Raich, and Sebelius: if non-economic local activity is routinely swept in via attenuated causal chains, the limit is formal only.',
    'If the limit is formal only, the victim set expands to virtually all local activity and extractiveness approaches snare-like levels; if the limit is operative, the constraint remains a bounded tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_limit, empirical, 'Whether the economic/noneconomic boundary limits the doctrine in practice.').

omega_variable(
    federalism_as_necessary_cost,
    'Is the loss of state policy autonomy an irreducible cost of national economic coordination, or does the broad effects test extract surplus authority beyond what coordination requires?',
    'Comparative analysis with the intermediate_channels reading: if channel-based and instrumentality-based regulation achieves most coordination benefits with less state displacement, the excess scope is extractive surplus.',
    'If surplus, the constraint trends toward snare; if necessary cost, it remains tangled rope with high but justified extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_as_necessary_cost, conceptual, 'Whether state sovereignty loss is coordination cost or extraction surplus.').

omega_variable(
    reading_stability_under_originalist_pressure,
    'Is this reading facing sustained authority erosion from an originalist revival, or has it stabilized as the controlling doctrine despite intermittent pushback?',
    'Track Supreme Court appointments and majority commerce-clause jurisprudence over the next two terms; a decision explicitly rejecting aggregation for non-economic activity would signal erosion.',
    'If erosion continues, the reading may be reclassified as a piton or scaffold in decline; if stable, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_under_originalist_pressure, empirical, 'Stability of the broad effects test against originalist jurisprudential pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(broad_effects_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.12).
narrative_ontology:measurement(broad_effects_tr_t16, commerce_clause_scope__broad_effects_test, theater_ratio, 16, 0.18).
narrative_ontology:measurement(broad_effects_tr_t32, commerce_clause_scope__broad_effects_test, theater_ratio, 32, 0.24).
narrative_ontology:measurement(broad_effects_tr_t48, commerce_clause_scope__broad_effects_test, theater_ratio, 48, 0.28).
narrative_ontology:measurement(broad_effects_tr_t64, commerce_clause_scope__broad_effects_test, theater_ratio, 64, 0.3).
narrative_ontology:measurement(broad_effects_tr_t80, commerce_clause_scope__broad_effects_test, theater_ratio, 80, 0.32).

% Extraction over time
narrative_ontology:measurement(broad_effects_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(broad_effects_be_t16, commerce_clause_scope__broad_effects_test, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(broad_effects_be_t32, commerce_clause_scope__broad_effects_test, base_extractiveness, 32, 0.85).
narrative_ontology:measurement(broad_effects_be_t48, commerce_clause_scope__broad_effects_test, base_extractiveness, 48, 0.88).
narrative_ontology:measurement(broad_effects_be_t64, commerce_clause_scope__broad_effects_test, base_extractiveness, 64, 0.8).
narrative_ontology:measurement(broad_effects_be_t80, commerce_clause_scope__broad_effects_test, base_extractiveness, 80, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(broad_effects_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(broad_effects_su_t16, commerce_clause_scope__broad_effects_test, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(broad_effects_su_t32, commerce_clause_scope__broad_effects_test, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(broad_effects_su_t48, commerce_clause_scope__broad_effects_test, suppression_requirement, 48, 0.82).
narrative_ontology:measurement(broad_effects_su_t64, commerce_clause_scope__broad_effects_test, suppression_requirement, 64, 0.8).
narrative_ontology:measurement(broad_effects_su_t80, commerce_clause_scope__broad_effects_test, suppression_requirement, 80, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. The broad_effects_test reading claims virtually unlimited federal commerce authority over economic activity; the intermediate_channels reading accepts substantial effects but imposes limiting principles; the narrow_originalist reading restricts commerce to trade crossing state lines. Each reading instantiates a structurally distinct constraint with different epsilon, beneficiaries, and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
