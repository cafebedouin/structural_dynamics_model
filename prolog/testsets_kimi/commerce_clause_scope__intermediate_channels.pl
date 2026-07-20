% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Intermediate Channels Reading with Limiting Principles
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   The Commerce Clause as interpreted through the intermediate_channels
 *   reading grants Congress authority to regulate channels of interstate
 *   commerce, instrumentalities and persons in interstate commerce, and
 *   activities substantially affecting interstate commerce, subject to
 *   limiting principles: non-economic activity requires a jurisdictional
 *   element, aggregation applies only to economic activity, and regulation
 *   cannot proceed via attenuated causal chains. This reading emerged in the
 *   post-Lopez era (1995) as an attempt to revive judicial limits on federal
 *   power while preserving the New Deal settlement for economic regulation.
 *   It functions as tangled rope: it solves genuine collective-action
 *   problems in a national economy while extracting regulatory authority from
 *   states through manipulable doctrinal distinctions.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary (institutional/arbitrage) â gains expansive commerce authority with fallback to taxing and spending powers
 *   - national_commercial_interests: Secondary beneficiary (powerful/mobile) â gains uniform national regulation and preemption of conflicting state laws
 *   - state_governments: Primary target (institutional/constrained) â loses regulatory autonomy to federal preemption despite nominal limiting principles
 *   - local_non_economic_actors: Secondary target (powerless/trapped) â subject to federal regulation of ostensibly local conduct through recharacterization
 *   - federal_judiciary: Administrative agent (institutional/constrained) â maintains unstable doctrinal architecture and bears costs of doctrinal incoherence
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â monitors manipulability of limiting principles and doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.57).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.55).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.57).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Intermediate Channels Reading with Limiting Principles").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '8f71f8b3-abce-4b9f-b21a-47bce6cf89dd').
narrative_ontology:cs_kernel_codification('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', formalized).
narrative_ontology:cs_authority_grounding('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', lineage).
narrative_ontology:cs_interpretation_layer_present('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd').
narrative_ontology:cs_reading_relation('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', foundational, substantial_effects_economic_only).
narrative_ontology:cs_axiom_status(substantial_effects_economic_only, holdable).
narrative_ontology:cs_axiom_grounding('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', substantial_effects_economic_only, conventional).
narrative_ontology:cs_axiom('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', foundational, non_economic_jurisdictional_required).
narrative_ontology:cs_axiom_status(non_economic_jurisdictional_required, holdable).
narrative_ontology:cs_axiom_grounding('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', non_economic_jurisdictional_required, conventional).
narrative_ontology:cs_reference_frame('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', commerce_with_categorical_limits).
narrative_ontology:cs_drift_state('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', post_lopez_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f71f8b3-abce-4b9f-b21a-47bce6cf89dd', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_commercial_interests).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_non_economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises commerce power through legislation and enforcement. Benefits from broad authority to regulate national economic problems while nominally constrained by judicial limiting principles. Can shift regulatory strategies to taxing or spending powers when commerce limits threaten.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Bear the erosion of regulatory autonomy as federal commerce power expands into traditional police powers. Limiting principles nominally protect family law, criminal law, and education, but the economic versus non-economic distinction is unstable and manipulable. States litigate to enforce limits but often lose.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, payer,
    institutional, generational, constrained, national).

% Individuals and communities engaged in local non-economic activity that becomes subject to federal regulation through attenuated causal chains or judicial recharacterization as economic. Exit is minimalâthey cannot opt out of federal law and rarely have resources to mount constitutional challenges.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_non_economic_actors, payer,
    powerless, immediate, trapped, local).

% Multi-state businesses and industries that benefit from uniform national regulation, reduced state trade barriers, and preempted conflicting state laws. Support broad federal commerce authority while adapting to regulatory requirements.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_commercial_interests, beneficiary,
    powerful, biographical, mobile, national).

% Maintains the doctrinal architecture of channels, instrumentalities, and substantial effects with limiting principles. Must draw unstable lines between economic and non-economic activity and police attenuated causal chains. Bears institutional costs of doctrinal incoherence when limiting principles are manipulated to reach desired outcomes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Analyze the doctrinal instability of the economic versus non-economic distinction and the manipulability of limiting principles. Document the gap between the formal doctrine and its practical application.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables national economic regulation addressing interstate collective-action problems, externalities, and race-to-bottom dynamics that individual state regulation cannot solve, while nominally preserving state regulatory capacity over non-economic local conduct.
% TRANSFER_FUNCTION: Transfers regulatory authority from state governments to the federal government for economic activity with interstate nexus, and transfers doctrinal legitimacy from the federal judiciary to the legislative branch by supplying a judicially manageable test for commerce power.
% ABSENT_VOICES: Narrow originalists who reject the substantial effects test entirely; radical federalists who would restore pre-New Deal commerce limits; and local non-economic actors whose conduct is regulated via attenuated causal chains but who lack litigation resources to challenge federal statutes.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, federal statutes currently justified under the substantial effects test with limiting principles would face immediate constitutional challenge. States would reclaim regulatory authority over labor, environmental, and criminal laws justified under attenuated commerce theories. The national commercial regulatory framework would fragment unless reconstituted under alternate enumerated powers or constitutional amendment.
% FOUNDING_PROBLEM: State-level regulation in the early twentieth century failed to address collective-action problems, interstate externalities, and destructive competition in a national industrial economy, requiring federal coordination.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and federalism scholars outside the benefiting parties attest that state regulation produced race-to-bottom dynamics in labor standards and environmental protection. However, public-choice scholars and state attorneys general attest that the limiting principles have failed to constrain federal overreach into non-economic local conduct, and that the intermediate reading now extracts more authority than the coordination problem requires.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.57, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.57) because federal power genuinely solves national collective-action problems but systematically overreaches into local non-economic spheres through unstable categorical distinctions. Suppression is moderate (0.55) because the constraint depends on federal courts actively enforcing preemption via the Supremacy Clause and striking down or upholding statutes under the limiting principles. Theater ratio is moderate and rising (0.42 at interval end): courts perform limiting-principle analysis in cases like Lopez, Morrison, and NFIB while often upholding broad federal power through recharacterization, producing formal limits with limited practical bite. Accessibility collapse is moderate (0.45): alternatives such as narrow originalism and pure federal supremacy exist and are actively argued, but the intermediate reading dominates judicial doctrine. Resistance is moderate-high (0.60) from states, litigants, and originalist scholars. The claim and metrics are independently authored: the constraint is claimed as tangled rope because a genuine coordination function coexists with asymmetric extraction, while the metrics acknowledge substantial performative maintenance and moderate-high resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the federal legislative seat, the arrangement is necessary coordination to prevent state-level externalities and race-to-bottom dynamics in a national economy. From the state government seat, the same doctrine operates as federal overreach that extracts police powers through unstable categorical distinctions. From the judicial seat, the doctrine requires performing limiting-principle analysis that rarely changes outcomes, producing high institutional theater. From the analytical seat, the gap between the formal doctrine and its application is the central diagnostic feature. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and national commercial interests sit near the beneficiary pole: the constraint expands federal authority and reduces regulatory fragmentation. State governments and local non-economic actors sit near the target pole: the constraint preempts their regulatory choices and subjects local conduct to federal control. The federal judiciary sits near symmetric: it administers the constraint but neither materially collects from nor pays its extraction, instead bearing institutional costs in doctrinal coherence. Directionality is structurally derived from beneficiary and victim declarations combined with exit options; no overrides are required.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the doctrine as pure coordination (rope) by acknowledging the victim set: states lose autonomous regulatory capacity and local actors face federal regulation of non-economic conduct. It prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function: the substantial effects test solves collective-action problems that state regulation cannot address. The limiting principles, while manipulable, are not pure theaterâthey do occasionally invalidate federal statutesâso piton is inappropriate. Temporal measurements show rising theater_ratio, suggesting possible future piton degradation if limiting principles become entirely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_stability,
    'Is the economic versus non-economic distinction a stable jurisdictional limit, or does it collapse under judicial pressure through recharacterization?',
    'Comparative doctrinal analysis across circuits and time: track rates at which courts recharacterize challenged activity as economic to uphold federal statutes.',
    'If the distinction is unstable and recharacterization is routine, the constraint extracts more than its coordination function justifies, pushing toward higher theater and extractiveness. If stable, the limiting principles remain genuine constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_stability, empirical, 'Stability of the economic versus non-economic jurisdictional boundary').

omega_variable(
    attenuated_chain_manipulability,
    'Does the prohibition on attenuated causal chains effectively prevent federal regulation of non-economic local conduct, or can any local activity be connected to interstate commerce through sufficient inferential steps?',
    'Track Supreme Court and lower court commerce clause cases, counting cases where local non-economic conduct is upheld versus struck down based on chain attenuation.',
    'If courts routinely uphold regulation despite long causal chains, the limiting principle is performative and theater_ratio should rise. If attenuation is a meaningful barrier, extraction is more constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attenuated_chain_manipulability, empirical, 'Whether attenuated causal chain limits are enforceable or performative').

omega_variable(
    coordination_extraction_ratio,
    'What proportion of federal commerce legislation under this reading solves genuine collective-action problems versus extracts regulatory authority from states?',
    'Public-choice or federalism scholarship categorizing commerce clause legislation by motivation and effect, distinguishing interstate externalities from local police power preemption.',
    'A high genuine coordination ratio supports the tangled rope classification; a low ratio would indicate the coordination story is largely cover for extraction, pushing the constraint toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_ratio, conceptual, 'Ratio of genuine coordination to extraction in commerce clause legislation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_intermediate_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cc_intermediate_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cc_intermediate_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cc_intermediate_tr_t15, commerce_clause_scope__intermediate_channels, theater_ratio, 15, 0.35).
narrative_ontology:measurement(cc_intermediate_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.38).
narrative_ontology:measurement(cc_intermediate_tr_t25, commerce_clause_scope__intermediate_channels, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cc_intermediate_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(cc_intermediate_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cc_intermediate_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cc_intermediate_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cc_intermediate_be_t15, commerce_clause_scope__intermediate_channels, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(cc_intermediate_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(cc_intermediate_be_t25, commerce_clause_scope__intermediate_channels, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(cc_intermediate_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.57).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_scope__intermediate_channels, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel, instantiating the intermediate_channels interpretation. It is structurally distinct from the broad_effects_test reading (higher epsilon, fewer judicial limits) and the narrow_originalist reading (lower epsilon, rejects substantial effects). The three readings form a constraint family linked by mutual doctrinal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
