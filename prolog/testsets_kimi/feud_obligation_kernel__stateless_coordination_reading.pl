% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Mechanism
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint is the stateless_coordination_reading of the
 *   feud_obligation_kernel, which addresses customary blood-feud obligations
 *   in kin-based societies lacking centralized enforcement. The reading
 *   frames these obligations as a self-enforcing coordination mechanism that
 *   generates justice and deterrence. Sibling readings include the
 *   extraction_cycle_reading (framing the mechanism as a destructive
 *   extraction spiral) and the christianized_pacification_reading (framing it
 *   as a violation of divine and royal monopoly on legitimate violence). The
 *   constraint names kin groups as beneficiaries of security and honor, and
 *   defectors from the obligation as bearers of social costs. Wergild and
 *   other settlement alternatives coexist, keeping suppression low. The
 *   authored claim is rope, reflecting the functionalist/coordination framing
 *   of this reading, while the metrics independently describe the moderate
 *   extraction imposed on defectors and the identity-locked exit structure
 *   that sustains the equilibrium.
 *
 * KEY AGENTS:
 *   - Feud participants (beneficiary / organized / identity-locked): Kin-group members who receive justice and deterrence benefits.
 *   - Feud defectors (payer / powerless / trapped): Individuals who break norms and suffer honor loss and expulsion.
 *   - Kin elders (agenda-setter / organized / constrained): Customary interpreters who mediate obligations but do not centrally enforce.
 *   - Centralizing rulers (excluded / powerful / mobile): State-building actors excluded from the customary framework who seek to replace it.
 *   - Legal anthropologists (observer / analytical): External analysts documenting the system's functional logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.42).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.25).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Mechanism").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '4200eda6-d292-4727-849c-ece36eb2fffb').
narrative_ontology:cs_kernel_codification('4200eda6-d292-4727-849c-ece36eb2fffb', distributed).
narrative_ontology:cs_authority_grounding('4200eda6-d292-4727-849c-ece36eb2fffb', self_enforcing).
narrative_ontology:cs_reading_relation('4200eda6-d292-4727-849c-ece36eb2fffb', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('4200eda6-d292-4727-849c-ece36eb2fffb', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('4200eda6-d292-4727-849c-ece36eb2fffb', foundational, kin_reciprocal_justice_legitimate).
narrative_ontology:cs_axiom_status(kin_reciprocal_justice_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4200eda6-d292-4727-849c-ece36eb2fffb', kin_reciprocal_justice_legitimate, conventional).
narrative_ontology:cs_axiom('4200eda6-d292-4727-849c-ece36eb2fffb', foundational, kin_group_violence_enforcement_authority).
narrative_ontology:cs_axiom_status(kin_group_violence_enforcement_authority, holdable).
narrative_ontology:cs_axiom_grounding('4200eda6-d292-4727-849c-ece36eb2fffb', kin_group_violence_enforcement_authority, conventional).
narrative_ontology:cs_reference_frame('4200eda6-d292-4727-849c-ece36eb2fffb', stateless_kin_order).
narrative_ontology:cs_drift_state('4200eda6-d292-4727-849c-ece36eb2fffb', early_state_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4200eda6-d292-4727-849c-ece36eb2fffb', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, kinship_based_justice).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, self_enforcing_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of kinship groups bound by reciprocal obligation to avenge wrongs or seek compensation for killings and injuries. They receive justice, deterrence against predation, and maintenance of collective honor. Their social identity is fused with kin membership; abandoning the obligation means social death, so exit is effectively impossible.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants, beneficiary,
    organized, generational, identity_locked, regional).

% Individuals who refuse to participate in feud obligations or who break kinship norms. They suffer loss of honor, expulsion from the kin network, and loss of access to collective security and redress. They are trapped because outside the kin group there is no alternative protective structure in the stateless context.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, regional).

% Senior members of kin groups who interpret customary norms, negotiate wergild settlements, and decide when obligations are triggered or satisfied. They do not centrally command enforcement but shape the parameters of reciprocal action. Their authority depends on continued community acceptance.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_elders, agenda_setter,
    organized, generational, constrained, regional).

% Monarchs or state builders seeking to monopolize legitimate violence and replace kin-based justice with royal courts or territorial law. They are excluded from the customary framework and actively oppose it, but their institutional alternatives are not yet dominant in the stateless regions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, centralizing_rulers, excluded,
    powerful, generational, mobile, national).

% Analytical observers studying the feud system from outside, comparing its equilibrium properties to centralized legal institutions and documenting its functional logic without participating in its honor economy.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, legal_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides justice and deterrence in the absence of centralized enforcement capacity by aligning incentives across kin groups so that injuries are reciprocally punished, creating a mutual deterrence equilibrium.
% TRANSFER_FUNCTION: Moves honor, social standing, and reciprocal obligation across kin groups; defectors transfer their social membership and security to the collective sanction of the kin network.
% ABSENT_VOICES: Centralizing monarchs and ecclesiastical authorities who would substitute sovereign or divine justice for kin-based vengeance; also pacifist defectors who would prefer unilateral disarmament but are not heard in the honor-coded discourse.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished, kin groups would lose a primary mechanism for deterring inter-group violence and obtaining redress; honor-based social order would destabilize, and either centralized enforcement or higher baseline violence would follow.
% FOUNDING_PROBLEM: The absence of a centralized state with a monopoly on legitimate violence leaves a vacuum in justice and security; individuals and kin groups face unchecked predation without a mechanism to deter offenses or obtain redress.
% FOUNDING_PROBLEM_CORROBORATION: Legal anthropologists and comparative historians from outside the beneficiary kin groups attest that stateless societies face genuine security dilemmas where self-help justice emerges as a functional response to the absence of centralized enforcement.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because defectors bear severe social costs, yet the mechanism is not primarily rent-seeking and produces a collective good. Suppression is low (0.25) because wergild and other dispute alternatives coexist, meaning the constraint does not collapse the choice set. Theater ratio is low-to-moderate (0.20): feud rituals carry performative dimensions, but enforcement is largely functional rather than theatrical. Accessibility collapse (0.35) is moderate because, although stateless actors lack centralized alternatives, wergild provides a known exit path. Resistance (0.30) reflects defector reluctance and the gradual pressure of centralizing authorities without implying systemic breakdown. The measurement grid shares time points across metrics to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The participant seat and the defector seat should compute very different per-seat types. Participants experience the constraint as protective coordination that secures honor and deters predation; defectors experience it as coercive ostracism that extracts their social standing and physical security. Kin elders sit closer to the coordination pole because their authority depends on maintaining the equilibrium, while centralizing rulers experience the constraint as an illegitimate rival to sovereign order. The engine derives this divergence from the same structural data via beneficiary-victim mapping and exit-option modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants are declared beneficiaries with identity-locked exit, which drives their directionality toward the full-beneficiary end. Feud defectors are declared victims with trapped exit, driving directionality toward the full-target end. Kin elders are not in the beneficiary or victim arrays and have constrained exit, placing them near the symmetric middle. Centralizing rulers are excluded and mobile, giving them low directionality as outside challengers rather than governed subjects. The resulting per-seat Ï values will be negative or near-zero for participants and strongly positive for defectors.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the beneficiary-victim split, one might mislabel the mechanism: looking only at defector costs would suggest a snare, while looking only at participant benefits would suggest a rope. The framework requires naming both sides and computing per-seat classifications, which prevents either mislabeling from capturing the whole constraint. The coexistence of wergild and the absence of centralized enforcement provide the coordination evidence that blocks a pure-snare classification, while the identity-locked exit and trapped defector path provide the extraction signal that blocks an unqualified rope classification. The claim/metric gap is deliberate: the reading claims rope, the metrics describe a hybrid structure, and the engine measures that divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stateless_coordination_vs_extraction,
    'Does the blood-feud mechanism produce net coordination gains (justice and deterrence) or net extraction (depletion of productive capacity and perpetual violence)?',
    'Comparative demographic and economic analysis of stateless societies with and without active feud systems, measuring homicide rates, productive investment, and inter-group conflict frequency.',
    'If net extraction dominates, this reading''s classification as rope/coordination is wrong and should shift toward snare or tangled_rope; if net coordination dominates, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stateless_coordination_vs_extraction, empirical, 'Whether the mechanism coordinates or extracts on net.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (kinship expulsion and physical ostracism) or internalized (shame and honor codes that persist after external barriers are removed)?',
    'Ethnographic observation of defector trajectories upon migration to non-kin contexts: if suppression persists, it is internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, pushing the defector seat toward snare-like classification; purely structural suppression keeps it closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    kernel_equilibrium_vs_commitment_system,
    'Is the blood-feud obligation best framed as a commitment system with a stabilized kernel, or as an emergent repeated-game equilibrium without a codified commitment?',
    'Analysis of whether the norm persists through transmitted customary law and ritual (kernel present) or purely through decentralized reciprocal incentives (no kernel).',
    'If no kernel exists, the cs_structure block may be misapplied and the constraint should be reclassified as a non-CS coordination mechanism rather than a kernel reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_equilibrium_vs_commitment_system, conceptual, 'Whether the constraint instantiates a kernel reading or a non-CS equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t2, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(feud_tr_t4, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(feud_tr_t6, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(feud_tr_t8, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 10, 0.22).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(feud_be_t2, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(feud_be_t4, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(feud_be_t6, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(feud_be_t8, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 10, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feud_obligation_kernel__stateless_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, identity_coordination).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The feud_obligation_kernel decomposes into three structurally distinct constraints depending on reading: stateless_coordination (functionalist), extraction_cycle (political economy), and christianized_pacification (theological/jurisprudential). Each reading has distinct beneficiary/victim structures and epsilon values, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
