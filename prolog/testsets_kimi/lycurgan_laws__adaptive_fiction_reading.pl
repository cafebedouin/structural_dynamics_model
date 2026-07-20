% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Adaptive Fiction
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Spartan constitutional order was publicly presented as an unchanging
 *   set of laws given by the semi-mythical lawgiver Lycurgus. This constraint
 *   story treats that presentation as a functional fictionâa noble lie that
 *   enabled covert adaptation by ephors and kings while preserving citizen
 *   cohesion. The constraint is one reading of the contested lycurgan_laws
 *   kernel, structurally distinct from readings that treat the laws as
 *   genuinely sacred and immutable (sacral_fidelity_reading) or as a brittle
 *   trap causing demographic collapse (demographic_trap_reading). The
 *   expected structural delta is mountain-level rhetoric concealing
 *   rope-level institutional flexibility; demographic decline is attributed
 *   to enforcement failure rather than constitutional rigidity per se.
 *
 * KEY AGENTS:
 *   - ephors: Primary agenda-setters (institutional/constrained) â interpret and covertly adapt the laws while publicly denying change
 *   - spartan_kings: Beneficiaries (powerful/constrained) â gain stability and interpretive space from the regime
 *   - spartan_citizens: Primary coordinated beneficiaries (organized/identity_locked) â fused to the immutability doctrine, bear military obligations, unaware of covert adaptation
 *   - perioeci: Excluded actors (moderate/constrained) â subject to the regime without voice in its governance
 *   - external_historians: Analytical observers (analytical/analytical) â propagate the mountain claim without access to insider adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.28).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.42).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Adaptive Fiction").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b').
narrative_ontology:cs_kernel_codification('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', fixed_text).
narrative_ontology:cs_authority_grounding('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', lineage).
narrative_ontology:cs_interpretation_layer_present('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b').
narrative_ontology:cs_reading_relation('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', foundational, constitutional_fiction_enables_adaptation).
narrative_ontology:cs_axiom_status(constitutional_fiction_enables_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', constitutional_fiction_enables_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', foundational, covert_interpretation_not_rigidity).
narrative_ontology:cs_axiom_status(covert_interpretation_not_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', covert_interpretation_not_rigidity, empirically_contingent).
narrative_ontology:cs_reference_frame('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', lycurgan_immutability_regime).
narrative_ontology:cs_drift_state('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', late_classical_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5757c6a-c1a8-49c5-bfc4-6d1369f89f9b', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_citizens).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephors).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five annually elected magistrates who administer Spartan governance and interpret the Lycurgan laws in daily practice; they covertly adapt provisions to changing circumstances while publicly maintaining that no change has occurred, preserving the immutability fiction.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephors, agenda_setter,
    institutional, generational, constrained, national).

% Dual hereditary monarchs who serve as military and religious leaders within the Lycurgan framework; they benefit from constitutional stability and interpretive flexibility that allows them to exercise agency without triggering formal revision debates.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, beneficiary,
    powerful, generational, constrained, national).

% Full citizen-warriors (homoioi) whose lives are structured by the Lycurgan regime from agoge through military service to retirement; their civic identity is fused with the belief in unchanging ancestral laws, making conceptual exit from the regime nearly impossible.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizens, beneficiary,
    organized, biographical, identity_locked, national).

% Free non-citizen inhabitants of Laconia and Messenia who engage in trade and craft production; they are subject to the Spartan constitutional order and its military demands but have no voice in its interpretation or any formal mechanism to challenge the immutability claim.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioeci, excluded,
    moderate, biographical, constrained, national).

% Greek and later Roman writers who transmit the image of an unchanging Spartan constitution; they observe and propagate the mountain claim from outside the interpretive circle, lacking access to the covert adaptation mechanisms known only to insider officeholders.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, external_historians, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustaining long-term social and military cohesion in a citizen-warrior society by providing a stable, taken-for-granted constitutional framework that eliminates recurrent bargaining over basic rules, while covertly permitting necessary adaptation through elite interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional flexibility to the ephors and kings; transfers the appearance of timeless, unchanging order from the figure of Lycurgus to the present regime; transfers military and civic obligation from the citizen body to the state.
% ABSENT_VOICES: Citizens who might have demanded formal constitutional revision had they known adaptation was occurring; perioeci and helots who bore the economic and military burdens of the regime without representation; constitutional innovators from other Greek poleis whose alternative models were rendered invisible by the immutability claim.
% DISAPPEARANCE_RATIONALE: Spartan society was organized around the expectation of an eternal, unchanging constitutional order; if the fiction vanished and the laws were openly treated as revisable, the identity-fusion of the citizens would destabilize, the ephors' interpretive monopoly would lose its essential cover, and the social equilibrium would shift from inherited obligation to explicit political bargaining.
% FOUNDING_PROBLEM: Pre-Lycurgan political instability, factional conflict among social classes, and the need to fix a constitutional order durable enough to sustain a militarized society over generations without recurrent civil strife.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians such as Herodotus and Thucydides attest to early Greek constitutional instability generally, but no external source independently corroborates the specific Spartan founding crisis from outside the Spartan mythological tradition; modern scholarship treats the 'Lycurgan' founding as a retrojected legitimizing narrative.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).
:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores low on extractiveness (0.28) because its primary operation is coordination of a citizen-warrior society, not resource extraction; the ephors and kings gain authority but not concentrated material rent. Suppression is moderate (0.42) because the immutability fiction suppresses knowledge of adaptation and blocks formal revision pathways. Theater_ratio is high (0.58) and rising over the interval because the public performance of unchanging ancestral laws increasingly outpaced the functional reality of covert elite interpretation. Accessibility_collapse is moderately high (0.62) because the identity-locked citizenry could not easily conceive alternatives to the Lycurgan model. Resistance is low-moderate (0.32) because the system's legitimacy was deeply internalized, though occasional royal or citizen opposition flared. The measurement series shares a single time grid (0â80) tracking the degeneration of the fiction into pure theater as demographic pressures mounted.
 *
 * PERSPECTIVAL GAP:
 *   The ephors and kings experience the constraint as a flexible instrument of governance with room for covert maneuver; the citizen body experiences it as an absolute, identity-constituting natural order. The engine should compute low directionality (near-beneficiary) for the officeholders and near-symmetric to low-beneficiary for the citizens, despite the citizens' identity-locked exit, because they are structurally coordinated rather than extracted from. Perioeci sit outside the beneficiary structure entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as spartan_citizens (coordinated by stable rules), ephors (agenda-setting interpretive authority), and spartan_kings (stability benefits). No victims are declared because the constraint's primary structural relationship is coordination. The directionality derivation will place officeholders nearer the beneficiary end due to their interpretive control, citizens near symmetric due to coordinated identity benefits, and excluded perioeci nearer the target end because they bear regime costs without coordination benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy mislabeling by separating the rhetoric (immutability as mountain claim) from the function (covert adaptation as coordination). If the founding problemâpre-Lycurgan instabilityâwas genuinely solved by the adaptive fiction, the constraint remains a rope. If the coordination function atrophied and only the immutability theater persisted without adaptation, it would drift toward piton. The temporal measurements show theater_ratio rising but not yet dominating; the founding_problem_status is contested, preventing premature obsolescence verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of a contested kernel, and how would sibling readings change its classification?',
    'Compare against the compiled stories for sacral_fidelity_reading and demographic_trap_reading; sacral fidelity would classify as mountain or commitment-system snare, while demographic trap would classify as piton or scaffold failure.',
    'Confirms that epsilon varies across kernel readings and that decomposition was necessary per the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the lycurgan_laws kernel family').

omega_variable(
    historicity_of_lycurgan_foundations,
    'Was there a historical Lycurgus and a single founding moment, or is the kernel entirely a retrojected tradition created to legitimize later arrangements?',
    'Archaeological correlation with pottery styles and settlement patterns; analysis of the earliest literary sources for anachronism.',
    'If entirely retrojected, the constraint''s authority_grounding shifts from lineage to extraction or practice, altering the commitment-system classification and drift_state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historicity_of_lycurgan_foundations, empirical, 'Whether the Lycurgan founding is historically factual or legitimizing fiction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of adaptation knowledge structural (actively hidden by officeholders) or internalized (citizens cannot conceive of constitutional change due to identity fusion)?',
    'Comparative analysis of citizen revolt or reform demands in other Greek poleis versus total absence in Sparta; evidence of deliberate concealment in elite sources.',
    'If internalized, effective suppression is higher than the structural measure suggests; the constraint functions more as identity_coordination than enforcement_mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of adaptation knowledge').

omega_variable(
    demographic_causation_ambiguity,
    'Was Spartan demographic decline caused by enforcement failure of the adaptive fiction, by genuine constitutional rigidity preventing needed reform, or by unrelated factors such as war casualties and wealth concentration?',
    'Demographic modeling correlating citizen numbers with known war losses, property consolidation trends, and comparative polis data.',
    'Would validate or undermine this reading''s core causal claim (enforcement failure, not rigidity) and shift classification toward demographic_trap_reading if rigidity is established.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_causation_ambiguity, empirical, 'Causal ambiguity in Spartan demographic decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lycu_tr_t16, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(lycu_tr_t32, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(lycu_tr_t48, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 48, 0.6).
narrative_ontology:measurement(lycu_tr_t64, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 64, 0.7).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.78).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lycu_be_t16, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(lycu_be_t32, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(lycu_be_t48, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 48, 0.34).
narrative_ontology:measurement(lycu_be_t64, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 64, 0.4).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.46).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lycurgan_laws__adaptive_fiction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the lycurgan_laws kernel, decomposed into three structurally distinct claims per the epsilon-invariance principle: sacral_fidelity_reading (divine immutability), adaptive_fiction_reading (covert adaptation through interpretive flexibility), and demographic_trap_reading (brittle unrevisability causing collapse). Each reading carries a distinct epsilon, beneficiary structure, and classification. The readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
