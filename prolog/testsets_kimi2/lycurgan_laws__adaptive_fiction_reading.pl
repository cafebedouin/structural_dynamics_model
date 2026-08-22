% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Noble Lie Masking Covert Adaptation
 *   domain: political/constitutional/philosophical
 *
 * SUMMARY:
 *   The Spartan constitution attributed to Lycurgus was presented as an
 *   immutable, divinely sanctioned order. This constraint story adopts the
 *   adaptive_fiction_reading: the immutability claim was a noble lie that
 *   coordinated Spartan society by foreclosing constitutional debate, while
 *   the ephors and kings exercised covert interpretive power to adapt the
 *   laws to changing circumstances. Demographic decline in the classical
 *   period is read here as a failure of enforcement and covert adaptation,
 *   not as an inevitable effect of rigid law. The constraint is the standing
 *   arrangement of claimed constitutional immutability that masked and
 *   enabled covert magisterial adaptation.
 *
 * KEY AGENTS:
 *   - spartan_magistracy: Primary agenda-setter and beneficiary (institutional/arbitrage) â maintains the immutability fiction and controls covert adaptation.
 *   - spartan_citizen_body: Primary target (organized/constrained) â bears the costs of demographic decline and is bound by the taboo against constitutional change.
 *   - subjugated_populations: Excluded and trapped (powerless/trapped) â outside the constitutional conversation but foundational to the system's economy.
 *   - external_greek_observers: Analytical observers (analytical/analytical) â report contradictions without altering the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.65).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Noble Lie Masking Covert Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional/philosophical").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '0db98874-35af-485d-8905-e9cf1b7e0209').
narrative_ontology:cs_kernel_codification('0db98874-35af-485d-8905-e9cf1b7e0209', fixed_text).
narrative_ontology:cs_authority_grounding('0db98874-35af-485d-8905-e9cf1b7e0209', extraction).
narrative_ontology:cs_interpretation_layer_present('0db98874-35af-485d-8905-e9cf1b7e0209').
narrative_ontology:cs_reading_relation('0db98874-35af-485d-8905-e9cf1b7e0209', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('0db98874-35af-485d-8905-e9cf1b7e0209', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('0db98874-35af-485d-8905-e9cf1b7e0209', foundational, political_stability_justifies_foundational_fictions).
narrative_ontology:cs_axiom_status(political_stability_justifies_foundational_fictions, holdable).
narrative_ontology:cs_axiom_grounding('0db98874-35af-485d-8905-e9cf1b7e0209', political_stability_justifies_foundational_fictions, instrumental).
narrative_ontology:cs_axiom('0db98874-35af-485d-8905-e9cf1b7e0209', foundational, covert_magisterial_adaptation_is_legitimate_constitutional_practice).
narrative_ontology:cs_axiom_status(covert_magisterial_adaptation_is_legitimate_constitutional_practice, holdable).
narrative_ontology:cs_axiom_grounding('0db98874-35af-485d-8905-e9cf1b7e0209', covert_magisterial_adaptation_is_legitimate_constitutional_practice, conventional).
narrative_ontology:cs_reference_frame('0db98874-35af-485d-8905-e9cf1b7e0209', covert_adaptation_framework).
narrative_ontology:cs_drift_state('0db98874-35af-485d-8905-e9cf1b7e0209', classical_demographic_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0db98874-35af-485d-8905-e9cf1b7e0209', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the claim of Lycurgan immutability while exercising covert interpretive power to adapt law to circumstances. They derive legitimacy from guardianship of the unchangeable laws and suppress open constitutional debate. Their exit from the constraint is unlimited in practice since they control its interpretation, though they are bound by the need to maintain the fiction.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy, beneficiary).

% Bound by the strict Lycurgan regimen and the taboo against constitutional change. They bear the costs of demographic decline, property inequality, and military obligation. They believe the laws are immutable and sacred, which prevents them from demanding formal revision even as practice shifts around them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body, payer,
    organized, biographical, constrained, national).

% Completely outside the constitutional order that the Lycurgan fiction legitimizes. Their exclusion is foundational to the Spartan system but they have no voice in its interpretation or maintenance.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, subjugated_populations, excluded,
    powerless, generational, trapped, national).

% Observe and report on Spartan customs from outside. They note contradictions between Spartan rhetoric of immutability and actual practice, but their observations do not alter the constraint's operation within Sparta.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, external_greek_observers, observer,
    analytical, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates Spartan political life by providing a stable, unchallengeable constitutional framework that forecloses legitimacy contests and succession crises, thereby reducing factional violence and maintaining oligarchic order.
% TRANSFER_FUNCTION: The arrangement transfers interpretive authority and the capacity for constitutional adaptation from the citizen body collectively to the spartan_magistracy, who alone may modify practice while maintaining the public fiction of immutability.
% ABSENT_VOICES: Helots and perioeci are structurally excluded from constitutional discourse. Democratic reformers within Sparta who would advocate for open constitutional revision are silenced by the sacred taboo surrounding the Lycurgan text. Outside Greek philosophers and historians who observe the gap between rhetoric and practice are ignored by the magistracy.
% DISAPPEARANCE_RATIONALE: If the immutability fiction vanished overnight, the Spartan political order would lose its central legitimating taboo. Open constitutional debate would become possible, the magistracy's covert interpretive monopoly would be exposed and contested, and the stability-producing prohibition on formal revision would dissolve, forcing a rearrangement of political authority.
% FOUNDING_PROBLEM: The need to stabilize a post-conquest society after the Messenian Wars and to prevent future tyrannical or democratic upheaval by fixing a constitutional order that no faction could openly challenge or revise.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle and later historians attest to Sparta's unusual political stability relative to other Greek poleis, corroborating that a severe coordination problem existed in the archaic period. However, no non-Spartan contemporary source corroborates that the immutability claim was a deliberately constructed fiction rather than genuine sacral law; the noble-lie framing is a retrospective analytical reconstruction from the observer seat. Beneficiary sources (the Spartan magistracy itself) never acknowledged the fiction as such.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects substantial but not total extraction: the magistracy captures interpretive power and legitimacy while the citizen body pays in foregone constitutional agency and demographic contraction. Suppression (0.72) is high because the constraint's persistence depends on actively suppressing open constitutional debate and punishing challenges to the Lycurgan frame. Theater_ratio (0.72) is high because the immutability claim is largely performative â actual practice drifted significantly from the claimed text. Accessibility_collapse (0.82) is high: once the immutability claim is accepted, alternatives (open constitutional revision) become nearly unthinkable. Resistance (0.38) is moderate: the citizen body internalized the sacredness of the laws, limiting open resistance, though covert resentment and demographic withdrawal constitute passive resistance.
 *
 * PERSPECTIVAL GAP:
 *   The spartan_magistracy seat experiences the constraint as a necessary and legitimate coordination mechanism: they preserve order by managing the tension between immutable rhetoric and practical necessity. The spartan_citizen_body experiences it as an unchangeable sacred order that paradoxically shifts around them without their input. The engine will compute per-seat classifications: the magistracy seat likely computes as rope or low-extraction tangled_rope, while the citizen body computes as snare-like or high-extraction tangled_rope. This divergence is the measurement the corpus exists to capture; the story does not reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the spartan_magistracy, which collects interpretive authority and political stability through the immutability claim. Victim declarations map to the spartan_citizen_body, which loses formal constitutional agency and suffers demographic costs. The magistracy's arbitrage-grade exit (control of interpretation) pushes directionality toward the beneficiary pole (low d); the citizen body's constrained exit (bound by sacred taboo) pushes toward the target pole (high d). Subjugated populations, though excluded, are trapped and thus sit at the extreme target end. No directionality overrides are needed because the structural derivation chain already captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing post-conquest political stasis â was plausibly live in the archaic period. By the classical period, the specific crisis had passed, yet the constraint persisted. The adaptive fiction reading diagnoses this persistence as serving a second-order coordination function (covert adaptation by the magistracy) rather than as pure inertial piton. Because the constraint has identifiable active beneficiaries (the magistracy) and active enforcement (suppression of constitutional debate), it cannot be a piton. Because the coordination function (political stability) was genuine, if deceptive, it resists classification as a pure snare. The tangled_rope typing captures both the real coordination and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_noble_lie,
    'Is the Lycurgan immutability claim a genuine sacral or natural-law commitment, or a deliberately constructed political fiction?',
    'Comparative source criticism of the earliest Spartan oral traditions against retrospective readings in Plutarch and Xenophon, plus anthropological analysis of archaic Greek lawgiving as political performance.',
    'If genuine sacral law, the constraint collapses toward the sacral_fidelity_reading (mountain or snare); if a constructed fiction, the adaptive_fiction_reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_noble_lie, conceptual, 'Ambiguity between genuine sacral commitment and deliberate political fiction').

omega_variable(
    covert_adaptation_scope,
    'How extensive was the covert adaptation practiced by the ephors and kings, and did it meaningfully alter the constitutional order or merely interpret at the margins?',
    'Detailed philological and historical analysis of institutional changes in property law, succession, and foreign policy to identify magisterial innovations presented as Lycurgan.',
    'If adaptation was marginal, the constraint is closer to a mountain with enforcement; if extensive, the tangled_rope reading is strongly supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_adaptation_scope, empirical, 'Scope of covert magisterial adaptation vs. textual fidelity').

omega_variable(
    demographic_causation,
    'Was demographic decline caused by the inflexibility of the Lycurgan framework (as the demographic trap reading claims) or by enforcement failures of the covert adaptation mechanism?',
    'Demographic and economic modeling of Spartan citizen numbers, land tenure patterns, and wealth concentration against institutional enforcement records.',
    'Resolves whether the constraint is best understood as a failed scaffold/piton (trap reading) or an active tangled rope whose adaptation mechanism broke down (this reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causation, empirical, 'Causal locus of Spartan demographic decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t8, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(lycu_tr_t16, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(lycu_tr_t24, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(lycu_tr_t32, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 32, 0.6).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 40, 0.72).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lycu_be_t8, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(lycu_be_t16, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(lycu_be_t24, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(lycu_be_t32, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lycu_su_t8, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(lycu_su_t16, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(lycu_su_t24, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(lycu_su_t32, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, demographic_trap_reading).

% DUAL FORMULATION NOTE:
% The lycurgan_laws kernel decomposes into three structurally distinct readings. This story (adaptive_fiction_reading) models the constraint as a tangled rope: genuine coordination through a fiction of immutability coupled with asymmetric extraction by the interpretive magistracy. The sacral_fidelity_reading treats the same kernel as a mountain or snare (sacred immutability). The demographic_trap_reading treats it as a piton or failed scaffold (brittle unrevisability causing collapse). Each reading has a different epsilon, stakeholder structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
