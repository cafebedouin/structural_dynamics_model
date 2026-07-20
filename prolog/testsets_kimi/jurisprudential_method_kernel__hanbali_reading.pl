% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Literalist Jurisprudential Method
 *   domain: religious/legal_philosophy
 *
 * SUMMARY:
 *   The Hanbali reading of the jurisprudential method kernel holds that
 *   Islamic law derives exclusively from the literal text of the Qur'an,
 *   authenticated Hadith, and the opinions of the Prophet's Companions.
 *   Analogical reasoning (qiyas) and juristic preference (istihsan) are
 *   condemned as bid'ah (heretical innovation) that corrupt the divine
 *   kernel; only unanimous consensus (ijma) among scholars can validate
 *   derivative rulings. This constraint coordinates the Muslim community
 *   under a strict textualist legal framework while asymmetrically extracting
 *   from rationalist jurists, whose methodological tools are delegitimized,
 *   and from customary practitioners, whose local traditions are overridden.
 *   The constraint is claimed as a Mountain of divine origin but operates as
 *   a Tangled Rope: it provides genuine legal coordination while
 *   concentrating interpretive authority in textualist scholars through
 *   active suppression of alternative methodologies.
 *
 * KEY AGENTS:
 *   - Textualist scholars (agenda_setter/institutional/arbitrage): Define and enforce the literalist legal methodology, control certification and institutional access, and derive concentrated authority from the constraint.
 *   - Rationalist jurists (payer/moderate/constrained): Bear the cost of methodological delegitimization; their analogical tools are classified as bid'ah, reducing their opinions to non-authoritative status.
 *   - Customary practitioners (payer/powerless/identity_locked): Bear the cost of local tradition suppression; their identity-fused practices are overridden by textual derivation.
 *   - Muslim community (beneficiary/organized/constrained): Receive coordinated legal closure at the cost of methodological flexibility.
 *   - Comparative legal scholars (observer/analytical): Analyze the structural asymmetry from outside the normative framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.88).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.82).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Literalist Jurisprudential Method").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal_philosophy").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'd823c01c-5118-4ef8-aeb3-7eed67ca808c').
narrative_ontology:cs_kernel_codification('d823c01c-5118-4ef8-aeb3-7eed67ca808c', fixed_text).
narrative_ontology:cs_authority_grounding('d823c01c-5118-4ef8-aeb3-7eed67ca808c', lineage).
narrative_ontology:cs_interpretation_layer_present('d823c01c-5118-4ef8-aeb3-7eed67ca808c').
narrative_ontology:cs_reading_relation('d823c01c-5118-4ef8-aeb3-7eed67ca808c', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('d823c01c-5118-4ef8-aeb3-7eed67ca808c', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('d823c01c-5118-4ef8-aeb3-7eed67ca808c', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('d823c01c-5118-4ef8-aeb3-7eed67ca808c', foundational, qiyas_constitutes_bidah).
narrative_ontology:cs_axiom_status(qiyas_constitutes_bidah, holdable).
narrative_ontology:cs_axiom_grounding('d823c01c-5118-4ef8-aeb3-7eed67ca808c', qiyas_constitutes_bidah, theological).
narrative_ontology:cs_axiom('d823c01c-5118-4ef8-aeb3-7eed67ca808c', foundational, only_unanimous_consensus_valid).
narrative_ontology:cs_axiom_status(only_unanimous_consensus_valid, holdable).
narrative_ontology:cs_axiom_grounding('d823c01c-5118-4ef8-aeb3-7eed67ca808c', only_unanimous_consensus_valid, deontological).
narrative_ontology:cs_reference_frame('d823c01c-5118-4ef8-aeb3-7eed67ca808c', prophetic_textual_continuity).
narrative_ontology:cs_drift_state('d823c01c-5118-4ef8-aeb3-7eed67ca808c', contemporary_salafi_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d823c01c-5118-4ef8-aeb3-7eed67ca808c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, muslim_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the curriculum, certification, and adjudication of legal opinions within the Hanbali tradition. They restrict legitimate legal sources to literal Qur'anic text, authenticated Hadith, and Companion opinions, actively policing the boundary against analogical reasoning (qiyas) and juristic preference (istihsan) as heretical innovation. Their scholarly authority is concentrated and self-reinforcing through the claim of textual fidelity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Trained in analogical and preferential reasoning methods that extend divine law to unprecedented cases. Their methodologies are structurally delegitimized under the Hanbali reading, reducing their opinions to non-authoritative status. They must either abandon rationalist tools, practice in marginal spaces, or accept diminished institutional standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, generational, constrained, global).

% Maintain local legal and social customs (urf, amal) that diverge from literal textual derivation. Their practices are delegitimized as corrupting innovation when they conflict with textualist rulings, forcing abandonment of customary frameworks or exclusion from the recognized community of Islamic practice.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Receives a unified legal framework claimed to rest directly on divine revelation without the uncertainty of human rational extension. They depend on textualist scholars for authoritative access to the law and gain epistemic closure, though at the cost of flexibility in addressing novel or local circumstances.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, muslim_community, beneficiary,
    organized, generational, constrained, global).

% Analyze the structural consequences of the Hanbali literalist method, documenting the suppression of rationalist methodologies and the consolidation of interpretive authority in textualist institutions. They operate outside the normative commitments of the tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, allegedly divine legal framework by restricting legitimate jurisprudential sources to literal texts and authenticated reports, eliminating the uncertainty and disagreement introduced by analogical reasoning and local custom.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from rationalist jurists and customary practitioners to textualist scholars, concentrating the power to define Islamic law in those who control access to and authentication of texts.
% ABSENT_VOICES: Rationalist jurists from the Hanafi and Shafi'i traditions, customary legal practitioners from non-Hijazi regions, and lay Muslims whose local practices are overridden by textual derivation are structurally marginalized; their methodological objections are pre-emptively classified as bid'ah.
% DISAPPEARANCE_RATIONALE: If the Hanbali literalist constraint vanished, rationalist jurists would regain authoritative standing, customary practices would resurface as legitimate sources, and textualist scholars would lose their monopoly on interpretive legitimacy; the structure of Islamic legal authority would fragment across competing methodologies.
% FOUNDING_PROBLEM: The proliferation of conflicting legal opinions and methodologies in the early Islamic centuries created uncertainty about divine law and threatened communal unity; the literalist method was constructed to anchor law unambiguously in revealed texts.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars attest the problem is still live, citing persistent deviation. Rationalist jurists and historians of Islamic law attest that the founding fragmentation was resolved through the very methodological diversity the Hanbali reading suppresses; no neutral corroborating party exists outside the scholarly factions.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint decouples legal authority from rationalist methodological capacity and concentrates it in textual authentication controlled by a narrow scholarly class. Suppression is high (0.82) because the constraint's persistence depends on actively policing the boundary against qiyas, istihsan, and customary practice, not on participant preference. Theater ratio is moderate (0.45): the textual study function is genuine, but a substantial share of enforcement activity defends methodological exclusivity rather than legal substance. Accessibility collapse is high (0.80) because alternative methodologies lose nearly all legitimacy once the literalist frame is accepted; resistance is moderate (0.60) because rationalist jurists resist but are institutionally marginalized. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Textualist scholars experience the constraint as preserving divine law from human corruption; rationalist jurists experience it as an arbitrary barrier to legitimate legal reasoning; customary practitioners experience it as the erasure of living tradition. The engine computes this divergence from structural position: agenda-setters with arbitrage exit sit near the beneficiary end, while payers with constrained or identity-locked exit sit near the target end. The Muslim community occupies an intermediate seat, receiving coordination benefits while paying in lost flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars are structural beneficiaries (low d): they set the interpretive agenda, control institutional access, and derive authority from the constraint's operation. Rationalist jurists are structural targets (high d): their methodologies are explicitly delegitimized and they bear the cost of exclusion. Customary practitioners are also targets (high d): their identity-locked local practices are overridden by textual derivation. The Muslim community sits at intermediate d: they receive coordination benefits but lose methodological flexibility. No overrides are needed because the derivation chain produces accurate directionality from the declared beneficiary/victim structure and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali literalist method was built to solve the founding problem of legal uncertainty and innovation in the early community. However, the founding problem status is contested: alternative readings (Hanafi, Maliki, Shafi'i) attest that the problem was solved through methodological pluralism, not textual exclusivity. The method persists not because the founding problem remains live in its original form, but because it has become an authority-preservation mechanism for textualist institutions. The R5 genealogy flags this as a potential mandatrophy: a coordination mechanism whose founding rationale has shifted to authority maintenance. The Tangled Rope classification captures this by requiring both genuine coordination and asymmetric extraction, preventing misclassification as pure Rope (the textualists' claim) or pure Snare (the rationalists' claim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanbali_reading_kernel_placement,
    'Does the Hanbali literalist reading instantiate a genuine epistemic constraint on interpretation, or does it function as an institutional mechanism for concentrating authority in textualist scholars?',
    'Comparative analysis of legal outcomes across madhhabs for identical cases; if literalist derivation consistently produces outcomes that benefit the textualist scholarly class at the expense of lay flexibility, the reading functions as extraction.',
    'Would reclassify from tangled_rope to snare if the coordination function is found to be entirely epiphenomenal to authority concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hanbali_reading_kernel_placement, conceptual, 'Whether the Hanbali reading is primarily epistemic or extractive.').

omega_variable(
    unanimous_consensus_impossibility,
    'The Hanbali reading demands unanimous consensus (ijma) for validity, yet the historical record shows persistent methodological disagreement; does this create a performative gap between claimed epistemic standard and actual practice?',
    'Historical survey of claimed ijma instances versus actual dissent records; measure the frequency with which unanimity is asserted while dissent is suppressed or excluded from the historical archive.',
    'Would raise theater_ratio and indicate that the unanimity requirement is a theatrical boundary rather than a genuine epistemic filter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unanimous_consensus_impossibility, empirical, 'Whether unanimous consensus is historically achieved or performatively asserted.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of analogical reasoning structural (institutional exclusion of rationalist jurists) or internalized (jurists self-censoring to avoid bid''ah designation)?',
    'Analysis of jurists who shifted from rationalist to textualist methods: whether the shift was incentivized by institutional rewards and threats (structural) or by genuine epistemic conversion (internalized).',
    'If primarily internalized, effective suppression exceeds the institutional measure and the constraint operates more deeply than external enforcement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of rationalist methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.35).
narrative_ontology:measurement(juri_tr_t800, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1000, 0.42).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.45).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 400, 0.7).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.74).
narrative_ontology:measurement(juri_be_t800, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 800, 0.78).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1000, 0.84).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(juri_su_t800, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 800, 0.72).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1000, 0.78).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
