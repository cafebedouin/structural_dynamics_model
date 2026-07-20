% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Createdness of the Qur'an (makhlÅ«q) â Rationalist Theological Reading
 *   domain: theological/political_authority
 *
 * SUMMARY:
 *   This constraint instantiates the created_reading of the
 *   quran_ontological_status kernel. It treats the doctrine that the Qur'an
 *   is created speech (makhlÅ«q) as a structural commitment system grounded
 *   in rationalist theology. The kernel is the Qur'an's ontological status;
 *   this reading claims that status is created and therefore interpretively
 *   flexible, preserving divine transcendence. Sibling readings are the
 *   uncreated_reading (eternal divine speech, mountain-like) and the
 *   state_enforced_creation_reading (this doctrine plus state inquisition).
 *   This reading isolates the doctrinal structure from state coercion to
 *   measure its intrinsic extraction and coordination properties.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: Agenda-setter (organized/global) â formulates and defends the createdness doctrine; gains hermeneutic authority and institutional control over legitimate interpretation
 *   - traditionalist_jurists: Primary target (organized/global, identity-locked) â authority derived from textual fixity is undermined by the ontological downgrade of the text
 *   - literalist_communities: Secondary target (moderate/regional, identity-locked) â direct ontic relationship to divine speech is severed
 *   - philosophical_schools: Beneficiary (organized/continental, mobile) â gains license for philosophical hermeneutics
 *   - reform_movements: Beneficiary (moderate/global, mobile) â gains doctrinal flexibility for reform
 *   - uncreated_reading_adherents: Excluded voice (organized/global) â structurally barred from rationalist theological institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.58).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.52).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Createdness of the Qur'an (makhlÅ«q) â Rationalist Theological Reading").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0').
narrative_ontology:cs_kernel_codification('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', fixed_text).
narrative_ontology:cs_authority_grounding('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', expertise).
narrative_ontology:cs_interpretation_layer_present('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0').
narrative_ontology:cs_reading_relation('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', foundational, divine_speech_created_status).
narrative_ontology:cs_axiom_status(divine_speech_created_status, holdable).
narrative_ontology:cs_axiom_grounding('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', divine_speech_created_status, deontological).
narrative_ontology:cs_axiom('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', foundational, rational_hermeneutic_license).
narrative_ontology:cs_axiom_status(rational_hermeneutic_license, holdable).
narrative_ontology:cs_axiom_grounding('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', rational_hermeneutic_license, instrumental).
narrative_ontology:cs_reference_frame('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', rational_theology_framework).
narrative_ontology:cs_drift_state('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', post_mihna_sunni_orthodoxy, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7316e64e-9ebf-4d4b-b9f1-21c06fc82ac0', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_transcendence_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, rational_theology_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulate, teach, and defend the doctrine that the Qur'an is created speech (makhlÅ«q) as a necessary implication of divine transcendence (tawhid). Derive hermeneutic authority from the claim that reason must adjudicate the attributes of God, which makes the text historically and interpretively flexible. Administer the boundaries of legitimate theology by ruling the uncreated reading rationally incoherent.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter,
    organized, generational, constrained, global).

% Derive religious and juristic authority from the textual fixity and unmediated divine origin of the Qur'an. Under the createdness doctrine, their chain of authority is cut at the root: if the text is created, literal transmission and fixed textual rulings lose their claim to uncreated divine backing. Their professional identity is fused with the uncreated reading; exit means abandoning the epistemic foundation of their legal methodology.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    organized, generational, identity_locked, global).

% Understand their relationship with God as mediated by direct, uncreated divine speech. The createdness doctrine reframes the Qur'an as a temporal artifact, which severs the immediate ontic link they believe exists between the believer and the divine word. Exit is existentially costly because their communal identity is constituted by literal adherence to the text as divine substance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    moderate, biographical, identity_locked, regional).

% Gain license to apply philosophical methods â logic, metaphysics, linguistic analysis â to the Qur'an once it is classed as created temporal speech rather than co-eternal divine substance. Their intellectual program depends on the doctrinal separation between God and text.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, civilizational, mobile, continental).

% Use interpretive flexibility licensed by the createdness doctrine to advance theological and social reforms that a fixed, uncreated text would block. They depend on the rationalist framework but are less institutionally anchored than the classical theologians.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, biographical, mobile, global).

% Hold the uncreated reading of the Qur'an as eternal divine speech. They are present in the broader Islamic discourse but structurally excluded from rationalist institutions where the createdness doctrine sets the boundaries of legitimate theology. Their arguments are ruled out of bounds a priori within the rationalist framework.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, uncreated_reading_adherents, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves absolute divine transcendence (tawhid) by preventing any temporal artifact, including the Qur'anic text, from being co-eternal with God; enables rational theological coordination by making revelation historically situated and open to interpretive adjustment rather than ontically fixed.
% TRANSFER_FUNCTION: Moves hermeneutic authority and institutional prestige from literalist textual custodians and traditional juridical chains to rationalist theologians and philosophical interpreters; moves the ontological status of the Qur'an from uncreated divine substance to created speech open to rational analysis.
% ABSENT_VOICES: Traditionalist jurists and literalist communities who hold the uncreated reading are epistemically present in the broader Islamic world but excluded from rationalist theological institutions; their objections are ruled illegitimate a priori within the rationalist framework, constituting the absent voice inside rationalist deliberation.
% DISAPPEARANCE_RATIONALE: If the createdness doctrine vanished, traditionalist jurisprudence would reclaim textual fixity as the foundation of law, philosophical interpretation would lose its license to apply non-literal methods, and the boundary between rationalist and traditionalist theology would collapse into a configuration where the uncreated reading dominates orthodox institutions.
% FOUNDING_PROBLEM: The theological paradox that an uncreated, co-eternal Qur'an threatens absolute divine unity (tawhid) by introducing multiplicity, temporality, and finitude into the divine essence.
% FOUNDING_PROBLEM_CORROBORATION: Mu'tazilite rationalist theologians attest the problem from within their framework; traditionalist jurists attest it is a pseudo-problem generated by Hellenistic philosophical importation rather than indigenous to revelation. Historians of religion corroborate that the problem parallels earlier Christian Christological and logos debates, suggesting the paradox emerged through inter-religious philosophical contact rather than arising spontaneously from early Islamic discourse.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the doctrine transfers significant hermeneutic authority and institutional status from traditionalists to rationalists, but the transfer is symbolic and epistemic rather than material. Suppression is moderate (0.52) because the uncreated reading is delegitimized and excluded from rationalist institutions, though this reading does not include state coercion. Theater_ratio is low-moderate (0.28): rationalist arguments are genuine philosophical theology, but defensive apologetics under traditionalist pressure introduce performative elements. Resistance is high (0.72) because traditionalist jurists mounted sustained, organized opposition (e.g., Ibn Hanbal). Accessibility_collapse is moderate (0.45): the uncreated reading persists as a live alternative in the broader tradition but collapses within rationalist institutional boundaries.
 *
 * PERSPECTIVAL GAP:
 *   Rationalist theologians experience the constraint as a necessary logical implication of tawhid â a coordination mechanism that preserves divine unity. Traditionalist jurists experience it as an alien philosophical imposition that destroys the ontic foundation of Islamic law. Literalist communities experience it as severing their direct, unmediated relationship with the divine word. The engine computes this divergence from the same structural data: beneficiaries with constrained exit versus identity-locked targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, philosophical schools, and reform movements are structural beneficiaries: the doctrine subsidizes their authority and interpretive freedom, pushing their directionality toward the beneficiary pole. Traditionalist jurists and literalist communities are targets: the doctrine extracts their authority and ontic security, and their identity-locked exit options amplify their effective extraction. Uncreated-reading adherents are excluded rather than coordinated, sitting at the far target end of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling pure coordination as extraction (or vice versa) by exhibiting both a genuine coordination function â preserving divine transcendence and enabling rational theology â and identifiable asymmetric extraction â traditionalist jurists and literalist communities bear the costs of displaced authority and ontic alienation. Without the coordination function, the doctrine would be a snare; without the victim asymmetry, it would be a rope. The tangled_rope classification captures the hybrid nature. The temporal measurements show slow metric substitution (theater_ratio rising from 0.15 to 0.32), indicating that over time performative defense of the doctrine partially replaces its original theological function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_transcendence_implication_vs_construction,
    'Is the createdness of the Qur''an a necessary logical implication of divine transcendence (mountain-like), or a constructed theological position serving rationalist epistemic interests?',
    'Comparative historical theology: if all rigorous monotheisms encountering Hellenistic philosophy independently generate created-revelation doctrines, the implication is structurally natural; if the doctrine appears only where rationalist schools gain institutional leverage, it is constructed.',
    'If natural, the constraint approaches mountain status for rationalist seats and the extraction metric is mis-specified; if constructed, the current tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_transcendence_implication_vs_construction, conceptual, 'Whether createdness is natural entailment or constructed position').

omega_variable(
    authority_displacement_vs_extraction,
    'Does the loss of authority by traditionalist jurists constitute extraction (a structural transfer of status through the constraint) or a legitimate reallocation following open intellectual contestation?',
    'Examine whether traditionalist jurists are institutionally excluded from theological production, education, and legitimacy under the rationalist framework, versus merely losing scholarly debates on equal footing.',
    'If exclusionary, victimhood and extractiveness are higher; if contestational, the constraint may recompute toward rope with asymmetric non-extractive costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_displacement_vs_extraction, conceptual, 'Whether traditionalist authority loss is extractive displacement or debate outcome').

omega_variable(
    kernel_reading_sibling_boundary,
    'This constraint instantiates the created_reading of the quran_ontological_status kernel. How robust is the structural boundary between this doctrinal reading and the state_enforced_creation_reading sibling?',
    'Compare metric profiles across the two constraint stories: if adding state enforcement does not significantly raise suppression or extractiveness, the doctrinal reading already carries the coercive structure; if it does, the boundary is valid.',
    'Would validate or dissolve the separation between doctrinal and state-enforced variants in the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Structural boundary between doctrinal and state-enforced createdness readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_created_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(quran_created_tr_t10, quran_ontological_status__created_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(quran_created_tr_t20, quran_ontological_status__created_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(quran_created_tr_t30, quran_ontological_status__created_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(quran_created_tr_t40, quran_ontological_status__created_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(quran_created_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(quran_created_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(quran_created_be_t10, quran_ontological_status__created_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(quran_created_be_t20, quran_ontological_status__created_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(quran_created_be_t30, quran_ontological_status__created_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(quran_created_be_t40, quran_ontological_status__created_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(quran_created_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_ontological_status__created_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel decomposes into three structurally distinct constraints. The uncreated_reading treats the Qur'an as ontic mountain (negligible extraction, no beneficiaries). The created_reading treated here isolates the doctrinal claim, revealing hybrid coordination-extraction. The state_enforced_creation_reading adds coercive institutional enforcement to the same doctrinal claim, independently raising suppression and potentially changing the computed classification. They are linked by doctrinal ancestry but have different Îµ values and must be evaluated separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
