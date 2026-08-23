% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Study of Sacrificial Law as Preparation for Messianic Restoration
 *   domain: religious/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   In Jewish law, the sacrificial order (Kodashim) remains biblically
 *   binding despite the Temple's destruction. The 'study as preparation'
 *   reading holds that the obligation persists in full force; study is not a
 *   substitute but a holding action that preserves the exact procedural
 *   knowledge for the moment of messianic restoration. Current practitioners
 *   bear the cost of mastering complex, inoperable laws — a deferred cosmic
 *   repair. The beneficiary is a future community that does not yet exist.
 *   The constraint is enforced by rabbinic authority and identity-lock; its
 *   extraction is low but structurally asymmetric.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.2).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.3).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Study of Sacrificial Law as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '258b9411-3375-4f65-b31a-add702d8fded').
narrative_ontology:cs_kernel_codification('258b9411-3375-4f65-b31a-add702d8fded', fixed_text).
narrative_ontology:cs_authority_grounding('258b9411-3375-4f65-b31a-add702d8fded', lineage).
narrative_ontology:cs_interpretation_layer_present('258b9411-3375-4f65-b31a-add702d8fded').
narrative_ontology:cs_reading_relation('258b9411-3375-4f65-b31a-add702d8fded', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_reading_relation('258b9411-3375-4f65-b31a-add702d8fded', kodashim_obligation__study_as_performance, forecloses).
narrative_ontology:cs_axiom('258b9411-3375-4f65-b31a-add702d8fded', foundational, sacrificial_law_binding_despite_impossibility).
narrative_ontology:cs_axiom_status(sacrificial_law_binding_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('258b9411-3375-4f65-b31a-add702d8fded', sacrificial_law_binding_despite_impossibility, theological).
narrative_ontology:cs_axiom('258b9411-3375-4f65-b31a-add702d8fded', foundational, study_as_preparation_for_restoration).
narrative_ontology:cs_axiom_status(study_as_preparation_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('258b9411-3375-4f65-b31a-add702d8fded', study_as_preparation_for_restoration, theological).
narrative_ontology:cs_reference_frame('258b9411-3375-4f65-b31a-add702d8fded', binding_law_awaiting_restoration).
narrative_ontology:cs_drift_state('258b9411-3375-4f65-b31a-add702d8fded', post_temple_destruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('258b9411-3375-4f65-b31a-add702d8fded', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_practitioners).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, torah_eternal_binding).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, messianic_restoration_certainty).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, study_preserves_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated to study intricate sacrificial laws (Kodashim) despite the Temple's destruction, investing significant time and cognitive effort with no immediate ritual payoff. Their religious identity fuses with this obligation, making exit psychologically and communally costly. They bear the cost of preserving a system they cannot enact.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_practitioners, payer,
    moderate, generational, identity_locked, global).

% The future restored community that will inherit the preserved technical knowledge and perform the sacrifices. They benefit from the current generation's study without bearing its cost. They do not yet exist and cannot influence the constraint.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_community, beneficiary,
    analytical, civilizational, analytical, universal).

% Define the curriculum, adjudicate the obligation's parameters, and enforce communal compliance through halakhic authority. They maintain the interpretation that study is a binding preparation, not a mere commemoration. Their institutional continuity depends on transmitting this obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Study Kodashim as historical, literary, and legal texts. They analyze the tradition from outside its normative claim, providing external corroboration or critique. Their exit is unconstrained; they bear no obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, academic_scholars, observer,
    analytical, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the exact technical knowledge required for Temple service across millennia of impossibility, ensuring that when the Messiah comes and the Temple is rebuilt, the priesthood can immediately resume the cosmic repair function without loss of transmission fidelity.
% TRANSFER_FUNCTION: Moves cognitive labor, time, and communal resources from current practitioners (who study, teach, and adjudicate the laws) to the messianic future community (who will enact them). The transfer is intergenerational and asymmetric: the present pays, the future receives.
% ABSENT_VOICES: The messianic future community is structurally absent — they cannot consent, object, or negotiate. Also absent are potential defectors in the current generation who would reject the obligation if not for identity-lock; their silence is enforced by communal pressure and internalized theology.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparation vanished, the technical knowledge would degrade within generations. Upon Temple restoration, the priesthood would lack the precise procedural knowledge to perform the cosmic repair function, breaking the chain of transmission that the constraint exists to maintain.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the sacrificial system — the core of Jewish cosmic repair — became physically impossible. The founding problem was how to keep the binding law alive and its knowledge intact during an indeterminate exile, so that restoration could be instantaneous when the messianic era arrives.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmudic sages themselves (e.g., Menachot 110a: 'Whoever studies the laws of sacrifice is as if he offered them') and by medieval codifiers (Maimonides, Hilkhot Beit HaBechirah). Modern historians of halakha (e.g., Haym Soloveitchik, 'Rupture and Reconstruction') corroborate from outside the beneficiary set that this study-obligation was a deliberate response to the Temple's loss, not an organic development.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.2, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the primary experience of practitioners is not resource extraction but cognitive devotion; the 'cost' is opportunity cost of study time, not material transfer. Suppression is moderate (0.3): communal and internalized pressure enforces the obligation, but alternatives (secular life, other Jewish movements) exist. Theater ratio is low (0.1): study is genuinely believed to be instrumentally necessary, not performative. Accessibility collapse is high (0.8): once the obligation is accepted as divine law, no alternative preserves the transmission chain. Resistance is low (0.2): the obligation is embraced as a core religious duty.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is a rope (pure coordination preserving knowledge). From the current practitioner seat, it is a tangled rope (coordination with asymmetric intergenerational transfer). From the messianic future seat (analytical), it is a mountain (the law simply is). The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Current practitioners are payers (d ≈ 0.8): they bear the study burden with no personal cosmic payoff, and identity-lock prevents exit. Messianic future is beneficiary (d ≈ 0.0): they receive the preserved knowledge without cost. Rabbinic authorities are agenda_setters (d ≈ 0.2): they administer the system and gain institutional legitimacy but do not extract materially. Academic observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by making the coordination function (transmission fidelity) explicit and the extraction asymmetric but low. If the messianic restoration never occurs, the constraint drifts toward piton (theatrical preservation). The founding problem remains live in the reading's own terms, so mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_structure,
    'How does this reading''s structural classification change if the messianic future is treated as a non-agent beneficiary versus a theological certainty?',
    'Compare classification outcomes when ''messianic_future_community'' is modeled as a beneficiary stakeholder versus when it is moved to vindicated_propositions as a theological claim.',
    'If the future community is a stakeholder beneficiary, the constraint is tangled_rope (asymmetric extraction). If it is a vindicated proposition, the constraint may classify as rope (pure coordination) because no agent extracts from another agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_structure, conceptual, 'Whether the messianic future is an agent-beneficiary or a vindicated proposition changes the beneficiary/victim structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression enforcing study primarily structural (communal sanctions) or internalized (theological identity fusion)?',
    'Longitudinal study of defectors from Orthodox communities: if suppression persists after leaving the community, it is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint''s extraction is carried by the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked religious obligations.').

omega_variable(
    extraction_future_discount,
    'What discount rate should apply to extraction that bears on the current generation but benefits a future that may never arrive?',
    'Intergenerational ethics frameworks (e.g., Ramsey discounting, zero discount for existential risks) applied to the constraint''s extractiveness metric.',
    'A zero discount rate keeps extractiveness at 0.2; a high discount rate would treat the benefit as negligible, raising effective extraction for current practitioners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_future_discount, preference, 'Ethical discounting of intergenerational transfer in religious obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.07).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(koda_tr_t1954, kodashim_obligation__study_as_preparation, theater_ratio, 1954, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.2).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement(koda_be_t1954, kodashim_obligation__study_as_preparation, base_extractiveness, 1954, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.28).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.32).
narrative_ontology:measurement(koda_su_t1954, kodashim_obligation__study_as_preparation, suppression_requirement, 1954, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, information_standard).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kodashim_obligation kernel. The kernel is the binding status of sacrificial law after the Temple's destruction. This reading (preparation) asserts the law remains binding and study preserves knowledge for future performance. The archive reading denies binding obligation; the performance reading asserts study itself enacts the cosmic function. The three readings have different ε values and victim/beneficiary structures, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
