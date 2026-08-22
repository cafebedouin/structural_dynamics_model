% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident-Necessity Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This story instantiates the 'real_incident_necessity' reading of the
 *   competence_occupation kernel: the claim that only actual catastrophic
 *   incidents supply the authentic conditions needed to occupy the competence
 *   kernel for high-reliability operators. Under this reading, simulation and
 *   drills are structurally demoted to rehearsal that cannot itself confer
 *   verified competence — only surviving and performing during a genuine
 *   catastrophe can. Because catastrophes are rare, unwanted, and cannot be
 *   manufactured or requested, this reading generates an unresolvable
 *   maintenance problem: organizations must certify competence using a
 *   standard whose qualifying event they can neither produce safely nor
 *   ethically wish for. The reading privileges incident-experienced veterans
 *   and the institutions that investigate incidents, while imposing costs on
 *   simulation-trained operators, frontline crews, and the organizations that
 *   employ them, none of whom can close the credibility gap through effort or
 *   investment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.58).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.42).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, tangled_rope).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident-Necessity Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'b00d29d6-b044-40a3-8a72-cd32be55346a').
narrative_ontology:cs_kernel_codification('b00d29d6-b044-40a3-8a72-cd32be55346a', distributed).
narrative_ontology:cs_authority_grounding('b00d29d6-b044-40a3-8a72-cd32be55346a', practice).
narrative_ontology:cs_interpretation_layer_present('b00d29d6-b044-40a3-8a72-cd32be55346a').
narrative_ontology:cs_reading_relation('b00d29d6-b044-40a3-8a72-cd32be55346a', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('b00d29d6-b044-40a3-8a72-cd32be55346a', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('b00d29d6-b044-40a3-8a72-cd32be55346a', foundational, authenticity_requires_irreversible_stakes).
narrative_ontology:cs_axiom_status(authenticity_requires_irreversible_stakes, holdable).
narrative_ontology:cs_axiom_grounding('b00d29d6-b044-40a3-8a72-cd32be55346a', authenticity_requires_irreversible_stakes, empirically_contingent).
narrative_ontology:cs_axiom('b00d29d6-b044-40a3-8a72-cd32be55346a', secondary, simulated_stakes_are_categorically_non_transferable).
narrative_ontology:cs_axiom_status(simulated_stakes_are_categorically_non_transferable, holdable).
narrative_ontology:cs_axiom_grounding('b00d29d6-b044-40a3-8a72-cd32be55346a', simulated_stakes_are_categorically_non_transferable, empirically_contingent).
narrative_ontology:cs_reference_frame('b00d29d6-b044-40a3-8a72-cd32be55346a', pre_simulation_apprenticeship_model).
narrative_ontology:cs_drift_state('b00d29d6-b044-40a3-8a72-cd32be55346a', post_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b00d29d6-b044-40a3-8a72-cd32be55346a', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, veteran_operators_with_incident_experience).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, regulatory_credentialing_authorities).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, simulation_trained_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_crews_awaiting_authentic_exposure).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizations_facing_readiness_gaps).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, authentic_stakes_produce_irreplaceable_competence).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, simulation_fidelity_ceiling_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive authority and case material from post-incident analysis; their institutional relevance depends on real incidents remaining the recognized gold-standard evidence of competence, which increases the weight given to their findings over simulation-derived assessments.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_bodies, beneficiary,
    institutional, generational, analytical, national).

% Hold elevated status, promotion priority, and informal authority because they have 'been through it.' They set the tacit standard by which newer operators are judged and have structural incentive to maintain the belief that their kind of exposure cannot be substituted.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, veteran_operators_with_incident_experience, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, veteran_operators_with_incident_experience, agenda_setter).

% Write certification standards that implicitly or explicitly privilege incident-derived competence records over simulation records, administering the credentialing gate that decides who counts as demonstrably competent.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_credentialing_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Have completed extensive simulation and drill regimes but are structurally barred from being recognized as fully competent under this reading, since the kernel requires an authenticity condition they cannot manufacture without a catastrophe occurring on their watch. Cannot exit the organization's competence hierarchy without abandoning the career track.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_trained_operators, payer,
    moderate, biographical, trapped, national).

% Operate under the shadow of an unresolved competence question: they are told they are not fully proven until tested by a real event, which they can neither request nor avoid. Their day-to-day work is shaped by anxiety over an unattainable and undesirable qualifying condition.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_crews_awaiting_authentic_exposure, payer,
    powerless, immediate, trapped, local).

% Must staff safety-critical operations continuously while this reading declares that true readiness cannot be verified absent catastrophe. They bear liability, insurance, and reputational costs from operating with a workforce the reading itself calls unverifiable, and cannot resolve the gap through investment in training.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizations_facing_readiness_gaps, payer,
    powerful, generational, constrained, national).

% Study near-miss data, incident reconstructions, and simulation-transfer studies to assess whether the authenticity claim is empirically supportable or is serving as a status-preserving narrative for those who hold incident experience.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Names a real epistemic problem: high-consequence, low-frequency events may test capacities that low-stakes rehearsal cannot fully replicate, and organizations genuinely need some way to distinguish rehearsed competence from competence under real stakes.
% TRANSFER_FUNCTION: Moves credentialing authority, promotion priority, and institutional prestige toward those who have experienced real incidents and toward the bodies that investigate those incidents, while withholding full recognition of competence from simulation-trained operators and imposing unresolvable liability exposure on the organizations that employ them.
% ABSENT_VOICES: Simulation researchers and human-factors engineers who could present transfer-of-training evidence are rarely invited into the credentialing conversation; survivors of incidents who found their 'authentic' experience psychologically damaging rather than competence-conferring are also absent from the standard-setting room.
% DISAPPEARANCE_RATIONALE: Incident-experienced veterans and credentialing bodies would say the world rearranges catastrophically — competence verification collapses into pure guesswork. Simulation-trained operators and the organizations employing them would say the world barely changes, since simulation-based readiness assessment continues functioning as it already does day to day; the dispute is exactly whether the reading's claimed necessity is real or a status-preserving fiction.
% FOUNDING_PROBLEM: High-reliability organizations (nuclear plants, aviation, emergency medicine) observed that some operators who performed well in drills froze, panicked, or made fatal errors during actual catastrophic events, suggesting a gap between simulated and real competence that needed accounting for.
% FOUNDING_PROBLEM_CORROBORATION: Incident investigation bodies and veteran operators attest the gap is real and the founding problem remains live. Independent human-factors researchers outside the credentialing and investigation establishment report mixed and often contrary findings: several transfer-of-training studies find well-designed high-fidelity simulation predicts real-event performance about as well as prior real-event experience does, undermining the claim that only catastrophe can occupy the kernel.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the real transfer of status, promotion priority, and institutional legitimacy toward incident-experienced veterans and investigation bodies, at the expense of simulation-trained operators who cannot manufacture the qualifying condition. Suppression is moderate (0.42) — it is not primarily coercive but operates through credentialing gates and informal hierarchy rather than force. Theater ratio is substantial and rising (0.40 to 0.61) because as the reading persists without empirical vindication, an increasing share of the 'authenticity' discourse becomes institutional performance — post-incident review rituals, veteran-narrative privileging in training culture — rather than a demonstrated competence differential. Accessibility collapse is high (0.71): once an organization adopts this reading, there is no procedural path for a simulation-trained operator to be recognized as fully competent short of surviving an actual event, which collapses the space of legitimate alternatives almost completely for that population. Resistance is moderate-high (0.55) because human-factors researchers and simulation-trained operators actively contest the doctrine, unlike a genuine natural-law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators with incident experience and investigation bodies sit near the beneficiary end: the kernel reading, once adopted, directly elevates their standing and does not require them to do anything further to collect the benefit — their qualifying event has already occurred. Regulatory credentialing authorities administer the gate and benefit from the reading's legitimacy even though they do not personally hold incident status. Simulation-trained operators and frontline crews sit near the full-target end: they are trapped by career structure into a hierarchy that declares their preparation permanently second-tier, with no legitimate exit except an event they cannot ethically seek. Organizations bear diffuse structural cost — liability and readiness uncertainty — without being able to buy their way out through training investment, since the reading declares training investment structurally insufficient by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — some operators who perform well in drills fail under real stakes — is empirically real in at least some domains and should not be dismissed as pure invention; that is why this is authored as tangled_rope rather than snare. But the reading's inference from 'some real-event gap exists' to 'only real incidents can occupy the kernel' overshoots the evidence and forecloses cheaper, less catastrophic paths to competence verification (per the sibling readings). The tangled_rope classification captures both halves: a genuine coordination problem (verifying competence under stakes) riding alongside asymmetric extraction (status and legitimacy accruing to those who happened to survive an incident, at the durable expense of those who did not and structurally cannot).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_versus_status_preservation,
    'Does the real-incident-necessity claim track a genuine epistemic gap between simulated and real performance, or does it primarily function to preserve the status of incident-experienced veterans and the institutional relevance of investigation bodies?',
    'Controlled comparison of post-incident performance between operators with high-fidelity simulation training only versus operators with prior real-incident exposure, controlling for tenure and training hours; review of transfer-of-training literature across aviation, nuclear, and emergency medicine domains.',
    'If the gap is empirically small or absent once simulation fidelity is controlled for, the reading functions primarily as extraction dressed as necessity and the tangled_rope classification would tilt toward snare. If the gap is robust, the coordination function is stronger than currently authored and the reading is closer to a genuine (if tragic) epistemic constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_versus_status_preservation, empirical, 'Whether the authenticity claim is empirically grounded or status-preserving.').

omega_variable(
    unattainable_qualifying_condition,
    'Can a competence standard whose qualifying condition (a catastrophic incident) is something no organization can ethically seek or manufacture ever be considered a coherent coordination mechanism, or is its incoherence itself diagnostic of extraction?',
    'Conceptual analysis of whether any coordination mechanism can function as intended when its verifying event is categorically unwelcome and non-reproducible; compare to other domains where ''testing under real stakes'' is possible without catastrophe (e.g., live-fire military exercises with real but bounded risk).',
    'If no coherent coordination mechanism can rest on an unattainable qualifying event, this reading''s coordination claim is largely rhetorical and the effective structure is closer to pure extraction wearing coordination language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unattainable_qualifying_condition, conceptual, 'Whether an unattainable qualifying condition can ground a genuine coordination function.').

omega_variable(
    kernel_framing_alternative,
    'Is the kernel more defensibly framed as ''competence occupation requires exposure under genuine consequence'' (which simulation with real stakes might satisfy) rather than ''requires an actual catastrophic incident'' (which only literal disaster satisfies)? The two framings would classify this reading very differently.',
    'Trace which framing the source discourse (safety-critical industry training standards, incident review board language) actually uses versus which framing critics attribute to it; check whether industry standards ever explicitly require literal catastrophe versus merely high-consequence exercise.',
    'Under the narrower literal-catastrophe framing (adopted here), the reading has no viable beneficiary structure since catastrophes are categorically unacceptable, pushing the classification toward tangled_rope/snare boundary. Under a broader high-consequence-exposure framing, this reading would collapse into something closer to the hybrid_occupation reading and might classify as rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Alternative framing of the kernel''s core requirement and its classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__real_incident_necessity, theater_ratio, 4, 0.45).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.49).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__real_incident_necessity, theater_ratio, 12, 0.53).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.56).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.59).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t4, competence_occupation__real_incident_necessity, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(comp_be_t12, competence_occupation__real_incident_necessity, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_occupation__real_incident_necessity, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(comp_su_t8, competence_occupation__real_incident_necessity, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(comp_su_t12, competence_occupation__real_incident_necessity, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(comp_su_t16, competence_occupation__real_incident_necessity, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(comp_su_t24, competence_occupation__real_incident_necessity, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.1).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_occupation kernel. simulation_sufficiency holds that high-fidelity simulation alone suffices (lower extractiveness, viable beneficiary structure including simulation vendors and trained operators). hybrid_occupation holds that continuous multi-mechanism exercise is required without settled configuration (moderate extractiveness, distributed beneficiary structure across training-mechanism providers). This reading (real_incident_necessity) is the most extractive and least coherent of the three because its qualifying condition is categorically unwanted, producing no legitimate beneficiary who could ethically seek to trigger it. All three share the same underlying kernel question — what does it take to occupy demonstrated competence in a high-reliability domain — but each reading authors a structurally distinct ε and beneficiary/victim set, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
