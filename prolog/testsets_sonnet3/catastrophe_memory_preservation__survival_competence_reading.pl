% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe-Memory Ritual as Survival-Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the survival-competence reading of the
 *   catastrophe-memory-preservation kernel: the claim that a recurring,
 *   costly community ritual functions as an operational transfer mechanism,
 *   embedding threat-recognition cues and response sequences in bodies and
 *   social roles across a time-span exceeding individual memory. On this
 *   reading the ritual is not merely commemorative — its drill content is
 *   load-bearing for future survival, which licenses (from within the
 *   reading) the high cost it imposes on present participants, especially
 *   initiates. The sibling readings (mourning_practice_reading: the ritual is
 *   symbolic continuity with no operational content; hybrid_atrophy_reading:
 *   it once had operational content but has degraded to mourning-practice
 *   under modernity) describe structurally different constraints with
 *   different ε values and are not blended into this one — see cs_structure
 *   and omegas for the committer routing.
 *
 * KEY AGENTS:
 *   - elder_ritual_custodians: administer and enforce the ritual cycle, institutional power, identity-locked to the custodial role
 *   - future_generation_survival_capacity: the invoked non-agent beneficiary interest whose existence justifies present cost
 *   - present_generation_participants: moderate-power adults bearing annual transfer cost, constrained exit
 *   - ritually_noncompliant_youth: powerless initiates bearing the sharpest initiation costs, trapped exit
 *   - regional_hazard_scientists: excluded organized actors holding the data that could confirm or falsify the operational-transfer claim
 *   - ethnographic_observer: analytical observer with no power to alter the ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.71).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe-Memory Ritual as Survival-Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '2f38112c-8191-4445-ab58-907e0e3f024c').
narrative_ontology:cs_kernel_codification('2f38112c-8191-4445-ab58-907e0e3f024c', implicit).
narrative_ontology:cs_authority_grounding('2f38112c-8191-4445-ab58-907e0e3f024c', practice).
narrative_ontology:cs_interpretation_layer_present('2f38112c-8191-4445-ab58-907e0e3f024c').
narrative_ontology:cs_reading_relation('2f38112c-8191-4445-ab58-907e0e3f024c', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_reading_relation('2f38112c-8191-4445-ab58-907e0e3f024c', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('2f38112c-8191-4445-ab58-907e0e3f024c', foundational, ritual_content_encodes_live_operational_knowledge).
narrative_ontology:cs_axiom_status(ritual_content_encodes_live_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('2f38112c-8191-4445-ab58-907e0e3f024c', ritual_content_encodes_live_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('2f38112c-8191-4445-ab58-907e0e3f024c', foundational, present_autonomy_subordinate_to_future_survival_capacity).
narrative_ontology:cs_axiom_status(present_autonomy_subordinate_to_future_survival_capacity, holdable).
narrative_ontology:cs_axiom_grounding('2f38112c-8191-4445-ab58-907e0e3f024c', present_autonomy_subordinate_to_future_survival_capacity, instrumental).
narrative_ontology:cs_reference_frame('2f38112c-8191-4445-ab58-907e0e3f024c', post_catastrophe_founding_transmission).
narrative_ontology:cs_drift_state('2f38112c-8191-4445-ab58-907e0e3f024c', contemporary_instrumented_hazard_monitoring_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f38112c-8191-4445-ab58-907e0e3f024c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generation_survival_capacity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, elder_ritual_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, ritually_noncompliant_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the annual catastrophe-commemoration cycle — the drill-embedded reenactment of flood, famine, or invasion warning signs. They set the calendar, decide who is initiated into the full sequence, and enforce participation through social sanction. Their own standing depends on being the ones who remember correctly; they cannot easily exit the role without dissolving the office itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, elder_ritual_custodians, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, elder_ritual_custodians, beneficiary).

% Not a present actor but the invoked interest: descendants who have not yet faced the recurring hazard the ritual encodes (flood cycle, volcanic dormancy period, famine interval). The ritual's costly repetition is justified by the claim that operational recognition skills — reading the warning signs, executing the response sequence — must be embodied and re-embodied to survive dormant intervals exceeding a human lifetime.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generation_survival_capacity, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, future_generation_survival_capacity).

% Adults who must annually surrender labor time, submit to the emotionally taxing reenactment (which fuses genuine grief for past dead with rehearsed threat-drill), and accept the custodians' authority over correct performance. Declining full participation costs standing in the community; leaving the community entirely is the only clean exit, which severs other kin and economic ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, constrained, local).

% Adolescents undergoing initiation into the full ritual sequence. They bear the sharpest costs — extended fasting, sleep-disrupted vigils, physically demanding drill components — with no say in whether the sequence still matches actual hazard patterns. Refusal marks them as unfit for adult standing; they have no meaningful capacity to renegotiate the terms of initiation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritually_noncompliant_youth, payer,
    powerless, biographical, trapped, local).

% Geologists or hydrologists who have modeled the actual recurrence interval and warning-sign profile of the hazard the ritual claims to encode. They are not consulted by the custodians and have no channel to update the ritual's drilled content when instrumental data diverges from traditional signs; their findings would either validate or falsify the operational-transfer claim central to this reading.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, regional_hazard_scientists, excluded,
    organized, generational, mobile, regional).

% Studies the ritual's content and asks whether the drilled sequence still functions as operational threat-recognition or has drifted into symbolic performance. Takes testimony from custodians, participants, and hazard scientists without power to alter the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ethnographic_observer, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits, across gaps exceeding individual memory span, the specific embodied recognition sequence for a recurring low-frequency, high-consequence hazard — which environmental cues precede the event and which response sequence to execute — so that when the hazard recurs no living person need improvise from zero.
% TRANSFER_FUNCTION: Moves costly present-generation time, labor, physical hardship, and emotional exposure into a standing capacity (embodied recognition skill, rehearsed response sequence) that is banked against a future hazard recurrence that may fall outside any living participant's lifetime.
% ABSENT_VOICES: Regional hazard scientists whose instrumental data could confirm or falsify whether the drilled signs and response sequence still track the actual hazard profile are structurally outside the custodial process; ritually noncompliant youth who bear the sharpest initiation costs have no forum to contest the sequence's content or necessity.
% DISAPPEARANCE_RATIONALE: Custodians and this reading hold that disappearance would leave the community with no embodied recognition sequence when the hazard recurs, producing the kind of catastrophic naive-first-encounter losses the ritual is designed to prevent — the world rearranges catastrophically at the next hazard event, not immediately. Present-generation participants and hazard scientists contest this: if the drilled content has drifted from the actual hazard signature, disappearance would cost little operationally while immediately relieving the annual burden — making the verdict itself a live dispute rather than settled fact.
% FOUNDING_PROBLEM: A prior catastrophic event (flood, eruption, famine, raid) killed or displaced a generation that had no forewarning; survivors instituted a repeated reenactment to ensure the next generation would recognize the precursor signs and execute a survival response before the event reached full force.
% FOUNDING_PROBLEM_CORROBORATION: Elder custodians attest the hazard interval remains live and the drilled sequence still matches it, citing oral tradition and selective incident recall. Regional hazard scientists, examining instrumental records independent of the custodial tradition, report the hazard's actual signature and recurrence interval diverge in specific respects from the drilled sequence — an external, non-beneficiary source that unsettles rather than confirms the custodians' claim of continued operational fidelity.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because, on this reading, the ritual demands substantial present-generation labor, physical hardship, and emotional exposure as the price of a claimed future benefit that no living participant will necessarily see realized or verified. Suppression is authored moderate-high (0.62) because participation is enforced through social sanction and initiation is compulsory for adult standing, but genuine exit exists (leaving the community) even if costly — it is not a fully closed trap for adults, though it is closer to closed for initiated youth. Theater ratio is authored low-moderate (0.28) and rising: some drift toward performative execution is present even within this reading's own frame, but the reading's core claim is that the operational content remains substantially intact, distinguishing it structurally from hybrid_atrophy_reading's claim of predominant drift.
 *
 * PERSPECTIVAL GAP:
 *   Custodians (agenda_setter, institutional power, identity-locked exit) experience the ritual as necessary stewardship of a survival capacity they are entrusted to maintain. Present-generation participants and especially noncompliant youth (payer roles, constrained/trapped exit) experience the same structure as compulsory extraction of present autonomy for a benefit accruing to people not yet born. The engine should compute divergent seat types from this asymmetry in exit options and power alone, without any adjustment to the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder custodians sit near the beneficiary end: they administer the mechanism and their social standing is itself a return on the arrangement. Future-generation survival capacity is declared a beneficiary but is a non-agent (agent: false) — it collects no rents and is excluded from directionality computation; it exists in the data to ground the coordination-function claim, not to distort beneficiary derivation. Present-generation participants and ritually noncompliant youth are declared victims: they bear the transfer cost now, with youth nearer the full-target end given their trapped exit and lack of any voice in sequence content.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (naive first-encounter catastrophe) as contested-but-still-plausibly-live: the hazard interval may exceed living memory, so absence of a recent incident is not evidence the problem is dead. This is precisely the ambiguity that separates this reading from hybrid_atrophy_reading, which holds the operational function is dead while the ritual persists. Classifying this reading as tangled_rope (rather than snare) preserves the possibility that the coordination function is real and the extraction is the necessary cost of maintaining a capacity that cannot be maintained cheaply — while the divergent founding_problem_corroboration (hazard scientists reporting drift) keeps the door open to the alternative reading without collapsing this story into it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_content_still_live,
    'Does the drilled ritual sequence still encode threat-recognition cues and response actions that match the actual hazard''s current signature, or has the sequence drifted from operational fidelity while retaining its social form?',
    'Compare the ritual''s drilled precursor-signs and response sequence against instrumental hazard records (seismic, hydrological, or climatic data as applicable) maintained independently by regional_hazard_scientists; assess divergence over the recorded interval.',
    'If the drilled content matches current hazard science closely, this reading (survival_competence_reading) is well-supported and the high extraction is coordination cost for a genuine capacity. If it has diverged substantially, the constraint is structurally closer to hybrid_atrophy_reading or even a snare dressed in survival language — the same observable ritual would then instantiate a different constraint under a different reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_content_still_live, empirical, 'Whether the ritual''s operational content remains synchronized with the actual hazard it claims to encode.').

omega_variable(
    kernel_reading_selection_basis,
    'What determines which reading of the catastrophe_memory_preservation kernel an observer or participant adopts — is it evidence about operational fidelity, institutional position (custodian vs. participant), or prior commitment to the value of ritual continuity itself?',
    'Compare stated justifications across custodians, participants, and external hazard scientists for why they favor one reading; look for whether reading choice tracks evidence exposure or tracks structural position (who benefits from which reading being accepted).',
    'If reading choice tracks structural position more than evidence, that is itself evidence the survival_competence_reading may function partly as legitimating cover for custodial authority regardless of its operational truth-value — without this collapsing the reading into hybrid_atrophy_reading or mourning_practice_reading as a separate empirical matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether reading-selection among the kernel''s three readings is evidence-driven or position-driven.').

omega_variable(
    youth_initiation_cost_proportionality,
    'Is the cost imposed on ritually_noncompliant_youth during initiation proportional to any plausible operational-transfer benefit, or does it exceed what embodied-skill transfer would require even under the most generous reading of the ritual''s function?',
    'Compare initiation intensity and duration against comparable, non-ritual skill-transfer regimes for equivalent hazard-response competencies (e.g., wilderness survival training, disaster-response drilling) to establish whether the cost is calibrated to pedagogical need or exceeds it.',
    'If costs are disproportionate to any operational-transfer requirement, that excess is extraction not explained by this reading''s own coordination claim, and would push the constraint toward snare for the youth seat specifically even while the reading holds for adult participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_initiation_cost_proportionality, empirical, 'Whether initiation cost for youth exceeds what operational skill transfer would require.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating readings of the catastrophe_memory_preservation kernel. survival_competence_reading (this file) asserts high, still-live operational transfer and authors high extractiveness with a tangled_rope claim. mourning_practice_reading asserts the ritual never carried operational content and authors much lower extractiveness with a rope-shaped claim. hybrid_atrophy_reading asserts the operational function was once real but has decayed under modernity, authoring rising theater_ratio and a piton-leaning claim. The three share the same observable ritual but differ in what they assert about its causal efficacy; each carries its own ε and stakeholder structure per the ε-invariance principle, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
