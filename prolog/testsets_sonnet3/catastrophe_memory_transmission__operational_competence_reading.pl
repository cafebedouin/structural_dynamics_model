% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Ritual as Transmission Mechanism for Catastrophe-Survival Competence
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the operational_competence_reading of the
 *   catastrophe_memory_transmission kernel: ritual as a rehearsal mechanism
 *   that encodes and transmits survival-relevant procedural knowledge
 *   (rapid-departure logistics at Passover, scarcity tolerance at Tisha B'Av)
 *   across generations who did not directly experience the originating
 *   catastrophe. Under this reading the ritual is evaluated by operational
 *   yield — does performing it actually produce or maintain competence in the
 *   population that performs it — rather than by its symbolic or
 *   identity-preserving function, which are the concerns of the sibling
 *   readings (symbol_continuity_reading, hybrid_embedded_reading) authored as
 *   separate constraints. The rising theater_ratio over the interval models a
 *   real drift this reading predicts: as originating catastrophes recede in
 *   living memory and communities urbanize/stabilize, the operational
 *   rehearsal component atrophies relative to the symbolic performance
 *   component, even while the ritual form is preserved unchanged. This is the
 *   reading's own account of degradation, not a claim about the sibling
 *   readings' trajectories.
 *
 * KEY AGENTS:
 *   - future_community_members: beneficiary of transmitted procedural competence
 *   - household_heads_coordinating_response: agenda_setter administering the rehearsal cycle
 *   - diaspora_communities_facing_recurring_threat: organized beneficiary maintaining distributed readiness
 *   - literalist_practitioners_mistaking_symbol_for_substance: payer bearing opportunity cost of inert performance
 *   - ritual_specialists_and_communal_elders: agenda_setter shaping pedagogical emphasis
 *   - comparative_ritual_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual as Transmission Mechanism for Catastrophe-Survival Competence").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '0d1b6f61-61b2-4fd7-b7fd-4c81a639af64').
narrative_ontology:cs_kernel_codification('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', implicit).
narrative_ontology:cs_authority_grounding('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', practice).
narrative_ontology:cs_interpretation_layer_present('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64').
narrative_ontology:cs_reading_relation('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', foundational, operational_content_is_separable_from_symbolic_form).
narrative_ontology:cs_axiom_status(operational_content_is_separable_from_symbolic_form, holdable).
narrative_ontology:cs_axiom_grounding('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', operational_content_is_separable_from_symbolic_form, empirically_contingent).
narrative_ontology:cs_axiom('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', foundational, ritual_value_is_measured_by_downstream_survival_yield).
narrative_ontology:cs_axiom_status(ritual_value_is_measured_by_downstream_survival_yield, holdable).
narrative_ontology:cs_axiom_grounding('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', ritual_value_is_measured_by_downstream_survival_yield, instrumental).
narrative_ontology:cs_reference_frame('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', operational_rehearsal_as_primary_function).
narrative_ontology:cs_drift_state('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', contemporary_diaspora_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d1b6f61-61b2-4fd7-b7fd-4c81a639af64', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, household_heads_coordinating_response).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities_facing_recurring_threat).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, ritual_as_encoded_procedural_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit a rehearsed repertoire of rapid-departure logistics, resource-scarcity discipline, and threat-recognition cues without having lived through the originating catastrophe themselves. They receive the competence encoded in the ritual sequence — what to pack, how fast, what to prioritize — as procedural memory transmitted through repeated performance rather than as a lecture or manual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_community_members, beneficiary,
    moderate, generational, constrained, regional).

% Run the ritual within their household each cycle, sequencing tasks (packing, portioning, timing) that double as the coordination drill. They benefit from the rehearsed competence directly and also administer the transmission, deciding how strictly the operational elements are enacted versus abbreviated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, household_heads_coordinating_response, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, household_heads_coordinating_response, beneficiary).

% Dispersed communities repeatedly facing expulsion, displacement, or resource denial across centuries use the ritual calendar as a distributed rehearsal infrastructure — no central authority is needed to keep the competence current because every household re-performs it. Their exit from the practice would mean losing a low-cost, self-renewing readiness mechanism with no equivalent replacement readily available.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities_facing_recurring_threat, beneficiary,
    organized, civilizational, constrained, global).

% Perform the ritual acts as fixed symbolic obligation — correct words, correct objects, correct timing — without engaging the underlying operational content (why the bread is unleavened, why the meal is eaten standing and ready to move, why fasting builds tolerance for scarcity). They pay an opportunity cost: the transmission channel is present but the competence payload is not extracted, leaving them with the form and without the readiness the form was built to carry.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance, payer,
    powerless, biographical, trapped, local).

% Shape how the ritual sequence is taught and emphasized — whether the operational rehearsal (pace, resource handling, threat cues) or the purely symbolic reading is foregrounded in instruction. Their choices about pedagogical emphasis determine whether the next generation receives functioning competence or inert form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_specialists_and_communal_elders, agenda_setter,
    organized, generational, constrained, regional).

% Study the ritual cross-culturally to assess whether its structure functions as an operational transmission mechanism (matching catastrophe-response drills in other survival contexts) or is better explained by other readings of the same kernel. Their analysis does not participate in the ritual's benefit or cost flows.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, comparative_ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual sequence coordinates a community's rehearsal of rapid-departure logistics, scarcity resource management, and threat-pattern recognition at a fixed, low-cost, self-renewing cadence — solving the problem of keeping catastrophe-response competence current across generations who did not experience the originating event.
% TRANSFER_FUNCTION: Moves procedural competence (packing speed, resource triage, hazard recognition) from those who encoded it into the ritual form toward future practitioners, transmitted through repeated embodied performance rather than explicit instruction; the cost is the time and discipline of performance, paid mainly by those who perform the ritual as empty form and receive no competence payload in return.
% ABSENT_VOICES: Practitioners who experience the ritual as purely symbolic obligation are rarely in a position to articulate what they are missing, since the operational content is by design non-propositional — it is not a claim they can dispute, only a competence they either absorb through practice or do not. Their absence from the conversation about the ritual's function is structural, not a matter of exclusion by another party.
% DISAPPEARANCE_RATIONALE: If the ritual practice vanished overnight, communities that treat it as the operative transmission channel would lose a distributed, low-overhead readiness mechanism — no replacement drill exists at comparable cost or reach — while communities for whom the same practice is already inert symbolic performance would notice little functional change, since they were not extracting the operational payload regardless of the ritual's continuation. Whether disappearance rearranges the world or leaves it unchanged depends on which reading of the ritual's function was operative for the specific community.
% FOUNDING_PROBLEM: Communities facing recurring, generationally-spaced catastrophe (forced displacement, famine, siege) needed a way to keep rapid-response competence alive between events separated by decades or centuries, when direct experiential transmission from survivor to descendant is not reliably available.
% FOUNDING_PROBLEM_CORROBORATION: Comparative ritual scholars and folklorists studying disaster-response traditions across cultures attest that ritually-encoded procedural sequences correlate with measurably faster household mobilization in communities that maintain literal performance of departure- and scarcity-themed rites, an assessment made from outside the practicing communities. Communal elders administering the ritual also attest the problem is live, but their attestation alone would not distinguish this reading from the symbol-continuity reading, since elders benefit under either interpretation.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because under this reading the ritual is fundamentally a coordination mechanism with a genuine payoff (rehearsed competence) rather than a rent-extraction structure — there is no party who collects at the direct expense of another in the core mechanism. The rising trajectory reflects the reading's own claim: as operational content is de-emphasized relative to symbolic form, the coordination function weakens and the practice drifts toward theater, which is exactly the mechanism the theater_ratio series (0.20 -> 0.42) is built to track. Suppression is low (0.22) because non-participation is not coercively punished in most contexts this reading covers; accessibility_collapse (0.40) and resistance (0.35) sit in the moderate rope range rather than mountain range, reflecting that alternative transmission mechanisms (written instruction, formal drills) are imaginable and sometimes used alongside ritual, not foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Future community members and diaspora communities are declared beneficiaries because the reading's core claim is that they receive functioning survival competence through the transmission channel — low d, near the beneficiary end. Household heads and ritual specialists occupy dual agenda-setter/beneficiary positions: they administer the transmission and also benefit from the competence they help maintain. The literalist practitioners are the reading's one victim class — not because anyone extracts from them, but because they pay the opportunity cost of performing the form without absorbing the payload, a real but diffuse cost distinct from active extraction, which is why extractiveness is authored moderate rather than high despite a named victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting catastrophe-response competence across generational gaps with no reliable direct experiential channel) is authored as contested rather than flatly dead: for diaspora and displacement-prone communities the problem remains structurally live, while for stabilized, low-threat communities the same ritual form may now be transmitting only symbolic residue. This reading treats that bifurcation honestly rather than declaring uniform mandatrophy resolution or uniform continued function — the rising theater_ratio is the instrument for detecting where, over time, the operational mandate has quietly lapsed while the form persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_symbolic_separability,
    'Is the operational (survival-competence) content of the ritual genuinely separable from its symbolic content, such that a practitioner could receive one without the other, or are they inseparable as the hybrid_embedded_reading claims?',
    'Comparative studies of practitioner cohorts who perform the ritual with varying degrees of literalist versus operationally-engaged intent, measuring actual downstream readiness behaviors (e.g., time-to-mobilize in crisis drills) against self-reported ritual understanding.',
    'If separable, this reading''s framing holds and the literalist-practitioner victim class is real and measurable. If inseparable, the hybrid_embedded_reading''s premise displaces this reading''s core claim, and the ''victim'' framing collapses since there would be no distinct competence payload to fail to extract.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_symbolic_separability, conceptual, 'Whether operational competence can be extracted independently of symbolic performance, as this reading requires.').

omega_variable(
    competence_extraction_universality,
    'Is the pattern-recognition/resource-coordination/threat-rehearsal function of ritual a cross-cultural universal (approaching mountain status) or a contingent, culturally-specific coordination mechanism (rope) that could fail to arise or could be replaced?',
    'Cross-cultural survey of catastrophe-adjacent ritual traditions to establish whether operational-encoding structure appears independent of specific religious content, versus being a specific historical adaptation within particular traditions.',
    'If near-universal across independently-evolved traditions, the constraint would lean toward mountain (an emergent regularity of how human groups solve the intergenerational-competence problem); if it is a contingent, tradition-specific choice with viable alternatives (written manuals, secular drills), rope is the more defensible claim, as currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_extraction_universality, empirical, 'Whether operational-competence encoding via ritual is a universal pattern or a contingent coordination choice.').

omega_variable(
    measurement_of_operational_yield,
    'How would one actually measure whether a given performance of the ritual is transmitting functioning competence versus inert symbolic form, given that the competence is largely tacit/procedural rather than propositional?',
    'Behavioral proxy measures (mobilization speed in simulated displacement scenarios, resource-triage decision quality under simulated scarcity) compared between high-ritual-fidelity and low-ritual-fidelity households, controlling for other sources of preparedness training.',
    'Without a working measurement proxy, the theater_ratio trajectory authored in this story is a plausible hypothesis, not an established empirical trend; a validated proxy would let the rising theater_ratio claim be tested rather than assumed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_of_operational_yield, empirical, 'The practical difficulty of measuring tacit operational competence transmission, underlying the authored theater_ratio series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 24, 0.23).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__operational_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'ritual transmits catastrophe memory' per the ε-invariance principle. operational_competence_reading (this file) authors moderate, rising extraction driven by theater-ratio drift in the competence-transmission function. symbol_continuity_reading authors the ritual's function as intrinsic identity/mourning preservation, with a different beneficiary structure (communal identity rather than future survival capacity) and correspondingly different ε. hybrid_embedded_reading denies the separability this reading assumes, treating symbolic fidelity and operational transmission as one inseparable mechanism, which changes both the victim structure (no literalist-practitioner victim class, since there is no separable payload to miss) and the classification logic. All three are linked via affects_constraints as members of the catastrophe_memory_transmission kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
