% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Preparedness as memorial performance instantiates a commitment system
 *   where the visible routine (annual drills, documentation, compliance
 *   audits) serves primarily to maintain an institutional narrative of
 *   readiness while actual operational competence atrophies. This is the HUSK
 *   READING of the preparedness_commitment kernel—the reading that emphasizes
 *   form-compliance without adaptive capacity. The constraint manifests as a
 *   piton: once-functional disaster response infrastructure persists through
 *   theatrical maintenance (the drill, the documentation, the compliance
 *   certification) despite degradation of the underlying competence that the
 *   form is supposed to instantiate. Communities participate in drills
 *   annually; staff execute protocols memorized from scripts; agencies
 *   satisfy regulatory gates by producing preparedness evidence. Yet when
 *   genuine novelty arrives—a disaster with parameters outside the trained
 *   scenarios, a compound failure, a cascading trigger—the institution's
 *   response collapses because it has no adaptive machinery, only a script.
 *   The theater_ratio rises over the measurement interval (0.68 → 0.81) as
 *   the routine becomes increasingly about performing preparedness rather
 *   than maintaining it. The suppression_requirement rises (0.55 → 0.68) as
 *   the institution must work harder to maintain the fiction that the ritual
 *   is functional—alternative narratives (that preparedness is broken, that
 *   competence has eroded, that the routine is theater) must be actively
 *   suppressed through policy framing and institutional identity defense.
 *
 * KEY AGENTS:
 *   - Community Participants: Powerless/trapped (local) — Compelled to participate in annual drills; no exit without social penalty. Experience preparedness as empty ritual.
 *   - Emergency Response Officials: Moderate/constrained (regional) — Balance regulatory compliance, budget constraints, and actual response capability. Benefits from visible routine (justifies funding); bears cost of degraded competence when novel disaster arrives.
 *   - Regulatory Authority: Institutional/arbitrage (national) — Experiences preparedness framework as coordination/compliance mechanism. Benefits through audit satisfaction and resource justification.
 *   - Adaptive Response Capacity: Organized/constrained (global) — Suppressed by form-dominance. Constrained from developing novel responses because institution's identity is invested in existing form. Competence collapse under novel stress.
 *   - Institution (Self-Aware): Institutional/arbitrage (national) — Acknowledges its own drills are 'box-checking.' Theater maintenance persists because abandoning the visible routine would signal institutional failure.
 *   - Analytical Observer: Analytical/analytical (universal) — Risks naturalizing institutional ritual degradation as immutable property of human organization rather than tractable design problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.68).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'cd929bf9-1b17-49e9-a5b1-792aa981cee2').
narrative_ontology:cs_kernel_codification('cd929bf9-1b17-49e9-a5b1-792aa981cee2', formalized).
narrative_ontology:cs_authority_grounding('cd929bf9-1b17-49e9-a5b1-792aa981cee2', extraction).
narrative_ontology:cs_interpretation_layer_present('cd929bf9-1b17-49e9-a5b1-792aa981cee2').
narrative_ontology:cs_reading_relation('cd929bf9-1b17-49e9-a5b1-792aa981cee2', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd929bf9-1b17-49e9-a5b1-792aa981cee2', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('cd929bf9-1b17-49e9-a5b1-792aa981cee2', foundational, form_compliance_substitutes_for_competence).
narrative_ontology:cs_axiom_status(form_compliance_substitutes_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('cd929bf9-1b17-49e9-a5b1-792aa981cee2', form_compliance_substitutes_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('cd929bf9-1b17-49e9-a5b1-792aa981cee2', secondary, institutional_memory_preserved_through_ritual).
narrative_ontology:cs_axiom_status(institutional_memory_preserved_through_ritual, overridden).
narrative_ontology:cs_axiom_grounding('cd929bf9-1b17-49e9-a5b1-792aa981cee2', institutional_memory_preserved_through_ritual, conventional).
narrative_ontology:cs_reference_frame('cd929bf9-1b17-49e9-a5b1-792aa981cee2', institutional_legitimacy_through_visible_preparedness).
narrative_ontology:cs_drift_state('cd929bf9-1b17-49e9-a5b1-792aa981cee2', contemporary_post_disaster_assessment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd929bf9-1b17-49e9-a5b1-792aa981cee2', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, institutional_continuity_narrative).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, adaptive_response_capacity).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, genuine_competence_retention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY PARTICIPANT (PITON) — Trapped in the ritual. Compelled to attend drills annually; cannot exit without social penalty. Experiences preparedness as ceremonial obligation disconnected from actual safety. The routine feels mandatory but hollow. No adaptive agency; their role is to reproduce the form.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY RESPONSE OFFICIAL (TANGLED ROPE) — Constrained by budget limits, regulatory mandates, and staff turnover. Benefits from the visible routine (justifies funding, satisfies compliance audits). Bears cost of degraded actual capacity when novel disaster arrives. Extraction runs both ways: institutional theater buys resources, but those resources are consumed by form maintenance rather than competence building.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Experiences preparedness requirements as coordination mechanism. Drills satisfy compliance gates; documentation legitimates governance. Net beneficiary through arbitrage: can point to preparedness framework to justify budget, then allocate resources elsewhere during calm periods.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADAPTIVE RESPONSE CAPACITY (SNARE) — Suppressed by the memorial routine's resource monopoly and ideological lock ('we have drills, therefore we are prepared'). Constrained from developing novel responses because the institution's identity is invested in the existing form. Experiential knowledge erosion as older practitioners retire without transmission to new staff trained only in script-following. Novel stressors (compound disasters, cascading failures) trigger competence collapse because the routine has no adaptive machinery.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE INSTITUTION (PITON) — Sees its own preparedness system as degraded ritual. Leadership acknowledges drills are 'box-checking.' Theater ratio (0.81) reflects that the routine's primary function is demonstrating care and maintaining legitimacy narrative, not building competence. Persists because abandoning the visible routine would signal institutional failure, even if replacement systems might be more effective. Inertial maintenance of performance.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From first principles: human institutions naturally drift toward ritual performance when the coordination problem (maintaining readiness across generations) is decoupled from actual outcomes. Preparedness without contact with real disaster becomes theater by structural inevitability. This perspective risks naturalizing what is actually a contingent institutional failure — treating institutional amnesia and form-over-function as immutable properties of human organization rather than tractable design problems.
constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_commitment__husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_commitment__husk_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The memorial performance extracts from adaptive capacity development—resources (budget, time, staff attention) consumed by form maintenance are unavailable for genuine competence building. The extraction increases over time as the routine becomes more elaborate and harder to update without disrupting the narrative. The victim (adaptive response capacity) bears full cost while the beneficiary (institutional continuity narrative) captures the legitimacy value. Suppression (0.68): High. The institution suppresses recognition of the form-competence gap through multiple mechanisms: framing drills as functional despite contradictory evidence; treating novel disaster as 'unprecedented' rather than failure of the routine to adapt; promoting staff through protocol compliance rather than demonstrated adaptive capacity; interpreting competence-collapse post-disaster as 'not what we trained for' rather than 'the training doesn't build competence.' Theater ratio (0.81): Very high. The routine is substantially performative—satisfying regulators, maintaining legitimacy narrative, demonstrating institutional care—rather than functional—building and retaining operational competence. The rise over time (0.68 → 0.81) reflects increasing divorce between form and function as the institution doubles down on performance rather than addressing the form-competence gap.
 *
 * PERSPECTIVAL GAP:
 *   The preparedness kernel admits radically different readings. The competence_reading sees preparedness as live exercised knowledge maintained through generational transmission of embodied skill. The husk_reading (this constraint) sees it as memorial performance that feels like retention but has lost operational substance. The hybrid_reading sees both layers operating simultaneously—memorial elements that stabilize institutional commitment alongside competence elements that maintain function. From the community participant's perspective, the routine is a piton (degraded, theatrical, trapped in participation). From the regulatory authority's perspective, it is rope (coordination mechanism). From the institution's own perspective, it is a piton (self-aware degradation). From the analytical observer's perspective, it risks appearing as mountain (natural institutional drift) but the structural data reveals it as a false summit—naturalizing a contingent institutional failure. The core perspectival gap is between agents who see the routine as functional (or accept it as theater that legitimates resource allocation) and agents who see it as suppressing genuine competence development.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the extraction flow. Community participants are trapped with no exit—full targets (high d). Emergency response officials are constrained but also benefit from the legitimacy routine—mixed (moderate d). Regulatory authorities arbitrage the compliance value—beneficiaries (low d). Adaptive response capacity is the victim of suppressed development—full target (high d). The institution itself experiences the constraint as moderately beneficial (legitimacy, resource justification) but also costly (knowledge erosion, response inadequacy)—moderate d. The analytical observer sees the structure from outside but risks being captured by the 'natural institutional drift' framing—high d despite analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via reading structure. The husk_reading resolves the mandatrophy by declaring: preparedness commitment is implemented as memorial performance that extracts from adaptive capacity while maintaining institutional narrative. This is not ambiguous between coordination and extraction—it is extractive on the competence dimension (suppresses development) while appearing coordinated on the legitimacy dimension (satisfies regulators). The mandatrophy dissolves when the reading specifies which dimension dominates: in the husk_reading, form-compliance dominates; competence is secondary and degrading. The hybrid_reading would resolve differently (both dimensions present and balanced). The competence_reading would resolve differently (competence dominant). The mandatrophy is not 'is this coordination or extraction?' but 'which reading of the kernel is correct?'—and that is an empirical question about institution-specific trajectories (theater ratio rising, competence collapse under novel stress, staff turnover without knowledge transfer).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_transmission_mechanism,
    'What structural features distinguish genuine competence retention (embodied skill, tacit knowledge, adaptive response capacity) from performative compliance (form adherence, audit satisfaction, narrative continuity)?',
    'Post-disaster assessment: comparison of communities with identical drill compliance but different learning-loop engagement; measurement of staff response speed and error rates in novel scenarios; tracking of knowledge transfer from retiring to new practitioners',
    'If competence and compliance are structurally independent: this reading is correct — drills can satisfy regulators while competence atrophies. If competence requires embodied practice: both readings coexist but at different timescales. If feedback from novel scenarios is integrated into routine revision: hybrid reading applies and piton classification is temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_transmission_mechanism, empirical, 'Whether compliance and competence are structurally decoupled').

omega_variable(
    institution_identity_lock,
    'Is the institution''s commitment to the preparedness form grounded in genuine epistemic belief about its effectiveness, or in identity fusion (institutional legitimacy narrative depends on visible preparedness routine)?',
    'Analysis of institutional response to competence failures: do leaders attempt routine revision when drills fail to catch real-world errors, or do they double down on form compliance? Tracking of funding allocation: does budget follow adaptive capacity development or performance maintenance? Organizational culture interviews: explicit vs implicit messaging about the routine''s purpose.',
    'If grounded in belief: institution might shift reading toward competence emphasis if presented with evidence. If identity-locked: the husk reading is entrenched; institutional legitimacy is constituted through the performance; evidence of competence failure triggers defensive reinforcement of form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institution_identity_lock, conceptual, 'Whether institutional commitment to form is belief-based or identity-locked').

omega_variable(
    novelty_stress_trigger,
    'At what degree of deviation from trained scenarios does competence collapse become observable? What is the threshold between ''scenario variation the routine can absorb'' and ''novel stress that triggers D5 break''?',
    'Retrospective analysis of disasters: correlation between drill congruence with actual event parameters and response effectiveness. Forward-looking: small-scale novel-stress exercises (unannounced component variation, compound triggers) to identify where the routine''s adaptive machinery fails.',
    'If threshold is low (<5% parameter variation): husk reading dominates; routine has minimal adaptive capacity. If threshold is high (>30% variation): routine has genuine flexibility; hybrid reading applies. Threshold location determines whether preparedness commitment can survive genuine novelty or requires constant form-update cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novelty_stress_trigger, empirical, 'Threshold of novelty at which competence collapse triggers').

omega_variable(
    reading_foreclosure_structure,
    'Does this reading (preparedness as memorial performance lacking competence) logically foreclose the competence_reading, or do the readings occupy different empirical positions that could coexist within different institutional designs?',
    'Structural analysis of the kernel: if preparedness commitment NECESSARILY requires either memorial form OR live competence but not both (i.e., the institution cannot simultaneously maintain both the ritual and the adaptive machinery), then foreclosure holds. If both can be present but trade off against each other (budget, attention, staff time), then coexistence applies.',
    'If foreclosure: only one reading can be right; the other is ideological cover. If coexistence: both readings describe real institutional positions; the question is which dominates. If influences: this reading creates downstream pressure on competence reading (form-dominance starves competence development) but doesn''t rule it out.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between husk_reading and competence_reading').

omega_variable(
    memorial_performance_intent,
    'Is the memorial function (honoring lost persons, maintaining cultural memory of past disasters) an axiom of this reading''s normative commitment, or a byproduct rationalization?',
    'Institutional documentation: explicit statements of preparedness purpose in founding charters, policy documents, and public messaging. Practitioner interviews: are drills framed as competence maintenance or as cultural ritual? Historical analysis: did the routine emerge from genuine competence need or from memorial/commemorative impulse?',
    'If memorial is a core axiom: the reading''s legitimacy grounding is partially deontological (duties to the dead, community memory preservation). If byproduct rationalization: the axiom is purely institutional inertia and form-compliance. This affects whether the reading is holdable (live commitment) or overridden (abandoned in practice despite formal dedication).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_performance_intent, conceptual, 'Whether memorial function is axiom or rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_husk_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(prep_husk_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.75).
narrative_ontology:measurement(prep_husk_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_husk_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prep_husk_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(prep_husk_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prep_husk_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prep_husk_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(prep_husk_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, disaster_response_capability_erosion).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, institutional_amnesia_cascade).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three constraint stories, each instantiating a different reading with different extractiveness values and classification profiles. The husk_reading (this constraint, ε=0.58, piton-dominant) represents form-compliance without competence. The competence_reading (ε=0.22, rope) represents genuine knowledge maintenance. The hybrid_reading (ε=0.35, tangled_rope) represents simultaneous operation of both memorial and competence functions. Each story is a complete constraint with its own perspectives, beneficiaries/victims, and measurements. They are linked via network edges because the institutional dynamics that favor the husk_reading create downstream pressure on competence development, which affects other constraints in the disaster preparedness cluster.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
