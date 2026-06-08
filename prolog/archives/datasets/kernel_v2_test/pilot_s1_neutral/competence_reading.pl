% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_preparedness/institutional_memory/operational_readiness
 *
 * SUMMARY:
 *   This constraint story instantiates the competence_reading of the
 *   preparedness_persistence kernel — the reading that privileges live
 *   exercised knowledge as an irreplaceable mechanism for maintaining
 *   operational readiness across institutional memory gaps. Drills are not
 *   ritual theater in this reading; they are the primary mechanism by which
 *   organizations preserve embodied, tacit knowledge that simulation cannot
 *   fully replicate. The constraint operates across disaster preparedness
 *   authorities (fire departments, emergency management agencies, nuclear
 *   facilities, military command structures) as a coordination mechanism with
 *   minimal extraction: the constraint solves the legitimate problem of
 *   maintaining competence under conditions (time pressure, incomplete
 *   information, actual stakes) that cannot be safely ethically replicated in
 *   training. The competence_reading stands in contrast to the husk_reading
 *   (which emphasizes drills as degraded ritual maintained for performative
 *   reasons) and the hybrid_reading (which holds that drills serve both
 *   functions simultaneously — real competence and institutional theater).
 *   The kernel they contest is the meaning of preparedness persistence: is it
 *   grounded in actual operational readiness, or in the institutional
 *   necessity of maintaining the appearance of readiness whether or not the
 *   underlying competence persists?
 *
 * KEY AGENTS:
 *   - Emergency Response Personnel: Moderate power, constrained exit (mandatory training) — practice maintains muscle memory and team synchronization; benefit flows directly to responder competence and survival rate
 *   - Protected Populations (Drill Participants): Powerless in institutional hierarchy, constrained exit (compulsory participation) — direct benefit from knowing evacuation routes and alert systems; minimal extraction experienced
 *   - Institutional Emergency Management Authority: Institutional power, mobile exit (could theoretically cease running drills) — benefits from maintaining institutional knowledge across personnel turnover and leadership changes; primary benefit is institutional survival capacity
 *   - Analytical Observer: Civilizational timescale — sees live practice as an irreducible requirement for preserving operational competence under stress conditions that simulation cannot ethically replicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.08).
domain_priors:suppression_score(competence_reading, 0.12).
domain_priors:theater_ratio(competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, rope).
narrative_ontology:human_readable(competence_reading, "Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/operational_readiness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, '2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3').
narrative_ontology:cs_kernel_codification('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', distributed).
narrative_ontology:cs_authority_grounding('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', practice).
narrative_ontology:cs_interpretation_layer_present('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3').
narrative_ontology:cs_reading_relation('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', competence_reading__preparedness_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', competence_reading__preparedness_hybrid_reading, influences).
narrative_ontology:cs_axiom('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', foundational, embodied_knowledge_irreplaceable_for_crisis_response).
narrative_ontology:cs_axiom_status(embodied_knowledge_irreplaceable_for_crisis_response, holdable).
narrative_ontology:cs_axiom_grounding('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', embodied_knowledge_irreplaceable_for_crisis_response, empirically_contingent).
narrative_ontology:cs_axiom('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', foundational, simulation_cannot_fully_replicate_actual_stress_conditions).
narrative_ontology:cs_axiom_status(simulation_cannot_fully_replicate_actual_stress_conditions, holdable).
narrative_ontology:cs_axiom_grounding('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', simulation_cannot_fully_replicate_actual_stress_conditions, empirically_contingent).
narrative_ontology:cs_reference_frame('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', operationally_grounded_preparedness).
narrative_ontology:cs_drift_state('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c4fee57-1cb9-43d9-9fe0-47fdcddfe7b3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(competence_reading, protected_populations).
narrative_ontology:constraint_beneficiary(competence_reading, institutional_survival_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_reading, institutional_emergency_management_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Firefighters, paramedics, emergency management staff, and other first responders participate in regular drills as a required component of their role. Drills maintain their operational competence under stress, ensure they know their position in the command structure, and embed the procedural muscle memory that becomes automatic during actual crisis. Participation is mandatory (constrained exit from training requirements) but the benefit flows directly to them: their survival rates, response effectiveness, and team cohesion all improve through practice.
narrative_ontology:constraint_stakeholder(competence_reading, emergency_response_personnel, beneficiary,
    moderate, biographical, constrained, local).

% Residents and workers in jurisdictions with organized preparedness drills (evacuation exercises, shelter-in-place drills, alert system tests) gain direct knowledge of what to do during actual emergencies. They learn evacuation routes, understand alert signals, practice the procedures that keep them safe. Participation may be compulsory in some contexts (schools, workplace drills) but the benefit is clear: those who know the evacuation route are faster to exit; those who have practiced shelter-in-place know which room to use and what supplies to bring. Competence in response saves lives.
narrative_ontology:constraint_stakeholder(competence_reading, protected_populations, beneficiary,
    powerless, biographical, constrained, local).

% The emergency management authority (fire chief, emergency director, military command) sets the drill schedule, designs exercises, and enforces participation requirements. They benefit from maintaining institutional knowledge across decades of organizational life — when personnel retire or transfer, the institution preserves its competence through documented procedures and ongoing practice. They could theoretically cease running drills (mobile exit), but doing so would collapse the readiness they depend on to fulfill their mandate. Their institutional survival depends on real operational capacity, and drills are the primary mechanism for maintaining it.
narrative_ontology:constraint_stakeholder(competence_reading, institutional_emergency_management_authority, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(competence_reading, institutional_emergency_management_authority, beneficiary).

% Operational competence — the actual ability of personnel and institutions to respond effectively to emergencies — is the beneficiary of the drill constraint. Drills maintain competence by providing the repeated practice, procedural reinforcement, and team coordination experience that cannot be acquired through reading manuals or watching videos. Competence itself is not an agent and collects no rents, but it is the non-agent beneficiary of this constraint's operation.
narrative_ontology:constraint_stakeholder(competence_reading, competence_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(competence_reading, competence_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining operational competence across decades of organizational life where actual emergencies are separated by years or decades, through embodied practice of response procedures under conditions as close as ethically possible to actual crisis.
% TRANSFER_FUNCTION: The constraint transfers embodied knowledge and procedural memory from experienced personnel to new cohorts, and from organizational history to present readiness capacity. Personnel invest time and effort in drills; the organization invests budget and administrative capacity; the return flows back as institutional survival capacity and individual competence under stress.
% ABSENT_VOICES: Populations in jurisdictions where preparedness investment has been cut or neglected (where drills have become rare or eliminated). These populations would voice the cost of reduced preparedness if they were represented in the planning process, but they are absent from preparedness authority decision-making. Their absence reflects geography and resource inequality, not deliberate exclusion — but it is significant because it means the competence_reading is tested primarily in well-resourced jurisdictions.
% DISAPPEARANCE_RATIONALE: If organized drills and inspections disappeared overnight, emergency response capacity would degrade systematically within 2-3 years as procedural memory decayed, personnel turnover erased institutional knowledge, and team coordination atrophied. New emergencies would be met with unpracticed responses and higher failure rates. The world would not return to pre-constraint conditions — the constraint did not create the emergency; it created the readiness to respond to it. Without the constraint, readiness capability would measurably decline.
% FOUNDING_PROBLEM: How can an organization maintain operational competence for low-frequency, high-stakes events (emergencies may occur once per decade or less frequently) across decades of organizational life, personnel turnover, and inevitable forgetting of procedures and team coordination patterns?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by emergency response personnel who report that drills are essential for maintaining muscle memory; by disaster analyses showing that response failures often trace to broken procedures or unclear chains of command that drills would have caught; and by organizations that have reduced drill frequency and subsequently experienced response degradation. The competence_reading is attested by practitioners (emergency managers, fire chiefs, military commanders) who see drills as the irreplaceable core of their preparedness mandate.
narrative_ontology:disappearance_verdict(competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGENCY RESPONSE PERSONNEL (ROPE) — Drills maintain operational readiness through repeated practice; personnel benefit from knowing their role under stress and from the coordination structure drills embed. Constrained exit (cannot skip mandatory training) but genuine coordination function — drills solve the legitimate problem of maintaining muscle memory and team synchronization under conditions that cannot be replicated in simulation alone. Low experienced extraction because the primary benefit flows to the responder: their competence, survival rate, and team effectiveness all increase.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: PROTECTED POPULATIONS / DRILL PARTICIPANTS (ROPE) — For those who participate in evacuation drills or shelter-in-place exercises, the constraint is coordination with minimal extraction. Participation is compulsory (constrained exit) but the benefit is direct: knowing the evacuation route, understanding the alert system, practicing muscle memory for life-safety procedures. The constraint solves the problem of maintaining collective readiness without requiring each person to rediscover their role during actual crisis. Experienced extraction is low because the benefit accrues to the participant.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: INSTITUTIONAL EMERGENCY MANAGEMENT AUTHORITY (ROPE) — The authority that organizes drills and inspections sees them as a pure coordination mechanism: maintaining the institutional knowledge of response procedures across personnel turnover, capturing lessons from near-misses, and rehearsing the command structure under the closest safe approximation to actual crisis conditions. The drills solve a fundamental coordination problem — how to maintain competence across decades when actual events may be separated by years. Exit options are mobile (the authority could theoretically stop running drills) but doing so would collapse the readiness it depends on. Experienced extraction: low, because the authority's primary benefit is institutional survival capacity, not rent-seeking from the constraint's operation.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / COMPETENCE PRESERVATION (MOUNTAIN) — From a civilizational timescale, live practice is an irreducible requirement for preserving operational competence under stress. Simulation cannot fully replace embodied practice because the actual conditions of crisis — time pressure, incomplete information, fatigue, high stakes — cannot be ethically replicated in training. This perspective sees drills as a natural law: any organization that wishes to maintain readiness across decades MUST run live drills. The constraint emerges from the structure of human learning and organizational memory, not from institutional choice. Extractiveness and suppression approach zero because the constraint is immutable given the goal of preparedness.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint exhibits minimal extraction because the primary benefits flow to those who participate: emergency personnel gain competence, protected populations gain readiness knowledge, and the institution maintains its survival capacity. The slight non-zero value (0.08 rather than 0.0) reflects minimal coordination overhead and the fact that some personnel and budget resources are devoted to organizing drills rather than other priorities — a legitimate cost of coordination, not extraction. Suppression (0.12): Low-moderate. Participation in drills is compulsory (suppressing the exit option), but suppression is not high because the compulsion is transparent, widely justified, and directly benefits the suppressed agent. Personnel cannot opt out of mandatory training, but this is openly acknowledged as a legitimate operational requirement. Theater ratio (0.15): Very low. Live drills have high functional content and low performative content in the competence_reading — the primary purpose IS to maintain real competence. Some theatrical elements exist (formal debriefs, documented procedures) but these serve the knowledge-preservation function rather than replacing it. The slight increase in theater ratio over the interval (0.10 to 0.18) reflects modest drift toward more documentation and formalization as institutions mature, but the functional core remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   All three primary perspectives (emergency personnel, protected populations, institutional authority) classify as Rope with low experienced extraction. The gap is not between different types but between the civilian analyst's perspective (potentially Mountain — seeing drills as natural law) and the practitioner's perspective (Rope — experiencing coordination with benefit). The competence_reading produces a presheaf where rope perspectives dominate and the mountain perspective represents the civilizational timescale view that emerges when one removes short-term institutional contingency from the picture. The missing perspective in this story is the husk_reading's view (which would see Piton — performative ritual maintained by inertia) — that perspective belongs to a separate constraint story instantiating the husk_reading of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is very low across all perspectives in the competence_reading because all stakeholders are beneficiaries relative to this constraint. Emergency personnel benefit from maintained competence. Protected populations benefit from readiness. The institution benefits from survival capacity. No one is structurally positioned as a target. This contrasts sharply with the husk_reading, where the constraint's beneficiaries would be institutional administrators and status-maintenance actors, and the victims would be actual preparedness (degraded by theater consuming resources) and honest practitioners (forced to participate in ritual). The competence_reading and husk_reading would have opposite directionality structures — opposite beneficiary/victim declarations — because they read the same institutional practice as serving opposite functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence_reading avoids mandatrophy by grounding the constraint's persistence in a genuine, ongoing coordination problem: maintaining operational competence across organizational memory gaps and personnel turnover. The mandate — 'preserve readiness for low-frequency, high-stakes emergencies' — is live and irreplaceable. The constraint serves this mandate directly: drills ARE the mechanism of mandate fulfillment. There is no wedge between the constraint and its reason for existence. The husk_reading, by contrast, exhibits mandatrophy: the mandate is still formally stated ('preserve readiness') but the constraint's actual function has degraded to pure theater; the mandate persists but the constraint no longer serves it. The hybrid_reading sits between — acknowledging that drills serve the mandate but also recognizing significant drift toward theater, suggesting partial mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_boundary,
    'At what fidelity threshold can simulation replace live drills for maintaining operational competence?',
    'Comparative study of response performance: teams trained primarily on simulation vs teams trained with regular live drills, under actual crisis conditions or high-fidelity scenario exercises that introduce realistic time pressure and incomplete information',
    'If simulation can achieve >85% fidelity: competence_reading reclassifies toward scaffold (drills become temporary, sunset as simulation advances). If simulation tops at <70%: mountain classification is confirmed (live drills are irreplaceable). If 70-85%: hybrid reading gains credibility (both modes required, perpetually).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Simulation fidelity threshold for replacing live drills').

omega_variable(
    natural_law_vs_institutional_choice,
    'Is the requirement for live drills a natural law of organizational competence, or a contingent product of current cognitive science and institutional capacity?',
    'Historical analysis of preparedness failures correlated with drill frequency and fidelity; longitudinal study of organizations that reduce drill frequency while maintaining equivalent response performance (testing whether competence decay is inevitable or institution-specific); neuroscience research on embodied memory and stress performance under conditions closer to actual crisis',
    'If natural law confirmed: mountain classification holds; no institutional choice escapes the requirement. If contingent on institutional design: competence_reading reclassifies as rope or tangled_rope (institutional choice about drill format, frequency, and enforcement level becomes salient; the husk_reading and hybrid_reading gain structural validity). This omega is the core reading contest — it gates whether preparedness persistence is a physical fact or a contested institutional claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_choice, empirical, 'Whether competence preservation through drills is natural law or institutional choice').

omega_variable(
    kernel_reading_incommensurability,
    'Can the competence_reading and husk_reading coexist in a single institutional framework, or do they force choice?',
    'Examination of actual disaster preparedness authorities that embrace both readings: the competence_reading emphasizes drills as live exercised knowledge; the husk_reading emphasizes drills as ritual theater. If organizations successfully hold both as complementary (drills are real competence practice AND performative signal), the readings coexist. If organizations split into separate camps (some maximize for real competence, some perform ritual), the readings are incompatible at the institutional level.',
    'If coexistent: both readings are legitimate; the constraint family is a presheaf over institutional choice (different authorities read the kernel differently). If incompatible: one reading will tend to dominate within any single authority as pressures select for either real competence or theater sustainability. This gates whether the constraint story is one perspectival reading of a shared kernel or a structural incompatibility between two framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether competence_reading and husk_reading can coexist in one institutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t10, competence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(comp_tr_t20, competence_reading, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(comp_be_t10, competence_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(comp_be_t20, competence_reading, base_extractiveness, 20, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(comp_su_t10, competence_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(comp_su_t20, competence_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_reading, preparedness_husk_reading).
narrative_ontology:affects_constraint(competence_reading, preparedness_hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three structurally distinct constraint stories: competence_reading (this file — Rope, low extraction, grounded in real operational function), husk_reading (Piton, moderate extraction, grounded in institutional theater), and hybrid_reading (Tangled Rope, mixed function and theater). Each story instantiates one reading of the kernel and has its own ε value, beneficiary/victim structure, and axioms. The three stories are linked by network.affects_constraints and by the cs_structure.reading_relations fields, which declare how each reading relates to the others. The competence_reading influences both siblings (establishing the standard for what 'real preparedness' means), but does not foreclose them — institutions can and do choose to emphasize theater (husk_reading) or balance both functions (hybrid_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
