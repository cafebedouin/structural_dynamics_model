% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Ritual as Transmitted Operational Survival Competence (Passover/Tisha B'Av Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story isolates the operational-competence reading of ritual
 *   catastrophe memory: the claim that practices like the Passover seder and
 *   Tisha B'Av fast function as low-cost, high-fidelity training mechanisms
 *   for rapid displacement and resource-scarcity survival skills, transmitted
 *   across generations independent of the symbolic meaning attached to them.
 *   The relevant ε here is low-to-moderate: rehearsal cost is real but
 *   modest, borne voluntarily, and the coordination function (pre-training a
 *   population for crisis response) is genuine. This is a distinct constraint
 *   from the symbol_continuity_reading (which evaluates the same rituals
 *   purely as identity/mourning preservation, with a different beneficiary
 *   structure and no operational-yield claim) and from the
 *   hybrid_embedded_reading (which denies the two functions can be separated
 *   at all). Per the ε-invariance principle, these are three separate
 *   constraints sharing a kernel, not one constraint viewed three ways.
 *
 * KEY AGENTS:
 *   - future_generations_survival_capacity: diffuse analytical beneficiary — the accruing procedural competence itself
 *   - diaspora_communities_facing_displacement_risk: primary beneficiary population (moderate/constrained) — rehearses and later draws on the competence
 *   - household_units_practicing_rehearsal: primary payer of rehearsal cost, secondary beneficiary of the trained skill
 *   - ritual_transmission_authorities: agenda-setting institutional seat that shapes which operational content is foregrounded
 *   - literalist_practitioners_mistaking_symbol_for_substance: the specific victim class of THIS reading — those who get the form without the trained competence
 *   - ritual_studies_scholars: analytical observer evaluating the operational-yield claim itself
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
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual as Transmitted Operational Survival Competence (Passover/Tisha B'Av Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '72c016c8-4c43-49f2-81ba-baf16ff09021').
narrative_ontology:cs_kernel_codification('72c016c8-4c43-49f2-81ba-baf16ff09021', distributed).
narrative_ontology:cs_authority_grounding('72c016c8-4c43-49f2-81ba-baf16ff09021', practice).
narrative_ontology:cs_interpretation_layer_present('72c016c8-4c43-49f2-81ba-baf16ff09021').
narrative_ontology:cs_reading_relation('72c016c8-4c43-49f2-81ba-baf16ff09021', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('72c016c8-4c43-49f2-81ba-baf16ff09021', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('72c016c8-4c43-49f2-81ba-baf16ff09021', foundational, operational_yield_is_measurable_and_separable).
narrative_ontology:cs_axiom_status(operational_yield_is_measurable_and_separable, holdable).
narrative_ontology:cs_axiom_grounding('72c016c8-4c43-49f2-81ba-baf16ff09021', operational_yield_is_measurable_and_separable, empirically_contingent).
narrative_ontology:cs_axiom('72c016c8-4c43-49f2-81ba-baf16ff09021', secondary, symbolic_fidelity_without_procedural_transfer_constitutes_failure).
narrative_ontology:cs_axiom_status(symbolic_fidelity_without_procedural_transfer_constitutes_failure, holdable).
narrative_ontology:cs_axiom_grounding('72c016c8-4c43-49f2-81ba-baf16ff09021', symbolic_fidelity_without_procedural_transfer_constitutes_failure, instrumental).
narrative_ontology:cs_reference_frame('72c016c8-4c43-49f2-81ba-baf16ff09021', rehearsal_as_procedural_training_transmission).
narrative_ontology:cs_drift_state('72c016c8-4c43-49f2-81ba-baf16ff09021', contemporary_diaspora_low_displacement_risk, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72c016c8-4c43-49f2-81ba-baf16ff09021', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations_survival_capacity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities_facing_displacement_risk).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, household_units_practicing_rehearsal).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, household_units_practicing_rehearsal).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, ritual_as_encoded_procedural_knowledge).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, threat_rehearsal_transmission_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Not an actor but the accruing capacity itself: households that rehearse rapid departure (Passover) or resource-scarcity discipline (Tisha B'Av) carry forward procedural competence — what to grab, how fast, how to ration — that pays off during actual displacement or siege events separated from the ritual by generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations_survival_capacity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, future_generations_survival_capacity).

% Communities historically and presently exposed to expulsion, pogrom, or forced relocation. They rehearse the operational content of the ritual yearly — packing order, unleavened bread as travel food requiring no rising time, fasting as scarcity conditioning — and this rehearsal measurably improves real-crisis response time and resource discipline when displacement recurs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities_facing_displacement_risk, beneficiary,
    moderate, generational, constrained, global).

% The families actually performing the seder or the fast bear the annual cost in time, food restriction, and labor of preparation. In return they and their children internalize a rehearsed procedure for departure-under-threat and scarcity-endurance that would otherwise have to be learned live, at much higher cost, during an actual crisis.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, household_units_practicing_rehearsal, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, household_units_practicing_rehearsal, payer).

% Rabbinic and communal authorities who set the liturgical calendar and the specific content of the rehearsed practices (what is eaten, what is forbidden, what is narrated). They can and do revise emphasis over centuries — shifting which operational elements are foregrounded — while maintaining the outer form, and they experience negligible direct cost from participants' practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_transmission_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Practitioners who treat the ritual's symbolic surface (retelling a story, following prescribed words) as the entire point and never absorb the underlying operational content — packing logic, timing discipline, scarcity tolerance. When actual crisis arrives they have the form without the trained competence, paying the cost of a fidelity that produced no transferable skill. Their loss is the operational reading's specific failure mode, distinct from the symbol-continuity reading's own success criterion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance, payer,
    moderate, biographical, constrained, local).

% Researchers who study whether ritual practice measurably transmits operational competence (pattern recognition, coordination skill, threat rehearsal) versus purely symbolic/identity functions. They evaluate the practices against operational yield criteria and can, in principle, falsify the competence-transmission claim for any specific ritual element.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Annual rehearsal of specific procedures — rapid packing and departure timing at Passover, resource rationing and deprivation tolerance at Tisha B'Av — trains a population, at low per-instance cost, in skills that are expensive and dangerous to learn for the first time during an actual catastrophe. This solves a genuine intergenerational transmission problem: how do you teach crisis competence without a crisis.
% TRANSFER_FUNCTION: Moves labor, food-preparation time, and a year of dietary/behavioral constraint from practicing households into stored procedural competence (in individuals and in communal memory) that is drawn down, without further payment, whenever an actual displacement or scarcity event occurs — often by descendants far removed from the households who paid the rehearsal cost.
% ABSENT_VOICES: Practitioners who experience the ritual as purely devotional or communal-identity work, with no operational content whatsoever, are not represented in this reading's stakeholder set — they belong to the symbol_continuity_reading, a sibling constraint with a different beneficiary/victim structure. Their absence here is a function of decomposition, not exclusion from a shared conversation.
% DISAPPEARANCE_RATIONALE: If the ritual practices vanished, some argue the operational competence they train (rapid-departure logistics, scarcity discipline) would have to be relearned expensively during the next actual crisis, since no substitute low-cost rehearsal mechanism currently exists at comparable scale. Others argue the operational content is incidental and the practices could vanish with no measurable loss to real-world crisis outcomes, since modern displacement events are rarely responded to using seder-derived packing heuristics. The operational-yield claim itself is what is contested, which is exactly the ambiguity this reading isolates for testing.
% FOUNDING_PROBLEM: Communities repeatedly facing sudden displacement, siege, or resource collapse needed a way to pre-train population-wide competence in rapid mobilization and scarcity endurance without waiting for the next actual disaster to teach the lesson at full cost.
% FOUNDING_PROBLEM_CORROBORATION: Some historians of Jewish communal practice and disaster-response researchers attest that ritual rehearsal correlates with faster, more organized community response during documented expulsion and refugee events (external corroboration outside the practicing communities). Other ritual studies scholars, also external to the practicing communities, attest that the correlation is weak or confounded by network density and communal cohesion rather than the specific rehearsed content — no consensus corroboration exists outside the interpretive dispute itself.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is modest (0.28 at interval end) and rises slowly: the cost of annual rehearsal (a fast, a restricted diet, a structured meal) is real but bounded, and it is paid in exchange for a plausible, non-trivial training benefit rather than pure rent extraction. Theater ratio rises more noticeably over the interval (0.20 to 0.42) because as literal, life-threatening displacement events recede in frequency for many diaspora communities, an increasing share of ritual performance becomes commemorative repetition disconnected from any live threat rehearsal function — this is the mechanism by which the operational reading itself risks decaying into the symbol-continuity reading over time, without formally becoming that reading. Accessibility collapse is moderate (0.35): alternative ways to transmit crisis competence exist (secular disaster preparedness training, oral history without ritual form) and are not suppressed, so this is not a mountain. Resistance is low-moderate (0.3): most practitioners do not experience the annual practice as coercive, though skeptics and literalist critics do push back on the operational-yield claim's plausibility.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora-community seat, the practice reads as rope: a modest, voluntary, genuinely functional coordination mechanism. From the literalist-practitioner seat, the same structure yields no operational benefit while extracting the identical rehearsal cost — for that seat alone, the constraint approaches a scaffold that never transitions (form persisting after the substance that justified it has failed to transfer). The engine should register this divergence structurally rather than the story averaging it into one composite verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Future survival capacity and diaspora communities at risk sit near the beneficiary end: they draw down trained competence without further payment when crisis actually recurs. Practicing households sit closer to symmetric — they pay the rehearsal cost annually and are also the ones most likely to benefit directly. Literalist practitioners are the reading's specific victim: they pay the same rehearsal cost as everyone else but, because they engage only the symbolic surface, receive none of the operational transfer — their d is pushed toward the target end not by exploitation but by a training failure internal to how they engage the practice. Ritual transmission authorities set the content but bear negligible direct cost, placing them near full beneficiary/agenda-setter despite not personally undergoing crisis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-training population-wide crisis competence without waiting for a live crisis) remains partially live wherever diaspora or displacement risk persists, but is contested where modern civil infrastructure has substantially reduced that risk for some communities. The rising theater_ratio trajectory is the specific signal to watch: if operational content continues to erode while form persists, this reading risks sliding toward mandatrophy — the coordination function it claims will have quietly died while the practice's justification narrative continues unchanged. Classifying this as rope rather than mountain or snare prevents two errors: treating a contingent, revisable coordination mechanism as unchangeable natural law, and treating a genuinely functional low-coercion training system as pure extraction merely because it has victims (the literalist-practitioner class) at the margin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_yield_measurability,
    'Can the operational-competence transmission this reading claims actually be measured and distinguished from confounds like communal cohesion, network density, or general risk-awareness culture?',
    'Comparative disaster-response studies tracking displacement-event outcomes across communities with varying ritual-rehearsal intensity, controlling for communal network structure and prior disaster exposure.',
    'If the operational yield cannot be isolated from confounds, this reading collapses toward the symbol_continuity_reading (the ritual''s only demonstrable function is identity/cohesion, not procedural training) — reclassification pressure toward that sibling constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_yield_measurability, empirical, 'Whether operational competence transmission is empirically separable from cohesion effects.').

omega_variable(
    reading_separability_from_hybrid,
    'Is it coherent to isolate ''operational yield'' as a distinct evaluative axis from symbolic form at all, or does the hybrid_embedded_reading correctly deny this separation?',
    'Ethnographic and cognitive-science work on non-propositional/embodied knowledge transmission in ritual contexts — determining whether procedural skill can in fact be extracted from symbolic practice without loss, or whether the two are constitutively fused as the hybrid reading claims.',
    'If the hybrid reading is correct that the functions are inseparable, this operational reading is not a distinct constraint but an analytical abstraction from a fused reality — this would not dissolve the story (the kernel-contest structure anticipates exactly this dispute) but would strengthen the case for the hybrid reading as the more structurally accurate one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_separability_from_hybrid, conceptual, 'Whether operational competence and symbolic form are analytically separable, as this reading assumes, or constitutively fused, as the hybrid reading claims.').

omega_variable(
    victim_class_boundary,
    'Is ''mistaking symbol for substance'' a real, identifiable failure mode with actual victims, or is it a construct of the operational reading itself that pathologizes practitioners who are actually well-served by the symbol_continuity function?',
    'Interview-based research asking practitioners directly what they believe the practice is for and whether they experience any felt lack when crisis does not follow the expected script.',
    'If literalist practitioners report full satisfaction and no felt deficit, the ''victim'' framing under this reading is itself an artifact of privileging operational yield as the correct evaluative frame — which would not falsify the reading but would narrow its victim claim considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_boundary, conceptual, 'Whether the literalist-practitioner victim class is a genuine structural harm or an artifact of this reading''s chosen evaluative frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(cata_tr_t80, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement_basis(cata_be_t80, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(cata_be_t100, observed).

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
% Part of a three-member constraint family decomposing the colloquial label 'ritual as catastrophe memory' per the ε-invariance principle: this story (operational_competence_reading) claims a low-moderate, genuinely functional coordination ε; the symbol_continuity_reading sibling claims a different ε profile centered on identity/mourning preservation with no operational-yield claim; the hybrid_embedded_reading sibling denies the separability both other readings assume. All three share the catastrophe_memory_transmission kernel but are authored as distinct constraints with distinct beneficiary/victim structures, per Rule 1 of the committer frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
