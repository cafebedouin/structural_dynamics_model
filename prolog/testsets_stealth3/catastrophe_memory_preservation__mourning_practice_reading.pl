% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Mourning Rite as Symbolic Continuity (Mourning-Practice Reading)
 *   domain: religious/collective-memory
 *
 * SUMMARY:
 *   A river-delta community that lost a substantial share of its population
 *   in a single flood maintains an annual mourning rite: a dawn procession to
 *   the breached levee, the reading of every victim's name, silence at the
 *   hour of the breach, a shared meal, and year-round tending of the memorial
 *   ground by a rotating roster of households. Attendance is requested, never
 *   compelled; members who abstain lose no standing, property, or membership.
 *   This file instantiates the mourning-practice account of what the rite
 *   does: it preserves symbolic continuity and collective identity — a shared
 *   calendar, a common address for grief, a handoff of witness across
 *   generations — without transferring operational skill. Metrics are
 *   authored from this reading's seat over the standing arrangement (the rite
 *   complex itself); the claimed type and the metric values are independent
 *   authored facts. This constraint is one member of a three-file family
 *   decomposing the kernel 'catastrophe memory preservation'; the sibling
 *   files are linked via network.affects_constraints and carry their own
 *   epsilon values and stakeholder structures. KEY AGENTS (by structural
 *   relationship): - survivor_generation_elders: agenda-setter and
 *   beneficiary (organized/identity_locked) — convene and shape the rite,
 *   honored as witnesses, fused with the obligation -
 *   descendant_community_members: beneficiary with secondary payer position
 *   (moderate/mobile) — attend, fund, and staff the rite voluntarily; receive
 *   identity continuity - ritual_officiants: beneficiary
 *   (moderate/constrained) — conduct the rites for service-bounded stipends;
 *   standing tied to continuation - material_prevention_advocates: excluded
 *   (moderate/mobile) — campaign for physical risk-reduction spending in the
 *   same budgets; outside the planning circle -
 *   collective_memory_researchers: observer (analytical/analytical) —
 *   comparative scholarship; no vote, no dependence
 *
 * KEY AGENTS:
 *   - survivor_generation_elders: agenda-setter and beneficiary (organized/identity_locked) — convene and shape the rite, honored as witnesses, fused with the obligation
 *   - descendant_community_members: beneficiary with secondary payer position (moderate/mobile) — attend, fund, and staff the rite voluntarily; receive identity continuity
 *   - ritual_officiants: beneficiary (moderate/constrained) — conduct the rites for service-bounded stipends; standing tied to continuation
 *   - material_prevention_advocates: excluded (moderate/mobile) — campaign for physical risk-reduction spending in the same budgets; outside the planning circle
 *   - collective_memory_researchers: observer (analytical/analytical) — comparative scholarship; no vote, no dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.16).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.14).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Mourning Rite as Symbolic Continuity (Mourning-Practice Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious/collective-memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '4dee24ab-58e1-48f9-968e-fc7d8a48a98d').
narrative_ontology:cs_kernel_codification('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', distributed).
narrative_ontology:cs_authority_grounding('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', lineage).
narrative_ontology:cs_interpretation_layer_present('4dee24ab-58e1-48f9-968e-fc7d8a48a98d').
narrative_ontology:cs_reading_relation('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', foundational, symbolic_continuity_is_the_function).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_the_function, holdable).
narrative_ontology:cs_axiom_grounding('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', symbolic_continuity_is_the_function, empirically_contingent).
narrative_ontology:cs_axiom('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', secondary, mourning_legitimacy_requires_voluntary_participation).
narrative_ontology:cs_axiom_status(mourning_legitimacy_requires_voluntary_participation, holdable).
narrative_ontology:cs_axiom_grounding('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', mourning_legitimacy_requires_voluntary_participation, deontological).
narrative_ontology:cs_reference_frame('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', symbolic_continuity_framework).
narrative_ontology:cs_drift_state('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', contemporary_post_witness_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4dee24ab-58e1-48f9-968e-fc7d8a48a98d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, survivor_generation_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, descendant_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_officiants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__mourning_practice_reading, descendant_community_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, symbolic_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lived through the catastrophe as adults; they convene the annual commemoration, fix its calendar and liturgy, and decide which elements may change. They speak the names of the dead, tend the memorial site between observances, and train successors in the order of the rite. Stepping back would mean breaking a promise they made to fellow survivors; they describe leading the rite as the terms on which they kept living. They receive public honor at each observance and draw no fee.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, survivor_generation_elders, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, survivor_generation_elders, beneficiary).

% Born after the catastrophe; they attend the annual rite, volunteer for site upkeep, and contribute to the memorial fund. Participation is requested, not compelled — a member who stops attending loses no standing, property, or membership, though relatives may express disappointment. What they get back is a fixed place in a story older than themselves: household milestones, marriage seasons, and civic holidays all key off the commemorative calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, descendant_community_members, beneficiary,
    moderate, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, descendant_community_members, payer).

% Conduct the rites — chant, procession order, blessing formulas — and advise the elders on adjustments when the community's composition changes. They are compensated with modest stipends drawn from the memorial fund, calibrated to comparable ceremonial work elsewhere; most supplement with ordinary trades. Their standing depends on the rite continuing, and they advocate gently for its maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_officiants, beneficiary,
    moderate, biographical, constrained, regional).

% Engineers, municipal planners, and some bereaved families who campaign for levees, warning sirens, and building codes in the same budget cycles that fund the commemoration. They are not hostile to mourning; they argue the commemorative line item crowds out capital spending and want a seat in the allocation debate. They rarely attend the rite and are not invited to plan it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, material_prevention_advocates, excluded,
    moderate, biographical, mobile, national).

% Historians, anthropologists, and psychologists who study the community alongside dozens of comparable post-catastrophe communities. They archive liturgies, interview participants, and publish comparisons of commemorative forms; nothing in the community's life depends on their findings and they hold no vote in the rite's design.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, collective_memory_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes remembrance: fixes dates, places, and forms so that hundreds of dispersed households grieve the same event at the same time in the same way, converting private grief into a renewable common reference point and giving newcomers a single entry point into the community's history.
% TRANSFER_FUNCTION: Moves attendance, volunteer labor, and modest fund contributions from participating households to the maintenance of memorial sites and the staging of observances; moves recognition and speaking precedence toward surviving witnesses and officiants during the rite itself.
% ABSENT_VOICES: Material-prevention advocates stand outside the planning circle: they would redirect part of the commemorative budget to physical risk reduction and say so in municipal hearings, not in rite planning. Some abstaining descendants likewise report the liturgy feels inherited rather than chosen but have no channel into form decisions short of joining the elder circle.
% DISAPPEARANCE_RATIONALE: The commemorative calendar organizes the community's year; the memorial sites, the fund, the officiants' roles, and the intergenerational handoff of witness testimony all hang on the rite. Overnight disappearance would strand grief without a shared container, idle the sites, and push members toward improvised private mourning or rival commemorative projects until a replacement form consolidated.
% FOUNDING_PROBLEM: In the catastrophe's aftermath the community faced unintegrable mass grief and the prospect that, within a generation, neither the dead nor the event would hold any common place in daily life; the rite was built to give mourning a shared schedule and the catastrophe a permanent address in the community's self-understanding.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: clinical literature on collective mourning after disaster records the same integrative function for secular state commemorations with no ritual establishment; municipal governments in comparable disaster regions have adopted official remembrance days citing identical needs; and documented cases of post-catastrophe settlements that skipped shared mourning show the predicted dissolution of common reference. None of these attestations originates with the rite's own participants.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.16): the rite draws voluntary attendance, volunteer labor, and modest fund contributions, and returns identity goods to the same households that pay; the residue above the identity_coordination floor (0.08) reflects real overhead — site upkeep, fund administration, officiant stipends bounded to comparable ceremonial wages. Suppression (0.14) is authored as a raw structural property, unscaled by power or scope: abstention is tolerated and carries no formal penalty; the residual value registers soft normative pull (family expectation, visible seating of non-attendees). Theater_ratio (0.20) is low because in this practice performance is the function — the name-reading is not a proxy for remembrance, it is remembrance; the slow rise across the series tracks conventionalization, not yet goal-drift. Accessibility_collapse (0.32) is low: secular commemorations, oral-history archives, and digital memorials remain viable alternatives and some members use them alongside the rite. Resistance (0.22) is low: critique appears as individual abstention and budget advocacy, not organized opposition. claimed_type is authored as rope from the structural facts — a genuine coordination problem (synchronized remembrance), net-beneficial to participants, no suppressed alternative, no victim set — independent of the metric values. All temporal series share one six-point grid; suppression_requirement is deliberately not tracked because the enforcement picture is static (there is no enforcement machinery to ratchet or decay). Receipt surface: gross flows recycle into site upkeep and observance staging; officiant stipends are service-bounded compensation rather than capture of surplus, and the elders' honor is conferred by participants rather than extracted as receipts — gain_flow is authored as the affirmative 'diffuse' after checking every named seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the elders' seat the rite is a constitutive obligation fused with witness identity — extraction computes near zero and exit is unthinkable; from the descendants' seat it is a balanced exchange of modest cost for belonging, with real exit available; from the advocates' seat outside the loop the same budget lines look like opportunity cost. The officiants' seat sees livelihood dependence without agenda control. The engine derives these divergences from the structural data; nothing in this file adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (elders, descendants, officiants) derive low directionality — the arrangement subsidizes them or trades evenly with them. Descendants carry a secondary payer position, pulling their seat toward symmetric (cost approximately equals benefit). No victim group is declared because the opt-in structure produces none: nobody bears a net transfer they did not consent to. The excluded advocates sit outside the transfer loop entirely — they are not coordinated by the rite and neither pay into it nor draw from it; their stake is the counterfactual budget allocation, not the rite itself. Observers hold the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving unintegrable mass grief a shared schedule and fixing the catastrophe in communal memory — remains live: grief arrives anew with each anniversary cycle and each cohort must receive the memory without having witnessed the event, so mandatrophy_resolved is not declared. The monitored risk is generational: as the witness generation thins, maintenance can slide from lived obligation toward heritage performance; the theater_ratio series is the early-warning instrument, and at 0.20 it sits far below drift thresholds. Misclassification guards: reading this as pure extraction would require victims the opt-in structure does not produce; reading it as natural law would erase its constructed, world-rearranging character — if the rite vanished overnight, the community would have to rebuild a commemorative form or dissolve its common reference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (mourning_practice_reading) of the kernel catastrophe_memory_preservation; the disagreement with the sibling readings is located in the transfer question — whether the rite''s preservation-work includes operational threat-recognition content — and in the evaluative reading of a symbolic-only present. Which structural element do the sibling readings modify?',
    'Specify which observable each reading treats as criterial: identity-cohesion outcomes (this reading), hazard-response performance (survival_competence_reading), or historical function trajectory (hybrid_atrophy_reading); the criterial observable selects the epsilon-invariant constraint.',
    'If the criterial observable shifts to operational performance, this constraint''s epsilon and beneficiary structure are replaced by the survival_competence sibling''s; if it shifts to historical trajectory, by the hybrid_atrophy sibling''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame indexicality: this file is one of three readings of the catastrophe-memory kernel.').

omega_variable(
    operational_transfer_null,
    'Does participation in the rite produce measurable operational transfer — improved hazard recognition, evacuation behavior, drill recall — relative to matched non-participating populations?',
    'Comparative cohort studies of communities with and without the rite facing subsequent comparable hazards; cross-generational recall testing of encoded warning content.',
    'Demonstrated transfer dissolves this reading''s epsilon-invariance — the constraint becomes the survival_competence sibling with materially higher stakes and a different cost calculus; null results consolidate this file as the stable reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_transfer_null, empirical, 'Whether the rite''s preservation-work includes operational content or is identity-only.').

omega_variable(
    voluntariness_depth,
    'Is participation genuinely opt-in, or does soft sanction — family expectation, visible marking of non-attendees, marriage-market considerations — make effective exit costly enough to count as suppressed exit?',
    'Abstention-rate tracking across several observance cycles plus structured interviews on reported consequences of non-participation.',
    'If abstention carries real penalty, suppression rises above the authored scalar and the constraint drifts toward a hybrid coordination/extraction profile with a diffuse payer set; if truly costless, the pure-coordination classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_depth, empirical, 'Depth of the opt-in structure behind the no-victim-set claim.').

omega_variable(
    symbolism_theater_confound,
    'Does the theater_ratio metric miscode functional symbolism as goal-drift in a practice whose performance IS its function?',
    'Participant-meaning surveys keyed to specific rite elements, distinguishing activity whose suspension would dissolve identity continuity (functional) from activity retained after no participant reports it as meaningful (theatrical).',
    'Misreading would push the constraint toward inertial classification as conventionalization deepens; correct separation keeps the theater series interpretable for symbolic practices generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolism_theater_confound, conceptual, 'Metric-validity ambiguity for practices where performance is constitutive.').

omega_variable(
    post_witness_mandate_horizon,
    'When the last living witnesses die, does the founding problem — integrating the grief of the affected generation — remain live for descendants, or does the rite become heritage maintenance?',
    'Longitudinal participant-meaning data across generational turnover; comparison of communities that have lost their witness generation with those retaining it.',
    'A dead mandate with persistent form would date a transition toward inertial theatrical maintenance and align the present with the hybrid_atrophy sibling''s account; a live mandate keeps this reading''s classification stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_witness_mandate_horizon, conceptual, 'Generational horizon of the founding problem''s liveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_mourning_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cmp_mourning_tr_t0, observed).
narrative_ontology:measurement(cmp_mourning_tr_t6, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(cmp_mourning_tr_t6, observed).
narrative_ontology:measurement(cmp_mourning_tr_t12, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(cmp_mourning_tr_t12, observed).
narrative_ontology:measurement(cmp_mourning_tr_t18, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement_basis(cmp_mourning_tr_t18, observed).
narrative_ontology:measurement(cmp_mourning_tr_t24, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(cmp_mourning_tr_t24, observed).
narrative_ontology:measurement(cmp_mourning_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(cmp_mourning_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cmp_mourning_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement_basis(cmp_mourning_be_t0, observed).
narrative_ontology:measurement(cmp_mourning_be_t6, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 6, 0.1).
narrative_ontology:measurement_basis(cmp_mourning_be_t6, observed).
narrative_ontology:measurement(cmp_mourning_be_t12, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement_basis(cmp_mourning_be_t12, observed).
narrative_ontology:measurement(cmp_mourning_be_t18, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 18, 0.13).
narrative_ontology:measurement_basis(cmp_mourning_be_t18, observed).
narrative_ontology:measurement(cmp_mourning_be_t24, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(cmp_mourning_be_t24, observed).
narrative_ontology:measurement(cmp_mourning_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(cmp_mourning_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual preserves catastrophe memory' decomposes into three structurally distinct constraints per the epsilon-invariance principle: survival_competence_reading (operational transfer; high stakes, empirically falsifiable), mourning_practice_reading (this file; symbolic identity coordination, low epsilon, opt-in participation), and hybrid_atrophy_reading (historical-decay claim; its epsilon depends on dating the atrophy). Each is a separate file with its own beneficiaries and metrics; this file links both siblings via affects_constraints. Within the family, the survival-competence claim, where empirically supported, is typically cited as evidence for the other two, making it the upstream node.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
