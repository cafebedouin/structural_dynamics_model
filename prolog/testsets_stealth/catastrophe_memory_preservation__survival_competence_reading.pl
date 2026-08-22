% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe-Memory Ritual Complex — Survival-Competence Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Communities living with hazards that recur on decadal-to-centennial
 *   timescales — riverine floods, tsunami coasts, fire-prone slopes,
 *   persecution cycles — maintain an annual ritual complex that fuses
 *   mourning for the founding catastrophe with mandatory rehearsal:
 *   processions along evacuation routes, recitation of the event chronicle,
 *   maintenance of marker stones and build-line prohibitions, and scheduled
 *   renewal of communal grief. This story instantiates the
 *   survival-competence reading of that arrangement: the ritual preserves
 *   operational threat-recognition capacity across generations, so that
 *   households who never saw the event nonetheless recognize its precursors,
 *   know the routes, and move early. The claim and the metrics are
 *   independent authored facts: the reading claims a tangled structure —
 *   genuine intergenerational training entangled with enforced grief and
 *   administrator authority — and the metrics describe costly, actively
 *   enforced, slowly theatricalizing operation without being tuned to any
 *   predicted engine output. The colloquial label 'catastrophe-memory ritual'
 *   decomposes, per the epsilon-invariance principle, into three linked
 *   stories over the same standing arrangement; this file is the
 *   survival-competence member of that family.
 *
 * KEY AGENTS:
 *   - ritual_administrators: Agenda-setting beneficiary (organized/identity_locked) — runs the observance, interprets the transmitted texts, collects standing and material support
 *   - future_generations_of_community_members: Primary intended beneficiary (powerless/trapped) — inherits whatever response capacity is or is not maintained
 *   - present_generation_participants: Primary payer (moderate/constrained) — bears the time, labor, offering, and land-use autonomy costs
 *   - elder_observant_households: Dual-positioned payer-beneficiary (moderate/identity_locked) — heaviest givers, deepest identity fusion with the practice
 *   - adolescent_participants: Payer-in-training (powerless/constrained) — bears the drill now and is its intended recipient
 *   - civil_defense_planners: Excluded institutional rival (institutional/arbitrage) — technical substitute kept outside the ritual framework's deliberations
 *   - disaster_researchers: Analytical observer (analytical/analytical) — measures differential outcomes across keeping and non-keeping communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe-Memory Ritual Complex — Survival-Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'd8fc162a-bc13-4653-b932-03c3b559e6bf').
narrative_ontology:cs_kernel_codification('d8fc162a-bc13-4653-b932-03c3b559e6bf', fixed_text).
narrative_ontology:cs_authority_grounding('d8fc162a-bc13-4653-b932-03c3b559e6bf', lineage).
narrative_ontology:cs_interpretation_layer_present('d8fc162a-bc13-4653-b932-03c3b559e6bf').
narrative_ontology:cs_reading_relation('d8fc162a-bc13-4653-b932-03c3b559e6bf', catastrophe_memory_preservation__mourning_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('d8fc162a-bc13-4653-b932-03c3b559e6bf', catastrophe_memory_preservation__hybrid_atrophy_reading, forecloses).
narrative_ontology:cs_axiom('d8fc162a-bc13-4653-b932-03c3b559e6bf', foundational, ritual_drill_transfers_operational_capacity).
narrative_ontology:cs_axiom_status(ritual_drill_transfers_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d8fc162a-bc13-4653-b932-03c3b559e6bf', ritual_drill_transfers_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('d8fc162a-bc13-4653-b932-03c3b559e6bf', secondary, commemorative_cost_is_survival_investment).
narrative_ontology:cs_axiom_status(commemorative_cost_is_survival_investment, holdable).
narrative_ontology:cs_axiom_grounding('d8fc162a-bc13-4653-b932-03c3b559e6bf', commemorative_cost_is_survival_investment, instrumental).
narrative_ontology:cs_reference_frame('d8fc162a-bc13-4653-b932-03c3b559e6bf', operational_transmission_regime).
narrative_ontology:cs_drift_state('d8fc162a-bc13-4653-b932-03c3b559e6bf', contemporary_secularized_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8fc162a-bc13-4653-b932-03c3b559e6bf', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations_of_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, ritual_administrators).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, adolescent_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, elder_observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, elder_observant_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Keep the calendar of the annual observance, maintain the marker stones and the event chronicle, decide each year's drill route and liturgy, and call households to attendance. They train successors and hold the interpretive authority over what the old inscriptions require today. They receive standing, honoraria, and livelihood from the offices they hold. Stepping down means surrendering a role that has organized their entire adult life and their family's place in the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_administrators, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, ritual_administrators, beneficiary).

% Not yet born or still children. They will inherit whatever response capacity the observance has kept alive — route knowledge, precursor signs, the habit of moving early — and they will face the same river, coast, or slope their ancestors did. They cannot decline the inheritance or opt out of the hazard; they can only receive what was or was not maintained on their behalf.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations_of_community_members, beneficiary,
    powerless, generational, trapped, regional).

% Adults who attend the annual rites, contribute labor and offerings, keep the build-line prohibitions on their land, and renew the community's grief on schedule. Attendance costs days each year; the prohibitions cap what their property is worth and where they may build. Skipping brings censure, fines in some congregations, and diminished standing; leaving the community altogether means abandoning kin, land, and livelihood at once.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, constrained, local).

% Older members for whom the observance is the spine of the year and the founding event is a personal or parental memory. They give the most time and carry the strongest felt duty to keep it going. They also draw identity, meaning, and grief-resolution from the practice. For them, letting it lapse is unthinkable regardless of its usefulness, and they police lapses in others accordingly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, elder_observant_households, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, elder_observant_households, beneficiary).

% Children and teenagers marched through the drill and the mourning alike. They absorb the routes and signs as training but experience the grief obligations as imposed. Some lean into the role; others count the years until they can move to the city. Their practical exit is limited while they depend on their households, but unlike their elders they have not yet made the practice part of who they are.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, adolescent_participants, payer,
    powerless, immediate, constrained, local).

% Regional and national agencies running sirens, engineered barriers, school drills, and warning systems. They regard the ritual observance as a rival channel for preparedness funding and public attention, and their technical alternative is rarely admitted into the ritual framework's own deliberations. Cooperation happens informally, if at all; their proposals for substitution are heard outside it or not at all.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, civil_defense_planners, excluded,
    institutional, generational, arbitrage, national).

% Comparative scholars and hazard scientists who measure whether communities that keep the observance respond faster and lose fewer people when events recur than demographically similar communities that do not. They publish, advise governments, and testify after disasters, but hold no seat in the observance's own deliberations and no standing to amend it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, disaster_researchers, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational transmission problem for rare-event competence: it schedules rehearsal on a clock faster than the hazard recurs, embeds route and sign knowledge in embodied practice rather than documents alone, maintains physical markers against erasure, and standardizes muster behavior so people who never saw the event can act in concert when it returns.
% TRANSFER_FUNCTION: Moves time, labor, offerings, and land-use autonomy from present households into the maintenance of shared response capacity held for future cohorts; moves interpretive authority and material support to the administrator line; moves private grief into scheduled communal form.
% ABSENT_VOICES: Civil-defense planners stand outside the framework with a substitution proposal no one inside is obliged to hear. Emigrants who left the community bear no seat and cannot register what the obligations cost them. The founding catastrophe's dead cannot consent to how their deaths are invoked to compel attendance. Children too young to assent are bound to grief obligations before they can evaluate them.
% DISAPPEARANCE_RATIONALE: If the observance vanished overnight, the transmission chains break within a generation: marker stones go unmaintained and their inscriptions unread, build-line prohibitions lose their enforcer and construction creeps back into the hazard zone, evacuation routes drop out of bodily memory, and the chronicle stops being recited. At the next recurrence the community meets the event as first-timers; the mortality and behavioral differential that researchers measure between keeping and non-keeping communities would appear here. Administrators would lose their offices, civil-defense agencies would absorb part of the function, and the grief calendar would dissolve into private mourning.
% FOUNDING_PROBLEM: After the founding catastrophe, survivors held knowledge the next generation would need only decades later, after every firsthand witness was gone: which signs precede the event, which routes escape it, where not to build. Ordinary instruction decayed with memory, and the community needed a vehicle that would force rehearsal and refresh the knowledge on a schedule independent of anyone's recollection.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative disaster epidemiology reporting lower mortality in communities that maintained evacuation observances than in matched communities that did not; geological and documentary reconstructions confirming the hazard recurs on the relevant timescale; post-event engineering surveys crediting maintained markers and drilled routes with survival outcomes. No external source attests the ritual mechanism specifically over general preparedness culture — that residual attribution gap is carried by the transfer-efficacy omega rather than resolved here.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Interval units are years since the founding observance was codified (a roughly century-long arc from living-witness era to third-generation practice). Extractiveness ends at 0.68 because participation is obligatory and costly — days of attendance, offerings, labor on markers, and build-line prohibitions that cap property value — while the protective payoff is deferred, probabilistic, and concentrated at unpredictable recurrence moments. Suppression ends at 0.62: persistence rests on censure, fines in stricter congregations, and exclusion from office and marriage standing, not on participant preference; note suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled downstream. Theater ratio ends at 0.35: the drill core remains real, but as living memory faded the ceremonial share grew relative to operational content, a slow Goodhart drift visible in the rising series. Accessibility collapse is 0.48, not high: genuine alternatives exist (civil-defense infrastructure, written manuals, school drills) and partially substitute, so understanding the arrangement does not foreclose every exit. Resistance is 0.52: chronic absenteeism, youth attrition to cities, and recurring proposals to secularize the drill component. The three series share one time grid (points 0/20/40/60/80/100) with every metric authored at every point; the trajectories are monotonic, not cyclical — enforcement intensity ratchets upward as intrinsic motivation decays with generational distance from the founding event, and the base_properties scalars report the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the administrator seat the arrangement is an inheritance being kept: the costs are the price of continuity and the enforcement is stewardship. From the present-participant seat the same calendar is a levy on time, money, and land use collected under social penalty. The future-generation seat experiences pure subsidy but cannot speak — its interests are represented only by administrators who claim to speak for it. Elder households sit astride the divide, paying most and fused deepest. Disaster researchers see a testable empirical claim where participants see a duty; civil-defense planners see a rival delivery channel. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are declared beneficiaries with trapped exit: derivation places them near the full-beneficiary end (d near 0) — the arrangement subsidizes them entirely and they cannot refuse the inheritance. Ritual administrators are agenda-setters with a secondary beneficiary position and identity_locked exit: derivation places them low (d roughly 0.15) — they collect standing and support and cannot abandon the office that constitutes them. Present-generation participants are declared victims with constrained exit: derivation places them near the full-target end (d roughly 0.85). Adolescent participants are declared victims, which derives a high d, but structurally they sit nearer symmetric (roughly 0.55): they pay attention and obedience now and are simultaneously the trainees the capacity is built for. No directionality override is authored to capture this: overrides key on the power atom, and the only atom the adolescents share (powerless) is occupied by the future-generation seat, whose d must stay near zero — a single override would misapply across structurally opposite seats. The adolescent asymmetry is therefore documented here and left to the derivation chain's known limitation.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading the founding problem — transferring rare-event competence past the death of every firsthand witness — is still live, so no mandatrophy resolution is declared and the mismatch consumer should read status=live paired with verdict=world_rearranges: no zombie flag. The classification work the type does here is bidirectional. A pure-coordination reading would erase the enforcement machinery, the autonomy costs, and the administrator's accumulating authority — the metrics forbid that. A pure-extraction reading would erase the drill function that comparative mortality data plausibly supports — the beneficiaries and coordination function forbid that. The tangled structure holds both: someone is coordinated (households rehearse together on a clock independent of individual memory) and someone pays (present households, under penalty). The omegas mark the two paths out: if transfer efficacy resolves null, the founding problem dies while the arrangement persists and the story migrates toward the hybrid-atrophy sibling's territory with a capture/zombie flag; if grief and drill prove separable, the entanglement loosens and the extraction component becomes independently addressable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_transfer_efficacy,
    'Does maintaining the observance actually preserve operational threat-recognition capacity — faster response, correct route choice, precursor recognition — relative to matched communities that lack it?',
    'Matched-pair post-event mortality and behavioral studies comparing observance-keeping and non-keeping communities; audits separating operational drill content from ceremonial content; retention testing of route and sign knowledge across age cohorts.',
    'Affirmative results anchor this reading and its epsilon; null results collapse the reading toward the mourning-practice sibling, re-index epsilon downward, and shift the arrangement toward a form whose costs purchase only meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_transfer_efficacy, empirical, 'Whether the ritual''s operational-transfer claim is empirically true.').

omega_variable(
    kernel_reading_index_commitment,
    'This story instantiates the survival_competence_reading of the catastrophe_memory_preservation kernel; how would epsilon, victim structure, and type shift if the sibling readings were instantiated instead?',
    'Author the sibling stories (mourning_practice_reading, hybrid_atrophy_reading) over the same standing arrangement and compare computed per-seat classifications across the family.',
    'Under the mourning-practice sibling the goods are symbolic, autonomy costs weigh against thinner benefits, and epsilon drops; under the hybrid-atrophy sibling epsilon is similar but the persistence basis shifts toward inertia and performance, moving the type toward degraded forms. The disagreement is located entirely in the transfer claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index_commitment, conceptual, 'Committer structure: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is non-participation deterred by structural sanction (censure, fines, exclusion from office and marriage standing) or by internalized duty that would persist if sanctions were removed?',
    'Post-exit trajectory of emigrants who left the community: if observance duties, build-line avoidance, and scheduled grief persist privately after exit, the internalized share is large.',
    'If internalized, effective suppression exceeds the structural measure and binds even geographically mobile members; if purely structural, suppression decays with enforcement capacity and exit widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism ambiguity in the measured suppression scalar.').

omega_variable(
    recurrence_timescale_uncertainty,
    'On what timescale does the founding catastrophe recur, and does that timescale outrun even ritually maintained memory?',
    'Paleo-event reconstruction — sediment layers, tree rings, documentary chronicles — bounding recurrence intervals for the specific hazard.',
    'Recurrence shorter than the ritual memory horizon validates the investment framing; recurrence long enough to exceed any maintainable capacity undermines the benefit side and raises net extraction on present participants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recurrence_timescale_uncertainty, empirical, 'Hazard recurrence interval versus achievable transmission horizon.').

omega_variable(
    grief_drill_separability,
    'Can the drill component (muster, routes, signs) be separated from the mourning component (lament, feast, obligation) without collapsing participation?',
    'Natural experiments where civil authorities add secular drills alongside the rite, or where reform factions split off drill-only observances; compare knowledge retention and attendance across the variants.',
    'If separable, the mourning wrapper is overhead riding on a real training core and the entanglement loosens toward a cleaner coordination-plus-extraction split; if inseparable, the grief bond is the carrier that makes century-scale rehearsal possible at all, and much of the measured cost is constitutive rather than extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grief_drill_separability, conceptual, 'Structural separability of the grief and drill components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_survival_reading_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cmp_survival_reading_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cmp_survival_reading_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cmp_survival_reading_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(cmp_survival_reading_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(cmp_survival_reading_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(cmp_survival_reading_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cmp_survival_reading_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cmp_survival_reading_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(cmp_survival_reading_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(cmp_survival_reading_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(cmp_survival_reading_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cmp_survival_reading_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cmp_survival_reading_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(cmp_survival_reading_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(cmp_survival_reading_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement(cmp_survival_reading_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(cmp_survival_reading_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'catastrophe-memory ritual' conflates three structurally distinct claims about one standing arrangement. This story (survival_competence_reading) authors epsilon for the arrangement assessed as an operational transmission regime. The mourning_practice_reading sibling authors epsilon for the same arrangement assessed as symbolic-continuity-only — different beneficiary weights, lower epsilon. The hybrid_atrophy_reading sibling authors epsilon for the arrangement assessed as a formerly operational practice now sustained by inertia and performance — similar epsilon, different persistence basis and type trajectory. Upstream/downstream: the survival reading is the historically prior claim; the hybrid reading cites the survival reading's past success as the baseline against which present atrophy is measured, so this story links to both siblings and the family must be read together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
