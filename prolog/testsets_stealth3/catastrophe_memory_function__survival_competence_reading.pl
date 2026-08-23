% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover Seder as Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel
 *   catastrophe_memory_function: the survival-competence reading (D5), under
 *   which the commemorative Passover practice is a transmission technology
 *   that converts catastrophic collective memory into embodied, first-person
 *   procedural competence — how to endure catastrophe, keep communal
 *   institutions alive without a center, and rebuild after rupture. The
 *   standing arrangement under assessment is the practice as it operates in
 *   diaspora households; epsilon is authored from this reading's own lights
 *   over that referent, never over the arrangement a rival reading would
 *   prefer. Sibling readings (loss-memory preservation; the hybrid synthesis)
 *   are separate constraints in separate files, linked through the network
 *   rather than folded into this one. Claim and metrics are authored
 *   independently: the reading claims rope — genuine coordination with net
 *   beneficiaries — while the metrics record real frictions: concentrated
 *   preparatory labor, compelled participation, and a slowly rising share of
 *   symbolic performance in secure contexts.
 *
 * KEY AGENTS:
 *   - - diaspora_jewish_households: Primary beneficiary (organized/identity_locked) — stages the annual rehearsal, receives the transmitted repertoire, bears hosting and calendar costs
 *   - - rabbinic_interpretive_authorities: Agenda setter (institutional/identity_locked) — defines the obligation, authorizes the scripts, adjudicates performance; collects interpretive standing from the practice's continuation
 *   - - women_bearing_preparation_labor: Primary payer (moderate/constrained) — performs the majority of preparatory work the evening requires, marginally recorded in the liturgy
 *   - - children_in_transmission_line: Designated recipient (powerless/trapped) — receives the competence transfer as first-person instruction, bears compulsory attendance and performance
 *   - - nonbelieving_compelled_participants: Secondary payer (moderate/identity_locked) — supplies presence and compliance without conviction under family and communal visibility
 *   - - liberation_movements_adopting_template: External beneficiary (organized/arbitrage) — imports the Exodus template's mobilizing power without the practice's obligations
 *   - - universalist_critics: Excluded voice (moderate/mobile) — objects from outside the room that particularist catastrophe-memory hardens boundaries
 *   - - ritual_studies_scholars: Analytical observer (analytical/analytical) — sees the full architecture, collects nothing, bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover Seder as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '9057eb5c-45be-44d7-bb77-70b598846a57').
narrative_ontology:cs_kernel_codification('9057eb5c-45be-44d7-bb77-70b598846a57', fixed_text).
narrative_ontology:cs_authority_grounding('9057eb5c-45be-44d7-bb77-70b598846a57', lineage).
narrative_ontology:cs_interpretation_layer_present('9057eb5c-45be-44d7-bb77-70b598846a57').
narrative_ontology:cs_reading_relation('9057eb5c-45be-44d7-bb77-70b598846a57', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('9057eb5c-45be-44d7-bb77-70b598846a57', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('9057eb5c-45be-44d7-bb77-70b598846a57', foundational, embodied_rehearsal_transmits_procedural_competence).
narrative_ontology:cs_axiom_status(embodied_rehearsal_transmits_procedural_competence, holdable).
narrative_ontology:cs_axiom_grounding('9057eb5c-45be-44d7-bb77-70b598846a57', embodied_rehearsal_transmits_procedural_competence, empirically_contingent).
narrative_ontology:cs_axiom('9057eb5c-45be-44d7-bb77-70b598846a57', foundational, decentralized_home_practice_removes_decapitation_point).
narrative_ontology:cs_axiom_status(decentralized_home_practice_removes_decapitation_point, holdable).
narrative_ontology:cs_axiom_grounding('9057eb5c-45be-44d7-bb77-70b598846a57', decentralized_home_practice_removes_decapitation_point, empirically_contingent).
narrative_ontology:cs_reference_frame('9057eb5c-45be-44d7-bb77-70b598846a57', embodied_capacity_transmission_protocol).
narrative_ontology:cs_drift_state('9057eb5c-45be-44d7-bb77-70b598846a57', contemporary_secure_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9057eb5c-45be-44d7-bb77-70b598846a57', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, children_in_transmission_line).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, liberation_movements_adopting_template).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, rabbinic_interpretive_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, women_bearing_preparation_labor).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, nonbelieving_compelled_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, children_in_transmission_line).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, embodied_rehearsal_transmission_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_continuity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stage the annual home rehearsal: clean, gather, retell, and host. What flows to them is the transmitted repertoire — the story, the questions, the practiced habits of endurance — plus the standing that comes from keeping the chain unbroken. What flows from them is calendar discipline, hosting expense, and the work of making children attend. Leaving the practice would mean stepping outside the family and communal web that constitutes their continuity; few treat that as a live option even where belief has thinned.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households, payer).

% Define the obligation, authorize and compile the telling-scripts, adjudicate how the evening is performed, and rule on edge cases. Their standing as interpreters exists only insofar as the practice continues and keeps raising new cases to interpret; successive generations of commentary are their institutional record. They run no single household's table, but the framework every table follows is theirs to maintain, and abandoning it would dissolve the authority it grounds.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, rabbinic_interpretive_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, rabbinic_interpretive_authorities, beneficiary).

% Perform the large majority of the preparatory work — the cleaning, the cooking, the table-setting the evening requires — while the recorded liturgy assigns them little of the speaking parts. Their labor is what makes the event possible; their names rarely appear in the script. Scaling back draws family complaint and communal remark; withdrawing altogether would mean leaving the communal life the evening anchors.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, women_bearing_preparation_labor, payer,
    moderate, biographical, constrained, global).

% Are the designated recipients: the script assigns them the questions, casts them in the four-children roles, and requires their presence at the table. Through the evening they receive the story as first-person instruction — the telling is addressed to them. They also carry obligations they did not choose: attendance is not negotiable, performance is expected, and departure from the script draws correction. They cannot decline; they are minors at a family table.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, children_in_transmission_line, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, children_in_transmission_line, payer).

% No longer hold the beliefs the evening rehearses but attend and perform anyway under family expectation and communal visibility. They supply presence and compliance without conviction, and the script offers them a pre-assigned dissenting role — the skeptical child — that marks their position as a moral type rather than a view to be argued. Skipping the evening costs relationships; attending costs candor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, nonbelieving_compelled_participants, payer,
    moderate, biographical, identity_locked, global).

% Borrow the Exodus narrative and sometimes the seder format itself — freedom seders, movement retellings — to organize their own constituencies. They receive the template's mobilizing power without carrying the practice's ongoing obligations, calendar discipline, or preparatory labor, and they can set it down whenever it stops serving them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, liberation_movements_adopting_template, beneficiary,
    organized, generational, arbitrage, global).

% Stand outside the practice — secular ethicists, universalist educators, advocates of assimilationist memory policy — and object that a memory organized around particular catastrophe and particular deliverance hardens the boundary between insiders and outsiders. They are not at the table, their objection never enters the evening's script, and it reaches practitioners only as external commentary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, universalist_critics, excluded,
    moderate, generational, mobile, global).

% Study the practice comparatively — alongside other communities' commemorative and disaster-preparedness rituals — and can see the whole architecture at once: what the rehearsal transmits, who does the unseen work, what the interpreters collect, and what happens in communities that let the practice lapse. They collect nothing from the practice and bear none of its costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, rabbinic_interpretive_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of transmitting catastrophe-derived survival competence across generations without centralized institutions: the annual home-based rehearsal converts inherited catastrophe into first-person procedural knowledge ('as though you personally left Egypt'), the mandated question-format trains inquiry and anomaly detection in the young, and the household venue leaves no decapitation point for a persecutor to strike.
% TRANSFER_FUNCTION: Moves household labor and calendar-time (concentrated on preparers) into a recurring pedagogical event; moves narrative authority downward from elders to questioning children; moves communal attention from ordinary life to catastrophic precedent once yearly; and exports the Exodus template to adopting movements outside the practicing community.
% ABSENT_VOICES: Universalist critics who would object that particularist catastrophe-memory hardens outgroup boundaries sit outside the room entirely; women preparers are present in body but their labor is nearly absent from the liturgy that records everyone else's duties; the doubting member's voice is pre-classified by the four-children typology as the wicked child, making structured dissent unsayable at the table.
% DISAPPEARANCE_RATIONALE: Households organize calendars, pedagogy, and identity around the annual rehearsal; if it vanished overnight, the principal embodied channel for converting catastrophe-memory into transmissible competence closes, communal continuity infrastructure loses its keystone event, and adopting movements lose a proven mobilization template — whether substitute channels (schools, media, museums) could carry the same identity-fused load is exactly the open question the omegas track.
% FOUNDING_PROBLEM: After the destruction of the Temple and under stateless dispersion, the community faced a doubled problem: preserving group memory across generations, and preserving practical adaptive know-how — how to endure expulsion, sustain mutual aid, and rebuild institutions — without any central authority that persecutors could destroy. The consolidated home seder was built to solve both at once.
% FOUNDING_PROBLEM_CORROBORATION: From outside the benefiting parties: comparative scholarship on cultural memory and ritual attests the mechanism's design fit for stateless transmission; diaspora-resilience historiography correlates intensive commemorative practice with institutional persistence; and the independent adoption of the Exodus template by external liberation movements attests the transmitted content's adaptive value from beyond the community. No source outside the beneficiary set attests that the original threat environment — statelessness under active persecution — remains the operative problem for secure contemporary diasporas; that status is genuinely disputed, which is why it is authored contested rather than live or dead.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28: the arrangement's costs are real — preparatory labor concentrates on women, attendance is compulsory for children and costly to skip for doubters, and the calendar demands annual household expenditure — but from this reading's own lights the arrangement predominantly subsidizes its participants, delivering competence and continuity they could not otherwise obtain, and its persistence tracks delivered value rather than blocked exits. Suppression is authored at 0.42 and is a RAW structural property the engine does not scale by power or scope: it reflects normative-communal enforcement (obligation, visibility, the pre-typed dissenting role) rather than a coercive apparatus, and the suppression_requirement series falls across the interval as communal enforcement capacity erodes in liberal diasporas — an enforcement-decay trajectory, which is why that series is tracked at all. Theater ratio 0.18: the rehearsal remains mostly functional, but in secure contexts a growing share of activity is symbolic performance detached from survival content. Accessibility collapse 0.38: substitutes exist (schools, media, museums) and remain visible once the mechanism is understood, but none deliver the identity-fused, embodied, household-embedded form, so alternatives narrow without vanishing. Resistance 0.30: feminist labor critique, secular attrition, and liturgical reform press on the practice from inside without threatening its core. All three tracked series share one time grid (t=0..60, step 10) so no metric row borrows another's endpoints. Gain flow is authored as rabbinic_interpretive_authorities: among the named seats, the interpretive class is the one that demonstrably converts the practice's continuation into accrued standing — households receive competence (benefit, not receipt of others' costs), movements take the template and leave, preparers and doubters pay. Fixing cost is prohibitive: replacing the embodied household channel with institutional substitutes would sever transmission before any substitute matured, a cost exceeding the benefit for the only actors positioned to attempt it. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already separate the seats, and an override keyed to a power atom would misapply across the multiple distinct agents sharing each atom.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the interpreter's seat the evening is a working transmission protocol they steward; from the preparer's seat it is hours of unrecorded labor beneath a script that credits everyone else's parts; from the doubter's seat it is compulsory performance of convictions no longer held; from the child's seat it is the most vivid instructional event of the year. Same table, same night, structurally different arrangements. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (households, children, adopting movements, interpretive authorities) place those seats near the beneficiary end of d; the movements' arbitrage exit pushes them furthest of all, since they take the template's value and bear none of the discipline. Victim declarations (preparers, compelled doubters) place those seats near the target end; identity-lock holds them there despite nominal mobility, because exit means leaving the communal web rather than switching venues. Children are beneficiaries who nonetheless sit trapped — subsidized but immobile, unable to decline an instruction addressed to them. Every seat carries global scope: the practice runs wherever diaspora households run, and performance quality is correspondingly hard to verify, which the engine folds into effective extraction for target seats while suppression remains unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against the snare misread: the practice's enforcement is normative, its benefits are broad, and its persistence tracks delivered value — reading it as pure extraction would mistake the price of transmission for predation. The opposite misread is guarded against symmetrically: the efficacy-attribution and secure-context-vestigiality omegas, together with the rising theater series, keep open the possibility that the function is thinning while the form persists — the rope-to-piton drift. Founding-problem status is therefore authored contested rather than dead: the founding threat environment has transformed, and whether the transmitted competence remains adaptive is precisely what the corpus should measure rather than assume.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which preserved content defines this constraint: loss-memory and boundary norms (the mourning_practice_reading), adaptive procedural capacity (this survival_competence_reading), or both at once (the hybrid_transformation_reading)?',
    'Comparative analysis of what the liturgy structurally encodes and what long-run practitioners report acquiring: grief-formation versus procedure-formation, indexed against communities that kept the form while dropping the functional content.',
    'Each sibling reading yields a different beneficiary/victim surface and a different epsilon over the same standing practice; this file authors only the D5 structure, and a hybrid resolution would redistribute both the credited function and the identified payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity of the catastrophe-memory kernel: this story is one reading, not the topic.').

omega_variable(
    transmission_efficacy_attribution,
    'Does the practice cause survival-competence transmission, or do communities with independent resilience factors merely tend to maintain such practices?',
    'Comparative diaspora studies controlling for demographic and economic confounders, plus examination of communities that lapsed from the practice while retaining comparable threat exposure.',
    'If the causal attribution fails, the coordination-function justification weakens and the same structure drifts toward theatrical maintenance of a form whose function is gone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_efficacy_attribution, empirical, 'Causal versus correlational status of the ritual-to-competence transmission claim.').

omega_variable(
    gendered_labor_separability,
    'Is the concentration of preparatory labor on women intrinsic to the transmission structure, or a separable accretion removable without function loss?',
    'Function-preservation tests under redistributed labor: shared-preparation households, feminist haggadot, and communitarian seder formats, assessed against transmission outcomes for children.',
    'If separable, effective extraction falls toward the coordination floor and the rope reading stands cleanly; if intrinsic, the structure carries a persistent asymmetric-cost component the rope claim does not capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_labor_separability, conceptual, 'Whether the payer position of preparers is structural or historical residue.').

omega_variable(
    secure_context_vestigiality,
    'In secure, prosperous diaspora contexts, does the transmitted competence remain adaptive, or has it become vestigial content carried by an intact form?',
    'Longitudinal observance-and-outcome studies, and comparison of household and communal response to stress events (resurgent persecution, displacement, economic rupture) across observance levels.',
    'An adverse resolution raises the theater ratio in those contexts and dates a rope-to-piton drift there, while leaving the reading intact for communities under active threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_context_vestigiality, empirical, 'Context-dependence of the transmitted capacity''s adaptive value.').

omega_variable(
    retention_mechanism_internalization,
    'Is member retention driven by perceived delivered value or by internalized obligation and identity fusion?',
    'Post-exit trajectory studies of leavers: if obligation-flavored compliance and identity strain persist after exit from the practice, the lock is internalized rather than structural.',
    'If internalized, the measured suppression understates the effective lock on payers; the suppression travels with leavers and the payer seats sit nearer the full-target end than the structural data alone suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retention_mechanism_internalization, empirical, 'Structural versus internalized character of the practice''s retention pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmf_survival_competence_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t0, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t10, catastrophe_memory_function__survival_competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t10, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t20, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t30, catastrophe_memory_function__survival_competence_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t30, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t40, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t50, observed).
narrative_ontology:measurement(cmf_survival_competence_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(cmf_survival_competence_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cmf_survival_competence_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t0, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t10, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t10, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t20, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t30, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t30, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t40, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t50, observed).
narrative_ontology:measurement(cmf_survival_competence_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(cmf_survival_competence_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cmf_survival_competence_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t0, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t10, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t10, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t20, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t30, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t30, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t40, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t50, observed).
narrative_ontology:measurement(cmf_survival_competence_su_t60, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(cmf_survival_competence_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Passover commemorates catastrophe' covers structurally distinct claims — preservation of loss-memory and boundary norms (mourning_practice_reading) versus transmission of adaptive procedural capacity (this file) — with the hybrid_transformation_reading asserting both. Each gets its own epsilon, beneficiaries, and victims over the same standing arrangement; values are reading-indexed per OQ-26. The mourning reading is upstream in discourse (it is the practice's own self-description and is cited as evidence for the practice's continuity), while this reading supplies the functional account the hybrid synthesizes; this file links both siblings rather than averaging across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
