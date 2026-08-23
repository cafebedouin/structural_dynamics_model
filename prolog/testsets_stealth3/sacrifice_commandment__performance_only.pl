% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment - Performance-Only Reading: Suspension Without Fulfillment
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   The Second Temple's destruction in 70 CE left the commandment of animal
 *   sacrifice binding in principle and unperformable in fact, and this story
 *   instantiates one reading of that arrangement: the commandment's discharge
 *   condition is physical execution on the altar, no available act
 *   substitutes for it, and the commandment therefore stands suspended rather
 *   than fulfilled. Under that reading the standing arrangement is stark: for
 *   roughly nineteen centuries the tradition's scarcest intellectual
 *   resource, first-rank scholarly attention, has been allocated at full
 *   depth (an entire Mishnah order, the associated Talmudic tractates, two
 *   codified volumes of Maimonides, a fixed daily liturgical recitation) to a
 *   rite that cannot occur, while the living law of civil adjudication and
 *   practical guidance competes for the same hours. The academies that
 *   administer the curriculum collect prestige, credentialing authority, and
 *   institutional continuity from the arrangement; the scholars bear the
 *   opportunity cost; the communities receive less living-law output than the
 *   diverted attention would have produced; and a marginal restoration
 *   movement draws its program from the same suspended liveness. This file is
 *   the performance_only reading of the sacrifice_commandment kernel; the
 *   kernel contest and the sibling readings are recorded in
 *   commentary.kernel_context, the omega variables, and the network note, and
 *   nothing here adjudicates them.
 *
 * KEY AGENTS:
 *   - rabbinic_academies: agenda-setter and primary beneficiary (institutional power, identity-locked exit, global scope) - administers the curriculum that keeps the suspended commandment's law at the prestige core, and collects status, credentialing authority, and continuity from doing so
 *   - rabbinic_scholars: primary target (moderate power, constrained exit, global scope) - their finite attention is the resource the arrangement consumes; redirection to living law is possible but carries prestige cost
 *   - jewish_communities: secondary target (organized power, constrained exit, global scope) - bear the diffuse shortfall in civil adjudication and practical guidance that the diverted attention would otherwise have produced
 *   - temple_restoration_advocates: marginal actors (moderate power, constrained exit, regional scope) - the practical corollary of this reading, since if only execution counts the remedy is to restore the execution site; sidelined by the academies' administration of the suspension
 *   - jewish_studies_scholars: analytical observer (analytical power, analytical exit, global scope) - historiography of the post-destruction crisis sees the whole structure from outside the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.78).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.6).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment - Performance-Only Reading: Suspension Without Fulfillment").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, 'c3adfae0-5f5e-4c4e-a379-9ac031108d8c').
narrative_ontology:cs_kernel_codification('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', fixed_text).
narrative_ontology:cs_authority_grounding('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', lineage).
narrative_ontology:cs_interpretation_layer_present('c3adfae0-5f5e-4c4e-a379-9ac031108d8c').
narrative_ontology:cs_reading_relation('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', foundational, sacrifice_discharge_requires_physical_execution).
narrative_ontology:cs_axiom_status(sacrifice_discharge_requires_physical_execution, holdable).
narrative_ontology:cs_axiom_grounding('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', sacrifice_discharge_requires_physical_execution, deontological).
narrative_ontology:cs_axiom('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', foundational, unperformable_commandment_remains_binding).
narrative_ontology:cs_axiom_status(unperformable_commandment_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', unperformable_commandment_remains_binding, deontological).
narrative_ontology:cs_reference_frame('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', temple_executable_commandment).
narrative_ontology:cs_drift_state('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', post_destruction_dispersion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c3adfae0-5f5e-4c4e-a379-9ac031108d8c', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_academies).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, rabbinic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, jewish_communities).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, non_lapse_of_divine_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the curriculum across the yeshiva and academy network: which orders sit at the center of advanced study, what mastery the credentialing examinations test, and which analytic methods confer standing. The law of the sacrifices - an entire Mishnah order, the associated Talmudic tractates, and two codified volumes - sits at the deep end of that curriculum, and the institutions confer their highest standing on those who master it. They maintain the daily liturgical recitation of the sacrificial passages and the petition for the Temple service's restoration. Their exit from this curricular shape would be the dissolution of the identity the institutions have built around it: the deep curriculum is not a program they run but what they are.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_academies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, rabbinic_academies, beneficiary).

% Spend the core of their working lives inside the deep curriculum: the sacrificial law's intricate analysis is where analytic distinction is made and where the tradition's hardest problems live. The hours are finite; every year given to the suspended rite's law is a year not given to civil adjudication, commercial law, or the practical questions communities actually bring. Redirection is possible - colleagues do it, and the practical-halakha track is honored - but it costs standing in the hierarchy that ranks deep sacrificial analysis highest, and for the most identified, leaving the deep end feels like leaving the tradition's core.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_scholars, payer,
    moderate, biographical, constrained, global).

% Depend on the scholarly class for civil adjudication, commercial guidance, and answers to practical questions, and receive that service from whatever attention remains after the deep curriculum takes its share. They fund the academies, send them their students, and recite the sacrificial passages in the daily liturgy; the shortfall in living-law attention reaches them as slower rulings, thinner commercial guidance, and practical questions deferred. They can found courts and fund practical-halakha institutions, and increasingly do, but the deep-end prestige sets the terms slowly and from above.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, jewish_communities, payer,
    organized, generational, constrained, global).

% Take the reading's practical corollary seriously: if the commandment counts only when performed on the altar, then the remedy is to make the altar possible again. They prepare vessels, train candidates for the priestly service, and search for the ritual prerequisites, operating at the margin of the halakhic mainstream and centered in Israel. The academies' administration of the suspension - study and liturgy in place of execution, restoration petitioned but not engineered - keeps their program outside the conversation that allocates attention, and they cannot force their way in.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_advocates, excluded,
    moderate, biographical, constrained, regional).

% Study the post-destruction crisis and the tradition's response from outside the framework: the historiography of 70 CE and its aftermath, the development of the substitution doctrines, the curriculum's evolution. They hold no position inside the arrangement, bear none of its costs, collect none of its standing, and can see the whole structure - the suspended commandment and the attention economy built around it - without a seat in any of it.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, jewish_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, rabbinic_academies).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a Temple-centered commandment legally alive across nineteen centuries of dispersion: the corpus of sacrificial law stays coherent and teachable, the academies share a curriculum and a credentialing hierarchy that binds a scattered scholarly class into one tradition, and the daily liturgy keeps the Temple service present in communal memory so the commandment neither dies of neglect nor dissolves into metaphor.
% TRANSFER_FUNCTION: Moves first-rank scholarly attention - the tradition's scarcest intellectual resource - from living law (civil adjudication, commercial guidance, practical response) to the law of a rite that cannot be performed; and moves standing, credentialing authority, and institutional continuity upward to the academies and the deep-end elite who master that law.
% ABSENT_VOICES: The would-be living-law specialists who never form: students the prestige hierarchy pulls into the deep end before they ever reach civil law, who would testify that the tradition's practical needs went begging while the deep analysis flourished. The communities whose commercial and civil disputes wait on under-resourced adjudication. And within the liturgy itself, the minorities whose rites abbreviated the sacrificial recitation - their objection survives in the divergent prayer-book traditions but carries no seat in the academies that set the curriculum.
% DISAPPEARANCE_RATIONALE: If the performance requirement and the suspended-but-binding treatment vanished overnight - if the commandment were annulled, or if study were held to discharge it - the deep curriculum would lose its legal warrant and demote to historical study, the daily sacrificial recitation would lose its reason and follow the Reform precedent out of the liturgy, the credentialing hierarchy would re-anchor on living law, and the restoration advocates would lose the legal basis that makes their program the reading's consistent corollary. The academy system, the liturgy, the prestige economy, and the restoration movement all stand on the commandment's suspended liveness.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE stripped a legal order built around sacrifice of its execution site. The tradition faced three exits: declare the commandment lapsed, declare it fulfilled by substitutes, or hold it suspended - binding in principle, unperformable in fact. This arrangement is the third exit: preserve the commandment's integrity by refusing both annulment and substitution, at the price of a permanent, publicly acknowledged suspension.
% FOUNDING_PROBLEM_CORROBORATION: The payer seat attests it from outside the beneficiary set: the scholars who bear the cost of the suspension are also its most scrupulous maintainers - they continue to hold the commandment binding and its law central, which a merely captured class would not do against its own interest. Communal liturgical practice corroborates independently: the daily recitation and restoration petition are maintained by communities that receive no institutional gain from the curriculum. Academic historiography of the post-70 crisis, produced entirely outside the framework, attests the founding decision and its refusal of both rival exits. No source inside the beneficiary set is relied on.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because this reading's defining claim removes every discharge story: what remains of nineteen centuries of first-rank attention allocated at full depth to an unperformable rite is opportunity cost, and the depth of the allocation (an entire Mishnah order at the prestige core of the curriculum) maximizes it. Suppression (0.6) is real but not total: the lock is credentialing, liturgical embedding, and identity rather than prohibition, and it is roughly one-third structural (curricular gates, the daily liturgical recitation that re-inscribes the arrangement) and two-thirds internalized (the deep-analysis self-concept that makes redirection feel like leaving the tradition's core) - a scholar may study civil law tomorrow, at a prestige price the hierarchy sets. Theater (0.48) reflects the arrangement's performative layer: the daily sacrificial recitation, the seder-plate memorials, and the restoration petition re-enact a rite that cannot occur; the study itself is genuine, if non-discharging, labor, so the ratio sits near half. Accessibility collapse is low (0.35): understanding the constraint does not close the alternative of living-law study - the option set survives; what bends is the motivation structure that ranks it. Resistance (0.5) is sustained: the rationalist concession-reading of sacrifice, the Reform liturgical excision of the sacrificial passages, and the modern practical-halakha prestige track all push against the deep-end allocation, and the practical-halakha track functions as a partial coalition of the payer seats. The measurement series share one nine-point grid. The series are smoothed: messianic episodes (the Sabbatean surge around grid point 1600, modern restoration movements) produce transient spikes in mobilization whose collapse never returns the apparatus to its pre-surge level - a rectifying ratchet - and the oscillation functions as intermittent reinforcement, since each restoration surge re-invests the suspended rite with imminence and re-mobilizes elite attention. The base_properties values are measured at the current post-surge plateau (grid endpoint 1950).
 *
 * PERSPECTIVAL GAP:
 *   From the academy seat the arrangement is the tradition's crown: the deep curriculum that makes a scholar, the legal liveness that keeps a Temple-centered commandment from dying of neglect, the continuity that carried the corpus across dispersion. From the scholar seat the same structure is a nineteen-century claim on the scarcest resource the tradition has, spent on a rite that cannot occur while civil law and practical guidance compete for the same hours. From the community seat it is a service shortfall with no visible administrator. The engine computes per-seat classifications from the structural data; the divergence between the administrator's and the payers' experience of one arrangement is the measurement the seat structure exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The academies are the declared beneficiary and sit near the beneficiary end: they collect prestige, credentialing authority, and continuity, and their exit is identity-locked - the institution has become its curriculum - which pins their directionality low despite the indirect costs their personnel bear. The scholars are declared victims with constrained exit: redirection is possible at prestige cost, so they sit near but not at the full-target end. The communities are declared victims with diffuse, second-order costs and constrained exit: they can found courts and fund practical-halakha institutions, and increasingly do, but slowly. The restoration advocates are structurally ambiguous - they draw purpose from the commandment's live-but-suspended status (a beneficiary relation) while the academies' administration of the suspension sidelines their program (a target relation) - and are left to the derivation's fallback, with the ambiguity flagged as an omega rather than forced by an override. No directionality overrides are authored: the beneficiary and victim declarations plus exit options produce the right d for every declared seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preserving a divine commandment's integrity after its execution site was destroyed, refusing both annulment and substitutionary discharge - is live from inside the framework: no Temple stands, so the commandment remains suspended, and the arrangement is not a zombie mandate. The classification risk here runs in the reading's own polemical direction: a deflationary reading wants to call the whole apparatus pure extraction or pure inertia. The structural data block both moves. It is not the inertial shape: the gains are captured - the academies demonstrably accrue prestige and credentialing authority, and gain_flow names that seat - so the no-concentrated-beneficiary test fails. It is not the pure-extraction shape: the coordination core is genuine (corpus coherence, commandment liveness, liturgical and scholarly-class continuity across dispersion) and the alternatives are not suppressed - living-law study is honored and accessible, and the exit from the deep end is costly but open. What remains is the tangled middle: a real coordination function and a real, concentrated transfer through the same structure, held by active enforcement. The founding problem's liveness is attested from outside the beneficiary set - by the payer-seat scholars themselves, who bear the cost and still maintain the commandment's binding status, and by communal liturgical practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_discharge_condition_contest,
    'This story is the performance_only reading of the sacrifice_commandment kernel: would the sibling readings author the same standing arrangement with epsilon near zero, and is this reading''s high extractiveness therefore a property of the discharge-condition dispute rather than of the arrangement itself?',
    'Author the sibling stories against the identical referent (the standing study apparatus, 70 CE to present) and compare epsilon: the study-as-exercise reading, if its premise holds, converts the same nineteen centuries of labor into the commandment''s exercise (epsilon near the coordination floor); the preservation-for-restoration reading converts part of it into preparation value (epsilon mid-range). The disagreement is located at one structural point, the discharge condition, and only the sibling files can carry their side of it.',
    'If the study-as-exercise reading is structurally right, this file''s epsilon is misattributed and the arrangement is a low-extraction coordination structure; if the preservation reading is right, epsilon drops toward the tangled middle. This file''s classification is conditional on its reading''s premise holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_discharge_condition_contest, conceptual, 'Kernel contest: the discharge condition determines whether 1,900 years of study labor was fulfillment, preservation, or diversion.').

omega_variable(
    academy_capture_vs_inertial_persistence,
    'Do the academies genuinely capture the arrangement''s gains, or does the deep-end allocation persist by institutional inertia with no concentrated beneficiary, which would move the story toward the no-capturer shape?',
    'Curriculum-allocation evidence: whether elite time-allocation tracks prestige rewards the academies control (capture) or persists where institutional reward is absent (inertia). Historical probes: curricular reform episodes such as the Mussar-era rebalancing and the modern practical-halakha tracks, and whether the prestige deep end reasserted itself afterward.',
    'If no seat captures, gain_flow should read diffuse and the classification drifts toward the inertial shape; if the academies capture, the named-seat receipt stands and the tangled middle holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academy_capture_vs_inertial_persistence, empirical, 'Whether the arrangement has a concentrated capturer or persists by inertia alone.').

omega_variable(
    suppression_identity_lock_mechanism,
    'Is the scholars'' continued allocation to the deep end held by structural enforcement (credentialing gates, curricular requirements, liturgical embedding) or by internalized identity fusion (the deep-analysis identity that makes redirection feel like self-erosion)?',
    'Post-exit trajectory: track scholars who redirect to living law; if the pull back toward the deep end persists after every structural gate is passed, the lock is substantially internalized. Compare late entrants to the tradition against lifelong curriculum products.',
    'If internalized, effective suppression exceeds the structural measure and survives institutional reform: rebalancing the curriculum would not release the attention, because the lock is carried in the scholars'' professional self-concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_identity_lock_mechanism, empirical, 'Structural versus internalized share of the lock on scholarly attention.').

omega_variable(
    restoration_contingency_of_verdict,
    'The high-extraction verdict is contingent on permanent suspension: if the Temple were rebuilt and execution resumed, would the accumulated study labor convert into preparation, collapsing this reading''s epsilon?',
    'Not resolvable by data inside the framework, since the restoration question is eschatological. Observable proxy: how this reading''s holders revalue the study labor during near-restoration episodes (red-heifer episodes, vessel-reproduction projects). If they retroactively describe the labor as preparation, the contingency is live.',
    'Any credible restoration path reclassifies the arrangement toward transitional support, a structure whose justification is the coming transition and which would carry a restoration clause rather than a sunset; permanent suspension is the load-bearing assumption of the high epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency_of_verdict, conceptual, 'Whether the high-extraction verdict survives a restoration counterfactual.').

omega_variable(
    living_law_shortfall_attribution,
    'Is the communities'' claimed cost, the living-law shortfall, actually caused by the deep-end allocation, or would the diverted attention have gone to other non-practical analysis absent the sacrifice curriculum?',
    'Comparative attention studies across communities and periods where the deep-end prestige was weaker (practical-halakha-centered traditions, post-Emancipation seminaries): did civil-law and practical-guidance output rise commensurately with the deep end''s decline?',
    'If the counterfactual attention would have gone to other speculative analysis, the diversion is study-to-study rather than study-to-living-law, the communities drop out of the victim set, and epsilon falls because the extraction''s opportunity cost was internal to the study economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_law_shortfall_attribution, empirical, 'Whether the opportunity cost of the deep-end allocation lands on living law or on other study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_commandment__performance_only, theater_ratio, 250, 0.26).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.32).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_commandment__performance_only, theater_ratio, 800, 0.36).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__performance_only, theater_ratio, 1100, 0.4).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_commandment__performance_only, theater_ratio, 1400, 0.43).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_commandment__performance_only, theater_ratio, 1600, 0.5).
narrative_ontology:measurement(sacr_tr_t1700, sacrifice_commandment__performance_only, theater_ratio, 1700, 0.46).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__performance_only, theater_ratio, 1950, 0.48).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sacr_be_t250, sacrifice_commandment__performance_only, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.52).
narrative_ontology:measurement(sacr_be_t800, sacrifice_commandment__performance_only, base_extractiveness, 800, 0.58).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__performance_only, base_extractiveness, 1100, 0.64).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_commandment__performance_only, base_extractiveness, 1400, 0.67).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_commandment__performance_only, base_extractiveness, 1600, 0.71).
narrative_ontology:measurement(sacr_be_t1700, sacrifice_commandment__performance_only, base_extractiveness, 1700, 0.7).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__performance_only, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t250, sacrifice_commandment__performance_only, suppression_requirement, 250, 0.28).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.4).
narrative_ontology:measurement(sacr_su_t800, sacrifice_commandment__performance_only, suppression_requirement, 800, 0.48).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_commandment__performance_only, suppression_requirement, 1100, 0.54).
narrative_ontology:measurement(sacr_su_t1400, sacrifice_commandment__performance_only, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_commandment__performance_only, suppression_requirement, 1600, 0.66).
narrative_ontology:measurement(sacr_su_t1700, sacrifice_commandment__performance_only, suppression_requirement, 1700, 0.56).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_commandment__performance_only, suppression_requirement, 1950, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, enforcement_mechanism).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three constraint stories because the label 'the sacrifice commandment after 70 CE' covers three structurally distinct claims with different discharge conditions: this file (performance_only - only execution discharges, the commandment is suspended, so the study apparatus's labor is opportunity cost; epsilon high), sacrifice_commandment__study_as_performance (study IS the exercise - the same labor is fulfillment; epsilon near the coordination floor), and sacrifice_commandment__archive_maintenance (study preserves for restoration - the labor is preparation value; epsilon mid-range). Same referent arrangement, three epsilon values, three beneficiary structures, three classifications - one story per reading, linked here. This reading's edges: it forecloses study_as_performance (the premises contradict on the discharge condition, so no single framework holds both) and influences archive_maintenance (the suspended-not-annulled status is the load-bearing condition the preservation project requires).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
