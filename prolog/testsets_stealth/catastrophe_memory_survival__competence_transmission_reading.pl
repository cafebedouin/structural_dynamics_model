% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual Competence Transmission (Catastrophe-Memory Kernel, Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Across catastrophe-prone and diaspora histories, communities bind
 *   practical survival knowledge — planting and rationing timing, food
 *   handling and sanitation, kinship mutual-aid obligations, adaptation
 *   playbooks — into ritual cycles whose repetition, communal enforcement,
 *   and portability let the knowledge survive the destruction of archives,
 *   schools, and land tenure. This story instantiates the
 *   competence_transmission_reading of the catastrophe_memory_survival
 *   kernel: the standing arrangement under classification is ritual practice
 *   as the operative carrier of survival competence, and epsilon is authored
 *   for that arrangement as this reading sees it. The sibling readings
 *   (symbol_survival, hybrid_encoding) are separate constraints over the same
 *   arrangements with their own epsilon and victim sets; they are linked
 *   through the network, not folded into this one. KEY AGENTS (by structural
 *   relationship): - diaspora_communities: Primary beneficiary
 *   (organized/constrained) — displaced populations whose adaptive capacity
 *   rides on the ritual channel - ritual_specialists: Administrator and
 *   secondary beneficiary (institutional/identity_locked) — run the
 *   transmission, collect status and support from the observance economy -
 *   content_decayed_communities: Primary bearer of costs
 *   (moderate/constrained) — perform the full cycle after the practical
 *   referents have dropped out - context_mismatched_observers: Secondary
 *   bearer of costs (moderate/constrained) — comply with protocols calibrated
 *   to a vanished ecology - younger_generation_questioners: Excluded voice
 *   (moderate/constrained) — ask for content without form; outside the
 *   interpretive conversation - comparative_ritual_scholars: Analytical
 *   observer (analytical/analytical) — compare transmission outcomes across
 *   traditions
 *
 * KEY AGENTS:
 *   - diaspora_communities: Primary beneficiary (organized/constrained) — adaptive capacity carried by the ritual channel
 *   - ritual_specialists: Administrator and secondary beneficiary (institutional/identity_locked) — run transmission, collect status and livelihood
 *   - content_decayed_communities: Primary cost-bearer (moderate/constrained) — full observance, lost referents
 *   - context_mismatched_observers: Secondary cost-bearer (moderate/constrained) — intact content, mismatched context
 *   - younger_generation_questioners: Excluded voice (moderate/constrained) — content-without-form proposal never reaches the agenda
 *   - comparative_ritual_scholars: Analytical observer (analytical/analytical) — cross-tradition outcome comparison
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual Competence Transmission (Catastrophe-Memory Kernel, Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'c6f4bb00-65f4-4ecb-883b-b581dc856abe').
narrative_ontology:cs_kernel_codification('c6f4bb00-65f4-4ecb-883b-b581dc856abe', distributed).
narrative_ontology:cs_authority_grounding('c6f4bb00-65f4-4ecb-883b-b581dc856abe', lineage).
narrative_ontology:cs_interpretation_layer_present('c6f4bb00-65f4-4ecb-883b-b581dc856abe').
narrative_ontology:cs_reading_relation('c6f4bb00-65f4-4ecb-883b-b581dc856abe', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6f4bb00-65f4-4ecb-883b-b581dc856abe', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('c6f4bb00-65f4-4ecb-883b-b581dc856abe', foundational, practical_content_is_the_survival_payload).
narrative_ontology:cs_axiom_status(practical_content_is_the_survival_payload, holdable).
narrative_ontology:cs_axiom_grounding('c6f4bb00-65f4-4ecb-883b-b581dc856abe', practical_content_is_the_survival_payload, empirically_contingent).
narrative_ontology:cs_axiom('c6f4bb00-65f4-4ecb-883b-b581dc856abe', secondary, form_without_content_is_net_cost).
narrative_ontology:cs_axiom_status(form_without_content_is_net_cost, holdable).
narrative_ontology:cs_axiom_grounding('c6f4bb00-65f4-4ecb-883b-b581dc856abe', form_without_content_is_net_cost, instrumental).
narrative_ontology:cs_reference_frame('c6f4bb00-65f4-4ecb-883b-b581dc856abe', ritual_as_living_competence_archive).
narrative_ontology:cs_drift_state('c6f4bb00-65f4-4ecb-883b-b581dc856abe', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6f4bb00-65f4-4ecb-883b-b581dc856abe', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, content_decayed_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, context_mismatched_observers).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, encoded_competence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities displaced by catastrophe, expulsion, or collapse carry their survival repertoire in ritual form: festival calendars that encode planting and rationing seasons, purity and food-handling protocols that encode sanitation knowledge, kinship obligations that encode mutual-aid networks. When archives, schools, and land tenure are destroyed, the ritual cycle is often the only carrier that made the journey intact. They gain adaptive capacity from it, and they pay for it in observance time, conformity, and the difficulty of revising protocols whose original referents are gone.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Priests, rabbis, lineage heads, and trained officiants run the transmission: they schedule the cycle, judge correct performance, correct drift, and decide what may change. Their standing, livelihood, and lifetime of training are constituted by the system they administer; stepping outside it would forfeit both role and communal standing. The observance economy materially supports them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists, beneficiary).

% Successor generations — urbanized descendants, post-catastrophe rebuilders — perform the full ritual cycle faithfully while the practical referents have dropped out: nobody can say which crop the first-fruits rite timed, which water source the purification step avoided, which rationing rule the fast commemorates. They bear the full observance cost and receive ceremony. Leaving the cycle would cost them communal membership and the scaffold that holds the community together, so they continue.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, content_decayed_communities, payer,
    moderate, generational, constrained, regional).

% Households observing protocols calibrated to a vanished ecology: agricultural timing rules in apartment-dense cities, water-use rites in drought districts, feast-and-fast cycles colliding with shift-work schedules. The content is intact but no longer maps onto their circumstances; they absorb the friction of compliance without the adaptive payoff the protocols were built to deliver.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, context_mismatched_observers, payer,
    moderate, biographical, constrained, local).

% Younger members who ask what each practice is for and are answered with citations of antiquity rather than demonstrations of function. They sit inside the observance but outside the interpretive conversation that decides what the ritual transmits; their proposal — keep the content, renew or drop the form — never reaches the agenda. Leaving costs them family and community; staying costs them the sense that the practice means anything.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generation_questioners, excluded,
    moderate, biographical, constrained, regional).

% Anthropologists and historians of religion who compare transmission outcomes across traditions: which encoded practices proved adaptive in documented crises, which decayed into performance, and what literacy and archival alternatives actually achieved. They take no side in observance and bear none of its costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, comparative_ritual_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual solves the problem of carrying distributed, embodied survival knowledge across generations and through disruptions that destroy documents, schools, and institutions: by binding timing rules, resource-management protocols, and kinship obligations to repeated communal performance on a fixed calendar, it keeps the repertoire available to people who cannot read, cannot archive, or survive institutional collapse.
% TRANSFER_FUNCTION: Moves time, labor, and conformity from observant households and communities to the ritual cycle and its officiants, in exchange for access to encoded practical knowledge whose value varies with context and with how much of the original referent survives.
% ABSENT_VOICES: Younger-generation questioners who want the content without the fossilized form, and formal educators who claim direct instruction transmits the same knowledge at lower cost, are both outside the interpretive conversation that decides what the ritual really transmits; the unanimity of the transmission arrangement partly reflects that these seats were never admitted.
% DISAPPEARANCE_RATIONALE: If ritual-as-transmission vanished overnight, diaspora communities would lose the one carrier that survives institutional destruction: communal calendars, food-handling norms, and mutual-aid cycles keyed to the ritual year would unravel within a generation or two wherever literacy and archives are also disrupted. Where stable literate institutions exist, replacement channels would eventually reconstitute much of the content — the rearrangement is uneven, and how uneven is exactly what the sibling readings contest.
% FOUNDING_PROBLEM: Recurrent catastrophe — flood, famine, exile, persecution — repeatedly destroyed the institutions that carried practical knowledge: archives burned, schools closed, land tenure collapsed, populations scattered. Communities needed a knowledge carrier robust to destruction, illiteracy, and dispersal.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historiography and comparative ethnography attest the founding problem from outside any single beneficiary community: documented flood-priest calendars, famine-food protocols preserved in liturgical cycles, and post-exile reconstructions keyed to ritual law. No external source attests the current content-fidelity of any particular community's transmission — that remains uncorroborated and is carried as an omega.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end): a real payload is delivered to the part of the network whose content is live, while full observance costs are drawn from every observant seat including those past content decay. Suppression (0.55) is structural rather than violent — sanction, membership stakes, and the subordination of documentary and schooled alternatives to the ritual channel; unilateral revision of protocol is punished, so alternatives persist but only partly collapse (accessibility_collapse 0.35). Theater (0.40) reflects the growing share of activity that is faithful performance of forms whose referents are gone. Resistance (0.50) is continuous: reform movements, secularization, and youth disaffection press on the system without displacing it. The three measurement series run on one shared grid (t=0..24, step 4). Extractiveness and theater rise together as content decay accumulates across the network. Suppression_requirement is included because the story specifically traces enforcement-capacity change: as referents drop out, enforcement must substitute for comprehension, so the machinery that holds correct performance hardens over the interval — a rising trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The cost-bearing seats and the administrator seat should compute very different types from identical structural data. From content_decayed_communities and context_mismatched_observers, the arrangement operates as full-price obligation with no delivered goods — near the full-target end. From diaspora_communities with live content, the same arrangement operates as a subsidized lifeline — near the beneficiary end. From ritual_specialists, it is vocation and livelihood — the system they administer is the system that constitutes them. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (diaspora_communities, ritual_specialists) drive low directionality for those seats; victim declarations (content_decayed_communities, context_mismatched_observers) combined with constrained exit drive high directionality — trapped-in-place payers sit nearer the full-target end than mobile ones would. No directionality overrides are needed: the derivation from declarations plus exit options captures every seat's relationship. Suppression enters the computation as a raw structural property, unscaled; only extractiveness is scaled, by directionality and by spatial scope — the global scope of diaspora networks makes verification of content-fidelity harder, which amplifies effective extraction modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite mislabels. Reading the whole arrangement as pure extraction erases the genuine transmission function that demonstrably carried communities through catastrophe — the coordination half is real and load-bearing. Reading it as pure coordination launders the form-only residue in which observance costs persist after the payload is gone — the extraction half is real and asymmetric. Mandatrophy here is per-community rather than global: where content is live the founding mandate is live; where form persists without content the mandate is locally dead while the arrangement persists. The temporal theater series tracks the aggregate drift toward the dead-mandate condition. If content fidelity collapsed network-wide, the expected recomputation is toward the inertial-form type — flagged in advance by the encoded_content_fidelity omega rather than asserted now.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'This constraint is one reading of the catastrophe_memory_survival kernel; would the symbol_survival or hybrid_encoding readings assign a different victim set, beneficiary set, or epsilon to the same ritual arrangements?',
    'Author the sibling stories over identical stakeholder situations and compare computed classifications; structural divergence between the readings locates the disagreement precisely.',
    'Under symbol_survival_reading the victim set shifts to communities whose practice-continuity breaks (regardless of content) and the competence payload leaves the accounting entirely; under hybrid_encoding_reading epsilon splits between registers and neither register''s cost-bearers stand alone. Classification of the same communities flips with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'Committer structure: which reading of the kernel partitions extraction and victimhood correctly.').

omega_variable(
    encoded_content_fidelity,
    'What fraction of observant communities currently retain retrievable practical content behind the forms they perform?',
    'Ethnographic audit asking practitioners to state the referent and procedure behind each protocol step, cross-checked against historical reconstructions of the original practice.',
    'High fidelity lowers effective extraction (cost-bearers receive the payload) and stabilizes the hybrid coordination reading; low fidelity means the theater measure understates decay and the network drifts toward inertial form-maintenance — a recomputation trigger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encoded_content_fidelity, empirical, 'Whether the encoded payload is still retrievable, or only the form persists.').

omega_variable(
    robustness_premium_question,
    'How much of the observance burden is the irreducible price of a carrier that survives catastrophe, versus overhead that a documentary or schooled channel would not impose?',
    'Historical counterfactual analysis: compare knowledge-survival outcomes across catastrophes for communities with ritual carriers versus contemporaneous communities relying on archives and formal schooling at comparable literacy rates.',
    'If documentary channels would have carried the same content through the same disruptions, the ritual premium is largely extractive overhead and epsilon rises; if not, a substantial share of the burden is the cost of robustness itself and epsilon falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(robustness_premium_question, empirical, 'Whether the observance burden is robustness cost or avoidable overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t4, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement_basis(cata_tr_t4, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(cata_tr_t12, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(cata_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t4, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(cata_be_t4, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(cata_be_t12, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(cata_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t4, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement_basis(cata_su_t4, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(cata_su_t12, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(cata_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, information_standard).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the catastrophe_memory_survival kernel per the epsilon-invariance principle: the colloquial label 'ritual preserves survival capacity' conflates three structurally distinct claims — practical-competence carriage (this file), symbolic identity continuity (symbol_survival_reading), and a dual-register synthesis (hybrid_encoding_reading). Each gets its own epsilon, beneficiary set, and victim set. This reading links to both siblings because they partition the same underlying arrangements and cite overlapping evidence; the upstream/downstream structure runs from competence-register findings into the hybrid synthesis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
