% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Ritual as Practical Survival Knowledge Transmission Vector
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint isolates the competence-transmission reading of ritual
 *   within the catastrophe_memory_survival kernel: ritual as a functioning
 *   technology for moving practical survival knowledge (timing, resource
 *   management, family crisis protocols) across generations, distinct from
 *   ritual's separately-modeled symbolic/identity function. Under this
 *   reading, extraction rises specifically where the practical content decays
 *   out of the transmitted form while the ceremonial shell persists —
 *   communities keep paying the social and material cost of ritual observance
 *   long after the competence it encoded has been lost, while the elders (or
 *   institutions) who still control what content gets transmitted, and to
 *   whom, occupy the agenda-setting seat. This is a distinct constraint from
 *   the symbol_survival_reading (where continuity of practice itself, not
 *   practical content, is what's being preserved and where losing the
 *   practice IS the loss) and from the hybrid_encoding_reading (which treats
 *   both registers as jointly load-bearing and does not isolate a single ε
 *   for either). Decomposed per the ε-invariance principle: measuring 'the
 *   ritual' by whether decodable practical content is present, transmitted,
 *   and applied yields a different, lower-to-moderate ε than measuring it by
 *   symbolic-continuity alone.
 *
 * KEY AGENTS:
 *   - ritual_elders_who_retain_practical_content: agenda_setter (moderate/constrained) — controls what practical knowledge is actually transmitted
 *   - diaspora_adaptive_households: beneficiary (moderate/mobile) — receives functioning competence and redeploys it
 *   - communities_facing_recurring_environmental_hazard: beneficiary (powerless/trapped) — depends on the ritual's accuracy for survival
 *   - urbanized_descendant_communities: payer (moderate/constrained) — bears the cost of form without the competence
 *   - younger_generations_receiving_hollowed_ritual_form: payer (powerless/trapped) — inherits obligation without knowledge
 *   - households_that_lost_the_practical_substrate: payer (powerless/trapped) — pays in unmanaged crisis when hazard recurs
 *   - religious_studies_researchers: observer (analytical/analytical) — documents which lineages still carry content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.47).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival Knowledge Transmission Vector").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'f8e842a6-8b2a-4baa-ba39-da59313c6640').
narrative_ontology:cs_kernel_codification('f8e842a6-8b2a-4baa-ba39-da59313c6640', implicit).
narrative_ontology:cs_authority_grounding('f8e842a6-8b2a-4baa-ba39-da59313c6640', practice).
narrative_ontology:cs_interpretation_layer_present('f8e842a6-8b2a-4baa-ba39-da59313c6640').
narrative_ontology:cs_reading_relation('f8e842a6-8b2a-4baa-ba39-da59313c6640', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8e842a6-8b2a-4baa-ba39-da59313c6640', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('f8e842a6-8b2a-4baa-ba39-da59313c6640', foundational, ritual_value_is_conditional_on_decodable_content).
narrative_ontology:cs_axiom_status(ritual_value_is_conditional_on_decodable_content, holdable).
narrative_ontology:cs_axiom_grounding('f8e842a6-8b2a-4baa-ba39-da59313c6640', ritual_value_is_conditional_on_decodable_content, empirically_contingent).
narrative_ontology:cs_axiom('f8e842a6-8b2a-4baa-ba39-da59313c6640', secondary, form_without_content_is_a_cost_not_a_good).
narrative_ontology:cs_axiom_status(form_without_content_is_a_cost_not_a_good, holdable).
narrative_ontology:cs_axiom_grounding('f8e842a6-8b2a-4baa-ba39-da59313c6640', form_without_content_is_a_cost_not_a_good, instrumental).
narrative_ontology:cs_reference_frame('f8e842a6-8b2a-4baa-ba39-da59313c6640', intact_elder_mediated_transmission).
narrative_ontology:cs_drift_state('f8e842a6-8b2a-4baa-ba39-da59313c6640', post_urbanization_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8e842a6-8b2a-4baa-ba39-da59313c6640', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_adaptive_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_who_retain_practical_content).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, communities_facing_recurring_environmental_hazard).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, urbanized_descendant_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generations_receiving_hollowed_ritual_form).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, households_that_lost_the_practical_substrate).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, ritual_as_functional_knowledge_technology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the working knowledge of timing (planting, storm, migration windows), resource rationing protocols, and family survival roles encoded inside ritual sequences. They decide how much of the practical substrate to transmit, to whom, and in what form. Their authority within the community rests on being the interpreters of when ritual timing maps onto real environmental risk.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_who_retain_practical_content, agenda_setter,
    moderate, generational, constrained, regional).

% Have relocated but retained enough of the ritual's practical content (seasonal timing logic, resource-pooling protocols, contingency roles within extended family) to re-apply it under new environmental and economic conditions. They receive functioning knowledge, not empty form, and use it to adapt faster than households that only inherited the ceremonial shell.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_adaptive_households, beneficiary,
    moderate, biographical, mobile, national).

% Live in the hazard zone the ritual's timing knowledge was built to manage (flood cycles, drought windows, storm seasons). Where the practical content survives inside the ritual, they benefit directly from accurate, low-cost, culturally embedded early-warning and resource-management logic that no external institution currently replaces for them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_facing_recurring_environmental_hazard, beneficiary,
    powerless, generational, trapped, regional).

% Perform the ritual's ceremonial form on schedule but have lost or never received the embedded practical instructions, because urban life removed the environmental cues the timing logic depended on. They pay the cost of maintaining the form (time, money, social obligation) while receiving none of the adaptive competence it was built to carry, and often do not know content is missing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, urbanized_descendant_communities, payer,
    moderate, biographical, constrained, national).

% Are taught the ritual as obligation and correct performance rather than as a transmitted survival technology, because the elders who could explain the practical rationale have died, migrated, or been displaced from the environment the knowledge referenced. They inherit the liability of upkeep without the asset of competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generations_receiving_hollowed_ritual_form, payer,
    powerless, biographical, trapped, local).

% Experienced a rupture in transmission (war, forced resettlement, elder death without an heir apprentice) and now hold a ritual that references resource and timing knowledge no one present can decode. When the hazard the ritual once managed recurs, they have the ceremony but not the competence, and pay the real cost of that gap in lost harvests, mistimed migrations, or unmanaged crises.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, households_that_lost_the_practical_substrate, payer,
    powerless, generational, trapped, regional).

% Document which communities' rituals still carry decodable practical content versus which have become purely symbolic, and study the transmission failures. Their classifications shape which reading of the ritual (competence-carrier vs. symbol-carrier) gets institutional and funding attention.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, religious_studies_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual sequences package perishable environmental and resource-management knowledge (when to plant, ration, migrate, or shelter; who does what in a household crisis) into a memorable, repeatable, intergenerationally transmissible form that survives the loss of written records, institutional continuity, or stable settlement.
% TRANSFER_FUNCTION: Moves accumulated practical competence from the generation that possessed direct environmental experience to descendants who may never face the original environment directly — a transfer of adaptive capacity, not merely of belief or belonging.
% ABSENT_VOICES: Households whose transmission chain broke are not consulted when the ritual's ceremonial form is preserved and celebrated by institutions (heritage bodies, diaspora associations) as if intact; the researchers documenting hollowed-out rituals are outside voices, but the households living the gap rarely appear in the record themselves.
% DISAPPEARANCE_RATIONALE: For communities still facing the original hazard with intact elder-to-household transmission, losing the ritual would mean losing a genuinely functioning low-cost warning and resource-coordination system — the world rearranges. For urbanized descendant communities where only the ceremonial shell survives, the ritual's disappearance would change little practically, since the competence it once carried is already gone; the form's loss there is symbolic, not functional. Which world you get depends on whether the specific lineage in question still carries content.
% FOUNDING_PROBLEM: Communities facing recurring, high-stakes environmental hazards (flood, drought, storm, famine cycles) needed a durable, low-literacy-dependent, socially enforced mechanism to transmit exact timing and resource-management protocols across generations without relying on unbroken institutional record-keeping.
% FOUNDING_PROBLEM_CORROBORATION: Elders in hazard-exposed regions attest the timing knowledge inside the ritual remains operationally load-bearing — they can point to specific ritual cues that map onto measurable seasonal or resource events. Independent ethnographers and disaster-risk researchers studying traditional ecological knowledge corroborate this for some lineages but document, for others, that the same ritual now runs on inherited habit with no one able to explain what the sequence was originally tracking — an assessment made by researchers outside the transmitting communities themselves, not by the communities' own claims of continuity.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.47 at interval end) and rising, tracking a specific decay dynamic: as environmental displacement, urbanization, and elder mortality sever the link between ritual cue and referent, the ceremonial form increasingly persists as unearned social obligation on households that receive no functional benefit from it. Theater ratio rises in parallel (0.52) for the same reason — a rising share of ritual performance is now proxy-goal maintenance (correct execution) rather than the original function (competence transfer), which is the diagnostic Goodhart-drift signature the theater_ratio metric exists to catch. Suppression (0.38) is moderate rather than low or high: social obligation to perform the ritual correctly persists and is mildly enforced by family and community expectation, but it is not backed by strong coercive machinery — most of the cost is borne through habit and unexamined obligation rather than active punishment. Accessibility collapse (0.42) and resistance (0.44) are mid-range, reflecting that alternative sources of practical knowledge (extension services, written almanacs, formal disaster planning) exist and are increasingly used where the ritual's content has hollowed out, and that some communities actively resist continuing empty-form obligations once they recognize the content is gone.
 *
 * PERSPECTIVAL GAP:
 *   The elder agenda-setter seat and the hazard-exposed beneficiary community experience genuine, functioning coordination — the ritual solves a real, high-stakes information problem cheaply and durably. The urbanized descendant and hollowed-transmission seats experience the identical ceremonial structure as pure cost: obligation without payoff. This is not a difference in belief; it is a difference in whether the transmission chain to that specific lineage stayed intact. The engine should compute divergent seat types from this data without either seat being 'wrong.'
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status derives from whether decodable practical content actually reaches and is usable by the receiving household: diaspora households and hazard-exposed communities that retain working knowledge sit near the beneficiary end. Victim status derives from bearing the transmission and performance cost of the ritual's form while the practical payload has decayed or was never received — urbanized descendants and post-rupture households sit near the target end despite performing the identical ceremony. Elders occupy the agenda-setting seat because they control transmission depth and completeness, not because they extract a concentrated rent; their moderate power and constrained exit reflect that their authority is itself contingent on the environment persisting long enough to validate the knowledge they hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting exact survival-relevant timing and resource protocols without institutional record-keeping) is genuinely live for hazard-exposed communities with intact transmission, and genuinely dead for urbanized descendants performing an emptied form — hence the contested founding_problem_status. Classifying this as tangled_rope rather than collapsing it into either pure rope (ignoring the hollowed-form victims) or pure snare (ignoring the communities where it still functions) prevents both mislabelings: the coordination function is real and documented by outside researchers, not merely self-asserted by beneficiaries, and the extraction on hollowed-transmission households is real and rising, not a rhetorical flourish.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_decay_detectability,
    'Can outside observers (or the community itself) reliably distinguish a ritual that still carries decodable practical content from one that has become purely symbolic, given that both are performed identically?',
    'Ethnographic and disaster-risk-literature cross-referencing: compare ritual timing cues against independently measured environmental/resource events for the specific lineage in question, and interview practitioners about whether they can explain the causal rationale behind ritual elements (not just the correct sequence).',
    'If content decay is largely undetectable from the outside (and even to many practitioners), the victim group in this reading is far larger than currently documented, since hollowed rituals would be systematically undercounted; if reliably detectable, the beneficiary/victim split authored here is closer to accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_decay_detectability, empirical, 'Whether hollowed-content rituals can be distinguished from content-intact rituals externally.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Is the practical-knowledge-transmission function of ritual truly separable from its symbolic/identity function, or does the hybrid_encoding_reading better describe the actual mechanism — with this decomposition into three readings itself being an analytical convenience rather than a structural fact about how ritual operates?',
    'Comparative case study across lineages: identify cases where practical content and symbolic function diverge in outcome (e.g., a ritual that fails practically but succeeds symbolically, or vice versa) to test whether the two registers are independently variable or jointly determined.',
    'If the registers are found to be inseparable in practice, this reading''s isolated ε measurement may overstate a distinction that does not cash out empirically, and the hybrid_encoding_reading would be the more structurally accurate single account rather than a third sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the competence-transmission and symbol-survival readings are genuinely structurally distinct or an analytical partition of one entangled mechanism.').

omega_variable(
    elder_authority_capture_risk,
    'Do elders who control transmission depth ever withhold full practical content to preserve their own social authority as interpreters, independent of whether the environment still requires that knowledge?',
    'Compare transmission completeness against elder social status incentives across multiple communities; look for cases where elders retain interpretive gatekeeping even after the practical knowledge has become locally irrelevant.',
    'If elder gatekeeping is partly self-interested rather than purely custodial, the agenda_setter seat carries a beneficiary component this story currently does not capture, which would push the classification toward a more concentrated extraction pattern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elder_authority_capture_risk, empirical, 'Whether elder control over transmission is purely custodial or partly self-interested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 80, 0.47).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.47).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__competence_transmission_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_memory_survival kernel (see kernel_context). symbol_survival_reading isolates ritual's identity/boundary-maintenance function and measures ε against continuity-of-practice as the good itself. hybrid_encoding_reading treats both registers as jointly necessary and does not isolate a single ε for either component. This story's ε (0.47, moderate, rising) reflects specifically the decay of practical content under the form; it should not be averaged or reconciled with the siblings' ε values — per the ε-invariance principle, they are three distinct constraints linked here for contamination-propagation analysis, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
