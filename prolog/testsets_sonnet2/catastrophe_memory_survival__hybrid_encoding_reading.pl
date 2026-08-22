% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe-Ritual as Dual-Register Survival Encoding (Hybrid Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the hybrid-encoding reading of the
 *   catastrophe_memory_survival kernel: the claim that the ritual complex
 *   which arises after a community-defining catastrophe cannot be cleanly
 *   sorted into 'symbolic identity practice' versus 'embedded practical
 *   knowledge' because it does both at once, through the same unseparated
 *   performance, and its survival function depends on the fusion rather than
 *   on either register alone. The sibling readings (symbol_survival_reading,
 *   competence_transmission_reading) each pick one register as the operative
 *   mechanism; this reading holds that the act of separating them is itself
 *   the distorting move, and that the distortion cost is born by analysts and
 *   institutions who must file the ritual under one heading, not by the
 *   communities who practice it. ε is authored low because the arrangement,
 *   read on its own hybrid terms, coordinates without extracting: no party
 *   inside the practicing community is structurally paying a rent to another
 *   for maintaining both registers together.
 *
 * KEY AGENTS:
 *   - communities_maintaining_ritual_practice: primary beneficiary — practices the fused ritual without needing to resolve which register is load-bearing
 *   - intergenerational_knowledge_holders: agenda-setting beneficiary — administers correct performance, standing depends on the fusion remaining unresolved
 *   - analysts_forcing_binary_classification: payer — bears the accuracy cost of a forced either/or classification the community itself does not impose
 *   - descendant_generations: beneficiary and payer — inherits the intact hybrid but is exposed if outside pressure strips one register
 *   - development_and_heritage_institutions: excluded — grant taxonomies have no slot for the hybrid claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe-Ritual as Dual-Register Survival Encoding (Hybrid Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '781cd461-66e2-4bf4-af08-c512badad664').
narrative_ontology:cs_kernel_codification('781cd461-66e2-4bf4-af08-c512badad664', implicit).
narrative_ontology:cs_authority_grounding('781cd461-66e2-4bf4-af08-c512badad664', practice).
narrative_ontology:cs_interpretation_layer_present('781cd461-66e2-4bf4-af08-c512badad664').
narrative_ontology:cs_reading_relation('781cd461-66e2-4bf4-af08-c512badad664', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('781cd461-66e2-4bf4-af08-c512badad664', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('781cd461-66e2-4bf4-af08-c512badad664', foundational, register_separability_denial).
narrative_ontology:cs_axiom_status(register_separability_denial, holdable).
narrative_ontology:cs_axiom_grounding('781cd461-66e2-4bf4-af08-c512badad664', register_separability_denial, empirically_contingent).
narrative_ontology:cs_axiom('781cd461-66e2-4bf4-af08-c512badad664', secondary, classification_cost_falls_on_classifier_not_practitioner).
narrative_ontology:cs_axiom_status(classification_cost_falls_on_classifier_not_practitioner, holdable).
narrative_ontology:cs_axiom_grounding('781cd461-66e2-4bf4-af08-c512badad664', classification_cost_falls_on_classifier_not_practitioner, empirically_contingent).
narrative_ontology:cs_reference_frame('781cd461-66e2-4bf4-af08-c512badad664', unseparated_dual_function_practice).
narrative_ontology:cs_drift_state('781cd461-66e2-4bf4-af08-c512badad664', contemporary_heritage_bureaucratization, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('781cd461-66e2-4bf4-af08-c512badad664', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_ritual_practice).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_holders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary_classification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, descendant_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform and transmit the ritual complex across generations after catastrophe (flood, famine, displacement, epidemic). They do not theorize the ritual as 'symbolic' or 'practical' — they simply do it, and doing it correctly happens to encode both boundary-marking (who we are, what separates us from the disaster's 'other') and embedded procedure (when to plant, how to store, whom to warn, how to move). Their benefit is continuity itself: the ritual works whether or not anyone can say which register is doing the work.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_ritual_practice, beneficiary,
    organized, generational, constrained, regional).

% Elders, ritual specialists, and family heads who administer the timing and correct performance of the rite. They set the practical agenda (when the rite happens, who participates, what variations are permitted) while also serving as the living authority on symbolic meaning. Their standing depends on the ritual's dual function remaining fused — separating out the 'competence' half would make them replaceable by a manual; separating out the 'symbol' half would make them replaceable by a priest with no practical stake.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_holders, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_holders, agenda_setter).

% Scholars, folklorists, development NGOs, and heritage bureaucracies who must classify the ritual for publication, funding, or preservation policy — as either 'symbolic/identity practice' or 'indigenous technical knowledge.' Forcing this choice costs them analytical accuracy: symbol-only readings miss the practical payload and risk having 'irrational superstition' stripped out by well-meaning development interventions; competence-only readings miss why communities resist substituting a technically superior but foreign procedure. They are not coerced, but their frameworks impose a cost on themselves that the community does not pay.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary_classification, payer,
    analytical, biographical, mobile, global).

% Children and grandchildns who inherit the ritual without being told which parts are 'meaningful' and which are 'useful' — they receive it as one thing. They benefit from the encoded survival knowledge surviving intact, and they pay a cost if outside pressure (missionary reform, state modernization, disaster-relief bureaucracy) strips the ritual to one register, because the register removed usually turns out to have carried information no one flagged as important until it was gone.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, descendant_generations, beneficiary,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, descendant_generations, payer).

% NGOs and heritage bodies that fund either 'cultural preservation' (symbol-register grants) or 'traditional ecological knowledge documentation' (competence-register grants) but rarely both from the same desk. Their absence from the hybrid reading is structural: their funding categories do not have a line item for a practice that is genuinely both, so a community's request to be understood that way often goes unheard.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, development_and_heritage_institutions, excluded,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves two coordination problems at once through a single performance: it marks who belongs to the group facing the catastrophe's aftermath (boundary maintenance) and it schedules/transmits the concrete adaptive behaviors (timing, resource husbandry, warning protocols) that let the group survive the catastrophe's recurrence. Neither function requires the other logically, but historically they co-evolved inside the same performative container.
% TRANSFER_FUNCTION: Nothing is extracted from one party and delivered to another in the ordinary sense; what moves is information and identity together, from the generation that survived the catastrophe to the generation that must survive its recurrence, carried inside a single unseparated performance.
% ABSENT_VOICES: Development and heritage institutions whose grant categories require the ritual to be filed as either 'culture' or 'knowledge' are structurally excluded from a framework that would let them fund or protect the hybrid as a hybrid; if present, they would object that program logic requires a single classifiable object.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared overnight, the community would lose both its distinctive marker of continuity after the catastrophe AND the embedded schedule of adaptive practices — timing of planting, storage, evacuation routes, mutual-aid obligations — that the ritual carries without anyone having written them down separately. Reconstructing either register alone from scratch (via ethnographic reconstruction or agronomic survey) would be slower and lossier than what the intact practice already encodes.
% FOUNDING_PROBLEM: A catastrophic event (flood, famine, epidemic, displacement) threatened both the group's continued existence as a distinct people and its members' physical survival of recurrence; the ritual arose to address both at once because the people who lived through it did not experience 'identity' and 'technique' as separable problems.
% FOUNDING_PROBLEM_CORROBORATION: Agricultural and disaster-risk researchers working independently of the communities (analyzing planting calendars, flood-warning timing, and mutual-aid triggers embedded in ritual calendars) corroborate that the practical-knowledge register remains functionally load-bearing today, not merely nostalgic; folklorists studying boundary-maintenance independently corroborate the symbolic register remains active. Neither corroborating body, on its own, attests to the hybrid claim — that requires reading both literatures together, which is precisely the gap this reading names.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, under this reading, the standing arrangement being assessed is the community's ongoing dual-register practice itself, and nothing internal to that practice transfers value from one intracommunity party to another — the elders who administer timing are not extracting from the descendants who inherit knowledge; they are both inside the same coordination structure. Suppression is low (0.15): nothing coerces community members into practicing both registers together — it survives because it works, not because alternatives are blocked. Theater ratio is modest (0.20) and rises slightly over the century-scale interval as some performative elements outlive their immediate practical referent (e.g., a warning gesture whose original hazard has receded but which is retained because it still marks belonging) — this is the expected slow drift of any long-lived practice, not a sign of capture. Resistance (0.35) reflects push-back the community mounts against classification pressure (missionary reform demanding the 'irrational' parts be dropped, development programs wanting the 'useful' parts extracted into a manual) rather than resistance to the ritual itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The practicing community and its knowledge-holders sit near the beneficiary end: the fused practice is what generates the ritual's value for them, and they bear no asymmetric cost for keeping the registers together. Analysts forcing binary classification are the payer group under this reading specifically because THEIR framework, not the community's, manufactures a cost — the cost of misclassification, of grant categories that don't fit, of publishing decisions that must pick a side. This is a genealogically unusual victim set (an analytical, mobile, institutionally powerful group bearing the cost) and is the central structural claim this reading makes: forcing the binary is what hurts, and it hurts the classifiers and the institutions that fund based on their classifications, not the community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (surviving both physical catastrophe and loss of group identity) remains live by the corroboration of two independent literatures (agronomic/disaster-timing research and folkloric/boundary-maintenance research), even though neither alone corroborates the specifically hybrid claim. This reading resists mandatrophy exactly by refusing the theoretical resolution the sibling readings offer: because it does not claim the ritual is 'really' about symbol OR 'really' about competence, it cannot be falsified by evidence that one register has weakened while the other persists — it predicts survival requires both, and predicts degradation of either register independently as the mechanism of loss, which is empirically checkable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_is_itself_a_third_classification,
    'Does declaring a ''hybrid'' reading merely relocate the classification problem rather than dissolve it — is ''both registers, unresolved'' itself a third theoretical box that some communities'' practices don''t fit either?',
    'Cross-case comparison: identify rituals where practitioners themselves insist on a strict single-register account (e.g., explicitly deny practical content, treat the rite as purely devotional) and test whether the hybrid frame misdescribes those cases by imposing dual-function analysis where none is locally claimed.',
    'If some communities'' own emic accounts are genuinely single-register, the hybrid reading would need to be scoped to a subset of catastrophe-rituals rather than treated as the general resolution of the kernel contest, and the sibling readings would retain unchallenged domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_is_itself_a_third_classification, conceptual, 'Whether ''hybrid'' is a resolution or a relocation of the classification problem.').

omega_variable(
    victim_class_severity_of_misclassification_cost,
    'How severe, in practice, is the cost borne by analysts and institutions who classify the ritual under a single register — is it a minor academic inconvenience, or does it drive real harm (e.g., development interventions that strip out load-bearing practical content because it was filed as ''mere symbolism'')?',
    'Case studies of development or heritage interventions that followed single-register classification, tracked for downstream loss of embedded practical knowledge (e.g., agricultural timing lost when a rite was ''modernized'' as purely ceremonial).',
    'If misclassification demonstrably causes material harm to descendant communities (not just analytical embarrassment to institutions), the true victim set extends beyond analysts to descendant_generations, which would raise this reading''s ε and shift it toward a tangled_rope framing where institutional extraction rides on a coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_severity_of_misclassification_cost, empirical, 'Whether classification-forcing costs stay confined to analysts or propagate to material harm for communities.').

omega_variable(
    sibling_reading_ranking,
    'Committer structure: this constraint is one of three readings of the catastrophe_memory_survival kernel — symbol_survival_reading, competence_transmission_reading, and this hybrid_encoding_reading. Is any one reading actually prior to (explanatorily grounding) the others, or are all three genuinely co-possible descriptions applied by different observers to the same practices?',
    'Longitudinal ethnography tracking whether communities whose practices lose the symbolic register first also lose the practical-knowledge register (supporting hybrid dependence) versus cases where one register persists robustly after the other has visibly eroded (supporting one of the single-register siblings).',
    'If the registers are shown to degrade independently in real cases, the hybrid reading''s core claim (that survival depends on both) weakens and the kernel contest resolves toward whichever single-register reading better predicts the independent case; if registers are shown to co-degrade, the hybrid reading is strengthened relative to both siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_ranking, conceptual, 'Whether the hybrid reading is empirically distinguishable from, or genealogically prior to, its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_survival kernel. symbol_survival_reading authors the ritual as primarily boundary/identity maintenance (survival = continuity of practice as marker); competence_transmission_reading authors it as primarily embedded practical-knowledge transmission (survival = successful adaptive-behavior encoding); this hybrid_encoding_reading holds that the coordination function requires both registers simultaneously and locates the victim class not among practitioners but among analysts and institutions forced to classify the practice under a single heading. Each reading carries its own ε, beneficiary/victim structure, and claimed_type; none is the 'correct' account, per the ε-invariance principle — they are three distinct constraints sharing a natural-language label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
