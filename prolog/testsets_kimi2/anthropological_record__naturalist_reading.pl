% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Materialist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint is the naturalist reading of the anthropological_record
 *   kernel. It holds that human origins (evolution, migration) are materially
 *   caused and knowable only through credentialed scientific method. The
 *   constraint operates through peer review, funding gatekeeping, curriculum
 *   control, and the methodological exclusion of supernatural causation.
 *   While it coordinates genuine empirical inquiry, it asymmetrically
 *   extracts epistemic authority from non-credentialed interpreters and
 *   indigenous knowledge holders, suppressing alternative epistemologies that
 *   cannot access credentialing infrastructure. Sibling readings include
 *   creationist_reading (divine creation compatible with scriptural timeline)
 *   and indigenous_epistemology_reading (relational continuity knowable via
 *   sustained oral tradition).
 *
 * KEY AGENTS:
 *   - credentialed_research_institutions: Primary agenda-setter (institutional/arbitrage) â controls publication, funding, and curriculum; benefits from authority concentration
 *   - indigenous_knowledge_holders: Primary target (powerless/identity_locked) â bears epistemic erasure and exclusion from the record
 *   - non_credentialed_interpreters: Secondary target (moderate/constrained) â suppressed regardless of evidential quality
 *   - religious_origin_interpreters: Secondary target (organized/constrained) â supernatural causation structurally excluded
 *   - epistemology_studies_scholars: Analytical observer (analytical/global) â documents credentialing effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.72).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.8).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Materialist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '47f5b957-50ea-476f-8085-bee6851a8c07').
narrative_ontology:cs_kernel_codification('47f5b957-50ea-476f-8085-bee6851a8c07', formalized).
narrative_ontology:cs_authority_grounding('47f5b957-50ea-476f-8085-bee6851a8c07', expertise).
narrative_ontology:cs_interpretation_layer_present('47f5b957-50ea-476f-8085-bee6851a8c07').
narrative_ontology:cs_reading_relation('47f5b957-50ea-476f-8085-bee6851a8c07', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('47f5b957-50ea-476f-8085-bee6851a8c07', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('47f5b957-50ea-476f-8085-bee6851a8c07', foundational, methodological_naturalism_binding).
narrative_ontology:cs_axiom_status(methodological_naturalism_binding, holdable).
narrative_ontology:cs_axiom_grounding('47f5b957-50ea-476f-8085-bee6851a8c07', methodological_naturalism_binding, conventional).
narrative_ontology:cs_axiom('47f5b957-50ea-476f-8085-bee6851a8c07', foundational, credentialing_legitimates_epistemic_authority).
narrative_ontology:cs_axiom_status(credentialing_legitimates_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('47f5b957-50ea-476f-8085-bee6851a8c07', credentialing_legitimates_epistemic_authority, conventional).
narrative_ontology:cs_reference_frame('47f5b957-50ea-476f-8085-bee6851a8c07', classical_empiricist_naturalism).
narrative_ontology:cs_drift_state('47f5b957-50ea-476f-8085-bee6851a8c07', contemporary_academy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47f5b957-50ea-476f-8085-bee6851a8c07', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_research_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_origin_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control peer review, research funding, tenure standards, and curricula for human origins. Define legitimate evidence as material and empirical. Exclude supernatural causation and non-peer-reviewed claims from the scientific record. Collect epistemic authority, funding, and institutional prestige.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Maintain oral traditions and relational knowledge about ancestral continuity and origins. Their epistemic frameworks are categorized as non-empirical and excluded from peer-reviewed publication and funding. Cannot exit their knowledge practice without breaking cultural identity.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    powerless, generational, identity_locked, regional).

% Conduct independent research, amateur archaeology, or alternative synthesis on human origins. Lack institutional credentials required for journal submission and grant eligibility. Their evidence is screened out regardless of quality by credentialing gatekeeping.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    moderate, biographical, constrained, national).

% Advance supernatural, theistic, or designed-causation accounts of human origins. Structurally excluded from public education, mainstream peer review, and research funding by methodological naturalism. Organized into churches and research institutes but barred from the dominant epistemic infrastructure.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religious_origin_interpreters, payer,
    organized, generational, constrained, global).

% Study the sociology and history of scientific knowledge production. Document how credentialing, peer review, and methodological naturalism function as gatekeeping mechanisms that concentrate authority and marginalize alternative epistemologies.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, epistemology_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, credentialed_research_institutions).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective knowledge production about human origins through shared empirical methods, reproducible standards, peer review, and falsifiability requirements, preventing idiosyncratic or unfalsifiable claims from dominating public understanding.
% TRANSFER_FUNCTION: Moves epistemic authority, research funding, and curriculum control from non-credentialed interpreters, indigenous knowledge holders, and religious scholars to credentialed scientific institutions and materialist research programs.
% ABSENT_VOICES: Indigenous knowledge holders whose oral traditions contain origin narratives; non-credentialed researchers with evidence challenging consensus timelines; creationist scholars advocating designed causation. They are absent from peer review panels, funding agencies, and curriculum design boards.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, indigenous and independent origin accounts would enter mainstream discourse, supernatural causation would become admissible in research programs, credentialing monopolies would fragment, and the authority of credentialed institutions would diffuse into competing epistemic frameworks.
% FOUNDING_PROBLEM: Human origins were subject to religious dogma, speculative philosophy, and local myth without shared empirical standards, producing incommensurable accounts and preventing cumulative, falsifiable knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Science historians attest that professionalization solved genuine coordination problems in the early twentieth century. Indigenous studies scholars and sociology-of-science researchers from outside the beneficiary set attest that the founding problem is substantially dead and the arrangement now functions as authority concentration and epistemic gatekeeping; no neutral party corroborates that the problem remains as severe as when the constraint was founded.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the credentialing gate concentrates authority and resources in credentialed institutions while excluding valid but non-credentialed contributions. Suppression (0.80) is high due to active enforcement through peer review, tenure standards, and curriculum design. Theater_ratio (0.45) reflects moderate performative boundary maintenance: much genuine science occurs, but a substantial share of activity defends methodological naturalism as an institutional boundary rather than as an open empirical stance. Accessibility_collapse (0.70) is high because once the naturalist frame is accepted, alternatives appear inherently illegitimate. Resistance (0.55) is moderate: creationist movements, indigenous rights advocacy, and open-science reformers mount significant but institutionally marginalized pushback. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination of empirical standards that prevent chaos in origins research. The payer seats experience it as epistemic violence, identity erasure, and structural exclusion. The engine computes this divergence from the structural asymmetry in power and exit options: credentialed institutions have arbitrage-grade exit, while indigenous knowledge holders are identity-locked.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed institutions are beneficiaries with arbitrage-grade exit and global scope, placing their directionality near the subsidy end. Indigenous knowledge holders are identity-locked targets with regional scope, placing their directionality near the full-target end. Non-credentialed and religious interpreters are constrained targets at national or global scope, receiving high effective extraction amplified by their limited exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a Rope because extraction is asymmetric and enforced; it is not a Snare because the coordination function (empirical knowledge production with shared, falsifiable standards) is genuine and not merely cover. The rising theater ratio and steadily increasing extractiveness over the interval indicate that coordination and extraction are structurally entangled â the Tangled Rope classification prevents collapsing this into either pure benignity or pure predation. Mandatrophy has not occurred; the founding problem is contested but the constraint's function has not fully atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_naturalism_ambiguity,
    'Is methodological naturalism a pragmatic research heuristic or an ontological commitment that constructively excludes non-materialist accounts?',
    'Historical and philosophical analysis of scientific practice when supernatural hypotheses have been entertained versus automatically excluded regardless of evidential potential.',
    'If pragmatic, the constraint could be reformed to reduce extraction without losing coordination; if ontological, the exclusion is constitutive and the constraint is more deeply extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_naturalism_ambiguity, conceptual, 'Ambiguity over whether naturalism is methodological or ontological').

omega_variable(
    credentialing_gatekeeping_legitimacy,
    'Does credentialing improve epistemic outcomes for human origins research, or does it concentrate authority while filtering out valid non-conforming evidence?',
    'Comparative study of research outcomes and error rates in credentialed versus independent or indigenous research programs on human origins.',
    'If credentialing is purely rent-seeking, the coordination story is cover and the constraint tends toward Snare; if it genuinely improves knowledge quality, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_gatekeeping_legitimacy, empirical, 'Whether credentialing improves knowledge or concentrates authority').

omega_variable(
    indigenous_suppression_mechanism,
    'Is the exclusion of indigenous knowledge structural (barriers to peer review, funding, and language) or internalized (indigenous communities stop seeking publication in dominant venues)?',
    'Post-exit trajectory analysis: do indigenous scholars who operate outside credentialed institutions retain and transmit their knowledge, or does the suppression persist in their own practice as self-censorship?',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates as deeper epistemic capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_suppression_mechanism, empirical, 'Structural vs internalized suppression of indigenous knowledge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthro_nat_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anthro_nat_tr_t10, anthropological_record__naturalist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(anthro_nat_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(anthro_nat_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(anthro_nat_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(anthro_nat_tr_t50, anthropological_record__naturalist_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(anthro_nat_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anthro_nat_be_t10, anthropological_record__naturalist_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(anthro_nat_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(anthro_nat_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(anthro_nat_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(anthro_nat_be_t50, anthropological_record__naturalist_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(anthro_nat_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anthro_nat_su_t10, anthropological_record__naturalist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(anthro_nat_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(anthro_nat_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(anthro_nat_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(anthro_nat_su_t50, anthropological_record__naturalist_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three structurally distinct constraints. Each reading instantiates a different constraint with a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
