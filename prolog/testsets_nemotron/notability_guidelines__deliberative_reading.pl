% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N as Perpetual Negotiation Process via AfD Deliberation
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This constraint models Wikipedia's notability guideline (WP:N) as read
 *   through the deliberative frame: notability is not a fixed property of
 *   topics but an emergent outcome of the Articles for Deletion (AfD)
 *   process, where editors deliberate on whether a topic meets the
 *   community's evolving standards for inclusion. The constraint is a
 *   governance Scaffold — it was built to solve the unbounded-inclusion
 *   problem of early Wikipedia, carries a sunset logic (the process is meant
 *   to be revisable and self-correcting), and its justification is the
 *   transition toward a legitimate, adaptive boundary, not a steady-state
 *   rule. The deliberative reading coexists with deletionist and inclusionist
 *   readings as live constitutional positions within the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.35).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N as Perpetual Negotiation Process via AfD Deliberation").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '5b2efff1-f926-423b-8082-568a9ea41f8d').
narrative_ontology:cs_kernel_codification('5b2efff1-f926-423b-8082-568a9ea41f8d', formalized).
narrative_ontology:cs_authority_grounding('5b2efff1-f926-423b-8082-568a9ea41f8d', practice).
narrative_ontology:cs_interpretation_layer_present('5b2efff1-f926-423b-8082-568a9ea41f8d').
narrative_ontology:cs_reading_relation('5b2efff1-f926-423b-8082-568a9ea41f8d', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b2efff1-f926-423b-8082-568a9ea41f8d', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('5b2efff1-f926-423b-8082-568a9ea41f8d', foundational, notability_as_deliberative_outcome).
narrative_ontology:cs_axiom_status(notability_as_deliberative_outcome, holdable).
narrative_ontology:cs_axiom_grounding('5b2efff1-f926-423b-8082-568a9ea41f8d', notability_as_deliberative_outcome, conventional).
narrative_ontology:cs_axiom('5b2efff1-f926-423b-8082-568a9ea41f8d', secondary, process_legitimacy_requires_revisability).
narrative_ontology:cs_axiom_status(process_legitimacy_requires_revisability, holdable).
narrative_ontology:cs_axiom_grounding('5b2efff1-f926-423b-8082-568a9ea41f8d', process_legitimacy_requires_revisability, conventional).
narrative_ontology:cs_reference_frame('5b2efff1-f926-423b-8082-568a9ea41f8d', early_wikipedia_consensus_process).
narrative_ontology:cs_drift_state('5b2efff1-f926-423b-8082-568a9ea41f8d', contemporary_afd_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b2efff1-f926-423b-8082-568a9ea41f8d', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_editors).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikimedia_foundation).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, topic_area_experts).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, marginalized_subjects).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, emerging_topic_practitioners).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, non_anglophone_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, topic_area_experts).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, deliberative_governance_legitimacy).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, notability_as_process_not_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established editors who participate in AfD discussions, shape consensus through policy interpretation, and maintain the deliberative infrastructure. Their reputation capital is tied to the existing process; leaving would mean abandoning years of governance investment.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editors, agenda_setter,
    organized, biographical, constrained, global).

% The hosting institution that benefits from a self-governing community producing a globally trusted reference work. The Foundation does not directly adjudicate notability but provides the platform and legal shield; it could impose top-down rules but chooses to defer to community process.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikimedia_foundation, beneficiary,
    institutional, generational, arbitrage, global).

% Academic and professional experts who gain citations and visibility from Wikipedia coverage of their fields. They invest time in improving articles and defending notability in AfDs. They pay with labor but benefit from the quality signal the process provides.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, topic_area_experts, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, topic_area_experts, payer).

% Individuals, communities, and topics systematically underrepresented in reliable secondary sources (indigenous knowledge, women scientists, Global South movements, LGBTQ+ histories). They bear the cost of notability standards that privilege mainstream media coverage and institutional recognition. They cannot easily exit because Wikipedia is the de facto global reference; their exclusion is structural.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginalized_subjects, payer,
    powerless, biographical, trapped, global).

% Practitioners in new fields (e.g., AI alignment, crypto-economics, pandemic-response research) whose work lacks established secondary literature. They face deletion pressure in AfDs before the field's notability infrastructure matures. Their exit is constrained by the field's dependence on Wikipedia for legitimacy.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, emerging_topic_practitioners, payer,
    moderate, immediate, constrained, global).

% Editors and subjects from non-English language contexts whose reliable sources may not meet English Wikipedia's sourcing expectations. They pay a translation and mediation tax to satisfy notability requirements. Exit is constrained because English Wikipedia dominates global visibility.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, non_anglophone_contributors, payer,
    moderate, biographical, constrained, global).

% Editors who believe the deliberative reading is cover for inclusion creep and would prefer bright-line notability standards. They participate in AfDs but their preferred constraint (deletionist_reading) is not the governing frame; they are excluded from setting the agenda at the constitutional level.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletionist_advocates, excluded,
    organized, biographical, mobile, global).

% Editors who believe the deliberative process still systematically excludes and would prefer presumption of inclusion. Like deletionists, they operate within AfDs but their constitutional vision is not the governing frame.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, inclusionist_advocates, excluded,
    organized, biographical, mobile, global).

% Researchers in platform governance, digital commons, and constitutional theory who study Wikipedia's notability process as a case of self-governing knowledge infrastructure. They neither collect nor pay but their analysis shapes external legitimacy.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimate, transparent, and revisable process for the community to collectively determine what topics merit inclusion in a general-reference encyclopedia, without requiring a central authority to predefine notability. The AfD process converts contested boundary questions into reasoned deliberation with documented rationale.
% TRANSFER_FUNCTION: Moves editorial authority and inclusion outcomes from a hypothetical central gatekeeper to a distributed deliberative process. The cost is the labor of AfD participation (borne disproportionately by established editors); the benefit is a notability boundary that adapts to evolving knowledge landscapes and community values.
% ABSENT_VOICES: Subjects of notability decisions themselves (the people, communities, and movements being discussed) are structurally absent from AfD deliberations — they are the objects of debate, not participants. Also absent: future readers who would benefit from coverage that does not yet exist because the notability boundary has not yet expanded to include it.
% DISAPPEARANCE_RATIONALE: If the deliberative AfD process vanished overnight, notability would either collapse into admin fiat (centralized gatekeeping) or expand uncontrollably (inclusionist free-for-all). The Wikipedia community would lose its primary mechanism for legitimate boundary-setting, and the encyclopedia's epistemic credibility would be contested. A new constraint would need to be built — either top-down policy or a forked governance model.
% FOUNDING_PROBLEM: Early Wikipedia faced an unbounded inclusion problem: without a notability mechanism, the project could not distinguish encyclopedic topics from vanity, promotion, trivia, or original research. The community needed a way to say 'no' that was legitimate, revisable, and not dependent on a central authority's judgment.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Wikipedia's early policy archives (2003-2006) and by external scholars of peer production (Benkler, Tapscott, Halfaker et al.). Deletionist advocates corroborate that the problem remains live (unbounded inclusion degrades quality). Inclusionist advocates and marginalized knowledge scholars corroborate that the problem has mutated: the original 'trivia filter' now functions as a 'mainstream sources filter' that excludes knowledge from oral traditions, marginalized communities, and non-anglophone contexts.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28) because the process extracts labor from participants (especially established editors who do the deliberative work) and excludes marginalized subjects who lack the sourcing infrastructure to satisfy the process. But it also provides genuine coordination: a single, transparent process replaces thousands of ad-hoc judgments. Suppression is moderate (0.35) — AfDs are public, appealable, and the process itself can be reformed — but the structural reliance on reliable secondary sources creates a sourcing barrier that suppresses topics from oral traditions and marginalized communities. Theater ratio is elevated (0.42) because a growing share of AfD activity performs deliberation while outcomes correlate strongly with pre-existing sourcing advantage; the process increasingly legitimates outcomes that were structurally predetermined. Accessibility collapse (0.48) and resistance (0.52) reflect that alternatives (deletionist bright lines, inclusionist presumption) remain live and contested — the constraint does not fully collapse the option space.
 *
 * PERSPECTIVAL GAP:
 *   From the editor seat, the AfD process is genuine coordination — it converts contested boundaries into reasoned outcomes. From the marginalized subject seat, the same process is extraction — it converts structural sourcing disadvantages into encyclopedic erasure. The engine computes this divergence from the power/exit/beneficiary structure; the deliberative reading does not adjudicate which perception is 'true.'
 *
 * DIRECTIONALITY LOGIC:
 *   Wikipedia editors (agenda_setter) sit near the beneficiary end — they control the process and accumulate governance capital. Wikimedia Foundation (beneficiary) sits at the arbitrage end — it gains global legitimacy from the process without bearing its labor costs. Topic experts (beneficiary/payer) are near symmetric — they contribute labor and gain citations. Marginalized subjects (payer, trapped) sit at the full-target end — they bear exclusion costs with no exit. Emerging topic practitioners and non-anglophone contributors (payers, constrained) sit toward the target end but with some mobility as their fields or language communities build sourcing infrastructure. Deletionist and inclusionist advocates (excluded) are structurally excluded from constitutional agenda-setting despite being organized participants in AfDs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded as a Scaffold (transitional solution to unbounded inclusion). The founding problem is contested: deletionists say it remains live; inclusionists and marginalized knowledge scholars say it has mutated into a new exclusion mechanism. The mandate has partially atrophied — the process now defends a sourcing standard that systematically disadvantages knowledge from oral, marginalized, and non-anglophone traditions. But the Scaffold sunset clause (policy revisability) remains formally active: the community could rewrite WP:N tomorrow. The tension between formal revisability and structural inertia is the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sourcing_standard_legitimacy,
    'Is the reliable secondary source standard a neutral epistemic criterion or a structural mechanism that privileges knowledge forms aligned with mainstream institutional media?',
    'Comparative analysis of AfD outcomes for topics with equivalent epistemic significance but different sourcing infrastructures (e.g., indigenous oral histories vs. academic conference proceedings; Global South social movements vs. Western corporate histories).',
    'If the standard is structurally biased, the deliberative process''s coordination function is contaminated by extraction — the constraint would shift toward tangled_rope or snare for marginalized_subjects. If neutral, the exclusion is an epistemic necessity, not a structural choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sourcing_standard_legitimacy, conceptual, 'Whether the sourcing requirement is epistemically neutral or structurally exclusionary.').

omega_variable(
    deliberation_vs_outcome_correlation,
    'To what extent do AfD deliberations genuinely weigh evidence versus performing a ritual that ratifies pre-existing sourcing advantages?',
    'Discourse analysis of AfD arguments: measure the proportion of keep/delete votes that engage with the specific topic''s sources versus invoking generic policy shortcuts. Correlate with outcome predictability from sourcing metrics alone.',
    'High ritualization would increase theater_ratio and support reclassification toward piton (atrophied function maintained theatrically). Low ritualization would support the scaffold''s claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_vs_outcome_correlation, empirical, 'Whether AfD deliberation is substantive or performative.').

omega_variable(
    scaffold_sunset_credibility,
    'Is the deliberative process''s formal revisability a credible sunset mechanism, or has the constraint become a Piton where the community cannot actually reform the sourcing standard despite nominal policy mutability?',
    'Track the history of WP:N reform attempts: how many proposed changes to the sourcing standard reached community consensus? How many were blocked by structural inertia (policy shortcuts, established editor resistance, Foundation deference)?',
    'If reform is structurally blocked despite formal openness, the constraint is a piton (degraded scaffold). If reform occurs at a rate matching evolving knowledge landscapes, it remains a genuine scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_credibility, empirical, 'Whether the scaffold''s sunset clause is operationally credible.').

omega_variable(
    kernel_reading_relations,
    'Does the deliberative reading''s core premise (notability as process output) logically foreclose the deletionist premise (notability as pre-existing quality threshold) or the inclusionist premise (notability as exclusion mechanism), or do all three coexist as live constitutional positions?',
    'Constitutional analysis: can a single editor simultaneously hold the deliberative frame and the deletionist frame without contradiction? Can the community''s governing documents be read as committing to one frame exclusively?',
    'If deliberative forecloses deletionist, the kernel has a forecloses relation. If all three coexist, the kernel is distributed with coexist_with relations. This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between the three readings of the notability_guidelines kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by marginalized subjects structural (lack of reliable sources in the required format) or internalized (editors from marginalized communities self-censoring or leaving because the process signals their knowledge is not welcome)?',
    'Post-exit trajectory study: track editors from marginalized communities who leave Wikipedia — does their sense of exclusion persist in other knowledge platforms? Survey current marginalized editors on whether they experience the process as external barrier or internalized unworthiness.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural metrics suggest — the target carries the suppression after exit. This would increase the effective extraction for the payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for marginalized participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deliberative_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, wikipedia_governance_infrastructure).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, reliable_sources_guideline).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, biographies_of_living_persons_policy).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel decomposes into three constraint stories: deletionist_reading (Mountain or Tangled Rope claim — epistemic quality filter), deliberative_reading (Scaffold claim — this story), and inclusionist_reading (Snare claim — gatekeeping apparatus). The deliberative reading occupies the current governance frame and influences both siblings by setting the procedural terms of AfD. All three stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__deliberative_reading, organized, 0.15).
constraint_indexing:directionality_override(notability_guidelines__deliberative_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
