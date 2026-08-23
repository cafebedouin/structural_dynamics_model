% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N as Perpetual Negotiation Process
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   Wikipedia's notability guideline (WP:N) is commonly treated as a fixed
 *   inclusion threshold. The deliberative reading reconceives it as a
 *   perpetual negotiation process: notability boundaries are not inputs to
 *   Articles for Deletion (AfD) but outputs of AfD deliberation. Each AfD
 *   functions as a temporary scaffold — a structured deliberation with a
 *   declared sunset (the closure of discussion) that produces a contingent
 *   notability judgment for that topic at that time. The meta-process has no
 *   sunset, but each instantiation does. The constraint coordinates a global
 *   editor community around the collective action problem of encyclopedic
 *   scope without a central editorial board. Extraction takes the form of
 *   editorial labor invested in deliberation and the systematic disadvantage
 *   of knowledge forms that do not map to 'reliable sources' as
 *   conventionally defined.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.35).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.25).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N as Perpetual Negotiation Process").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'dadcf0d6-0a0b-437c-8edf-48dd9ba01b30').
narrative_ontology:cs_kernel_codification('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', distributed).
narrative_ontology:cs_authority_grounding('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', practice).
narrative_ontology:cs_interpretation_layer_present('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30').
narrative_ontology:cs_reading_relation('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', foundational, notability_is_process_output).
narrative_ontology:cs_axiom_status(notability_is_process_output, holdable).
narrative_ontology:cs_axiom_grounding('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', notability_is_process_output, conventional).
narrative_ontology:cs_axiom('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', foundational, afd_deliberation_legitimate).
narrative_ontology:cs_axiom_status(afd_deliberation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', afd_deliberation_legitimate, conventional).
narrative_ontology:cs_reference_frame('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', consensus_building_process).
narrative_ontology:cs_drift_state('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', contemporary_editorial_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dadcf0d6-0a0b-437c-8edf-48dd9ba01b30', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_readers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, active_editors).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, excluded_article_subjects).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, marginalized_knowledge_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and participate in AfD deliberations, apply notability guidelines, enforce closure. They bear the labor cost of deliberation but gain epistemic authority and community standing through participation. Exit is constrained — leaving means abandoning invested reputation and the primary platform for their editorial work.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, active_editors, agenda_setter,
    organized, biographical, constrained, global).

% Receive curated, relatively reliable encyclopedic content without participating in deliberation. They bear indirect costs when topics they value are deleted, but can switch to other information sources with low friction. Their benefit is the coordination output: a usable encyclopedia.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_readers, beneficiary,
    organized, immediate, mobile, global).

% Topics (individuals, organizations, events) that fail AfD and are deleted. They have no voice in the deliberation, no recourse to appeal, and no alternative platform with Wikipedia's visibility. The constraint extracts their potential encyclopedic representation without consent or compensation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, excluded_article_subjects, payer,
    powerless, immediate, trapped, global).

% Communities whose knowledge traditions (oral history, indigenous epistemologies, Global South scholarship, non-institutional expertise) do not produce 'reliable sources' as Wikipedia defines them. They can organize to create sources or advocate in AfD, but the structural barrier is high. Forking to alternative wikis loses Wikipedia's network effects and search dominance. Their epistemic identity is often bound to the knowledge being excluded (identity_locked dynamics).
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginalized_knowledge_communities, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, marginalized_knowledge_communities, excluded).

% Editors who regularly participate in Articles for Deletion discussions. They perform the deliberative labor that produces notability judgments. Their authority derives from sustained participation. Exit is constrained by reputation investment and the lack of alternative venues with equivalent deliberative throughput.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, afd_participants, agenda_setter,
    organized, biographical, constrained, global).

% Editors who argue for strict notability standards and frequent deletion. They benefit from the deliberative process as a mechanism to enforce quality thresholds. They are not direct payers or beneficiaries of extraction — they are ideological proponents of one reading of the kernel.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletionist_advocates, observer,
    organized, biographical, constrained, global).

% Editors who argue for broad inclusion and oppose deletion. They benefit from the deliberative process as a mechanism to defend marginal topics. Like deletionists, they are ideological proponents, not direct extraction targets.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, inclusionist_advocates, observer,
    organized, biographical, constrained, global).

% Hosts the platform, employs staff who support community processes, and bears legal/financial responsibility. The Foundation benefits from the coordination infrastructure that sustains Wikipedia's value proposition. It can modify platform affordances but cannot unilaterally override community consensus on notability. Exit is arbitrage-grade — it could restructure governance but would lose the volunteer editor base.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikimedia_foundation, agenda_setter,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective action problem of determining which topics warrant encyclopedic coverage by structuring community deliberation through Articles for Deletion (AfD) rather than imposing fixed criteria or central editorial control.
% TRANSFER_FUNCTION: Moves editorial authority from fixed notability standards to emergent consensus built through AfD deliberation; transfers inclusion/exclusion outcomes from pre-defined rules to negotiated boundaries. The 'currency' is editorial labor and epistemic recognition — editors invest labor to argue for/against inclusion; topics gain or lose the recognition of an encyclopedic article.
% ABSENT_VOICES: Potential article subjects who never enter AfD because they lack awareness or access to Wikipedia's processes; knowledge holders from oral traditions and non-Western epistemologies not represented in the reliable source ecosystem; future readers who would benefit from articles that never get created because their topics cannot meet the source requirement; Wikipedias in smaller languages that inherit English Wikipedia's notability framework without equivalent source ecosystems.
% DISAPPEARANCE_RATIONALE: If AfD deliberation vanished overnight, notability would revert to either rigid bureaucratic criteria (deletionist outcome: mass deletions by fixed rules) or uncontrolled inclusion (inclusionist outcome: vanity pages, vandalism, loss of reliability). The negotiated boundary itself would collapse, reorganizing Wikipedia's epistemic boundaries and its credibility as a reference work. The coordination function would need replacement — either algorithmic, bureaucratic, or market-based.
% FOUNDING_PROBLEM: Early Wikipedia (2001-2005) faced uncontrolled growth with no shared standard for article inclusion, leading to vandalism, vanity pages, promotional content, and unreliable articles. The community needed a mechanism to collectively judge notability without a central editorial board, using the only resource they had: volunteer editorial labor structured through discussion.
% FOUNDING_PROBLEM_CORROBORATION: Wikimedia Foundation annual reports (2015-present) document ongoing scope management challenges as article count grows. Academic studies of Wikipedia governance (Halfaker et al. 2013 on newcomer retention; Forte & Bruckman 2008 on deletion processes; Geiger & Halfaker 2013 on AfD as socialization) attest the coordination problem remains live. The deletionist and inclusionist readings themselves corroborate the problem's persistence by offering competing solutions — if the problem were solved, the contest would not persist.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because the primary cost is editorial labor in deliberation, not resource transfer to a concentrated beneficiary. Suppression is low (0.25) because alternatives exist (other wikis, personal sites, other platforms) and exit is possible, though costly for topics dependent on Wikipedia's visibility. Theater ratio is low (0.20) — AfD deliberations are genuine coordination events, not performative rituals, though procedural formalism increases over time. Accessibility collapse is moderate (0.40) — the 'reliable sources' criterion creates a structural barrier for non-institutional knowledge, but the deliberative process itself remains open to new arguments. Resistance is moderate (0.45) — constant meta-debate about notability (deletionist vs inclusionist) indicates the constraint is actively contested, not naturalized.
 *
 * PERSPECTIVAL GAP:
 *   From the active editor seat, the constraint is a genuine rope/scaffold — a coordination mechanism they built and maintain. From the marginalized knowledge community seat, the same structure operates as a snare — the 'reliable sources' requirement extracts their epistemic contributions while excluding their knowledge forms. The engine computes this divergence from the structural data: same constraint, different power/exit/scope positions yield different effective extraction. The deliberative reading's claim (scaffold) reflects the editor seat; the inclusionist reading's claim (snare) reflects the marginalized community seat. Both are structurally true from their respective seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Active editors (agenda_setters) have low directionality (d ~0.2) — they administer the process and benefit from its coordination function. Wikipedia readers (beneficiaries) have very low d (~0.1) — they receive curated content without bearing deliberation costs. Excluded article subjects (payers) have high d (~0.8) — they bear the cost of exclusion with trapped exit (cannot easily create an alternative Wikipedia article). Marginalized knowledge communities (payers) have high d (~0.75) — constrained exit (can fork but lose Wikipedia's network effects) and identity_locked dynamics (their epistemic identity is bound to the knowledge being excluded). Wikimedia Foundation (agenda_setter) has very low d (~0.05) — institutional beneficiary of the coordination infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing encyclopedic scope at scale without central editorial control) remains live — Wikipedia's growth has not eliminated the coordination problem. However, the original justification (preventing vandalism/vanity pages) has been partially superseded by automated tools and established editor culture. The scaffold persists because the coordination problem evolves (new topic areas, new epistemic communities) rather than because the original problem is solved. Mandatrophy is not resolved — the constraint continues to serve its coordination function, though its extraction profile has shifted toward epistemic communities excluded by the 'reliable sources' criterion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the deliberative reading of WP:N structurally relate to the deletionist and inclusionist readings of the same kernel?',
    'Map the structural deltas: deletionist_reading treats notability as fixed epistemic filter (high extraction, low coordination); inclusionist_reading treats notability as exclusionary gatekeeping (high extraction, suppression of alternatives); deliberative_reading treats notability as emergent from AfD process (coordination function primary, extraction secondary). Resolution requires tracing how each reading''s beneficiary/victim structure and claimed_type diverge from the shared kernel.',
    'If the three readings compute to different constraint types from the same kernel, the kernel itself is a contested commitment structure requiring family-level analysis. If they compute to the same type, the contest is rhetorical not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment kernel decomposition: one kernel (WP:N), three readings with distinct structural profiles').

omega_variable(
    deliberative_inclusion_ambiguity,
    'Does the AfD deliberation process genuinely incorporate marginalized epistemic perspectives, or does it structurally reproduce exclusion through source reliability requirements?',
    'Longitudinal analysis of AfD outcomes for topics from oral traditions, Global South knowledge, and non-institutional expertise. Compare retention rates and argument patterns against mainstream institutional topics.',
    'If deliberation systematically excludes marginalized knowledge despite procedural openness, the scaffold''s coordination function is partial and extraction toward dominant epistemic communities is higher than measured. Would shift classification toward tangled_rope for affected communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_inclusion_ambiguity, empirical, 'Whether the deliberative mechanism is structurally inclusive or reproduces epistemic hierarchy').

omega_variable(
    perpetual_vs_transitional_tension,
    'Can a ''perpetual negotiation process'' satisfy the scaffold requirement of a sunset clause, or does perpetuity indicate a rope or tangled_rope?',
    'Examine whether individual AfD deliberations function as discrete scaffolds (each with sunset at closure) while the meta-process persists, or whether the constraint is the meta-process itself which lacks sunset. Analyze community discourse on whether notability policy is ''finished'' or permanently provisional.',
    'If the meta-process has no sunset and no transitional justification, scaffold classification fails; the constraint becomes rope (if extraction low) or tangled_rope (if extraction asymmetric). The has_sunset_clause declaration applies per-deliberation, not to the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_vs_transitional_tension, conceptual, 'Tension between perpetual process framing and scaffold''s transitional requirement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.23).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.24).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deliberative_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the notability_guidelines kernel. Three readings instantiate three constraints: deletionist_reading (low epsilon, claimed mountain/rope), deliberative_reading (moderate epsilon, claimed scaffold), inclusionist_reading (high epsilon, claimed snare). All three share the same kernel text but differ in structural relationship declarations: deletionist names no victims; deliberative names excluded subjects and marginalized communities as victims; inclusionist names marginalized communities as primary victims with systemic extraction. The deliberative reading influences both siblings by providing the procedural mechanism (AfD) that the deletionist reading relies on for legitimacy and the inclusionist reading targets as the site of exclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
