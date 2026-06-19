% ============================================================================
% CONSTRAINT STORY: institutional_barrier_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_barrier_structure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_barrier_structure
 *   human_readable: Academic Career Incentive Structure Enforcing Disciplinary Boundaries
 *   domain: epistemology/institutional
 *
 * SUMMARY:
 *   Academic career advancement depends on peer review, grant funding, and
 *   publication in high-impact journals, all controlled by disciplinary
 *   specialists who systematically penalize breadth. The structure is
 *   presented as quality control protecting methodological rigor;
 *   cross-domain researchers and independent analysts increasingly read it as
 *   boundary enforcement protecting positional authority. This story models
 *   the institutional validation reading of the knowledge legitimacy kernel:
 *   the constraint is the career incentive structure that enforces the
 *   institutional reading's authority by making alternative approaches to
 *   knowledge production professionally unviable.
 *
 * KEY AGENTS:
 *   - disciplinary_specialists: Primary beneficiaries (powerful/mobile) — control review processes, benefit from boundary maintenance
 *   - cross_domain_synthesizers: Primary victims (moderate/constrained) — systematically penalized for breadth
 *   - early_career_interdisciplinary_researchers: Secondary victims (powerless/identity_locked) — face impossible choice between intellectual agenda and career viability
 *   - established_journal_editors: Agenda setters (institutional/mobile) — enforce scope boundaries to protect journal prestige
 *   - patients_seeking_mechanistic_frameworks: Excluded voices (powerless/trapped) — need integrative knowledge but have no voice in legitimacy criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_barrier_structure, 0.68).
domain_priors:suppression_score(institutional_barrier_structure, 0.76).
domain_priors:theater_ratio(institutional_barrier_structure, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_barrier_structure, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_barrier_structure, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(institutional_barrier_structure, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_barrier_structure, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(institutional_barrier_structure, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_barrier_structure, snare).
narrative_ontology:human_readable(institutional_barrier_structure, "Academic Career Incentive Structure Enforcing Disciplinary Boundaries").
narrative_ontology:topic_domain(institutional_barrier_structure, "epistemology/institutional").

domain_priors:requires_active_enforcement(institutional_barrier_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(institutional_barrier_structure, 'a78a8333-8a66-4cff-bd6b-b411eb4ab1cc').
narrative_ontology:cs_kernel_codification('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', distributed).
narrative_ontology:cs_authority_grounding('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', expertise).
narrative_ontology:cs_interpretation_layer_present('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc').
narrative_ontology:cs_reading_relation('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', institutional_barrier_structure__knowledge_legitimacy_synthesis_hypothesis, forecloses).
narrative_ontology:cs_reading_relation('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', institutional_barrier_structure__knowledge_legitimacy_pragmatic_action, forecloses).
narrative_ontology:cs_axiom('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', foundational, institutional_validation_required).
narrative_ontology:cs_axiom_status(institutional_validation_required, holdable).
narrative_ontology:cs_axiom_grounding('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', institutional_validation_required, conventional).
narrative_ontology:cs_axiom('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', foundational, disciplinary_depth_over_breadth).
narrative_ontology:cs_axiom_status(disciplinary_depth_over_breadth, holdable).
narrative_ontology:cs_axiom_grounding('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', disciplinary_depth_over_breadth, conventional).
narrative_ontology:cs_axiom('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', secondary, peer_review_sufficient_quality_control).
narrative_ontology:cs_axiom_status(peer_review_sufficient_quality_control, holdable).
narrative_ontology:cs_axiom_grounding('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', peer_review_sufficient_quality_control, empirically_contingent).
narrative_ontology:cs_reference_frame('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', methodological_rigor_protection).
narrative_ontology:cs_drift_state('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', contemporary_specialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a78a8333-8a66-4cff-bd6b-b411eb4ab1cc', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_barrier_structure, disciplinary_specialists).
narrative_ontology:constraint_beneficiary(institutional_barrier_structure, established_journal_editors).
narrative_ontology:constraint_beneficiary(institutional_barrier_structure, single_domain_grant_reviewers).
narrative_ontology:constraint_victim(institutional_barrier_structure, cross_domain_synthesizers).
narrative_ontology:constraint_victim(institutional_barrier_structure, early_career_interdisciplinary_researchers).
narrative_ontology:constraint_victim(institutional_barrier_structure, mechanism_focused_investigators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established researchers with deep expertise in single domains. They control peer review, grant panels, and tenure committees within their disciplines. The boundary enforcement protects their positional authority: cross-domain work that synthesizes across their specialty threatens their gatekeeping role. They benefit from a system that values depth over breadth and treats disciplinary credentials as the primary legitimacy signal.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, disciplinary_specialists, beneficiary,
    powerful, biographical, mobile, global).

% Researchers attempting to connect insights across disciplinary boundaries. They face systematic career penalties: grant proposals rejected as 'not focused enough,' papers rejected for 'lacking disciplinary depth,' tenure cases weakened by 'scattered publication record.' Their work is evaluated by specialists in each component domain who lack incentive to value the synthesis. Exit options are constrained because alternative career paths outside academia often require the same narrow credentials the system enforces.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, cross_domain_synthesizers, payer,
    moderate, biographical, constrained, global).

% Junior researchers who entered academia to pursue cross-domain questions but discover the incentive structure penalizes exactly that approach. They face an impossible choice: abandon the intellectual agenda that motivated their career, or accept systematic disadvantage in funding, publication, and advancement. Many are identity-locked because their self-concept as researchers is fused with the interdisciplinary questions they care about, making exit psychologically unthinkable even as the structure extracts from them.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, early_career_interdisciplinary_researchers, payer,
    powerless, biographical, identity_locked, global).

% Researchers pursuing mechanistic explanations that require integrating evidence from multiple domains. They produce coherent frameworks connecting validated components but lack the single-domain experimental proof the institutional structure demands. Their synthesis work is treated as 'speculative' or 'preliminary' regardless of the quality of component evidence or mechanistic plausibility. They bear the cost of producing knowledge the system cannot validate through its existing review apparatus.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, mechanism_focused_investigators, payer,
    moderate, biographical, constrained, global).

% Control publication access in high-impact disciplinary journals. They enforce scope boundaries to maintain journal identity and impact factor, which depends on citations within the established disciplinary network. Cross-domain papers threaten this citation structure and are harder to review because they require expertise across multiple domains. They benefit from the current structure because it simplifies editorial decisions and protects journal prestige within existing disciplinary hierarchies.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, established_journal_editors, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(institutional_barrier_structure, established_journal_editors, beneficiary).

% Evaluate funding proposals within their area of expertise. They are selected for deep domain knowledge, which makes them structurally unable to evaluate cross-domain synthesis quality. They default to penalizing proposals that span boundaries because they cannot assess competence in the domains outside their expertise. The structure gives them authority to reject what they cannot evaluate, which they exercise to minimize perceived risk.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, single_domain_grant_reviewers, agenda_setter,
    institutional, biographical, mobile, national).

% Institutional funders who increasingly recognize the value of interdisciplinary research and create special programs to support it, but delegate evaluation to the same disciplinary review panels that enforce boundaries. They observe the structural mismatch between stated priorities and actual funding outcomes but lack direct control over reviewer selection and evaluation criteria.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, funding_agencies, observer,
    institutional, generational, analytical, national).

% Individuals with complex or poorly-understood conditions who would benefit from cross-domain mechanistic synthesis but are excluded from the knowledge production process entirely. The institutional structure optimizes for disciplinary rigor over mechanistic coherence, leaving them without the integrative frameworks they need. They have no voice in how knowledge legitimacy is defined or what research gets funded.
narrative_ontology:constraint_stakeholder(institutional_barrier_structure, patients_seeking_mechanistic_frameworks, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains quality control in knowledge production by requiring deep expertise and methodological rigor within established domains; protects against false positives and premature claims by enforcing high evidentiary standards.
% TRANSFER_FUNCTION: Moves career advancement, funding, publication access, and intellectual authority from researchers pursuing cross-domain synthesis to those maintaining disciplinary boundaries. The transfer is enforced through peer review, grant evaluation, and tenure decisions.
% ABSENT_VOICES: Patients and practitioners who need integrative mechanistic frameworks are structurally excluded from defining what counts as legitimate knowledge. Independent researchers outside academic institutions who produce cross-domain synthesis have no pathway to legitimacy within the system.
% DISAPPEARANCE_RATIONALE: If the boundary enforcement vanished overnight, funding would flow to mechanistic synthesis projects currently rejected as 'unfocused,' journals would publish cross-domain frameworks currently dismissed as 'speculative,' and early-career researchers would pursue integrative questions without career penalty. The knowledge production landscape would reorganize around mechanistic coherence rather than disciplinary purity.
% FOUNDING_PROBLEM: Early scientific institutions needed to establish methodological rigor and protect against individual bias, charlatanism, and premature claims in an era when knowledge production was unstructured and quality control was absent.
% FOUNDING_PROBLEM_CORROBORATION: Disciplinary specialists and journal editors attest the founding problem remains live, citing ongoing risks of false positives and methodological sloppiness. Cross-domain researchers, independent analysts of scientific productivity, and patients seeking integrative frameworks attest the founding problem has shifted: the primary risk is now false negatives and systematic exclusion of valid synthesis, not false positives. Historical analysis from science studies scholars documents the transition from quality-protection to boundary-maintenance as the dominant function.
narrative_ontology:disappearance_verdict(institutional_barrier_structure, world_rearranges).
narrative_ontology:founding_problem_status(institutional_barrier_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(institutional_barrier_structure, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(institutional_barrier_structure, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_barrier_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_barrier_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_barrier_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the career penalties for cross-domain work are decoupled from the actual quality or utility of the synthesis produced — the structure extracts career advancement from researchers pursuing breadth regardless of whether their work is rigorous. Suppression is high (0.76) because exit options are constrained: alternative career paths require the same narrow credentials, and early-career researchers are often identity-locked to their intellectual agendas. Theater ratio is moderate (0.42): quality control is a real function, but a growing share of boundary enforcement defends disciplinary authority rather than methodological rigor. The measurement series shows extraction and suppression intensifying over the 40-year interval as disciplinary specialization deepened and grant competition increased, while theater ratio rises as the gap between stated support for interdisciplinary work and actual funding outcomes widens.
 *
 * PERSPECTIVAL GAP:
 *   From the disciplinary specialist seat, the constraint operates as necessary quality control protecting against premature synthesis and methodological sloppiness — a coordination function maintaining scientific rigor. From the cross-domain synthesizer seat, the same structure operates as enforced extraction: career advancement is systematically transferred from those pursuing mechanistic integration to those maintaining disciplinary boundaries, regardless of synthesis quality. The engine computes this divergence from the structural data; the claimed type (snare) reflects the victim perspective while acknowledging the coordination story is the cover under which extraction operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Disciplinary specialists are structural beneficiaries: they control the review apparatus, their positional authority is protected by boundary enforcement, and they face no penalty for rejecting cross-domain work they cannot evaluate. Cross-domain synthesizers and early-career interdisciplinary researchers are targets: they bear systematic career costs for pursuing breadth, their work is evaluated by those with no incentive to value synthesis, and their exit options are constrained by the same credential requirements the structure enforces. Journal editors and grant reviewers are agenda setters who also benefit: the structure simplifies their decisions and protects their institutional prestige within existing hierarchies.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was protecting knowledge quality in an era of unstructured science. That function has not disappeared, but it has been substantially captured by boundary maintenance: the structure now extracts from cross-domain researchers not because their work lacks rigor, but because it threatens the positional authority of disciplinary gatekeepers. The founding problem (quality control) is contested: specialists claim it remains live, but the systematic exclusion of rigorous synthesis suggests the structure now optimizes for disciplinary purity over mechanistic coherence. This is mandatrophy in progress: the coordination function persists but is increasingly subordinate to the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_quality_vs_boundary_enforcement,
    'Does the career penalty for cross-domain work track the actual quality and rigor of the synthesis, or does it track boundary-crossing itself regardless of quality?',
    'Controlled comparison of funding success rates and publication outcomes for high-quality single-domain vs high-quality cross-domain proposals, holding methodological rigor constant. If boundary-crossing predicts rejection independent of quality metrics, the structure is enforcing boundaries rather than protecting quality.',
    'If penalties track boundary-crossing rather than quality, the extraction is pure positional rent and the coordination story is cover. If penalties track quality, the structure is functioning as claimed and the measured extraction reflects the genuine cost of maintaining rigor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_quality_vs_boundary_enforcement, empirical, 'Whether career penalties track synthesis quality or boundary-crossing itself.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-lock for early-career interdisciplinary researchers structural (no alternative career paths exist) or internalized (they cannot imagine exit even when paths exist)?',
    'Post-exit trajectory analysis: if researchers who leave academia for industry or independent work report the identity-lock persisted after structural barriers were removed, the suppression is partially internalized. If exit immediately resolves the constraint, it was purely structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit. This would support higher suppression scores and strengthen the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural or internalized.').

omega_variable(
    coordination_extraction_separability,
    'Is quality control structurally inseparable from disciplinary boundary enforcement, or can rigor be maintained while allowing cross-domain synthesis?',
    'Natural experiment from institutions or funding programs that explicitly reward cross-domain work: if quality outcomes hold while boundary enforcement relaxes, the functions are separable. If quality degrades when boundaries relax, they are coupled.',
    'If separable, the boundary enforcement is pure extraction riding on a real coordination function, and the structure should be classified as tangled rope or snare depending on whether coordination or extraction dominates. If inseparable, part of the measured extraction is the necessary cost of quality control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether quality control and boundary enforcement are structurally separable.').

omega_variable(
    false_negative_vs_false_positive_tradeoff,
    'Has the optimal balance between false positive risk (accepting bad synthesis) and false negative risk (rejecting good synthesis) shifted as scientific knowledge matured, or does the current structure still reflect the appropriate tradeoff?',
    'Historical analysis of major scientific advances: what fraction originated from cross-domain synthesis that would have been rejected under current review standards? Economic analysis of opportunity cost: what is the social cost of systematically excluding valid mechanistic frameworks?',
    'If the tradeoff has shifted and the structure has not adapted, the founding problem is genuinely obsolete and the constraint is operating as mandatrophy. If the tradeoff remains appropriate, the structure is functioning as designed and the measured extraction reflects necessary quality control costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_negative_vs_false_positive_tradeoff, preference, 'Whether the false positive/false negative tradeoff the structure embodies remains appropriate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_barrier_structure, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_barrier_structure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inst_tr_t8, institutional_barrier_structure, theater_ratio, 8, 0.29).
narrative_ontology:measurement(inst_tr_t16, institutional_barrier_structure, theater_ratio, 16, 0.33).
narrative_ontology:measurement(inst_tr_t24, institutional_barrier_structure, theater_ratio, 24, 0.37).
narrative_ontology:measurement(inst_tr_t32, institutional_barrier_structure, theater_ratio, 32, 0.4).
narrative_ontology:measurement(inst_tr_t40, institutional_barrier_structure, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_barrier_structure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inst_be_t8, institutional_barrier_structure, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(inst_be_t16, institutional_barrier_structure, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(inst_be_t24, institutional_barrier_structure, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(inst_be_t32, institutional_barrier_structure, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(inst_be_t40, institutional_barrier_structure, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, institutional_barrier_structure, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(inst_su_t8, institutional_barrier_structure, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(inst_su_t16, institutional_barrier_structure, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(inst_su_t24, institutional_barrier_structure, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(inst_su_t32, institutional_barrier_structure, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(inst_su_t40, institutional_barrier_structure, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_barrier_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_barrier_structure, knowledge_legitimacy_biomedicine_institutional_reading).
narrative_ontology:affects_constraint(institutional_barrier_structure, peer_review_gatekeeping).
narrative_ontology:affects_constraint(institutional_barrier_structure, grant_funding_concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
