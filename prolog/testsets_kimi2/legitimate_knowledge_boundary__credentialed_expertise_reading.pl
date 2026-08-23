% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Peer Review as Legitimacy Gate
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the credentialed_expertise_reading of the
 *   legitimate_knowledge_boundary kernel. It posits that legitimate knowledge
 *   is produced exclusively through methodologically rigorous inquiry
 *   validated by credentialed peer review. Structurally, this creates a
 *   centralized gatekeeping system in which journals, grant panels, and
 *   degree-granting institutions define what counts as valid knowledge,
 *   conferring epistemic monopoly on affiliated experts while delegitimizing
 *   non-institutional, indigenous, and experiential ways of knowing. The
 *   arrangement carries a genuine coordination functionâfraud filtration
 *   and methodological standardizationâbut operates with high asymmetric
 *   extraction: barriers to entry are steep, expert consensus substitutes for
 *   direct truth-tracking, and the machinery of validation has become a site
 *   of institutional rent capture.
 *
 * KEY AGENTS:
 *   - credentialled_experts: Primary beneficiary (organized/identity_locked) â receive epistemic monopoly and resource concentration
 *   - academic_gatekeepers: Agenda-setter and secondary beneficiary (institutional/constrained) â administer validation rules and capture institutional rents
 *   - policy_makers: Secondary beneficiary (powerful/mobile) â leverage expert consensus for legitimacy cover
 *   - non_institutional_inquirers: Primary payer (moderate/constrained) â excluded from legitimacy and funding by credential barriers
 *   - indigenous_knowledge_holders: Secondary payer (powerless/trapped) â knowledge systems delegitimized by methodological monoculture
 *   - science_and_technology_studies_scholars: Analytical observer (analytical/analytical) â document the constraint's sociology without direct stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.7).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Peer Review as Legitimacy Gate").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '4628b907-9c89-404a-82a5-5de6fddc396e').
narrative_ontology:cs_kernel_codification('4628b907-9c89-404a-82a5-5de6fddc396e', formalized).
narrative_ontology:cs_authority_grounding('4628b907-9c89-404a-82a5-5de6fddc396e', expertise).
narrative_ontology:cs_interpretation_layer_present('4628b907-9c89-404a-82a5-5de6fddc396e').
narrative_ontology:cs_reading_relation('4628b907-9c89-404a-82a5-5de6fddc396e', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4628b907-9c89-404a-82a5-5de6fddc396e', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('4628b907-9c89-404a-82a5-5de6fddc396e', foundational, methodological_rigor_as_necessary_condition).
narrative_ontology:cs_axiom_status(methodological_rigor_as_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('4628b907-9c89-404a-82a5-5de6fddc396e', methodological_rigor_as_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('4628b907-9c89-404a-82a5-5de6fddc396e', foundational, credential_as_epistemic_proxy).
narrative_ontology:cs_axiom_status(credential_as_epistemic_proxy, holdable).
narrative_ontology:cs_axiom_grounding('4628b907-9c89-404a-82a5-5de6fddc396e', credential_as_epistemic_proxy, conventional).
narrative_ontology:cs_reference_frame('4628b907-9c89-404a-82a5-5de6fddc396e', empiricist_peer_validation_norm).
narrative_ontology:cs_drift_state('4628b907-9c89-404a-82a5-5de6fddc396e', post_replication_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4628b907-9c89-404a-82a5-5de6fddc396e', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialled_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_gatekeepers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_institutional_inquirers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_monism).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold advanced degrees and institutional affiliations that grant standing in peer review systems. Their claims receive automatic epistemic priority; funding, publication, and policy influence flow through credentialed networks. Exit from the credential system means loss of authority and income, fusing professional identity with the constraint.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialled_experts, beneficiary,
    organized, biographical, identity_locked, global).

% Administer journals, grant panels, and tenure committees that define methodological rigor and select reviewers. They set the rules for what counts as valid knowledge and enforce them through editorial and funding decisions. They also capture substantial revenue, prestige, and career-control rents from dominating the validation pipeline.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_gatekeepers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_gatekeepers, beneficiary).

% Rely on credentialed expert consensus to justify regulatory and policy decisions. The constraint provides deference and legitimacy cover: 'following the science' displaces political accountability. They benefit from the concentrated, controllable nature of credentialed advice while remaining free to shop for favorable expertise.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers, beneficiary,
    powerful, biographical, mobile, national).

% Produce research, data, or analysis outside degree-granting institutions or without credentialled affiliation. Their work is systematically excluded from high-impact journals and mainstream grant streams. Some publish in grey literature or preprints, but these lack the legitimacy halo of peer review and are ignored in policy and media.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_institutional_inquirers, payer,
    moderate, biographical, constrained, global).

% Maintain complex knowledge systems validated through intergenerational practice, ecological observation, and community consent. These systems fail to register as methodologically rigorous under credentialled peer review because they do not use Western statistical or experimental formats. Their knowledge is extracted or ignored, and their communities are excluded from setting research agendas that affect them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, indigenous_knowledge_holders, payer,
    powerless, generational, trapped, global).

% Study the sociology of knowledge production and credentialing. They document how peer review reproduces inequality, how methodological standards encode cultural assumptions, and how replication crises undermine the reliability claims of the credential system. They do not collect from or pay into the constraint directly.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, science_and_technology_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_gatekeepers).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters empirical claims through standardized methods and intersubjective review to prevent fraud, error, and charlatanism in collective knowledge production at scale.
% TRANSFER_FUNCTION: Moves epistemic authority, funding access, and policy influence from non-credentialed inquirers and indigenous knowledge systems to credentialled experts and academic gatekeeping institutions.
% ABSENT_VOICES: Indigenous knowledge holders, independent researchers, and experiential experts are structurally absent from peer review panels, grant committees, and editorial boards; their exclusion from the credentialing pipeline means their objections to methodological monoculture never reach the validation stage.
% DISAPPEARANCE_RATIONALE: Without the credential boundary, funding flows, policy justification, and epistemic authority would redistribute toward plural validation mechanisms; the current concentration of knowledge-legitimacy in credentialed institutions would dissolve, and indigenous and independent inquiry would gain standing.
% FOUNDING_PROBLEM: Early modern and twentieth-century knowledge production faced unreplicable claims, ideological capture, and outright fraud; a centralized, intersubjective validation mechanism was needed to coordinate reliable empirical inquiry.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and STS scholars outside the beneficiary set attest that the founding problem was real but is now used as cover for institutional capture. Internal beneficiaries assert the problem remains acute. Independent meta-researchers note that replication crises persist inside the credentialed system itself, undermining the claim that credentialing solved the founding problem.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65 because the constraint monopolizes epistemic legitimacy and concentrates funding and publication access in credentialed networks, extracting authority from excluded knowledge producers. Suppression is 0.70 because non-credentialed and indigenous knowledge is actively delegitimized through formatting requirements, language barriers, and exclusion from review panels. Theater_ratio is 0.42: peer review retains genuine filtration value but an increasing share of enforcement activity is performativeâritualized review, impact-factor gaming, and legitimacy maintenance for gatekeeping institutions. Accessibility_collapse is 0.60 because, once inside the credential system, alternatives appear illegitimate by definition, though marginalized alternatives continue to exist outside. Resistance is 0.55 due to sustained open-science, decolonization, and indigenous-sovereignty movements. The measurement series share one time grid so the engine can track Goodhart drift and extraction accumulation without grid misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialled expert seat, the constraint is quality control and fraud preventionâa necessary filter without which public discourse would drown in misinformation. From the non-institutional and indigenous seats, the same structure reads as an epistemic enclosure: their knowledge is not rejected on empirical grounds but on formatting grounds (lack of statistical method, lack of credentialled author, lack of English-language framing). The engine computes this divergence from the same structural dataârole, exit options, and powerâwithout requiring reconciled consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialled experts and academic gatekeepers are structurally subsidized by the constraint: it amplifies their authority and narrows the field of competitors. Their directionality is near the beneficiary pole. Non-institutional inquirers and indigenous knowledge holders are structural targets: the constraint extracts epistemic authority from them by refusing to recognize their knowledge as legitimate unless reformatted through credentialing machinery. Policy makers sit closer to the beneficiary end because the constraint supplies them with deference and legitimacy cover, though they do not administer it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a genuine coordination problemâhow to filter fraud and standardize empirical methods in an era of expanding scientific production. That founding problem is contested: replication crises inside the credentialed system suggest the filter is imperfect, while the exclusion of valid non-credentialed knowledge suggests the filter is over-inclusive. The constraint has not resolved into a pure Snare because the coordination function remains partially operational; nor is it a pure Rope because the concentration of epistemic authority in credentialed institutions extracts more than the coordination cost justifies. It persists as Tangled Rope because the beneficiaries of the extraction (gatekeepers, credentialled experts) are also the administrators of the coordination function, making the two inseparable in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_as_epistemic_proxy_ambiguity,
    'Does the credential function as a genuine proxy for reliability, or as a boundary marker that excludes valid knowledge from non-credentialed sources regardless of its reliability?',
    'Large-scale outcome studies comparing credentialed vs. non-credentialed research reliability; citation and funding bias studies controlling for methodological quality.',
    'If credentials are weak proxies for reliability, the extraction is higher than the coordination function justifies, pushing classification toward snare. If strong proxies, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_as_epistemic_proxy_ambiguity, empirical, 'Whether credentials track reliability or enforce closure').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-credentialed knowledge structural (no journals accept it, no funding supports it) or internalized (non-credentialed inquirers believe their own knowledge is inherently inferior)?',
    'Surveys of independent researchers and indigenous communities on perceived legitimacy barriers; post-exit trajectory studies tracking whether suppressed knowledge production resumes after structural barriers are removed.',
    'If internalized, effective suppression exceeds structural measures and directionality for trapped agents is higher than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of non-credentialed knowledge').

omega_variable(
    kernel_reading_framing,
    'This constraint is the credentialed_expertise_reading of the legitimate_knowledge_boundary kernel. Would adopting the experiential_pluralism_reading or hybrid_coproduction_reading change the beneficiary/victim structure or the epsilon referent for the same institutional machinery?',
    'Comparative analysis of the sibling constraints once authored; examine whether the same peer-review institutions map to different epsilon values under different kernel readings.',
    'If the same machinery is a Mountain under one reading and a Snare under another, the epsilon-invariance principle requires decomposition rather than observer-dependent classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Sibling reading structural divergence and epsilon referent stability').

omega_variable(
    peer_review_theater_vs_function,
    'What proportion of current peer review activity constitutes genuine quality filtration versus performative gatekeeping that maintains institutional legitimacy?',
    'Meta-research on review consistency and predictive validity; economic analysis of publisher profit margins vs. marginal review service costs.',
    'A rising theater ratio would push the constraint toward piton or snare; a stable low ratio would support rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_theater_vs_function, empirical, 'Performative versus functional share of peer review activity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(legi_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(legi_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(legi_su_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(legi_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
