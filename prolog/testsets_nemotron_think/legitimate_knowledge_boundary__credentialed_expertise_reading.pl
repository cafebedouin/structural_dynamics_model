% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Peer Review as Sole Legitimizer of Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'credentialed expertise reading' of
 *   the contested kernel 'legitimate_knowledge_boundary'. It asserts that
 *   legitimate knowledge derives exclusively from methodologically rigorous
 *   inquiry validated by credentialed peer review. The reading instantiate a
 *   constraint with high barriers to entry, centralized gatekeeping through
 *   journals and institutions, asymmetric enforcement of methodological
 *   standards (novel/challenging work faces higher bars than incremental work
 *   within dominant paradigms), and the treatment of expert consensus as a
 *   truth-proxy. The coordination function is genuine: peer review solves the
 *   problem of distributed trust at scale. But the extraction is substantial
 *   and asymmetric: the system transfers epistemic authority, career capital,
 *   and material resources to credentialed insiders while suppressing
 *   alternative epistemic forms. The constraint requires active enforcement
 *   through editorial gatekeeping, credentialing bodies, and funding
 *   mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.72).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.78).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Peer Review as Sole Legitimizer of Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'be61caee-f2cc-40c5-bf40-1a6300159969').
narrative_ontology:cs_kernel_codification('be61caee-f2cc-40c5-bf40-1a6300159969', formalized).
narrative_ontology:cs_authority_grounding('be61caee-f2cc-40c5-bf40-1a6300159969', extraction).
narrative_ontology:cs_interpretation_layer_present('be61caee-f2cc-40c5-bf40-1a6300159969').
narrative_ontology:cs_reading_relation('be61caee-f2cc-40c5-bf40-1a6300159969', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_reading_relation('be61caee-f2cc-40c5-bf40-1a6300159969', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('be61caee-f2cc-40c5-bf40-1a6300159969', foundational, credentialed_peer_review_sole_legitimizer).
narrative_ontology:cs_axiom_status(credentialed_peer_review_sole_legitimizer, holdable).
narrative_ontology:cs_axiom_grounding('be61caee-f2cc-40c5-bf40-1a6300159969', credentialed_peer_review_sole_legitimizer, conventional).
narrative_ontology:cs_axiom('be61caee-f2cc-40c5-bf40-1a6300159969', secondary, expert_consensus_tracks_truth).
narrative_ontology:cs_axiom_status(expert_consensus_tracks_truth, holdable).
narrative_ontology:cs_axiom_grounding('be61caee-f2cc-40c5-bf40-1a6300159969', expert_consensus_tracks_truth, empirically_contingent).
narrative_ontology:cs_reference_frame('be61caee-f2cc-40c5-bf40-1a6300159969', post_ww2_peer_review_consensus).
narrative_ontology:cs_drift_state('be61caee-f2cc-40c5-bf40-1a6300159969', contemporary_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be61caee-f2cc-40c5-bf40-1a6300159969', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, journal_publishers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, early_career_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, outsider_scholars).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers_and_public).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_method_superiority).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, institutional_epistemic_authority).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_quality_guarantee).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold recognized credentials and institutional affiliations that grant them access to publication venues, funding, and epistemic authority. Their work receives the benefit of the doubt in peer review; they serve as reviewers and editors, shaping the standards that reinforce their position. Exit means leaving the academic system entirely, forfeiting the capital they have accumulated.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, beneficiary,
    organized, biographical, constrained, global).

% Universities and research institutes control credentialing (PhDs, faculty positions) and host the infrastructure of peer review. They capture prestige, funding, and legitimacy from the system. They set hiring and promotion criteria that mandate publication in peer-reviewed venues, making the constraint self-reinforcing. They can create alternative pathways but rarely do, as the current system secures their gatekeeping role.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, agenda_setter).

% Commercial and society publishers own the venues where peer review occurs. They extract rent from both authors (APCs) and institutions (subscriptions) while relying on unpaid expert labor for review. Their business model depends on the credentialed peer review seal as a quality mark that justifies paywalls. They enforce the constraint through copyright, indexing requirements, and editorial policies.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, journal_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Must publish in high-impact peer-reviewed journals to secure positions, grants, and tenure. They provide free review labor, pay article processing charges, and face high rejection rates under asymmetric standards (novel work held to higher bars than incremental extensions of established paradigms). Their career survival depends on satisfying gatekeepers who benefit from the constraint.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, early_career_researchers, payer,
    moderate, biographical, constrained, global).

% Researchers without institutional affiliation, from marginalized disciplines, or working in non-dominant paradigms (e.g., indigenous science, citizen science, independent scholars). Their work is systematically excluded by credential checks, methodological narrowness, and reviewer bias. They bear the cost of the constraint's suppression with no access to its coordination benefits. Exit is not an option — they are already outside.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, outsider_scholars, payer,
    powerless, biographical, trapped, global).

% Communities holding place-based, embodied, or intergenerational knowledge (indigenous communities, patient groups, craft practitioners, local ecologists). Their knowledge forms are not recognized as legitimate because they do not conform to the methodological template of peer-reviewed publication. The constraint renders their epistemic contributions invisible to policy and funding. Their identity is fused to their knowledge practices; exit means epistemic erasure.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders, excluded,
    powerless, generational, identity_locked, local).

% Rely on the peer-reviewed literature as a filter for decision-making. They benefit from a (putatively) reliable knowledge base but suffer when the constraint suppresses relevant experiential or interdisciplinary knowledge (e.g., in public health, environmental management, social policy). They have no direct role in the constraint's operation but bear its downstream consequences.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers_and_public, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers_and_public, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, institutionally embedded mechanism for evaluating knowledge claims across distributed researchers, enabling cumulative science, credit allocation, and a (contested) proxy for reliability that funders and policymakers can use without assessing each claim individually.
% TRANSFER_FUNCTION: Moves epistemic authority, career capital, funding, and publication access from outsiders, early-career researchers, and non-credentialed knowledge holders to credentialed experts, institutions, and publishers. The transfer operates through gatekeeping: the credential and the peer-reviewed publication become the tollgates for legitimate participation.
% ABSENT_VOICES: Indigenous knowledge keepers, patient-experience experts, community-based researchers, and independent scholars are structurally excluded from the peer-review system. They would challenge the equation of methodological rigor with a specific institutional form, and the treatment of lived experience as anecdote rather than evidence. Their absence is maintained by credential requirements, language barriers, formatting norms, and the epistemic hierarchy the constraint enforces.
% DISAPPEARANCE_RATIONALE: If credentialed peer review vanished overnight, the coordination function it provides (quality filtering, credit allocation, cumulative reference) would not disappear — researchers would build alternative validation mechanisms (open review, replication markets, community validation, AI-assisted screening). But the distribution of epistemic authority would radically shift: institutional gatekeepers would lose their monopoly, experiential knowledge would gain direct policy access, and the career structure of academia would collapse. The world rearranges because the constraint organizes the entire political economy of knowledge production.
% FOUNDING_PROBLEM: Pre-WWII science lacked a scalable, trusted mechanism for certifying claims across a rapidly expanding, geographically distributed research enterprise. Journals existed but review was editorial, not expert; fraud and error were hard to catch; funders and governments had no reliable proxy for quality. The post-war institutionalization of systematic peer review solved the coordination problem of trust at scale.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science (e.g., Csisaras, Baldwin) document that peer review was not universal until the 1960s-70s and was adopted partly for bureaucratic manageability, not purely epistemic reasons. The replication crisis literature (Ioannidis, Open Science Collaboration) and science studies scholars (Jasanoff, Wynne) attest that the founding problem — reliable quality certification at scale — is substantially unsolved by the current system. Institutional leaders and publishers maintain the problem is still live; critics outside the beneficiary set argue it has morphed into a rent-extraction mechanism.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers significant value (career survival, funding, epistemic recognition) from a broad base of knowledge producers to a narrow set of credentialed gatekeepers and institutions. Suppression (0.78) is higher still because the constraint's persistence depends on actively excluding alternative validation pathways — not merely lacking them, but structurally marginalizing them (funding rules, promotion criteria, policy evidence hierarchies). Theater ratio (0.42) reflects that peer review performs real quality-control work, but a growing share of its activity is performative: checking boxes for methodological conformity rather than substantive engagement, enforcing formatting over rigor, and serving as a barrier rather than a filter. Accessibility collapse (0.82) is high because once the peer-reviewed literature is accepted as the sole legitimate source, alternatives become epistemically invisible — not just unavailable, but unthinkable as knowledge. Resistance (0.55) is moderate: open science, replication crisis, indigenous data sovereignty, and patient-led research mount real challenges, but they remain marginal to the core institutional apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (credentialed experts, institutions), the constraint appears as a Rope: a genuine coordination mechanism they built and maintain that solves the trust-at-scale problem. From the payer seats (early-career, outsiders), it appears as a Snare: an enforced barrier that extracts their labor and excludes their contributions. From the excluded seat (experiential knowledge holders), it appears as a structural erasure: their knowledge is not just unevaluated but rendered invisible by the constraint's categories. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality that the coordination function is real but asymmetrically extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts, academic institutions, and journal publishers are structural beneficiaries (d near 0.0-0.2): they collect the rents (prestige, funding, revenue) and control the rules. Early-career researchers are payers with constrained exit (d ~0.7): they must pay the toll to advance, but can theoretically leave academia. Outsider scholars are payers with trapped exit (d ~0.9): they bear the exclusion with no pathway in. Experiential knowledge holders are excluded and identity-locked (d ~0.95): their epistemic identity is constituted by the very knowledge forms the constraint renders illegitimate. Policy-makers are observers with analytical exit (d ~0.5): they use the constraint's output but could theoretically consult other sources — though institutional pressure makes this costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scalable trust certification for post-war big science) is contested: the coordination function persists but the extraction has accumulated. The constraint no longer merely solves the founding problem; it has become the primary mechanism for distributing epistemic authority and material resources in the knowledge economy. The mandatrophy is unresolved: the arrangement persists because the beneficiaries (institutions, publishers, established experts) control the levers of change, while the costs are distributed across a fragmented, low-power payer base. The theater ratio rise and extraction accumulation in the measurements capture this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_quality_correlation,
    'Does credentialed peer review actually correlate with knowledge quality/reliability, or has the correlation decayed as the system scaled?',
    'Large-scale replication studies comparing peer-reviewed vs. pre-print vs. alternative validation pathways; tracking of retraction rates, fraud detection, and predictive validity of publication venue for subsequent reproducibility.',
    'If the correlation is weak or negative, the coordination function is largely theatrical and the constraint is a Snare masquerading as a Rope. If strong, the extraction is the price of a genuine coordination service.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peer_review_quality_correlation, empirical, 'Whether the constraint''s coordination function delivers on its epistemic promise').

omega_variable(
    asymmetric_rigor_enforcement,
    'Is methodological rigor enforced symmetrically, or are dominant paradigms and established researchers held to lower standards than challengers and outsiders?',
    'Content analysis of review reports across fields; comparison of rejection reasons for paradigm-conforming vs. paradigm-challenging submissions; audit studies with matched manuscripts from credentialed vs. non-credentialed authors.',
    'Asymmetric enforcement would confirm the extraction is structural — the constraint does not merely coordinate but actively suppresses epistemic competition. Symmetric enforcement would support the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_rigor_enforcement, empirical, 'Whether the constraint''s rigor requirement operates as a neutral filter or a gatekeeping tool').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''legitimate_knowledge_boundary'' refer to a single epistemic boundary, or does it conflate distinct boundaries (truth-tracking, policy-relevance, cultural-legitimacy, funding-eligibility) that different readings legitimately prioritize?',
    'Disaggregation of the kernel into its functional components: what specific decisions or allocations does each reading treat as the stakes of legitimacy? Mapping the decision-contexts where each reading''s boundary is invoked.',
    'If the kernel conflates multiple boundaries, the three readings may not be competing for the same territory — they may be talking past each other. This would reframe the contest as a category error rather than a substantive disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a coherent contest or a conflation of distinct epistemic boundaries').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of experiential knowledge primarily structural (funding rules, publication formats) or internalized (communities accepting that their knowledge is ''not real science'')?',
    'Post-exit trajectories: when communities build their own validation infrastructures (indigenous peer review, patient-led registries), does the suppression persist? Comparative analysis of communities that resist internalization vs. those that accept marginalization.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would increase the constraint''s extractiveness for the experiential_knowledge_holders seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression mechanism for excluded knowledge holders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_ce_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lkb_ce_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(lkb_ce_tr_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(lkb_ce_tr_t48, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(lkb_ce_tr_t64, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 64, 0.4).
narrative_ontology:measurement(lkb_ce_tr_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(lkb_ce_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lkb_ce_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(lkb_ce_be_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(lkb_ce_be_t48, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(lkb_ce_be_t64, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 64, 0.68).
narrative_ontology:measurement(lkb_ce_be_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lkb_ce_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lkb_ce_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(lkb_ce_su_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(lkb_ce_su_t48, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(lkb_ce_su_t64, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 64, 0.76).
narrative_ontology:measurement(lkb_ce_su_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 80, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, information_standard).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.03).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_allocation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, evidence_based_policy_mandate).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_promotion_criteria).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimate_knowledge_boundary kernel. The credentialed_expertise_reading draws the legitimacy boundary at institutional peer review; the experiential_pluralism_reading draws it at community validation; the hybrid_coproduction_reading attempts integration. The ε values differ substantially: this reading shows high extraction (0.72) because it treats the peer-review gate as a tollgate; the experiential reading would show high suppression from the credentialed side; the hybrid reading would show moderate extraction with complex coordination. They are linked via affects_constraints because the credentialed reading's institutional dominance structurally shapes the operating environment for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, institutional, 0.15).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
