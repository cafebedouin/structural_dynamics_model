% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership via Social Performance and Recognition
 *   domain: social_ontology/political_philosophy
 *
 * SUMMARY:
 *   Gender category membership, in the social-role reading, is constituted
 *   through sustained performance and social recognition. A person becomes a
 *   woman (or is recognized as a woman) not through biological markers or
 *   self-declaration alone, but through performing femininity in
 *   recognizable, normative ways and receiving attestation from others across
 *   multiple social contexts. Trans women may be conditionally included in
 *   this framework if they pass (i.e., perform in ways that secure
 *   recognition), but their membership remains fragile and context-dependent.
 *   Cis women are also enrolled in continuous performance and re-recognition.
 *   The constraint solves a real coordination problem (how to adjudicate
 *   membership in the absence of fixed, inspectable markers) but extracts
 *   costs unevenly: gatekeepers benefit from authority and predictability;
 *   those whose performances are ambiguous or non-normative bear testing,
 *   scrutiny, and reclassification risk. This is ONE READING of a contested
 *   kernel; the biological-sex reading and gender-identity reading are other
 *   constraints instantiating the same kernel with different ε values and
 *   victim structures.
 *
 * KEY AGENTS:
 *   - Trans women conditional entrants: Identity-locked in a dual performance regime (internal commitment to gender identity + external performance for recognition).
 *   - Cis women collateral targets: Enrolled in the same performance-recognition machinery but with cumulative recognition capital; also bear costs when the boundary-policing machinery turns scrutinizing.
 *   - Established social recognizers: Collectively maintain the boundary by attest who belongs through interaction.
 *   - Category boundary police: Concentrated institutional power holders (HR, admissions, healthcare, media) who set norms and validate recognition.
 *   - Alternative readings advocates: Excluded from the framework itself; their arguments for biological or identity groundings are suppressed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.48).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership via Social Performance and Recognition").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '3fe37225-8469-4e8b-94ba-0696b272801c').
narrative_ontology:cs_kernel_codification('3fe37225-8469-4e8b-94ba-0696b272801c', distributed).
narrative_ontology:cs_authority_grounding('3fe37225-8469-4e8b-94ba-0696b272801c', practice).
narrative_ontology:cs_interpretation_layer_present('3fe37225-8469-4e8b-94ba-0696b272801c').
narrative_ontology:cs_reading_relation('3fe37225-8469-4e8b-94ba-0696b272801c', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fe37225-8469-4e8b-94ba-0696b272801c', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('3fe37225-8469-4e8b-94ba-0696b272801c', foundational, performance_constitutes_gender_membership).
narrative_ontology:cs_axiom_status(performance_constitutes_gender_membership, holdable).
narrative_ontology:cs_axiom_grounding('3fe37225-8469-4e8b-94ba-0696b272801c', performance_constitutes_gender_membership, conventional).
narrative_ontology:cs_axiom('3fe37225-8469-4e8b-94ba-0696b272801c', foundational, social_recognition_is_binding).
narrative_ontology:cs_axiom_status(social_recognition_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('3fe37225-8469-4e8b-94ba-0696b272801c', social_recognition_is_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('3fe37225-8469-4e8b-94ba-0696b272801c', performance_recognition_constitutes_membership).
narrative_ontology:cs_drift_state('3fe37225-8469-4e8b-94ba-0696b272801c', contemporary_identity_politics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3fe37225-8469-4e8b-94ba-0696b272801c', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, established_social_recognizers).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, categorical_stability_maintainers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_conditional_entrants).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, boundary_policing_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women_collateral_targets).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, category_boundary_police).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_collateral_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trans women who undergo the process of social recognition and performance to be accepted into the women's category. They must pass tests of performative competence (dress, gesture, voice, interaction patterns), manage ongoing scrutiny, and maintain consistent performance across multiple social contexts. Failure to sustain recognition at any point risks reclassification or exclusion. The cost of entry is continuous identity work.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_conditional_entrants, payer,
    powerless, biographical, identity_locked, national).

% Participants in social institutions (workplace, education, healthcare, kinship networks, public spaces) who collectively determine who counts as a woman through recognition practices. They assess conformity to performance norms, validate or withhold recognition, and communicate categorization through interaction. They benefit from the clarity and predictability of categorical boundaries maintained through gatekeeping.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, established_social_recognizers, beneficiary,
    organized, generational, mobile, national).

% Cisgender women who benefit from category recognition (social, legal, economic services) but also bear the costs of the boundary-maintenance machinery. They must continuously perform and re-prove their categorical membership; the same gatekeeping mechanisms that test trans women's entry can be deployed to scrutinize, exclude, or reclassify cis women perceived as insufficiently performative. They experience the constraint's suppression as internalized performance anxiety and external policing.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_collateral_targets, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, cis_women_collateral_targets, beneficiary).

% Actors with concentrated power to adjudicate categorical membership: institutional officials (HR, admissions, healthcare), cultural arbiters, and mass communication platforms that set norms. They enforce recognition standards and determine which performances count. They benefit from the reputation and authority that comes from gatekeeping — managing who belongs.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, category_boundary_police, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, category_boundary_police, beneficiary).

% Advocates for biological-sex-grounded or self-declaration-grounded readings of gender category membership. They argue that social-recognition gatekeeping is either too permissive (fails to exclude those with male biology) or too restrictive (denies self-declared identities), and they are systematically excluded from the recognition framework this reading instantiates. Their presence in the debate is heavily suppressed.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, alternative_gender_readings_advocates, excluded,
    moderate, generational, constrained, national).

% Judicial, legislative, and administrative bodies tasked with clarifying what counts as category membership for purposes of law (employment discrimination, sports competition, healthcare access, asylum). They observe how the social-role reading operates and adjudicate conflicts between readings, sometimes choosing to override social recognition with legal definition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, institutional_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, established_social_recognizers).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of category-membership adjudication: in the absence of a fixed, inspectable boundary marker, humans must coordinate on shared recognition practices to generate meaningful categorical distinctions. Social performance and recognition provide a decentralized mechanism for this coordination — community members collectively attest to membership through interaction and acceptance rather than a single gatekeeper determining status.
% TRANSFER_FUNCTION: Transfers the cost of categorical membership-proof from institutional fiat to individual performance work. Those seeking entry into the women's category must perform identity in recognizable forms and sustain that performance across social contexts. Benefits accrue to those who already hold stable recognition (established members) and to those who control the recognition machinery (gatekeepers); costs are borne by those whose membership is ambiguous or contested.
% ABSENT_VOICES: Biological-sex-reading advocates argue trans women should be excluded from the category regardless of passing/recognition; gender-identity-reading advocates argue self-declared identity suffices and performance should not be required. Neither group is seated at the social-recognition table — their exclusion is enforced by the reading's core premise (recognition is the gate). Additionally, those who cannot or will not perform according to recognized norms (gender-nonconforming individuals, across gender categories) are structurally voiceless.
% DISAPPEARANCE_RATIONALE: If social-recognition gatekeeping disappeared, the mechanisms for collectively adjudicating category membership would collapse into either legal/institutional definition (biological sex reading), self-declaration (identity reading), or persistent ambiguity. The social fabric depends on some shared understanding of who belongs where; removing the performance-recognition mechanism would force a shift to one of the alternative framings or radical decoupling of social category from institutional recognition.
% FOUNDING_PROBLEM: Pre-modern gender categories were (in this reading's framing) maintained through unquestioned tradition and visible roles; modernity fragmentized this: people move across contexts, perform in new ways, claim identities that disrupt inherited patterns. The founding problem was coordination in the absence of tradition — how do strangers, workmates, and institutions agree on categorical membership when people's performances don't align with inherited expectations? Social recognition (asking: does this person sustain recognizable performance across contexts and receive attestation?) emerged as a decentralized answer.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists and anthropologists who study gender performance (Butler, Goffman lineage) attest that gender is socially maintained through recognition; they support the reading. However, biological-sex advocates argue the founding problem was never really about coordination but about maintaining boundary against intrusion; identity advocates argue the problem was coercive enforcement, not coordination deficit. The three readings frame the founding problem itself differently — there is no external corroboration that can settle which problem actually existed.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint does solve a real coordination problem (category adjudication without fixed markers) but achieves this by displacing boundary maintenance onto individual performance work. Trans women must continuously prove they pass; cis women must maintain sufficient performative conformity to avoid reclassification. The coordination benefit is real; the extraction is the cost of membership-proofing distributed unevenly. Suppression is elevated (0.62) because the constraint persists partly through enforcement machinery: gatekeepers actively police boundaries, alternative readings are excluded from the framework, and there is social/institutional pressure to use the recognition framework rather than explore alternatives. Theater rises over the interval (0.25 to 0.43 at t=30) as the performance aspect becomes increasingly elaborate and ceremonial (ritual affirmation events, institutional recognition procedures) relative to actual coordination need — by t=30 the machinery is partly performative. Accessibility collapse is moderate (0.58) because alternatives (biological definition, identity-based definition, radical decoupling) remain conceptually available even though social pressure and institutional capture make them hard to adopt. Resistance is high (0.71) because multiple stakeholders actively contest the reading: biological-sex advocates argue it is too permissive; identity advocates argue it is too restrictive; some cis women resist the continuous performance burden; trans-exclusionary groups resist trans inclusion; gender-nonconforming people resist the normativity requirement. The measurement series shows modest extraction accumulation through the middle interval (extractiveness rises 0.38→0.52 from t=0 to t=30) followed by a slight decline to 0.48 by t=40, consistent with political pressure and institutional countermeasure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (established recognizers, boundary police) experience the constraint as legitimate coordination: clear categories, shared norms, predictable adjudication, authority to decide membership. The payer seats (trans women, cis women) experience it differently: trans women as a gauntlet of approval-seeking and passing-performance, dependent on judges' good faith; cis women as dual burden (recognition benefits vs. policing costs). The constraint's type should compute differently from these seats: from the agenda-setter's perspective it is rope (coordination with shared benefit); from payer perspectives it is tangled-rope or snare (coordination story as cover for extraction and boundary-maintenance authority). The engine derives directionality from power, exit options, and beneficiary/victim declarations; these asymmetries should produce per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women have identity-locked exit (self-concept fused with gender identity; leaving the category is not a real option even if recognition fails). They are listed as payers because they bear the costs of continuous performance and risk of reclassification. Their d is high (near target end, ~0.75-0.85) because identity-lock removes the optionality that would otherwise give them arbitrage-grade exit. Cis women are dual-positioned: they benefit from category recognition (social identity, access to women-specific services and spaces) but also bear performance and policing costs. Their d is moderate-symmetric (~0.45-0.55) because their benefits from recognition are substantial but offset by the policing burden. Established recognizers have moderate power and mobile exit (they can step out of gatekeeping roles), but they benefit from the arrangement's authority and predictability, making their d low (beneficiary end, ~0.2-0.3). Boundary police are institutional and benefit significantly from the authority gatekeeping grants; their d is near beneficiary (~0.15-0.25). Alternative-readings advocates are excluded, so they do not feed the directionality derivation (they have no structural relationship to this constraint's operation — they are outside it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination in the absence of fixed markers) is contested. Biological-sex advocates say the real problem was boundary-maintenance against unwanted intrusion, and the performance-recognition reading solves the wrong problem. Identity advocates say the real problem was coercive gatekeeping, and performance-recognition maintains it. This reading claims to solve coordination; mandatrophy surfaces when gatekeeping becomes primarily about exclusion-maintenance rather than genuine coordination. The theater ratio creeping toward 0.41-0.43 suggests the machinery is becoming more performative (ritual, ceremony, institutional procedure) and less coordinating (actually needed to solve the adjudication problem). If theater continues rising and extractiveness plateaus, the constraint would shift toward piton classification (inertial maintenance rather than genuine function). The ambiguous victim structure (trans women clearly targeted for performance-proofing; cis women targeted for conformity policing) is the mandatrophy hallmark: both groups bear extraction costs from a constraint that claims to benefit both through stable coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_identity_fusion,
    'Is the performance-recognition requirement compatible with genuine self-determined gender identity, or does requiring performance for recognition constitute a constraint on identity itself?',
    'Qualitative research tracking trans women''s subjective experience of the performance requirement (internalized vs. external, chosen vs. coerced); comparison with jurisdictions that mandate identity-only frameworks and measure identity-stability outcomes.',
    'If performance and identity are inseparable (performance as expression of identity), the constraint is genuine coordination. If performance is separable from identity (performance as masking or passing), the constraint extracts identity-suppression as hidden cost; reclassification to snare may be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_identity_fusion, conceptual, 'Whether performance-for-recognition is a constraint on identity itself or authentic expression of it.').

omega_variable(
    victim_set_ambiguity,
    'Who are the primary victims of this constraint? Trans women (bearing performance costs and recognition fragility)? Cis women (bearing policing and performance anxiety)? Or is the victim set distributed such that no stable victim group exists?',
    'Intersectional analysis: partition agents by race, class, disability status, and measure extraction variation within groups (is performance cost uniform for all trans women or does it differ by passing-privilege?). Empirical measure of who bears highest suppression cost.',
    'If victim set is clearly trans-women-primary, the constraint is snare-leaning. If victim set is distributed (cis women + trans women both), it remains tangled-rope but with mandatrophy flag (coordinating both into the same cost-bearing role). If victim set is unclear, structural ambiguity omega remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_ambiguity, empirical, 'Ambiguity in victim identification due to distributed costs and intersecting extraction mechanisms.').

omega_variable(
    coordination_necessity,
    'Is social performance and recognition actually the least-costly way to coordinate on category membership in modern institutional contexts, or are there less extractive alternatives (legal definition, self-declaration, radical decoupling)?',
    'Comparative institutional analysis: document coordination costs (transaction failures, boundary disputes, administrative overhead) under different framings across jurisdictions. Measure administrative efficiency, litigation frequency, institutional clarity.',
    'If performance-recognition is genuinely least-costly coordination, extraction is coordination cost and the constraint remains rope-classified. If alternatives are demonstrably cheaper for coordination while performance-recognition persists due to other interests (gatekeeping authority, boundary control), the constraint is misclassified as tangled-rope and should be reclassified snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity, empirical, 'Whether the performance-recognition mechanism is structurally necessary for coordination or sustained by other interests.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings (biological, identity, social-role) have genuinely incommensurable core premises, or can an agent coherently hold two or all three simultaneously?',
    'Philosophical analysis of the logical structure of each reading''s foundational claims; empirical observation of how agents actually combine readings in practice (e.g., someone who believes gender is grounded in identity but also values social recognition).',
    'If readings are incommensurable, one or more should show forecloses relations. If readings are compatible (held simultaneously), the foreclosure relations should be removed and replaced with coexists_with, shifting the constraint-family network topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical compatibility/incompatibility of the three readings'' foundational premises.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the suppression measured (0.62) primarily structural (institutional gatekeeping, explicit exclusion rules, resource barriers) or internalized (trans women and cis women internalizing performance requirements, policing themselves)?',
    'Post-exit ethnography: track what happens to suppression when agents leave the recognition framework entirely (radical decoupling, emigration, institutional defection). If suppression persists, it is internalized; if it dissipates, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — agents carry the policing with them after exit. Treatment implications shift: internalization suggests longer-term deprogramming is needed, not just barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural vs. internalized mechanism of suppression in performance-recognition gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__social_role_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__social_role_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__social_role_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__social_role_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__social_role_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__social_role_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__social_role_reading, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__social_role_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__social_role_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__social_role_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__social_role_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__social_role_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the GENDERED_CATEGORY_MEMBERSHIP kernel. The biological-sex reading grounds membership in immutable markers and produces low extraction (mountain-adjacent). The identity reading grounds membership in self-declaration and produces moderate extraction (rope/tangled-rope boundary). This social-role reading grounds membership in performance and recognition, producing moderate extraction with distributed gatekeeping (tangled-rope). All three are instantiations of the same kernel (one persisting commitment: how to adjudicate gender category membership) but with different ε values, beneficiary/victim structures, and gatekeeping mechanisms. They are linked by affects_constraints to model structural influence: changes in institutional adoption of one reading create pressure on the others. Ε-invariance is preserved: each reading has its own ε referent (the standing arrangement the reading itself describes), and the three ε values differ substantially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
