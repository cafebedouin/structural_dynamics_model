% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Membership via Immutable Biology (Biology Reading)
 *   domain: social/legal/ontological
 *
 * SUMMARY:
 *   Under the biology reading of the sex/gender category kernel, category
 *   membership in 'woman' is determined by immutable reproductive biology
 *   (chromosomes, gonadal/reproductive anatomy assigned at birth), treated as
 *   objective and natural. This reading instantiates a constraint that
 *   coordinates legal and institutional recognition of a sex-specific
 *   category while extracting from trans women (excluded regardless of
 *   identity/transition) and intersex individuals (forced into binary
 *   assignment). The reading is one of three structurally distinct framings
 *   of the same kernel (sex/gender category); the identity_reading and
 *   hybrid_reading are separate constraints with different ε values,
 *   different victim sets, and different enforcement structures. This story
 *   authors ONLY the biology reading as a clean ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - cis_women (beneficiary; category member by unambiguous assignment; collective benefit from protections and recognition)
 *   - trans_women (payer; structurally excluded; identity-locked exit; subject to categorical denial)
 *   - intersex individuals (payer; forced into binary; trapped between assignment and self-identification)
 *   - enforcement infrastructure (agenda-setter; courts, legislatures, medical boards; maintains boundary)
 *   - sex-essentialist doctrine (vindicated proposition; benefits from institutional recognition and legal operationalization)
 *   - identity-reading advocates (excluded; would contest category criterion)
 *   - analytical observer (sees the constraint's structure from outside the contest)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership via Immutable Biology (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/ontological").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '656a12f4-135c-42f6-b2cc-ab2cfdf69dee').
narrative_ontology:cs_kernel_codification('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', fixed_text).
narrative_ontology:cs_authority_grounding('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', lineage).
narrative_ontology:cs_interpretation_layer_present('656a12f4-135c-42f6-b2cc-ab2cfdf69dee').
narrative_ontology:cs_reading_relation('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', foundational, reproductive_biology_determines_category).
narrative_ontology:cs_axiom_status(reproductive_biology_determines_category, holdable).
narrative_ontology:cs_axiom_grounding('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', reproductive_biology_determines_category, empirically_contingent).
narrative_ontology:cs_axiom('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', foundational, immutable_status_at_birth).
narrative_ontology:cs_axiom_status(immutable_status_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', immutable_status_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', secondary, sex_category_objectively_real).
narrative_ontology:cs_axiom_status(sex_category_objectively_real, overridden).
narrative_ontology:cs_axiom_grounding('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', sex_category_objectively_real, deontological).
narrative_ontology:cs_reference_frame('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', biological_sex_essentialism).
narrative_ontology:cs_drift_state('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', contemporary_trans_visibility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('656a12f4-135c-42f6-b2cc-ab2cfdf69dee', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_category_holder).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals_forced_binary).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, sex_essential_enforcement_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds unambiguous membership in the 'woman' category under this reading by virtue of chromosomal/anatomical assignment at birth. Benefits from category-specific legal protections, social recognition, institutional access (women's sports, single-sex spaces, scholarship programs). The reading treats cis women as the sole population whose sex-based harms are legible within the category framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_category_holder, beneficiary,
    moderate, generational, analytical, global).

% Structurally excluded from the 'woman' category under this reading regardless of medical transition, legal documentation, or lived experience. Bears the cost of enforced misclassification: denied access to women's institutional spaces, ineligible for sex-specific legal protections, subject to categorical denial in healthcare/employment/housing/sports contexts. Identity-locked exit: cannot exit gender identity itself; exit from the category system requires abandoning self-identification.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Forced into binary assignment (man/woman) despite ambiguous chromosomal or anatomical characteristics at birth. Medical assignment is often made by institutional actors (doctors) under the biological reading; reassignment requires institutional appeal and documentation, but the category system offers no third option. Bears the cost of categorical misfit: assigned category may not match chromosomal status, internal anatomy, or identity; no institutional exit from the binary.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals_forced_binary, payer,
    powerless, biographical, trapped, global).

% Courts, legislatures, and administrative bodies that enforce boundary maintenance by adjudicating category membership (sports eligibility, bathroom access, legal documentation, institutional spaces). Must develop and defend criteria for 'authentic' biological status, handle edge cases, manage intersex assignment, and justify exclusions. The reading requires active enforcement because category boundaries are contested and category membership must be verified/proven.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, enforcement_infrastructure_actors, agenda_setter,
    institutional, generational, analytical, national).

% The doctrine that biological sex is a foundational category for understanding social harm, institutional design, and legal protection. This reading instantiates that doctrine by making immutable biology the category criterion. The doctrine benefits from the reading's enforcement (naturalization and institutional recognition), but is not itself an actor.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sex_essentialism_doctrine_holders, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(sex_gender_category__biology_reading, sex_essentialism_doctrine_holders).

% Scholarly and activist traditions that ground women's oppression in reproductive biology and sexed division of labor. This reading provides institutional and legal operationalization of that analysis. The tradition benefits from institutional recognition of its core premises, but cannot itself collect rents or hold power.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, feminist_material_analysis_tradition, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(sex_gender_category__biology_reading, feminist_material_analysis_tradition).

% Parties who hold the competing 'identity_reading' (self-identification determines category membership). Would argue for category membership based on subjective identity rather than birth assignment. Excluded from agenda-setting under this reading; their advocacy is treated as category confusion rather than legitimate alternative framing.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, identity_reading_advocates, excluded,
    moderate, biographical, mobile, global).

% Parties who hold the competing 'hybrid_reading' (combination of biology and medical transition gatekeeping). Would argue for conditional category membership after medical/legal processes. Excluded from agenda-setting under the pure biology reading; their position is treated as compromise rather than legitimate alternative.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, hybrid_reading_advocates, excluded,
    moderate, biographical, mobile, global).

% Standpoint from which the constraint's structure is visible: a system that coordinates exclusion of trans women and intersex boundary cases while benefiting cis women category membership and vindicating sex-essentialist doctrine.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, cis_women_category_holder).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a categorical boundary for 'woman' based on immutable biological criteria (chromosomes, reproductive anatomy at birth), enabling: sex-specific legal protections (anti-discrimination law keyed to sex), institutional spaces organized by sex, and empirical research on sex-based harms grounded in a stable referent population.
% TRANSFER_FUNCTION: Moves institutional legitimacy, legal recognition, and access to sex-specific resources TO cis women (who are unambiguously included) and FROM trans women and intersex individuals (who are structurally excluded or forced into inaccurate assignment). Transfers enforcement burden to institutional actors who must defend/maintain boundary; transfers identity-fit costs to those whose bodies or identity do not map to assigned categories.
% ABSENT_VOICES: Trans women and intersex individuals are structurally excluded from adjudicating the categories that classify them; they can object but not set the terms. Their exclusion is structural to the boundary enforcement—the reading's core claim is that their identity/subjective experience is not a legitimate category criterion, so their testimony for inclusion is treated as misconstruing the question.
% DISAPPEARANCE_RATIONALE: If this constraint (biological criteria determining sex category) vanished overnight: those holding the biology reading would argue that sex-specific protections dissolve and sex-based harm becomes invisible (world_rearranges); those holding the identity_reading would argue that institutional access becomes more equitable and individuals self-determine their categories (world_rearranges toward justice); those holding the hybrid_reading would argue that medical gatekeeping becomes the operative boundary (world_rearranges into a different constraint). The disappearance verdict is contested because the parties radically disagree what the constraint's absence would mean.
% FOUNDING_PROBLEM: Sex-based harm and discrimination structured through reproductive biology and sexed division of labor require a stable referent category for women as the population experiencing that specific harm; without a biological criterion, sex-specific protections (Title IX, reproductive healthcare access, anti-trafficking law) have no determinate subject population and legal recourse becomes incoherent.
% FOUNDING_PROBLEM_CORROBORATION: Sex-essentialist scholars and some feminist legal theorists attest the founding problem is live: sex-based harm remains rooted in reproductive biology and cannot be addressed without a stable category. OUTSIDE corroboration: anti-trafficking organizations and reproductive rights advocates cite the need for sex-specific legal protections. Counter-corroboration (from the identity_reading seats and trans advocates): the founding problem assumes a unitary sex-based harm category; once disaggregated by lived experience, the category dissolves and identity-based protections address the real harms. No consensus outside the biology-reading beneficiary seats.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because the reading channels institutional legitimacy and legal access specifically to cis women while denying it to trans women and forcing intersex individuals into misfit categories. Suppression is even higher (0.72) because maintaining the boundary against contestation requires active enforcement—institutional actors must verify 'authentic' biological status, exclude trans women from institutional spaces, manage intersex assignment, and defend against the alternative readings' claims. Theater ratio is substantial (0.41) because over the 40-year interval, an increasing share of boundary maintenance becomes ritual verification (documentation audits, institutional gatekeeping) rather than novel coordination: the biological criterion was established earlier; enforcement infrastructure has hardened into procedural performance. The measurements show suppression and theater both rising faster than base extractiveness—the core function (coordination of sex-specific protections) is stable, but the administrative cost of defending the boundary against contestation is accumulating. All metrics on one shared time grid (t ∈ {0, 8, 16, 24, 32, 40}); no grid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (cis_women) and the payer seats (trans_women, intersex individuals) should compute dramatically differently under the engine's per-seat classification. From the cis_women beneficiary position, this appears as genuine coordination: a stable category enabling sex-specific law and recognition. From the trans_women payer position, this appears as pure extraction: categorical denial that persists because enforced, not chosen. From the intersex position, this appears as coercive categorization—assignment by institutional authority without exit. The engine computes these divergences from the structural data (power, exit_options, role); the authored claim (tangled_rope) does NOT attempt to reconcile them—the claim names the structure; the metrics describe its actual operation; the divergence is exactly what the per-seat computation exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women carry d near the beneficiary end because they are unambiguously included, hold a secure category, benefit from legal protections and institutional access, and face no enforcement burden. Trans women carry d near the target end because they are structurally excluded and bear the cost of categorization they cannot exit or contest effectively. Intersex individuals carry d even further target-ward because they face BOTH categorical exclusion (non-binary anatomy forced into binary categories) AND trapped exit (no institutional path out). Enforcement infrastructure actors carry high power but constrained exit (they administer the constraint; they cannot simply stop) and thus moderate d. The suppression measurement (0.72) encodes that the constraint's persistence depends on enforcing the boundary against resistance—it is not naturally stable; it requires active institutional defense.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT a case of mandatrophy (function-death with institutional persistence). The founding problem—need for stable sex-specific legal category to address sex-based harm—remains contested but arguably live. The constraint exhibits rising theater (0.41 at t=40) because institutional enforcement is increasingly procedural/performative relative to its coordination function, but this is NOT mandatrophy; it is the cost accumulation that occurs when a boundary is actively contested. The reading does not claim the function is obsolete; it claims the function requires this biological criterion and that enforcement is justified to maintain it. Mandatrophy would only be diagnosed if the founding problem were dead AND the constraint persisted; here, status is 'contested'—no mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.72) purely STRUCTURAL (external barriers: policy, institutional exclusion, enforcement machinery) or does it include INTERNALIZED components (trans women and intersex individuals have cognitively adapted to exclusion, developed shame about non-conformity, or believe the categorization is legitimate)?',
    'Post-exit trajectory: if trans women and intersex individuals maintain suppression-like behaviors (shame, self-exclusion, compliance with category boundaries) AFTER legal/institutional barriers fall, that signals internalization. If suppression drops sharply upon barrier removal, it was structural. Qualitative interviews tracking identity integrity post-exit versus mid-exclusion provide additional evidence.',
    'If internalized: effective suppression is higher than the structural measure; changing institutional policy alone will not end extraction. If structural: changing institutional barriers will quickly reduce effective suppression. The classification of this constraint (tangled_rope vs snare) hinges partly on this distinction—pure structural suppression is more consistent with tangled_rope; internalized suppression that persists without external enforcement is more consistent with snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in identity-based exclusion').

omega_variable(
    biological_determinism_operationalization,
    'Can the ''immutable reproductive biology'' criterion be operationalized consistently, or does implementation require contested adjudication and boundary policing?',
    'Institutional audit: document what the enforcement infrastructure ACTUALLY uses to verify biological status (documented chromosomes at birth? current anatomy? medical records? self-report?). Check for consistency across cases and jurisdictions. If implementation varies, the apparent biological criterion is actually a procedural category maintained by institutional discretion, not biological fact.',
    'If biology is operationalized consistently via objective standard: this reading''s claim to naturality is supported. If operationalization requires institutional judgment and varies by case: the reading instantiates an institutional category disguised as biological criterion, supporting reclassification toward snare (pure institutional extraction) or reframing the reading itself as a misconceived framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_operationalization, empirical, 'Whether biological criterion can be operationalized as objective fact or requires contested institutional judgment').

omega_variable(
    sex_based_harm_category_stability,
    'Is the category ''cis women'' (as sole victim population for sex-based harm) stable and coherent, or does sex-based harm vary so radically across cis women''s positions (race, class, disability, sexuality) that a single category obscures more than it reveals?',
    'Comparative analysis of sex-based harm distributions: does reproductive biology predict vulnerability to sex-specific harms better than other axes? Do cis women from different social positions experience ''sex-based harm'' coherently, or do the harms fragment along other lines? Do trans women face sex-based harms (pregnancy exclusion, reproductive healthcare barriers, sexual violence) despite formal exclusion from the category?',
    'If the category is unstable: the reading''s coordination function (stable referent for sex-specific law) is compromised; sex-specific protections may need to be disaggregated by axis rather than unified in a category. If stable: the reading''s framing is supported. This bears on whether the constraint genuinely solves a coordination problem or merely naturalizes an exclusionary category.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sex_based_harm_category_stability, conceptual, 'Whether sex-based harm aligns with or fragments across the biological sex category').

omega_variable(
    reading_boundary_competition,
    'How do the biology_reading, identity_reading, and hybrid_reading relate structurally? Is the competition zero-sum (one reading excludes another from institutional legitimacy) or can the readings coexist by serving different institutional domains?',
    'Institutional mapping: track which domains use which reading (sports use biology; healthcare may use hybrid or identity; legal gender recognition uses identity or hybrid in many jurisdictions; sex-specific protections use biology or hybrid). If readings segregate by domain, coexistence is possible. If a single domain contests all three readings, zero-sum competition holds.',
    'If coexistence is possible: this reading is one legitimate framing among several, and institutional pluralism might reduce extraction by allowing individuals to choose the framing that fits their situation. If zero-sum: the readings foreclose each other, and institutional choice will necessarily exclude some populations. This affects whether the divergence in per-seat classification is resolvable via institutional choice or fundamentally contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_competition, conceptual, 'Whether competing readings coexist across domains or zero-sum compete for institutional legitimacy').

omega_variable(
    intersex_boundary_forcing,
    'What proportion of the measured suppression (0.72) is attributable to intersex individuals'' forced binary assignment versus trans women''s categorical exclusion? Does the constraint''s enforcement burden rise systematically with intersex visibility?',
    'Historical enforcement cost tracking: document the institutional labor devoted to intersex adjudication (medical assignment at birth, reassignment appeals, documentation disputes) versus trans woman exclusion (sports policy, institutional access denial). Compare across time periods and jurisdictions with different policies toward intersex recognition.',
    'If intersex boundary-forcing consumes disproportionate enforcement effort: the biological reading incurs a hidden cost (administrative complexity, identity harm to intersex individuals) not visible in the beneficiary/victim analysis. If trans woman exclusion is the dominant enforcement cost: the extraction is primarily distributive (transfer from trans women to cis women) rather than administrative waste. This informs whether the constraint is optimizable (reduce intersex forcing) or structurally extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_boundary_forcing, empirical, 'Enforcement cost allocation between intersex boundary-forcing and trans woman exclusion').

omega_variable(
    kernel_reading_distinct_constraints,
    'Are biology_reading, identity_reading, and hybrid_reading truly SEPARATE constraints with distinct ε values, or are they the same constraint measured from different observer perspectives?',
    'ε-invariance test: author each reading as a separate constraint story and compute its ε independently. If the referent (sex/gender category arrangement) produces different ε values across readings, they are separate constraints by the ε-invariance principle (OQ-26). If ε is the same but interpretation differs, they are the same constraint viewed from different frames (perspective-dependent, not constraint-distinct).',
    'If separate constraints: the kernel is genuinely contested, and the readings cannot be reconciled by better information or compromise—they instantiate structurally different arrangements. If same constraint, different perspectives: the readings might be reconcilable via institutional arrangement (e.g., legal gender recognition uses identity; sports use biology; both operate on the same underlying category). This affects whether institutional pluralism is possible or whether institutional choice must select one reading and exclude others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinct_constraints, conceptual, 'Whether competing readings are separate ε-distinct constraints or the same constraint viewed from different frames').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__biology_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__biology_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__biology_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(sex__tr_t32, sex_gender_category__biology_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__biology_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__biology_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__biology_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(sex__be_t32, sex_gender_category__biology_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__biology_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__biology_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__biology_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(sex__su_t32, sex_gender_category__biology_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three structurally distinct constraints: biology_reading (this file), identity_reading, and hybrid_reading. Each reading instantiates a different constraint because each produces a different ε (extraction level), different victim/beneficiary structures, and different enforcement mechanisms. The readings are linked by network.affects_constraints to indicate kernel membership and to enable the engine to track how institutional choice among readings affects the constraint family's overall extractiveness profile. Per ε-invariance principle (OQ-26), a single observable measured via three different definitional frameworks that produce different ε values indicates three constraints, not one constraint measured three ways. All three stories share the same kernel_id; they differ in reading_id and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
