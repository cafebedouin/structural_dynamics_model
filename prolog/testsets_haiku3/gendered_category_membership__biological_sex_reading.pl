% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership: Biological Sex Reading
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   definition of category membership for 'woman' and 'man'. Under the
 *   biological-sex reading, category membership is grounded in immutable
 *   biological markers (chromosomes, reproductive anatomy, sex assignment at
 *   birth) and codified in legal documentation and institutional practice.
 *   Trans women are structurally excluded; sex-segregated spaces preserve the
 *   binary biology boundary; enforcement machinery operates through document
 *   systems, facility gatekeeping, and legal prohibition. The constraint
 *   exhibits high extractiveness because it transfers social recognition and
 *   bodily autonomy from trans women and gender-nonconforming individuals to
 *   cis women and institutional administrations. High suppression reflects
 *   the enforcement cost of maintaining the boundary against identity-based
 *   alternatives. This is ONE reading of the kernel — the
 *   biological_sex_reading. Sibling readings (gender_identity_reading,
 *   social_role_reading) instantiate different constraints with different
 *   beneficiary/victim structures, different extractiveness profiles, and
 *   different coherence conditions. This story authors only the
 *   biological-sex reading as a clean ε-invariant constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.81).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.89).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership: Biological Sex Reading").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '6e72237e-ccab-4377-a981-950c03f5f824').
narrative_ontology:cs_kernel_codification('6e72237e-ccab-4377-a981-950c03f5f824', fixed_text).
narrative_ontology:cs_authority_grounding('6e72237e-ccab-4377-a981-950c03f5f824', lineage).
narrative_ontology:cs_interpretation_layer_present('6e72237e-ccab-4377-a981-950c03f5f824').
narrative_ontology:cs_reading_relation('6e72237e-ccab-4377-a981-950c03f5f824', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6e72237e-ccab-4377-a981-950c03f5f824', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('6e72237e-ccab-4377-a981-950c03f5f824', foundational, sex_assignment_at_birth_determines_category_membership).
narrative_ontology:cs_axiom_status(sex_assignment_at_birth_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('6e72237e-ccab-4377-a981-950c03f5f824', sex_assignment_at_birth_determines_category_membership, empirically_contingent).
narrative_ontology:cs_axiom('6e72237e-ccab-4377-a981-950c03f5f824', foundational, biological_sex_is_immutable_across_lifespan).
narrative_ontology:cs_axiom_status(biological_sex_is_immutable_across_lifespan, holdable).
narrative_ontology:cs_axiom_grounding('6e72237e-ccab-4377-a981-950c03f5f824', biological_sex_is_immutable_across_lifespan, empirically_contingent).
narrative_ontology:cs_axiom('6e72237e-ccab-4377-a981-950c03f5f824', secondary, binary_sex_categories_reflect_natural_biological_division).
narrative_ontology:cs_axiom_status(binary_sex_categories_reflect_natural_biological_division, overridden).
narrative_ontology:cs_axiom_grounding('6e72237e-ccab-4377-a981-950c03f5f824', binary_sex_categories_reflect_natural_biological_division, empirically_contingent).
narrative_ontology:cs_reference_frame('6e72237e-ccab-4377-a981-950c03f5f824', biological_sex_natural_taxonomy).
narrative_ontology:cs_drift_state('6e72237e-ccab-4377-a981-950c03f5f824', contemporary_trans_visibility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e72237e-ccab-4377-a981-950c03f5f824', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, institutional_boundary_maintainers).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_nonconforming_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, feminist_boundary_defenders).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned as the primary constituency protected by sex-segregated spaces and categorical boundaries grounded in biological markers. Receive legal recognition, facility access, and social validation of their category membership without requiring disclosure or proof beyond natal assignment. Also carry costs: surveillance of bodily markers, medicalization of reproduction, mandatory participation in essentialist framing of womanhood. Identity as woman is constructed through this biological reading but they cannot exit without rejecting their entire social positioning.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women, beneficiary,
    organized, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cis_women, payer).

% Structurally excluded from the 'woman' category under this reading because natal sex assignment does not match current gender identity. Barred from sex-segregated spaces (bathrooms, shelters, prisons, sports), legal documentation, and institutional recognition. No viable exit: remaining closeted carries psychological/social costs; transition is materially obstructed by enforcement; moving to a jurisdiction with a different reading still leaves global enforcement machinery operational. Subjected to high surveillance, family rejection, employment discrimination, and violence. The constraint directly suppresses their category identity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, trapped, universal).

% Do not fit either pole of the binary category system grounded in biological markers. Face coercion to perform one of the two categories and documentation enforcing binary assignment. No institutional recognition of their actual positioning. Trapped by both enforcement (legal identity systems) and material dependence (healthcare, shelter, employment tied to categorical assignment).
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_nonconforming_individuals, payer,
    powerless, biographical, trapped, universal).

% State bureaucracies, religious institutions, sports federations, healthcare systems, prison systems. Set and enforce the biological-markers boundary. Administer the constraint through document systems, facility policies, medical gatekeeping. Benefit from simplified categorization (reduces administrative complexity) and from the political capital generated by maintaining boundaries. Can reframe or relax the boundary, but do not face direct extraction from it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, institutional_boundary_maintainers, agenda_setter,
    institutional, generational, arbitrage, universal).

% Political movement centered on the belief that sex-based categorization is the essential foundation for understanding and remedying women's oppression. Frame category maintenance as protective of cis women's interests and read trans women's inclusion as category dilution that erases sex-based analysis. Mobilize cis women to defend the biological boundary. Are not directly enforcing the constraint but supply the ideological framework that justifies institutional enforcement.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, feminist_boundary_defenders, beneficiary,
    organized, generational, mobile, universal).

% Political movement contending that gender identity, not natal sex assignment, determines category membership. Advocate for trans women's inclusion, institutional recognition of gender identity, and de-coupling of categories from biological markers. Excluded from the institutional decision-making that maintains the biological boundary but actively resist it through litigation, legislative advocacy, and norm-setting in specific jurisdictions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, universal).

% External vantage point: documents the structural relationships, traces the enforcement machinery, measures the extractiveness from each seated position. Does not participate in the constraint's reproduction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, institutional_boundary_maintainers).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Biological-marker-based categorization solves a coordination problem: creating a shared vocabulary for sex-differentiated social practices (reproduction-related healthcare, sex-segregated safety spaces, data collection on sex-based oppression). Under this reading, the coordination function requires that the boundary track immutable biological markers at birth to remain stable and administratively clear.
% TRANSFER_FUNCTION: Moves social recognition, legal status, facility access, and bodily autonomy FROM trans women and gender-nonconforming individuals TO cis women and institutional boundary-maintainers. The mechanism: enforced exclusion from the 'woman' category and its attendant protections, combined with mandatory participation in a binary system that offers no third option.
% ABSENT_VOICES: Trans women and gender-nonconforming individuals are structurally excluded from the boundary-setting process. They would testify that the 'immutable biological marker' frame forecloses their existence and that including gender identity produces a more defensible coordination logic. Additionally excluded: people with DSDs (differences of sex development), intersex individuals whose natal markers do not fit the binary, and those whose gender identity was assigned but never matched subjective sense of self — these populations demonstrate that 'immutability' and 'binary biology' are themselves constructs, not natural facts.
% DISAPPEARANCE_RATIONALE: If the biological-sex-reading constraint and its institutional enforcement disappeared, the category 'woman' would immediately reorganize around alternative readings (gender identity, social role, or a hybrid frame). Sex-segregated spaces would renegotiate their boundaries. Legal systems would shift documentation practices. The sex-based data necessary for tracking reproductive-health disparities would reorganize around different collection categories. Cis women's institutional positioning would shift; institutional boundary-maintainers would face different legitimacy demands. The constraint's disappearance would be massively generative.
% FOUNDING_PROBLEM: Developed nations built sex-segregated social infrastructure (bathrooms, prisons, shelters, sports, military units, medical specialties) based on anatomical and reproductive differentiation. The problem the biological-sex reading was built to solve: how to maintain these separations without constant verification of every individual's anatomy. Answer: codify sex assignment at birth as the stable marker that moves through all downstream systems.
% FOUNDING_PROBLEM_CORROBORATION: Institutional maintenance structures (prison administrations, sports federations, healthcare systems) attest the founding problem remains live: they cite ongoing need for sex-segregated facilities and single-sex documentation. Trans women's advocates contest that the original problem (providing safe, dignified facilities and healthcare) is solved by identity-based boundaries and that the biological-marker reading now functions to exclude rather than include. Independent research on trans women's healthcare outcomes and safety in sex-segregated spaces (published in medical journals and ethics literature outside any single benefiting party) shows the founding problem statement is contestable: facilities organized around gender identity have solved the founding problem without requiring biological markers.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the biological-sex reading systematically excludes trans women and gender-nonconforming individuals from institutional recognition, legal documentation, facility access, and category membership itself. The constraint transfers their social existence into a residual category or denial. Suppression is higher still (0.89) because enforcement requires constant vigilance: monitoring who uses which facilities, verifying identity documents, preventing institutional boundary erosion. The temporal trajectory shows rising suppression and extractiveness from interval start to midpoint (t=0 to t=25), then stabilizing — this reflects the period when institutional frameworks (legal systems, sports federations, healthcare protocols) actively hardened biological-sex enforcement in response to gender-identity advocacy pressure. Theater rises to 0.42 but plateaus: the coordination function (safe spaces, data collection) is genuine, but an increasing share of institutional energy goes to boundary maintenance rather than to the original coordination problem. The coercion_grid differentiates levels: individual trans women face the highest suppression (0.87 at t=50); organizational bodies (sports bodies, medical gatekeepers) implement it; class-level cis women benefit from it; structural systems (law, bureaucracy, documentation) architect and amplify it. Class-level resistance is lower than individual (0.73 vs 0.71 at t=50) because organized cis women are invested in boundary maintenance, and trans women as a class lack the structural power to mount coordinated resistance.
 *
 * PERSPECTIVAL GAP:
 *   The biological-sex reading produces radically different constraint experiences across seats. From cis women's positioned perspective (some beneficiary, some payer), the constraint is protective — it creates shared spaces and a political vocabulary for sex-based oppression. From trans women's perspective (trapped payer), the constraint is violent and identity-annihilating. From institutional administrations' perspective, the constraint is a coordination solution (one marker moves through all systems). These divergences emerge from structural asymmetries in power, exit options, and identity-lock, not from different factual beliefs about biology. The constraint itself manufactures disagreement: by forcing all three seats to organize around the same biological-marker boundary, it guarantees that any change to the boundary will appear to one seat as liberation and to another as erasure. This is the structure of a tangled_rope: genuine coordination function (safe spaces, shared vocabulary) bound to asymmetric extraction (trans women's exclusion). The engine should compute this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and gender-nonconforming individuals sit at the target end (d near 1.0): they are trapped by the constraint, subjected to identity suppression, excluded from beneficiary protections, and face escalating enforcement costs. Cis women sit at a hybrid position (d near 0.5): they benefit from the category protection and safe spaces, but are also caught in the essentialist framing — forced to ground their identity in immutable biology rather than in lived experience or self-understanding. The institutional boundary-maintainers sit at the beneficiary end (d near 0.0): they experience the constraint as reducing administrative complexity. Feminist boundary-defenders sit mobile (d moderate): they mobilize the constraint's frame but are not directly enforced by it. The gender_identity_advocates are excluded, so d is undefined for the constraint itself, though they would experience it as targeting their core constituency. This directionality distribution is structurally asymmetric: power is concentrated in institutional hands; extraction flows from the powerless (trans women) to the organized (institutional bodies, boundary-defending feminism). The engine computes this from the beneficiary/victim + exit + power declarations; I have not pre-tuned it.
 *
 * MANDATROPHY ANALYSIS:
 *   The biological-sex reading faces a mandatrophy condition: the founding problem (maintaining safe, segregated infrastructure) is solved by identity-based boundaries just as effectively as by biological markers. In multiple jurisdictions where identity-based documentation replaced biological markers, sex-segregated spaces continued functioning; safety outcomes did not deteriorate; data collection on sex-based oppression adapted. Yet the biological-sex reading persists, increasingly defended through explicit rejection of the identity-based alternative. The founding problem is dead (alternative institutional forms exist and work) but the mandate persists. This is exactly what mandatrophy detection should flag: a constraint whose original function is superseded but whose enforcement machinery hardens. The theater_ratio rise (from 0.28 to 0.42 across the interval) is diagnostic: more institutional energy goes to defending the boundary than to the actual coordination problem. The constraint has not been abandoned; it has become performative — its role is now defending a particular definition of womanhood against an alternative, not solving the original facility-safety problem. This is a true mandatrophy case: the constraint should face remand to decision-makers on whether the real coordination function (safe, dignified facilities and healthcare) would be better served by a different boundary definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_construction,
    'Is the biological sex distinction (XX/XY chromosomes, reproductive anatomy, sex assignment) a natural fact or a constructed categorization scheme layered onto biological variation?',
    'Review of biological literature on sexual development (DSDs, intersex conditions, androgen sensitivity, gonadal dysgenesis): if biological categories do not naturally cluster into two discrete groups, the binary is a construction. Examine how different societies and historical periods have categorized sex — if no universal pattern emerges, construction is likely.',
    'If biological sex is a construction, then the reading''s grounding claim (category membership follows immutable biological markers) loses its natural-law foundation. The constraint reclassifies from mountain to tangled_rope or snare depending on extraction and beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_vs_construction, empirical, 'Whether binary biological sex is natural or constructed categorization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (external barriers: legal systems, facility gatekeeping, family rejection) or internalized (trans women have adopted the society''s message that they are not really women, delay transition from belief rather than barrier)?',
    'Post-exit suppression trajectory: if trans women who fully transition to a jurisdiction with identity-based documentation and affirming institutions still carry suppression-like behaviors (self-doubt, hesitation, internal checking against the biological-marker frame), the suppression is partially internalized and will persist after structural barriers are removed.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests — it has colonized the target''s self-understanding. Remedying the constraint requires not just removing legal barriers but addressing internalized oppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the biological-sex reading logically foreclose the gender-identity reading within a single coherent framework, or can the two readings coexist by appealing to different contexts, constituencies, or underlying principles?',
    'Test whether any party can coherently hold both readings: for instance, ''biological sex determines category membership for reproductive healthcare purposes, but gender identity determines social recognition and facility access.'' If coherent hybrid frameworks exist, the readings coexist; if holding both produces logical contradiction, foreclosure is likely.',
    'If foreclosure is real (the readings are incompatible in any single framework), the constraint and its sibling are competitors for institutional legitimacy, not alternative framings of the same situation. Classification changes from tangled_rope to snare-grade extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether biological-sex and gender-identity readings are logically compatible in a single framework.').

omega_variable(
    mandate_obsolescence,
    'Is the founding problem (safe, segregated facilities and data collection on sex-based oppression) genuinely still live, or has it been superseded by alternative institutional forms that solve it equally well under different boundaries?',
    'Comparative institutional analysis: documentation and safety outcomes in jurisdictions that use gender-identity boundaries vs. biological-sex boundaries. Measure: facility safety incidents, facility usage patterns, data quality on sex-disparities in health/labor. If outcomes are equivalent or identical, the founding problem is dead.',
    'If the founding problem is dead, the constraint satisfies the mandatrophy condition: it persists in defending a particular boundary definition despite the original problem being solvable under alternative definitions. Flags for legislative/administrative remand.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the founding problem remains live or has been superseded.').

omega_variable(
    identity_lock_escape_paths,
    'Are there material exits for trans women and gender-nonconforming individuals, or is the constraint truly an identity-locked trap with no viable exit path?',
    'Lifecycle tracking: count trans women who remain in the jurisdiction, transition despite costs, migrate to jurisdictions with alternative readings, or remain closeted. Measure material barriers to each path: cost of migration, family support systems, employment availability post-transition, legal recognition speed. If any path remains open, exit is not fully trapped; if all paths are materially obstructed, identity-lock is confirmed.',
    'If identity-lock is confirmed (exit is trapped), the constraint''s directionality for trans women computes near d=1.0 (full target), raising effective extraction. Classification remains tangled_rope but with different d-distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_escape_paths, empirical, 'Whether trans women and gender-nonconforming individuals have viable exit paths or are identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__biological_sex_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__biological_sex_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__biological_sex_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(gend_tr_t35, gendered_category_membership__biological_sex_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(gend_tr_t50, gendered_category_membership__biological_sex_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__biological_sex_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__biological_sex_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__biological_sex_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(gend_be_t35, gendered_category_membership__biological_sex_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement(gend_be_t50, gendered_category_membership__biological_sex_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__biological_sex_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__biological_sex_reading, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__biological_sex_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(gend_su_t35, gendered_category_membership__biological_sex_reading, suppression_requirement, 35, 0.88).
narrative_ontology:measurement(gend_su_t50, gendered_category_membership__biological_sex_reading, suppression_requirement, 50, 0.89).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(gend_grid_01, gendered_category_membership__biological_sex_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(gend_grid_02, gendered_category_membership__biological_sex_reading, accessibility_collapse(class), 50, 0.79).
narrative_ontology:measurement(gend_grid_03, gendered_category_membership__biological_sex_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(gend_grid_04, gendered_category_membership__biological_sex_reading, accessibility_collapse(individual), 50, 0.81).
narrative_ontology:measurement(gend_grid_05, gendered_category_membership__biological_sex_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(gend_grid_06, gendered_category_membership__biological_sex_reading, accessibility_collapse(organizational), 50, 0.84).
narrative_ontology:measurement(gend_grid_07, gendered_category_membership__biological_sex_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(gend_grid_08, gendered_category_membership__biological_sex_reading, accessibility_collapse(structural), 50, 0.88).
narrative_ontology:measurement(gend_grid_09, gendered_category_membership__biological_sex_reading, resistance(class), 0, 0.61).
narrative_ontology:measurement(gend_grid_10, gendered_category_membership__biological_sex_reading, resistance(class), 50, 0.73).
narrative_ontology:measurement(gend_grid_11, gendered_category_membership__biological_sex_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(gend_grid_12, gendered_category_membership__biological_sex_reading, resistance(individual), 50, 0.71).
narrative_ontology:measurement(gend_grid_13, gendered_category_membership__biological_sex_reading, resistance(organizational), 0, 0.64).
narrative_ontology:measurement(gend_grid_14, gendered_category_membership__biological_sex_reading, resistance(organizational), 50, 0.76).
narrative_ontology:measurement(gend_grid_15, gendered_category_membership__biological_sex_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(gend_grid_16, gendered_category_membership__biological_sex_reading, resistance(structural), 50, 0.81).
narrative_ontology:measurement(gend_grid_17, gendered_category_membership__biological_sex_reading, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(gend_grid_18, gendered_category_membership__biological_sex_reading, stakes_inflation(class), 50, 0.74).
narrative_ontology:measurement(gend_grid_19, gendered_category_membership__biological_sex_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(gend_grid_20, gendered_category_membership__biological_sex_reading, stakes_inflation(individual), 50, 0.79).
narrative_ontology:measurement(gend_grid_21, gendered_category_membership__biological_sex_reading, stakes_inflation(organizational), 0, 0.64).
narrative_ontology:measurement(gend_grid_22, gendered_category_membership__biological_sex_reading, stakes_inflation(organizational), 50, 0.76).
narrative_ontology:measurement(gend_grid_23, gendered_category_membership__biological_sex_reading, stakes_inflation(structural), 0, 0.69).
narrative_ontology:measurement(gend_grid_24, gendered_category_membership__biological_sex_reading, stakes_inflation(structural), 50, 0.82).
narrative_ontology:measurement(gend_grid_25, gendered_category_membership__biological_sex_reading, suppression(class), 0, 0.65).
narrative_ontology:measurement(gend_grid_26, gendered_category_membership__biological_sex_reading, suppression(class), 50, 0.84).
narrative_ontology:measurement(gend_grid_27, gendered_category_membership__biological_sex_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(gend_grid_28, gendered_category_membership__biological_sex_reading, suppression(individual), 50, 0.87).
narrative_ontology:measurement(gend_grid_29, gendered_category_membership__biological_sex_reading, suppression(organizational), 0, 0.71).
narrative_ontology:measurement(gend_grid_30, gendered_category_membership__biological_sex_reading, suppression(organizational), 50, 0.89).
narrative_ontology:measurement(gend_grid_31, gendered_category_membership__biological_sex_reading, suppression(structural), 0, 0.76).
narrative_ontology:measurement(gend_grid_32, gendered_category_membership__biological_sex_reading, suppression(structural), 50, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, sex_segregated_facility_access__institutional_policy).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, reproductive_category_medical_gatekeeping).

% DUAL FORMULATION NOTE:
% Part of a constraint family decomposing the contested kernel 'gendered_category_membership' into three readings, each a distinct constraint with distinct ε, beneficiary/victim structures, and implementation mechanics. The biological_sex_reading (this story) excludes trans women and gender-nonconforming individuals from the category 'woman'. The gender_identity_reading (sibling constraint) includes trans women on grounds of subjective identity and self-declaration. The social_role_reading (third constraint) grounds membership in sustained social performance and community recognition. These three constraints are not alternatives to be evaluated and chosen — they coexist as live institutional framings held by different parties, though each party experiences the contest as existential. The engine should compute per-seat classifications showing that the same biological-sex constraint measures as mountain from a trans-exclusion-supporting seat but as snare from a trans-woman's seat, precisely because the readings are incompatible at the committer level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
