% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Boundary via Reproductive Biology
 *   domain: social/legal/ontological
 *
 * SUMMARY:
 *   This constraint instantiates the BIOLOGY READING of the
 *   sex_gender_category kernel. Under this reading, sex category membership
 *   is determined exclusively by chromosomal and anatomical status at birth
 *   (reproductive biology), treating it as immutable and objective. Trans
 *   women are classified as male despite gender identity and transition;
 *   intersex individuals are forced into a binary category despite physical
 *   variation. The constraint is presented as natural (reproductive biology
 *   is objective) and as necessary (legal sex-based protections depend on a
 *   stable category). This reading competes with the identity_reading
 *   (membership determined by gender identity) and the hybrid_reading
 *   (membership determined by combination of biology and documented medical
 *   transition). The engine will compute per-seat types from the structural
 *   data; the authored claim and metrics are independent—they may diverge,
 *   and that divergence is the measurement.
 *
 * KEY AGENTS:
 *   - cis_women_as_category_maintainers: Organize and maintain the category boundary; beneficiaries of category coherence for collective sex-based organizing and legal protections.
 *   - trans_women: Classified as male; bear costs of non-recognition, exclusion from women-only spaces, identity-locking to a classification they reject.
 *   - intersex_individuals: Forced into binary categories despite physical non-binary variation; bear costs of forced assignment and medical coercion.
 *   - sex_discrimination_law_jurisdictions: Benefit from stable biological boundary for statutory language; constrained by institutional need for legal clarity.
 *   - identity_reading_advocates: Excluded from conversations where this constraint is set and maintained; would reframe category entirely.
 *   - hybrid_reading_supporters: Propose softening the boundary; occupy some institutional positions but are not primary enforcers.
 *   - sports_governing_bodies: Active enforcers of the constraint via eligibility rules; increasingly contested.
 *   - analytical_observer: Maps the structural relationships across all three readings and distinguishes their ε values.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.71).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Boundary via Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/ontological").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '7a4f917a-5f24-497b-be66-b0caa5c17448').
narrative_ontology:cs_kernel_codification('7a4f917a-5f24-497b-be66-b0caa5c17448', formalized).
narrative_ontology:cs_authority_grounding('7a4f917a-5f24-497b-be66-b0caa5c17448', extraction).
narrative_ontology:cs_interpretation_layer_present('7a4f917a-5f24-497b-be66-b0caa5c17448').
narrative_ontology:cs_reading_relation('7a4f917a-5f24-497b-be66-b0caa5c17448', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('7a4f917a-5f24-497b-be66-b0caa5c17448', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7a4f917a-5f24-497b-be66-b0caa5c17448', foundational, reproductive_biology_determines_category).
narrative_ontology:cs_axiom_status(reproductive_biology_determines_category, holdable).
narrative_ontology:cs_axiom_grounding('7a4f917a-5f24-497b-be66-b0caa5c17448', reproductive_biology_determines_category, empirically_contingent).
narrative_ontology:cs_axiom('7a4f917a-5f24-497b-be66-b0caa5c17448', foundational, sex_category_must_be_immutable).
narrative_ontology:cs_axiom_status(sex_category_must_be_immutable, holdable).
narrative_ontology:cs_axiom_grounding('7a4f917a-5f24-497b-be66-b0caa5c17448', sex_category_must_be_immutable, deontological).
narrative_ontology:cs_reference_frame('7a4f917a-5f24-497b-be66-b0caa5c17448', reproductive_biology_immutable_boundary).
narrative_ontology:cs_drift_state('7a4f917a-5f24-497b-be66-b0caa5c17448', contemporary_trans_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7a4f917a-5f24-497b-be66-b0caa5c17448', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_as_category_maintainers).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sex_discrimination_law_jurisdictions).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, feminist_scholarship_on_sex_harms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain sex category membership as exclusively tied to chromosomal/anatomical status at birth. Benefit from category coherence as a basis for identifying and addressing sex-based harms, legal protections (reproductive rights, discrimination law), and collective organizing around shared sex-based interests. Enforce the boundary by requiring biological documentation for legal sex recognition, excluding trans women from women-only spaces and women-specific athletics, and resisting institutional redefinition. Argue that decoupling sex from reproductive biology would dissolve the legal and social category necessary to protect against sex-specific oppression.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_as_category_maintainers, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, cis_women_as_category_maintainers, agenda_setter).

% Classified as male under this reading despite gender identity, medical transition, and social transition. Bear costs: legal sex-marker non-recognition in most jurisdictions, exclusion from women-only spaces and sports, excluded from single-sex spousal and parental rights in many legal systems, categorized with males for prison assignment despite feminist objections to this outcome. Exit is severely constrained—changing legal sex requires biological documentation they cannot satisfy; the only 'exit' is negating their identity, which contradicts their lived reality. Identity is locked to a self-understanding that contradicts the classification.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Face forced binary sex classification despite physical variation (ambiguous genitalia, mixed/mosaic gonads, hormonal variation) that does not map cleanly to binary reproductive categories. Undergo medical assignment (often in infancy without consent), legal assignment to match, and enforcement pressure to perform the assigned sex. Bear costs of unwanted surgery, legal misclassification, inability to reflect actual biology in legal status. Exit options are minimal—the constraint's definition of reproductive biology does not accommodate variation; recognition of intersex as a category would directly dissolve this reading's classification scheme.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Enact and enforce legal sex-based protections using 'sex' defined as reproductive biology. Benefit from the category's clarity for statutory language across hundreds of laws (Title IX, reproductive rights, discrimination statutes, rape law). Changing the boundary would require rewriting statutes across multiple areas. Constrained by pressure to recognize trans women's legal sex and by emerging international human-rights norms favoring recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sex_discrimination_law_jurisdictions, beneficiary,
    institutional, generational, constrained, national).

% Advocate for sex category membership determined by gender identity rather than reproductive biology. Structurally excluded from spaces where this constraint is debated and maintained (women-only organizing spaces, feminist scholarship). Would argue for decoupling sex category from reproductive biology, restructuring legal protections around harm-based or identity-based groups rather than reproductive categories. Their exclusion from the conversation is the enforcement object itself.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, identity_reading_advocates, excluded,
    organized, generational, mobile, global).

% Propose compromise frameworks (sex reassignment surgery + time period, medical gatekeeping) that would allow some trans women into the 'woman' category while preserving a bounded category. Occupy some institutional positions (medical authorities, legal doctrine) but are not primary decision-makers in enforcing this constraint. Their proposals would soften the biological boundary.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, hybrid_reading_supporters, excluded,
    organized, generational, mobile, national).

% Academic and activist fields that explain sex-based oppression (rape, reproductive coercion, menstruation-based discrimination) within frameworks treating 'sex' as a unified category rooted in reproductive biology. The constraint provides coherence to their analysis—decades of theoretical work depends on treating sex as a category rooted in reproductive fact. They benefit from the category's stability and from the institutional authority it grants their interpretation of sex-based harm.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, feminist_scholarship_on_sex_harms, beneficiary,
    institutional, generational, constrained, global).

% Enforce sex-based athletic categories using reproductive-biology definition. Argue athletic fairness (hormonal advantage, skeletal density effects) requires mapping athletic divisions to reproductive biology. Active enforcers via eligibility rules; increasingly contested by trans athletes seeking inclusion, feminist athletes defending single-sex sports from trans inclusion, and some jurisdictions mandating trans inclusion via law.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Maps the structural relationships between the three readings, distinguishes the constraints they instantiate, and examines how the boundary's enforcement impacts victims under each reading. Takes no position on which reading is correct; measures the cost structure each produces and how per-seat classifications diverge across the competing framings.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, cis_women_as_category_maintainers).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable, legally recognizable category for identification of sex-based harms, protective legislation (reproductive rights, workplace discrimination, Title IX), and collective organizing by people sharing vulnerability to sex-based oppression rooted in reproductive function.
% TRANSFER_FUNCTION: Moves social legitimacy and legal recognition FROM trans women and intersex individuals TO cis women, legal institutions, and feminist scholarship: trans women are denied recognition as women despite transition; intersex individuals are forced into binary classifications; the category coherence, legal recognition, spaces, and analytical authority saved by enforcing the boundary accrue to cis women and to institutions that depend on biological-sex categories for statutory law and harm analysis.
% ABSENT_VOICES: Identity-reading advocates are excluded from conversations that maintain this constraint (women-only feminist organizing spaces, women's studies departments, legal doctrine committees focused on sex-based protections). Hybrid-reading proponents occupy some institutional positions but are not primary enforcers. Trans women and intersex individuals are present in some conversations but systematically excluded from consensus-setting conversations where the boundary is defended.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if sex category membership were decoupled from reproductive biology—legal and social frameworks would immediately reorganize. Sex-discrimination law would require rewriting across hundreds of statutes to clarify what 'sex' means (harm-based? identity-based? biology-based?). Feminist organizing would splinter into different strategies depending on the new category foundation. Sports would rewrite eligibility. Intersex individuals would gain legal recognition outside a forced binary. The constraint is not natural—it is institutional choice maintained by active enforcement.
% FOUNDING_PROBLEM: Historical problem: people with reproductive systems are subjected to sex-specific harms (rape, forced pregnancy, reproductive coercion, menstruation-based discrimination and exclusion) rooted in their reproductive biology. Legal and social systems needed a category to identify who faces these harms and offer protective law and collective action. Reproductive biology provided a historically stable proxy for 'who is subject to sex-based oppression.'
% FOUNDING_PROBLEM_CORROBORATION: Cis women, feminist scholars, and reproductive-rights advocates attest the problem is live and biological category is necessary. Trans rights advocates, intersex activists, and some feminist theorists attest the founding problem (sex-based harm) is real but the biological boundary is overly broad (trans women experience gender-based harms not rooted in reproductive biology) and underly broad (intersex people face pregnancy-based harms but are misclassified if assigned male at birth). International human-rights bodies (ECHR, UN Human Rights Committee) have begun recognizing trans women's legal sex and intersex persons' right to non-binary recognition, fragmenting institutional consensus away from this reading.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).

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
 *   Extraction is high (0.68) because the constraint imposes status costs on trans women and intersex individuals that cis women and legal systems benefit from—the category boundary exists to exclude and classify them differently. Suppression is comparably high (0.71) because enforcement of the boundary requires active gatekeeping: biological documentation, exclusion from spaces, legal recognition denial, and institutional pressure to conform to assignment. Theater is moderate-low (0.42) because the constraint has a real coordination function (legal sex-based protections), but enforcement activity increasingly focuses on boundary maintenance rather than the underlying sex-based harms the category was built to address. The measurement series shows extraction and theater rising over the 40-unit interval (contemporary moment is t=30, showing rising theater as identity recognition increases elsewhere and enforcement escalates in response) while suppression plateaus—suggesting the constraint is shifting from genuine coordination function to increasingly performative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The analysis diverges sharply across stakeholder seats. From cis women's organized position, the constraint is genuine coordination (protecting sex-based interests) that requires active enforcement against forces that would dilute the category. From trans women's powerless position, the same structure is enforced exclusion from a category that accurately describes their social position. From intersex individuals' trapped position, it is forced binary assignment that contradicts their biology. The engine computes per-seat classifications from power, exit, beneficiary/victim declarations, and suppression/extraction/theater; where those computations diverge from the claimed tangled_rope, that divergence identifies which seats experience the constraint differently—a key measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women as agenda-setters and beneficiaries have low d (beneficiaries → d near 0.0): they set and maintain the boundary; they benefit from category coherence for legal protections and collective organizing; they have high institutional and organized power; exit is identity-locked but in the direction their identity takes anyway (cis identity aligns with the category). Trans women have high d (victims/payers, powerless, identity-locked in the opposite direction → d near 1.0): they pay the cost of non-recognition; they cannot exit by adopting the constraint's terms without negating identity; they are powerless to change the boundary. Intersex individuals have even higher d (victims/payers, powerless, trapped → d approaches 1.0): they have no exit that lets them exit—the constraint's definition admits no variation. Sex discrimination law jurisdictions benefit institutionally but are constrained (d near 0.3-0.4: they collect legal clarity but are increasingly pressured to recognize other categories). Sports governing bodies are agenda-setters but increasingly contested; their d sits moderate (0.4-0.5) because while they benefit from the clarity, they no longer have consensus authority to maintain the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sex-based harms rooted in reproductive biology; need for legal category to protect) was real and live. Founding problem status is now contested: cis women and law argue the problem is still live and the biological boundary is necessary; trans rights advocates argue the founding problem is real but the boundary is over-broad (excludes those who experience gender-based but not strictly reproductive harms) and under-broad (intersex people face pregnancy-based harms but are classified male if assigned male at birth). Mandatrophy is unresolved. The theater ratio rising while extraction plateaus suggests some performance is entering the enforcement logic—defending the boundary is increasingly central and the underlying sex-based harm protection is increasingly assumed background. This is consistent with late-stage mandatrophy drift where the function persists but its enforcement becomes spectacle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_as_natural_or_constructed,
    'Is the reproductive-biology boundary a natural fact about sex, or a constructed social choice that naturalizes itself by calling biology ''immutable''?',
    'Philosophical analysis: examine whether ''biological sex'' is a pre-social fact or a socially constructed category imposed on biological variation. Empirical: document the history of biology-based sex categorization in law and medicine—has the boundary definition been stable or has it shifted with social context?',
    'If ''biology'' is constructed, the constraint''s claim to naturalness is false and it should be reclassified as snare (dressed-up extraction). If ''biology'' is truly natural, the constraint may hold as tangled_rope (genuine coordination on which extraction rides).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biology_as_natural_or_constructed, conceptual, 'Whether reproductive biology is a natural fact or a social category.').

omega_variable(
    sex_based_harms_boundary,
    'Which harms are properly ''sex-based'' (rooted in reproductive biology) vs. ''gender-based'' (rooted in gender non-conformity or identity transition)? Can the founding problem be addressed by a more granular categorization than binary sex?',
    'Empirical: survey which people experience which harms and whether reproductive biology or gender identity better predicts vulnerability. Institutional: observe whether legal protections could be rewritten to protect ''people vulnerable to reproductive coercion'' rather than ''women,'' etc., and whether that redefinition changes outcomes.',
    'If gender-based harms are substantial and poorly-targeted by reproductive-biology categories, the constraint is over-broad and should be decomposed into multiple constraints for different harm categories. If sex-based harms are strongly correlated with reproductive biology alone, the constraint''s boundary is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_harms_boundary, empirical, 'Whether reproductive biology or gender identity better predicts vulnerability to sex/gender-based harms.').

omega_variable(
    intersex_category_dissolution,
    'Does recognizing intersex as a distinct category (not forced into binary) dissolve the reproductive-biology reading entirely, or can a biology reading accommodate non-binary biology?',
    'Formal: can the category be extended to ''reproductive system variation including intersex'' rather than forced binary? Empirical: document intersex individuals'' sex-based harm exposure and whether it is better addressed by sex-binary categories or by recognizing intersex category.',
    'If the answer is ''category can accommodate intersex biology,'' the constraint softens but persists; if ''recognizing intersex means the binary itself is invalid,'' the constraint is fundamentally undermined and must reclassify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_category_dissolution, empirical, 'Whether the reproductive-biology category can accommodate non-binary biological variation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of trans women''s category membership claims primarily structural (legal systems, institutional gatekeeping, physical exclusion) or partly internalized (trans women internalize the classification and suppress their own claims)?',
    'Post-transition context analysis: in jurisdictions where trans women''s legal sex is recognized, do trans women''s suppression and costs drop? If they drop significantly, suppression was structural; if they persist, suppression is partly internalized.',
    'If suppression is largely structural, reclassification could reduce it sharply; if partly internalized, reclassification alone will not address the cost—redress requires identity reintegration work beyond law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of trans women''s membership claims is structural or internalized.').

omega_variable(
    kitchen_table_feminism_vs_institutional_feminism,
    'Does sex-based protecting law (reproductive rights, Title IX, etc.) genuinely depend on a biological-sex category boundary, or can it be rewritten to protect harm-based groups that cut across sex categories?',
    'Institutional experiment: document jurisdictions that have rewritten sex-discrimination law using harm-based language (people vulnerable to pregnancy-based discrimination, menstruation-based exclusion) rather than sex categories. Measure whether protections persist and whether trans and intersex people are better-protected.',
    'If law can be rewritten without weakening sex-based protections, the constraint''s coordination function is reducible and the extraction component is separable—supporting decomposition into multiple constraints. If rewriting weakens protections, the biological category is genuinely necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kitchen_table_feminism_vs_institutional_feminism, empirical, 'Whether sex-discrimination law requires biological-sex categories or can be rewritten for harm-based protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__biology_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(sex__tr_t5, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__biology_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(sex__tr_t15, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__biology_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(sex__tr_t25, observed).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__biology_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(sex__tr_t30, observed).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(sex__tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__biology_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(sex__be_t5, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__biology_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(sex__be_t15, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__biology_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(sex__be_t25, observed).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__biology_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sex__be_t30, observed).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sex__be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__biology_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(sex__su_t5, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(sex__su_t15, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__biology_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(sex__su_t25, observed).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__biology_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(sex__su_t30, observed).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(sex__su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel instantiates three structurally distinct constraints: biology_reading (this story), identity_reading, and hybrid_reading. They share the same kernel (what makes someone a 'woman'?) but instantiate different ε values, beneficiary/victim structures, and classification types. This reading assumes membership is determined by immutable reproductive biology; the identity_reading assumes membership is determined by subjective gender identity; the hybrid_reading assumes membership is determined by combination with medical gatekeeping. The three readings are in direct contest for institutional authority. Each reading is a separate constraint story with its own ε-invariant classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__biology_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
