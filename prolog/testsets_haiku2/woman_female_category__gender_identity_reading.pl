% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity Reading: Woman/Female Category Membership
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story represents the gender-identity reading of the
 *   contested kernel 'woman/female category membership.' The reading asserts
 *   that category membership is determined by internal self-identification
 *   with the gender category, independent of chromosomal sex, reproductive
 *   anatomy, or developmental biology. Under this reading, a trans woman who
 *   identifies as a woman qualifies for category membership, with
 *   corresponding access to spaces, protections, and recognitions designated
 *   'for women/females.' The reading generates high extraction (0.68) because
 *   it forecloses competing institutional frameworks (the sex-biology
 *   reading, the hybrid-contextual reading) and concentrates epistemic
 *   authority in identity-based criteria, imposing costs on those who hold
 *   alternative framings or rely on sex-based institutional logic. This is
 *   deliberately one reading of a kernel contest, not a neutral description
 *   of 'the facts about sex and gender.'
 *
 * KEY AGENTS:
 *   - transgender_individuals_seeking_identity_recognition: Primary beneficiaries (power=organized); seek recognition and access
 *   - sex_essentialism_advocates: Primary payers (power=organized); bear epistemic closure and institutional reorientation costs
 *   - women_in_sex_segregated_spaces: Dual-positioned (power=moderate); gain dignity inclusion, bear boundary renegotiation costs
 *   - institutional_administrators: Agenda-setters (power=institutional); enforce policy and manage contention
 *   - legal_doctrine_interpreters: Agenda-setters/observers (power=institutional); arbitrate kernel reading authority
 *   - sex_biology_reading_advocates: Excluded (power=organized, trapped); their framing is minoritized
 *   - hybrid_contextual_reading_advocates: Excluded/partially admitted (power=organized, constrained); occupy intermediate institutional ground
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.71).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity Reading: Woman/Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'e3ddb18c-50df-43f3-96f4-8ddb1d16d687').
narrative_ontology:cs_kernel_codification('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', distributed).
narrative_ontology:cs_authority_grounding('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', distributed).
narrative_ontology:cs_reading_relation('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', foundational, identity_constitutive_of_category_membership).
narrative_ontology:cs_axiom_status(identity_constitutive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', identity_constitutive_of_category_membership, deontological).
narrative_ontology:cs_axiom('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', foundational, biological_sex_not_determinative_of_gender_category).
narrative_ontology:cs_axiom_status(biological_sex_not_determinative_of_gender_category, holdable).
narrative_ontology:cs_axiom_grounding('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', biological_sex_not_determinative_of_gender_category, deontological).
narrative_ontology:cs_reference_frame('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', identity_based_gender_recognition).
narrative_ontology:cs_drift_state('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', contemporary_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3ddb18c-50df-43f3-96f4-8ddb1d16d687', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_recognition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, sex_essentialism_advocates).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, women_in_sex_segregated_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, women_in_sex_segregated_spaces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition of gender identity independent of birth sex classification. Under this reading, claim membership in woman/female category based on internal identification, with access to gendered spaces, documentation, and institutional recognition. Benefits from the constraint's recognition of self-identification over biological criteria.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_recognition, beneficiary,
    organized, biographical, mobile, national).

% Argue that biological sex is irreducible to identity and that sex-segregated spaces, categories, and protections exist to serve material reproductive interests. Under this reading, experience the constraint as foreclosing their articulation of sex-based reasoning in policy and forcing adoption of identity-centered framing. Bear costs through epistemic closure (unable to advance sex-based legal theories in mainstream discourse) and institutional reorientation.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_essentialism_advocates, payer,
    organized, generational, constrained, national).

% Access sex-segregated facilities (bathrooms, shelters, prisons, sports) justified on the basis of biological sex and reproductive vulnerability. Under this reading, experience the constraint as redefining access criteria to include trans women, which some perceive as compromising the original purpose of sex segregation (bodily privacy, protection from male-pattern violence). Simultaneously benefit from the dignity recognition extended to all who identify as women, and bear costs from altered access boundaries and negotiated space-sharing.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, women_in_sex_segregated_spaces, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, women_in_sex_segregated_spaces, beneficiary).

% Implement policies operationalizing this reading: updating documentation systems, training staff on gender-identity-based admissions to gendered spaces, adjudicating boundary disputes. Enforce the reading by revising institutional rules and responding to challenges. Bear costs in policy redesign, staff resource allocation, and ongoing contention management between incompatible stakeholder frameworks.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, institutional_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Courts, legislatures, and regulatory bodies interpret and operationalize this reading through statute, case law, and administrative guidance. Carry authority to declare which kernel reading wins in contested spaces (bathrooms, sports, prisons, legal documentation). Navigate internal doctrine: civil rights law emphasizing identity-based discrimination vs. sex-segregation doctrines grounded in biological distinction.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_doctrine_interpreters, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, legal_doctrine_interpreters, observer).

% Advance the sibling reading (sex biology reading) as the authoritative kernel interpretation. Would argue for category membership determined by biological sex markers. Excluded from mainstream institutional and legal discourse by the current dominance of the gender-identity reading; their premises are actively contested and delegitimized in some jurisdictions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_biology_reading_advocates, excluded,
    organized, generational, trapped, national).

% Advance the sibling hybrid reading: category membership contextual (biology for medical/sports/safety contexts, identity for social/legal recognition). Occupy an intermediate position; some institutional actors adopt elements of this reading (e.g. medical transition requirements for legal document change, sex-specific healthcare) while others enforce pure identity-based reading in overlapping domains.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, hybrid_contextual_reading_advocates, excluded,
    organized, generational, constrained, national).

% Monitor compliance with gender-identity recognition and sex-based equality commitments, interpreting international law to adjudicate between readings. Their interpretation influences national policy through soft law (recommendations, advisory opinions) and treaty obligation interpretation.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_recognition).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared institutional framework for recognizing gender identity independent of birth-assigned sex: enables legal documentation systems, access to gendered spaces and services, and institutional consistency in treating gender identity as the basis for sex-category membership. Solves the coordination problem of what criterion determines category membership when self-identification and biological sex diverge.
% TRANSFER_FUNCTION: Transfers epistemic authority from sex-biology-based institutional reasoning to gender-identity-based reasoning. Moves recognition/dignity benefits toward transgender individuals at the cost of foreclosing sex-essentialist frameworks in official policy discourse. Redistributes access to sex-segregated spaces from an exclusive biological-sex basis to an inclusive identity-based basis.
% ABSENT_VOICES: Sex-biology reading advocates and hybrid-contextual advocates are partially excluded or minoritized in mainstream institutional discourse. Athletes competing in sex-segregated sports under biology-based rules, prisoners held in facilities assigned by birth sex, and medical researchers studying reproductive sex differences operate under competing or subordinated framings.
% DISAPPEARANCE_RATIONALE: If this reading's enforcement vanished overnight, legal systems would revert to sex-biology criteria for category membership (under the sex_biology_reading) or adopt contextual application (under the hybrid_contextual_reading). Different jurisdictions would diverge in outcome. Trans individuals would lose legal recognition of gender identity independent of biological markers; sex-essentialist advocates would regain institutional voice; sex-segregated space access would reorganize around biological criteria or remain contested.
% FOUNDING_PROBLEM: Historical exclusion of transgender individuals from legal and social recognition of gender identity; institutional enforcement of birth-assigned sex categories; lack of institutional mechanisms for identity-based gender recognition independent of medical transition or biological change.
% FOUNDING_PROBLEM_CORROBORATION: Transgender-rights organizations, civil-rights advocates, and international human rights bodies (UN, ECHR) attest the founding problem is ongoing; medical literature documents persistent social exclusion and institutional barriers. Sex-essentialism advocates contest that this is the problem being solved, asserting instead that institutional sex-category coherence (not individual recognition) is the relevant problem.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.42 → 0.68) as the gender-identity reading gains institutional embedding: legal documentation systems shift to identity-based categories, sex-segregated space access policies incorporate identity criteria, and sex-biology-based institutional reasoning becomes increasingly marginalized in official discourse. This is not a pure coordination ratchet — the extraction lies in the foreclosure of competing epistemic frameworks and the cost imposed on actors who cannot align their institutional practice with identity-based categorization. Suppression (0.71) reflects active enforcement: challenging or operating from the sex-biology reading faces institutional sanction, reputational costs, and legal liability in some jurisdictions. Theater (0.42) is moderate because the coordination function (unified institutional framework for gender recognition) is genuine, but an increasing share of institutional energy goes to defending the boundary against contesting framings rather than serving the original problem. Resistance (0.79) is high because both sex-essentialism advocates and hybrid-contextual advocates mount sustained counter-arguments and alternative frameworks; the constraint persists because institutional power (legal doctrine, educational institutions, civil-rights authority) currently favors the gender-identity reading, not because resistance has collapsed.
 *
 * PERSPECTIVAL GAP:
 *   The gender-identity reading sees the constraint as justice (recognizing trans dignity, ending institutional exclusion); the sex-biology reading sees it as epistemic capture and institutional power consolidation (forcing sex-category incoherence, subordinating biological reasoning). From the trans-individual seat, the constraint enables recognition previously denied; from the sex-essentialist seat, the constraint compels participation in a framework they regard as false or incoherent. From the women-in-segregated-spaces seat, the constraint simultaneously extends dignity (inclusion of trans women as women) and destabilizes the material basis of sex segregation (privacy, safety). From the institutional seat, the constraint is high-friction: comply with identity criteria, train staff, defend against legal challenges from sex-essentialism advocates, manage real space-sharing disputes and contention between incompatible stakeholder frameworks. The engine computes these divergences; the authored tangled_rope claim reflects that the constraint combines genuine coordination (institutional coherence on gender recognition) with substantial asymmetric extraction (foreclosure of competing framings, costs imposed on dissenting seats).
 *
 * DIRECTIONALITY LOGIC:
 *   Trans individuals and their advocates are the structural beneficiaries (d near 0.0): they receive institutional recognition, access, legal status, and dignity that flows directly from the constraint's enforcement. Sex-essentialism advocates are structural payers (d near 1.0): they lose epistemic authority, face institutional marginalization and reputational costs for advancing sex-biology reasoning, and experience the constraint as foreclosing their frameworks. Women in sex-segregated spaces are asymmetrically positioned (d~0.5 to 0.7): they gain dignity inclusion (all who identify as women gain recognition) but bear costs when sex-segregated spaces must accommodate trans women; their exit is constrained (they cannot simply leave sex-segregated spaces as a class response). Institutional administrators are trapped in the constraint (d~0.5-0.6): they must enforce it while managing internal contradiction (some staff/users hold sex-biology framing, others hold identity framing), and their constraint-compliance is actively scrutinized by both sides. The hybrid-contextual advocates are structurally compressed: their framing offers a middle path but is actively rejected by both the gender-identity reading (for compromising trans dignity) and the sex-biology reading (for compromising sex-category coherence), giving them high d (trapped) despite their attempt to mediate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit mandatrophy characteristics. The founding problem (institutional exclusion of transgender individuals from gender-identity-based recognition) remains live — trans individuals continue to seek recognition independent of biological markers, and institutional barriers persist in many jurisdictions. The constraint's function has not atrophied. However, the theater_ratio rises (0.25 → 0.42) as the reading becomes institutionally embedded: enforcement energy increasingly goes to defending the reading's boundaries against competing framings rather than solving the original recognition problem. This is NOT yet mandatrophy (the constraint still solves the founding problem), but it is a warning trajectory — if the constraint persists while the institutional ground shifts to hybrid-contextual or sex-biology frameworks, theater would continue rising and the constraint would degrade toward Piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_boundary,
    'Is the gender-identity reading logically coercive (does adopting it require rejecting sex-biology reasoning for ALL purposes), or is it contextually limited (can identity-based category membership coexist with biology-based institutional reasoning in different domains)?',
    'Institutional practice across time: do jurisdictions adopting the gender-identity reading maintain biology-based criteria in any domains (medicine, sports, safety protocols), or does adoption drive full displacement of sex-biology reasoning?',
    'If the reading forces full displacement, it is more extractive (forecloses alternatives completely); if contextual coexistence is possible, extracted costs on sex-essentialism advocates drop and the hybrid-reading advocates gain institutional legitimacy as a live middle path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_boundary, conceptual, 'Whether the gender-identity reading permits institutional coexistence with sex-biology reasoning or logically forecloses it.').

omega_variable(
    sex_segregated_space_material_purpose,
    'What is the constitutive material basis of sex-segregated spaces: bodily privacy from sexual dimorphism, or protection from male-pattern violence and sexual harm?',
    'Empirical: do trans women in sex-segregated spaces (bathrooms, shelters, prisons) produce measurable changes in the spaces'' stated functions? Do privacy/safety outcomes shift? Do populations using those spaces report changes in experienced vulnerability?',
    'If the material basis is irreducible to identity (i.e., the vulnerability being protected against is rooted in male-typical anatomy or male-typical patterns of harm, not identity), then inclusion of trans women who retain male-typical anatomy or patterns creates genuine material conflict, raising the extracted cost on women in segregated spaces and shifting the constraint toward pure snare. If the basis is identity-pure, the constraint is coherent as a coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sex_segregated_space_material_purpose, empirical, 'Whether sex-segregated-space functions depend on biological sex or identity.').

omega_variable(
    suppression_structural_vs_normative,
    'Is the suppression (0.71) of sex-essentialism advocates structural (legal liability, institutional rejection of their reasoning, economic costs from loss of authority) or internalized (they have come to doubt their own premises)?',
    'Post-suppression: if the reading''s institutional dominance declined (e.g., hybrid-reading gains authority), would sex-essentialism advocates rapidly revive sex-biology reasoning, or has internalization locked their positioning?',
    'If structural, suppression would decline if institutional context shifted; if internalized, suppression persists regardless of context change. Internalized suppression suggests the constraint''s extraction is more entrenched (captured the values of the suppressed seat), raising the risk of mandatrophy if context shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_normative, empirical, 'Whether suppression of sex-essentialism reasoning is sustained by institutional coercion or internalized doubt.').

omega_variable(
    beneficiary_expansion_trajectory,
    'Will the beneficiary set expand over the interval to include non-trans individuals seeking recognition under identity-based criteria (e.g. non-binary individuals, gender-diverse individuals not claiming trans identity)?',
    'Institutional development: does the reading''s framing (''internal self-identification'') admit category membership for people whose identification does not track conventional woman/female categories?',
    'If beneficiaries expand, extractiveness may rise further (more seats benefit, broader institutional reorientation required) and theater may fall (the coordination function — unified identity-based framework — expands to cover more ground, becoming less theatrical). If beneficiaries remain bounded to trans women, extractiveness plateaus and the constraint stabilizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_expansion_trajectory, empirical, 'Whether the beneficiary set will expand beyond trans women seeking woman/female category membership.').

omega_variable(
    reading_kernel_contention_asymmetry,
    'Is the contention between gender-identity reading and sex-biology reading fundamentally asymmetric — do they disagree about the nature of the kernel (what counts as category membership), or do they share the kernel and only disagree about how to measure membership?',
    'Textual/doctrinal analysis: do advocates of each reading describe the same category (woman/female) or different ones? Do they share a common reference point?',
    'If asymmetric (they describe different categories), the readings are incommensurable and true foreclosure is possible — only one can govern a single category. If symmetric (shared kernel, different measurements), the readings can coexist through domain-based contextualization (hybrid-reading model).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contention_asymmetry, conceptual, 'Whether the gender-identity and sex-biology readings describe the same kernel or different phenomena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(woma_tr_t8, observed).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__gender_identity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(woma_tr_t16, observed).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__gender_identity_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(woma_tr_t24, observed).
narrative_ontology:measurement(woma_tr_t32, woman_female_category__gender_identity_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(woma_tr_t32, observed).
narrative_ontology:measurement(woma_tr_t40, woman_female_category__gender_identity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(woma_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(woma_be_t8, observed).
narrative_ontology:measurement(woma_be_t16, woman_female_category__gender_identity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(woma_be_t16, observed).
narrative_ontology:measurement(woma_be_t24, woman_female_category__gender_identity_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(woma_be_t24, observed).
narrative_ontology:measurement(woma_be_t32, woman_female_category__gender_identity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(woma_be_t32, observed).
narrative_ontology:measurement(woma_be_t40, woman_female_category__gender_identity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(woma_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t8, woman_female_category__gender_identity_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(woma_su_t8, observed).
narrative_ontology:measurement(woma_su_t16, woman_female_category__gender_identity_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(woma_su_t16, observed).
narrative_ontology:measurement(woma_su_t24, woman_female_category__gender_identity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(woma_su_t24, observed).
narrative_ontology:measurement(woma_su_t32, woman_female_category__gender_identity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(woma_su_t32, observed).
narrative_ontology:measurement(woma_su_t40, woman_female_category__gender_identity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(woma_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The woman/female category kernel decomposes into three structurally distinct constraints: (1) gender_identity_reading (this file) — category membership by self-identification independent of biology, ε=0.68, tangled_rope, high extraction from sex-essentialism advocates; (2) sex_biology_reading — category membership by chromosomal sex and anatomy, ε likely 0.15-0.25, rope or mountain, minimal extraction if properly embedded (no party forced into false categorization); (3) hybrid_contextual_reading — membership varies by institutional context (biology for medical/sports, identity for legal/social), ε likely 0.35-0.45, tangled_rope with lower asymmetric extraction than pure identity reading. Each reading has a different victim set, different beneficiary structure, and different institutional implications. They are not measurements of the same constraint — they embody different axioms about what category membership means and different distributions of epistemic authority. The three stories form a kernel family linked by affects_constraints; the contention between them is not a measurement ambiguity but a genuine institutional contest over which reading governs in particular domains and institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
