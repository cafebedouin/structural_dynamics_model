% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership by Self-Identified Gender (Gender Identity Reading)
 *   domain: social/political/bioethical
 *
 * SUMMARY:
 *   This constraint story instantiates the gender_identity_reading of the
 *   contested kernel 'gendered_category_membership'. The kernel asks what
 *   grounds membership in gender categories. This reading holds that
 *   subjective identity and self-declaration are sufficient. The resulting
 *   constraint reallocates categorical access from sex-based to
 *   identity-based criteria, generating gatekeeping costs for institutions
 *   and positioning cis women as perpetrators of exclusion when they resist.
 *   Sibling readingsâbiological_sex_reading and social_role_readingâare
 *   modeled as separate constraints per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (moderate/constrained) â gain categorical inclusion and spatial access
 *   - cis_women: Primary target/payer (organized/constrained) â bear the costs of reclassified boundaries and social censure
 *   - gender_identity_advocates: Agenda-setter and secondary beneficiary (organized/mobile) â enforce the norm and collect ideological influence
 *   - sex_segregated_service_providers: Secondary payer (institutional/constrained) â bear administrative and liability costs of implementation
 *   - gender_critical_feminists: Excluded voice (organized/constrained) â oppose the reading but are kept out of institutional policy-making
 *   - social_ontologists: Analytical observer (analytical/analytical) â tracks the ontological and distributional shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.55).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.65).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership by Self-Identified Gender (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social/political/bioethical").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e').
narrative_ontology:cs_kernel_codification('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', distributed).
narrative_ontology:cs_authority_grounding('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', practice).
narrative_ontology:cs_interpretation_layer_present('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e').
narrative_ontology:cs_reading_relation('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', foundational, self_identification_sufficient_for_membership).
narrative_ontology:cs_axiom_status(self_identification_sufficient_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', self_identification_sufficient_for_membership, deontological).
narrative_ontology:cs_axiom('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', foundational, sex_based_categorization_exclusionary_harm).
narrative_ontology:cs_axiom_status(sex_based_categorization_exclusionary_harm, holdable).
narrative_ontology:cs_axiom_grounding('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', sex_based_categorization_exclusionary_harm, deontological).
narrative_ontology:cs_reference_frame('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', self_identification_as_sufficient_criterion).
narrative_ontology:cs_drift_state('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', institutional_mainstream_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e9dd451-2b92-481d-b1c2-cc0bd1a2e92e', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sex_segregated_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain categorical inclusion as women through self-declaration, accessing legal recognition, sex-segregated spaces, and social legitimacy previously reserved for cis women. Cannot easily exit the pervasive gender category system, but benefit directly from its reconfiguration.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, constrained, national).

% Lose exclusive categorical boundaries and access to some sex-segregated spaces; face social censure and accusations of exclusion when asserting sex-based rights or requesting sex-segregated provision. Cannot exit the gender category system.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women, payer,
    organized, biographical, constrained, national).

% Set the normative and policy agenda that self-declaration governs category membership; enforce compliance through institutional advocacy, social pressure, and training; benefit from ideological vindication and expanded policy influence.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_identity_advocates, beneficiary).

% Administer intake, housing, sports, and intimate facilities according to self-declared gender; bear legal liability, operational redesign costs, and gatekeeping burdens when balancing inclusion against sex-based safety or privacy needs.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sex_segregated_service_providers, payer,
    institutional, biographical, constrained, regional).

% Assert that biological sex is immutable and that category membership and sex-segregated provision should remain sex-based; structurally excluded from progressive institutional policy-making and publicly characterized as perpetrators of exclusion.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, excluded,
    organized, generational, constrained, national).

% Analyze the ontological shift from sex-based to identity-based category membership; document the distributional effects, conceptual contests, and institutional drift between competing readings of the kernel.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, social_ontologists, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves social coordination over who belongs in gender categories and who may access sex-segregated spaces by substituting subjective self-declaration for biological or performative verification, reducing gatekeeping disputes in institutional intake and bureaucratic classification.
% TRANSFER_FUNCTION: Moves categorical membership, spatial access, and social legitimacy from cis women as the prior default occupants of the 'woman' category to anyone declaring a female gender identity; moves administrative burden, legal liability, and reputational risk to sex-segregated institutions and to cis women who resist the reclassification.
% ABSENT_VOICES: Gender-critical feminists, some medical professionals, and frontline service staff who regard biological sex as the relevant axis for certain provisions are largely excluded from progressive institutional policy-making; their objections are treated as moral failure or bigotry rather than legitimate structural concern.
% DISAPPEARANCE_RATIONALE: If self-declaration ceased to govern category membership overnight, sex-segregated spaces would revert to sex-based intake protocols, trans women would lose current categorical inclusion and access, and progressive institutional alliances would fracture around the reversion to biological or performative criteria.
% FOUNDING_PROBLEM: Historical denial of legal and social personhood to trans people, and the specific harms of violence, administrative erasure, and exclusion caused by requiring biological sex markers or medical gatekeeping for gender recognition.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and human rights monitors attest to ongoing violence and exclusion. Gender-critical feminists and some civil liberties organizations outside the beneficiary set argue the founding problem of basic non-personhood is largely solved in Western jurisdictions and the current arrangement has shifted to a redistribution function; no consensus corroboration exists from neutral institutional observers.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55â0.60) because the constraint genuinely resolves coordination problems for trans people while extracting from cis women and institutions through space re-segregation and gatekeeping. Suppression is moderate-high (0.65â0.75) because social and institutional censure enforce the norm and frame resistance as moral failure. Theater ratio rises over the interval (0.20â0.50) as institutional adoption becomes partly performativeâsignaling inclusion independently of operational reform. Accessibility collapse is moderate (0.60) because sex-based alternatives remain legally present in many jurisdictions but are socially collapsing in progressive institutions. Resistance is moderate (0.55) due to organized gender-critical opposition and some legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Trans women experience this constraint as Rope-like coordinationâan arrangement that resolves their historical exclusion. Cis women experience it as Snare-like extractionâa structure that strips categorical boundaries and penalizes resistance. Institutions experience it as Tangled Rope: genuine coordination benefit of a clear intake rule versus the active costs of enforcement, liability, and conflict management. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are declared beneficiaries with constrained exit, yielding low directionality and damped effective extraction. Cis women are declared victims with constrained exit, yielding high directionality and amplified effective extraction. Gender_identity_advocates sit low as agenda-setters with mobile exit. Sex_segregated_service_providers sit high as institutional victims whose operational constraints trap them in the enforcement role despite their power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling. A purely rights-based framing without victim accounting would present as Rope; a purely conflict-based framing without the coordination function for trans inclusion would present as Snare. The mandatory presence of both beneficiaries and victims, plus active enforcement, gates the classification to Tangled Rope and captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissent against self-ID categorization primarily structural (institutional sanctions, employment consequences, legal penalties) or internalized (self-censorship, fear of ostracism)?',
    'Post-exit trajectory study: if individuals who leave progressive institutional environments continue to self-censor on sex-based categorization, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates more extractively on payer seats than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    coordination_extraction_boundary,
    'Does self-declaration function as a genuine coordination mechanism for social inclusion, or primarily as a normative vehicle to redistribute categorical rights and spaces from cis women to trans women?',
    'Comparative analysis of jurisdictions with and without self-ID laws: measure whether the policy reduces overall administrative conflict over category membership or simply shifts conflict onto cis women and institutions.',
    'If the coordination function is separable from the redistribution, the extraction component may be isolable; if inseparable, the moderate epsilon is the inherent price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    material_interest_vs_ontological_commitment,
    'Is the institutional adoption of the gender_identity_reading driven by material interest in risk reduction and constituency management, or by ontological commitment to identity-as-reality?',
    'Trace institutional adoption curves against litigation risk and advocacy pressure versus internal belief-formation metrics.',
    'If material-interest-driven, the constraint is more susceptible to rapid reversal under shifted incentives and may compute as less stable; if ontologically committed, the kernel is more deeply embedded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_interest_vs_ontological_commitment, empirical, 'Motivational basis of institutional adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__gender_identity_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__gender_identity_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__gender_identity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gendered_category_membership kernel, decomposed per the epsilon-invariance principle. Each reading has a distinct epsilon, beneficiary structure, and classification. The gender_identity_reading influences the institutional environment for its siblings without logically foreclosing the social_role_reading, while it directly contradicts the biological_sex_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
