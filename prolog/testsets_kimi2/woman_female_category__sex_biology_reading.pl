% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biological Determination of Female Category Membership
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates the sex-biology reading of the contested
 *   kernel woman_female_category: legal and social membership in the category
 *   woman or female is determined exclusively by chromosomal sex,
 *   reproductive anatomy, and developmental biology. The reading asserts a
 *   protective coordination function for natal females in sex-segregated
 *   spaces while actively excluding trans women and erasing intersex
 *   variance. The constraint is heavily contested, with extractiveness and
 *   suppression rising over the measured interval as enforcement mechanisms
 *   have hardened.
 *
 * KEY AGENTS:
 *   - natal_females: Primary beneficiary and secondary payer (organized/constrained) â receive protections but bear enforcement scrutiny.
 *   - trans_women: Primary target (moderate/identity_locked) â excluded from spaces and legal recognition.
 *   - state_administrators: Agenda setter (institutional/arbitrage) â enforce classification through documentation and screening.
 *   - intersex_individuals: Excluded payer (powerless/trapped) â forced into binary categories that misrepresent their biology.
 *   - gender_identity_advocates: Excluded voice (organized/constrained) â contest the framework but are structurally marginalized in policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.88).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.82).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biological Determination of Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '55a2b9db-d4c1-4e53-8bec-ac60fc576419').
narrative_ontology:cs_kernel_codification('55a2b9db-d4c1-4e53-8bec-ac60fc576419', formalized).
narrative_ontology:cs_authority_grounding('55a2b9db-d4c1-4e53-8bec-ac60fc576419', lineage).
narrative_ontology:cs_interpretation_layer_present('55a2b9db-d4c1-4e53-8bec-ac60fc576419').
narrative_ontology:cs_reading_relation('55a2b9db-d4c1-4e53-8bec-ac60fc576419', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('55a2b9db-d4c1-4e53-8bec-ac60fc576419', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('55a2b9db-d4c1-4e53-8bec-ac60fc576419', foundational, biological_sex_determines_category_membership).
narrative_ontology:cs_axiom_status(biological_sex_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('55a2b9db-d4c1-4e53-8bec-ac60fc576419', biological_sex_determines_category_membership, empirically_contingent).
narrative_ontology:cs_axiom('55a2b9db-d4c1-4e53-8bec-ac60fc576419', foundational, natal_female_safety_requires_segregation).
narrative_ontology:cs_axiom_status(natal_female_safety_requires_segregation, holdable).
narrative_ontology:cs_axiom_grounding('55a2b9db-d4c1-4e53-8bec-ac60fc576419', natal_female_safety_requires_segregation, instrumental).
narrative_ontology:cs_reference_frame('55a2b9db-d4c1-4e53-8bec-ac60fc576419', biological_dimorphism_framework).
narrative_ontology:cs_drift_state('55a2b9db-d4c1-4e53-8bec-ac60fc576419', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55a2b9db-d4c1-4e53-8bec-ac60fc576419', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_individuals).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, biological_sex_binary).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive access to sex-segregated spaces and legal protections predicated on biological sex. Simultaneously subjected to documentation checks, anatomical verification, and policing of sex-category boundaries to maintain eligibility for those spaces.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, natal_females, payer).

% Excluded from female-only spaces, legal categorization as women, and associated protections under this framework. Social and legal gender recognition is overridden by chromosomal and anatomical criteria, locking them out regardless of transition status.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, national).

% Administer sex classification on birth certificates, manage prison and shelter intake protocols, enforce sports eligibility rules through anatomical and chromosomal testing, and set evidentiary standards for category membership.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, state_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Possess anatomical or chromosomal configurations that do not fit the binary definition. Forced into either male or female categories through medical or administrative fiat, often resulting in inappropriate placement or denial of services.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Argue for self-identification as the basis for category membership. Structurally excluded from policy-making in jurisdictions where the sex-biology reading dominates, though present in public discourse.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Segments populations by reproductive biology to provide sex-segregated spaces (prisons, shelters, sports, healthcare) intended to protect natal females from male-pattern violence and physiological disadvantage.
% TRANSFER_FUNCTION: Moves legal recognition and spatial access away from individuals with male-typical chromosomes or anatomy toward individuals with female-typical chromosomes or anatomy, enforced through medical documentation and institutional screening.
% ABSENT_VOICES: Trans women contesting exclusion are present in public discourse but excluded from protected-space decisions; intersex individuals are rarely included in the definitional framework despite being directly affected by its binary structure.
% DISAPPEARANCE_RATIONALE: If sex-biological classification vanished overnight, prison and shelter placements would reorganize, sports categories would require new eligibility criteria, and birth certificate systems would need replacement classification schemes.
% FOUNDING_PROBLEM: Natal females face elevated risk of male violence and physiological disadvantages that historically justified separate spaces and legal protections.
% FOUNDING_PROBLEM_CORROBORATION: Domestic violence shelters and some feminist legal scholars corroborate the ongoing need for sex-segregated refuge; medical and human rights organizations outside the beneficiary set contest that biological gatekeeping is the appropriate or necessary solution.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is high because the constraint's enforcement requires invasive verification and denies recognition and access to a defined population. Suppression (0.82) is high because persistence depends on active legal and institutional exclusion of alternative classification schemes. Theater_ratio (0.45) reflects moderate performative maintenance: the protective justification is partly genuine and partly theatrical political signaling. Accessibility_collapse (0.65) indicates that while gender-identity alternatives are culturally visible, they are legally inaccessible where this constraint holds. Resistance (0.78) is high due to sustained advocacy and legal challenge from excluded parties.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as a technical administrative rule; the beneficiary seat experiences it as protective boundary; the payer seats experience it as categorical erasure and exclusion. The engine computes these divergences from structural data â the state has arbitrage exit, natal females are constrained, and excluded groups are identity-locked or trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females are declared as both beneficiaries (receiving sex-based protections) and victims (bearing enforcement costs and bodily scrutiny), yielding a near-symmetric directionality for that seat. Trans women and intersex individuals are pure targets because they bear exclusion costs without receiving compensatory benefits. State administrators sit near the beneficiary end because they control the classification apparatus and can arbitrage across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting natal females from male violence and disadvantage â is contested as still live. The constraint's current operation includes significant enforcement machinery that extends beyond the original protective remit. This drift risks mandatrophy: the arrangement persists even if the protective function could be achieved with less exclusionary means. The T17 accumulation pattern is visible in the measurement series, with extractiveness rising from 0.4 to 0.88 over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to trans women''s access) or internalized (natal females'' identity fusion with the protected category)?',
    'Post-policy-exit trajectory: if natal females continue to demand sex-based gatekeeping after legal barriers are removed, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measure; the constraint persists through identity-lock even without state enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    binary_biological_empirical_validity,
    'Do chromosomal and anatomical criteria cleanly sort all humans into male and female categories as the constraint assumes?',
    'Systematic review of intersex prevalence and chromosomal variation (XXY, XO, mosaicism) and their treatment under the constraint.',
    'If the binary sort fails empirically, the constraint''s coordination function collapses into arbitrary enforcement, pushing classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_biological_empirical_validity, empirical, 'Whether biological sex is strictly binary').

omega_variable(
    natal_female_dual_position,
    'Does the constraint extract more from natal females through enforcement costs than it provides in protective benefits?',
    'Comparative analysis of natal female safety outcomes and documentation burden across jurisdictions with and without strict biological gatekeeping.',
    'If extraction exceeds benefit, natal females are net payers and the constraint''s coordination story is cover for a different extraction dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natal_female_dual_position, empirical, 'Net benefit or cost to natal females').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(woma_tr_t40, woman_female_category__sex_biology_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(woma_tr_t50, woman_female_category__sex_biology_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(woma_be_t40, woman_female_category__sex_biology_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(woma_be_t50, woman_female_category__sex_biology_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(woma_su_t40, woman_female_category__sex_biology_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(woma_su_t50, woman_female_category__sex_biology_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel woman_female_category. The sex_biology_reading, gender_identity_reading, and hybrid_contextual_reading are mutually exclusive definitions of the same legal/social category. They form a constraint family linked by kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
