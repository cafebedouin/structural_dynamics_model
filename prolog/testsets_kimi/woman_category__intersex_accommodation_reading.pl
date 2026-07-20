% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the intersex_accommodation_reading of the
 *   woman_category kernel: it defines 'woman' to include not only typical
 *   female biology but also intersex variations that do not fit the male
 *   category. The reading is contested by both strict biological sex readings
 *   (which exclude many intersex people) and gender identity readings (which
 *   reject biological gatekeeping). Its most visible enforcement site is
 *   elite sport, where the inclusionary definition is maintained through
 *   active medical policingâtestosterone thresholds, genetic verification,
 *   and mandatory hormone suppression. The victim set is people with
 *   female-typical or ambiguous biology (the Semenya case typifies the
 *   dynamic). The authored metrics describe the elite-sports enforcement
 *   context, where extractiveness is high; general policy domains show lower
 *   extraction but are not the primary enforcement site.
 *
 * KEY AGENTS:
 *   - sports_governing_bodies (institutional/analytical): Agenda-setter â sets testosterone thresholds and medical verification protocols.
 *   - intersex_athletes (powerless/trapped): Primary target â bear medical costs, privacy invasion, and competitive exclusion; also gain formal recognition.
 *   - female_athletes (organized/constrained): Beneficiary â compete in a field where higher-testosterone intersex athletes are medically suppressed or excluded.
 *   - medical_expert_panels (institutional/analytical): Observer â lend scientific legitimacy to enforcement without direct benefit.
 *   - gender_identity_advocates and intersex_advocacy_groups (organized/moderate/constrained): Excluded voices â reject the biological gatekeeping framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.72).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.78).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '8699a4dd-257e-495d-9fd7-2e88e38599aa').
narrative_ontology:cs_kernel_codification('8699a4dd-257e-495d-9fd7-2e88e38599aa', formalized).
narrative_ontology:cs_authority_grounding('8699a4dd-257e-495d-9fd7-2e88e38599aa', expertise).
narrative_ontology:cs_interpretation_layer_present('8699a4dd-257e-495d-9fd7-2e88e38599aa').
narrative_ontology:cs_reading_relation('8699a4dd-257e-495d-9fd7-2e88e38599aa', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('8699a4dd-257e-495d-9fd7-2e88e38599aa', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('8699a4dd-257e-495d-9fd7-2e88e38599aa', foundational, biological_sex_is_nonbinary_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_nonbinary_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('8699a4dd-257e-495d-9fd7-2e88e38599aa', biological_sex_is_nonbinary_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('8699a4dd-257e-495d-9fd7-2e88e38599aa', foundational, intersex_variations_belong_in_woman_category).
narrative_ontology:cs_axiom_status(intersex_variations_belong_in_woman_category, holdable).
narrative_ontology:cs_axiom_grounding('8699a4dd-257e-495d-9fd7-2e88e38599aa', intersex_variations_belong_in_woman_category, conventional).
narrative_ontology:cs_reference_frame('8699a4dd-257e-495d-9fd7-2e88e38599aa', inclusive_biological_typology).
narrative_ontology:cs_drift_state('8699a4dd-257e-495d-9fd7-2e88e38599aa', contemporary_elite_sports_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8699a4dd-257e-495d-9fd7-2e88e38599aa', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_athletes).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, female_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_athletes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets testosterone thresholds and medical verification protocols for women's competition. Justifies rules as protecting fair competition while accommodating intersex variations. Retains authority to define category boundaries and adjudicates edge cases through medical panels and court of arbitration for sport.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Have intersex variations that place them in the woman category under this reading but subject them to mandatory hormone suppression, surgery, or invasive medical verification to compete. Bear costs of medical intervention, privacy violation, and public scrutiny. Receive legal recognition as women but at the price of bodily autonomy.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_athletes, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, intersex_athletes, beneficiary).

% Compete in the women's category. Benefit from a field where athletes with certain intersex traits are medically suppressed or excluded, reducing direct competition from higher-testosterone athletes. Do not set the rules but gain competitive advantage from their enforcement.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, female_athletes, beneficiary,
    organized, biographical, constrained, global).

% Argue that medical testing and hormone suppression violate bodily autonomy and human rights. Are consulted selectively by sports bodies but not given veto power over regulations. Would object to the enforcement mechanism while supporting recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocacy_groups, excluded,
    moderate, generational, constrained, global).

% Hold that gender identity, not biology, should determine category membership. Are excluded from the definitional framework of this reading, which remains anchored in biological sex. Would object that the reading is still exclusionary toward trans women.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Endocrinologists and geneticists who advise sports federations on testosterone thresholds and sex verification. Provide scientific legitimacy to the enforcement apparatus. Do not directly benefit but lend expertise that sustains the constraint.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_expert_panels, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legal and social classification of people with intersex variations who do not fit typical male or female biological categories, providing a category membership rule that avoids complete exclusion.
% TRANSFER_FUNCTION: Moves bodily autonomy, medical privacy, and competitive opportunity from intersex athletes to sports governing bodies and medical panels, in exchange for formal legal recognition as women.
% ABSENT_VOICES: Gender identity advocates who reject biological criteria altogether, and sex-binary advocates who reject spectrum definitions, are structurally excluded from the policymaking table; intersex human rights advocates who oppose medical testing are consulted but not granted authority.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, intersex athletes would no longer be subject to mandatory hormone suppression or medical verification to compete as women; sports categories would face immediate pressure to reorganize around either strict binary tests or identity-based criteria; the current medical-administrative apparatus would lose its jurisdiction.
% FOUNDING_PROBLEM: Binary sex categories in law and sport fail to account for intersex variations, leaving a small but significant population legally unclassifiable and socially excluded.
% FOUNDING_PROBLEM_CORROBORATION: Intersex advocacy organizations attest the problem is live, citing historical exclusion. Sports federations attest the problem is resolved by current rules. Independent bioethicists and human rights monitors corroborate that exclusion persists but dispute whether the current medicalized solution is proportionate; no party outside the benefiting institutions uncritically accepts the enforcement mechanism as the only resolution.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint enforces category membership through invasive medical intervention and exclusion, extracting bodily autonomy from a trapped, powerless population. Suppression is high (0.78) because alternatives (compete without medical modification, or adopt identity-based criteria) are actively barred by sports federations. Theater ratio is moderate-high (0.52): the medical testing apparatus performs rigor while the underlying fairness question remains unresolved. Accessibility collapse is high (0.75) because once the biological-spectrum criterion is institutionalized, self-identification or non-medical alternatives collapse in regulated spaces. Resistance is substantial (0.68) from human rights advocates, affected athletes, and competing definitional movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (sports federations) experiences the constraint as necessary coordination to preserve fair competition under an inclusive biological definition. The payer seat (intersex athletes) experiences the same structure as coercive medical extraction that conditions recognition on bodily modification. The beneficiary seat (other athletes) sees the constraint as protection. The engine computes this divergence from the structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sports governing bodies are near the beneficiary endâthey control the rules and gain authority. Intersex athletes are near the target endâtrapped exit, powerless, directly medicated. Female athletes sit low-to-mid d: they benefit from the enforcement but do not control it. Medical panels are near symmetric/analytical: they provide legitimacy without capturing gains. Excluded advocates have no d assignment (no stake). The beneficiary/victim declarations plus exit options drive the engine's d derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and victim declarations. The coordination function (inclusion of intersex women) is real; the victim set (intersex athletes subjected to medical policing) is also real. Without the coordination story, the constraint would read as a pure snare; without the victim story, it would read as a rope. The tangled_rope classification captures the hybrid structure. If the medical enforcement were to atrophy while the definitional inclusion persisted, the constraint would drift toward rope; if the inclusionary narrative were dropped while enforcement remained, it would become a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the intersex_accommodation_reading of the woman_category kernel; would classification change if the sex_biology_reading or gender_identity_reading were adopted instead?',
    'Compare the victim sets and enforcement mechanisms across the three readings in the same institutional context.',
    'If the sex_biology reading were adopted, the victim set would shift to excluded intersex women; if the gender_identity reading were adopted, the biological enforcement apparatus would dissolve and the extraction vector would change entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest uncertainty for woman_category').

omega_variable(
    domain_dependent_extraction,
    'Does the extractiveness of this constraint remain low in general policy domains, or does the elite sports enforcement pattern generalize?',
    'Cross-domain comparison of how this definitional reading is enforced in healthcare, education, and criminal law versus elite athletics.',
    'If general-policy enforcement is similarly medicalized, epsilon is higher than authored and the constraint approaches snare; if sports is an outlier, the reading is more coordinative in general law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_dependent_extraction, empirical, 'Domain dependency of extraction across policy areas').

omega_variable(
    medical_policing_legitimacy,
    'Is the medical testing and hormone suppression required by sports enforcement a necessary consequence of this reading''s biological-spectrum axiom, or an instrumental drift toward extraction?',
    'Historical comparison of pre- and post-implementation enforcement patterns; examine whether the inclusive definition inherently requires medical gatekeeping or if the gatekeeping was added by sports institutions.',
    'If medical policing is inherent, the reading carries a structural extraction cost for intersex women; if it is instrumental drift, the constraint is a scaffold that decayed into tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_policing_legitimacy, conceptual, 'Whether medical policing is inherent or drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t4, woman_category__intersex_accommodation_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(woma_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(woma_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.52).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(woma_be_t4, woman_category__intersex_accommodation_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(woma_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(woma_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(woma_su_t4, woman_category__intersex_accommodation_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(woma_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(woma_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three epsilon-invariant readings because the colloquial label 'woman' conflates structurally distinct claims: biological binary, biological spectrum, and gender identity. Each reading carries a different victim set, enforcement mechanism, and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
