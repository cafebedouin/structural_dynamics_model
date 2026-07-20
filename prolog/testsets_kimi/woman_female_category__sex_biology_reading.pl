% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biological Category Boundary for Female Spaces
 *   domain: political/bioethics/gender/law
 *
 * SUMMARY:
 *   This constraint story instantiates the sex-biology reading of the
 *   contested 'woman/female category' kernel: the claim that membership in
 *   the female category is determined exhaustively by chromosomal sex
 *   (XX/XY), reproductive anatomy, and developmental biology (gamete
 *   production capacity). In policy contexts this reading enforces
 *   biological-sex segregation in prisons, shelters, sports, and
 *   anti-discrimination law. It is one of three declared readings of the same
 *   kernel; the others (gender-identity reading, hybrid-contextual reading)
 *   are modeled as separate constraints.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_protections: Primary beneficiary (organized/constrained) â gains sex-segregated spaces and protections.
 *   - trans_women_excluded: Primary target (moderate/identity_locked) â excluded from female category and spaces.
 *   - biological_females: Secondary target (moderate/constrained) â subjected to biological reductionism, verification, and essentialist enforcement.
 *   - state_administrators: Agenda-setter (institutional/mobile) â enforces the biological boundary through policy.
 *   - medical_expertise: Analytical observer (institutional/analytical) â supplies the biological authority cited to legitimate the boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.72).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.78).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biological Category Boundary for Female Spaces").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political/bioethics/gender/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'fb78cfdf-49fe-4af5-98b4-b8e8f130b41e').
narrative_ontology:cs_kernel_codification('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', formalized).
narrative_ontology:cs_authority_grounding('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', expertise).
narrative_ontology:cs_reading_relation('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', foundational, biological_sex_determines_female_category).
narrative_ontology:cs_axiom_status(biological_sex_determines_female_category, holdable).
narrative_ontology:cs_axiom_grounding('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', biological_sex_determines_female_category, empirically_contingent).
narrative_ontology:cs_axiom('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', foundational, natal_female_safety_requires_sex_segregation).
narrative_ontology:cs_axiom_status(natal_female_safety_requires_sex_segregation, holdable).
narrative_ontology:cs_axiom_grounding('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', natal_female_safety_requires_sex_segregation, instrumental).
narrative_ontology:cs_reference_frame('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', biological_sex_as_fixed_boundary).
narrative_ontology:cs_drift_state('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', contemporary_policy_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb78cfdf-49fe-4af5-98b4-b8e8f130b41e', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_excluded).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, biological_females).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek access to sex-segregated shelters, prisons, sports categories, and services on the basis of biological sex; rely on state and institutional enforcement of a biological boundary to maintain spaces they experience as protective against male-pattern violence and unfair physical competition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_protections, beneficiary,
    organized, biographical, constrained, national).

% Live and identify as women but are categorically excluded from female-designated spaces, services, and legal classifications under a biological definition; bear the costs of housing and program exclusion in shelters and prisons, and of erasure from demographic categories they identify with.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_excluded, payer,
    moderate, biographical, identity_locked, national).

% All individuals with XX chromosomes and typical female developmental biology; under this constraint they are subject to biological verification, documentation demands, and reductionist definitions that treat their femaleness as exhaustively determined by gamete production capacity and chromosomal configuration, with intersex and DSD individuals falling into ambiguous enforcement gaps.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, biological_females, payer,
    moderate, biographical, constrained, national).

% Government agencies, prison and shelter administrators, and sports regulators who implement biological-sex classification through intake procedures, legal documentation checks, and facility assignment; they enforce the boundary and could change its criteria through policy revision.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, state_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Developmental biologists, geneticists, and physicians whose research on chromosomal sex, gonadal development, and gametogenesis is cited as the epistemic foundation for the boundary; they observe and classify biological variation but do not directly set the policy that operationalizes the boundary.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_expertise, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, natal_females_seeking_protections).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, verifiable criterion for sex-segregated spaces and servicesâprisons, shelters, sports, medical researchâeliminating subjective or self-reported membership and aiming to protect natal females from male-pattern violence and physical unfairness.
% TRANSFER_FUNCTION: Moves access to female-only spaces, legal sex categories, and sports divisions from individuals with male developmental biology to individuals with female developmental biology, enforced through birth-certificate review, anatomical inspection, and chromosomal testing.
% ABSENT_VOICES: Trans women and intersex individuals are structurally excluded from the category definition itself; gender-identity advocates and human rights organizations contesting the boundary are often dismissed as ideologically motivated rather than engaged as structural interlocutors.
% DISAPPEARANCE_RATIONALE: If the biological-sex boundary vanished, prison and shelter intake would reorganize around different criteria; sports governing bodies would redesign competition classes; demographic, medical, and legal classification systems would lose their binary anchor; natal females would lose the current sex-segregation framework, and trans women would gain access to previously excluded spaces.
% FOUNDING_PROBLEM: The need for sex-segregated spaces to protect natal females from male-pattern violence and sexual predation; the need for fair competition in physical sports; the need for clear, stable demographic and medical categories.
% FOUNDING_PROBLEM_CORROBORATION: Feminist advocacy organizations and some victim-service providers attest the founding problem remains live and justifies biological boundaries. Human rights organizations, major medical associations, and social-work professional bodies attest the problem is either overstated or solvable without biological exclusion; no corroborating consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint categorically excludes a defined population from spaces and services they need, and imposes verification costs on all biological females. Suppression (0.78) is higher still because the boundary's persistence depends on active enforcementâintake screening, document checks, and legal prohibition of gender-identity-based alternativesârather than spontaneous compliance. Theater ratio (0.45) reflects that a substantial share of enforcement activity has become performative: the boundary is publicly defended as a sacred biological fact even where empirical outcomes (safety, fairness) are contested. Resistance (0.75) is high due to sustained legal and social contestation from human rights organizations, medical bodies, and trans advocacy. Accessibility collapse (0.60) is moderate: gender-identity-based alternatives exist in some jurisdictions but are actively suppressed in the domains this constraint governs. The measurement series share a single time grid (0â24) to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (state administrators, natal females seeking protections) experience the constraint as a necessary coordination mechanism for safety and fairness. The payer seats (trans women excluded, biological females subjected to verification) experience it as an enforced extraction of access and dignity. The engine computes this divergence from the structural data: same constraint, opposite directionalities, yielding different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is natal_females_seeking_protections: from their seat the constraint subsidizes their safety and competitive-fairness interests (low d). The targets are trans_women_excluded, who lose access and status through the same structure (high d, amplified by identity_locked exit), and biological_females, who bear the verification and reductionist costs of enforcement (moderate-high d). State_administrators sit near the beneficiary end as the enforcing party with mobile exit. Medical_expertise is analytical and does not collect or pay. The structural asymmetry is that the constraint coordinates protection for one group through the same mechanism that extracts access from another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as tangled_rope rather than snare because it possesses a genuine coordination functionâorganizing sex-segregated shelter, prison, and sports access that a substantial constituency regards as protective. A pure snare would lack this coordination story and operate as cover for exclusion alone. It is not a rope because the coordination is not symmetric: one group is protected while another is excluded by the same structure, and the arrangement requires active enforcement (suppression 0.78) to persist against rising resistance. Were the founding problem (male-pattern violence in shared spaces) solved by alternative means, the constraint's persistence would become pure extraction; current measurements show extraction rising over the interval, indicating accumulating rent-seeking layered onto the original coordination rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_females_victim_ambiguity,
    'Are biological females structurally beneficiaries of the biological boundary (through protection) or victims (through reductionism, verification, and misclassification of DSDs), or does the constraint produce a split cost-benefit profile within the same group?',
    'Empirical study of verification outcomes: rates of invasive examination, documentation challenges for intersex individuals, and self-reported safety versus dignity trade-offs among natal females in segregated spaces.',
    'If biological females are net victims, the beneficiary set narrows to state administrators or diffuse cultural traditionalism, pushing the constraint toward snare. If they are net beneficiaries, the tangled_rope classification holds but with a narrower victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_females_victim_ambiguity, empirical, 'Whether biological females are beneficiaries or victims of the boundary they are sorted into.').

omega_variable(
    verification_cost_scope,
    'Does the enforcement of a biological boundary require genotypic or phenotypic verification that extracts privacy and dignity from all women, or can it operate through passive documentation alone?',
    'Audit of enforcement practices across prisons, shelters, and sports bodies: frequency of physical examination, chromosomal testing, and contested-cases review.',
    'High verification intrusion raises effective extraction for biological_females and pushes their directionalities toward the target end; low-intrusive enforcement limits the victim set to exclusion of trans women.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_scope, empirical, 'The material cost of biological verification on all women.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (protecting natal females in shelters and prisons) be achieved without the extraction function (excluding trans women and policing biological boundaries), or are the two structurally inseparable?',
    'Comparative analysis of jurisdictions using alternative frameworks (risk-based screening, self-identification with harm-reduction protocols) to measure safety and fairness outcomes.',
    'If separable, the biological boundary is a tangled rope where extraction exceeds coordination cost; if inseparable, part of the measured extraction is the inherent price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether protection and exclusion are structurally separable in this domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t6, woman_female_category__sex_biology_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__sex_biology_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(woma_tr_t18, woman_female_category__sex_biology_reading, theater_ratio, 18, 0.45).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__sex_biology_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t6, woman_female_category__sex_biology_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(woma_be_t12, woman_female_category__sex_biology_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(woma_be_t18, woman_female_category__sex_biology_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(woma_be_t24, woman_female_category__sex_biology_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(woma_su_t6, woman_female_category__sex_biology_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(woma_su_t12, woman_female_category__sex_biology_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(woma_su_t18, woman_female_category__sex_biology_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(woma_su_t24, woman_female_category__sex_biology_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the woman_female_category kernel. It shares the same natural-language label with gender_identity_reading and hybrid_contextual_reading, but each instantiates a structurally distinct constraint with different epsilon values, beneficiary/victim structures, and classification outcomes. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
