% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological-Sex Reading of 'Woman'/'Man' Category Membership
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the biological-sex reading of the contested
 *   'gendered category membership' kernel: category membership in
 *   'woman'/'man' is grounded in chromosomal and anatomical markers fixed at
 *   birth, independent of subsequent identity, transition, or social role.
 *   Under this reading, trans women are structurally excluded from 'woman'
 *   status and trans men from 'man' status; sex-segregated spaces (shelters,
 *   prisons, sports, some medical and carceral contexts) are administered on
 *   this boundary, and intersex individuals are treated as edge cases
 *   assigned by clinical convention rather than as evidence the binary
 *   premise is incomplete. The reading has real coordination value — a
 *   simple, administratively tractable criterion for allocating access to
 *   genuinely vulnerable-population spaces — but this coordination function
 *   is bundled with active, enforced exclusion of a specific population whose
 *   felt and lived identity the reading does not recognize as
 *   category-determining. Enforcement (legislation, litigation, facility
 *   policy, sports governance) is required to hold the boundary against
 *   sustained legal and social challenge, which is why this reads as
 *   tangled_rope rather than a clean mountain or rope.
 *
 * KEY AGENTS:
 *   - gender_critical_advocacy_organizations: agenda-setting beneficiary (organized/mobile) — sets and defends the boundary
 *   - cis_women_in_sex_segregated_spaces: beneficiary (moderate/constrained) — retains uncontested category status
 *   - sports_governing_bodies: institutional beneficiary/agenda-setter — adopts the boundary as defensible eligibility criterion
 *   - trans_women: primary target (powerless/identity_locked) — excluded from 'woman' regardless of transition or legal status
 *   - trans_men: target (powerless/identity_locked) — misclassified as 'women' against identity
 *   - intersex_individuals: target (powerless/trapped) — binary premise is empirically false for their bodies
 *   - legislatures_and_courts: observer/agenda-setter — converts the contested reading into enforceable law
 *   - medical_and_endocrinological_bodies: excluded — holds the evidence complicating the binary premise but is sidelined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.71).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological-Sex Reading of 'Woman'/'Man' Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'd2b99d0a-76b7-4e8e-b403-5150c5e34e88').
narrative_ontology:cs_kernel_codification('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', distributed).
narrative_ontology:cs_authority_grounding('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', distributed).
narrative_ontology:cs_reading_relation('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', foundational, birth_assigned_biology_is_category_determining).
narrative_ontology:cs_axiom_status(birth_assigned_biology_is_category_determining, holdable).
narrative_ontology:cs_axiom_grounding('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', birth_assigned_biology_is_category_determining, empirically_contingent).
narrative_ontology:cs_axiom('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', foundational, category_membership_is_fixed_and_non_revisable_post_birth).
narrative_ontology:cs_axiom_status(category_membership_is_fixed_and_non_revisable_post_birth, holdable).
narrative_ontology:cs_axiom_grounding('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', category_membership_is_fixed_and_non_revisable_post_birth, conventional).
narrative_ontology:cs_reference_frame('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', clinical_binary_sex_classification_pre_1990s).
narrative_ontology:cs_drift_state('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', contemporary_trans_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d2b99d0a-76b7-4e8e-b403-5150c5e34e88', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, gender_critical_advocacy_organizations).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, sports_governing_bodies).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies legislatures, litigates, and produces public messaging to fix category membership to chromosomal/anatomical markers at birth, arguing this is necessary to preserve sex-based protections. Sets policy agendas around bathroom access, sports eligibility, and single-sex services. Not personally excluded from any category by the rule it advances; gains organizational standing, funding, and political influence from maintaining the boundary as a live issue.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_critical_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, gender_critical_advocacy_organizations, beneficiary).

% Use single-sex shelters, changing rooms, prisons, and sports categories predicated on the biological-sex boundary. Some experience genuine safety or fairness benefits from a strictly enforced boundary; others are invoked as beneficiaries by advocacy groups without having sought the framing themselves. Retain full, uncontested membership in the category regardless of how the boundary is drawn.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_in_sex_segregated_spaces, beneficiary,
    moderate, biographical, constrained, national).

% Administers eligibility rules for competitive sport, adopting biological-sex criteria (chromosomal testing, testosterone thresholds tied to birth-assigned sex) to define women's divisions. Gains a defensible, litigation-resistant rationale for exclusionary eligibility rules and avoids reputational exposure from perceived unfairness disputes; can adjust criteria across jurisdictions to manage controversy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sports_governing_bodies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, sports_governing_bodies, agenda_setter).

% Excluded from the 'woman' category as defined by this reading regardless of transition status, legal sex recognition, or years lived as women. Barred from women's shelters, prisons, sports divisions, and sometimes bathrooms under policies built on this boundary. Cannot exit the constraint by any personal action short of reversing their transition or identity, which is not a live option; face documented harms from misgendering, exposure to danger in male-designated facilities, and loss of legal/social standing.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Classified as 'women' under this reading despite social and often legal recognition as men, forcing them into female-designated spaces (prisons, shelters, some sports categories) against their identity and often at active personal risk. Their exclusion from 'man' status under the same rule mirrors trans women's exclusion from 'woman' status.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_men, payer,
    powerless, biographical, identity_locked, national).

% Possess chromosomal or anatomical configurations that do not sort cleanly into the binary the reading requires, so the rule's core premise is empirically false for their bodies before any question of identity arises. Assigned a sex at birth by clinical convention, sometimes revised through non-consensual surgical intervention, and then held to that assignment by category rules that treat the binary as a fixed biological fact rather than a statistical approximation.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Adjudicate disputes over which reading of category membership governs law, weighing testimony from advocacy groups, medical bodies, and affected individuals. Their rulings determine which reading has legal force in a given jurisdiction, converting a contested philosophical claim into enforceable policy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, legislatures_and_courts, agenda_setter).

% Possess the clinical evidence on the prevalence and variability of intersex conditions and the physiological effects of transition, which complicates a strict binary premise, but are frequently sidelined in the political debate in favor of advocacy framing on both sides. Would testify that the binary is a useful approximation, not an exceptionless biological fact, if given a central evidentiary role.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_and_endocrinological_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple criterion (birth-assigned chromosomal/anatomical sex) for allocating access to sex-segregated resources — shelters, prisons, sports divisions, medical services — without requiring case-by-case adjudication of identity or social role.
% TRANSFER_FUNCTION: Moves social recognition, physical safety, and access to segregated spaces away from trans women, trans men, and intersex individuals and toward maintaining a simple, litigation-resistant administrative boundary that cis women retain by default and advocacy organizations and sports bodies use to anchor policy positions.
% ABSENT_VOICES: Trans people directly subject to the boundary and intersex individuals whose bodies do not fit the binary premise are frequently not centered in the legislative and media debate that sets policy; medical and endocrinological bodies with the relevant clinical evidence are often crowded out by advocacy framing from both directions.
% DISAPPEARANCE_RATIONALE: Advocacy organizations and some cis women would say sex-segregated protections collapse and safety/fairness structures unravel if the boundary vanished. Trans and intersex stakeholders and much clinical opinion would say the world mostly stays the same — segregated spaces would simply reorganize around a different, more inclusive membership criterion, as many jurisdictions already do without documented harm to cis women's safety. The parties dispute the counterfactual itself, not just its value.
% FOUNDING_PROBLEM: Historically, sex-segregated spaces and categories (bathrooms, prisons, sports, medical services) were built on an assumed strict biological binary because that was the dominant medical and social understanding of sex at the time; the boundary was meant to solve genuine privacy, safety, and fairness coordination problems in contexts of physical vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Endocrinological and developmental biology bodies attest that the strict binary premise was always an approximation — intersex variation was documented in clinical literature well before the current political dispute, meaning the founding premise was empirically incomplete from the outset, not merely outdated. Some cis-women's safety advocates, from outside the gender-critical organizing apparatus, corroborate that segregated-space safety concerns are real but contest that biological markers at birth are the only or best criterion for addressing them.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects substantial, non-negligible costs imposed on trans and intersex populations — loss of legal recognition, exclusion from appropriate facilities, exposure to danger, misgendering — set against real coordination value in simplifying access rules for segregated vulnerable-population spaces. Suppression (0.71) is high because maintaining the boundary against sustained legal, medical, and social challenge requires active enforcement: legislation, litigation, facility policy, and public messaging campaigns. Theater ratio is comparatively low (0.28) because much of the enforcement activity is functionally real (actual facility policy, actual eligibility testing) rather than purely performative, though a growing share is public messaging rather than adjudicated fact-finding. Accessibility collapse (0.6) is moderate-high: once the boundary is legally or institutionally adopted, alternative criteria (identity-based, role-based) become very difficult to invoke case-by-case, though they remain live in other jurisdictions and in ongoing litigation. Resistance (0.75) is high — trans advocacy organizations, some medical bodies, and civil rights litigation actively contest the boundary, distinguishing this from a settled natural fact.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this reading is experienced as a protective clarification of what was always definitionally true. From the trans and intersex payer seats, the identical structure operates as an enforced exclusion mechanism that treats their bodies or identities as errors to be sorted around rather than as legitimate category claims. The engine computes these divergent seat-level classifications from the declared power/exit/scope data; the claimed_type here names the tangled structure the authoring seat believes is present across both readings simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cis women retaining default category status, advocacy organizations gaining political standing, sports bodies gaining a defensible rule) sit near the low end of directionality — the constraint either subsidizes their position or costs them nothing structurally. Victims (trans women, trans men, intersex individuals) sit at the high end: they bear the category's costs directly and cannot exit by any action short of abandoning their identity, which the schema captures via identity_locked exit options for trans stakeholders and trapped for intersex individuals whose bodies fall outside the binary the rule assumes. Legislatures and courts are structurally positioned as adjudicators whose rulings determine which reading has legal force, giving them agenda-setting power without being personally subject to the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating access to genuinely vulnerable-population spaces under conditions of physical risk — was real and remains partly live (privacy and safety concerns in shared facilities are not fabricated). But the specific mechanism chosen to solve it (a strict, birth-assigned biological binary) was empirically incomplete even at its founding (intersex variation was documented well before this dispute), and its persistence as the exclusive criterion, rather than one criterion among several, is what the tangled_rope classification is measuring: a genuine coordination need has been fused with an enforced exclusion boundary that does not track the need cleanly. Classifying this as tangled_rope rather than snare acknowledges the real coordination function for cis women's segregated-space concerns; classifying it as tangled_rope rather than rope acknowledges that the boundary requires active suppression of a specific population to hold, and that population pays asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binary_premise_empirical_completeness,
    'Is the strict chromosomal/anatomical binary an accurate description of human biological sex variation, or a useful approximation that breaks down at the margins (intersex conditions)?',
    'Review of clinical and developmental biology literature on the prevalence and clinical treatment of intersex conditions, and whether the reading''s proponents treat these as exceptions-that-prove-the-binary or as evidence the binary is a statistical mode rather than an exceptionless law.',
    'If the binary is a statistical approximation rather than a strict biological fact, the reading''s emerges_naturally-style claim to be simply ''reading off biology'' weakens substantially, and the classification shifts further toward constructed-and-enforced rather than discovered-and-neutral.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_premise_empirical_completeness, empirical, 'Whether the chromosomal/anatomical binary is empirically exceptionless or a useful approximation with documented exceptions.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does adopting the biological_sex_reading as legally binding logically foreclose the gender_identity_reading and social_role_reading within the same jurisdiction''s legal framework, or can multiple readings coexist across different institutional contexts (e.g., sports vs. legal identity documents vs. informal social recognition)?',
    'Comparative legal analysis of jurisdictions that have adopted mixed regimes (e.g., self-ID for legal documents but biological criteria for sports) to determine whether coexistence is institutionally stable or whether one reading tends to dominate and force out the others.',
    'If coexistence is stable, this reading is better modeled as one domain-specific criterion among several rather than a totalizing claim about category membership; if one reading tends to force out others once adopted, the forecloses relation in cs_structure would need revisiting for specific institutional contexts even though the general relation is authored as coexists_with here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether legal adoption of this reading forecloses sibling readings in practice, or genuine multi-criteria coexistence is institutionally viable.').

omega_variable(
    safety_rationale_versus_exclusion_function,
    'How much of the coordination value claimed for this reading (privacy/safety in segregated spaces) is actually served by the biological-marker criterion specifically, versus being separable into criteria that do not require excluding trans people (e.g., risk-based individual assessment, presence-based accommodation)?',
    'Comparative outcome data from jurisdictions and facilities that have adopted alternative criteria (self-identification with safeguards, individualized risk assessment) versus strict biological-marker regimes, measuring documented safety incidents.',
    'If alternative criteria achieve comparable safety outcomes without the exclusionary cost, this weakens the coordination-function component of the tangled_rope classification and strengthens a reading of the constraint as closer to snare (extraction with a safety cover story) rather than genuine hybrid coordination/extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_rationale_versus_exclusion_function, empirical, 'Whether the biological-marker criterion is necessary for the claimed safety coordination function or separable from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__biological_sex_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__biological_sex_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(gend_tr_t18, gendered_category_membership__biological_sex_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__biological_sex_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__biological_sex_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__biological_sex_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(gend_be_t18, gendered_category_membership__biological_sex_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__biological_sex_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__biological_sex_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__biological_sex_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(gend_su_t18, gendered_category_membership__biological_sex_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__biological_sex_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gendered_category_membership kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: biological_sex_reading (this file, tangled_rope, high enforcement-dependent extraction directed at trans/intersex populations), gender_identity_reading (self-declaration criterion, different beneficiary/victim structure), and social_role_reading (sustained social performance/recognition criterion, a third distinct structure). The three do not share an ε value or a beneficiary/victim set — each reading names its own. The forecloses relation to gender_identity_reading reflects that a jurisdiction cannot simultaneously hold 'membership is fixed by birth biology' and 'membership is fixed by self-declared identity' as the SAME legal criterion for the SAME category without contradiction, though both readings coexist as live political positions across different jurisdictions and institutions. The coexists_with relation to social_role_reading reflects that social recognition criteria can operate informally alongside a legally binding biological criterion without direct logical contradiction (e.g., someone may be socially recognized in a role while legally classified otherwise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
