% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind (Formal Equality) Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the colorblind reading of the Equal Protection
 *   Clause kernel: the categorical claim that the state may never classify by
 *   race, regardless of remedial or diversity-based purpose. This is one of
 *   three structurally distinct constraints that share a single
 *   constitutional text — the antisubordination reading and the remedial
 *   reading are separate constraints with different beneficiary/victim
 *   structures and different ε, linked via network.affects_constraints, not
 *   folded into this file. Under the colorblind reading, the coordination
 *   function (a stable, administrable, motive-blind rule) is real, but it
 *   operates through an enforcement apparatus (judicial invalidation of
 *   race-conscious admissions and remedial programs) that transfers advantage
 *   away from historically excluded groups and toward applicants and
 *   institutions favored by facial neutrality. The claimed type is
 *   tangled_rope: genuine coordination value (predictability, reduced
 *   litigation, formal equal treatment) coexists with asymmetric extraction
 *   (loss of remedial pathway for groups whose disadvantage the state itself
 *   helped constitute).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.71).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause — Colorblind (Formal Equality) Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'b2c268fe-8c57-479c-a6a8-9c684bc2ecf4').
narrative_ontology:cs_kernel_codification('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', fixed_text).
narrative_ontology:cs_authority_grounding('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', lineage).
narrative_ontology:cs_interpretation_layer_present('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4').
narrative_ontology:cs_reading_relation('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', foundational, racial_classification_itself_is_the_harm).
narrative_ontology:cs_axiom_status(racial_classification_itself_is_the_harm, holdable).
narrative_ontology:cs_axiom_grounding('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', racial_classification_itself_is_the_harm, deontological).
narrative_ontology:cs_axiom('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', secondary, state_purpose_is_constitutionally_irrelevant_to_classification_validity).
narrative_ontology:cs_axiom_status(state_purpose_is_constitutionally_irrelevant_to_classification_validity, holdable).
narrative_ontology:cs_axiom_grounding('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', state_purpose_is_constitutionally_irrelevant_to_classification_validity, conventional).
narrative_ontology:cs_reference_frame('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', post_brown_formal_equality_consensus).
narrative_ontology:cs_drift_state('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', post_2023_admissions_rulings_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b2c268fe-8c57-479c-a6a8-9c684bc2ecf4', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_facial_neutrality_defense).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, white_and_asian_american_applicants_in_race_conscious_regimes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_legal_movement_litigators).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, underrepresented_minority_faculty_and_staff_pipelines).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, k12_districts_relying_on_race_conscious_integration_plans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_facial_neutrality_defense).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, constitutional_colorblindness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the clause as a categorical bar on racial classification by the state, striking down race-conscious admissions and other race-aware remedial programs as per se violations. Sets the doctrinal rule that lower courts, universities, and legislatures must follow; can revise the reading only through future appointments or reversal.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court_majority_coalition, agenda_setter,
    institutional, generational, analytical, national).

% Under prior race-conscious regimes, some in this group perceived themselves as disadvantaged relative to holistic-review admissions weighting race as a plus factor. Under the colorblind reading, admissions decisions are formally reprocessed without racial classification, which this group experiences as removal of a disadvantage. They can litigate, transfer, or apply broadly across institutions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, white_and_asian_american_applicants_in_race_conscious_regimes, beneficiary,
    organized, biographical, mobile, national).

% Lose access to admissions pathways that had accounted for the documented effects of segregation, redlining, and unequal K-12 funding on their applicant pool. Facially neutral proxies (geography, essays, socioeconomic status) replace direct consideration, but empirical post-implementation data shows enrollment declines at selective institutions. Individual applicants cannot relitigate the systemic disadvantage; their only recourse is competing within a formally identical but substantively unequal process.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_racial_minority_applicants, payer,
    powerless, biographical, constrained, national).

% Depend on diverse student pipelines to sustain diverse graduate, faculty, and professional cohorts over time. As undergraduate diversity declines under the colorblind rule, downstream representation in law, medicine, and academia is projected to narrow, compounding over generations. They have no direct standing to challenge the doctrine and can only advocate through institutional policy workarounds.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, underrepresented_minority_faculty_and_staff_pipelines, payer,
    moderate, generational, constrained, national).

% Gain a clean, litigation-resistant compliance standard: strip race from admissions files entirely and the institution is largely insulated from equal-protection suits. This reduces legal exposure and administrative complexity. At the same time, institutions committed to diversity as an educational and reputational value bear the cost of redesigning admissions and often see enrollment outcomes they did not want.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_facial_neutrality_defense, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, selective_institutions_seeking_facial_neutrality_defense, payer).

% Advocacy organizations and legal scholars who built decades of litigation strategy around formal equality doctrine achieve their central goal with this reading's ascendance. They gain institutional standing, funding, and precedent to extend the colorblind rule into other domains (employment, contracting, redistricting).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_legal_movement_litigators, beneficiary,
    organized, civilizational, arbitrage, national).

% School districts that used race-conscious student assignment to counter residential segregation lose that tool and must resort to proxies (income, geography) that historically achieve less integration. Districts operate under fixed municipal boundaries and cannot exit the jurisdiction whose demographic patterns the doctrine now leaves largely unaddressed.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, k12_districts_relying_on_race_conscious_integration_plans, payer,
    moderate, generational, trapped, regional).

% Produce empirical documentation of ongoing disparate effects of historical discrimination (wealth gaps, school funding disparities, residential segregation) that the colorblind reading's categorical rule treats as constitutionally irrelevant to present state action. Their findings are submitted as amicus evidence but the doctrine's per se rule forecloses the inquiry into present effects of past exclusion regardless of the evidentiary record.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_historians_and_social_scientists, excluded,
    moderate, civilizational, analytical, national).

% Apply the categorical rule to concrete admissions, contracting, and redistricting disputes, generating a body of case law that further entrenches or narrows the doctrine depending on factual variation. They do not set the rule but shape its granular application.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, lower_federal_and_state_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, bright-line rule — no racial classification by the state, ever — that eliminates case-by-case scrutiny of institutional motive and reduces litigation uncertainty for institutions seeking a stable compliance standard.
% TRANSFER_FUNCTION: Moves admissions and resource-allocation advantage away from applicants and communities whose disadvantage was constituted by documented historical state action, and toward applicants who compete under a formally identical process that does not account for that history; also transfers litigation risk away from institutions that abandon race-conscious remedial programs.
% ABSENT_VOICES: Civil rights historians and social scientists documenting continuing effects of segregation and redlining submit evidence that the categorical rule treats as constitutionally irrelevant; K-12 districts and minority pipeline institutions bearing generational costs have no direct doctrinal standing to reopen the question once decided.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, institutions could resume race-conscious admissions and remedial programs where narrowly tailored, K-12 districts could reintroduce race-conscious integration tools, and litigation strategy built on the categorical rule would lose its doctrinal anchor — enrollment patterns and compliance regimes at selective institutions would shift within admissions cycles.
% FOUNDING_PROBLEM: The Equal Protection Clause was enacted to prevent state-sponsored racial subordination in the aftermath of slavery; the colorblind reading reframes the founding problem as any state use of racial categories as such, treating the classification itself (not its subordinating function) as the constitutional harm to be eliminated.
% FOUNDING_PROBLEM_CORROBORATION: The colorblind legal movement and the current Supreme Court majority attest that formal equality fulfills the clause's original purpose. Civil rights historians, dissenting justices, and social science researchers outside the litigating coalition attest that the founding problem — caste-like racial subordination and its enduring structural effects — remains live and is not addressed, and in some respects is worsened, by a rule that forbids remedial race-consciousness alongside invidious discrimination.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the measured interval (0.32 to 0.58) reflecting the doctrine's expansion from higher-education admissions into K-12 assignment plans, government contracting, and redistricting as the colorblind rule is extended by lower courts and legislatures citing it as precedent. Theater ratio is moderate and rising (0.22 to 0.40): the rule's formal-equality framing performs neutrality even as post-implementation enrollment data at institutions applying it shows substantive effects concentrated on excluded groups, which the categorical rule is structurally indifferent to by design. Suppression is high and rising (0.45 to 0.71) because the doctrine forecloses inquiry into an institution's remedial motive or into disparate effects — an institution attempting a workaround remedial program can be enjoined regardless of documented need, and the categorical form of the rule leaves little room for case-specific accommodation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (the enforcing judiciary) and from the colorblind-movement beneficiary seat, this reading is experienced as principled formal equality — a genuine coordination achievement ending decades of contested race-based sorting. From the payer seats (excluded minority applicants, affected districts), the same rule is experienced as removal of the only tool available to address effects the state itself caused, operating through the same categorical enforcement mechanism. The engine computes this divergence from the structural beneficiary/victim/enforcement data; the claimed_type of tangled_rope is authored to reflect that both a real coordination function and a real asymmetric extraction function are present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court majority coalition sets and enforces the rule; it is the agenda_setter but not itself a beneficiary in a material sense — the material beneficiaries are the colorblind legal movement (achieves its doctrinal goal), applicants in groups previously subject to race-conscious downward pressure in admissions math, and institutions seeking a low-litigation-risk compliance posture. The victims are historically excluded minority applicants, downstream faculty/professional pipelines, and K-12 districts whose remedial tools are foreclosed. These victim groups have constrained or trapped exit options — an individual applicant cannot renegotiate the national admissions rule, and a school district cannot exit its own jurisdiction's demographic history.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-sponsored racial subordination — is treated by this reading's proponents as solved by the categorical rule itself (formal equality achieves the founding purpose). Civil rights historians and dissenting justices corroborate that the founding problem, understood as caste-like subordination and its structural legacy, remains substantially live, which places founding_problem_status as contested rather than resolved. This is precisely the mismatch the R5 genealogy interview is designed to surface: status=contested paired with disappearance_verdict=world_rearranges signals that the doctrine's claimed resolution of its founding problem is itself the live dispute, not a settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_kernel_reading_selection,
    'Is the categorical, motive-blind reading of the Equal Protection Clause the correct reading of the kernel, or is it one contested reading among several equally defensible ones (remedial, antisubordination)?',
    'This is not empirically resolvable within constitutional interpretation alone; it depends on originalist versus purposive/antisubordination methodological commitments, and on which historical evidence (Reconstruction-era debates, Freedmen''s Bureau race-conscious legislation contemporaneous with the Fourteenth Amendment) is treated as dispositive.',
    'If the antisubordination or remedial reading is judicially adopted instead, race-conscious remedial and diversity-based programs become permissible again, reversing the beneficiary/victim structure of this constraint entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colorblind_kernel_reading_selection, conceptual, 'Whether the colorblind reading is the correct interpretation of the kernel or one of several contested readings.').

omega_variable(
    formal_versus_substantive_equality_tradeoff,
    'Does the categorical rule''s administrability benefit (predictability, reduced litigation over institutional motive) outweigh its cost in foreclosing remedies for documented, ongoing effects of historical state-sponsored discrimination?',
    'Longitudinal tracking of enrollment, pipeline representation, and disparity trends at institutions and districts operating under the colorblind rule versus comparable ones operating under remedial or antisubordination readings (where jurisdictionally available), compared against pre-adoption baselines.',
    'If disparities widen substantially and persistently post-adoption with no compensating administrability gain realized in practice, this supports classifying the rule''s coordination function as largely pretextual relative to its extractive effect; if disparities stabilize and litigation costs meaningfully fall, this supports a more genuine coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_versus_substantive_equality_tradeoff, empirical, 'Whether the rule''s administrability benefit justifies its foreclosure of remedial pathways.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are applicants who benefit from the colorblind rule''s removal of race-conscious weighting genuine beneficiaries of a coordination function, or beneficiaries of a redistribution the doctrine merely relabels as neutral?',
    'Compare admissions outcomes under holistic race-conscious review versus facially neutral proxy review for statistically matched applicant cohorts, isolating whether proxy-based review reproduces similar sorting through different variables (thereby showing the ''neutral'' benefit was substitutive rather than principled).',
    'If proxy variables substantially reproduce prior sorting patterns, the claimed beneficiary group''s gain is smaller than assumed and the extraction from excluded groups is the dominant effect; if proxies produce genuinely different, merit-correlated outcomes, the coordination framing is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether the beneficiary class''s gain reflects genuine neutral coordination or relabeled redistribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(equa_tr_t4, equal_protection_kernel__colorblind_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(equa_tr_t8, equal_protection_kernel__colorblind_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(equa_tr_t12, equal_protection_kernel__colorblind_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(equa_tr_t16, equal_protection_kernel__colorblind_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__colorblind_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(equa_be_t4, equal_protection_kernel__colorblind_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(equa_be_t8, equal_protection_kernel__colorblind_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(equa_be_t12, equal_protection_kernel__colorblind_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(equa_be_t16, equal_protection_kernel__colorblind_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__colorblind_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(equa_su_t4, equal_protection_kernel__colorblind_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(equa_su_t8, equal_protection_kernel__colorblind_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(equa_su_t12, equal_protection_kernel__colorblind_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(equa_su_t16, equal_protection_kernel__colorblind_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__colorblind_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__colorblind_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_kernel, each authored as a separate constraint story with its own ε and stakeholder structure per the ε-invariance principle: colorblind_reading (this file, tangled_rope — categorical bar on state racial classification), remedial_reading (permits narrowly tailored race-conscious remedy), and antisubordination_reading (targets subordination specifically, not classification per se). The three are not the same constraint measured differently; they are structurally distinct claims about what the clause requires, with different beneficiary/victim sets and different extraction profiles, linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
