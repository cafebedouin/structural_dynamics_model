% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading: Government May Not Classify by Race
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause asserts that the
 *   Fourteenth Amendment forbids all governmental racial classifications,
 *   treating individuals as rights-bearers independent of group membership.
 *   This reading presents itself as a Mountain: a fixed constitutional
 *   principle (formal equality) that would persist regardless of who defends
 *   it, with negligible extraction (ε≈0.05) because it merely requires
 *   government to ignore race. Beneficiaries are all individuals as
 *   rights-bearers; victims are individuals subjected to race-conscious
 *   government action (admissions, contracting, districting). The reading
 *   claims the founding problem (securing equal citizenship against Black
 *   Codes) is substantially solved, though this is contested. Sibling
 *   readings (remedial, diversity) instantiate different constraints with
 *   different ε and stakeholder structures. The colorblind reading forecloses
 *   both siblings within any single commitment framework: its core premise
 *   (race never relevant) directly contradicts theirs (race sometimes
 *   relevant/relevant for remediation/diversity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.05).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Colorblind Reading: Government May Not Classify by Race").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'aa05470d-16c0-4561-a749-30843d77d5c6').
narrative_ontology:cs_kernel_codification('aa05470d-16c0-4561-a749-30843d77d5c6', fixed_text).
narrative_ontology:cs_authority_grounding('aa05470d-16c0-4561-a749-30843d77d5c6', lineage).
narrative_ontology:cs_interpretation_layer_present('aa05470d-16c0-4561-a749-30843d77d5c6').
narrative_ontology:cs_reading_relation('aa05470d-16c0-4561-a749-30843d77d5c6', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('aa05470d-16c0-4561-a749-30843d77d5c6', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('aa05470d-16c0-4561-a749-30843d77d5c6', foundational, government_may_not_classify_by_race).
narrative_ontology:cs_axiom_status(government_may_not_classify_by_race, holdable).
narrative_ontology:cs_axiom_grounding('aa05470d-16c0-4561-a749-30843d77d5c6', government_may_not_classify_by_race, deontological).
narrative_ontology:cs_axiom('aa05470d-16c0-4561-a749-30843d77d5c6', secondary, individual_rights_trump_group_interests).
narrative_ontology:cs_axiom_status(individual_rights_trump_group_interests, holdable).
narrative_ontology:cs_axiom_grounding('aa05470d-16c0-4561-a749-30843d77d5c6', individual_rights_trump_group_interests, deontological).
narrative_ontology:cs_reference_frame('aa05470d-16c0-4561-a749-30843d77d5c6', colorblind_constitutional_principle).
narrative_ontology:cs_drift_state('aa05470d-16c0-4561-a749-30843d77d5c6', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa05470d-16c0-4561-a749-30843d77d5c6', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, individuals_subjected_to_race_conscious_government_action).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_against_group_classification).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, constitutional_colorblindness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person, regardless of race, benefits from the guarantee that government will not classify or treat them based on race. The constraint operates as a shield: no racial boxes to check, no racial preferences for or against. Exit is mobile — the right travels with the person; no identity lock because the right is individual, not group-based.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals who are classified by race and given or denied opportunities based on that classification (e.g., applicants to universities with race-conscious admissions, contractors in set-aside programs, voters in majority-minority districts). They bear the cost of the classification: stigma, mismatch, or denied opportunity. Exit is trapped — they cannot opt out of the government action that classifies them; the classification is imposed by state power.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individuals_subjected_to_race_conscious_government_action, payer,
    powerless, immediate, trapped, national).

% Courts (especially the Supreme Court) interpret and enforce the colorblind principle. They set the doctrinal standard (strict scrutiny, colorblindness, etc.) and determine which race-conscious policies survive. They benefit from the constraint's clarity as an administrable rule. Exit is arbitrage — courts can shift doctrine (as in SFFA) without personal cost; their institutional role is to adjudicate, not to bear the constraint's effects.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups that were historically subjected to caste legislation (Black Americans, Native Americans, etc.) and seek race-conscious remediation (affirmative action, voting rights remedies, reparations). Under the colorblind reading, their claims are structurally excluded — the constraint's logic treats their remediation-seeking as the very classification it forbids. They are not in the conversation because the colorblind framework denies group-based claims. Exit is constrained — they can litigate, organize, seek legislative remedies, but the constitutional constraint blocks the judicial path.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_groups_seeking_remediation, excluded,
    organized, generational, constrained, national).

% Universities, employers, and civil rights organizations that pursue race-conscious diversity policies. They are excluded from the colorblind framework's coordination function — the constraint treats their diversity rationale as impermissible classification. They can advocate for doctrinal change, comply with colorblind rules, or seek alternative (race-neutral) diversity mechanisms. Exit is constrained — they operate within the constraint but seek to modify or circumvent it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, diversity_advocates_and_institutions, excluded,
    organized, biographical, constrained, national).

% Scholars and jurists who analyze the constraint from an originalist/colorblind perspective. They neither collect nor pay; they observe the doctrinal trajectory and argue for the reading's fidelity to the constitutional text. Their seat is analytical — they see the full structure but are not subject to it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, originalist_scholars_and_judges, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable constitutional rule: government may not classify citizens by race. This coordinates expectations of equal treatment, eliminates the need for case-by-case balancing of racial interests, and prevents the state from distributing benefits/burdens on racial grounds. The coordination is solving the problem of racial caste — a single, bright-line rule against classification.
% TRANSFER_FUNCTION: Blocks the transfer of opportunities, benefits, and burdens along racial lines. Prevents government from moving resources from one racial group to another (remedial transfers) or using race as a proxy for diversity (diversity transfers). The constraint stops the transfer; it does not itself move resources.
% ABSENT_VOICES: Historically subordinated groups seeking remediation (Black Americans, Native Americans, Latinos) and diversity advocates (universities, employers, civil rights organizations) are structurally excluded. They would argue that formal colorblindness entrenches substantive inequality and that the constraint's beneficiaries are not 'all individuals equally' but those who benefit from the status quo. They are absent because the colorblind framework denies standing to group-based claims.
% DISAPPEARANCE_RATIONALE: If the colorblind constraint vanished overnight, race-conscious admissions, contracting set-asides, majority-minority districting, and other remedial/diversity policies would proliferate immediately. The constitutional barrier would be gone; legislative and executive branches would expand race-conscious programs. The world of government racial classification would rearrange dramatically.
% FOUNDING_PROBLEM: The Fourteenth Amendment was enacted to secure equal citizenship for freedmen against the Black Codes — state laws that imposed racial caste after the Civil War. The founding problem was governmental racial classification that created a hereditary underclass. The colorblind reading holds that the solution was a flat prohibition on racial classification by government.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Raoul Berger, Michael McConnell) and colorblind-originalist justices (Thomas, Scalia) attest the founding problem was anti-caste formal equality and is substantially solved. Historians of Reconstruction (e.g., Eric Foner) and anti-subordination scholars (e.g., Owen Fiss, Reva Siegel) attest the founding problem was substantive inequality requiring race-conscious remediation and remains live. The corroboration is split along methodological lines; no consensus outside the benefiting parties (originalist/colorblind coalition).
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_clause__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint, as this reading understands it, simply prohibits classification — it does not transfer resources. Suppression is low (0.12) because the constraint operates as a legal rule, not active coercion; resistance is near-zero (0.08) because few openly defend racial classification as such. Accessibility collapse is very high (0.92): once the colorblind principle is accepted, race-conscious alternatives are logically foreclosed. Theater ratio (0.25) reflects the Court's historical performative adherence — claiming colorblindness while carving exceptions (Bakke, Grutter) — though SFFA v. Harvard (2023) reduces theater by enforcing the principle more consistently. The metrics are authored from the reading's own lights: the constraint IS the colorblind principle, not Court doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the colorblind seat (all individuals), the constraint is a mountain: formal equality, no extraction, natural law. From the remedial seat (historically subordinated groups), the SAME constraint operates as a snare: it blocks remediation, extracts opportunity, and entrenches substantive inequality. From the diversity seat (educational institutions), it operates as a tangled rope: it coordinates a diversity rationale but extracts institutional autonomy. The engine computes this seat divergence from the structural data — beneficiaries (all_individuals) vs. victims (those subjected to race-conscious policy) vs. excluded (groups seeking remediation).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: all_individuals_as_rights_bearers — the constraint subsidizes every individual by guaranteeing race-neutral treatment (d ≈ 0.0). Victims: individuals_subjected_to_race_conscious_government_action — they bear the cost of being classified and treated by race (d ≈ 1.0). Excluded: historically_subordinated_groups_seeking_remediation and diversity_advocates — they would object but are structurally excluded from the colorblind framework's beneficiary logic. The agenda_setter is the judiciary (courts) interpreting and enforcing the constraint. Directionality derives from beneficiary/victim declarations: the reading's own structure makes individuals the sole rights-holders; race-conscious policy is the extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading claims the founding problem (equal citizenship against caste legislation) is substantially solved — the constraint persists but its original justification has attenuated. However, the reading asserts the constraint is NOT mandatrophy because the principle (race never relevant) is permanent, not transitional. The mismatch test: founding_problem_status = contested, disappearance_verdict = world_rearranges. If the constraint vanished, race-conscious policies would proliferate — the world rearranges. This mismatch (contested status + rearranges) flags potential capture: the constraint persists because it now benefits opponents of remediation, not because the founding problem is live. The omega 'formal_equality_addresses_substantive_inequality' captures the irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the colorblind equality principle a genuine natural law / fixed constitutional commitment, or a constructed constraint that benefits identifiable agents (e.g., opponents of race-conscious remediation)?',
    'Historical-originalist analysis of the 14th Amendment''s enactment understanding vs. later doctrinal construction; comparative analysis of whether the principle operates without identifiable beneficiaries capturing its enforcement.',
    'If constructed with identifiable beneficiaries, the false_summit_mountain signature triggers and reclassifies to tangled_rope; if genuine natural law, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether colorblind equality is a structural feature of constitutional order or a contested construction with beneficiaries').

omega_variable(
    founding_understanding_contested,
    'Did the 14th Amendment''s framers and ratifiers understand equal protection as forbidding all racial classifications (colorblind), or as forbidding only caste legislation while permitting race-conscious remediation?',
    'Originalist historical evidence (congressional debates, ratification records, Freedmen''s Bureau Acts, Civil Rights Act of 1866) vs. anti-classification vs. anti-subordination scholarly debate.',
    'If the founding understanding was anti-subordination (remedial), the colorblind reading is a later doctrinal construction; if anti-classification, the colorblind reading has stronger originalist claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_understanding_contested, empirical, 'Historical dispute over the 14th Amendment''s original meaning').

omega_variable(
    formal_equality_addresses_substantive_inequality,
    'Can a formal colorblind rule adequately address or dismantle substantive racial inequality produced by past and present structural forces?',
    'Empirical study of colorblind regimes (e.g., post-SFFA university admissions, California Prop 209, Michigan Proposal 2) measuring racial disparity trajectories vs. race-conscious regimes.',
    'If formal equality fails to reduce substantive disparities, the colorblind reading''s coordination function is questioned; if it succeeds, the reading''s claim to solve the founding problem is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equality_addresses_substantive_inequality, empirical, 'Whether formal non-classification suffices for substantive equality').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (colorblind_reading) of the contested kernel equal_protection_clause. Sibling readings: remedial_reading, diversity_reading. What structural elements do the readings disagree on?',
    'Map each reading''s beneficiary/victim structure, ε referent, and claimed_type. The disagreement is located in: (1) whether race-conscious government action creates victims (colorblind: yes; remedial/diversity: no, or different victims), (2) whether the founding problem is live (colorblind: contested/dead; remedial: live), (3) ε level (colorblind: ~0.05; remedial/diversity: higher due to ongoing remediation apparatus).',
    'Each reading instantiates a different constraint with different ε, different stakeholders, different classification. The kernel contest is not a measurement parameter but a structural decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame decomposition: colorblind_reading vs remedial_reading vs diversity_reading as distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ep_colorblind_tr_t28, equal_protection_clause__colorblind_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(ep_colorblind_tr_t56, equal_protection_clause__colorblind_reading, theater_ratio, 56, 0.22).
narrative_ontology:measurement(ep_colorblind_tr_t80, equal_protection_clause__colorblind_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(ep_colorblind_tr_t100, equal_protection_clause__colorblind_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(ep_colorblind_tr_t130, equal_protection_clause__colorblind_reading, theater_ratio, 130, 0.3).
narrative_ontology:measurement(ep_colorblind_tr_t150, equal_protection_clause__colorblind_reading, theater_ratio, 150, 0.25).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t28, equal_protection_clause__colorblind_reading, base_extractiveness, 28, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t56, equal_protection_clause__colorblind_reading, base_extractiveness, 56, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t80, equal_protection_clause__colorblind_reading, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t100, equal_protection_clause__colorblind_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t130, equal_protection_clause__colorblind_reading, base_extractiveness, 130, 0.05).
narrative_ontology:measurement(ep_colorblind_be_t150, equal_protection_clause__colorblind_reading, base_extractiveness, 150, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ep_colorblind_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ep_colorblind_su_t28, equal_protection_clause__colorblind_reading, suppression_requirement, 28, 0.12).
narrative_ontology:measurement(ep_colorblind_su_t56, equal_protection_clause__colorblind_reading, suppression_requirement, 56, 0.12).
narrative_ontology:measurement(ep_colorblind_su_t80, equal_protection_clause__colorblind_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(ep_colorblind_su_t100, equal_protection_clause__colorblind_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement(ep_colorblind_su_t130, equal_protection_clause__colorblind_reading, suppression_requirement, 130, 0.12).
narrative_ontology:measurement(ep_colorblind_su_t150, equal_protection_clause__colorblind_reading, suppression_requirement, 150, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, information_standard).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.02).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of the equal_protection_clause kernel into three constraint stories. The colorblind_reading claims Mountain (formal equality, ε≈0.05). The remedial_reading and diversity_reading claim Tangled Rope or Scaffold (ongoing remediation/diversity apparatus with coordination function but asymmetric extraction). All three linked via affects_constraints. The upstream constraint (colorblind principle) is often cited as limiting the downstream constraints (remedial/diversity permissions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__colorblind_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_clause__colorblind_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
