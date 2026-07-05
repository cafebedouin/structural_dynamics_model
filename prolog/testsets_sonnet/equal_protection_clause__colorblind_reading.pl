% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Equal Protection Clause — Colorblind (Anti-Classification) Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates the colorblind (anti-classification) reading of
 *   the Equal Protection Clause as its own structurally clean constraint: any
 *   governmental use of race, whether invidious or remedial, is treated as
 *   constitutionally suspect and generally impermissible; the rights-bearer
 *   is the individual, never a racial group; and any race-conscious
 *   accommodation — however well-intentioned — creates individual victims
 *   among those disadvantaged by the classification. This is a distinct
 *   constraint from the remedial and diversity readings of the SAME clause
 *   text (constraint_ids: equal_protection_clause__remedial_reading,
 *   equal_protection_clause__diversity_reading, generated separately). All
 *   three readings share the kernel (the Fourteenth Amendment's equal
 *   protection text) but instantiate structurally different
 *   beneficiary/victim sets, different ε, and different persistence claims —
 *   per the ε-invariance principle, they are not one constraint measured
 *   three ways, they are three constraints.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers: primary declared beneficiary (moderate/analytical) — the doctrine's rights-holder
 *   - rejected_majority_group_applicants: paradigmatic litigant beneficiary (moderate/constrained) — vindicated when race-conscious policy is struck
 *   - beneficiaries_of_race_conscious_remedies: structural victim (powerless/trapped) — loses access to remedy once classification itself is forbidden
 *   - courts_administering_strict_scrutiny: agenda_setter (institutional/analytical) — holds interpretive discretion the doctrine formally denies to legislatures
 *   - civil_rights_historians_and_naacp_ldf: excluded analytical voice (organized/constrained) — genealogy contests the doctrine's claimed permanence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.18).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.42).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause — Colorblind (Anti-Classification) Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).
domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'f41fb957-4d4a-4f05-9c8a-0979091af09c').
narrative_ontology:cs_kernel_codification('f41fb957-4d4a-4f05-9c8a-0979091af09c', fixed_text).
narrative_ontology:cs_authority_grounding('f41fb957-4d4a-4f05-9c8a-0979091af09c', lineage).
narrative_ontology:cs_interpretation_layer_present('f41fb957-4d4a-4f05-9c8a-0979091af09c').
narrative_ontology:cs_reading_relation('f41fb957-4d4a-4f05-9c8a-0979091af09c', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('f41fb957-4d4a-4f05-9c8a-0979091af09c', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('f41fb957-4d4a-4f05-9c8a-0979091af09c', foundational, race_classification_always_suspect_regardless_of_purpose).
narrative_ontology:cs_axiom_status(race_classification_always_suspect_regardless_of_purpose, holdable).
narrative_ontology:cs_axiom_grounding('f41fb957-4d4a-4f05-9c8a-0979091af09c', race_classification_always_suspect_regardless_of_purpose, deontological).
narrative_ontology:cs_axiom('f41fb957-4d4a-4f05-9c8a-0979091af09c', foundational, individual_not_group_is_sole_constitutional_rights_bearer).
narrative_ontology:cs_axiom_status(individual_not_group_is_sole_constitutional_rights_bearer, holdable).
narrative_ontology:cs_axiom_grounding('f41fb957-4d4a-4f05-9c8a-0979091af09c', individual_not_group_is_sole_constitutional_rights_bearer, conventional).
narrative_ontology:cs_reference_frame('f41fb957-4d4a-4f05-9c8a-0979091af09c', post_reconstruction_anti_caste_principle).
narrative_ontology:cs_drift_state('f41fb957-4d4a-4f05-9c8a-0979091af09c', contemporary_strict_scrutiny_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f41fb957-4d4a-4f05-9c8a-0979091af09c', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_rights_bearers).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, beneficiaries_of_race_conscious_remedies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, state_and_local_governments).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individualism_as_constitutional_first_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, every person is entitled to be judged without reference to racial classification by the state, regardless of which racial group they belong to. This is framed as a universal, symmetric protection — not group-specific — and applies equally to a white applicant denied a seat and a Black plaintiff denied a contract on racial grounds. Their exit is analytical: the protection is doctrinal, not something they can decline or trade.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_rights_bearers, beneficiary,
    moderate, civilizational, analytical, national).

% Applicants (often, though not exclusively, white or Asian-American in the litigated cases) denied admission, employment, or contracts where race-conscious policies factored into the decision. Under the colorblind reading they are the paradigmatic rights-bearer whose individual claim is vindicated when race-conscious policy is struck down. They litigate as individuals, not as a racial class, and their only real recourse is judicial — they cannot simply exit the applicant pool and still obtain the benefit sought.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, rejected_majority_group_applicants, payer).

% Groups and individuals who would have benefited from race-conscious admissions, hiring, or districting designed to offset historical subordination or achieve integration. Under the colorblind reading, any policy attentive to their group membership is itself the constitutional violation, so the remedy they would have received is foreclosed regardless of the underlying disparity. They have no exit from this outcome — the doctrine, once applied, forecloses the mechanism they would use to seek redress.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, beneficiaries_of_race_conscious_remedies, payer,
    powerless, generational, trapped, national).

% Federal and state courts apply strict scrutiny to any facially race-conscious government action, testing it against the anti-classification principle. Courts administer the doctrine, decide which policies survive, and their interpretive choices determine how absolute the colorblind rule actually is in practice — they hold the discretion the doctrine formally denies to legislatures and agencies.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, courts_administering_strict_scrutiny, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and scholars documenting the historical function of race-conscious remedy (school desegregation, voting rights enforcement, affirmative action) argue the colorblind reading was substantially absent from equal protection doctrine until the late twentieth century and that its ascendance coincides with, and enables, the rollback of group remedies. Their historical-genealogy argument is not part of the litigated doctrinal record the colorblind reading rests on and is largely excluded from the controlling case law's own reasoning.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_historians_and_naacp_ldf, excluded,
    organized, generational, constrained, national).

% Legislatures, school boards, and agencies that might otherwise design race-conscious policy (targeted remediation, integration plans, contracting set-asides) must instead design facially race-neutral alternatives or abandon the goal, bearing compliance and litigation costs and often achieving weaker versions of their original policy aims.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, state_and_local_governments, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, state_and_local_governments, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administrable rule — no governmental use of race, period — that is comparatively easy for courts to apply consistently across jurisdictions and policy domains without adjudicating the merits or history of any particular racial classification.
% TRANSFER_FUNCTION: Moves the benefit of individualized, race-blind treatment to any individual challenging a race-conscious policy, and moves the cost — foreclosure of group-targeted remedy — onto those groups whose disadvantage the race-conscious policy was designed to address.
% ABSENT_VOICES: Communities that experienced the concrete, measurable effects of historical group subordination (segregated schooling, redlining, disenfranchisement) are not represented as groups in this doctrine's own reasoning — the doctrine's individualist premise structurally declines to hear group-level harm as a category of injury at all, so their voice is foreclosed by the frame itself, not merely absent from a particular case.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished as controlling doctrine, race-conscious remediation and diversity-based policy would regain constitutional headroom; admissions, contracting, and districting practice would shift substantially, and the litigation posture of both plaintiffs and governments would reorganize around the remedial or diversity framework instead.
% FOUNDING_PROBLEM: The colorblind reading traces its lineage to the dissent tradition (Plessy) and later majority doctrine holding that governmental sorting of citizens by race is inherently suspect regardless of asserted benevolent purpose, built to prevent both invidious segregationist classification and, later, any racial classification at all — including remedial ones.
% FOUNDING_PROBLEM_CORROBORATION: Originalist and formalist legal scholars attest the anti-classification principle is a stable, unbroken constitutional commitment tracing to the Fourteenth Amendment's text. Legal historians and civil rights litigators outside that tradition attest the doctrine's absolutist form is a late-twentieth-century judicial construction that departed from decades of doctrine tolerating remedial race-consciousness — this corroboration comes from historians and litigators who are not beneficiaries of the colorblind rule's individual-plaintiff wins.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.18 at interval end) because the colorblind reading, taken purely as formal rule application — race is not a permissible sorting criterion — has genuinely minimal per-transaction extraction: it does not itself redistribute resources, it forbids a category of redistribution mechanism. Suppression is authored moderate and rising (0.10 to 0.42) because the doctrine's practical operation increasingly requires active judicial intervention to strike down race-conscious policies that legislatures and institutions continue to attempt — the suppression is the doctrine's enforcement against persistent remedial and diversity-based policymaking, not suppression of individual dissent. Theater ratio stays low throughout — this is a doctrine genuinely applied in courts with real stakes, not performative maintenance. Accessibility collapse (0.6) and resistance (0.55) are both mid-range rather than mountain-typical extremes: this reflects the authored judgment that the colorblind reading, unlike a true natural law, remains a contested doctrinal choice actively resisted by remedial and diversity constituencies, even though its proponents claim mountain-like permanence and naturalness.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual litigants challenging race-conscious policy are the clearest beneficiaries — the doctrine directly vindicates their claim and imposes essentially no cost on them structurally. Beneficiaries of race-conscious remedy are the clearest victims: the doctrine, applied consistently, forecloses the policy mechanism that would have benefited them, regardless of the underlying disparity the remedy targeted. Courts sit as agenda_setter/analytical because they administer and interpret the doctrine's boundaries rather than experiencing its distributive effects directly. State and local governments are payers because they bear compliance and redesign costs when race-conscious policy tools are foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing captures the doctrine's genealogical contest directly: proponents (largely coextensive with its declared beneficiaries) attest the anti-classification principle is textually permanent and was always the correct reading; historians and litigators outside that beneficiary set attest the absolutist form is a later judicial construction departing from a prior tolerance for remedial race-consciousness. This mismatch — status contested, corroboration split along beneficiary lines — is exactly the R5 signal the framework is built to surface rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_reading_naturalness_vs_construction,
    'Is the anti-classification (colorblind) principle a natural, textually compelled reading of equal protection that has always been latent in the Fourteenth Amendment, or is it a constructed doctrinal choice that gained ascendance in a specific historical-political period and benefits identifiable litigant classes?',
    'Historical-doctrinal analysis tracing the actual case law from Reconstruction through the present, comparing periods where race-conscious remedy was judicially tolerated against periods of anti-classification ascendance; corroboration from historians outside the beneficiary set (rejected majority-group applicants, formalist legal scholars) versus corroboration from within it.',
    'If constructed, the doctrine functions closer to a tangled_rope or scaffold serving identifiable beneficiaries at the expense of identifiable victims rather than a mountain of textual necessity; if genuinely compelled by the text and structurally inevitable regardless of who enforces it, the mountain claim holds despite declared beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_reading_naturalness_vs_construction, conceptual, 'Whether the colorblind reading is discovered constitutional bedrock or a constructed doctrinal choice with identifiable winners.').

omega_variable(
    kernel_committer_structure,
    'Given that the equal_protection_clause kernel supports at least three structurally distinct readings (colorblind, remedial, diversity) with different beneficiary/victim sets and different ε, which reading a court applies in a given case is itself a contested, unresolved committer choice rather than a determinate application of settled law.',
    'Track which reading controls across successive Supreme Court compositions and case lines; a stable, non-oscillating controlling reading over multiple decades would indicate genuine settlement, while oscillation indicates the kernel remains actively contested.',
    'If the colorblind reading becomes durably dominant, remedial and diversity readings become practically foreclosed even though this story does not claim to foreclose them structurally — the sibling stories should show declining viability metrics correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'The equal_protection_clause kernel is read differently by different judicial coalitions; this story is one reading among three live candidates.').

omega_variable(
    remedial_beneficiary_harm_measurement,
    'How should the harm to beneficiaries_of_race_conscious_remedies be measured when the counterfactual (what they would have received absent the colorblind rule) is itself contested and depends on which remedial policy would have been adopted?',
    'Empirical studies comparing outcomes for comparable cohorts across jurisdictions that retained versus abandoned race-conscious remedial policy under legal compulsion.',
    'A larger measured counterfactual harm would support a higher effective extraction reading of this constraint on the victim side than the low ε authored here, which reflects only the doctrine''s formal-rule-application character, not its downstream distributive effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_beneficiary_harm_measurement, empirical, 'Uncertainty in quantifying the foreclosed-remedy harm to the doctrine''s declared victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.07).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__colorblind_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(equa_tr_t2007, equal_protection_clause__colorblind_reading, theater_ratio, 2007, 0.11).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__colorblind_reading, theater_ratio, 2016, 0.13).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.06).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.09).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__colorblind_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement(equa_be_t2007, equal_protection_clause__colorblind_reading, base_extractiveness, 2007, 0.15).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__colorblind_reading, base_extractiveness, 2016, 0.16).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.18).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__colorblind_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(equa_su_t2007, equal_protection_clause__colorblind_reading, suppression_requirement, 2007, 0.34).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__colorblind_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This story, equal_protection_clause__remedial_reading, and equal_protection_clause__diversity_reading form a constraint family: three structurally distinct readings of the same equal_protection_clause kernel text, each with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle. The colorblind reading's ascendance in controlling case law structurally influences (without logically foreclosing, in the legal-doctrinal sense the framework tracks) the viability and litigation posture of the remedial and diversity readings — a rising colorblind reading corresponds to narrowing constitutional headroom for race-conscious remedy and diversity-interest policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
