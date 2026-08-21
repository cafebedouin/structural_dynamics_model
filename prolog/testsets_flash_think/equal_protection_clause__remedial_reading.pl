% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection: Race-Conscious Remediation Reading
 *   domain: Constitutional Law / Political Philosophy / Education Policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which mandates race-conscious policies to actively address and
 *   overcome the effects of historical group subordination and achieve
 *   substantive equality. It is a highly contested interpretation,
 *   particularly by those advocating for a 'colorblind' approach. This
 *   reading views race-conscious measures as a temporary necessity, intended
 *   to sunset once the remedial goals are achieved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.85).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.78).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection: Race-Conscious Remediation Reading").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "Constitutional Law / Political Philosophy / Education Policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '8753726f-b6b5-486d-841b-4f631d2ca0db').
narrative_ontology:cs_kernel_codification('8753726f-b6b5-486d-841b-4f631d2ca0db', fixed_text).
narrative_ontology:cs_authority_grounding('8753726f-b6b5-486d-841b-4f631d2ca0db', lineage).
narrative_ontology:cs_interpretation_layer_present('8753726f-b6b5-486d-841b-4f631d2ca0db').
narrative_ontology:cs_reading_relation('8753726f-b6b5-486d-841b-4f631d2ca0db', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('8753726f-b6b5-486d-841b-4f631d2ca0db', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('8753726f-b6b5-486d-841b-4f631d2ca0db', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('8753726f-b6b5-486d-841b-4f631d2ca0db', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('8753726f-b6b5-486d-841b-4f631d2ca0db', foundational, race_conscious_remedy_necessity).
narrative_ontology:cs_axiom_status(race_conscious_remedy_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8753726f-b6b5-486d-841b-4f631d2ca0db', race_conscious_remedy_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('8753726f-b6b5-486d-841b-4f631d2ca0db', post_reconstruction_amendment_intent).
narrative_ontology:cs_drift_state('8753726f-b6b5-486d-841b-4f631d2ca0db', contemporary_judicial_reinterpretation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8753726f-b6b5-486d-841b-4f631d2ca0db', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, educational_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the intended recipients of race-conscious remediation, designed to overcome the lingering effects of systemic discrimination. They benefit from policies that aim to level the playing field in education, employment, and other areas, but their 'exit' from the need for such policies is tied to the achievement of substantive equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, identity_locked, national).

% Individuals who, by virtue of not belonging to historically subordinated groups, may experience disadvantage (e.g., denial of admission, employment) due to race-conscious policies. They bear the direct costs of remediation, often perceiving it as reverse discrimination, with limited individual recourse.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups, payer,
    moderate, biographical, constrained, local).

% The judiciary, particularly the Supreme Court, interprets the Equal Protection Clause and determines the legality and scope of race-conscious remedial policies. They set the legal framework and enforce its application, navigating intense political and social pressure.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Universities, schools, and other public bodies are tasked with implementing race-conscious remedial policies. They bear the administrative and political costs of designing and defending these programs, often facing legal challenges and public scrutiny. They also act as agenda-setters in their specific domains.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter).

% Advocates for a strictly colorblind interpretation of the Equal Protection Clause, who argue that any racial classification is unconstitutional. They actively challenge race-conscious policies in courts and public discourse, seeking to dismantle the remedial reading's legal basis.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, biographical, mobile, national).

% While distinct from the remedial reading, advocates for diversity-based affirmative action often find common cause with the remedial reading in defending race-conscious policies. They observe and participate in the legal and political debates, though their primary justification differs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, diversity_advocates, observer,
    organized, biographical, mobile, national).

% Academics who analyze the historical, philosophical, and legal underpinnings of equal protection and race-conscious policies. They provide critical commentary, develop theoretical frameworks, and influence judicial and public understanding, but do not directly implement or enforce the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate societal efforts, particularly within public institutions, to actively dismantle the lingering effects of historical racial subordination and achieve a state of substantive equality for all groups.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from individuals who are not members of historically subordinated racial groups to those who are, as a means of rectifying past and ongoing systemic disadvantages.
% ABSENT_VOICES: Those who believe that all racial classifications are inherently discriminatory and that a truly equal society must be colorblind, regardless of historical context or remedial intent. Their arguments are often marginalized or dismissed within the framework of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal mandate for active, race-conscious remediation of historical group-based harms would disappear. Public institutions would likely revert to formally colorblind policies, which, without active intervention, would re-entrench existing racial inequalities and disparities across society.
% FOUNDING_PROBLEM: The persistence of profound racial inequality and group subordination in American society, stemming from centuries of slavery, Jim Crow laws, and systemic discrimination, which formal legal equality alone has failed to overcome.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, sociological studies on wealth and opportunity gaps, historical analyses of systemic racism, and a significant body of legal scholarship from outside the direct beneficiaries consistently attest to the ongoing nature of racial disparities and the problem's live status.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant societal restructuring and reallocation of opportunities required by this reading, impacting individuals not targeted for remediation. Suppression (0.78) is high due to the active enforcement needed to implement these policies against deeply entrenched resistance and legal challenges. The low theater ratio (0.15) indicates that the constraint is primarily functional, directly mandating action rather than merely performing. The claimed type is 'scaffold' because its justification is explicitly transitional—to achieve substantive equality, after which the race-conscious measures would ideally no longer be necessary.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a necessary, albeit temporary, mechanism for justice and true equality. From the perspective of individual members of non-preferred groups, it can be seen as an unfair imposition or 'reverse discrimination.' The courts, as agenda-setters, must balance these competing perspectives while adhering to constitutional principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups are the primary beneficiaries, as the constraint is designed to rectify systemic disadvantages they face. Individual members of non-preferred groups are the primary payers, as they may experience direct costs or foregone opportunities due to remedial policies. Courts act as agenda-setters, defining and enforcing the scope of these policies. Educational institutions are both payers (implementing and defending policies) and secondary agenda-setters within their domains.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criteria,
    'What objective, measurable criteria would signal that historical group subordination has been sufficiently remedied, allowing race-conscious policies to sunset?',
    'Empirical studies demonstrating the elimination of significant racial disparities in key socioeconomic indicators (e.g., wealth, education, health, incarceration rates) that are causally linked to historical subordination.',
    'If clear, agreed-upon criteria are established and met, the ''scaffold'' classification is validated, and the constraint could transition to a ''rope'' or ''piton'' as its function atrophies. If criteria remain undefined or perpetually unmet, the ''scaffold'' claim becomes performative, potentially reclassifying as a ''tangled_rope'' or ''snare'' if extraction persists without a clear end-state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_completion_criteria, conceptual, 'Defining the conditions for the sunset of race-conscious remediation.').

omega_variable(
    unintended_consequences_of_remediation,
    'To what extent do race-conscious remedial policies create new forms of stigma, resentment, or reinforce racial essentialism, thereby undermining their long-term goals?',
    'Longitudinal sociological and psychological studies tracking the social and individual impacts of race-conscious policies on both beneficiaries and non-beneficiaries, including measures of intergroup relations and identity formation.',
    'If significant negative unintended consequences are empirically demonstrated, the net benefit of the constraint could be questioned, potentially increasing its effective extractiveness or revealing a deeper coordination failure, pushing it towards a ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unintended_consequences_of_remediation, empirical, 'Assessing the unforeseen negative impacts of race-conscious remediation.').

omega_variable(
    causal_link_historical_subordination,
    'How directly and robustly can current racial disparities be causally linked to historical group subordination, as opposed to other socioeconomic factors?',
    'Advanced econometric and sociological modeling that controls for various confounding factors to isolate the causal impact of historical discrimination on contemporary outcomes.',
    'A weak or contested causal link would undermine the foundational empirical premise of this reading, weakening its legitimacy and potentially shifting its classification towards a ''snare'' if the remedial justification is seen as cover for other forms of resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_historical_subordination, empirical, 'Strength of the causal link between historical subordination and current disparities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1960, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1960, equal_protection_clause__remedial_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(equa_tr_t1975, equal_protection_clause__remedial_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__remedial_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(equa_tr_t2005, equal_protection_clause__remedial_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1960, equal_protection_clause__remedial_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(equa_be_t1975, equal_protection_clause__remedial_reading, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__remedial_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(equa_be_t2005, equal_protection_clause__remedial_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1960, equal_protection_clause__remedial_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(equa_su_t1975, equal_protection_clause__remedial_reading, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__remedial_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(equa_su_t2005, equal_protection_clause__remedial_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading has a unique structural profile and set of beneficiaries/victims, necessitating separate constraint stories linked by network effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
