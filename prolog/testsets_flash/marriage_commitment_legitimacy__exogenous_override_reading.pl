% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story represents the 'exogenous override' reading of the
 *   LDS Church's 1890 Manifesto, which formally ended the practice of plural
 *   marriage. In this reading, the Manifesto is understood as a direct
 *   capitulation to overwhelming federal coercion, with the theological
 *   doctrine of plural marriage remaining unchanged but its practice
 *   suspended under duress. The federal government is the primary
 *   beneficiary, extracting institutional compliance, while LDS members and
 *   institutional authority are victims bearing the costs of doctrinal
 *   abandonment and a legitimacy crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '02b84a22-819c-4b55-a119-2444a02fc17a').
narrative_ontology:cs_kernel_codification('02b84a22-819c-4b55-a119-2444a02fc17a', fixed_text).
narrative_ontology:cs_authority_grounding('02b84a22-819c-4b55-a119-2444a02fc17a', extraction).
narrative_ontology:cs_interpretation_layer_present('02b84a22-819c-4b55-a119-2444a02fc17a').
narrative_ontology:cs_reading_relation('02b84a22-819c-4b55-a119-2444a02fc17a', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('02b84a22-819c-4b55-a119-2444a02fc17a', marriage_commitment_legitimacy__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('02b84a22-819c-4b55-a119-2444a02fc17a', foundational, divine_command_unchangeable_by_secular_force).
narrative_ontology:cs_axiom_status(divine_command_unchangeable_by_secular_force, holdable).
narrative_ontology:cs_axiom_grounding('02b84a22-819c-4b55-a119-2444a02fc17a', divine_command_unchangeable_by_secular_force, deontological).
narrative_ontology:cs_axiom('02b84a22-819c-4b55-a119-2444a02fc17a', secondary, institutional_survival_requires_external_compliance).
narrative_ontology:cs_axiom_status(institutional_survival_requires_external_compliance, holdable).
narrative_ontology:cs_axiom_grounding('02b84a22-819c-4b55-a119-2444a02fc17a', institutional_survival_requires_external_compliance, instrumental).
narrative_ontology:cs_reference_frame('02b84a22-819c-4b55-a119-2444a02fc17a', divine_command_plural_marriage).
narrative_ontology:cs_drift_state('02b84a22-819c-4b55-a119-2444a02fc17a', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('02b84a22-819c-4b55-a119-2444a02fc17a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_american_society).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted legal and political pressure, including confiscation of property and disenfranchisement, to force the LDS Church to abandon plural marriage. Benefited from enforcing national legal norms and asserting federal supremacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Issued the Manifesto under duress, suspending the practice of plural marriage to preserve the institution from existential threat. Paid the cost of institutional capitulation and managing internal dissent, while attempting to preserve theological integrity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_authority, payer,
    organized, generational, identity_locked, global).

% Were forced to abandon a deeply held religious practice, facing social ostracization, legal penalties, and a crisis of faith regarding the nature of prophetic authority and divine command. Paid the cost of doctrinal abandonment and personal sacrifice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_members, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the perceived 'civilizing' of the LDS Church and the enforcement of monogamous norms, which aligned with prevailing social and moral standards. Saw the federal government's actions as a victory for national unity and moral order.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_american_society, beneficiary,
    organized, generational, mobile, national).

% Born into a church where plural marriage was no longer practiced, they inherit the theological and historical ambiguities of the Manifesto without direct experience of the coercion. They would question the nature of divine command and institutional integrity if fully aware of the duress.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, future_generations_lds, excluded,
    powerless, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto coordinated the LDS Church's institutional survival by aligning its practices with federal law, preventing further persecution and property confiscation.
% TRANSFER_FUNCTION: Transferred the right to define and enforce marriage norms from the LDS Church to the federal government, and transferred the cost of this capitulation onto LDS institutional authority and its members.
% ABSENT_VOICES: Those members who felt the Manifesto was a betrayal of divine command and chose to continue plural marriage in secret or leave the church; their voices were suppressed by institutional pressure and federal enforcement. Future generations, who inherit the ambiguity, are also absent from the original decision.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished, the LDS Church's relationship with the federal government would be fundamentally altered, and the theological justification for its current monogamous practice would be destabilized, potentially leading to a re-evaluation of historical doctrine and practice.
% FOUNDING_PROBLEM: The federal government faced a challenge to its legal and moral authority from the LDS Church's practice of plural marriage, which it viewed as a violation of national norms and a threat to social order.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the LDS Church corroborate that the federal government's problem of enforcing monogamy was resolved by the Manifesto. Within the LDS Church, the problem is framed as 'preserving the Church,' which is still live, but the original federal challenge is dead.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully imposed its will, forcing a fundamental change in religious practice. Suppression is very high (0.9) due to the severe legal and political penalties (disenfranchisement, property confiscation) used to enforce compliance. Theater ratio is moderate-high (0.6) as the public declaration of ending plural marriage was a performance of compliance, while internal theological commitments and some clandestine practices persisted for a time. Resistance is high (0.75) reflecting the significant internal struggle and external pressure required to achieve this capitulation.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary enforcement of law and order, a 'rope' to bring a deviant practice into line. From the perspective of many LDS members at the time, it was a 'snare' – a coercive imposition that extracted their religious freedom and practice under threat of institutional destruction. The engine's classification as 'snare' from the victim seats reflects this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and mainstream American society are clear beneficiaries (d near 0.0) as they achieved their policy goals and saw their norms vindicated. LDS institutional authority and individual members are targets (d near 1.0) as they bore the direct costs of coercion, including loss of property, civil rights, and a profound challenge to their religious identity and practice. The 'identity_locked' exit option for LDS members reflects the deep fusion of their identity with their religious community, making true exit extremely costly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_integrity_vs_practice,
    'To what extent did the theological doctrine of plural marriage remain ''unchanged'' versus undergoing a de facto reinterpretation or recontextualization due to the suspension of practice?',
    'Analysis of post-Manifesto theological discourse, sermons, and official statements for subtle shifts in doctrinal emphasis or interpretation, particularly regarding the ''eternity'' of plural marriage covenants.',
    'If significant reinterpretation occurred, the ''exogenous override'' reading''s claim of ''doctrine unchanged'' is weakened, moving it closer to the ''endogenous reinterpretation'' reading. This would reduce the perceived extractiveness from the ''theological abandonment'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_practice, conceptual, 'Ambiguity regarding the persistence of theological doctrine versus the suspension of practice.').

omega_variable(
    extent_of_clandestine_practice,
    'What was the actual extent and duration of clandestine plural marriage practices after the Manifesto, and how did institutional authority manage or suppress these?',
    'Historical research into private records, diaries, and oral histories, cross-referenced with institutional disciplinary actions and excommunications related to post-Manifesto plural marriages.',
    'A higher degree of clandestine practice would indicate a greater ''theater_ratio'' and ''suppression'' within the LDS institution itself, as it enforced the federal mandate while managing internal dissent. This would amplify the ''snare'' classification for members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_clandestine_practice, empirical, 'The actual degree of compliance versus continued clandestine practice.').

omega_variable(
    legitimacy_crisis_magnitude,
    'How profound and widespread was the ''legitimacy crisis'' among LDS members regarding prophetic authority after the Manifesto, and how was it resolved or managed over time?',
    'Sociological and historical analysis of member testimonies, apostasy rates, and the development of apologetic narratives within the Church to reconcile the change with divine command.',
    'A more severe and prolonged crisis would underscore the high ''extractiveness'' and ''suppression'' experienced by members, reinforcing the ''snare'' classification. The resolution mechanism would inform the ''mandatrophy_analysis'' of the constraint''s internal dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_crisis_magnitude, empirical, 'The impact of the Manifesto on member faith and institutional legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1898, 0.55).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1894, 0.8).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1898, 0.83).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1894, 0.85).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1898, 0.88).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_property_rights).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, lds_political_participation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. The other readings are 'endogenous_reinterpretation_reading' and 'hybrid_pragmatic_reading', each representing a distinct structural interpretation of the Manifesto's impact on LDS doctrine and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
