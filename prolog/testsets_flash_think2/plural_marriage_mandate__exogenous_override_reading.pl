% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Exogenous Coercion
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto from the perspective
 *   that it represents federal coercion forcing the abandonment of a divine
 *   requirement, rather than a legitimate doctrinal reinterpretation. The
 *   federal government, through legislative acts and judicial enforcement,
 *   exerted immense pressure on the religious community, leading to the
 *   Manifesto as a means of institutional survival. This reading emphasizes
 *   the high suppression costs borne by practicing polygamists and the
 *   extractive nature of the state's actions to achieve social and political
 *   conformity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.85).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Exogenous Coercion").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '9b65d56b-a1c8-4e78-a910-9f46300b0f82').
narrative_ontology:cs_kernel_codification('9b65d56b-a1c8-4e78-a910-9f46300b0f82', formalized).
narrative_ontology:cs_authority_grounding('9b65d56b-a1c8-4e78-a910-9f46300b0f82', extraction).
narrative_ontology:cs_reading_relation('9b65d56b-a1c8-4e78-a910-9f46300b0f82', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('9b65d56b-a1c8-4e78-a910-9f46300b0f82', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('9b65d56b-a1c8-4e78-a910-9f46300b0f82', foundational, divine_mandate_immutable_by_state_power).
narrative_ontology:cs_axiom_status(divine_mandate_immutable_by_state_power, holdable).
narrative_ontology:cs_axiom_grounding('9b65d56b-a1c8-4e78-a910-9f46300b0f82', divine_mandate_immutable_by_state_power, theological).
narrative_ontology:cs_axiom('9b65d56b-a1c8-4e78-a910-9f46300b0f82', foundational, state_coercion_illegitimate_in_religious_matters).
narrative_ontology:cs_axiom_status(state_coercion_illegitimate_in_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('9b65d56b-a1c8-4e78-a910-9f46300b0f82', state_coercion_illegitimate_in_religious_matters, deontological).
narrative_ontology:cs_reference_frame('9b65d56b-a1c8-4e78-a910-9f46300b0f82', divine_command_sovereignty).
narrative_ontology:cs_drift_state('9b65d56b-a1c8-4e78-a910-9f46300b0f82', post_1890_manifesto, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9b65d56b-a1c8-4e78-a910-9f46300b0f82', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, mainstream_american_society).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_activists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, religious_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used legislative and judicial power (e.g., Edmunds-Tucker Act, Supreme Court rulings) to criminalize plural marriage, seize church property, and disenfranchise polygamists, forcing the abandonment of the practice to achieve territorial conformity and statehood for Utah.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faced imprisonment, fines, property confiscation, and social ostracization for continuing to practice plural marriage after federal anti-polygamy laws were enacted. Their options were to abandon their religious practice, go into hiding, or face severe legal consequences.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, local).

% As members of the broader religious community, they were compelled to conform to the Manifesto's declaration, abandoning a practice they believed to be divinely commanded. While not all were directly prosecuted, the institutional pressure and legal threat affected their collective identity and practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, religious_community_members, payer,
    moderate, biographical, constrained, national).

% Benefited from the perceived social and moral conformity achieved by the federal government's actions, aligning the territory with prevailing national norms regarding marriage and social order. This facilitated Utah's path to statehood.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, mainstream_american_society, beneficiary,
    organized, biographical, mobile, national).

% Issued the 1890 Manifesto under extreme duress, facing the imminent destruction of the church's temporal assets and the continued imprisonment of its members. From this reading, their action was a forced capitulation to superior coercive power, not a voluntary doctrinal change.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, religious_leadership, agenda_setter,
    powerful, generational, constrained, national).

% Advocated for federal intervention against plural marriage, viewing it as a moral and social evil. Their efforts culminated in the federal coercion that led to the Manifesto, achieving their policy goals.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_activists, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, from this reading, primarily served to coordinate federal law and social norms across the United States, forcing a religious minority to conform to mainstream American social and legal standards regarding marriage and territorial governance.
% TRANSFER_FUNCTION: Transferred the right to practice plural marriage from the religious community to the federal government's control, effectively transferring property and freedom from polygamists to the state, and ensuring social conformity to national norms.
% ABSENT_VOICES: Those who believed plural marriage was a divine command and refused to abandon it, often driven underground or into exile. Their voices were suppressed by legal and social pressure, and their continued practice was criminalized.
% DISAPPEARANCE_RATIONALE: If the federal coercion and the subsequent Manifesto had not occurred, the religious community's history, doctrinal development, and relationship with the U.S. government would be fundamentally different. The social and legal landscape of the American West would also have evolved along a different path, with ongoing conflict over religious freedom and state sovereignty.
% FOUNDING_PROBLEM: The federal government's desire to assert sovereignty over territories and enforce a singular vision of marriage and social order, viewing plural marriage as an affront to American values and a barrier to statehood for Utah.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of federal legislation (e.g., Edmunds-Tucker Act), court cases, and military actions; accounts from polygamist families detailing arrests, property seizures, and forced displacement; scholarly historical analyses of the period. The tension between religious freedom and state power, and the legacy of this coercion, remains a live issue for some descendants and scholars, corroborated by ongoing legal and social debates.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at the peak of coercion) because the federal government effectively seized control over a core religious practice and extracted conformity, property, and freedom from the religious community. Suppression is very high (0.92) due to the direct and severe legal penalties, property confiscation, and disenfranchisement used to enforce the abandonment of plural marriage. The theater ratio is low (0.10) because the coercion was overt and effective, with little performative cover for its direct impact. Resistance was initially high but gradually diminished under overwhelming state power, leading to the Manifesto.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal government and mainstream society, the actions taken were legitimate enforcement of law and promotion of social order. However, from the perspective of practicing polygamists and this reading, the same actions constituted illegitimate coercion and extraction, forcing the abandonment of a divinely mandated practice. The engine's classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government, mainstream American society, and anti-polygamy activists are clear beneficiaries, achieving their goals of social conformity and state sovereignty. Practicing polygamists and the broader religious community are the primary victims, bearing the direct costs of legal penalties, loss of property, and the forced abandonment of a religious tenet. The religious leadership, while acting as agenda-setters for their community, were themselves constrained actors operating under extreme duress.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_coercion,
    'Was the 1890 Manifesto a legitimate prophetic reinterpretation of divine will, or a forced capitulation to federal coercion?',
    'Analysis of internal church records and prophetic statements for evidence of independent doctrinal evolution versus external government documents detailing coercive pressures and threats.',
    'If divine reinterpretation, the constraint''s extractiveness would be lower, and its claimed type might shift towards a Rope or Scaffold (internal coordination). If forced capitulation, the Snare classification and high extractiveness are strongly supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_will_vs_coercion, conceptual, 'Ambiguity regarding the true nature of the Manifesto''s origin.').

omega_variable(
    voluntary_abandonment_vs_forced_conformity,
    'To what extent was the abandonment of plural marriage a voluntary act of religious obedience, versus a forced conformity under duress?',
    'Examination of individual testimonies, diaries, and historical accounts from the period, particularly from those who continued to practice plural marriage in secret or exile, to gauge the perceived voluntariness of compliance.',
    'If largely voluntary, the suppression metric would be lower. If primarily forced, the high suppression and Snare classification are reinforced, highlighting the victim status of those who complied under threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_abandonment_vs_forced_conformity, empirical, 'The degree of voluntariness in the abandonment of plural marriage.').

omega_variable(
    institutional_survival_vs_doctrinal_integrity,
    'Was the primary motivation for the Manifesto the preservation of the church''s salvific mission (institutional survival), or the maintenance of doctrinal integrity in the face of external threat?',
    'Comparative analysis of the rhetoric used by church leaders at the time versus their private communications and later historical reflections, alongside the specific threats posed by federal legislation.',
    'If institutional survival was paramount, it supports the idea of a pragmatic response to coercion. If doctrinal integrity was the driving force, it would suggest a different internal logic for the decision, potentially altering the interpretation of the ''victim'' role for the leadership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_survival_vs_doctrinal_integrity, conceptual, 'The underlying motivation for the religious leadership''s decision to issue the Manifesto.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1880, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(plur_tr_t1885, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1885, 0.12).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(plur_tr_t1895, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1895, 0.1).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1880, 0.7).
narrative_ontology:measurement(plur_be_t1885, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1885, 0.78).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(plur_be_t1895, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1895, 0.83).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1900, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(plur_su_t1885, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1885, 0.85).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.92).
narrative_ontology:measurement(plur_su_t1895, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1895, 0.9).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1900, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
