% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto: Institutional Pragmatism Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 1890 Manifesto as a strategic
 *   institutional adaptation by the church leadership to external federal
 *   coercion. The Manifesto publicly suspended the practice of plural
 *   marriage, but this reading emphasizes that the underlying doctrinal claim
 *   was not repudiated, and the practice continued secretly for a period
 *   (1890-1904). The 'revelation narrative' served as a legitimating cover
 *   for a survival-driven capitulation to superior coercive power. The
 *   constraint is classified as a Tangled Rope because it served a genuine
 *   coordination function (institutional survival) but involved significant
 *   asymmetric extraction from individual members and relied heavily on
 *   theatricality and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.75).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto: Institutional Pragmatism Reading").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1').
narrative_ontology:cs_kernel_codification('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', formalized).
narrative_ontology:cs_authority_grounding('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', lineage).
narrative_ontology:cs_interpretation_layer_present('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1').
narrative_ontology:cs_reading_relation('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', foundational, revelation_as_institutional_tool).
narrative_ontology:cs_axiom_status(revelation_as_institutional_tool, holdable).
narrative_ontology:cs_axiom_grounding('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', revelation_as_institutional_tool, conventional).
narrative_ontology:cs_reference_frame('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', institutional_survival_imperative).
narrative_ontology:cs_drift_state('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f384ac77-e8c0-4bf0-b8b3-a634d0ac1ab1', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the Manifesto, publicly suspending plural marriage while privately allowing its continuation for a period. Benefited from restored political rights and institutional survival, but faced internal dissent and external pressure.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Were compelled to abandon or conceal plural marriages, facing social ostracism, legal penalties, and spiritual confusion. Their identity was deeply tied to the practice, making exit from the church unthinkable.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, identity_locked, local).

% Were publicly assured that plural marriage had ceased, while some leaders continued the practice in secret. Faced a crisis of trust and potential spiritual harm upon later discovery of the deception.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, constrained, local).

% Exerted coercive power through legislation, arrests, and property confiscation, forcing the church to abandon plural marriage to secure statehood and political rights. Its goal was the cessation of the practice, not its legitimation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% The abstract concept of the church's continued existence and growth, which was directly enabled by the Manifesto's strategic adaptation to external pressure.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the church's public stance on plural marriage with the demands of the federal government, allowing the institution to survive and gain political legitimacy while preserving a core doctrinal claim through suspension rather than repudiation.
% TRANSFER_FUNCTION: Transferred political and legal autonomy from the church to the federal government in exchange for institutional survival and restored civil rights. It also transferred the burden of doctrinal ambiguity and personal sacrifice onto individual members.
% ABSENT_VOICES: Radical polygamist factions who refused to abandon the practice and were later excommunicated; they would argue for continued resistance and adherence to the original doctrine, but were marginalized by the institutional leadership.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its subsequent enforcement had vanished, the church would likely have faced continued federal persecution, potentially leading to its dissolution or forced relocation, and the social and political landscape of the American West would have been dramatically different.
% FOUNDING_PROBLEM: The church faced existential threat from the federal government due to its practice of plural marriage, risking disincorporation, property confiscation, and the disenfranchisement of its members.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal legislation, and contemporary journalistic accounts from outside the church leadership corroborate the severe existential threat posed by the federal government's anti-polygamy campaign. The problem was resolved by the Manifesto's public suspension of the practice.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because individual members bore the costs of doctrinal ambiguity, personal sacrifice, and legal risk, while the institution secured its survival. Suppression is high due to both federal coercion and internal church discipline against dissenters. Theater ratio is very high, reflecting the gap between the public declaration of cessation and the private continuation of plural marriage, with the 'revelation' serving as a performative justification for a pragmatic decision. Accessibility collapse is moderate as some members found ways to continue the practice secretly or left the church, but the institutional pressure was immense. Resistance was high from both federal authorities (prior to the Manifesto) and internal dissenters (after the Manifesto).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, the Manifesto was a necessary, divinely guided act of preservation. From the perspective of coerced polygamists, it was a traumatic imposition that forced them to abandon deeply held beliefs and practices. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership acted as the agenda-setter, navigating the crisis and benefiting from institutional survival. Coerced polygamists and deceived monogamists were the primary payers, bearing the direct costs of the policy shift. The federal government, while an external enforcer, also acted as an agenda-setter, dictating the terms of the church's survival. Institutional survival itself is an abstract beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to ensure institutional survival in the face of federal persecution. While the immediate threat of disincorporation was resolved, the underlying doctrinal tension and the use of revelation to legitimate pragmatic shifts continued to shape the institution. The high theater ratio and contested founding problem status indicate a significant gap between the stated purpose and the actual operation, preventing mislabeling as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_secret_continuation,
    'What was the true extent and duration of secret plural marriage practices after the 1890 Manifesto, and how widely was this known within the church hierarchy?',
    'Further archival research into private diaries, letters, and church disciplinary records from the 1890-1904 period, combined with demographic analysis of birth records.',
    'A higher confirmed extent of secret practice would increase the ''theater_ratio'' and ''extractiveness'' metrics for the church leadership seat, reinforcing the ''tangled_rope'' classification by highlighting the internal deception and burden on members. A lower extent would shift the constraint closer to a ''scaffold'' or ''rope'' if the public cessation was largely genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_secret_continuation, empirical, 'Ambiguity regarding the actual implementation of the Manifesto''s public declaration.').

omega_variable(
    doctrinal_vs_pragmatic_motivation,
    'To what extent was the 1890 Manifesto driven by genuine prophetic inspiration versus institutional pragmatism and political necessity?',
    'Conceptual analysis of the historical context, theological arguments, and the outcomes of the Manifesto, comparing them against alternative historical paths and counterfactuals. This is a conceptual omega, not purely empirical.',
    'If primarily pragmatic, this reading''s ''tangled_rope'' classification is strongly supported, emphasizing the extractive and theatrical aspects. If primarily prophetic, it would align more with the ''endogenous reinterpretation reading'', potentially shifting the classification towards a ''rope'' or even ''mountain'' from the perspective of believers, by re-framing the extraction as a divinely ordained sacrifice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_vs_pragmatic_motivation, conceptual, 'The irreducible ambiguity between divine command and institutional survival as the primary driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.7).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.78).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.82).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.85).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.65).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.68).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.72).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.75).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'plural_marriage_mandate' kernel, each representing a distinct structural claim about the 1890 Manifesto. This 'institutional pragmatism' reading focuses on the strategic adaptation and legitimation claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
