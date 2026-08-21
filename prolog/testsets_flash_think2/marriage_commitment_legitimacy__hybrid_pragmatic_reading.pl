% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Religious Institution's Hybrid Pragmatic Adaptation to Marriage Norms
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the 'hybrid pragmatic' reading of a
 *   religious institution's adaptation to evolving marriage norms. This
 *   reading posits that the institution, facing an exogenous crisis (e.g.,
 *   federal legal challenges), strategically deployed its prophetic authority
 *   to reinterpret its core theological commitments, preserving its
 *   institutional form and legal standing through scope ambiguity. This is
 *   distinct from readings that see pure capitulation or pure divine
 *   reinterpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.7).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Religious Institution's Hybrid Pragmatic Adaptation to Marriage Norms").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '70f8904f-6ba8-4dde-9561-e83fafe58b03').
narrative_ontology:cs_kernel_codification('70f8904f-6ba8-4dde-9561-e83fafe58b03', fixed_text).
narrative_ontology:cs_authority_grounding('70f8904f-6ba8-4dde-9561-e83fafe58b03', lineage).
narrative_ontology:cs_interpretation_layer_present('70f8904f-6ba8-4dde-9561-e83fafe58b03').
narrative_ontology:cs_reading_relation('70f8904f-6ba8-4dde-9561-e83fafe58b03', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('70f8904f-6ba8-4dde-9561-e83fafe58b03', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('70f8904f-6ba8-4dde-9561-e83fafe58b03', foundational, institutional_preservation_is_paramount).
narrative_ontology:cs_axiom_status(institutional_preservation_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('70f8904f-6ba8-4dde-9561-e83fafe58b03', institutional_preservation_is_paramount, instrumental).
narrative_ontology:cs_axiom('70f8904f-6ba8-4dde-9561-e83fafe58b03', foundational, doctrinal_flexibility_through_scope_ambiguity).
narrative_ontology:cs_axiom_status(doctrinal_flexibility_through_scope_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('70f8904f-6ba8-4dde-9561-e83fafe58b03', doctrinal_flexibility_through_scope_ambiguity, conventional).
narrative_ontology:cs_reference_frame('70f8904f-6ba8-4dde-9561-e83fafe58b03', institutional_survival_with_doctrinal_integrity).
narrative_ontology:cs_drift_state('70f8904f-6ba8-4dde-9561-e83fafe58b03', contemporary_legal_and_social_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70f8904f-6ba8-4dde-9561-e83fafe58b03', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages the institution's adaptation to external legal and social pressures regarding marriage. Benefits from preserving the institution's legal standing and public legitimacy, and from maintaining doctrinal flexibility through reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Bear the costs of interpretive uncertainty and legitimacy ambiguity. Their identity is deeply tied to the institution, making exit difficult despite potential cognitive dissonance or dissatisfaction with the adaptation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, global).

% Exerted the exogenous pressure that necessitated the institutional adaptation. Benefits from the institution's compliance with federal law and evolving social norms.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyze the theological, historical, and sociological implications of the institution's adaptation, often highlighting the interpretive shifts and their consequences.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars, observer,
    analytical, generational, analytical, global).

% Reject the hybrid pragmatic reading, viewing it as either a capitulation to secular forces or a false reinterpretation of divine mandate. Their voices are often marginalized within the mainstream institutional discourse.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dissenting_factions, excluded,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the institution's response to changing legal and social norms regarding marriage, allowing it to maintain legal standing and public legitimacy while preserving core (reinterpreted) theological commitments.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and institutional stability to the leadership, while transferring interpretive uncertainty and legitimacy ambiguity to the rank-and-file members.
% ABSENT_VOICES: Dissenting factions who view the adaptation as a betrayal of core doctrine or as insufficient resistance to external pressure are structurally marginalized from the official discourse.
% DISAPPEARANCE_RATIONALE: If this hybrid pragmatic reading vanished, the institution would face an immediate crisis of legitimacy. It would either be seen as purely capitulating to external forces (exogenous override) or as having fundamentally altered its divine mandate (endogenous reinterpretation), leading to schism, legal challenges, and a reorganization of its internal and external relationships.
% FOUNDING_PROBLEM: The institution faced an exogenous crisis where its traditional theological commitments regarding marriage came into direct conflict with evolving federal law and social norms, threatening its legal status, tax-exempt status, and public legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal analyses of the period, and sociological studies of religious adaptation corroborate the existence and severity of the crisis. Independent theological scholarship also documents the interpretive shifts required to navigate this tension.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the adaptation provides institutional stability and flexibility to the leadership, but at the cost of interpretive clarity and potential cognitive dissonance for members. Suppression is high (0.70) as the institution actively manages and discourages alternative interpretations that might challenge the pragmatic adaptation. Theater ratio is moderate (0.40) reflecting the performative aspect of maintaining 'prophetic authority' while navigating a complex, externally driven change. Accessibility collapse is moderate (0.60) as members' interpretive options are narrowed, and resistance is moderate (0.50) due to internal questioning and dissenting factions.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this is a necessary and divinely guided adaptation for survival. From the rank-and-file members' perspective, it can be experienced as a top-down imposition that creates theological uncertainty and demands a re-alignment of personal beliefs with institutional pronouncements. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary, gaining flexibility and preserving the institution's legal status. Rank-and-file members are the victims, bearing the costs of interpretive ambiguity and potential identity strain. The federal government acts as an external agenda-setter, influencing the constraint's formation. Dissenting factions are excluded, their alternative readings suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the adaptation as a pure Rope (genuine coordination with symmetric benefits) by highlighting the asymmetric extraction from members and the active enforcement required to maintain the hybrid interpretation. It also avoids mislabeling it as a pure Snare by acknowledging the genuine coordination function of institutional preservation, even if that preservation comes at a cost to some members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''hybrid_pragmatic_reading'' of the ''marriage_commitment_legitimacy'' kernel, or does it lean more towards a sibling reading?',
    'Detailed textual analysis of institutional pronouncements, internal documents, and member testimonials to assess the balance between acknowledging external pressure and asserting internal theological justification.',
    'If the balance shifts significantly towards pure external coercion, the constraint would align more with the ''exogenous_override_reading'' (potentially a Snare). If it shifts towards pure internal revelation, it would align with the ''endogenous_reinterpretation_reading'' (potentially a Rope or Tangled Rope with different beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity in the primary driver of institutional adaptation.').

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'To what extent is the preservation of institutional form and legal standing prioritized over strict doctrinal consistency in this adaptation?',
    'Analysis of historical decisions where doctrinal consistency was sacrificed for institutional survival, or vice versa. Examination of internal debates and dissenting voices within the institution.',
    'If institutional survival is overwhelmingly prioritized, the extractiveness from members (bearing interpretive costs) would be higher, pushing the classification closer to a Snare. If doctrinal integrity is genuinely maintained through reinterpretation, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, empirical, 'The true balance between institutional pragmatism and theological fidelity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the interpretive suppression experienced by rank-and-file members primarily structural (e.g., official pronouncements, disciplinary actions) or internalized (e.g., self-censorship, cognitive dissonance management)?',
    'Sociological studies of member experiences, including surveys on dissent expression and psychological impacts of interpretive ambiguity. Post-exit interviews with former members.',
    'If internalized suppression is dominant, the effective suppression is higher than structural measures suggest, as members carry the suppression with them. This would amplify the effective extraction from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(marr_tr_t18, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(marr_tr_t24, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(marr_be_t18, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement(marr_be_t24, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(marr_su_t18, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(marr_su_t24, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, religious_freedom_doctrine).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_autonomy_claims).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel, each representing a distinct interpretation of the institution's 'Manifesto' and its implications for marriage norms. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
