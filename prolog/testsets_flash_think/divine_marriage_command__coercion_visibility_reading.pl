% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coercion visibility' reading of
 *   the divine marriage command kernel. It posits that the Manifesto, which
 *   ended the practice of polygamy, was an acknowledged response to severe
 *   federal coercion, and its theological legitimacy is primarily derived
 *   from the necessity of institutional survival. This reading acknowledges
 *   exogenous pressure as a valid input for doctrinal shift, closing the
 *   M-set gap between divine command and practice by admitting pragmatic
 *   grounds for change. The constraint is classified as a Tangled Rope
 *   because it served a genuine coordination function (institutional
 *   survival) but was born from and maintained through significant external
 *   and internal coercion, extracting a fundamental doctrinal and lifestyle
 *   change from its members.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.75).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '4f2255bf-a569-43c7-9841-f17ede4e3d22').
narrative_ontology:cs_kernel_codification('4f2255bf-a569-43c7-9841-f17ede4e3d22', formalized).
narrative_ontology:cs_authority_grounding('4f2255bf-a569-43c7-9841-f17ede4e3d22', lineage).
narrative_ontology:cs_interpretation_layer_present('4f2255bf-a569-43c7-9841-f17ede4e3d22').
narrative_ontology:cs_reading_relation('4f2255bf-a569-43c7-9841-f17ede4e3d22', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f2255bf-a569-43c7-9841-f17ede4e3d22', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('4f2255bf-a569-43c7-9841-f17ede4e3d22', foundational, institutional_survival_is_theologically_justified).
narrative_ontology:cs_axiom_status(institutional_survival_is_theologically_justified, holdable).
narrative_ontology:cs_axiom_grounding('4f2255bf-a569-43c7-9841-f17ede4e3d22', institutional_survival_is_theologically_justified, theological).
narrative_ontology:cs_axiom('4f2255bf-a569-43c7-9841-f17ede4e3d22', foundational, exogenous_pressure_can_shape_doctrine).
narrative_ontology:cs_axiom_status(exogenous_pressure_can_shape_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4f2255bf-a569-43c7-9841-f17ede4e3d22', exogenous_pressure_can_shape_doctrine, conventional).
narrative_ontology:cs_reference_frame('4f2255bf-a569-43c7-9841-f17ede4e3d22', pure_revelatory_doctrine).
narrative_ontology:cs_drift_state('4f2255bf-a569-43c7-9841-f17ede4e3d22', post_manifesto_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('4f2255bf-a569-43c7-9841-f17ede4e3d22', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, monogamous_members).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, the_institution).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, dissenting_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The highest ecclesiastical authority that issued the Manifesto, acknowledging federal coercion as a primary driver for the doctrinal shift. They benefit from the institution's survival and social integration, but bear the burden of justifying the shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Members who practiced polygamy and were compelled by the Manifesto to abandon their practices, face legal prosecution, or go underground. They bore the direct costs of the doctrinal shift, often experiencing profound personal and social disruption.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamous_members, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, polygamous_members, excluded).

% Members who either already practiced monogamy or readily adopted it. They benefited from the institution's increased social acceptance and reduced legal persecution, aligning with broader societal norms.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, monogamous_members, beneficiary,
    moderate, biographical, mobile, local).

% The external coercive force that applied legal, military, and economic pressure to compel the institution to abandon polygamy. They achieved their policy objective of enforcing federal law and promoting social norms.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% The religious organization itself, whose survival was threatened by federal action. It benefited from the doctrinal shift by avoiding dissolution, retaining property, and gaining social legitimacy, albeit at the cost of internal doctrinal tension.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, the_institution, beneficiary,
    institutional, civilizational, constrained, global).

% Groups or individuals who rejected the Manifesto and continued to practice polygamy, often forming splinter communities. They were excluded from the mainstream institution and faced ongoing legal and social marginalization.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, dissenting_factions, excluded,
    powerless, generational, trapped, local).

% Academics and researchers who analyze the historical context, theological implications, and social impact of the Manifesto, often highlighting the role of federal coercion in its issuance. They provide an external, analytical perspective.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, historians_and_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, the_institution).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the religious institution's practices with federal law and broader societal norms, ensuring its legal and physical survival in the face of existential threats.
% TRANSFER_FUNCTION: Transfers the practice of polygamy from the institution's accepted norms to a prohibited status, thereby transferring legal and social acceptance to the institution and its members, while imposing significant costs on those who previously practiced polygamy.
% ABSENT_VOICES: Those who continued to believe in and practice polygamy, viewing the Manifesto as a betrayal of divine command rather than a legitimate doctrinal shift. Their voices were marginalized or actively suppressed by both the federal government and the institutional leadership seeking survival.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its associated doctrinal shift vanished, the religious institution would immediately face renewed federal persecution, legal challenges, and social ostracization, threatening its very existence. Its current legal status, property rights, and social integration are directly dependent on this constraint.
% FOUNDING_PROBLEM: The federal government's sustained legal, military, and economic coercion against the practice of polygamy, threatening the religious institution's property, leadership, and continued existence.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court documents, contemporary news reports, and independent historical scholarship from outside the benefiting parties (e.g., non-denominational historians) corroborate the severe federal coercion and the existential threat it posed to the institution. While the legal prohibition on polygamy remains, the specific, direct federal campaign that necessitated the Manifesto is no longer active in the same form.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high due to the profound personal and social costs imposed on polygamous members forced to abandon their practices. Suppression is very high, reflecting the overwhelming power of the federal government and the institutional authority's enforcement of the new norm. Theater ratio is moderate, as the theological framing of the Manifesto as divinely guided may contain performative elements, but the underlying shift was a genuine, coerced response to an existential threat. Accessibility collapse is near total for the practice of polygamy within the mainstream institution. Resistance was significant, both from federal authorities against the original practice and from some members against the new norm, but ultimately overcome.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership, while acknowledging coercion, frames the outcome as a necessary, divinely sanctioned act for survival. Polygamous members experienced it as a devastating extraction. The federal government viewed it as successful enforcement of law. These divergent perspectives are computed by the engine from the structural data, not reconciled in the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership and the institution itself are beneficiaries, as the constraint ensured their survival and legitimacy. Monogamous members also benefited from increased social acceptance. Polygamous members and dissenting factions are clear victims, bearing the direct costs of the forced change. The federal government, as the primary coercive force, also acted as an agenda-setter and beneficiary of its policy goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival in the face of federal coercion) is largely 'dead' in its original form, as the specific federal campaign against polygamy has ceased. However, the constraint persists, and its 'founding_problem_status' is 'contested' because the institution continues to frame the Manifesto's principles as live and essential for its ongoing legitimacy and social integration. This persistence, despite the original coercive threat having receded, indicates a potential for mandatrophy, where the constraint's function has shifted from immediate survival to maintaining a new, established norm, potentially masking ongoing extraction from those who dissent or are marginalized by the new norm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the theological legitimacy of the Manifesto truly derived from institutional survival necessity, or is this a post-hoc rationalization for a pragmatic decision made under duress?',
    'Analysis of internal institutional discourse and theological justifications over time, comparing initial pragmatic statements with later, more formalized theological explanations.',
    'If primarily a rationalization, it highlights the institution''s capacity for adaptive, non-revelatory doctrinal shifts under pressure, potentially undermining claims of pure revelatory guidance. If genuinely derived, it establishes a precedent for ''survival necessity'' as a theological ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity regarding the true source of the Manifesto''s theological legitimacy.').

omega_variable(
    coercion_vs_internal_evolution,
    'To what extent was the doctrinal shift purely a result of federal coercion, versus an internal evolution within the institution that merely aligned with external pressures?',
    'Detailed historical analysis of internal debates, theological writings, and leadership statements preceding the Manifesto, seeking evidence of internal pressures for change independent of federal action.',
    'If significant internal evolutionary pressures existed, the ''coercion visibility'' reading might overemphasize external factors, suggesting a more complex interplay of forces. If purely coerced, it underscores the power of external pressure to force fundamental doctrinal change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_internal_evolution, empirical, 'The balance between external coercion and internal evolutionary factors in the doctrinal shift.').

omega_variable(
    revelation_concept_impact,
    'Does acknowledging coercion as a valid input for doctrinal change fundamentally alter or undermine the institution''s concept of continuous divine revelation?',
    'Theological and philosophical analysis of how the institution''s doctrine of revelation has been articulated and understood before and after the Manifesto, particularly in light of this reading''s emphasis on pragmatic necessity.',
    'If it fundamentally alters the concept, it could lead to a legitimacy crisis for readings that rely on an unmediated, purely divine source of doctrine. If the concept of revelation is flexible enough to accommodate such inputs, it strengthens the institution''s adaptive capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_concept_impact, conceptual, 'The impact of acknowledging coercion on the institution''s doctrine of revelation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(divi_tr_t1900, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1910, 0.25).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(divi_tr_t1930, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(divi_tr_t1940, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1940, 0.32).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1950, 0.35).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(divi_be_t1900, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1920, 0.65).
narrative_ontology:measurement(divi_be_t1930, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement(divi_be_t1940, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1950, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(divi_su_t1900, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1910, 0.75).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(divi_su_t1930, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(divi_su_t1940, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1950, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, each representing a distinct interpretation of the Manifesto and its theological implications. This 'coercion_visibility_reading' focuses on the role of federal pressure and institutional survival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
