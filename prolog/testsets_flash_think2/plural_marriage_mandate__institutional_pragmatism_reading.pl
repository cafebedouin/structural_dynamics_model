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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: Plural Marriage Mandate (Institutional Pragmatism Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto, which officially
 *   suspended the practice of plural marriage, through the lens of
 *   institutional pragmatism. It argues that the Manifesto was a strategic
 *   adaptation by church leadership to ensure institutional survival in the
 *   face of overwhelming federal coercion, with the accompanying 'revelation'
 *   narrative serving to legitimate this capitulation. The period from 1890
 *   to 1904 is critical, as it covers the initial suspension and the
 *   subsequent internal enforcement against continued secret plural
 *   marriages, culminating in the Second Manifesto of 1904.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.85).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate (Institutional Pragmatism Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '92700622-66a6-4f49-a941-b4e5ce9e4986').
narrative_ontology:cs_kernel_codification('92700622-66a6-4f49-a941-b4e5ce9e4986', fixed_text).
narrative_ontology:cs_authority_grounding('92700622-66a6-4f49-a941-b4e5ce9e4986', extraction).
narrative_ontology:cs_interpretation_layer_present('92700622-66a6-4f49-a941-b4e5ce9e4986').
narrative_ontology:cs_reading_relation('92700622-66a6-4f49-a941-b4e5ce9e4986', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('92700622-66a6-4f49-a941-b4e5ce9e4986', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_axiom('92700622-66a6-4f49-a941-b4e5ce9e4986', foundational, doctrinal_claims_serve_institutional_survival).
narrative_ontology:cs_axiom_status(doctrinal_claims_serve_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('92700622-66a6-4f49-a941-b4e5ce9e4986', doctrinal_claims_serve_institutional_survival, instrumental).
narrative_ontology:cs_axiom('92700622-66a6-4f49-a941-b4e5ce9e4986', secondary, revelation_narrative_as_legitimation).
narrative_ontology:cs_axiom_status(revelation_narrative_as_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('92700622-66a6-4f49-a941-b4e5ce9e4986', revelation_narrative_as_legitimation, conventional).
narrative_ontology:cs_reference_frame('92700622-66a6-4f49-a941-b4e5ce9e4986', institutional_survival_imperative).
narrative_ontology:cs_drift_state('92700622-66a6-4f49-a941-b4e5ce9e4986', post_1890_manifesto, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('92700622-66a6-4f49-a941-b4e5ce9e4986', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, loyal_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orchestrated the strategic adaptation to federal pressure, issuing the Manifesto to suspend plural marriage while framing it as divine revelation. Benefited from institutional survival, restored political rights, and maintained control over the narrative and membership.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Were forced to abandon a practice they believed was divinely commanded, facing social ostracism, legal persecution, and profound personal disruption. Many continued the practice in secret, bearing significant risk.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, polygamist_families, payer,
    powerless, biographical, trapped, local).

% Members who accepted the Manifesto as a genuine revelation and entered monogamous marriages, often unaware of the continued secret practice of plural marriage by some leaders and members. Their loyalty and identity were tied to the church's official narrative.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, identity_locked, local).

% Exerted superior coercive power through anti-polygamy legislation, threatening the church's existence. Its pressure directly led to the Manifesto, achieving its policy goals.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the church's survival and its reintegration into mainstream American society, which reduced social stigma. Their identity was deeply intertwined with the church, making exit difficult despite potential internal conflicts over the change.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, loyal_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Groups who rejected the Manifesto as a betrayal of divine command, often forming splinter denominations. They were excluded from the main church's narrative and institutional structure, facing continued social and religious marginalization.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, dissident_factions, excluded,
    organized, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the church's adaptation to overwhelming external coercive power, ensuring its institutional survival and maintaining a unified, albeit internally conflicted, religious community.
% TRANSFER_FUNCTION: Transfers political and social legitimacy from the federal government to the church, in exchange for the suspension of plural marriage. It also transfers loyalty and compliance from members to church leadership, in exchange for the continued existence of the institution and its salvific claims.
% ABSENT_VOICES: Dissident polygamist factions and those who felt betrayed by the shift in doctrine were either excommunicated or marginalized; they would argue the Manifesto was a capitulation, not a revelation, and that the church abandoned its core principles.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its subsequent enforcement vanished, the entire institutional history and doctrinal trajectory of the church would be fundamentally altered. Its relationship with the state, its internal coherence, and the legitimacy of its leadership would collapse, leading to a complete reorganization of its structure and belief system.
% FOUNDING_PROBLEM: The existential threat posed by the federal government's anti-polygamy legislation (e.g., Edmunds-Tucker Act), which included disincorporation of the church, confiscation of assets, and disenfranchisement of members, threatening the very survival of the institution.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and contemporary journalistic accounts from outside the church leadership corroborate the severe existential threat faced by the church in the late 19th century. The problem itself (federal persecution for plural marriage) is no longer live, though the church continues to frame its survival as a live concern.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the church leadership extracted compliance and loyalty from its members, particularly polygamist families, who bore significant personal and spiritual costs. Suppression is very high, driven initially by federal power and subsequently by internal church enforcement to maintain the new norm and institutional unity. The theater ratio is high and increasing, reflecting the growing gap between the official narrative of divine revelation and the pragmatic, survival-driven institutional actions, especially as secret continuations of plural marriage became known. The claimed type is 'tangled_rope' because it served a genuine coordination function (institutional survival) but involved significant asymmetric extraction and required active enforcement, both external (federal) and internal (church leadership).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, the Manifesto was a necessary, divinely guided act of institutional preservation. From the perspective of polygamist families, it was a traumatic betrayal. Deceived monogamists experienced a different form of extraction, rooted in the dissonance between official doctrine and hidden practice. The engine's classification as 'tangled_rope' captures this blend of coordination (for the institution) and extraction (from members).
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is the primary beneficiary, securing institutional survival and political rights. Polygamist families and deceived monogamists are the primary targets, bearing the costs of abandonment, disruption, and deception. The federal government acts as an external agenda-setter, imposing the conditions for the constraint. Loyal members are beneficiaries of institutional stability but also bear the cost of adapting to the new norm, often through identity-locked compliance. Dissident factions are excluded, their resistance suppressed by both federal and church authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_revelation,
    'Was the 1890 Manifesto a genuine prophetic revelation, or a pragmatic institutional response framed as revelation?',
    'Analysis of internal church records, private correspondence of leaders, and comparison with other instances of ''revelation'' in response to external pressure.',
    'If primarily pragmatic, the ''theater_ratio'' and ''extractiveness'' would be further amplified, highlighting the deceptive nature of the legitimation. If genuinely revelatory, the ''theater_ratio'' would decrease, and the ''claimed_type'' might shift closer to a ''rope'' for loyal members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_revelation, conceptual, 'Ambiguity regarding the true nature of the Manifesto''s origin.').

omega_variable(
    extent_of_secret_continuations,
    'How widespread and institutionally sanctioned were secret plural marriages after 1890, and for how long?',
    'Forensic historical research into private records, diaries, and genealogical data, cross-referenced with church disciplinary actions.',
    'If widespread and tacitly sanctioned by leadership, the ''extractiveness'' from deceived monogamists and the ''suppression'' of internal dissent would be higher, reinforcing the ''tangled_rope'' classification and potentially pushing it towards ''snare'' for those most affected by the deception.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_secret_continuations, empirical, 'Uncertainty about the actual practice of plural marriage post-Manifesto.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent did members'' compliance stem from internalized belief in the Manifesto''s divine origin versus structural pressure from church leadership and social ostracism?',
    'Sociological studies of former members, analysis of personal narratives, and comparison of compliance rates across different levels of social integration within the church.',
    'If internalized belief was dominant, the ''suppression'' metric might be re-evaluated as less coercive. If structural pressure was primary, the ''suppression'' would be confirmed as high, highlighting the coercive aspects of institutional control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for member compliance.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''institutional pragmatism'' framing the most defensible interpretation, or does it obscure other valid readings of the Manifesto''s impact?',
    'Comparative analysis with the ''exogenous_override_reading'' and ''endogenous_reinterpretation_reading'', evaluating which framing best accounts for the full range of historical evidence and lived experiences.',
    'If an alternative reading (e.g., ''exogenous_override'') were adopted, the emphasis might shift from internal church agency to external federal coercion, potentially altering the perceived ''extractiveness'' and ''suppression'' mechanisms, though likely retaining a ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Underdetermination of the primary interpretive frame for the Manifesto.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.55).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.6).
narrative_ontology:measurement(plur_tr_t1901, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1901, 0.63).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.65).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.7).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.74).
narrative_ontology:measurement(plur_be_t1901, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1901, 0.76).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.82).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.83).
narrative_ontology:measurement(plur_su_t1901, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1901, 0.84).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'plural_marriage_mandate' kernel, each representing a distinct structural interpretation of the 1890 Manifesto and its implications for the church and its members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
