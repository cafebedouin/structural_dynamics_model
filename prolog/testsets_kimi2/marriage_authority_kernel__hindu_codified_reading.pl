% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 Codified Authority Reading
 *   domain: comparative_law/constitutional_pluralism
 *
 * SUMMARY:
 *   In India, marriage and family law for Hindus derives authority from the
 *   Hindu Marriage Act 1955, a codified statute interpreted by civil courts.
 *   This constraint story captures the hindu_codified_reading of the marriage
 *   authority kernel: the claim that Hindu personal law is a uniform,
 *   state-adjudicated regime distinct from Shariat, canonical Christian law,
 *   Parsi custom, or a secular civil code. The reading coordinates Hindu
 *   marriages under one statute but extracts through moderate gender inequity
 *   and community-boundary policing. It is claimed as tangled_rope
 *   (coordination plus extraction) and the metrics are authored
 *   independently.
 *
 * KEY AGENTS:
 *   - Civil courts (agenda_setter): Institutional interpreter bound by codified text
 *   - Hindu men (beneficiary): Collect patriarchal residual and community legitimacy
 *   - Hindu women (payer): Bear moderate inequity in divorce, maintenance, and identity pressure
 *   - Hindu orthodox organizations (beneficiary): Collect state recognition of community boundaries
 *   - Interfaith couples (excluded): Forced out of the regime by religious identity requirement
 *   - Secular advocates (excluded): Pushed out of the legislative conversation by personal law consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Codified Authority Reading").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '4701084d-b222-4281-b43b-de99d2bfbccb').
narrative_ontology:cs_kernel_codification('4701084d-b222-4281-b43b-de99d2bfbccb', formalized).
narrative_ontology:cs_authority_grounding('4701084d-b222-4281-b43b-de99d2bfbccb', lineage).
narrative_ontology:cs_interpretation_layer_present('4701084d-b222-4281-b43b-de99d2bfbccb').
narrative_ontology:cs_reading_relation('4701084d-b222-4281-b43b-de99d2bfbccb', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4701084d-b222-4281-b43b-de99d2bfbccb', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4701084d-b222-4281-b43b-de99d2bfbccb', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('4701084d-b222-4281-b43b-de99d2bfbccb', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('4701084d-b222-4281-b43b-de99d2bfbccb', foundational, codified_statute_as_authoritative_source).
narrative_ontology:cs_axiom_status(codified_statute_as_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('4701084d-b222-4281-b43b-de99d2bfbccb', codified_statute_as_authoritative_source, conventional).
narrative_ontology:cs_axiom('4701084d-b222-4281-b43b-de99d2bfbccb', foundational, civil_judiciary_as_interpreter).
narrative_ontology:cs_axiom_status(civil_judiciary_as_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('4701084d-b222-4281-b43b-de99d2bfbccb', civil_judiciary_as_interpreter, conventional).
narrative_ontology:cs_reference_frame('4701084d-b222-4281-b43b-de99d2bfbccb', hindu_marriage_act_codified_framework).
narrative_ontology:cs_drift_state('4701084d-b222-4281-b43b-de99d2bfbccb', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4701084d-b222-4281-b43b-de99d2bfbccb', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_men).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_orthodox_organizations).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and adjudicate disputes under the Hindu Marriage Act 1955. Bound by codified text but develop jurisprudence on Hindu customs, maintenance, and divorce. Cannot exit the personal law framework without constitutional amendment or legislative repeal.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from residual patriarchal structures in codified law and its interpretation: favorable inheritance linkage, divorce ground asymmetries historically carried forward, and social legitimacy of community-endorsed marriage. Exit to the secular Special Marriage Act is legally possible but socially constrained.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_men, beneficiary,
    powerful, biographical, constrained, national).

% Bear the structural costs of moderate gender inequity in divorce grounds, maintenance adjudication, and community pressure to marry under Hindu rites. Increasingly use courts to leverage progressive interpretations, but the codified framework still channels them through identity-based rules rather than neutral rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    moderate, biographical, constrained, national).

% Derive legitimacy from state codification and enforcement of Hindu personal law. Advocate for community-endogamous marriage and resist secularization or uniform civil code. Benefit from the state's recognition of religious identity as a legal category.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_orthodox_organizations, beneficiary,
    organized, generational, constrained, national).

% Excluded from the Hindu Marriage Act when one partner is non-Hindu. Must navigate the Special Marriage Act or face legal non-recognition within the Hindu framework. Their exclusion is the boundary condition that enforces the constraint's community scope.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, excluded,
    powerless, immediate, trapped, national).

% Advocate for a uniform civil code replacing all personal laws. Are excluded from the legislative and interpretive process that preserves Hindu codified law as a distinct regime. Their opposition is structurally muted by the political consensus around personal law pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform statutory framework for Hindu marriages, replacing diverse and often contradictory local customs with a single codified law adjudicated by state civil courts.
% TRANSFER_FUNCTION: Moves authority over marriage and divorce from diverse customary and communal sources to the codified Hindu Marriage Act and civil judiciary; moves moderate gender-based burdens from Hindu women to the structural advantage of Hindu male community members and orthodox organizations.
% ABSENT_VOICES: Interfaith couples seeking recognition within Hindu law, secular advocates demanding a uniform civil code, and Hindu women whose dissent from community identity is suppressed by the social cost of exiting to the Special Marriage Act.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and civil court authority over Hindu marriages vanished, Hindu marriages would revert to uncodified custom or shift entirely to the Special Marriage Act. Community boundaries would destabilize, and the Indian state's mechanism for managing religious identity through personal law would lose a central pillar.
% FOUNDING_PROBLEM: Post-Independence need to modernize and unify Hindu marriage practices, provide state adjudication infrastructure for a large majority population, and secure community identity within a secular constitutional framework.
% FOUNDING_PROBLEM_CORROBORATION: The Union Legislature and judiciary attest the problem is partially live, citing achieved uniformity and civil adjudication. Feminist legal scholars and secular advocates attest the founding problem is substantially solved and the arrangement now functions to preserve identity-based governance and male community advantage; their analysis is corroborated by comparative gender-equity audits against the Special Marriage Act.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50 to 0.45 over the interval) reflects moderate but persistent gender asymmetry and identity-based exclusion. Suppression (0.60) captures the legal and social enforcement of community boundaries: while the Special Marriage Act exists as an exit, social stigma and the state's administrative preference for personal law suppress its use. Theater ratio (0.25 to 0.32) reflects the judicial and political performance that the 1955 codification represents ancient Hindu tradition rather than a modern legislative construct. Accessibility collapse (0.55) is moderate: the SMA is legally knowable but socially collapsed as an alternative for many Hindus. Resistance (0.45) comes from feminist legal mobilization and UCC advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The civil court seat experiences this constraint as a legitimate interpretive framework with genuine modernization and uniformity achievements. The Hindu women seat experiences the same framework as a partially extractive identity lock that channels them through gender-asymmetric rules. The engine computes this divergence from the structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu men and orthodox organizations sit near the beneficiary pole: the constraint subsidizes their community identity and, residually, patriarchal advantage. Hindu women sit near the target pole: they bear the costs of moderate inequity and identity-based channeling. Civil courts sit near symmetric but slightly beneficiary because they derive institutional authority from the regime. Interfaith couples and secular advocates are excluded, receiving neither coordination benefit nor direct extraction but bearing the externality of a fragmented legal landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmodernizing and unifying Hindu marriageâwas genuinely live in 1955. The regime has partially solved it (uniformity, civil adjudication), so it is not a pure snare. However, the persistence of gender-asymmetric provisions and the political block on further reform suggest the arrangement now serves community-boundary maintenance and male advantage beyond its original modernization mandate. This prevents mislabeling it as pure coordination (Rope) or pure extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does this constraint represent the Hindu codified reading exclusively, or does it conflate multiple marriage authority sources?',
    'Cross-reference with sibling constraint stories for Muslim, Christian, Parsi, and secular readings; verify no stakeholder or metric imports from alternate readings.',
    'If conflated, epsilon and directionality would be unstable across measurements; if clean, this reading stands as one of five parallel constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the story maintains epsilon-invariance for the Hindu codified reading only.').

omega_variable(
    gender_equity_gap,
    'Does the Hindu Marriage Act 1955 as currently interpreted still encode structural extraction from Hindu women relative to a sex-neutral code, or has amendment and judicial interpretation closed the gap?',
    'Comparative audit of statutory provisions (divorce grounds, maintenance, inheritance linkage) against Special Marriage Act benchmarks and Supreme Court gender-equity jurisprudence.',
    'If substantial gaps remain, the constraint''s extractiveness is higher than coordination; if closed, the constraint approaches pure coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_gap, empirical, 'Empirical status of gender equity within Hindu codified marriage law.').

omega_variable(
    community_boundary_enforcement,
    'Does the constraint''s limitation to Hindus function as legitimate community self-governance or as identity-based exclusion of interfaith and dissenting members?',
    'Measure rates of Special Marriage Act opt-out among Hindus, social costs reported, and litigation forcing HMA applicability on non-Hindu spouses.',
    'If exclusionary, spatial_scope and suppression metrics understate extraction for interfaith and dissenting Hindus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_boundary_enforcement, conceptual, 'Whether community boundary maintenance is coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(marr_tr_t25, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(marr_tr_t55, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 55, 0.35).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.32).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(marr_be_t25, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(marr_be_t55, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 55, 0.5).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority_kernel__hindu_codified_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is the Hindu codified reading of the marriage_authority_kernel, one of five parallel personal-law readings (Muslim Shariat, Christian canonical, Parsi communal, secular civil) that together model India's plural marriage-authority regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
