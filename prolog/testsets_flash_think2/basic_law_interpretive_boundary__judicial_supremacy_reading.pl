% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Basic Law Interpretive Boundary
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the Basic
 *   Laws' interpretive boundary, where the Supreme Court holds ultimate
 *   authority to interpret and enforce these laws, including invalidating
 *   contradictory legislation. This reading is one of several competing
 *   interpretations of the constitutional order. The constraint functions as
 *   a Tangled Rope, providing coordination (constitutional stability, rights
 *   protection) but also extracting legislative power from the Knesset
 *   through active judicial enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Basic Law Interpretive Boundary").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, 'd4c2e002-96e3-4f1b-a0b0-2340bcc6ec47').
narrative_ontology:cs_kernel_codification('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', formalized).
narrative_ontology:cs_authority_grounding('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', lineage).
narrative_ontology:cs_interpretation_layer_present('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47').
narrative_ontology:cs_reading_relation('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', foundational, constitutional_supremacy_of_basic_laws).
narrative_ontology:cs_axiom_status(constitutional_supremacy_of_basic_laws, holdable).
narrative_ontology:cs_axiom_grounding('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', constitutional_supremacy_of_basic_laws, deontological).
narrative_ontology:cs_axiom('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', foundational, judicial_review_as_constitutional_enforcement).
narrative_ontology:cs_axiom_status(judicial_review_as_constitutional_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', judicial_review_as_constitutional_enforcement, conventional).
narrative_ontology:cs_reference_frame('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', judicial_enforcement_of_basic_laws).
narrative_ontology:cs_drift_state('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', contemporary_political_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d4c2e002-96e3-4f1b-a0b0-2340bcc6ec47', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_independence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order legal framework, invalidating contradictory legislation passed by the Knesset. This role grants it significant power over the legislative process and the protection of individual rights.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% As the legislative body, its power to enact laws is constrained by the Supreme Court's interpretation of the Basic Laws. Legislation can be nullified, requiring the Knesset to either revise laws or face political and legal challenges.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% Individuals or groups whose rights are protected by the Basic Laws. They benefit from the Supreme Court's ability to invalidate legislation that infringes upon these rights, gaining a powerful veto mechanism through litigation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, immediate, constrained, national).

% The political coalition holding power in the Knesset. Their legislative agenda can be thwarted by judicial review, forcing them to compromise or abandon policies that conflict with the Basic Laws as interpreted by the Court.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority, payer,
    powerful, biographical, constrained, national).

% Political factions and legal scholars who argue for the ultimate authority of the Knesset, including its power to interpret and amend Basic Laws without judicial oversight. This reading of the constraint actively suppresses their preferred constitutional order.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    organized, generational, identity_locked, national).

% Academics and legal experts who analyze the constitutional framework, the role of the Supreme Court, and the implications of judicial review. They provide critical commentary and comparative analysis, influencing public and legal discourse.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable, higher-order legal framework (the Basic Laws) that provides a consistent basis for legal interpretation and protects fundamental rights, preventing legislative overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over Basic Laws from the Knesset to the Supreme Court, and transfers a de facto veto power to rights-claimants through the mechanism of judicial review.
% ABSENT_VOICES: Advocates of parliamentary sovereignty are structurally excluded from the decision-making process regarding the scope of judicial review. They would argue for the Knesset's ultimate authority and the right to legislate without judicial invalidation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire legal and political system would face a crisis of authority. The locus of ultimate legal power would become ambiguous, fundamental rights would lack a clear enforcement mechanism, and the stability of the constitutional order would collapse, requiring a complete re-establishment of foundational legal principles.
% FOUNDING_PROBLEM: To establish a robust, higher-order legal framework protecting fundamental rights and providing constitutional stability in the absence of a formal, entrenched constitution, preventing transient legislative majorities from undermining core democratic principles.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, human rights organizations, and a significant portion of the legal community attest to the ongoing necessity of a strong judicial mechanism to protect rights and maintain constitutional stability, particularly in a politically polarized environment. This corroboration comes from outside the immediate beneficiaries of the Supreme Court's power.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the Supreme Court's power to invalidate legislation significantly curtails the Knesset's legislative autonomy. Suppression is also high (0.75) as it actively suppresses legislative alternatives that conflict with the Court's interpretation. Theater ratio is low (0.1) because the Court's actions are generally functional and aimed at upholding its interpretation of the Basic Laws, rather than being performative. Accessibility collapse is moderate-high (0.7) as legislative options are substantially narrowed. Resistance is moderate-high (0.6) due to ongoing political contestation over the Court's role and powers.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's perspective, this constraint is a necessary Rope, upholding constitutional principles and protecting rights. From the Knesset's perspective, particularly the legislative majority, it operates as a Snare, extracting their legislative authority and suppressing their policy agenda. Rights claimants perceive it as a vital Rope, providing a crucial safeguard.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and rights claimants are beneficiaries (low d) as they gain power and protection from this arrangement. The Knesset and the legislative majority are targets (high d) as their legislative power is curtailed. Parliamentary sovereignty advocates are excluded, their preferred constitutional order actively suppressed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide constitutional stability and rights protection remains live. However, the specific mechanism of judicial supremacy, as interpreted here, is contested. The high extractiveness and suppression, coupled with ongoing resistance, indicate that while the founding problem is live, the solution has become a site of significant power transfer, preventing mislabeling as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_judicial_invalidation,
    'What is the actual frequency and impact of judicial invalidation of Knesset legislation, and does it disproportionately affect certain types of legislation or political factions?',
    'Empirical study of Supreme Court rulings over time, analyzing the number of invalidated laws, their subject matter, and the political composition of the Knesset at the time of invalidation.',
    'If invalidation is rare or primarily targets minor legislation, the effective extractiveness might be lower than perceived. If it consistently targets core policy areas of specific political factions, it would confirm high, targeted extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_judicial_invalidation, empirical, 'Empirical extent and political targeting of judicial invalidation.').

omega_variable(
    legitimacy_of_judicial_review_in_unwritten_constitution,
    'Is the Supreme Court''s assertion of judicial supremacy a legitimate evolution of constitutional practice, or an overreach in the absence of a formally entrenched constitution?',
    'Analysis of historical constitutional conventions, comparative constitutional law, and the degree of public and political acceptance of the Court''s role over time. This is a conceptual debate with no single empirical resolution.',
    'If deemed an overreach, the constraint''s legitimacy would be undermined, potentially increasing resistance and calls for legislative curbs on judicial power. If seen as legitimate, it reinforces the Court''s role and the constraint''s stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_review_in_unwritten_constitution, conceptual, 'Conceptual legitimacy of judicial supremacy in an uncodified constitutional system.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint truly a distinct ''judicial supremacy'' reading, or is it better understood as a more assertive variant of a ''balanced contestation'' reading?',
    'Detailed textual analysis of judicial opinions and political discourse, focusing on whether the language used explicitly asserts ultimate judicial authority or merely a strong, but still balanced, role within a system of checks and balances.',
    'If reclassified as a variant of ''balanced_contestation_reading'', the perceived extractiveness and suppression might be slightly lower, and the ''forecloses'' relationship with ''parliamentary_sovereignty_reading'' might weaken to ''influences'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinction between judicial supremacy and assertive balanced contestation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(basi_tr_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(basi_be_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(basi_be_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 18, 0.78).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(basi_su_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(basi_su_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_boundary' kernel, each representing a different structural claim about the locus of constitutional authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
