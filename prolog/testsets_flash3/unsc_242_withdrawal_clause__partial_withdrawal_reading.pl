% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'partial withdrawal' reading of UNSC
 *   Resolution 242's withdrawal clause. This reading interprets the English
 *   text's indefinite article 'territories' as permitting a non-total
 *   withdrawal, allowing the occupying power to retain strategic areas for
 *   'secure boundaries'. This interpretation converts textual ambiguity into
 *   negotiating leverage, benefiting the occupying power and international
 *   mediators who manage phased agreements, while imposing costs on
 *   territorial claimants who seek full withdrawal. The constraint is
 *   classified as a Tangled Rope due to its genuine coordination function
 *   (providing a framework for negotiations) intertwined with asymmetric
 *   extraction (benefiting the occupying power through indefinite retention).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.7).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'f79ff912-62f4-49da-9822-8a092321bbcf').
narrative_ontology:cs_kernel_codification('f79ff912-62f4-49da-9822-8a092321bbcf', fixed_text).
narrative_ontology:cs_authority_grounding('f79ff912-62f4-49da-9822-8a092321bbcf', lineage).
narrative_ontology:cs_interpretation_layer_present('f79ff912-62f4-49da-9822-8a092321bbcf').
narrative_ontology:cs_reading_relation('f79ff912-62f4-49da-9822-8a092321bbcf', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f79ff912-62f4-49da-9822-8a092321bbcf', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('f79ff912-62f4-49da-9822-8a092321bbcf', foundational, indefinite_article_permits_partial_withdrawal).
narrative_ontology:cs_axiom_status(indefinite_article_permits_partial_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('f79ff912-62f4-49da-9822-8a092321bbcf', indefinite_article_permits_partial_withdrawal, conventional).
narrative_ontology:cs_axiom('f79ff912-62f4-49da-9822-8a092321bbcf', foundational, secure_boundaries_may_require_territorial_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_may_require_territorial_retention, holdable).
narrative_ontology:cs_axiom_grounding('f79ff912-62f4-49da-9822-8a092321bbcf', secure_boundaries_may_require_territorial_retention, instrumental).
narrative_ontology:cs_reference_frame('f79ff912-62f4-49da-9822-8a092321bbcf', negotiated_phased_withdrawal_framework).
narrative_ontology:cs_drift_state('f79ff912-62f4-49da-9822-8a092321bbcf', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f79ff912-62f4-49da-9822-8a092321bbcf', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ambiguity of 'territories' in the English text, allowing it to negotiate partial withdrawal and retain strategic areas. Actively enforces its interpretation through diplomatic and military means, resisting calls for full withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, constrained, regional).

% Bear the cost of continued occupation and the indefinite nature of withdrawal. Their claims for full territorial integrity are undermined by the 'partial withdrawal' reading, leaving them without a clear enforcement line or timeline for return.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    powerless, generational, trapped, regional).

% Benefit from the flexibility this reading provides for ongoing negotiations and phased withdrawal agreements. Their role is sustained by the indefinite scope, allowing for continuous diplomatic engagement without a definitive resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_mediators, beneficiary,
    institutional, biographical, mobile, global).

% Bear the cost of interpretive incoherence in international law. Their attempts to establish clear legal principles for territorial integrity are complicated by the conflicting interpretations, leading to ongoing academic debate and a lack of clear precedent.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, observer).

% The original drafter and enforcer of Resolution 242. Its members hold divergent views on the withdrawal clause, leading to a lack of unified enforcement and perpetuating the ambiguity that benefits the occupying power.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for diplomatic negotiations regarding the withdrawal of forces from occupied territories, allowing for phased agreements and the consideration of security concerns for all parties.
% TRANSFER_FUNCTION: Transfers negotiating leverage and de facto control over strategic territories to the occupying power, while transferring the burden of indefinite occupation and legal ambiguity to territorial claimants.
% ABSENT_VOICES: Populations displaced from occupied territories, whose right of return is made contingent on a negotiated settlement that may never fully materialize under this reading. They would advocate for immediate and full withdrawal based on international law.
% DISAPPEARANCE_RATIONALE: If this reading of the withdrawal clause vanished, the legal and diplomatic landscape would shift dramatically. The occupying power would lose its primary justification for partial withdrawal, increasing pressure for full withdrawal. Territorial claimants would have a stronger legal basis for their demands, and international mediators would need a new framework for negotiations.
% FOUNDING_PROBLEM: To establish a framework for peace in the Middle East following the 1967 Arab-Israeli War, specifically addressing the withdrawal of Israeli forces from occupied territories and the right of all states in the area to live in peace within secure and recognized boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing conflict and diplomatic efforts in the region attest to the founding problem remaining live. International bodies, regional powers, and academic analyses outside the direct beneficiaries of this reading consistently highlight the unresolved nature of the conflict and the centrality of territorial disputes.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) because the reading enables the occupying power to retain significant strategic value, but it is conditional on ongoing negotiations and international pressure. Suppression is high (0.7) as the occupying power actively resists alternative interpretations and maintains control over the territories. Theater ratio is low (0.2) because the diplomatic efforts and security concerns are real, though the ambiguity serves to perpetuate the status quo. Accessibility collapse is moderate (0.4) as full withdrawal remains a theoretical alternative, but practical and political barriers are substantial. Resistance is high (0.75) from territorial claimants and their allies.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power perceives this reading as a legitimate and necessary interpretation for its security, enabling coordination towards a stable peace. Territorial claimants, however, experience it as a mechanism of prolonged extraction and suppression of their rights. International mediators view it as a pragmatic tool for managing a complex conflict. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and international mediators are beneficiaries, as this reading provides them with flexibility and sustained diplomatic roles. Territorial claimants and international legal scholars are victims, bearing the costs of indefinite occupation and interpretive incoherence. The UN Security Council, as the drafter, is an agenda-setter whose internal divisions contribute to the persistence of this ambiguous reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_intent,
    'Was the indefinite article ''territories'' in the English text of UNSC Resolution 242 an intentional ambiguity to facilitate negotiation, or an accidental linguistic difference from the French ''des territoires'' (the territories)?',
    'Declassified diplomatic archives and testimonies from the original drafters, if available, clarifying their specific intent regarding the English phrasing.',
    'If intentional, it reinforces this reading''s legitimacy as a tool for negotiation. If accidental, it weakens the basis for partial withdrawal, strengthening the maximal withdrawal reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_intent, empirical, 'Ambiguity regarding the drafters'' intent behind the English text of Resolution 242.').

omega_variable(
    secure_boundaries_definition,
    'What constitutes ''secure and recognized boundaries'' in the context of Resolution 242, and does it inherently require the retention of occupied strategic territories?',
    'International legal precedent, expert opinions from military strategists and international law scholars, and a consensus-based re-evaluation by the UN Security Council.',
    'If ''secure boundaries'' can be achieved without retaining occupied territories, this reading''s justification is weakened, shifting leverage towards full withdrawal. If retention is deemed essential, this reading gains stronger normative grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_definition, conceptual, 'The definition and requirements of ''secure and recognized boundaries'' and their relation to territorial retention.').

omega_variable(
    mandatrophy_of_negotiation,
    'Has the ''negotiating leverage'' function of this reading atrophied into a mechanism for indefinite occupation, rather than facilitating genuine progress towards peace?',
    'Longitudinal analysis of diplomatic outcomes: if negotiations consistently fail to produce significant territorial withdrawal over decades, it suggests the function has degraded.',
    'If atrophied, the constraint shifts closer to a Snare, as its coordination function becomes theatrical cover for pure extraction. If still genuinely facilitating progress, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_negotiation, empirical, 'Whether the negotiation function has degraded into indefinite occupation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNSC Resolution 242 withdrawal clause. It is linked to the 'maximal_withdrawal_reading' and 'interpretive_authority_structure' as part of a constraint family, where different interpretations of the same text create distinct structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
