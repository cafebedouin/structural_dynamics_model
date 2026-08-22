% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Partial Withdrawal Reading (Discretionary Scope)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the partial_withdrawal_reading of the UNSC
 *   242 withdrawal clause kernel. The reading treats the English indefinite
 *   article ('from territories') and the 'secure boundaries' language as
 *   deliberately discretionary, permitting the occupying power to retain
 *   strategic territories during a phased, conditional withdrawal process.
 *   The constraint operates in international diplomatic and legal space: it
 *   coordinates a post-war settlement framework while asymmetrically
 *   extracting territorial control from claimant parties who lack a fixed
 *   enforcement line. It is actively maintained against the
 *   maximal_withdrawal_reading (which treats the French definite article as
 *   controlling and mandates total withdrawal) and against challenges to the
 *   interpretive authority structure itself.
 *
 * KEY AGENTS:
 *   - occupying_power: Post-war state retaining strategic territories under the indefinite textual reading (powerful/generational/constrained).
 *   - mediating_powers: Great-power and institutional intermediaries controlling phased withdrawal sequencing (institutional/generational/mobile).
 *   - territorial_claimants: Dispossessed parties seeking fixed boundaries without enforcement line (powerless/generational/identity_locked).
 *   - maximal_withdrawal_advocates: Legal scholars and ICJ-aligned actors pressing the definite-article interpretation (organized/biographical/constrained).
 *   - international_legal_historians: Analytical observers documenting drafting history and textual divergence (analytical/civilizational/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Partial Withdrawal Reading (Discretionary Scope)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'fbcb78e3-fa24-4b82-be9b-c439f448d854').
narrative_ontology:cs_kernel_codification('fbcb78e3-fa24-4b82-be9b-c439f448d854', fixed_text).
narrative_ontology:cs_authority_grounding('fbcb78e3-fa24-4b82-be9b-c439f448d854', lineage).
narrative_ontology:cs_interpretation_layer_present('fbcb78e3-fa24-4b82-be9b-c439f448d854').
narrative_ontology:cs_reading_relation('fbcb78e3-fa24-4b82-be9b-c439f448d854', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbcb78e3-fa24-4b82-be9b-c439f448d854', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('fbcb78e3-fa24-4b82-be9b-c439f448d854', foundational, withdrawal_scope_discretionary_per_drafters_intent).
narrative_ontology:cs_axiom_status(withdrawal_scope_discretionary_per_drafters_intent, holdable).
narrative_ontology:cs_axiom_grounding('fbcb78e3-fa24-4b82-be9b-c439f448d854', withdrawal_scope_discretionary_per_drafters_intent, conventional).
narrative_ontology:cs_axiom('fbcb78e3-fa24-4b82-be9b-c439f448d854', foundational, secure_boundaries_permit_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permit_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('fbcb78e3-fa24-4b82-be9b-c439f448d854', secure_boundaries_permit_strategic_retention, instrumental).
narrative_ontology:cs_reference_frame('fbcb78e3-fa24-4b82-be9b-c439f448d854', negotiated_secure_boundaries).
narrative_ontology:cs_drift_state('fbcb78e3-fa24-4b82-be9b-c439f448d854', contemporary_icj_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbcb78e3-fa24-4b82-be9b-c439f448d854', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains military and civilian control over strategic territories under the indefinite English textual reading of Resolution 242. Uses the discretionary scope argument to justify phased, conditional redeployment rather than total withdrawal, retaining territorial buffers and settlement infrastructure. Exit from the constraint framework is constrained by security doctrine and domestic political alignment with the retention policy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary).

% Great-power and multilateral intermediaries that control the sequencing of withdrawal and normalization through diplomatic frameworks, aid conditionality, and Security Council veto practice. They derive sustained geopolitical leverage and regional stability management authority from the indefinite, mediator-controlled phased process.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers, agenda_setter).

% Palestinian and allied Arab-state claimants seeking restoration of territories occupied in 1967. They lack a fixed enforcement line to compel total withdrawal, and their political identity is fused to the territorial claim. They bear the ongoing costs of dispossession, fragmentation, and indefinite postponement of boundary fixation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    powerless, generational, identity_locked, regional).

% Legal scholars, ICJ-aligned advocates, and claimant-state legal teams who argue that the French definite article and Charter Article 2(4) mandate total withdrawal from all occupied territories. Structurally excluded from operative interpretation by the political dominance of the English indefinite reading and the security-framed diplomacy of mediating powers.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, maximal_withdrawal_advocates, excluded,
    organized, biographical, constrained, global).

% Analytical observers who document the drafting history of Resolution 242, the divergence between English and French texts, and the subsequent interpretive practice. They neither collect from nor pay into the constraint, but provide the evidentiary basis for evaluating the drafters'-intent claim.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a phased, conditional framework for territorial redeployment that avoids immediate total withdrawal perceived as destabilizing, allowing negotiating parties to sequence normalization, security guarantees, and mutual recognition over time.
% TRANSFER_FUNCTION: Transfers territorial control and temporal flexibility from claimant parties to the occupying power, and transfers diplomatic leverage over the sequencing of withdrawal to mediating powers who manage the process.
% ABSENT_VOICES: Maximal withdrawal advocates and claimant parties pressing for fixed territorial boundaries are present in legal discourse but excluded from operative interpretation; the French-language definiteness and Charter territorial integrity default are backgrounded in enforcement practice.
% DISAPPEARANCE_RATIONALE: If the partial withdrawal reading vanished overnight, the occupying power would lose its primary textual justification for retaining strategic territories, claimant enforcement lines would crystallize around the maximal total-withdrawal reading, and the mediator-managed phased framework would collapse into an immediate demand for territorial integrity restoration.
% FOUNDING_PROBLEM: How to reconcile Israeli post-war territorial control with Arab state demands for restoration after the 1967 conflict, while allowing negotiated secure boundaries rather than imposing an immediate return to pre-war lines rejected by the occupying power.
% FOUNDING_PROBLEM_CORROBORATION: The drafting states (UK, US) attest the intentional indefiniteness to permit negotiated borders. Claimant parties and ICJ advisory opinions contest that this indefinite frame was ever legitimate; external legal historians corroborate the textual divergence but dispute its operative meaning.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint is genuinely conditional and phased; it is not total extraction but leveraged indefinite retention. Suppression (0.60) reflects the active diplomatic, interpretive, and veto-based work required to maintain the partial reading against the maximal alternative. Theater ratio (0.45) captures the increasing share of legal argumentation and diplomatic performance devoted to justifying retention under the 'secure boundaries' framing rather than to actual territorial transfer. Accessibility collapse (0.60) indicates that while the maximal reading remains textually available, it is operationally marginalized by political practice. Resistance (0.55) reflects persistent claimant-state, civil-society, and international-legal opposition.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power experiences the constraint as a necessary security arrangement and diplomatic framework that prevents destabilizing immediate withdrawal. The territorial claimants experience it as an indefinite postponement of restoration, where the textual form itself becomes a tool of extraction. The mediating powers experience it as a functional coordination mechanism they administer, while the maximal withdrawal advocates experience it as a suppressed legal truth. The engine computes these divergences from structural position and exit options, not from subjective framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and mediating powers are structural beneficiaries of the discretionary scope: the indefinite textual frame subsidizes their security posture and diplomatic leverage (low d). Territorial claimants are the targets: they bear the cost of indefinite occupation, fragmented governance, and absence of a fixed enforcement mechanism (high d, amplified by identity-locked exit and regional scope). The excluded maximal withdrawal advocates sit at high directionality as well, since the constraint's persistence depends on suppressing their interpretive alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâsecuring recognized boundaries after the 1967 conflict without imposing an immediate return to pre-war linesâwas genuine, and the partial reading provided a real coordination function by creating a phased negotiation framework. However, the persistence of the arrangement far beyond its original security context, without a sunset clause, and with active suppression of the maximal alternative, prevents classification as scaffold or rope. It is not yet a piton because the coordination function is still invoked in diplomatic practice and the occupying power actively benefits from the extraction. The mandate is contested but structurally alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_as_leverage,
    'Does the indefinite English article in UNSC 242 represent a deliberate drafters'' compromise permitting partial withdrawal, or a translation anomaly that should yield to the French definite article and Charter territorial integrity default?',
    'Archival discovery of drafting-committee intent or authoritative ICJ ruling on textual primacy and languages of equal authenticity.',
    'If definiteness is authoritative, the partial reading collapses and the constraint reclassifies toward maximal enforcement; if indefiniteness is deliberate, the partial reading retains its structural leverage and extractive capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_as_leverage, conceptual, 'Whether the kernel ambiguity is a constructed lever or a genuine drafting compromise.').

omega_variable(
    enforcement_crystallization,
    'Can claimant parties convert the maximal withdrawal norm into a fixed enforcement line (e.g., through ICC or ICJ proceedings, or multilateral sanctions), or does the partial reading permanently preempt crystallization?',
    'Observation of international court proceedings, state recognition practices, and UNSC voting patterns regarding the occupied territories.',
    'If enforcement crystallizes, the partial reading''s extractive power diminishes and the constraint may shift toward scaffold or decay; if enforcement remains diffuse, extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_crystallization, empirical, 'Whether legal enforcement can fix the indefinite boundary.').

omega_variable(
    normative_vs_structural_suppression,
    'Is the suppression of the maximal withdrawal reading achieved primarily through structural diplomatic veto power, or through normative acceptance of the security-framing over territorial integrity?',
    'Comparative analysis of UNSC voting records, General Assembly resolutions, and regional state practice over the interval.',
    'Structural veto suppression indicates higher extractiveness and active enforcement; normative acceptance indicates lower suppression and a coordination function that is more genuinely consensual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_structural_suppression, empirical, 'Whether suppression is coercive-diplomatic or normative-internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_partial_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unsc_242_partial_tr_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(unsc_242_partial_tr_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(unsc_242_partial_tr_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(unsc_242_partial_tr_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(unsc_242_partial_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(unsc_242_partial_tr_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 48, 0.43).
narrative_ontology:measurement(unsc_242_partial_tr_t57, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 57, 0.45).

% Extraction over time
narrative_ontology:measurement(unsc_242_partial_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(unsc_242_partial_be_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(unsc_242_partial_be_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(unsc_242_partial_be_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(unsc_242_partial_be_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 32, 0.54).
narrative_ontology:measurement(unsc_242_partial_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(unsc_242_partial_be_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement(unsc_242_partial_be_t57, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 57, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unsc_242_withdrawal_clause__partial_withdrawal_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNSC 242 withdrawal clause kernel. The partial withdrawal reading (discretionary scope, indefinite article) and the maximal withdrawal reading (mandatory total withdrawal, definite article) are structurally distinct constraints linked through textual ambiguity. A third constraint, interpretive_authority_structure, captures the contested authority to resolve that ambiguity. Each has its own epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
