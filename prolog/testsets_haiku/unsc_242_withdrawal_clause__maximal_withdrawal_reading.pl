% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Mandatory Withdrawal Clause (Maximal Reading)
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) established the principle that territorial
 *   acquisition by military force is inadmissible under the UN Charter. The
 *   maximal reading interprets this principle to mandate complete withdrawal
 *   of the occupying military power from ALL occupied territories without
 *   negotiated boundary adjustments. This reading anchors on the French
 *   definite article ('des territoires') in the original text, understood to
 *   mean 'the territories' (specific, complete set), whereas the English
 *   indefinite article ('from territories') permits interpretation as 'some
 *   territories' (discretionary scope). The maximal reading treats the
 *   withdrawal clause as a binding, comprehensive legal obligation—a Rope
 *   that coordinates the international order around the territorial integrity
 *   principle by binding occupiers to full retrocession. The constraint is
 *   contested: the partial-withdrawal reading interprets the same language as
 *   permitting negotiated outcomes and strategic boundary retention. The
 *   interpretive authority structure reading contests the UNSC's interpretive
 *   supremacy itself, claiming that multiple authorities (ICJ, drafting
 *   states, customary practice) have equally valid claims to define the
 *   text's meaning.
 *
 * KEY AGENTS:
 *   - Occupying military power: institutional actor bearing the withdrawal obligation and the costs of compliance (loss of territory, military repositioning, strategic adjustment, international isolation if non-compliant)
 *   - Dispossessed territorial claimants: moderate power, enforceable beneficiaries under the maximal reading; their claim is anchored in the binding withdrawal mandate
 *   - United Nations Security Council: formal agenda-setter and claimed interpreter; authority contested by the ICJ and occupying states
 *   - International Court of Justice: institutional observer claiming interpretive authority; benefits from the constraint if its judicial reading is the authoritative one
 *   - Partial-withdrawal coalition: excluded from the maximal reading's rule-making; their interpretation would reframe withdrawal as discretionary
 *   - Academic and diplomatic community: observers and interpreters; produce the consensus or contestation grounding legitimacy
 *   - Third-party regional powers: institutional payers bearing diplomatic and strategic costs of enforcing the constraint against a powerful occupier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.68).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.71).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Mandatory Withdrawal Clause (Maximal Reading)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '1d26f410-aeda-460f-8440-681d9b12bebe').
narrative_ontology:cs_kernel_codification('1d26f410-aeda-460f-8440-681d9b12bebe', fixed_text).
narrative_ontology:cs_authority_grounding('1d26f410-aeda-460f-8440-681d9b12bebe', extraction).
narrative_ontology:cs_interpretation_layer_present('1d26f410-aeda-460f-8440-681d9b12bebe').
narrative_ontology:cs_reading_relation('1d26f410-aeda-460f-8440-681d9b12bebe', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('1d26f410-aeda-460f-8440-681d9b12bebe', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('1d26f410-aeda-460f-8440-681d9b12bebe', foundational, withdrawal_mandatory_from_all_territories).
narrative_ontology:cs_axiom_status(withdrawal_mandatory_from_all_territories, holdable).
narrative_ontology:cs_axiom_grounding('1d26f410-aeda-460f-8440-681d9b12bebe', withdrawal_mandatory_from_all_territories, deontological).
narrative_ontology:cs_axiom('1d26f410-aeda-460f-8440-681d9b12bebe', secondary, french_text_controls_english_indefinite_article).
narrative_ontology:cs_axiom_status(french_text_controls_english_indefinite_article, holdable).
narrative_ontology:cs_axiom_grounding('1d26f410-aeda-460f-8440-681d9b12bebe', french_text_controls_english_indefinite_article, conventional).
narrative_ontology:cs_reference_frame('1d26f410-aeda-460f-8440-681d9b12bebe', charter_article_2_4_territorial_integrity_absolute).
narrative_ontology:cs_drift_state('1d26f410-aeda-460f-8440-681d9b12bebe', contemporary_decades_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d26f410-aeda-460f-8440-681d9b12bebe', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_law_community_endorsing_withdrawal_reading).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_power).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 endpoint) reflects that the maximal reading imposes a mandatory, comprehensive obligation on the occupier without negotiated escape clauses or boundary discretion—the obligation is non-waivable and high-cost. Suppression requirement (0.71) rises over the interval because the occupier's resistance to the constraint hardens, and maintaining the constraint requires active enforcement: UNSC resolutions, ICJ advisory opinions, diplomatic isolation, sanctions threats, international court proceedings. The occupier's incentive to evade the obligation grows as the territorial interest consolidates, so suppression must intensify. Theater ratio (0.42) reflects that the constraint's stated coordination function (preventing forcible boundary change) is genuine, but growing enforcement activity defends the maximal reading's interpretive authority rather than the underlying territorial integrity principle. The constraint operates as stated but carries increasing performative weight (diplomatic theater, repeated resolutions, academic debate) relative to material compliance—the gap between the obligation and actual withdrawal widens, and theater fills the gap. All measurements are drawn from one shared time grid (t=0, 8, 16, 24, 32, 40, 48, 55) so that every metric is authored at every examined time point. The trajectory shows initial rise in both extractiveness and suppression (intervals 0–32, corresponding to escalating enforcement pressure and occupier resistance), then stabilization at high levels (intervals 32–55, corresponding to entrenched non-compliance and routinized enforcement theater). This pattern is consistent with a mandatory constraint whose beneficiaries lack unilateral enforcement capacity and whose payer (the occupier) has sufficient institutional power to resist indefinitely while facing sustained diplomatic pressure.
 *
 * PERSPECTIVAL GAP:
 *   The occupier and the dispossessed claimants experience radically different constraint types from the same legal text. From the occupier's seat, the maximal reading is an extractive imposition: a costly obligation that transfers territorial control to a rival without negotiated compensation or boundary adjustment, enforced by institutional actors (UNSC, ICJ) over whom the occupier has limited influence. From the dispossessed claimants' seat, the same constraint is genuine coordination: it binds all states to respect boundaries, anchors their legal claim to the territory, and creates international enforcement machinery on their behalf. From the UNSC's seat, the constraint is its own institutional authority—affirming its power to mandate state behavior. From the ICJ's seat, it is an opportunity to establish judicial interpretation as the authoritative reading mechanism. From the academic community's seat, it is a live interpretive problem whose resolution determines the international legal order's structure. The engine should compute these divergent types from the structural data (beneficiary/victim, power, exit options, enforcement mechanism) independently; the claim 'Rope' reflects the maximal reading's framing, but the computed types may diverge. That divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the occupying military power: high (near 1.0), because the constraint targets them as the obligated party, they bear the withdrawal costs, their exit options are constrained (they cannot unilaterally opt out of international law without institutional isolation), and they have powerful interest in evading the obligation. Directionality for dispossessed claimants: low (near 0.0), because they benefit from the constraint without running it, they gain enforceable legal rights they did not have before, and the constraint subsidizes their position. Directionality for the UNSC and ICJ: near analytical (0.5), because both are interpreters claiming neutrality, but both benefit from the constraint insofar as it establishes their interpretive authority. Directionality for third-party regional powers: moderate-to-high (0.55–0.70), because they bear diplomatic and strategic costs of enforcing the constraint against an allied occupier, their exit is constrained (they cannot unilaterally override UNSC mandates), and they are neither the primary beneficiary nor the primary target. The beneficiary/victim declarations map to this structure: beneficiaries are the dispossessed claimants (low d) and the international law community endorsing the withdrawal reading (moderate d, institutional power, interest in constraint persistence). The victim is the occupying power (high d, constrained exit, bearing withdrawal costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The maximal-withdrawal reading claims to instantiate a Rope: genuine coordination binding all states to territorial integrity, preventing a collective-action problem (forcible boundary change), and carrying modest enforcement costs relative to the coordination benefit. The authored metrics (extractiveness 0.68, suppression 0.71) suggest the constraint may be computing as Tangled Rope or Snare rather than pure Rope, because suppression is high and the occupier's resistance is substantial. This divergence is analytically important: the claim is that the withdrawal obligation is coordinate-binding (a Rope), but the empirical operation shows the occupier bearing extractive costs without meaningful coordination benefit from the occupier's perspective. The beneficiary/victim asymmetry and the enforcement intensity suggest the constraint functions as extraction from the occupier to benefit the dispossessed claimants, even though the underlying mandate is framed as coordination. Mandatrophy analysis: the founding problem (preventing forcible territorial conquest) remains live, and the founding-problem corroboration is strong (ICJ, academic consensus, GA resolutions). The constraint shows no signs of mandate obsolescence. However, the measured gap between the stated coordination function (binding all states equally to territorial integrity) and the actual asymmetric operation (extracting from the occupier to benefit claimants) points to a possible classification tension: is this a Rope (genuine coordination) or a Tangled Rope (hybrid coordination/extraction)? The answer depends on whether the occupier's withdrawal costs are necessary coordination costs (the price of joining a binding order) or extractive surplus (costs beyond what coordination requires). The measurement trajectory (stabilization at high suppression/extractiveness after t=32) suggests the costs are extractive surplus: the constraint persists not because coordination requires it, but because the beneficiary set has sufficient institutional power to enforce it against an unwilling occupier. This points toward Tangled Rope or Snare as the computed type, even though the claimed type is Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_vs_english_article_determinacy,
    'Does the French definite article ''des territoires'' control the English indefinite article ''from territories'', or are both legitimate equally in a multilingual treaty text?',
    'Comparative treaty interpretation doctrine (Vienna Convention Article 33 on authentic multilingual texts; jurisprudence from ICJ cases on treaty language divergence, e.g., LaGrand, Territorial Dispute). Determine whether ''discrepancy'' requires the definite reading or permits both readings as equally authoritative.',
    'If French definite controls: the maximal reading is legally binding and the partial reading is foreclosed as a matter of treaty interpretation. If both are equally authoritative: the interpretive ambiguity permits the partial reading as a live alternative, and the maximal reading becomes one of two defensible readings rather than the sole authoritative one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(french_vs_english_article_determinacy, empirical, 'Whether multilingual treaty ambiguity permits discretion or mandates a specific language''s grammar.').

omega_variable(
    charter_article_2_4_mandatory_vs_discretionary,
    'Does Charter Article 2(4) create a mandatory prohibition on forcible territorial acquisition, or a statement of principle subject to practical implementation by states?',
    'ICJ case law on Article 2(4) interpretation (Nicaragua v. USA, Military and Paramilitary Activities; Armed Activities on the Territory of the Congo). Determine whether the Article creates binding obligation or aspirational norm.',
    'If mandatory: the withdrawal obligation is binding and non-negotiable, supporting the maximal reading. If discretionary: withdrawal becomes subject to state negotiation, supporting the partial reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_article_2_4_mandatory_vs_discretionary, empirical, 'Whether the Charter''s territorial integrity principle is binding or aspirational.').

omega_variable(
    unsc_interpretive_authority_vs_state_intent,
    'Is the UNSC the authoritative interpreter of its own resolutions (institutional reading), or do drafting states'' intentions control (originalist reading)?',
    'Examine drafting records of Resolution 242 (e.g., UK-US deliberations, Soviet negotiating positions, Non-Aligned Movement input). Compare to subsequent UNSC interpretations and ICJ deference to UNSC construction.',
    'If UNSC authority controls: the maximal reading is binding as the institution''s authoritative statement. If drafting intent controls: the occupying state''s (and its allies'') original intent to permit discretion may override the maximal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_interpretive_authority_vs_state_intent, conceptual, 'Whether institutional interpretation or original intent determines the constraint''s scope.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the measured suppression (0.71) primarily structural (diplomatic isolation, sanctions, international court pressure) or internalized (occupier accepting the withdrawal obligation as legitimate and policing itself)?',
    'Track occupier behavior post-withdrawal: if suppression persists after the enforcement mechanism (UNSC/ICJ pressure) is removed, reclassify as partially internalized. If suppression evaporates, it is purely structural and external.',
    'If internalized: the occupier has accepted the constraint''s legitimacy and will persist in complying even if enforcement relaxes—the constraint is more stable. If structural: the occupier complies only under coercion, and withdrawal would likely follow enforcement decay—the constraint is fragile and dependent on continuous enforcement capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether the occupier''s compliance is coerced or consensual.').

omega_variable(
    kernel_reading_vs_constraint_classification,
    'Is this constraint better understood as one reading of the UNSC 242 kernel, or as a separate constraint distinct from the partial-withdrawal reading?',
    'Test the ε-invariance principle: does adopting the maximal reading vs. the partial reading change the observable constraint''s extractiveness and suppression scores? If ε differs substantially between readings, they are separate constraints (per DP-001). If ε is invariant, they are alternative readings of the same constraint.',
    'If separate constraints: author two JSON files with distinct ε values and network them via affects_constraints. If same constraint, one reading: this single file correctly models the ambiguity as an omega variable within the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_constraint_classification, conceptual, 'Whether kernel readings are multiple constraints or alternative framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unsc_tr_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(unsc_tr_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(unsc_tr_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(unsc_tr_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(unsc_tr_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(unsc_tr_t55, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unsc_be_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(unsc_be_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(unsc_be_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(unsc_be_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(unsc_be_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 48, 0.67).
narrative_ontology:measurement(unsc_be_t55, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 55, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(unsc_su_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(unsc_su_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(unsc_su_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(unsc_su_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(unsc_su_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement(unsc_su_t55, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 55, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel unsc_242_withdrawal_clause. The kernel comprises UNSC Resolution 242 and UN Charter Article 2(4), which both sides cite but interpret contradictorily. The maximal-withdrawal reading interprets the same text to mandate complete withdrawal from all occupied territories, binding the occupier to full retrocession. The partial-withdrawal reading interprets the text as permitting negotiated boundary adjustments and strategic retention. The interpretive-authority-structure reading contests the UNSC's authority to resolve the ambiguity. All three readings reference the same Charter and Resolution text; they differ in (1) what the text mandates (mandatory vs. discretionary withdrawal), (2) which language controls (French definite vs. English indefinite article), and (3) who has authority to interpret (UNSC vs. ICJ vs. drafting states vs. customary practice). The ε values differ: the maximal reading instantiates high mandatory extractiveness (0.68) from the occupier, while the partial reading would instantiate lower discretionary extractiveness permitting negotiated escape. They are authored as separate constraint stories with distinct metrics, linked via network edges. The family relationship is: maximal_withdrawal → influences partial_withdrawal (if withdrawal is mandatory, the partial reading's discretion is constrained) AND maximal_withdrawal ← coexists_with interpretive_authority (both readings remain live until the interpretive question is resolved).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
