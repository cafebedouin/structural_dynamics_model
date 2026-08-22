% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions — Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the conditional-reciprocity reading of the Geneva
 *   Conventions kernel: the Conventions are read as a mutual-restraint
 *   bargain among state armies, where full protection for a party's forces is
 *   conditioned on that party's own compliance with the visibility/command
 *   criteria of Article 4. Under this reading, irregular forces who cannot or
 *   do not organize under fixed command with distinctive insignia and openly
 *   carried arms forfeit POW status and receive degraded protection if
 *   captured, and civilian-immunity proportionality calculations are read
 *   more permissively in zones of presumed adversary non-compliance. This is
 *   a distinct constraint from the humanitarian_ceiling_reading (which denies
 *   that non-compliance narrows protection at all) and from the
 *   security_maximization_reading (which treats the Conventions as
 *   suspendable aspiration). Each reading has its own epsilon and is authored
 *   as a separate story; this one does not average or hedge across them.
 *
 * KEY AGENTS:
 *   - state_militaries: agenda_setter/beneficiary (institutional/arbitrage) — set and apply the Article 4 classification test
 *   - regular_uniformed_combatants: beneficiary (moderate/constrained) — automatically satisfy the compliance test
 *   - captured_irregular_fighters: payer (powerless/trapped) — bear the degraded-protection consequence of non-compliance classification
 *   - civilians_in_asymmetric_conflict_zones: payer (powerless/trapped) — bear a widened proportionality floor
 *   - irregular_force_commanders: excluded (moderate/constrained) — had no voice in setting the criteria that classify their forces as non-compliant
 *   - international_humanitarian_law_scholars: observer (analytical/analytical) — track the gap between doctrine and detention/targeting practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'b052a356-4c91-428d-9337-be1b6bd63562').
narrative_ontology:cs_kernel_codification('b052a356-4c91-428d-9337-be1b6bd63562', fixed_text).
narrative_ontology:cs_authority_grounding('b052a356-4c91-428d-9337-be1b6bd63562', lineage).
narrative_ontology:cs_interpretation_layer_present('b052a356-4c91-428d-9337-be1b6bd63562').
narrative_ontology:cs_reading_relation('b052a356-4c91-428d-9337-be1b6bd63562', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('b052a356-4c91-428d-9337-be1b6bd63562', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('b052a356-4c91-428d-9337-be1b6bd63562', foundational, protection_conditioned_on_adversary_compliance).
narrative_ontology:cs_axiom_status(protection_conditioned_on_adversary_compliance, holdable).
narrative_ontology:cs_axiom_grounding('b052a356-4c91-428d-9337-be1b6bd63562', protection_conditioned_on_adversary_compliance, conventional).
narrative_ontology:cs_axiom('b052a356-4c91-428d-9337-be1b6bd63562', secondary, article_4_criteria_as_moral_forfeiture_trigger).
narrative_ontology:cs_axiom_status(article_4_criteria_as_moral_forfeiture_trigger, holdable).
narrative_ontology:cs_axiom_grounding('b052a356-4c91-428d-9337-be1b6bd63562', article_4_criteria_as_moral_forfeiture_trigger, instrumental).
narrative_ontology:cs_reference_frame('b052a356-4c91-428d-9337-be1b6bd63562', reciprocal_restraint_bargain_1949).
narrative_ontology:cs_drift_state('b052a356-4c91-428d-9337-be1b6bd63562', post_9_11_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b052a356-4c91-428d-9337-be1b6bd63562', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_fighters).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret Article 4 eligibility determinations, classify detainees as POWs or unlawful combatants, and calibrate proportional responses to perceived adversary non-compliance. Retain full protections for their own uniformed forces while narrowing obligations toward irregular adversaries under a reciprocity theory they themselves apply.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, beneficiary).

% Fight under organized command with distinctive insignia and carry arms openly, satisfying Article 4 criteria automatically. If captured, they receive full POW protections under this reading — the conditional structure was built around their compliance profile and rewards it directly.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants, beneficiary,
    moderate, biographical, constrained, national).

% Fight without uniforms or fixed insignia, often by necessity of asymmetric capability rather than choice, and are captured. Under this reading they fall outside Article 4 protections, are classified as unlawful combatants, and receive degraded treatment — interrogation without POW safeguards, prolonged detention without POW-standard review, denial of combatant immunity for otherwise lawful acts of war. They have no capacity to alter the classification once captured.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_fighters, payer,
    powerless, immediate, trapped, regional).

% Live in areas where irregular forces operate without distinguishing themselves from the civilian population. Under this reading, proportionality calculations are applied against a backdrop of presumed irregular non-compliance, which in practice widens the zone of permissible collateral harm compared to conflicts between compliant state parties.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, payer,
    powerless, biographical, trapped, regional).

% Organize resistance or insurgent forces, frequently for reasons of asymmetric material disadvantage that make open uniformed formation tactically suicidal. Have no seat in the treaty-drafting or interpretive process that set the Article 4 criteria against which their forces are judged non-compliant.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_force_commanders, excluded,
    moderate, biographical, constrained, regional).

% Study how states apply the conditional-reciprocity reading in practice, comparing stated doctrine to detention and targeting outcomes, and publish analyses that states cite selectively when the findings support their classification practices.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides mutual assurance among state militaries that observing the laws of war will be reciprocated, making restraint individually rational rather than a unilateral disadvantage — this solves a genuine coordination problem among peer state armies with comparable capabilities.
% TRANSFER_FUNCTION: Moves protection away from captured irregular fighters and reduces the practical floor of civilian immunity in asymmetric zones, while consolidating full legal protection and reciprocal restraint around uniformed state forces.
% ABSENT_VOICES: Irregular force commanders and the populations they emerge from had no seat at the 1949 diplomatic conference that fixed the Article 4 criteria; those criteria structurally disadvantage exactly the asymmetric warfare methods available to materially weaker parties.
% DISAPPEARANCE_RATIONALE: State militaries would argue the conditional structure is essential to preventing reciprocity collapse and would resist its removal; irregular forces and civilian-protection advocates would argue the world barely changes for the worse and substantially improves for detainees and conflict-zone civilians, since the humanitarian floor would rise to unconditional. The parties genuinely dispute which world results.
% FOUNDING_PROBLEM: Post-WWII drafters sought to prevent the total-war logic of reciprocal atrocity — where each side justified escalating brutality by pointing to the other's violations — by making full protections conditional on each party maintaining a verifiable, disciplined force structure.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors and several state defense ministries attest the conditionality remains necessary to prevent free-riding by forces that exploit civilian cover. Independent IHL scholars, the ICRC's own commentary practice, and international criminal tribunals attest that the Article 4 criteria have become a classification loophole exploited to strip protections from materially weaker belligerents rather than a genuine safeguard against reciprocity collapse — this corroboration comes from bodies outside the beneficiary set of state militaries.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the conditional-reciprocity reading, applied consistently, produces a real transfer: protections concentrate on parties able to organize as uniformed regular forces (predominantly state militaries and their allies) while stripping protection from materially weaker irregular belligerents whose asymmetric methods are a function of capability disadvantage, not moral choice. Suppression (0.62) is high because the reading requires active enforcement — detention classification boards, military commissions, and interrogation regimes that operate specifically because the target population lacks POW-standard review rights. Theater ratio (0.38) is moderate: some classification hearings perform legal process without altering outcomes, but there is also a genuine coordination core (regular armies observing the laws of war toward each other) that is not merely theatrical. Accessibility collapse (0.5) and resistance (0.6) are calibrated to a genuinely contested tangled rope, not a mountain: irregular forces, human rights organizations, and international tribunals actively contest the classification framework, and alternative readings (humanitarian_ceiling) remain live and litigated, so alternatives have not collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the state-military seat, the conditional structure is a rational, mutually beneficial bargain — restraint is safe because everyone durably has skin in the reciprocal game. From the captured-irregular-fighter seat, the same structure operates as a one-way ratchet: their non-compliance (often forced by disparity in resources, not intent to violate the laws of war) is read as forfeiting a floor of protection they never had power to secure in the first place. The engine's per-seat computation should show this asymmetry as the structural fact the reading produces, not as an interpretive dispute to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries sit near the beneficiary end: they draft the classification criteria, apply them to adversaries, and retain full reciprocal protection for their own forces regardless of the adversary's actual conduct, since their forces default to Article 4 compliance by construction (uniforms, command structure). Captured irregular fighters and conflict-zone civilians sit near the target end: they cannot alter their classification once captured (trapped exit), and the criteria that determine their treatment were fixed by parties they had no part in negotiating. Regular uniformed combatants of all state parties benefit symmetrically as a class, even adversarial ones, because the reciprocal-restraint logic protects any force that satisfies the visibility/command test.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing reciprocal atrocity escalation between comparably organized state armies — was largely solved for state-to-state conflict by the mid-20th century; most interstate wars since 1949 have seen substantial (if imperfect) POW compliance. But the conditional-reciprocity reading has been redeployed against a founding problem it was not built for: asymmetric conflict against materially weaker irregular forces, where the visibility/command test functions less as an anti-abuse safeguard and more as a categorical exclusion mechanism. This is a mandatrophy signature under contest — the mandate (prevent reciprocity collapse among peers) may be dead for its original context while the mechanism (conditional protection) persists and is applied to a population it was never designed to police.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_versus_ceiling_kernel_indeterminacy,
    'Does the Geneva Conventions kernel intend a genuinely conditional bargain (protection contingent on the adversary''s own compliance profile) or an unconditional humanitarian floor that happens to use Article 4 as an administrative sorting mechanism rather than a moral forfeiture trigger?',
    'Textual and travaux preparatoires analysis of the 1949 Diplomatic Conference records, cross-referenced against subsequent ICRC customary law studies and the jurisprudence of international criminal tribunals (ICTY, ICTR) on combatant status determinations.',
    'If the ceiling reading is the kernel''s true intent, this reading''s degraded treatment of irregular fighters constitutes a misapplication rather than a legitimate reading, which would reclassify much of the measured extraction as illegitimate rather than structurally licensed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_versus_ceiling_kernel_indeterminacy, conceptual, 'Whether the kernel''s core commitment is conditional reciprocity or an unconditional floor administered via Article 4 sorting.').

omega_variable(
    asymmetric_capability_versus_bad_faith_noncompliance,
    'Is irregular-force non-compliance with Article 4 criteria typically a bad-faith choice to exploit civilian cover, or a structural consequence of capability asymmetry that makes uniformed formation tactically nonviable?',
    'Empirical case studies comparing irregular forces with resources sufficient to organize distinctively (and their compliance rates) against genuinely resource-constrained forces, to isolate capability from intent.',
    'If non-compliance is predominantly capability-driven rather than bad-faith, the reciprocity theory''s moral premise (non-compliance as forfeiture) is substantially weakened, supporting a reclassification toward snare; if predominantly bad-faith, the tangled_rope reading''s coordination justification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_capability_versus_bad_faith_noncompliance, empirical, 'Whether irregular non-compliance reflects choice or forced capability constraint.').

omega_variable(
    sibling_reading_selection_pressure,
    'Which reading of the kernel a given state actor adopts appears correlated with that state''s relative military position (dominant states favor conditional-reciprocity or security-maximization readings; weaker states and non-state actors favor the humanitarian-ceiling reading) — is this correlation causal (reading selected to license or constrain based on interest) or coincidental (reading selected on independent legal-philosophical grounds that happens to correlate with power)?',
    'Comparative analysis of state legal position papers across conflicts, tracking whether individual states'' reading preference shifts when their relative power position shifts (e.g., a state adopting the ceiling reading as an occupying power but the conditional reading as counter-insurgent).',
    'A causal finding would support treating reading-selection itself as part of the extractive structure (the choice of reading is itself strategic, not merely interpretive), strengthening the tangled_rope classification and undermining the claim that this reading is neutral legal doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Whether reading-selection correlates with, and is driven by, the selecting party''s relative power position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.34).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1980, 0.46).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the geneva_conventions_1949 kernel. humanitarian_ceiling_reading authors a substantially lower epsilon (protections held unconditional, minimal extraction) and classifies as rope or mountain-adjacent from the perspective that fixes protections as non-negotiable law. security_maximization_reading authors a substantially higher epsilon (protections treated as suspendable) and likely classifies as snare given the near-total discretion it grants states. This reading occupies the structural middle: real conditionality with real enforcement machinery and identifiable victims, hence tangled_rope. All three share the same kernel text but instantiate different authority-and-obligation structures; they are linked via affects_constraints rather than merged into one story per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
