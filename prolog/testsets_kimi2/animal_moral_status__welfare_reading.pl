% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Regulated Use Framework
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint is the welfare reading of the animal moral status kernel:
 *   a regulatory and normative framework that treats animals as sentient
 *   beings owed duties of non-cruelty, while permitting their continued use
 *   in agriculture, research, and entertainment under humane standards. It
 *   stands between the property reading (no moral standing) and the
 *   abolitionist reading (use itself is violation). The constraint
 *   coordinates public behavior to reduce suffering but extracts from animals
 *   by legitimizing their instrumentalization. Its beneficiaries gain social
 *   license, profit, and institutional roles; its victims are the animals who
 *   remain trapped in systems of regulated exploitation.
 *
 * KEY AGENTS:
 *   - regulated_industries (powerful/constrained beneficiary) â gain continued license to use animals
 *   - welfare_organizations (organized/constrained beneficiary) â gain legitimacy and funding as welfare intermediaries
 *   - public_consumers (organized/mobile beneficiary) â gain moral comfort and cheap products
 *   - animals_in_regulated_use (powerless/trapped payer) â bear the costs of humane confinement and slaughter
 *   - animal_welfare_regulators (institutional/analytical agenda_setter) â administer the standards
 *   - abolitionist_advocates (moderate/mobile excluded) â oppose the framework's foundational premise but lack structural voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.5).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Regulated Use Framework").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'aa37526b-8f8a-404f-b3e7-6abe58a6a801').
narrative_ontology:cs_kernel_codification('aa37526b-8f8a-404f-b3e7-6abe58a6a801', formalized).
narrative_ontology:cs_authority_grounding('aa37526b-8f8a-404f-b3e7-6abe58a6a801', expertise).
narrative_ontology:cs_interpretation_layer_present('aa37526b-8f8a-404f-b3e7-6abe58a6a801').
narrative_ontology:cs_reading_relation('aa37526b-8f8a-404f-b3e7-6abe58a6a801', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa37526b-8f8a-404f-b3e7-6abe58a6a801', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('aa37526b-8f8a-404f-b3e7-6abe58a6a801', foundational, sentience_generates_welfare_duty_not_rights).
narrative_ontology:cs_axiom_status(sentience_generates_welfare_duty_not_rights, holdable).
narrative_ontology:cs_axiom_grounding('aa37526b-8f8a-404f-b3e7-6abe58a6a801', sentience_generates_welfare_duty_not_rights, deontological).
narrative_ontology:cs_axiom('aa37526b-8f8a-404f-b3e7-6abe58a6a801', foundational, humane_use_socially_permissible).
narrative_ontology:cs_axiom_status(humane_use_socially_permissible, holdable).
narrative_ontology:cs_axiom_grounding('aa37526b-8f8a-404f-b3e7-6abe58a6a801', humane_use_socially_permissible, conventional).
narrative_ontology:cs_reference_frame('aa37526b-8f8a-404f-b3e7-6abe58a6a801', regulated_use_with_minimized_suffering).
narrative_ontology:cs_drift_state('aa37526b-8f8a-404f-b3e7-6abe58a6a801', contemporary_abolitionist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa37526b-8f8a-404f-b3e7-6abe58a6a801', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, public_consumers).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_based_moral_consideration).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, anti_cruelty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate animal-use enterprises under welfare standards. Receive continued legal permission to exploit animals, social license, and profit. Pivoting away from animal use is possible but requires fundamental business-model change.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_industries, beneficiary,
    powerful, biographical, constrained, national).

% Monitor, inspect, and advocate within the welfare framework. Gain institutional legitimacy, funding, and policy access as recognized intermediaries. Their current role depends on the persistence of regulated use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, beneficiary,
    organized, biographical, constrained, national).

% Purchase animal products with moral comfort provided by welfare labels and regulatory assurance. Benefit from cultural normalization and continued availability. Plant-based alternatives exist but require behavioral and dietary change.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, public_consumers, beneficiary,
    organized, immediate, mobile, national).

% Live under regulated confinement, handling, and slaughter regimes marketed as humane. Bear the costs of instrumentalization, early death, and physical manipulation. No exit from the system.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Draft, administer, and enforce anti-cruelty and welfare standards. Set inspection schedules and prosecute violations. Authority derives from statutory mandate and veterinary expertise. They could alter standards but operate within the permissibility-of-use frame.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that use itself violates animal interests regardless of welfare standards. Present in public discourse but structurally excluded from the regulatory framework, which presupposes use as permissible.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human conduct toward animals to reduce suffering below unregulated baselines by establishing inspectable standards for housing, handling, transport, and slaughter.
% TRANSFER_FUNCTION: Moves moral license and economic surplus from the public and animals to regulated industries and consumers, while moving institutional legitimacy and funding to welfare organizations; animals bear the costs of confinement, instrumentalization, and early death under humane conditions.
% ABSENT_VOICES: Abolitionist advocates who regard all use as exploitation are present in discourse but excluded from the framework's design, which presupposes use as permissible.
% DISAPPEARANCE_RATIONALE: Without the welfare framework, the social license for intensive animal use would erode rapidly, consumer markets would shift, welfare organizations would lose their intermediary role, and abolitionist or plant-based alternatives would gain institutional traction.
% FOUNDING_PROBLEM: Unregulated animal use produced visible, unchecked cruelty and public scandal that threatened market stability and social peace; the framework was built to prevent race-to-the-bottom practices and to restore public trust.
% FOUNDING_PROBLEM_CORROBORATION: Veterinary historians and legal scholars attest the problem of unregulated cruelty in the 19th and 20th centuries. Abolitionist advocates and critical animal studies scholars outside the beneficiary set attest that the founding problem has shifted: the system of regulated use itself is now the primary harm, not merely its lack of regulation.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: welfare standards do reduce some suffering relative to absence, but the framework still instrumentalizes sentient beings and channels economic surplus to human beneficiaries. Suppression (0.50) reflects the moderate but real suppression of abolitionist alternatives through regulatory framing and public discourse that treats use as default. Theater ratio (0.40) captures the significant humane-washing gap between standard-setting and on-the-ground practice. Accessibility collapse (0.35) is moderate: alternatives (veganism, abolition) exist but are culturally and economically marginalized. Resistance (0.55) is substantial from abolitionist movements and growing plant-based sectors.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and welfare-organization seats, the constraint appears as genuine coordination preventing worse suffering; from the animal seat it is extraction with a softer edge; from the abolitionist seat it is a snare that legitimizes violence. The engine computes this divergence from structural data â the authored claim of tangled_rope is independent.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals_in_regulated_use are full targets (trapped, powerless) â the constraint extracts directly from their bodies and lives. Regulated_industries and public_consumers are beneficiaries: they collect economic surplus and moral comfort. Welfare_organizations are beneficiaries of legitimacy but constrained by their dependence on the use-regime. Regulators sit near symmetric: they administer the constraint without personally collecting extraction, but their institutional identity is fused with the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination function (real suffering reduction) and asymmetric extraction (animals still pay with their lives). Without the victim declaration, it would read as rope; without the beneficiary/coordination declaration, it would read as snare. The tangled_rope gate forces both to be present. The founding problem (unregulated cruelty) is contested because the current problem may be use itself, suggesting possible scaffold or piton drift, but the active enforcement and live beneficiaries keep it tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_use_boundary,
    'Is the permission of animal use within welfare frameworks a stable moral compromise, or does it structurally require escalating extraction to maintain?',
    'Comparative longitudinal study of welfare regimes: if welfare improvements consistently lag behind production intensification, the framework serves extraction.',
    'If extraction is structurally required, the constraint is more extractive than its coordination function suggests; if not, it may approximate genuine scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_use_boundary, empirical, 'Whether welfare and use are structurally compatible or inherently opposed.').

omega_variable(
    abolitionist_voice_exclusion,
    'Are abolitionist voices structurally excluded from welfare governance by design, or merely outcompeted in democratic deliberation?',
    'Analysis of regulatory advisory panels and legislative history: if abolitionist testimony is systematically omitted from official reports despite public presence, exclusion is structural.',
    'If structural, suppression is higher than formal metrics indicate; if deliberative, the constraint is less coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_voice_exclusion, empirical, 'Whether abolitionist exclusion is architectural or electoral.').

omega_variable(
    use_permissibility_axiom,
    'Does the welfare reading''s core axiomâthat use is permissible if suffering is minimizedâforeclose the abolitionist reading within a single normative framework?',
    'Formal analysis of the logical relationship between the axioms; observation of whether any coherent single framework has successfully integrated both.',
    'If the readings are logically foreclosing, the kernel is rationally unresolved; if merely coexistent, the dispute is strategic rather than logical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(use_permissibility_axiom, conceptual, 'Logical relationship between welfare and abolitionist axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amswr_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(amswr_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(amswr_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(amswr_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(amswr_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(amswr_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(amswr_tr_t60, animal_moral_status__welfare_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(amswr_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(amswr_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(amswr_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(amswr_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(amswr_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(amswr_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(amswr_be_t60, animal_moral_status__welfare_reading, base_extractiveness, 60, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_moral_status__welfare_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
