% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Nicene Homoousios â Metaphysical Equality Reading
 *   domain: historical/theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoousios clause of the Nicene Creed, read as strict ontological
 *   equality between Father and Son, functions as a metaphysical boundary
 *   constraint within the fourth- and fifth-century imperial church. This
 *   reading instantiates one commitment system of the homoousios_nicene
 *   kernel: it treats 'of one substance' as asserting full metaphysical
 *   identity, co-eternality, and the exclusion of any subordination in being.
 *   Through ecumenical councils (Nicaea 325, Constantinople 381, Ephesus 431,
 *   Chalcedon 451), this reading was enforced by conciliar authority backed
 *   with imperial coercion. The enforcement distributed interpretive monopoly
 *   to the Nicene episcopal hierarchy, anathematized Arian and
 *   subordinationist christologies, and suppressed alternative readings of
 *   the same kernel. The constraint carries both a genuine coordination
 *   functionâresolving theological fragmentation through a precise
 *   Trinitarian formulaâand asymmetric extractionâcentralization of
 *   sacramental and doctrinal power in the episcopal hierarchy.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: Primary agenda-setter and beneficiary (institutional/generational) â convenes councils, defines orthodoxy, captures interpretive monopoly.
 *   - imperial_orthodox_enforcers: Secondary agenda-setter and beneficiary (institutional/generational) â supplies coercive enforcement, gains imperial religious unity.
 *   - arian_christians: Primary payer (organized/trapped) â anathematized, dispossessed, excluded from legal standing.
 *   - subordinationist_theologians: Secondary payer (moderate/identity_locked) â silenced, deposed, epistemically marginalized.
 *   - non_episcopal_theologians: Excluded voice (moderate/constrained) â structurally absent from conciliar deliberations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.74).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.9).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Nicene Homoousios â Metaphysical Equality Reading").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical/theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2').
narrative_ontology:cs_kernel_codification('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', fixed_text).
narrative_ontology:cs_authority_grounding('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', lineage).
narrative_ontology:cs_interpretation_layer_present('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2').
narrative_ontology:cs_reading_relation('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', foundational, homoousios_strict_ontological_identity).
narrative_ontology:cs_axiom_status(homoousios_strict_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', homoousios_strict_ontological_identity, theological).
narrative_ontology:cs_axiom('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', foundational, co_eternal_no_subordination).
narrative_ontology:cs_axiom_status(co_eternal_no_subordination, holdable).
narrative_ontology:cs_axiom_grounding('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', co_eternal_no_subordination, theological).
narrative_ontology:cs_reference_frame('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', nicene_orthodox_reference).
narrative_ontology:cs_drift_state('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', post_theodosian_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ca4ad15-1a68-49c3-ae3f-0f95d7aeffd2', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_orthodox_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_christians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and presides over ecumenical councils, defines the orthodox Trinitarian formula, issues anathemas against dissent, and gains a monopoly on legitimate theological interpretation and sacramental authority across the imperial church. Exit from this role means abandoning episcopal office and its authority, which is existentially and politically costly.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, beneficiary).

% Supplies coercive enforcement for conciliar decisions through imperial edicts, deposition of heterodox bishops, exile, and seizure of church property. Benefits from a unified imperial church that legitimizes rule and reduces religiously motivated civil strife. Can alter theological alignment for political reasons, though doing so undermines the constraint's stability.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_orthodox_enforcers, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_orthodox_enforcers, beneficiary).

% Profess that the Son is of different substance or is created. Under the Nicene enforcement regime, they are anathematized, lose legal standing and church property, face exile, and are barred from imperial patronage. Their theological community is declared illegitimate; exit requires renouncing their christology and accepting homoousios, which amounts to theological surrender and community dissolution.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_christians, payer,
    organized, generational, trapped, global).

% Hold that the Son derives being from the Father or is functionally subordinate. Their theological method and personal identity are fused with this framework. Under the constraint, they are silenced, deposed, or exiled; exit requires reconstructing their entire theological epistemology and self-concept, which most experience as impossible without psychic and social rupture.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians, payer,
    moderate, biographical, identity_locked, global).

% Lay ascetics, monastic teachers, and local prophetic voices who possess theological insight but lack episcopal ordination. They are structurally excluded from conciliar deliberations where interpretive power is distributed exclusively to the episcopal hierarchy; their contributions enter the record only through episcopal mediation or posthumous vindication.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, non_episcopal_theologians, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of theological fragmentation in the imperial church by establishing a single, metaphysically precise Trinitarian boundary (homoousios as strict ontological identity) that bishops and congregations can coordinate around, preventing endless local variation in christological confession and worship practice.
% TRANSFER_FUNCTION: Moves interpretive authority and sacramental legitimacy from heterodox teachers and communities to the Nicene episcopal hierarchy; moves anathema, civil disability, exile, and loss of church property onto Arian and subordinationist Christians.
% ABSENT_VOICES: Subordinationist theologians and Arian bishops are formally excluded from conciliar deliberations after Nicaea; their objections survive only in polemical refutations, not in official acts. Monastic and lay voices with non-episcopal theological methods are also structurally absent from the conciliar setting, despite significant theological influence.
% DISAPPEARANCE_RATIONALE: If the homoousios constraint vanished, the episcopal hierarchy would lose its monopoly on Trinitarian interpretation, Arian and subordinationist communities would reclaim churches and teaching posts, imperial religious policy would lose its theological anchor, and the boundary between orthodoxy and heresy would revert to local negotiation rather than conciliar fiat.
% FOUNDING_PROBLEM: Theological fragmentation in the imperial church threatened imperial unity and ecclesial coherence; multiple incompatible christologies (Arian, subordinationist, modalist) produced competing episcopal factions and popular confusion about the nature of Christ and the proper object of worship.
% FOUNDING_PROBLEM_CORROBORATION: Nicene bishops attest the problem is still live, citing persistent heresy. Arian historians and modern critical scholars attest that the fragmentation was partly manufactured by the conciliar process itself and that pre-Nicene local diversity was manageable; Eusebius of Caesarea provides ambiguous contemporary testimony, and modern secular historiography corroborates that the crisis was as much political as theological.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the constraint transfers substantial interpretive and sacramental authority to the episcopal hierarchy while exacting anathema and civil disability from heterodox parties. Suppression (0.90) is near-maximal because persistence depends on active conciliar and imperial enforcement, including deposition of bishops, exile, and property seizure. Theater ratio (0.40) reflects moderate performativity: the Trinitarian theology is substantively meaningful, but a significant share of conciliar activity performs and consolidates power rather than refining metaphysics. Accessibility collapse (0.70) indicates that alternative readings (Arian, subordinationist) are heavily marginalized but persist regionally. Resistance (0.75) captures the sustained Arian resurgences under Constantius II and Valens, which actively contested the constraint. Measurements track the hardening of enforcement from Nicaea (325) through Chalcedon (451) on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The episcopal hierarchy experiences the constraint as necessary coordination around a metaphysical boundary that preserves ecclesial unity; from this seat the theology is primary and the costs to dissenters are justified by the gravity of heresy. The Arian and subordinationist seats experience the same structure as coercive extraction that suppresses legitimate theological difference and centralizes power. The engine computes this divergence from the identical structural data: low directionality for the hierarchy, high directionality for the anathematized.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (nicene_episcopal_hierarchy, imperial_orthodox_enforcers) derive low directionality: these agents are subsidized by the constraint's allocation of interpretive monopoly and political unity. Victim declarations (arian_christians, subordinationist_theologians) derive high directionality: they bear the extraction directly through anathema, exclusion, and loss of institutional standing. The non-episcopal theologians are excluded rather than directly targeted and sit outside the primary extraction flow. Power and exit modulate the derived d: the hierarchy is institutional with constrained exit, while Arian Christians are organized but trapped (collective capacity yet no legal exit), and subordinationist theologians are identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiaries and victims. A purely theological reading without the victim set would misclassify as rope; a purely political reading without the coordination function would misclassify as snare. The presence of a genuine coordination problem (preventing Trinitarian fragmentation) alongside asymmetric extraction (episcopal power consolidation) and active enforcement (anathemas, imperial edicts) places it in tangled_rope. If the enforcement were removed and the formula persisted by consensus alone, it would drift toward rope; if the coordination function were shown to be entirely cover for power, it would collapse to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_coordination_vs_political_extraction,
    'Does the enforcement of homoousios primarily solve a genuine theological coordination problem (preventing fragmentation of Trinitarian worship), or does it primarily serve to consolidate episcopal and imperial power?',
    'Comparative analysis of theological diversity tolerance in pre-Nicene local churches versus post-Nicene centralized enforcement; assessment of whether the anathemas track metaphysical error or political threat.',
    'If primarily political extraction, classification shifts toward snare; if primarily theological coordination with asymmetric side-effects, tangled rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coordination_vs_political_extraction, conceptual, 'Ambiguity between genuine theological coordination and power consolidation.').

omega_variable(
    coercion_source_ambiguity,
    'Is the suppression of alternative readings driven by the internal logic of the Trinitarian metaphysics, or by imperial-coercive enforcement external to the theology?',
    'Historical comparison of theological dispute resolution in periods of weak imperial involvement versus strong imperial involvement.',
    'If suppression collapses without imperial backing, the constraint''s persistence is externally enforced; if it persists, the suppression is structurally theological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_source_ambiguity, empirical, 'Whether suppression is intrinsic or extrinsic to the theological claim.').

omega_variable(
    kernel_reading_foreclosure_validity,
    'Does the metaphysical equality reading genuinely foreclose the honorific similarity and subordinationist readings, or do interpretive ambiguities in fourth-century Greek usage of ousia and hypostasis permit logical coexistence?',
    'Historical-semantic analysis of ousia/hypostasis usage in fourth-century Greek theology; assessment of whether the terms were sufficiently precise to logically exclude sibling readings at the time of Nicaea.',
    'If the foreclosure is retrospectively imposed rather than contemporaneously precise, the constraint''s suppression metric is higher than its logical necessity suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_validity, conceptual, 'Whether the reading foreclosure is logically tight or historically constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(homo_tr_t340, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 340, 0.22).
narrative_ontology:measurement(homo_tr_t355, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 355, 0.28).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 381, 0.35).
narrative_ontology:measurement(homo_tr_t431, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 431, 0.38).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 451, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.4).
narrative_ontology:measurement(homo_be_t340, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 340, 0.48).
narrative_ontology:measurement(homo_be_t355, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 355, 0.58).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 381, 0.68).
narrative_ontology:measurement(homo_be_t431, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 431, 0.71).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 451, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.5).
narrative_ontology:measurement(homo_su_t340, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 340, 0.62).
narrative_ontology:measurement(homo_su_t355, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 355, 0.75).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 381, 0.85).
narrative_ontology:measurement(homo_su_t431, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 431, 0.88).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 451, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraints: metaphysical_equality_reading (strict ontological identity, enforced by conciliar authority), subordinationist_reading (compatible with derivation or functional subordination), and honorific_similarity_reading (honorific unity without ontological reduction). Each reading has distinct epsilon, beneficiary/victim structure, and classification. This constraint is the metaphysical equality reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
