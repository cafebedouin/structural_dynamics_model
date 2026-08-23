% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousios Christological Formula (Semi-Arian Position)
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The homoiousios ('like in substance') formula emerged after Nicaea (325)
 *   as a conservative Eastern alternative to homoousios ('same substance').
 *   It claimed to solve the coordination problem of confessing Christ's
 *   divinity in scriptural language while preserving the Father's monarchy.
 *   Under Constantius II (337-361), it became an imperially enforced creed
 *   (the 'Dated Creed' of 359), requiring assent on pain of exile. The
 *   formula genuinely coordinated a theological middle ground but extracted
 *   compliance through state power — a tangled rope. Its collapse at
 *   Constantinople I (381) shows the coordination function could not survive
 *   without the extraction machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.58).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.65).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousios Christological Formula (Semi-Arian Position)").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, '59d07741-3a7f-4350-b4df-4ce2048752a9').
narrative_ontology:cs_kernel_codification('59d07741-3a7f-4350-b4df-4ce2048752a9', fixed_text).
narrative_ontology:cs_authority_grounding('59d07741-3a7f-4350-b4df-4ce2048752a9', lineage).
narrative_ontology:cs_interpretation_layer_present('59d07741-3a7f-4350-b4df-4ce2048752a9').
narrative_ontology:cs_reading_relation('59d07741-3a7f-4350-b4df-4ce2048752a9', nicene_christological_kernel__homoousios_reading, forecloses).
narrative_ontology:cs_axiom('59d07741-3a7f-4350-b4df-4ce2048752a9', foundational, christ_distinct_from_father_in_substance).
narrative_ontology:cs_axiom_status(christ_distinct_from_father_in_substance, holdable).
narrative_ontology:cs_axiom_grounding('59d07741-3a7f-4350-b4df-4ce2048752a9', christ_distinct_from_father_in_substance, deontological).
narrative_ontology:cs_axiom('59d07741-3a7f-4350-b4df-4ce2048752a9', secondary, scriptural_vocabulary_sufficient_for_christology).
narrative_ontology:cs_axiom_status(scriptural_vocabulary_sufficient_for_christology, holdable).
narrative_ontology:cs_axiom_grounding('59d07741-3a7f-4350-b4df-4ce2048752a9', scriptural_vocabulary_sufficient_for_christology, conventional).
narrative_ontology:cs_reference_frame('59d07741-3a7f-4350-b4df-4ce2048752a9', scriptural_monotheistic_framework).
narrative_ontology:cs_drift_state('59d07741-3a7f-4350-b4df-4ce2048752a9', post_nicaea_325, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59d07741-3a7f-4350-b4df-4ce2048752a9', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_church_networks).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_autonomy_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_uniformity_enforcement).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, institutional_cohesion_party).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, homoiousian_bishops).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, laity_and_monastics).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, scriptural_monotheism_preserved).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, father_son_distinction_maintained).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians (Basil of Ancyra, George of Laodicea, Macedonius) who formulated and defended the homoiousios formula. They controlled regional synods in the East, administered the creedal test, and gained ecclesiastical authority through imperial patronage under Constantius II. Their identity fused with the formula — abandoning it meant losing their episcopal office and theological self-understanding.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousian_bishops, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, homoiousian_bishops, beneficiary).

% Churches in Asia Minor, Syria, and Palestine that used the homoiousios formula to resist centralized doctrinal control from Alexandria and Rome. The formula gave them a theological basis for autonomy while remaining within the imperial church. Exit meant either submitting to homoousios (centralization) or joining the anomoean fringe (isolation).
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_church_networks, beneficiary,
    moderate, biographical, constrained, regional).

% Theologians and biblical scholars who argued that homoiousios (unlike homoousios) used scriptural language and preserved the Father-Son distinction evident in the Gospels. They benefited from the formula's protection of interpretive freedom against a single imposed vocabulary. Their exit was constrained by the need to remain in communion with the imperial church.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_autonomy_faction, beneficiary,
    moderate, biographical, constrained, continental).

% The imperial bureaucracy (Constantius II, Valens) that sought a single creedal formula to unify the empire. The homoiousios position fragmented this unity by offering a rival formula that split Eastern churches. They paid in lost political cohesion, military distraction from doctrinal disputes, and the cost of enforcing compliance through exile and synods. Their exit was mobile — they could switch to enforcing homoousios (as Theodosius I did).
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_uniformity_enforcement, payer,
    institutional, immediate, mobile, continental).

% The broader episcopal structure and conciliar machinery that required doctrinal consensus to function. The homoiousios formula created a durable schism within the Eastern episcopate, paralyzing councils and forcing repeated imperial intervention. They were trapped because the church's institutional legitimacy depended on resolving the dispute, yet no resolution was possible without coercing one side.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, institutional_cohesion_party, payer,
    organized, generational, trapped, continental).

% Bishops and communities (Athanasius, the Cappadocians, later Theodosius's supporters) who held the homoousios formula as non-negotiable. They were structurally excluded from the homoiousios consensus — their refusal to assent meant exile, deposition, and persecution. Their identity was fused to homoousios; exit meant theological apostasy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoousian_loyalists, excluded,
    organized, generational, identity_locked, continental).

% Radical Arians (Aetius, Eunomius) who denied any likeness of substance. They were excluded by both homoiousios and homoousios positions as heretical. The homoiousios formula's claim to be a 'middle way' specifically defined itself against them. Their exit options were constrained to marginal communities or flight beyond imperial borders.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, anomoean_heteroousians, excluded,
    moderate, biographical, constrained, regional).

% Ordinary Christians and monastic communities subjected to shifting imperial creeds. They bore the cost of episcopal turnover, liturgical disruption, and the cognitive burden of tracking which formula was currently orthodox. Exit was effectively trapped — geographic mobility was low, and religious identity was bound to the local bishop's communion.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, laity_and_monastics, payer,
    powerless, biographical, trapped, local).

% Modern scholarly observer analyzing the constraint from outside the historical moment. Sees the full structural field: the genuine theological coordination problem (how to speak of Christ's divinity without ditheism), the imperial extraction layer, and the identity-lock dynamics that made compromise impossible.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, historical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological middle term that preserves monotheistic clarity (one God, the Father) while affirming the Son's full divinity and distinct personhood — using scriptural categories ('like the Father') rather than philosophical ousia-language.
% TRANSFER_FUNCTION: Moves episcopal authority and imperial legitimacy from the center (Alexandria/Rome, homoousios) to regional Eastern sees and their theological vocabulary; moves the cost of doctrinal enforcement onto dissenting bishops and laity through exile, deposition, and liturgical disruption.
% ABSENT_VOICES: The laity and monastics who bore the disruption of repeated episcopal exiles and creedal shifts had no voice in the synods. The anomoean faction was formally anathematized and excluded from the 'middle way' negotiation. Western churches (Rome) were largely absent from the Eastern homoiousios synods.
% DISAPPEARANCE_RATIONALE: If the homoiousios formula vanished in 360, the Eastern church would not simply revert to homoousios — the theological vocabulary, episcopal alliances, and imperial patronage networks built around it would collapse, forcing a reorganization that historically required Theodosius's imposition of homoousios and the 381 Constantinople council.
% FOUNDING_PROBLEM: How to confess Christ's divinity using only scriptural language while preserving strict monotheism (the Father as the one God) and the Son's distinct personhood — avoiding both Sabellianism (collapse of distinction) and Arianism (denial of full divinity).
% FOUNDING_PROBLEM_CORROBORATION: The homoiousians themselves attested the problem was live (Basil of Ancyra's letters). The homoousian party (Athanasius, Gregory of Nazianzus) attested it was a pseudo-problem created by refusing homoousios. Modern patristic scholarship (Ayres, Behr, Khaled Anatolios) corroborates that the founding problem was genuine and structurally difficult, not merely a cover for power — but also that the homoiousios solution could not stabilize without imperial enforcement.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate but real costs: episcopal exiles, creedal tests, liturgical imposition. Suppression (0.65) is high because the formula's persistence depended on imperial enforcement against both homoousian and anomoean alternatives. Theater ratio (0.42) captures genuine theological debate increasingly overshadowed by political theater (the 'Arian' label as polemic, synods as imperial theater). Accessibility collapse (0.55) is moderate — alternatives existed but were politically dangerous. Resistance (0.72) is high from both homoousian and anomoean sides. The claimed_type 'tangled_rope' reflects the dual structure: real coordination (monotheistic clarity via scriptural language) + asymmetric extraction (imperial enforcement benefiting regional Eastern sees at the cost of imperial unity and homoousian conscience).
 *
 * PERSPECTIVAL GAP:
 *   From the homoiousian bishop's seat, the constraint is a rope — a genuine coordination solution they built and maintain. From the homoousian loyalist's seat, it is a snare — an enforced formula that exiles them for conscience. From the imperial seat, it is a failed scaffold — a temporary unity measure that fragmented the church. The engine computes this divergence from the structural data; the homoiousian claim of 'middle way' does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Homoiousian bishops are agenda_setters with identity_locked exit — their office and self-concept fused to the formula. Regional churches and exegetes are beneficiaries with constrained exit — they gain autonomy but cannot leave the imperial church. Imperial enforcement and institutional cohesion are payers: the former mobile (can switch policies), the latter trapped (the church's unity requires resolution). Homoousians and anomoeans are excluded — their refusal is the condition of the homoiousios consensus. Laity are powerless payers with trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The homoiousios formula was built for a live founding problem (monotheistic Christology in scriptural terms). By 381, the problem was contested — homoousians argued Nicaea had solved it; homoiousians argued the problem persisted. The formula's mandate atrophied because its coordination function (unifying the East) inverted into a fragmentation engine, yet the enforcement machinery persisted until Theodosius replaced it. This is not pure extraction (snare) because the coordination was real and intended; not pure coordination (rope) because enforcement was asymmetric. The mandatrophy resolution is that the arrangement outlived its coordinating capacity but not its extractive machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoiousios_coordination_vs_extraction_boundary,
    'Is the homoiousios formula''s coordination function (preserving monotheism via scriptural language) structurally separable from its extraction function (imperial enforcement of a specific vocabulary), or are they fused such that the coordination cannot exist without the extraction?',
    'Counterfactual: if Constantius II had not enforced homoiousios by exile, would the formula have stabilized as a voluntary theological consensus in the East? Evidence from the 340s-350s (pre-enforcement) vs. 359-361 (enforcement period).',
    'If separable, the extraction is an imperial imposition on a genuine theological rope; if fused, the formula is inherently a tangled rope whose coordination requires coercion. Changes classification from ''imperial capture of a rope'' to ''inherently extractive coordination''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoiousios_coordination_vs_extraction_boundary, conceptual, 'Whether the homoiousios coordination function is structurally independent of imperial enforcement.').

omega_variable(
    identity_lock_mechanism_episcopal,
    'What specific identity-fusion mechanism bound homoiousian bishops to the formula — professional identity (career/office), relational identity (communion networks), ideological identity (monotheistic conviction), or institutional identity (the Eastern episcopate ''becoming'' its formula)?',
    'Comparative analysis of episcopal behavior under pressure: those who flipped to homoousios under Julian/Valens vs. those who accepted exile. Correlation with pre-359 theological writings vs. career patterns.',
    'If primarily professional/office identity, the lock is contingent on imperial patronage; if ideological/relational, the lock persists beyond enforcement and explains the formula''s afterlife in ''Macedonian'' pneumatology. Affects whether the constraint is piton-like (inertial) or snare-like (active extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_episcopal, empirical, 'Mechanism of identity lock for homoiousian bishops under enforcement pressure.').

omega_variable(
    scriptural_vs_philosophical_vocabulary_claim,
    'Does the homoiousios formula genuinely use scriptural vocabulary (John 14:28, 1 Cor 15:28) while homoousios introduces non-scriptural ousia-language, or is this a retrospective rhetorical claim masking a substantive theological difference?',
    'Philological analysis of pre-325 and 325-360 theological usage: frequency of homoiousios/homoousios/heteroousios in Christian texts, patristic citations of scriptural ''likeness'' language, and the actual semantic work each term does in homoiousian vs. homoousian arguments.',
    'If genuine, the coordination function has independent scriptural warrant (strengthening rope character); if rhetorical, the claimed coordination is a cover for anti-Nicene politics (strengthening snare character).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_vs_philosophical_vocabulary_claim, empirical, 'Whether the homoiousios formula''s scriptural vocabulary claim is philologically substantiated or polemical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_homoiousios_tr_t325, nicene_christological_kernel__homoiousios_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(nicene_homoiousios_tr_t341, nicene_christological_kernel__homoiousios_reading, theater_ratio, 341, 0.28).
narrative_ontology:measurement(nicene_homoiousios_tr_t351, nicene_christological_kernel__homoiousios_reading, theater_ratio, 351, 0.35).
narrative_ontology:measurement(nicene_homoiousios_tr_t359, nicene_christological_kernel__homoiousios_reading, theater_ratio, 359, 0.42).
narrative_ontology:measurement(nicene_homoiousios_tr_t365, nicene_christological_kernel__homoiousios_reading, theater_ratio, 365, 0.45).
narrative_ontology:measurement(nicene_homoiousios_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.42).

% Extraction over time
narrative_ontology:measurement(nicene_homoiousios_be_t325, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 325, 0.25).
narrative_ontology:measurement(nicene_homoiousios_be_t341, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 341, 0.45).
narrative_ontology:measurement(nicene_homoiousios_be_t351, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 351, 0.55).
narrative_ontology:measurement(nicene_homoiousios_be_t359, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 359, 0.62).
narrative_ontology:measurement(nicene_homoiousios_be_t365, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 365, 0.58).
narrative_ontology:measurement(nicene_homoiousios_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nicene_homoiousios_su_t325, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 325, 0.2).
narrative_ontology:measurement(nicene_homoiousios_su_t341, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 341, 0.5).
narrative_ontology:measurement(nicene_homoiousios_su_t351, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 351, 0.65).
narrative_ontology:measurement(nicene_homoiousios_su_t359, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 359, 0.72).
narrative_ontology:measurement(nicene_homoiousios_su_t365, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 365, 0.68).
narrative_ontology:measurement(nicene_homoiousios_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.08).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, constantinopolitan_creed_381).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, christological_coercion_machinery_4th_century).

% DUAL FORMULATION NOTE:
% This constraint and homoousios_reading form a constraint family decomposing the nicene_christological_kernel. The homoiousios reading has higher extractiveness (0.58 vs. ~0.35 for homoousios post-381) because its coordination function never stabilized without imperial enforcement, whereas homoousios achieved voluntary consensus after 381. The kernel's ε-invariance principle requires separate stories: homoiousios is a tangled rope (coordination + extraction); homoousios is a rope that became a mountain (post-381 consensus with negligible extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, organized, 0.35).
constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
