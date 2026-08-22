% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Nicene Homoousios under Subordinationist Reading
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The Nicene term homoousios (325) was intended to secure the Son's full
 *   ontological equality with the Father against Arian subordinationism.
 *   However, the term's pre-Nicene usage (Origen, Dionysius of Alexandria,
 *   Paul of Samosata) allowed readings where 'same substance' could mean
 *   generic unity of kind (like human beings sharing humanity) rather than
 *   numerical identity of essence. This semantic openness permitted a
 *   persistent subordinationist reading: the Son is homoousios with the
 *   Father — genuinely divine, sharing the divine essence — but derives his
 *   being from the Father and is functionally/ontologically subordinate. This
 *   reading was held by Semi-Arians (homoiousios -> homoousios shift),
 *   Homoiousians, and some post-Nicene theologians who accepted the term but
 *   resisted its metaphysical equality entailment. The constraint operates as
 *   a tangled rope: it genuinely coordinates a shared theological vocabulary
 *   across warring factions (coordination function), but asymmetrically
 *   extracts theological legitimacy from the conciliar tradition to benefit
 *   subordinationist communities (extraction function), and requires active
 *   conciliar/imperial enforcement to maintain the equality reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.72).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Nicene Homoousios under Subordinationist Reading").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, 'c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5').
narrative_ontology:cs_kernel_codification('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', formalized).
narrative_ontology:cs_authority_grounding('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', lineage).
narrative_ontology:cs_interpretation_layer_present('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5').
narrative_ontology:cs_reading_relation('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, holdable).
narrative_ontology:cs_axiom_grounding('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', son_derives_being_from_father, deontological).
narrative_ontology:cs_axiom('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', foundational, homoousios_compatible_with_subordination).
narrative_ontology:cs_axiom_status(homoousios_compatible_with_subordination, holdable).
narrative_ontology:cs_axiom_grounding('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', homoousios_compatible_with_subordination, conventional).
narrative_ontology:cs_reference_frame('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', nicene_conciliar_unity).
narrative_ontology:cs_drift_state('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', post_constantinople_381, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c9ecb6c9-486d-4b4b-a470-8ef86dadc0d5', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, arian_remnants).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, semi_arian_networks).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodoxy).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_equality_advocates).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, conciliar_tradition_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, arian_remnants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities (Arian, Semi-Arian, Homoian) that read homoousios as compatible with the Son's derivation from the Father. They gain theological space for subordination without being forced into strict metaphysical equality or honorific similarity. Their exit from the imperial church is constrained by political persecution and loss of ecclesiastical recognition.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_communities, beneficiary,
    organized, generational, constrained, continental).

% Surviving Arian networks that accept homoousios as a compromise term while maintaining the Son's subordinate generation. They benefit from the reading's legitimization but pay through ongoing marginalization by Nicene enforcement structures and loss of imperial patronage.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, arian_remnants, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, arian_remnants, payer).

% Theological groups (e.g., Basil of Ancyra, George of Laodicea) that affirm homoousios while insisting on the Son's hypostatic distinction and subordination. They gain a conciliar vocabulary that does not collapse into metaphysical identity, but operate under constant pressure from both Nicene hardliners and Arian rejection.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, semi_arian_networks, beneficiary,
    organized, biographical, constrained, continental).

% The established conciliar tradition (Athanasius, Cappadocians, later Chalcedonian orthodoxy) that reads homoousios as securing full ontological equality. This reading excludes their theological flexibility by forcing homoousios to carry subordinationist content; they lose the term's univocal witness to metaphysical identity. They retain institutional power to enforce their reading but cannot prevent the subordinationist reading from persisting as a live alternative.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodoxy, payer,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, nicene_orthodoxy, agenda_setter).

% Theologians and bishops committed to homoousios as strict identity of essence (ousia). They are excluded from the subordinationist reading's framework because it redefines the term's logical grammar. They can exit to alternative formulations (e.g., homoiousios rejection, later enhypostasia language) but lose the conciliar term's authority.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, metaphysical_equality_advocates, payer,
    powerful, generational, mobile, continental).

% The imperial-church apparatus (Constantius, Valens, Theodosius, councils) that administers doctrinal boundaries. It pays the cost of perpetual conciliar re-litigation and schism management when homoousios admits subordinationist readings. It sets the agenda by convening councils but cannot stabilize the term's meaning without continuous enforcement.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_tradition_authority, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, conciliar_tradition_authority, payer).

% Groups (e.g., certain Monarchian, Sabellian, or early Unitarian trajectories) that prioritize the Father's monarchy so absolutely that they reject homoousios entirely. They would object to any reading that grants the Son full divinity, but are structurally excluded from the Nicene vocabulary debate because their position was condemned earlier.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_monarchians, excluded,
    moderate, generational, trapped, regional).

% Modern historians of doctrine (e.g., Hanson, Ayres, Khaled Anatolios) who analyze the term's semantic range across the 4th century. They neither collect nor pay; they map the structural instability that the constraint's persistence reveals.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, patristic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a conciliar vocabulary (homoousios) that can bridge pro-Nicene and subordinationist parties without requiring either to abandon their core theological grammar — a shared term that absorbs the Son's full divinity and his derivation from the Father simultaneously.
% TRANSFER_FUNCTION: Moves theological legitimacy and imperial recognition from the conciliar tradition (which monopolized homoousios for metaphysical equality) to subordinationist communities (which gain a foothold in the official vocabulary), while the conciliar authority pays in enforcement costs and semantic instability.
% ABSENT_VOICES: Strict Monarchians/Sabellians (who reject any distinction in the Godhead) and radical Arians (who reject homoousios entirely) are excluded — they would deny the term's applicability to the Son at all, but the constraint only operates among parties who accept homoousios as a viable term.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading vanished overnight, the Nicene vocabulary would collapse into a univocal metaphysical equality claim, eliminating the theological space that allowed Arian/Semi-Arian communities to remain within the imperial church's linguistic orbit. The 4th-century doctrinal wars would have resolved faster toward a single orthodoxy, but the term's historical function as a contested bridge would be lost.
% FOUNDING_PROBLEM: The 4th-century church needed a term to express the Son's full divinity against Arian subordinationism, but the term chosen (homoousios) carried pre-Nicene associations with materialistic identity and Sabellian modalism that made it vulnerable to subordinationist re-reading — the founding problem was securing a vocabulary that could not be read as subordinationist, which homoousios failed to achieve.
% FOUNDING_PROBLEM_CORROBORATION: Athanasius and the Cappadocians attest the founding problem was live and the term failed to secure its intent (subordinationist readings persisted). Modern scholars (Hanson, 'The Search for the Christian Doctrine of God'; Ayres, 'Nicaea and its Legacy') corroborate from outside the benefiting parties that homoousios was semantically unstable and admitted the very subordination it was meant to exclude.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the conciliar tradition's loss of semantic control — the term they imposed to exclude subordinationism becomes a vehicle for it. Suppression (0.72) is high because maintaining the equality reading requires continuous conciliar legislation (Constantinople 381, Chalcedon 451) and imperial coercion against subordinationist bishops. Theater ratio (0.35) captures the performative conciliar unity masking deep semantic fracture. Accessibility collapse (0.62) is moderate: alternatives (homoiousios, heteroousios, monosyllabic silence) existed but collapsed under imperial pressure. Resistance (0.58) reflects the persistent subordinationist refusal to accept the equality reading as definitive.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinationist seat, homoousios is a genuine coordination achievement — a term that finally lets them confess the Son's full divinity without surrendering the Father's monarchy. From the Nicene orthodoxy seat, the same term is a snare — a vocabulary they imposed that refuses to stay imposed. The conciliar authority experiences it as a piton — a term they must keep enforcing at increasing cost because no alternative vocabulary has stabilized. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist communities (organized, constrained exit) are structural beneficiaries: they gain conciliar vocabulary without metaphysical equality. Nicene orthodoxy (institutional, arbitrage exit) is a victim: it loses the term's univocal witness and pays enforcement costs. Conciliar authority (institutional, analytical exit) is both agenda-setter and payer: it sets the doctrinal agenda but bears the cost of perpetual instability. Scriptural monarchians (moderate, trapped) are excluded: their stricter monotheism has no seat at the homoousios table.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (securing ontological equality via homoousios) has atrophied into its opposite — the term now structurally permits the subordination it was meant to exclude. The constraint persists not because it solves the founding problem but because no replacement vocabulary has achieved conciliar consensus. The theater of conciliar unity (Constantinople 381 reaffirming Nicaea) masks the term's functional failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_intent_vs_reception,
    'Did the Nicene fathers (Hosius, Alexander, Athanasius) intend homoousios to exclude all subordinationist readings, or did they accept a term they knew was semantically open?',
    'Patristic textual analysis of pre-325 usage, conciliar acts, and immediate post-Nicene defenses (Athanasius'' De Decretis) — specifically whether they treat the term''s meaning as self-evident or as requiring interpretive guardrails.',
    'If intent was exclusionary, the subordinationist reading is a semantic hijack (higher extraction). If intent was deliberately open, the constraint is a genuine coordination mechanism that the equality reading later tried to close (lower extraction, more rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_intent_vs_reception, conceptual, 'Whether the constraint''s extraction originates in semantic hijacking or deliberate openness.').

omega_variable(
    imperial_enforcement_as_extraction_mechanism,
    'Is the high suppression requirement driven by theological conviction or by imperial interest in a unified imperial church vocabulary?',
    'Correlation of conciliar convocation patterns with imperial succession (Constantius vs. Julian vs. Valens vs. Theodosius) and analysis of exile/recall patterns for subordinationist vs. Nicene bishops.',
    'If imperial interest dominates, the extraction is political-theological hybrid (tangled_rope confirmed). If theological conviction dominates, the constraint approaches a snare (orthodoxy extracting compliance from dissent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_enforcement_as_extraction_mechanism, empirical, 'Whether enforcement is theologically or imperially motivated.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the subordinationist reading''s core premise (Son derives being from Father) logically foreclose the metaphysical equality reading''s core premise (Son is numerically identical in essence to Father) within a single theological framework?',
    'Formal analysis of the logical relations between ''derivative divinity'' and ''numerical identity of essence'' in 4th-century metaphysical vocabularies (ousia, hypostasis, physis).',
    'If forecloses: the readings cannot coexist in one framework (reading_relations = forecloses). If coexists_with: different parties hold both simultaneously (the historical 4th-century reality). This determines cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical compatibility of subordinationist and equality readings within one framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t341, homoousios_nicene__subordinationist_reading, theater_ratio, 341, 0.28).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t357, homoousios_nicene__subordinationist_reading, theater_ratio, 357, 0.32).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t360, homoousios_nicene__subordinationist_reading, theater_ratio, 360, 0.35).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t367, homoousios_nicene__subordinationist_reading, theater_ratio, 367, 0.38).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t374, homoousios_nicene__subordinationist_reading, theater_ratio, 374, 0.4).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.35).

% Extraction over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t341, homoousios_nicene__subordinationist_reading, base_extractiveness, 341, 0.55).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t357, homoousios_nicene__subordinationist_reading, base_extractiveness, 357, 0.62).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t360, homoousios_nicene__subordinationist_reading, base_extractiveness, 360, 0.68).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t367, homoousios_nicene__subordinationist_reading, base_extractiveness, 367, 0.71).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t374, homoousios_nicene__subordinationist_reading, base_extractiveness, 374, 0.73).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t341, homoousios_nicene__subordinationist_reading, suppression_requirement, 341, 0.65).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t357, homoousios_nicene__subordinationist_reading, suppression_requirement, 357, 0.7).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t360, homoousios_nicene__subordinationist_reading, suppression_requirement, 360, 0.72).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t367, homoousios_nicene__subordinationist_reading, suppression_requirement, 367, 0.75).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t374, homoousios_nicene__subordinationist_reading, suppression_requirement, 374, 0.78).
narrative_ontology:measurement(homoousios_nicene__subordinationist_reading_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, chalcedonian_dyophysitism).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, cyrilline_miaphysitism).

% DUAL FORMULATION NOTE:
% Part of the homoousios_nicene constraint family. This reading (subordinationist) and the metaphysical_equality_reading share the same kernel but instantiate different constraints with different ε values. The subordinationist reading has higher extractiveness (0.68 vs. ~0.35 for equality) because it admits the term's semantic instability as a feature; the equality reading must enforce univocity. The honorific_similarity_reading occupies the semantic middle (homoiousios blur) with its own extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, institutional, 0.35).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, organized, 0.25).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, powerful, 0.65).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, moderate, 0.6).
constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
