% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Legitimacy Framework
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   The Amun-Ra polytheistic legitimacy framework governed New Kingdom Egypt
 *   (c. 1550–1070 BCE) and persisted in adapted forms through the Third
 *   Intermediate and Late Periods. Divine authority flows through the Amun
 *   priesthood at Thebes, who interpret the multi-deity cosmology with
 *   Amun-Ra as chief patron. This reading distributes interpretive authority
 *   across the priesthood, accommodates regional cult variation through
 *   syncretism, and constrains the pharaoh by requiring priestly validation.
 *   Temple economies are major beneficiaries, extracting surplus through
 *   ritualized obligations. The constraint is a tangled rope: it genuinely
 *   coordinates cosmic order, royal legitimacy, and inter-regional cohesion
 *   (coordination function) while asymmetrically extracting from the pharaoh,
 *   foreign deity adherents, and common producers (extraction function), and
 *   it requires active enforcement through oracle control, ritual monopoly,
 *   and temple policing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Legitimacy Framework").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, 'bc88188d-3242-405d-bf21-78e06001fc0d').
narrative_ontology:cs_kernel_codification('bc88188d-3242-405d-bf21-78e06001fc0d', distributed).
narrative_ontology:cs_authority_grounding('bc88188d-3242-405d-bf21-78e06001fc0d', lineage).
narrative_ontology:cs_interpretation_layer_present('bc88188d-3242-405d-bf21-78e06001fc0d').
narrative_ontology:cs_reading_relation('bc88188d-3242-405d-bf21-78e06001fc0d', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('bc88188d-3242-405d-bf21-78e06001fc0d', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('bc88188d-3242-405d-bf21-78e06001fc0d', foundational, amun_ra_as_chief_patron).
narrative_ontology:cs_axiom_status(amun_ra_as_chief_patron, holdable).
narrative_ontology:cs_axiom_grounding('bc88188d-3242-405d-bf21-78e06001fc0d', amun_ra_as_chief_patron, theological).
narrative_ontology:cs_axiom('bc88188d-3242-405d-bf21-78e06001fc0d', foundational, priestly_mediation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(priestly_mediation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bc88188d-3242-405d-bf21-78e06001fc0d', priestly_mediation_required_for_legitimacy, conventional).
narrative_ontology:cs_axiom('bc88188d-3242-405d-bf21-78e06001fc0d', secondary, multi_deity_cosmology_accommodates_regional_variation).
narrative_ontology:cs_axiom_status(multi_deity_cosmology_accommodates_regional_variation, holdable).
narrative_ontology:cs_axiom_grounding('bc88188d-3242-405d-bf21-78e06001fc0d', multi_deity_cosmology_accommodates_regional_variation, conventional).
narrative_ontology:cs_reference_frame('bc88188d-3242-405d-bf21-78e06001fc0d', middle_kingdom_amun_synthesis).
narrative_ontology:cs_drift_state('bc88188d-3242-405d-bf21-78e06001fc0d', new_kingdom_amun_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc88188d-3242-405d-bf21-78e06001fc0d', '2026-06-11T14:30:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, foreign_deity_adherents).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, common_producers).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, maat_as_cosmic_order).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_mediation_necessity).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, decentralized_divine_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretation of divine will through oracle, ritual, and text. Validates pharaonic authority through coronation and heb-sed ceremonies. Collects tribute, land grants, and labor from temple estates. Their identity is fused with the cosmology — exit means dissolution of the priestly self.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, beneficiary).

% Vast agricultural estates, workshops, and trade networks operated by temples. Receive mandatory offerings, corvée labor, and tax exemptions justified by divine mandate. Depend on the cosmology's authority to maintain economic privileges. Exit would mean loss of legal protections and revenue streams.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies, beneficiary,
    organized, generational, constrained, regional).

% Local priesthoods of Amun-Ra syncretized with regional deities (Amun-Min, Amun-Kamutef, etc.). Enjoy autonomy in ritual practice and local resource control. The polytheistic framework accommodates their variation. Exit means aligning with a different theological center or losing local standing.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers, beneficiary,
    moderate, generational, mobile, local).

% Nominal supreme ruler but requires priestly validation for legitimacy. Must fund temple construction, endowments, and festivals. Coronation and jubilee rituals are controlled by the priesthood. Cannot easily exit — divine kingship is the only legitimacy framework available, but priestly interpretation constrains his agency.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).

% Communities worshipping deities outside the Amun-Ra framework (e.g., Seth in Ombos, Hathor in Dendera before integration). Face marginalization, temple resource diversion, and ritual suppression. Their gods are subordinated or demonized in official theology. Exit means cultural assimilation or repression.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, foreign_deity_adherents, payer,
    powerless, biographical, trapped, local).

% Farmers, artisans, and laborers who bear corvée labor, temple taxation, and offering obligations. Receive ritual assurance and redistributive festival food in return. No meaningful exit — survival depends on the agricultural cycle governed by the temple calendar. The cosmology naturalizes their extraction.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_producers, payer,
    powerless, immediate, trapped, local).

% Reconstructs the system from inscriptions, archaeology, and comparative religion. Sees the full structural asymmetry: a coordination framework for cosmic order that extracts surplus through priestly mediation. No stake in the ancient arrangement.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, modern_egyptologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains maat (cosmic order) through a shared multi-deity cosmology that coordinates agricultural cycles, royal succession, inter-regional diplomacy, and social cohesion across the Nile Valley. The priesthood provides authoritative interpretation that prevents fragmentation into competing local theologies.
% TRANSFER_FUNCTION: Moves agricultural surplus, labor, and political legitimacy from producers and the pharaoh to the Amun priesthood and temple economies, mediated by ritual obligations (offerings, festivals, corvée) justified as maintaining divine favor.
% ABSENT_VOICES: Seth-worshipping communities in the Delta (demonized in official theology), Nubian deity traditions (subordinated as 'foreign'), and any emergent monotheistic tendencies (suppressed until Akhenaten). These voices are structurally excluded because the polytheistic framework defines them as chaotic or inferior.
% DISAPPEARANCE_RATIONALE: If the Amun-Ra legitimacy framework vanished overnight, the pharaoh would lose his primary validation mechanism, temple economies would lose their legal basis for resource extraction, regional cults would lose their integrating cosmology, and the state would need a new legitimacy substrate — as occurred historically with the Atenist revolution and later Persian/Ptolemaic adaptations.
% FOUNDING_PROBLEM: Early Dynastic and Old Kingdom fragmentation: competing local theologies, unstable royal succession, and inability to coordinate Nile Valley-wide resource mobilization for monument building and famine response. The Amun-Ra synthesis provided a unifying cosmic framework.
% FOUNDING_PROBLEM_CORROBORATION: Middle Kingdom texts (e.g., Prophecy of Neferti, Complaints of Khakheperraseneb) attest to the ongoing need for maat-maintenance. New Kingdom coronation inscriptions confirm the priesthood's validation role. Non-beneficiary corroboration: Hittite diplomatic correspondence treats Amun-Ra as the Egyptian state's divine guarantor; Greek observers (Herodotus, Diodorus) describe the system's coordinating function from outside.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the significant but not total surplus capture: temple estates controlled ~15-20% of arable land and received mandated offerings, but the state retained administrative control. Suppression (0.62) is moderate-high: the framework actively marginalizes competing theologies (Seth cults, Aten precursors) and enforces ritual compliance, but allows regional variation within the Amun-Ra umbrella. Theater ratio (0.28) is low-moderate: priestly ritual has genuine coordinating function (calendrical, diplomatic, social), but a growing share of temple activity serves economic extraction rather than cosmic maintenance. The metrics describe a system where coordination and extraction are structurally entangled — neither pure rope nor pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood's seat, this is a genuine coordination mechanism maintaining maat — the extraction is the cost of cosmic order. From the pharaoh's seat, it is a constraint on his sovereignty that extracts legitimacy and resources. From common producers' seat, it is an inescapable tax system cloaked in theology. The engine will compute different effective types per seat: likely rope/tangled_rope for beneficiaries, snare/tangled_rope for payers. The authored claim (tangled_rope) captures the system-level hybridity; seat divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood (agenda_setter/beneficiary) sits at the beneficiary end (d ≈ 0.15): they control interpretation, collect rents, and their identity is fused with the cosmology. Temple economies and regional cult centers are beneficiaries (d ≈ 0.25-0.35) with constrained but not trapped exit. The pharaoh (payer) sits near the target end (d ≈ 0.75): he holds nominal supreme power but is structurally constrained by the very priesthood that validates him. Foreign deity adherents and common producers are full targets (d ≈ 0.85-0.95): trapped by geography, survival dependence, and identity. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Nile Valley coordination under maat) remains live — the system still coordinates agricultural cycles and social cohesion. But the extraction function has grown: temple landholdings expanded from ~10% (Middle Kingdom) to ~20%+ (New Kingdom), and priestly political power rivaled the throne by the 20th Dynasty. The mandate has not atrophied; rather, the coordination function has been progressively colonized by extraction. This is not a piton (degraded function) but a tangled_rope where extraction has accumulated atop persistent coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the priestly mediation structurally necessary for the multi-deity cosmology''s coordinating function, or could maat be maintained through a different institutional form (e.g., direct pharaonic ritual, decentralized local practice)?',
    'Counterfactual analysis of periods when priestly authority weakened (First Intermediate Period, Amarna interlude, Late Period): did coordination collapse or reconfigure? Archaeological evidence for state vs. temple administration of agriculture and labor.',
    'If priestly mediation is contingent, the extraction is separable from coordination — the tangled_rope could become a rope with institutional reform. If necessary, the extraction is the price of coordination itself — the tangled_rope is structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination and extraction components are structurally separable or inextricably fused.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds the Amun priesthood to the cosmology — professional identity (career path dependence), relational identity (self-concept through divine relationship), ideological identity (worldview making exit unthinkable), or institutional identity (the priesthood has ''become'' its function)?',
    'Comparative analysis of priestly autobiographies (e.g., Harkhuf, Ahmose son of Ebana), career trajectories, and evidence for priestly dissent or defection during theological crises (Amarna period).',
    'If identity_locked is professional, exit becomes possible with institutional change. If ideological/relational, the priesthood will defend the cosmology to destruction — raising suppression and theater ratio. This determines whether the constraint is reformable or must be broken.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'The mechanism of identity lock for the primary agenda_setter/beneficiary.').

omega_variable(
    reading_relation_atenist,
    'Does the amun_polytheistic_reading foreclose, coexist with, or influence the atenist_monotheistic_reading?',
    'Structural analysis of the Amarna period: did Akhenaten''s Atenism logically require the destruction of the Amun priesthood (foreclose), or could a modified Amun cult have coexisted? Did the Amun framework''s restoration under Tutankhamun represent coexistence or foreclosure of Atenism?',
    'If forecloses: the two readings are mutually exclusive in any single framework — the kernel admits no synthesis. If coexists_with: both can be live positions simultaneously (as in late 18th Dynasty transition). If influences: the Amun framework creates structural conditions (priestly power, temple wealth) that make Atenist revolution more likely but not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_atenist, conceptual, 'Structural relationship from this reading to the Atenist sibling reading.').

omega_variable(
    reading_relation_folk,
    'Does the amun_polytheistic_reading foreclose, coexist with, or influence the folk_syncretistic_reading?',
    'Analysis of household archaeology (Deir el-Medina, Amarna workmen''s village), personal piety texts, and magical papyri: does official theology suppress folk practice, tolerate it as a safety valve, or depend on it for legitimacy?',
    'If coexists_with (likely): folk practice operates as a parallel legitimacy layer that neither rules out the other. If influences: official theology shapes folk practice''s deity repertoire (e.g., Amulet iconography follows state theology). If forecloses (unlikely): official theology would actively suppress household ritual — not evidenced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_folk, empirical, 'Structural relationship from this reading to the folk syncretistic sibling reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) primarily structural (temple police, oracle control, legal penalties for ritual non-compliance) or internalized (producers believe the cosmology is natural law, have fused identity with their ritual obligations, lack conceptual alternatives)?',
    'Post-exit suppression trajectory: examine periods of priestly weakening (Amarna, Third Intermediate Period) — did common producers immediately abandon temple obligations, or did ritual compliance persist? If internalized, suppression persists after structural enforcement decays.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint extracts more efficiently. If structural, suppression tracks enforcement capacity and is more vulnerable to institutional collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an interpersonal/institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t200, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t300, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t400, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t500, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 500, 0.28).

% Extraction over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t200, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t300, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t400, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t500, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 500, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t200, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t300, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t400, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 400, 0.6).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t500, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 500, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economy_resource_allocation).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, royal_succession_legitimacy).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. This reading (amun_polytheistic) has ε=0.58 with distributed priestly beneficiaries and pharaoh as payer. The atenist_monotheistic_reading centralizes authority in the pharaoh (ε≈0.75, pharaoh as agenda_setter/beneficiary, priesthood as victim). The folk_syncretistic_reading has low ε≈0.20 with household practitioners as beneficiaries and no concentrated victims. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerful, 0.75).
constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
