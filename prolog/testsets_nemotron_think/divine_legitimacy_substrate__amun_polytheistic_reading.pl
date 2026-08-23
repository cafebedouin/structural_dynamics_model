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
 *   human_readable: Amun-Ra Priestly Legitimacy Substrate (Polytheistic Reading)
 *   domain: ancient_history/religious_studies/political_economy_of_belief
 *
 * SUMMARY:
 *   In New Kingdom Egypt (c. 1550–1070 BCE), divine legitimacy for pharaonic
 *   rule flowed through the priestly interpretation of a multi-deity
 *   cosmology headed by Amun-Ra. The Amun priesthood, centered at Karnak but
 *   with distributed colleges at regional cult centers (Thebes, Heliopolis,
 *   Memphis, Napata), controlled the ritual calendar, oracular process, and
 *   cosmological narrative that validated royal authority. In return, the
 *   pharaoh endowed temple estates with land, labor, and tribute — creating a
 *   massive temple economy that by the Ramesside period controlled an
 *   estimated 15–20% of arable land and comparable shares of labor. The
 *   constraint is claimed as a tangled_rope: it coordinates legitimization
 *   across a culturally diverse valley, accommodates regional cult variation
 *   through a shared cosmological grammar, and manages resource flows — but
 *   it simultaneously extracts surplus for temple economies and constrains
 *   pharaonic agency through the requirement of priestly validation. The
 *   Atenist interlude (Akhenaten, c. 1353–1336 BCE) demonstrates the
 *   constraint's active enforcement: the priesthood's suppression of the
 *   Atenist reading was not passive but involved destruction of monuments,
 *   erasure of names, and restoration theology. Folk syncretistic practice
 *   persisted throughout but was denied interpretive authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.55).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.45).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Priestly Legitimacy Substrate (Polytheistic Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '27bc2176-d952-4c54-9bce-8ae06f857f78').
narrative_ontology:cs_kernel_codification('27bc2176-d952-4c54-9bce-8ae06f857f78', formalized).
narrative_ontology:cs_authority_grounding('27bc2176-d952-4c54-9bce-8ae06f857f78', lineage).
narrative_ontology:cs_interpretation_layer_present('27bc2176-d952-4c54-9bce-8ae06f857f78').
narrative_ontology:cs_reading_relation('27bc2176-d952-4c54-9bce-8ae06f857f78', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('27bc2176-d952-4c54-9bce-8ae06f857f78', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('27bc2176-d952-4c54-9bce-8ae06f857f78', foundational, amun_ra_as_cosmic_king).
narrative_ontology:cs_axiom_status(amun_ra_as_cosmic_king, holdable).
narrative_ontology:cs_axiom_grounding('27bc2176-d952-4c54-9bce-8ae06f857f78', amun_ra_as_cosmic_king, deontological).
narrative_ontology:cs_axiom('27bc2176-d952-4c54-9bce-8ae06f857f78', foundational, priesthood_as_necessary_intermediaries).
narrative_ontology:cs_axiom_status(priesthood_as_necessary_intermediaries, holdable).
narrative_ontology:cs_axiom_grounding('27bc2176-d952-4c54-9bce-8ae06f857f78', priesthood_as_necessary_intermediaries, conventional).
narrative_ontology:cs_reference_frame('27bc2176-d952-4c54-9bce-8ae06f857f78', new_kingdom_amun_theocracy).
narrative_ontology:cs_drift_state('27bc2176-d952-4c54-9bce-8ae06f857f78', late_ramesside_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27bc2176-d952-4c54-9bce-8ae06f857f78', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, folk_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_supremacy).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_interpretive_authority).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, cosmological_order_maat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the ritual calendar, oracular process, and cosmological narrative that validates royal authority. Manages temple estates, collects surplus, and interprets divine will through festivals, processions, and oracle consultations. Priestly office is hereditary and identity-fused — exit means abandoning the cosmic role that defines the self. Their authority rests on lineage succession and textual tradition.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive agricultural surplus, labor drafts, and trade monopolies through pharaonic endowments justified by priestly validation. By the Ramesside period, Amun's estate at Karnak alone controlled vast lands, workshops, and fleets. Exit would mean losing the divine mandate that secures their property rights — but they are not the primary interpreters, only the economic beneficiaries.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies, beneficiary,
    organized, generational, constrained, national).

% Local priesthoods at Thebes, Heliopolis, Memphis, Napata, and other centers operate within the Amun-Ra cosmological framework. They gain resources, autonomy, and legitimacy from the central priesthood's cosmological grammar, which accommodates their local deities as manifestations of Amun. Their exit is constrained: breaking with the Amun framework loses the coordinating cosmology but retains local cult — a partial exit.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers, beneficiary,
    organized, generational, constrained, regional).

% Nominally supreme ruler but must secure priestly legitimation for each reign (coronation, sed festival, oracular confirmation). Funds temple endowments from state surplus. Cannot easily exit: royal ka theology fuses the king's identity with the cosmological order the priesthood maintains. Akhenaten's attempt to exit (Atenist reform) collapsed within a generation, demonstrating the constraint's enforcement capacity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority, payer,
    powerful, biographical, constrained, national).

% Household and village practitioners who venerate multiple deities (Bes, Taweret, Hathor, local spirits) through pragmatic ritual — fertility, protection, healing. They bear temple taxation and labor drafts but have no voice in the cosmological narrative that justifies extraction. Their practice is tolerated as 'popular religion' but denied interpretive authority. Exit is trapped: the cosmological framework permeates daily life, and no alternative legitimizing structure exists at their scale.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, folk_practitioners, excluded,
    powerless, biographical, trapped, local).

% The Akhenaten-era faction that attempted to replace the Amun cosmology with Aten exclusivity. They were structurally excluded from the priestly interpretive apparatus and responded by seizing state power to impose a new reading. Their exit from the Amun constraint was attempted via state coercion but failed — the priesthood's suppression (monument destruction, name erasure, restoration theology) was total. Post-Amarna, they are erased from the official record.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformists, excluded,
    moderate, biographical, trapped, national).

% Modern Egyptology and comparative religion scholarship that sees the full structure: the coordination function (legitimacy across diversity), the extraction (temple economy), the enforcement (oracle monopoly, festival control), and the excluded voices (folk practice, Atenist memory). No stake in the constraint's persistence.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legitimizes pharaonic rule through divine sanction across a culturally diverse Nile Valley; coordinates resource flows to temple economies that manage grain storage, craft production, and redistribution; maintains cosmological order (maat) through a shared ritual calendar and interpretive grammar that accommodates regional cult variation.
% TRANSFER_FUNCTION: Moves agricultural surplus, labor, and political legitimacy from pharaonic authority and populace to temple economies via ritual obligation (temple endowments, festival contributions, labor drafts) and validation requirements (coronation, sed festival, oracular consultation). The pharaoh pays in resources and constrained agency; the priesthood receives surplus and interpretive control.
% ABSENT_VOICES: Folk practitioners and heterodox theologians are excluded from formal interpretive authority; their pragmatic syncretism is marginalized as 'superstition' or 'popular religion' by the priestly orthodoxy. The Atenist reformists were not merely absent but actively erased — their voices survive only in the destruction layers of Amarna and the restoration inscriptions that followed.
% DISAPPEARANCE_RATIONALE: If the Amun priestly interpretation vanished overnight, pharaonic legitimacy would lose its primary cosmological anchor (the king as Amun's son/maintainer of maat), temple economies would lose their divine mandate for resource claims, and regional cult centers would lose the coordinating framework that integrates local deities into a valley-wide cosmology. The political economy of the New Kingdom would reorganize — as it did, partially, in the Third Intermediate Period when the High Priests of Amun became de facto rulers.
% FOUNDING_PROBLEM: How to legitimize rule across a culturally diverse Nile Valley (Upper vs. Lower Egypt, Nubian frontier, Libyan hinterlands) while managing massive temple economies that control land, labor, and grain — after the Second Intermediate Period demonstrated the fragility of kingship without divine sanction.
% FOUNDING_PROBLEM_CORROBORATION: The priesthood attests the problem is live: maat requires constant priestly maintenance (temple inscriptions, coronation texts). Historians note the founding problem (state formation after fragmentation) was solved by the early New Kingdom — the arrangement persists as extraction (Kemp, 'Ancient Egypt: Anatomy of a Civilization'; Assmann, 'The Mind of Egypt'; Bleiberg, 'The Economy of Ancient Egypt'). Deir el-Medina ostraca and Hekanakhte letters provide folk-level corroboration of temple demands.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.55) reflects the temple economy's share of surplus — substantial but not totalizing; the pharaoh retains military and administrative control. Suppression (0.45) is moderate: the Atenist episode shows the priesthood can mobilize hard suppression when its cosmological monopoly is threatened, but folk practice is tolerated at the margins. Theater ratio (0.4) captures the growing gap between ritual performance (increasingly elaborate festivals, processions, oracle consultations) and the coordination function (legitimacy, resource management) — by the late Ramesside period, the priesthood's political power (High Priests of Amun effectively ruling Upper Egypt) suggests ritual has become partly performative maintenance of extraction. Accessibility collapse (0.5) and resistance (0.4) reflect that alternatives exist (Atenism, folk practice, Libyan dynastic models) but face high barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood's seat, the arrangement is a rope: genuine coordination of legitimacy, cosmology, and resource management across a diverse valley. From the pharaoh's seat, it is a tangled_rope: coordination is real but extraction is asymmetric and enforcement constrains royal agency. From the folk practitioner's seat, it approaches a snare: the cosmological narrative extracts labor and surplus while denying them interpretive standing. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) states the author's structural judgment, not a reconciliation of perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and temple economies are structural beneficiaries (d ~ 0.15–0.25): they collect the surplus, control the interpretive apparatus, and set the validation terms. The pharaonic authority is a constrained payer (d ~ 0.7): despite nominal supreme power, the king must secure priestly legitimation for each reign, fund temple endowments, and cannot easily exit the arrangement (identity_locked via royal ka theology). Regional cult centers are beneficiaries with constrained exit (d ~ 0.3): they gain resources and autonomy within the Amun framework but depend on the central priesthood for cosmological coherence. Folk practitioners are excluded (d ~ 0.9): they bear the cost of temple taxation and labor drafts without interpretive voice. The analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimizing rule across a culturally diverse Nile Valley while managing temple economies — was live in the early New Kingdom (state formation after Second Intermediate Period). By the late Ramesside period, the problem has shifted: the temple economy has become a competing power center (High Priests of Amun wield de facto sovereignty in Upper Egypt), and the legitimizing function serves priestly interests as much as royal ones. The founding_problem_status is 'contested' because the priesthood claims the cosmological order (maat) requires their mediation (live), while historical evidence shows the arrangement persists after its state-formation function is solved (dead). Corroboration comes from outside the beneficiary set: Deir el-Medina workmen's records show folk experience of temple demands; Hekanakhte letters reveal household-level resource pressure; modern Egyptology (Kemp, Assmann, Bleiberg) provides analytical corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_construction,
    'Is the Amun-Ra priestly interpretation a genuine coordination necessity for Nile Valley polity, or a constructed extraction mechanism that presents itself as cosmological law?',
    'Counterfactual comparison: did polities without centralized priestly interpretation (e.g., early Middle Kingdom, Libyan period) fail at coordination or merely redistribute extraction? Archaeological evidence on state capacity vs. temple wealth across periods.',
    'If genuine coordination necessity, the constraint is a rope with extractive drift; if constructed extraction, it is a snare with coordination cover. Determines whether the ''distributed interpretive authority'' claim is functional or rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction, conceptual, 'Natural-law vs. constructed-status ambiguity for the priestly interpretive monopoly.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of alternative legitimacies (Atenist, folk syncretistic) structural (temple monopoly on oracle, festival, text) or internalized (belief in maat as cosmic order requiring priestly mediation)?',
    'Post-suppression trajectory: after Akhenaten''s Atenist experiment collapsed, did folk practice revert spontaneously (internalized) or require priestly re-imposition (structural)? Deir el-Medina evidence on personal piety vs. official cult.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists in agents after formal enforcement lapses. If structural, suppression tracks institutional capacity directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious legitimacy.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the Amun polytheistic reading''s structural boundary lie relative to the folk syncretistic reading — are they distinct constraints or a single constraint with variable enforcement?',
    'Compare ε values: if folk syncretistic reading has substantially lower extractiveness and different beneficiary structure, they are separate constraints (ε-invariance). If metrics are continuous, single constraint with regional variance.',
    'If separate constraints, the ''accommodates regional variation'' claim is a coordination function of THIS reading; if single constraint, regional variation is enforcement inconsistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Constraint identity boundary between priestly orthodoxy and folk practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t60, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t120, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t180, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 180, 0.38).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t240, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 240, 0.4).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_tr_t300, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 300, 0.4).

% Extraction over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t60, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t120, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t180, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 180, 0.52).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t240, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 240, 0.55).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_be_t300, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 300, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t60, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t120, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t180, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 180, 0.45).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t240, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 240, 0.4).
narrative_ontology:measurement(divine_legitimacy_substrate__amun_polytheistic_reading_su_t300, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 300, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economy_extraction).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_ritual).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, oracle_authority).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the divine_legitimacy_substrate kernel family. The atenist_monotheistic_reading forecloses this reading's core premise (Amun-Ra supremacy vs. Aten exclusivity). The folk_syncretistic_reading coexists with this reading — both operated simultaneously in New Kingdom Egypt, with the priestly reading claiming authority over the folk reading's domain. All three readings share the kernel 'divine legitimacy flows through an interpretive structure' but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
