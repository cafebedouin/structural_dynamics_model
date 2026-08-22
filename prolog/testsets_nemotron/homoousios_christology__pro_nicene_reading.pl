% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Christ is Homoousios with the Father (Pro-Nicene Reading)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   The pro-Nicene reading of homoousios (consubstantial) establishes Christ
 *   as identical in divine substance (ousia) with the Father. Adopted at
 *   Nicaea (325) and reinforced at Constantinople (381), this formula becomes
 *   the doctrinal gate for legitimate episcopal office and imperial-church
 *   alignment. The constraint operates as a tangled rope: it coordinates a
 *   trans-Mediterranean church's teaching and communion (genuine coordination
 *   function) while extracting episcopal office, property, and status from
 *   dissenting groups through anathema and imperial enforcement (asymmetric
 *   extraction). The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as mountain (divine revelation, immutable truth) while authored
 *   metrics describe enforced, extractive operation — the engine measures
 *   that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.82).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Christ is Homoousios with the Father (Pro-Nicene Reading)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053').
narrative_ontology:cs_kernel_codification('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', fixed_text).
narrative_ontology:cs_authority_grounding('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', lineage).
narrative_ontology:cs_interpretation_layer_present('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053').
narrative_ontology:cs_reading_relation('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', foundational, son_identical_substance_father).
narrative_ontology:cs_axiom_status(son_identical_substance_father, holdable).
narrative_ontology:cs_axiom_grounding('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', son_identical_substance_father, deontological).
narrative_ontology:cs_axiom('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', foundational, ousia_language_authoritative_for_christology).
narrative_ontology:cs_axiom_status(ousia_language_authoritative_for_christology, holdable).
narrative_ontology:cs_axiom_grounding('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', ousia_language_authoritative_for_christology, conventional).
narrative_ontology:cs_reference_frame('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', apostolic_trinitarian_tradition).
narrative_ontology:cs_drift_state('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', constantinople_381, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('ef0b0d3b-ad2f-4dc8-9ec4-50b7b6b8b053', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_orthodox_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_authority).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_bishops_and_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_laity).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, christological_orthodoxy).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_coherence).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, apostolic_succession_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls episcopal appointments, conciliar agendas, and the apparatus of anathema. Uses homoousios as the doctrinal gate for legitimate office and imperial patronage. Collects institutional authority, revenue streams from church properties, and alignment with imperial power.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Gain secure ecclesiastical office, imperial protection, and control over diocesan resources by confessing homoousios. Their careers and communities depend on maintaining the Nicene settlement. Dissent within the hierarchy is policed through synodical discipline.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_orthodox_bishops, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, nicene_orthodox_bishops, agenda_setter).

% Receives a unified imperial church that legitimizes the Christian emperor's rule and provides a trans-regional administrative framework. The homoousios formula becomes the theological cement for political unity. Emperors enforce it through exile, property confiscation, and military suppression of dissenting groups.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Lose episcopal sees, face exile, property seizure, and legal disabilities. Their congregations are driven underground or dispersed. Theological identity is fused with their christology — abandoning homoiousios/created-substance language means surrendering the ecclesial communities they shepherd. Exit requires renouncing their ordination lineage and doctrinal inheritance.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_bishops_and_communities, payer,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, arian_bishops_and_communities, excluded).

% Attempt the homoiousios compromise but are squeezed from both sides: anathematized by Nicenes as crypto-Arians, pressured by Arians as insufficiently radical. Some flip to full Nicene confession to retain office; others join Arian exile networks. Their institutional position is unstable — neither fully excluded nor fully admitted.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_bishops, payer,
    moderate, biographical, constrained, continental).

% Subject to episcopal discipline, denied communion, or forced into schismatic communities. In Arian-dominated regions (e.g., Gothic kingdoms), Nicene laity suffer parallel disabilities. Geographic and social mobility is limited; religious identity is bound to household and village networks. Exit means exile or conversion under pressure.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_laity, payer,
    powerless, biographical, trapped, local).

% Produce the conceptual vocabulary (ousia, hypostasis, prosopon) that makes homoousios intelligible and defensible. Their texts become the authoritative interpretive tradition. They do not directly collect rents but their intellectual authority shapes the constraint's persistence.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, patristic_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single doctrinal standard that unifies the imperial church's teaching, liturgy, and episcopal communion across linguistic and cultural boundaries — solving the coordination problem of christological consensus in a trans-Mediterranean institution.
% TRANSFER_FUNCTION: Moves episcopal office, imperial patronage, church property revenues, and legitimate sacramental authority from dissenting groups to confessing Nicene hierarchs. Anathema operates as the transfer mechanism: exclusion from the communion of the Great Church transfers resources and status to those who remain.
% ABSENT_VOICES: Jewish-Christian communities, non-Chalcedonian miaphysite precursors, and barbarian Gothic Christians living under Arian kings — all would object to the imperial enforcement of a Greek philosophical formula as the boundary of Christian identity, but were structurally excluded from the conciliar process.
% DISAPPEARANCE_RATIONALE: If homoousios and its enforcement vanished overnight, the imperial church would fracture into competing christological communions (Arian, semi-Arian, Nicene, Apollinarian, etc.), episcopal succession lines would rupture, imperial legitimacy would lose its theological anchor, and the trans-Mediterranean ecclesiastical administration would dissolve into regional churches.
% FOUNDING_PROBLEM: The Arian controversy threatened to split the newly legalized church along christological lines, undermining Constantine's vision of a unified imperial church as a pillar of political order. Homoousios was adopted at Nicaea (325) as the formula that could exclude subordinationism while preserving monotheism.
% FOUNDING_PROBLEM_CORROBORATION: Nicene apologists (Athanasius, the Cappadocians) attest the problem was live and the formula necessary. Arian and semi-Arian sources (preserved in fragments and opponent citations) attest the problem was manufactured — that the Father/Son relationship was already settled in scripture and tradition without philosophical ousia-language. Modern patristic scholars (e.g., Ayres, Khaled Anatolios) corroborate from outside the benefiting parties that the 'problem' was as much political as theological: the formula served to consolidate a specific episcopal-imperial alliance.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial: the formula gates access to the entire institutional apparatus of the imperial church — sees, revenues, imperial protection, sacramental legitimacy. Suppression (0.82) is high: persistence depends on active enforcement (exile, property seizure, military action against dissenting communities, synodical discipline). Theater ratio (0.22) is low-moderate: the theological coherence function is real (the Cappadocian synthesis genuinely resolves trinitarian logic), but a growing share of enforcement activity defends institutional boundaries rather than doctrinal truth. Accessibility collapse (0.78) is high: once the formula is accepted as the boundary of orthodoxy, alternative christologies become unintelligible within the system. Resistance (0.45) is moderate: Arian and semi-Arian networks persist for decades with imperial patronage at times, but cannot overcome the Nicene-imperial alignment after Theodosius.
 *
 * PERSPECTIVAL GAP:
 *   From the Nicene bishop's seat, homoousios is the indispensable coordination that makes catholic communion possible — a rope. From the Arian bishop's seat, it is an imposed formula backed by imperial swords that extracts their office and flocks — a snare. The imperial authority sees a pillar of political unity — a rope. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial church hierarchy and imperial authority are structural beneficiaries (d near 0.0–0.2): they collect the rents of office, property, and legitimacy. Nicene bishops are beneficiaries with constrained exit (d ~0.3): they gain but are also bound by the system they administer. Arian bishops are identity-locked targets (d ~0.9): their theological identity is fused with their ecclesial existence — exit means self-annihilation. Semi-Arians are constrained payers (d ~0.7): squeezed between camps, some defect. Dissenting laity are trapped (d ~0.95): no institutional leverage, geographic mobility limited. Patristic theologians are analytical observers (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arian threat to imperial church unity) was live in 325 but by 381 the Arian movement had fragmented and lost imperial patronage. Yet the enforcement apparatus intensified (suppression 0.45→0.82, extraction 0.35→0.68). The constraint did not sunset — it expanded into a permanent doctrinal boundary that now serves to police intra-Nicene disputes (Apollinarianism, Nestorianism, Eutychianism). The original mandate (exclude subordinationism) is dead; the constraint persists as a general gate for christological legitimacy — classic mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the homoousios formula structurally necessary for trinitarian coherence, or is the Cappadocian synthesis (three hypostases in one ousia) a post-hoc rationalization that could have been reached without the exclusionary enforcement?',
    'Counterfactual analysis: compare the theological resources available pre-Nicaea (Origen, Tertullian, Dionysius of Alexandria) with the post-Nicene settlement. If the conceptual vocabulary for distinguishing ousia/hypostasis existed independently, the enforcement was extractive; if the vocabulary emerged only through the controversy, the coordination function is genuine.',
    'If the coordination function is separable from the enforcement, the constraint is a tangled_rope with a thinner coordination core. If inseparable, the extraction is the price of the coordination itself — a thicker rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the theological coordination and the institutional extraction are structurally separable.').

omega_variable(
    imperial_vs_ecclesiastical_agency,
    'Does the enforcement intensity derive from ecclesiastical conviction or imperial political utility? Who is the principal and who the agent in the enforcement apparatus?',
    'Trace the correlation between imperial policy shifts (Constantius II''s Arianism, Julian''s tolerance, Valens''s Arianism, Theodosius''s Nicene enforcement) and conciliar/episcopal actions. If episcopal enforcement tracks imperial preference with short lag, the church hierarchy is the agent; if episcopal conviction drives imperial policy, the hierarchy is the principal.',
    'If imperial-driven, the extraction primarily serves political unity (imperial beneficiary). If church-driven, the extraction primarily serves ecclesiastical authority (hierarchy beneficiary). Changes the beneficiary structure and directionality assignments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_vs_ecclesiastical_agency, empirical, 'Principal-agent structure of the enforcement coalition.').

omega_variable(
    identity_locked_mechanism_arian_bishops,
    'Is the Arian bishops'' identity_locked exit structural (institutional barriers: no alternative sees, property tied to office) or internalized (theological conviction that their ordination lineage and communities cannot exist outside their christology)?',
    'Track post-exile trajectories: did exiled Arian bishops reconstitute communities in Gothic kingdoms (structural exit possible) or dissolve (internalized lock)? Compare with semi-Arian flip rates — those who switched to Nicene confession under pressure.',
    'If structural, the lock is imposed by the constraint''s enforcement design. If internalized, the constraint has colonized the agents'' self-conception — the suppression persists after exit. Either way, directionality remains high; the mechanism informs whether the constraint is a snare (structural) or has internalized extraction components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_arian_bishops, empirical, 'Structural vs. internalized identity lock for Arian episcopal targets.').

omega_variable(
    kernel_reading_framing,
    'Does the pro-Nicene reading''s claim to be the sole legitimate instantiation of the homoousios_christology kernel reflect a genuine logical foreclosure of siblings, or an institutional capture of the kernel''s interpretive authority?',
    'Analyze whether the semi-Arian homoiousios position is logically incompatible with the kernel''s core commitment (Christ''s full divinity) or merely linguistically distinct. If logically compatible, the pro-Nicene ''forecloses'' claim is institutional capture, not logical necessity.',
    'If logical foreclosure, the kernel has a single stable reading. If institutional capture, the kernel is genuinely contested and the pro-Nicene reading is one enforcement-backed instantiation among others — supporting the tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the pro-Nicene reading''s foreclosure of siblings is logical or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.08).
narrative_ontology:measurement(homo_tr_t337, homoousios_christology__pro_nicene_reading, theater_ratio, 337, 0.12).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__pro_nicene_reading, theater_ratio, 350, 0.16).
narrative_ontology:measurement(homo_tr_t359, homoousios_christology__pro_nicene_reading, theater_ratio, 359, 0.19).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__pro_nicene_reading, theater_ratio, 370, 0.21).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.22).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(homo_be_t337, homoousios_christology__pro_nicene_reading, base_extractiveness, 337, 0.48).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__pro_nicene_reading, base_extractiveness, 350, 0.55).
narrative_ontology:measurement(homo_be_t359, homoousios_christology__pro_nicene_reading, base_extractiveness, 359, 0.62).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__pro_nicene_reading, base_extractiveness, 370, 0.68).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement(homo_su_t337, homoousios_christology__pro_nicene_reading, suppression_requirement, 337, 0.62).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__pro_nicene_reading, suppression_requirement, 350, 0.71).
narrative_ontology:measurement(homo_su_t359, homoousios_christology__pro_nicene_reading, suppression_requirement, 359, 0.78).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__pro_nicene_reading, suppression_requirement, 370, 0.81).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, chalcedonian_christology).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, filioque_controversy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, iconoclast_controversy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, papal_primacy_claims).

% DUAL FORMULATION NOTE:
% Part of the homoousios_christology constraint family with arian_reading and semi_arian_reading. This reading (pro_nicene) carries the highest enforcement ε and institutional extraction; arian_reading carries lower enforcement but persistent alternative communities; semi_arian_reading is the unstable compromise. The three stories share the kernel_id homoousios_christology but have distinct ε, stakeholder structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, institutional, 0.1).
constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, organized, 0.85).
constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, moderate, 0.7).
constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
