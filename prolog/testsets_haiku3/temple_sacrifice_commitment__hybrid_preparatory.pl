% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment in Preparatory Suspension (Hybrid Reading)
 *   domain: religious_law/commitment_system/halakhic_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple (70 CE), the Jewish legal
 *   tradition faced an unprecedented question: how to maintain the covenant
 *   commitment to temple sacrifice when the material conditions for
 *   performance no longer exist and will not exist until messianic
 *   restoration. The hybrid_preparatory reading positions the rabbinic
 *   transformation of sacrifice law into textual study as a suspended state —
 *   neither full occupation of the commandment (which requires temple and
 *   animals) nor mere archival preservation of a dead practice. Instead,
 *   study of sacrifice law is cast as preparatory exercise, maintaining
 *   readiness and interpretive sophistication for the moment when material
 *   restoration becomes possible. This reading holds the commitment in active
 *   suspension, requiring ongoing cognitive and communal resource investment
 *   for an indefinitely deferred future condition. The constraint is
 *   substantially extractive (extraction grows from 0.48 to 0.62 over the
 *   interval) because it demands that communities fund the study of law that
 *   cannot currently be performed, extracting resources for an uncertain
 *   future benefit while deferring participation rights in the present.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: Institutional seat that defines and maintains the interpretation of sacrifice law, controls the suspension frame, and benefits from the legitimacy and cognitive resources oriented toward the tradition.
 *   - lay_community_members: Bear the deferred participation (cannot perform sacrifice themselves) and the indirect costs of maintaining scholarly infrastructure.
 *   - scholars_and_community_study_leaders: Whose intellectual labor occupies the constraint, bearing opportunity costs and identity-fusion with the preparatory commitment.
 *   - messianic_anticipants: The implicit future beneficiaries (at restoration, they would perform sacrifice again) — their existence is asserted but their arrival is indefinitely postponed.
 *   - competing_Jewish_movements: Movements that reject the suspension frame (e.g., study_as_exercise or symbolic_transformation readings) or abandon the commitment entirely — they would dispute the hybrid_preparatory framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.62).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.41).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment in Preparatory Suspension (Hybrid Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/commitment_system/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '72cbdb86-7a85-486f-b77e-aad6436acf29').
narrative_ontology:cs_kernel_codification('72cbdb86-7a85-486f-b77e-aad6436acf29', fixed_text).
narrative_ontology:cs_authority_grounding('72cbdb86-7a85-486f-b77e-aad6436acf29', lineage).
narrative_ontology:cs_interpretation_layer_present('72cbdb86-7a85-486f-b77e-aad6436acf29').
narrative_ontology:cs_reading_relation('72cbdb86-7a85-486f-b77e-aad6436acf29', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('72cbdb86-7a85-486f-b77e-aad6436acf29', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('72cbdb86-7a85-486f-b77e-aad6436acf29', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('72cbdb86-7a85-486f-b77e-aad6436acf29', foundational, suspension_preparatory_not_archival).
narrative_ontology:cs_axiom_status(suspension_preparatory_not_archival, holdable).
narrative_ontology:cs_axiom_grounding('72cbdb86-7a85-486f-b77e-aad6436acf29', suspension_preparatory_not_archival, deontological).
narrative_ontology:cs_axiom('72cbdb86-7a85-486f-b77e-aad6436acf29', secondary, restoration_timeline_indefinite_but_binding).
narrative_ontology:cs_axiom_status(restoration_timeline_indefinite_but_binding, holdable).
narrative_ontology:cs_axiom_grounding('72cbdb86-7a85-486f-b77e-aad6436acf29', restoration_timeline_indefinite_but_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('72cbdb86-7a85-486f-b77e-aad6436acf29', temple_destroyed_commitment_suspended).
narrative_ontology:cs_drift_state('72cbdb86-7a85-486f-b77e-aad6436acf29', contemporary_indefinite_restoration_expectation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72cbdb86-7a85-486f-b77e-aad6436acf29', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_resource_contributors).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, lay_practitioners_deferred_participation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, scholars_and_study_leaders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, scholars_and_study_leaders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the interpretation that temple sacrifice law is maintained in suspended preparatory state. Controls the scholarly curriculum, the legal standards for what counts as proper study, and the legitimacy claims around the tradition's continuity. Receives institutional authority, funding flows, and intellectual legitimacy from the community's orientation toward the tradition. Can reframe the commitment (as study_as_exercise or symbolic_transformation readings do) or defend the current frame against competing interpretations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Perform the scholarly work of studying sacrifice law, deriving intellectual status and institutional position from that work. Bear the extraction of their cognitive resources, career time, and opportunity costs. Are identity-fused with the scholarly tradition (exiting would mean abandoning professional identity and community status). Benefit from the interpretive authority's validation of their work as meaningful and preparatory rather than archival.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, scholars_and_study_leaders, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, scholars_and_study_leaders, payer).

% Fund the scholarly infrastructure and study centers where sacrifice law is maintained and taught. Receive the symbolic benefit of participation in the covenant tradition and the diffuse benefit of community cohesion around shared commitment. Bear the extraction of their charitable contributions for a practice they cannot currently perform, with indefinitely deferred promise of future participation if restoration occurs. Their exit options are constrained by community pressure and their own attachment to the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_resource_contributors, payer,
    moderate, generational, constrained, global).

% Participate in the covenant tradition through prayer, study, and identification with the community's commitment, but are denied direct performance of sacrifice law (which requires temple, priesthood, animals). Bear the deferred participation — the constraint withholds from them the primary expression of the commandment. Their participation is indirect and mediated through the scholarly tradition maintained by others. Exit is constrained by identity, family tradition, and community belonging.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, lay_practitioners, payer,
    powerless, biographical, constrained, global).

% Promote alternative readings of the sacrifice commitment (study_as_exercise, symbolic_transformation, or abandonment of the commitment entirely). Are structurally excluded from the institutional authority that enforces the hybrid_preparatory framing, though they maintain their own interpretive communities and scholarship. Would argue that the preparatory frame masks archival function or that the commitment has been legitimately transformed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, competing_jewish_movements, excluded,
    powerful, generational, mobile, global).

% The implicit beneficiaries at messianic restoration who would perform sacrifice again if/when the temple is rebuilt and the conditions for performance return. This is a non-agent entry (theological category, not an actual seat) — they are named for narrative completeness as the ultimate recipients of the preparatory work, but they do not exist in the present and their future existence is contested.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, future_restoration_participants, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__hybrid_preparatory, future_restoration_participants).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the Jewish covenant commitment to temple sacrifice across a period when material performance is impossible, preserving interpretive sophistication and readiness for future restoration. Solves the coordination problem: how to keep the commitment alive when the temple is destroyed, without either abandoning the law or claiming it is already transformed. Creates a shared frame that the tradition continues in suspended state rather than as either dead archiving or current performance.
% TRANSFER_FUNCTION: Transfers intellectual and financial resources from the lay community and charitable contributors to the scholarly infrastructure that maintains and refines the law. Defers participation rights in the primary commandment (performing sacrifice) indefinitely, in exchange for the promise of future restoration and the current benefit of community cohesion around shared commitment. Moves legitimacy and institutional authority toward the rabbinic interpretation that frames suspension as preparation rather than archiving.
% ABSENT_VOICES: Competing movements that advocate for study_as_exercise, symbolic_transformation, or abandonment of the sacrifice commitment are excluded from the institutional authority that enforces this reading. They would argue for reframing the commitment as currently performed (study readings) or legitimately transformed (prayer, ethical action). Their exclusion is what the constraint's enforcement machinery exists to maintain — they would dispute that suspension is the correct frame.
% DISAPPEARANCE_RATIONALE: If the hybrid_preparatory frame disappeared and were replaced by study_as_exercise or symbolic_transformation readings, the commitment would be reinterpreted as currently performed rather than suspended. If the commitment itself disappeared, the tradition would lose a constitutive element and communities would reorganize around alternative readings of covenant continuity. The world would rearrange because the constraint shapes how communities understand their relationship to the destroyed temple and the possibility of restoration — different frames entail different resource allocations, different participation structures, and different institutional authorities.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) created an unprecedented crisis: the primary expression of the covenant (temple sacrifice) became materially impossible, yet the rabbis confronted the question of whether the commandment was suspended, transformed, or dead. The hybrid_preparatory reading addresses this by proposing that the commitment is suspended in preparation for restoration, which allows the tradition to be maintained in active form rather than archived.
% FOUNDING_PROBLEM_CORROBORATION: Early rabbinic sources (Mishnah, Gemara, geonic literature) attest that the founding problem was live and urgent — they spent centuries refining sacrifice law despite its non-performance, explicitly framing the work as preparation for restoration. Modern historians and rabbinic scholars outside the benefiting parties (academic historians, competing movements) dispute whether the founding problem is still live: they argue the expectation of imminent restoration has become indefinite, effectively converting the suspension into either archival preservation or symbolic theater. No external source corroborates the restoration as temporally proximate — the corroboration for the founding problem exists only in early rabbinic texts.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint extracts intellectual and financial resources from the community for a commitment whose fulfillment is indefinitely deferred. The measurement trajectory shows a rise from early-interval low (0.48) to mid-interval peak (0.64 at t=16), then slight decline (0.62 at t=20), reflecting periods of intensified scholastic engagement (Geonic and medieval periods with expanded halakhic literature on sacrifice) followed by normalization. Theater_ratio rises steadily from 0.35 to 0.50 (crossing the 0.50 threshold at t=16), indicating growing performative emphasis as the actual restoration timeline recedes — the theatrical maintenance of readiness becomes more visible relative to any functional preparation. Suppression is low-to-moderate (0.41) because the constraint is not maintained through active coercion but through identity fusion with the tradition and diffuse institutional authority. The constraint requires active enforcement (rabbinic authority must continuously defend the reading against competing interpretations) but suppression does not rely on coercive barriers — exit is theoretically available (communities can adopt a different reading) but is constrained by identity and institutional position (identity_locked for scholars; constrained for lay contributors).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic_interpretive_authority seat experiences this as genuine coordination and preparation (d near 0.3) — they are maintaining a tradition, performing scholarly work they see as meaningful, and receiving institutional legitimacy and resource flows from the community. The lay_community_members and resource-contributors sit at the opposite end (d near 0.8) — they are investing in a practice they cannot currently perform, with indefinite deferral and no guarantee of future fulfillment. Scholars occupy a middle position (d near 0.6) with secondary dual-position: they are beneficiaries of institutional status and scholarly legitimacy (primary role: beneficiary) but also bear the extraction of their own intellectual time and career opportunity costs (secondary role: payer). The computed per-seat types should diverge: from the interpretive authority's position, this may compute as rope (genuine coordination); from the resource-contributor's position, it may compute as tangled_rope or snare (extraction under the guise of preparation).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic interpretive authority benefits structurally from the suspension frame: it legitimates their institutional role as keepers of the interpretive tradition, it focuses community resources (funding, attention, scholarly labor) toward their domain of authority, and it provides indefinite work (refinement and expansion of sacrifice law). Their directionality is near the beneficiary end (d ~ 0.25). Community resource-contributors bear costs (their charitable contributions fund study of non-performable law, their participation in the covenant is deferred, their agency in the present is constrained by a future-oriented commitment) without direct benefit; their directionality is near the target end (d ~ 0.75). Scholars sit between: they benefit from institutional position and intellectual legitimacy but bear the extraction of their cognitive resources and opportunity costs; directionality is near symmetric (d ~ 0.50). The constraint is enforced through rabbinic authority's control of interpretation and through community attachment to the tradition (identity-locking for scholars, institutional inertia for lay participants); suppression is relatively low because exit, while costly, is theoretically available.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real and was historically live: the Temple's destruction created a genuine crisis of commitment — how to maintain the covenant when its primary material expression was impossible. The hybrid_preparatory reading resolves this by positing suspension and preparation, which is a coherent coordinate to the problem. However, the founding_problem_status is now contested-to-dead: the expectation of imminent messianic restoration (live in early rabbinic literature, with generations expected to witness it) has become increasingly indefinite, pushing the constraint toward archival or purely symbolic function. A mandatrophy reading would argue: the founding problem (Temple destruction, need to preserve commitment during temporary exile) has been resolved by institutional adaptation, but the constraint persists due to inertia and institutional capture by rabbinic authority. This is not quite mandatrophy (which requires the founding problem to be provably dead and the constraint maintained only by extraction) — the reading maintains ambiguity: the problem might still be live (restoration might yet come), or it might be dead (restoration might be indefinitely deferred or mythologized). This ambiguity is captured by the omega variables, particularly suspension_vs_archival_boundary. If the archival reading becomes dominant (restoration recognized as indefinite deferral), the constraint would reclassify toward piton (performative maintenance of a tradition whose function has atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_archival_boundary,
    'Is the constraint maintaining genuine preparatory readiness for messianic restoration, or is it functionally equivalent to archival preservation of a defunct practice that will never materialize?',
    'Historical trajectory: if textual refinement and halakhic innovation continue with messianic restoration framed as teleological endpoint, the preparatory reading holds; if innovation ceases and the texts function as closed historical records, the archival reading becomes more plausible.',
    'If archival: the constraint shifts toward piton (performance without function, maintained by institutional inertia); if preparatory: it remains tangled_rope (genuine coordination function with asymmetric resource extraction for uncertain future benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_archival_boundary, conceptual, 'Whether suspension is temporally bounded preparation or unbounded archival equivalence.').

omega_variable(
    study_cognitive_extraction_referent,
    'Is the extractiveness measurement tracking extraction from the community that funds study, or the cognitive load imposed on scholars who perform the study themselves?',
    'Stakeholder analysis: identify whether resource flows (charitable contributions, institutional funding) toward study are asymmetric relative to participation rights, or whether the extraction is primarily the opportunity cost of scholarly time.',
    'If community-resource extraction: the victims are funders; the constraint is snare-adjacent (paying for others'' readiness). If scholar-time extraction: victims are scholars themselves (identity-locked, unable to exit scholarly roles); classification stays tangled_rope but victim identity shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_cognitive_extraction_referent, empirical, 'Whose resources are extracted by the study commitment.').

omega_variable(
    messianic_certainty_and_indefinite_deferral,
    'How much of the constraint''s structural stability depends on genuine expectation of messianic restoration, versus indefinite deferral? If restoration is perpetually ''imminent'' but never arrives, does the constraint become performative theater?',
    'Historical analysis of textual indications of restoration timeline expectations across rabbinic periods; sociological study of how communities actually frame the indefiniteness in practice.',
    'High certainty of timely restoration: extractiveness drops (preparation is narrowly bounded); indefinite deferral with theater framing: theater_ratio rises and the constraint approaches piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_certainty_and_indefinite_deferral, empirical, 'Temporal horizons of restoration expectations and their role in maintaining commitment.').

omega_variable(
    kernel_reading_contest_sibling_ambiguity,
    'Is this reading (hybrid_preparatory) genuinely distinct from the study_as_exercise reading, or are they rhetorically different formulations of the same structural arrangement?',
    'Textual and practice evidence: do authoritative sources and communities distinguish ''suspension awaiting restoration'' from ''study as current instantiation''? Or do they use both frames interchangeably?',
    'If distinct: two separate constraints (ε values differ meaningfully; one is preparatory, the other performative); if interchangeable: one constraint viewed through competing framings (same ε, different narrative); this omega triggers decomposition decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_sibling_ambiguity, conceptual, 'Whether hybrid_preparatory and study_as_exercise are structurally separable constraints or one constraint with two readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.35).
narrative_ontology:measurement(temp_tr_t4, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 4, 0.38).
narrative_ontology:measurement(temp_tr_t8, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 8, 0.42).
narrative_ontology:measurement(temp_tr_t12, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 12, 0.46).
narrative_ontology:measurement(temp_tr_t16, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 16, 0.5).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(temp_be_t4, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(temp_be_t8, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(temp_be_t12, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(temp_be_t16, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(temp_su_t4, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(temp_su_t8, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(temp_su_t12, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(temp_su_t16, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Temple sacrifice commitment kernel decomposes into four structurally distinct constraint stories (ε-invariance principle, OQ-26). The hybrid_preparatory reading frames suspension as active preparation; study_as_exercise frames study as current performance; performance_only frames study as archival preservation; symbolic_transformation frames prayer/study as authorized replacement. Each reading has different ε, different beneficiary/victim structure, different timeline (immediate vs. indefinite deferral), and different type classification. Network links establish that interpreting one reading changes the legitimacy conditions for the others — they compete for institutional authority and interpretive franchise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
