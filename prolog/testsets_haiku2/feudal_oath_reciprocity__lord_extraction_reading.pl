% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Maximal Extraction (Lord Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This is the lord's reading of the feudal oath: the oath grants the lord
 *   maximal extraction rights, limited only by the vassal's capacity to
 *   deliver service and goods without immediate rebellion. The vassal is
 *   bound by sacred oath to obey, and the oath text is deliberately ambiguous
 *   on extraction limits—the lord interprets what the vassal owes, and the
 *   vassal's remedy for excess is rebellion (an act that breaks the oath and
 *   forfeits everything). This reading frames the oath as a pure extraction
 *   mechanism licensed by legal and sacramental authority, not as a bounded
 *   reciprocal contract.
 *
 * KEY AGENTS:
 *   - territorial_lord: Sets and interprets oath obligations; extracts up to rebellion threshold
 *   - vassal_class: Bound by identity-locked oath; extraction target; compliance is rational equilibrium
 *   - serf_population: Bear cascading extraction indirectly; excluded from oath negotiations
 *   - church_hierarchy: Administers oath legitimacy; could limit extraction but typically aligns with lords
 *   - legal_chronicler_class: Record the gap between oath text and escalating practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.78).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.82).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Maximal Extraction (Lord Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'd66c8006-c4f1-4c2c-b23f-206e9995122a').
narrative_ontology:cs_kernel_codification('d66c8006-c4f1-4c2c-b23f-206e9995122a', fixed_text).
narrative_ontology:cs_authority_grounding('d66c8006-c4f1-4c2c-b23f-206e9995122a', extraction).
narrative_ontology:cs_interpretation_layer_present('d66c8006-c4f1-4c2c-b23f-206e9995122a').
narrative_ontology:cs_reading_relation('d66c8006-c4f1-4c2c-b23f-206e9995122a', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d66c8006-c4f1-4c2c-b23f-206e9995122a', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('d66c8006-c4f1-4c2c-b23f-206e9995122a', foundational, oath_grants_maximal_extraction_authority).
narrative_ontology:cs_axiom_status(oath_grants_maximal_extraction_authority, holdable).
narrative_ontology:cs_axiom_grounding('d66c8006-c4f1-4c2c-b23f-206e9995122a', oath_grants_maximal_extraction_authority, deontological).
narrative_ontology:cs_axiom('d66c8006-c4f1-4c2c-b23f-206e9995122a', foundational, vassal_capacity_is_sole_extraction_limit).
narrative_ontology:cs_axiom_status(vassal_capacity_is_sole_extraction_limit, holdable).
narrative_ontology:cs_axiom_grounding('d66c8006-c4f1-4c2c-b23f-206e9995122a', vassal_capacity_is_sole_extraction_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('d66c8006-c4f1-4c2c-b23f-206e9995122a', lord_interpretive_supremacy).
narrative_ontology:cs_drift_state('d66c8006-c4f1-4c2c-b23f-206e9995122a', late_twelfth_century_charter_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d66c8006-c4f1-4c2c-b23f-206e9995122a', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_class).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, serf_population_dependent_on_vassal_lands).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of vassalage through oath administration and interprets the oath's scope in real-time disputes. Determines what services, goods, and forbearances the vassal owes; adjusts demands to extract up to the threshold where rebellion risk becomes acute. Maintains armed force to enforce oath compliance and punish breach. The oath's text is deliberately ambiguous on extraction limits, allowing the lord to claim ever-expanding obligations as long as the vassal remains capable of compliance.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord, agenda_setter,
    powerful, generational, arbitrage, regional).

% Bound by oath to the lord for protection, land grants, and legitimacy. Must render military service, labor (corvée), goods tributes, and whatever else the lord demands within the vassal's capacity to deliver. Cannot exit without forfeiting land, rank, and the oath-sanctioned social position on which their authority over dependents rests. Rebellion is theoretically possible but catastrophically costly; compliance is the rational equilibrium even as demands creep upward. The oath binds the vassal's identity as a noble—exit means ceasing to be a vassal at all, socially and legally.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_class, payer,
    moderate, biographical, identity_locked, regional).

% Bear extraction indirectly through the vassal, who must meet the lord's demands by extracting intensively from their own dependent population. As the lord's demands rise, the vassal intensifies extraction from serfs on their lands—longer corvée days, larger tribute shares, harsher enforcement. Serfs are explicitly excluded from oath negotiations and have no standing to object. They bear the squeeze without recourse.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, serf_population_dependent_on_vassal_lands, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, serf_population_dependent_on_vassal_lands, excluded).

% Are structurally barred from offering vassals better terms without explicitly repudiating the oath's authority structure itself. Oaths are read as absolute commitments; a competing lord who offered a vassal release from service would be seen as oath-breaking and would lose reputation and military alliances. The oath system locks in extraction across the entire regional power structure.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, competing_lords, excluded,
    powerful, generational, constrained, regional).

% Administers the sacramental oath-taking and provides moral legitimacy to the vassalage system. Collects dues from both lords and vassals. Takes testimony in oath disputes. Could intervene to limit extraction via theological argumentation (Christian charity, sacramental duty), but typically benefits from the hierarchical system and rarely challenges lords' interpretation of vassal obligations.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, church_hierarchy, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, church_hierarchy, beneficiary).

% Clerks, scribes, and jurists who record oaths and their interpretation over time. They observe the gap between oath text (often vague or silent on limits) and practice (escalating extraction bounded only by rebellion risk). Their written records document whether extraction is presented as coordinated reciprocity or maximal taking.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, legal_chronicler_class, observer,
    moderate, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, territorial_lord).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The oath establishes a formal hierarchy binding lord to provide military protection and land grants, vassal to render service and loyalty in exchange. The stated coordination problem: in a fragmented feudal landscape, personal oath-loyalty creates a durable military alliance and stable land tenure where no other authority enforces contracts.
% TRANSFER_FUNCTION: Moves labor (military service, corvée), goods (tributes, rents), and forbearance (loss of autonomy, submission to judgment) from vassal to lord. The transfer is open-ended: the oath binds the vassal to serve the lord's will, and the lord unilaterally interprets what that will demands. As the lord's power grows or threats shift, the obligations ratchet upward.
% ABSENT_VOICES: Serfs dependent on vassal lands have no seat at oath-taking and no standing to object to the cascading extraction the oath enables. Competing lords who might offer better terms are locked out by the oath's sacramental finality. Subordinate clergy who might appeal to charity and sacramental duty are silenced by the church hierarchy's alignment with the lord's power.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished overnight, the lord would lose the mechanism that binds vassals to service without paying market wages. Vassals would demand written, bounded contracts or would seek overlords elsewhere. The serf population would face immediate pressure to renegotiate or flee. The entire hierarchy of extraction would need to be rearticulated through explicit purchase, negotiation, or open coercion (rather than oath-sanctioned submission).
% FOUNDING_PROBLEM: Political fragmentation: in the post-Roman early medieval landscape, large-scale states cannot enforce law. Small landholders need military protection and legitimacy; lords need loyal fighting forces. Personal oath-loyalty (fidelitas) is the mechanism that bonds them without a central authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of feudalism (Bloch, Duby, and their successors writing from outside the benefiting parties) attest the founding problem was real in the 9th–10th centuries. By the 12th century, however, competing analyses dispute whether it persists: some chronicle evidence and lord's letters claim it is still necessary for stability; vassal revolts and charter demands (outside the benefiting parties) assert the problem is solved enough that the oath now persists as naked extraction. Church chroniclers whose interests are aligned with the hierarchy offer limited independent corroboration; those chroniclers who record vassal grievances provide the external attestation.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint permits the lord to extract all vassal surplus up to the rebellion threshold, and the oath text does not specify what counts as breach. Suppression is higher (0.82) because oath-breaking is sacramentally and legally fatal—exit is identity death, not merely economic loss. Theater ratio is moderate (0.41) because the oath's legitimating narrative (protection, stability, order) is partially real but increasingly serves as cover for escalating extraction: as the interval advances, more of the lord's enforcement effort is devoted to preventing oath-renegotiation (suppressing charter demands, preventing vassals from shopping for better lords) than to the original protection function. Measurement series on shared time grid track three decades of intensifying extraction and suppression as lords consolidate power. Accessibility collapse is substantial (0.71) because once a vassal has sworn, the alternatives are functionally unavailable—breaking oath means social death and disinheritance—yet this collapse is partly internalized (the vassal internalizes the oath as identity-binding) and partly structural (the sacramental and legal system punishes breach).
 *
 * PERSPECTIVAL GAP:
 *   From the lord's seat, the oath is a tool for binding loyal fighting forces without paying market wages—a coordination solution that persists because it works better than alternatives. From the vassal's seat, the oath is a trap: the initial protection benefit is real, but the identity-lock prevents exit even as demands ratchet upward. From the serf's seat, it is irrelevant except that the vassal's intensified extraction of them is the mechanism by which the vassal survives the lord's escalating demands. The engine should compute a snare-type classification from the vassal's seat (high d, high ε, identity-locked exit) and possibly a rope-type from the lord's seat (low d, extraction reframed as legitimate obligation maintenance). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The territorial lord is the structural beneficiary: collects the extraction, sets the terms, controls enforcement. Directionality near 0.0 (beneficiary end). The vassal class bears the costs: pays labor and goods, constrained by identity-lock (forfeiture of noble rank, lands, legitimacy), so directionality near 1.0 (target end). The serf population is a victim of cascading extraction but not a direct party to the oath; they are included in the victims list because the constraint's operation systematically intensifies their exploitation. Church hierarchy is a secondary beneficiary (collects dues, maintains authority) but not the primary extractor; treating them as observer rather than beneficiary reflects their intermediary role. Competing lords are excluded—they would benefit from vassals breaking oaths, but the oath system's sacramental and legal finality prevents them from openly offering alternatives without appearing oath-breaking themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for personal loyalty to bind fragmented military forces) is real in the 9th–10th centuries and arguably persists where no higher authority can enforce contracts. However, by the 12th–13th centuries, chronic vassal unrest and charter demands provide external evidence (outside the benefiting parties) that the problem is substantially solved—stable inheritance practices, established custom, and negotiated charters could replace the oath-as-maximal-extraction reading. The constraint persists as mandatrophic: the oath's founding function has atrophied (charter-bound obligations now set the real limits), but the extraction apparatus remains in place, supported by oaths that are claimed as immutable by the same parties who benefit from extracting under their cover. This is the diagnostic for mandatrophy: the founding problem is dead or contested, yet the constraint persists and even intensifies (theater_ratio rises as enforcement effort shifts from protection to oath-enforcement against charter-seekers). The classification prevents reading the oath as coordination when the sacramental and legal machinery has become primarily a tool for preventing renegotiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_text_ambiguity_vs_practice,
    'Is the extraction unbounded because the oath text is structurally ambiguous (leaving interpretation to the lord), or because the lord has power to reinterpret unambiguous text over time?',
    'Textual analysis of surviving oath documents (charters, chronicles, formularies) across the interval: does the text explicitly reserve interpretation to the lord, or does it claim to set bounds? Comparison of oath language with lord''s behavioral demands over time.',
    'If text is genuinely ambiguous, the snare classification holds—the oath structure itself licenses extraction. If text claims bounds but the lord reinterprets, the constraint is a reading of the text (the lord''s reading wins through power), and an omega on interpretive authority becomes central. If text is progressively overwritten (early texts are bounded, later ones become open-ended), it documents mandatrophy: the founding contract is abandoned in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oath_text_ambiguity_vs_practice, empirical, 'Whether the oath''s extractive scope is textually licensed or achieved through power-based reinterpretation.').

omega_variable(
    vassal_identity_lock_mechanism,
    'Is the vassal''s identity-locked exit status a structural feature of the oath (breaking oath = social and legal death) or a consequence of the vassal''s investment in rank and land (the lord controls succession, so exit means disinheritance)?',
    'Historical evidence of vassals who violated oaths and the consequences they faced: was social death automatic, or did it depend on the lord''s will? Cases where vassals negotiated release or renegotiation.',
    'If identity-lock is structural (the oath binds the self, exit is existential death), suppression is higher and the constraint is more snare-like. If identity-lock is property-dependent (the lord controls the vassal''s material base, exit means loss of it but not social erasure), the exit option shifts toward constrained from identity_locked, and the classification might shift toward tangled_rope (pure extraction requires legal exit closure, not just economic pressure). The measured suppression value depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vassal_identity_lock_mechanism, empirical, 'Whether the vassal''s exit closure is existential (oath-breaking = identity death) or economic (lord-controlled property confiscation).').

omega_variable(
    rebellion_threshold_measurability,
    'Is the rebellion threshold a determinate constraint on extraction (the lord can estimate how far they can push before revolt becomes rational), or is it indeterminate (rebellion is rare and unpredictable, so the lord operates without a clear limit)?',
    'Historical record of vassal revolts: do they cluster at predictable extraction levels, or are they scattered and context-dependent? Do lord''s charters and demands show evidence of calibration to avoid triggering revolt, or do they escalate without apparent concern for rebellion risk?',
    'If the threshold is determinate, extraction is bounded by an observable limit (the snare is bounded, not unlimited). If indeterminate, extraction is functionally unlimited—the lord extracts until rebellion happens, and then ratchets back temporarily. The measured extractiveness value (0.78) assumes the lord operates near a determinate threshold; if the threshold is indeterminate, the true extractiveness might be higher (closer to 0.9) because the lord is not restraining extraction in advance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rebellion_threshold_measurability, empirical, 'Whether the rebellion threshold functions as a determinate limit on extraction or is too unpredictable to guide lord behavior.').

omega_variable(
    reading_contest_framing,
    'Does the medieval record itself frame the oath-extraction dispute as a reading contest (different interpretations of the same kernel), or does it present competing systems (oath-based vs. charter-based) as alternatives?',
    'Textual and chronicle evidence: do medieval parties speak of the oath as having a contested meaning (the vassal_coordination_reading and lord_extraction_reading as interpretive options), or do they present the oath and written charters as incompatible frameworks that must be chosen between?',
    'If it is a reading contest, the kernel and reading framework is appropriate. If it is a framework choice, the constraint family should be reorganized around the deeper dispute (oath-system vs. charter-system) rather than readings of a single kernel. This affects how the sibling constraints are classified and linked. The classification itself (snare vs. tangled_rope) does not change, but the genealogy does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_framing, conceptual, 'Whether feudal oath-reciprocity is a single contested kernel with multiple readings or two incompatible institutional frameworks.').

omega_variable(
    serf_extraction_cascade_causality,
    'Is the intensified extraction of serfs a direct, necessary consequence of the lord''s extraction from vassals (the vassal intensifies serf extraction to meet lord demands), or a separate intensification driven by other factors (population pressure, technological change, falling real yields)?',
    'Time-series analysis of serf extraction intensity and vassal extraction intensity across the interval: do they move in tandem, or do they diverge? Narrative evidence from chroniclers about whether lords explicitly command vassals to intensify serf extraction to meet rising lord demands.',
    'If causally linked, the serf population is a true victim of the feudal oath constraint (the constraint''s operation cascades to them). If independent, serf intensification is a separate constraint and the victims list should exclude serfs (or note them as indirectly affected only). The extraction measure (0.78) assumes causal linkage; if serf extraction is independent, the feudal oath''s measured extractiveness might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(serf_extraction_cascade_causality, empirical, 'Whether intensified serf extraction is caused by escalating lord demands on vassals or driven by independent factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t5, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(feud_tr_t5, observed).
narrative_ontology:measurement(feud_tr_t10, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(feud_tr_t10, observed).
narrative_ontology:measurement(feud_tr_t15, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(feud_tr_t15, observed).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(feud_tr_t20, observed).
narrative_ontology:measurement(feud_tr_t25, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(feud_tr_t25, observed).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(feud_tr_t30, observed).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(feud_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t5, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(feud_be_t5, observed).
narrative_ontology:measurement(feud_be_t10, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(feud_be_t10, observed).
narrative_ontology:measurement(feud_be_t15, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(feud_be_t15, observed).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(feud_be_t20, observed).
narrative_ontology:measurement(feud_be_t25, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(feud_be_t25, observed).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(feud_be_t30, observed).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(feud_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t5, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(feud_su_t5, observed).
narrative_ontology:measurement(feud_su_t10, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(feud_su_t10, observed).
narrative_ontology:measurement(feud_su_t15, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(feud_su_t15, observed).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(feud_su_t20, observed).
narrative_ontology:measurement(feud_su_t25, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(feud_su_t25, observed).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(feud_su_t30, observed).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(feud_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_charter_formalization).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, serf_extraction_intensification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The vassal_coordination_reading frames the oath as bounded reciprocal contract; the ecclesiastical_mediation_reading frames it as charity-limited. This reading (lord_extraction_reading) frames the oath as authorizing maximal extraction. All three readings share the same kernel—the oath itself—but author different ε values (high for this reading, lower for the coordinate readings) because they assess the standing arrangement under contest (the oath as lords practice it) from different normative and institutional standpoints. See the cs_structure and commentary.kernel_context for the reading relations and axiom distinctions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
