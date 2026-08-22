% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocal Obligation (Vassal Coordination Reading)
 *   domain: medieval_political_economy/institutional_analysis
 *
 * SUMMARY:
 *   Under the vassal_coordination_reading, the feudal oath operates as a
 *   rope: a genuine collective-action problem (binding dispersed landholders
 *   without centralized enforcement) solved by mutual, documented obligation
 *   enforced through the charter text itself. The constraint is CLAIMED as
 *   rope and the metrics support this: moderate extractiveness (0.38)
 *   reflects that the lord does extract rent and service, but the extraction
 *   is bounded by the oath's documented limits; suppression (0.42) remains
 *   moderate because the charter provides both parties with grounds to
 *   challenge breach — neither is fully trapped. Theater is low (0.15)
 *   because the functional compliance burden is substantial: charters must be
 *   maintained, breach claims must be adjudicated, and both parties invest in
 *   the relationship's reputation. The temporal measurements show remarkable
 *   stability across the interval — extractiveness and theater remain nearly
 *   flat, suggesting a mature, equilibrated arrangement. This reading
 *   contrasts sharply with the lord_extraction_reading (which would author
 *   high ε, high suppression, claiming the oath is cover for maximal
 *   extraction) and the ecclesiastical_mediation_reading (which would split
 *   the extraction between secular obligation and moral sanction).
 *
 * KEY AGENTS:
 *   - vassal_class: moderate power, generational time horizon, constrained exit — benefit from bounded obligations and inherit land rights
 *   - lord_class: powerful, generational time horizon, mobile exit — benefit from coordinated service and legitimacy but must honor charter limits
 *   - charter_keepers: institutional power, analytical exit — administer the text and resolve breach disputes
 *   - ecclesiastical_authority: institutional power, analytical exit — witness oaths, confirm sacramental character (observational, not extractive)
 *   - competing_lords: powerful, biographical horizon, constrained exit — excluded from oath; their threat keeps the constraint binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.38).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.42).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocal Obligation (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'b173a504-480f-4caa-8792-db0ecdec8579').
narrative_ontology:cs_kernel_codification('b173a504-480f-4caa-8792-db0ecdec8579', fixed_text).
narrative_ontology:cs_authority_grounding('b173a504-480f-4caa-8792-db0ecdec8579', lineage).
narrative_ontology:cs_interpretation_layer_present('b173a504-480f-4caa-8792-db0ecdec8579').
narrative_ontology:cs_reading_relation('b173a504-480f-4caa-8792-db0ecdec8579', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b173a504-480f-4caa-8792-db0ecdec8579', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('b173a504-480f-4caa-8792-db0ecdec8579', foundational, mutual_reciprocal_enforceability).
narrative_ontology:cs_axiom_status(mutual_reciprocal_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('b173a504-480f-4caa-8792-db0ecdec8579', mutual_reciprocal_enforceability, conventional).
narrative_ontology:cs_axiom('b173a504-480f-4caa-8792-db0ecdec8579', foundational, charter_text_supreme_binding).
narrative_ontology:cs_axiom_status(charter_text_supreme_binding, holdable).
narrative_ontology:cs_axiom_grounding('b173a504-480f-4caa-8792-db0ecdec8579', charter_text_supreme_binding, conventional).
narrative_ontology:cs_reference_frame('b173a504-480f-4caa-8792-db0ecdec8579', feudal_oath_as_documented_compact).
narrative_ontology:cs_drift_state('b173a504-480f-4caa-8792-db0ecdec8579', late_medieval_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b173a504-480f-4caa-8792-db0ecdec8579', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lord_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, lord_class).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, mutual_enforceability_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, charter_precedence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive fixed, bounded protection and judicial support from the lord in exchange for military service and homage rent. The oath limits what the lord can demand — extracting beyond the charter constitutes breach of faith, giving vassals legitimate grounds to withdraw fealty. They invest in the relationship and depend on its stability for land tenure and social standing.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class, payer).

% Receive military service, homage rent, and counsel from vassals; bind dispersed landholders into a coordinated hierarchy. Their obligation to protect vassals and uphold the charter's terms limits unilateral extraction but provides legitimacy and compliance stability in a governance structure that lacks centralized enforcement.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lord_class, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lord_class, payer).

% Maintain written, witnessed oath texts and adjudicate claims of breach when lord or vassal alleges the other violated the compact. Their authority rests on the text's integrity and their reputation for impartial reading. They do not themselves benefit from the arrangement but administer its rules.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_keepers, agenda_setter,
    institutional, generational, analytical, regional).

% Witness oaths, confirm their sacramental character, and may intervene if oath-breaking is alleged on moral grounds. They see the oath as binding both parties by divine sanction; their role in THIS reading is observational — they confirm the mutual enforceability without adding a separate moral extraction layer.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_authority, observer,
    institutional, generational, analytical, regional).

% Are outside the oath relationship but would benefit from vassals' defection if the lord breached the charter; their exclusion from the compact is what the mutual-obligation framework exists to prevent — they represent the alternative exit that the oath's enforceability makes costly.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, competing_lords, excluded,
    powerful, biographical, constrained, regional).

% Are not parties to the oath and receive no direct benefit from its reciprocal structure — their obligations run to the lord by status, not by compact. The oath's coordination function protects them indirectly by stabilizing the lord's governance, but they have no standing to claim breach.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, serfs_unfree_dependents, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds dispersed landholders into a predictable, multi-generational governance hierarchy: the lord coordinates defense, justice, and land allocation; vassals coordinate their military and financial contributions into a single command structure. The charter text makes mutual obligations explicit and verifiable, replacing informal patronage with documented reciprocity.
% TRANSFER_FUNCTION: Moves military service, homage rent (typically 1/3 to 1/2 annual revenue from granted land), and counsel from vassal to lord; in return, the lord provides protection, judicial authority, and inheritance guarantees. The charter fixes these transfers — exceeding them constitutes breach and releases the vassal from fealty.
% ABSENT_VOICES: Serfs and unfree dependents have no standing in the oath; they are excluded from its reciprocal framework and cannot claim protection of its terms. Competing lords are also excluded — they would object that the mutual-obligation doctrine limits their ability to recruit vassals by offering unlimited extraction.
% DISAPPEARANCE_RATIONALE: If feudal oath reciprocity and its charter enforcement vanished, vassals would lose the legal ground to withdraw fealty when lords exceeded the compact's terms; lords would face constant defection risk and would substitute with explicit written contracts or coercive kinship bonds. The organizational form — a multi-generational hierarchy resting on mutual oath — would collapse into either fragmented warbands (smaller-scale coercive units) or nascent bureaucratic states with written law codes.
% FOUNDING_PROBLEM: Early medieval governance required binding dispersed, powerful landholders into a stable hierarchy without centralized enforcement machinery. Kinship alone was insufficient; pure coercion was unstable; the oath provided a sacred compact that both lord and vassal could use to hold the other accountable.
% FOUNDING_PROBLEM_CORROBORATION: Charter texts themselves attest the binding problem: oaths enumerate specific mutual obligations and procedures for breach claims, indicating the founding problem was pressing and recognized. Monastic chroniclers (external to the feudal hierarchy) confirm that oath-breaking and breach litigation were central to political stability across the period.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays moderate (0.38) because the charter text is the constraint's enforcement mechanism — vassals can credibly claim breach if the lord demands beyond the written terms, and charter-keepers must adjudicate those claims. The lord cannot unilaterally raise the rate without renegotiating the oath, which gives vassals negotiating power. Suppression (0.42) is not low (as a pure coordination mechanism would be) because the lord enforces the oath through military power — defecting vassals face retaliation — but it is not high because the charter provides the vassal with a legal exit: honest oath-keeping by the lord ensures continued fealty. Theater (0.15) is low because the coordination and extraction functions are tightly coupled — the oath's enforcement machinery IS the document itself, not performative ritual. The measurements show stability because the feudal system, once established, reaches equilibrium: lords respect charter limits because defection is costly (vassals leave), and vassals remain loyal because the oath guarantees inherited rights and bounded demands. This stability is the signature of a mature rope.
 *
 * PERSPECTIVAL GAP:
 *   From the vassal's seat, the oath is a binding compact that the lord must honor — it is genuinely protective and mutually enforcing. From the lord's seat, the oath is an obligation that constrains his power but provides stable revenue and service — it is beneficial because it prevents defection. From the charter-keeper's seat, both parties are clients bound by the document's text. From the ecclesiastical seat, both parties are bound by sacramental oath. From competing lords' seats (excluded), the oath is a frustration — it prevents recruitment of dissatisfied vassals because breach carries reputational cost. The engine computes these differences from the structural data (role, power, exit, time horizon); this reading's authored metrics support the symmetric, mutually beneficial interpretation rather than the asymmetric extraction reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassal and lord are beneficiaries in this reading — the coordination solves a genuine problem for both. The vassal's directionality sits near 0.4 (mild beneficiary, some payer burden from rent and service); the lord's sits near 0.35 (mild beneficiary, some payer burden from protection obligation). Neither is a structural target — the charter's reciprocal language prevents the kind of asymmetric extraction that would place one party near d=1.0. Competing lords are excluded (not coordinated), which is why they are not stakeholders — their exclusion is the enforcement object itself. This symmetric, bounded directionality structure distinguishes the vassal_coordination_reading from the lord_extraction_reading, where directionality would be heavily asymmetric (lord near 1.0, vassal near 0.9, both targets of the lord's extraction with no charter limits).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false-rope trap by being explicit about the charter's enforceability. The founding problem (binding dispersed landholders) is live and attested throughout the feudal period — charters enumerate breach procedures and remedies, indicating active engagement with the mutual-obligation doctrine. If the founding problem had died (e.g., if centralized bureaucratic states made feudal oath unnecessary), the arrangement would persist as inertial performance (piton). But the evidence shows oath-breaking and breach litigation remained active mechanisms for adjusting the relationship, not ceremonial performance masking extraction. This indicates functional coordination rather than mandatrophic persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_enforcement_mechanism,
    'What enforcement mechanism made charter breach costly enough that lords voluntarily honored documented limits, when centralized courts did not exist?',
    'Historical analysis of breach litigation outcomes: if vassals successfully defected (with chronicle documentation), then reputation and fealty-withdrawal were the enforcement mechanism. If breach claims were rare or ignored, then the charter was more symbolic than binding.',
    'If reputation and defection are the mechanism, the vassal_coordination_reading holds — mutual enforceability is real. If breach was common and unpunished, the constraint shifts toward the lord_extraction_reading — the charter is a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_enforcement_mechanism, empirical, 'Whether feudal oath reciprocity was enforced through reputation/defection or was ceremonial cover for extraction.').

omega_variable(
    reading_dominance_by_region_and_period,
    'Did different regions or periods instantiate different readings? Did the same region shift from coordination to extraction as centralization progressed?',
    'Charter analysis across regions and centuries: coding breach language, lord-vassal dispute frequency, and oath renegotiation patterns. Regions with frequent successful defection on breach claims would instantiate the coordination reading; regions with lord-dominated breach decisions would show extraction reading.',
    'Evidence of regional/temporal reading variation would confirm that the kernel (the oath itself) admits multiple structurally distinct instantiations. Convergence toward extraction reading in later periods would suggest mandatrophy: the coordination function atrophied as centralized states emerged, but the oath persisted theatrically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_dominance_by_region_and_period, empirical, 'Whether feudal oath reciprocity was instantiated uniformly or varied by political context, suggesting different readings were live in different places/times.').

omega_variable(
    ecclesiastical_enforcement_asymmetry,
    'Did ecclesiastical sanction (oath-breaking as mortal sin) enforce the vassal''s compliance differently than the lord''s compliance? Did confession and absolution asymmetrically benefit lords?',
    'Theological analysis of penitential literature and bishop letters: if confession + penance allowed lords to oath-break and absolve themselves, while vassal oath-breaking risked excommunication, the ecclesiastical reading introduces asymmetric moral extraction.',
    'If ecclesiastical enforcement was symmetric, it reinforces the vassal_coordination_reading. If it was asymmetric (benefiting the lord), the ecclesiastical_mediation_reading might actually hide an extraction reading — the Church''s moral language could mask power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_enforcement_asymmetry, conceptual, 'Whether ecclesiastical oath enforcement was symmetric (both parties equally bound by sacrament) or asymmetric (favoring lords), which would reframe the reading landscape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t8, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(feud_tr_t8, observed).
narrative_ontology:measurement(feud_tr_t16, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement_basis(feud_tr_t16, observed).
narrative_ontology:measurement(feud_tr_t24, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement_basis(feud_tr_t24, observed).
narrative_ontology:measurement(feud_tr_t32, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement_basis(feud_tr_t32, observed).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(feud_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t8, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement_basis(feud_be_t8, observed).
narrative_ontology:measurement(feud_be_t16, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(feud_be_t16, observed).
narrative_ontology:measurement(feud_be_t24, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(feud_be_t24, observed).
narrative_ontology:measurement(feud_be_t32, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(feud_be_t32, observed).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(feud_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t8, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(feud_su_t8, observed).
narrative_ontology:measurement(feud_su_t16, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(feud_su_t16, observed).
narrative_ontology:measurement(feud_su_t24, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement_basis(feud_su_t24, observed).
narrative_ontology:measurement(feud_su_t32, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(feud_su_t32, observed).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(feud_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% The feudal_oath_reciprocity kernel admits three structurally distinct constraint readings: vassal_coordination_reading (this story, low ε rope with mutual enforceability), lord_extraction_reading (high ε snare, oath as cover for maximal extraction), and ecclesiastical_mediation_reading (moderate ε tangled_rope, moral sanction layer splitting extraction between lord and Church). Each reading produces a different ε value, different beneficiary structure, and different type classification from the same textual kernel. The three stories share the same kernel_id and are linked via affects_constraints to model the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
