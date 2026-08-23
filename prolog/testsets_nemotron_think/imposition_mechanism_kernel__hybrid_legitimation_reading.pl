% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Imperial Hybrid Legitimation Mechanism (Charismatic Transfer + Institutional Incentives)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the hybrid legitimation mechanism observed in
 *   early imperial formations (e.g., Han China, Roman Principate, Mauryan
 *   India) where new norms — bureaucratic procedures, ritual calendars, legal
 *   codes, moral vocabularies — achieved legitimacy not through popular
 *   demand (endogenous climb) nor raw decree (exogenous override) but through
 *   the emperor's performed exemplariness combined with institutional
 *   incentive structures. Adoption was stratified: court elites emulated the
 *   emperor voluntarily for status; provincial elites adopted for office and
 *   revenue; peasant masses complied through a slow mix of festival
 *   participation, tax incentives, and elite pressure. The mechanism's
 *   coherence depends on the emperor's continuing charismatic performance;
 *   when the center weakens, the incentive structure remains but loses its
 *   legitimating aura, drifting toward theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.35).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Imperial Hybrid Legitimation Mechanism (Charismatic Transfer + Institutional Incentives)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'ca0168fd-8cdb-4b1a-9719-19a890372c70').
narrative_ontology:cs_kernel_codification('ca0168fd-8cdb-4b1a-9719-19a890372c70', distributed).
narrative_ontology:cs_authority_grounding('ca0168fd-8cdb-4b1a-9719-19a890372c70', extraction).
narrative_ontology:cs_interpretation_layer_present('ca0168fd-8cdb-4b1a-9719-19a890372c70').
narrative_ontology:cs_reading_relation('ca0168fd-8cdb-4b1a-9719-19a890372c70', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca0168fd-8cdb-4b1a-9719-19a890372c70', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ca0168fd-8cdb-4b1a-9719-19a890372c70', foundational, symbolic_authority_transfer_is_primary_legitimation_vector).
narrative_ontology:cs_axiom_status(symbolic_authority_transfer_is_primary_legitimation_vector, holdable).
narrative_ontology:cs_axiom_grounding('ca0168fd-8cdb-4b1a-9719-19a890372c70', symbolic_authority_transfer_is_primary_legitimation_vector, conventional).
narrative_ontology:cs_axiom('ca0168fd-8cdb-4b1a-9719-19a890372c70', foundational, institutional_incentives_complement_rather_than_replace_charismatic_legitimacy).
narrative_ontology:cs_axiom_status(institutional_incentives_complement_rather_than_replace_charismatic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ca0168fd-8cdb-4b1a-9719-19a890372c70', institutional_incentives_complement_rather_than_replace_charismatic_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('ca0168fd-8cdb-4b1a-9719-19a890372c70', imperial_charismatic_legitimation_model).
narrative_ontology:cs_drift_state('ca0168fd-8cdb-4b1a-9719-19a890372c70', post_imperial_collapse, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ca0168fd-8cdb-4b1a-9719-19a890372c70', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_masses).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_tradition_holders).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, minority_cultures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_universalism_doctrine).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, civilizing_mission_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodies the symbolic authority that legitimizes new norms; his personal example (ritual performance, moral conduct, cultural patronage) is the primary vector of legitimacy transfer. He does not directly enforce compliance but his charismatic presence makes elite emulation structurally rational. The constraint's coherence depends on his continued exemplary performance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, emperor, agenda_setter,
    institutional, generational, analytical, universal).

% First adopters of new norms through direct exposure to the emperor's example. They gain status, access, and resource flows by performing imperial culture. Their adoption is voluntary in form but structurally incentivized — non-adoption means exclusion from the imperial favor economy. They capture the prestige rents of early adoption.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, beneficiary,
    powerful, biographical, mobile, national).

% Adopt imperial norms to mediate between center and periphery. They receive administrative appointments, tax farming rights, and status recognition in exchange for enforcing normative compliance in their domains. They pay the cost of suppressing local resistance and funding local imperial cult infrastructure. Their position is dual: they extract from peasants while paying tribute upward.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_elites, payer).

% Designs and administers the incentive structures (examination systems, rank hierarchies, ritual calendars) that translate charismatic legitimacy into routine compliance. They benefit from the expanded administrative scope and professionalization the mechanism creates. Their enforcement is moderate — they rely on the mechanism's self-reinforcing logic more than overt coercion.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy, beneficiary).

% Adopt new norms last, driven by a combination of elite pressure, fiscal incentives (tax relief for ritual participation), and the slow percolation of imperial culture through festival cycles. They bear the compliance costs (labor for imperial rituals, foregone local practices) without capturing status benefits. Exit is geographically and economically blocked.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_masses, payer,
    powerless, biographical, trapped, local).

% Village elders, lineage heads, and cult specialists whose authority derives from pre-imperial cosmologies. They are structurally excluded from the imperial legitimation circuit — their traditions are either assimilated (as 'folk variants') or suppressed. Their resistance is cultural persistence; their exit would mean abandoning the identity that constitutes their authority.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_tradition_holders, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, local_tradition_holders, excluded).

% Non-integrated populations at the imperial margins (border peoples, religious minorities, pastoralists). The hybrid mechanism offers them no adoption pathway — they are neither invited to emulate the emperor nor given bureaucratic incentives. They experience the mechanism as pure extraction (tribute, labor, territorial encroachment) without the legitimacy transfer that binds core populations.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, minority_cultures, excluded,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, minority_cultures, payer).

% Comparative historians and sociologists who reconstruct the mechanism from chronicles, archaeological strata, and institutional records. They see the full stratified adoption pattern and the charisma-incentive interplay that participants at any single level could not perceive. Their analysis is the only seat from which the hybrid structure is fully visible.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legitimacy transfer from imperial center to periphery enabling unified normative order across linguistically, religiously, and ethnically diverse populations without maintaining standing armies in every valley.
% TRANSFER_FUNCTION: Moves normative compliance and ritual adherence from peripheral populations to imperial center via elite intermediaries; status, administrative office, and fiscal privileges flow from center to provincial elites; tax revenue and labor flow from masses to bureaucracy.
% ABSENT_VOICES: Peasant masses and minority cultures experience the norm imposition as a structure they inhabit but cannot contest within the imperial discourse; their resistance appears only as 'banditry,' 'superstition,' or 'barbarian recalcitrance' in the bureaucratic record.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism vanished overnight, the normative unity of the empire would fracture: provincial elites would revert to local power bases, peasant masses would resume autonomous village ritual cycles, and the bureaucracy would lose its legitimating mandate — the imperial polity would either collapse or revert to pure coercion (exogenous override) at vastly higher cost.
% FOUNDING_PROBLEM: How to achieve normative unity across a diverse empire without pure coercion (which breeds resistance and requires unsustainable military expenditure) or pure bottom-up adoption (which is too slow, uneven, and fails to integrate peripheral zones).
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical sociology (Tilly on coercion-capital, Mann on IEMP networks, Eisenstadt on imperial institutionalization) attests to the structural problem of integrating diverse territories. Imperial chronicles and bureaucratic records attest to the hybrid solution but are beneficiary sources. Non-imperial sources (local gazetteers, rebel manifestos, minority oral histories) corroborate the problem's reality while contesting the solution's legitimacy.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the mechanism genuinely coordinates normative unity across diversity — a real collective action problem — but the coordination is asymmetrically priced: elites capture prestige rents, masses bear compliance costs. Suppression is moderate (0.35) because the mechanism relies more on incentive alignment and charismatic attraction than force, though local resistance is managed. Theater ratio rises over time (0.1→0.25) as the charismatic center's exemplary power decays while the incentive bureaucracy persists. Accessibility collapse (0.55) reflects that local alternatives persist but become structurally marginalized. Resistance (0.42) captures the persistent low-level non-compliance and cultural persistence that never fully disappears.
 *
 * PERSPECTIVAL GAP:
 *   From the emperor's seat, the mechanism is pure coordination — his example naturally attracts emulation. From the provincial elite seat, it is a favorable exchange — status for compliance. From the peasant seat, it is an imposed normative order with extractive fiscal hooks. From the local tradition holder seat, it is an existential threat to their cosmological authority. The engine computes these divergent seat types from the power/exit/spatial_scope structure; the claimed_type (tangled_rope) reflects the analyst's integrated view.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor sits at the analytical/beneficiary pole — he authors the constraint but does not experience its extraction. Imperial court and provincial elites are net beneficiaries (d ~ 0.15-0.25): they capture status and resource flows. State bureaucracy is near-symmetric (d ~ 0.45): it administers the mechanism and benefits from its expansion but bears enforcement costs. Peasant masses, local tradition holders, and minority cultures are net targets (d ~ 0.7-0.9): they pay compliance costs with minimal return. The stratified adoption timeline (elites first, masses later) is the temporal signature of this directionality gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The mechanism was founded to solve the integration problem of early empire. That problem persists in successor states, but the specific hybrid solution (imperial charisma + bureaucratic incentives) is historically superseded — the charismatic center is gone, leaving only the incentive bureaucracy. This is mandatrophy: the mandate (imperial legitimation) has outlived its founding function, but the institutional shell persists. The founding_problem_status = contested captures this: the structural problem lives, the specific solution is dead, but the arrangement continues because no actor has both the power and incentive to replace it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the hybrid_legitimation_reading of imposition_mechanism_kernel. What structural elements distinguish it from endogenous_climb_reading and exogenous_override_reading as a separate constraint rather than a parameter variation?',
    'Compare the three readings'' beneficiary/victim structures, enforcement profiles, and temporal adoption curves. If they produce non-overlapping stakeholder seat types and divergent epsilon trajectories, they are distinct constraints per epsilon-invariance.',
    'If the readings collapse to one constraint under measurement, the kernel is not genuinely contested — the ''three readings'' are observer framings of one structure. If they remain distinct, the kernel decomposition is validated and each reading gets independent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s three declared readings are structurally distinct constraints or observer framings of one constraint.').

omega_variable(
    charisma_vs_coercion_boundary,
    'How much of the measured compliance is genuine charismatic legitimacy (voluntary emulation of the emperor) vs. coerced compliance dressed in charismatic language?',
    'Counterfactual: remove the emperor''s exemplary performance while keeping incentive structures intact. If compliance holds, charisma was decorative; if compliance collapses, charisma was load-bearing. Historical test: succession crises where charismatic center fails but bureaucracy persists.',
    'If charisma is decorative, the mechanism is exogenous_override with a cultural veneer (higher extraction, lower coordination). If charisma is load-bearing, the hybrid structure is real and the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charisma_vs_coercion_boundary, empirical, 'Whether the charismatic vector is structurally load-bearing or performative cover for coercion.').

omega_variable(
    elite_capture_of_incentives,
    'Did the institutional incentive structures primarily serve imperial integration, or did they become rent-extraction mechanisms for provincial elites that the center could not control?',
    'Trace the fiscal flows: did tax revenue from peasant compliance reach the imperial treasury, or was it captured at the provincial level? Compare bureaucratic records (center''s view) with local gazetteers (periphery''s view).',
    'If incentives were captured, the mechanism''s coordination function degrades over time — it becomes a piton (theatrical maintenance of imperial forms while extraction concentrates locally). This would show as rising theater_ratio and extractiveness in the late interval, which the measurements capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_incentives, empirical, 'Whether institutional incentives remained aligned with imperial integration or were captured as elite rent extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(impo_tr_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 120, 0.23).
narrative_ontology:measurement(impo_tr_t160, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 160, 0.24).
narrative_ontology:measurement(impo_tr_t200, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 200, 0.25).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(impo_be_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 120, 0.42).
narrative_ontology:measurement(impo_be_t160, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 160, 0.44).
narrative_ontology:measurement(impo_be_t200, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(impo_su_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 120, 0.33).
narrative_ontology:measurement(impo_su_t160, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 160, 0.34).
narrative_ontology:measurement(impo_su_t200, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.08).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three readings differing in the primary legitimation vector: endogenous_climb_reading (bottom-up popular adoption), exogenous_override_reading (state coercion), and this hybrid_legitimation_reading (charismatic authority transfer + institutional incentives). This reading captures the stratified adoption pattern where elites adopt first via charismatic identification, masses later via incentive structures, and the mechanism's coherence depends on the charismatic center's continued performance. The three readings form a constraint family linked by shared referent (the imposition mechanism) but distinct epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
