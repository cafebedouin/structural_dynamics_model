% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law (Sovereignty Restoration Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law for Hong Kong (2020) is read here as a
 *   legitimate exercise of sovereign authority to restore constitutional
 *   order after the 2019 unrest. This is the sovereignty_restoration_reading
 *   of the nsl_legal_text kernel. The law criminalizes secession, subversion,
 *   terrorism, and collusion with foreign forces. While framed as a security
 *   measure, its operation eliminates opposition political capacity, placing
 *   it structurally between coordination (restoring governance function) and
 *   extraction (transferring political autonomy from opposition to central
 *   authorities). The sibling readings are democratic_enclosure_reading
 *   (permanent closure of democratic space) and
 *   jurisdictional_capture_reading (mainland legal transplant eroding common
 *   law autonomy).
 *
 * KEY AGENTS:
 *   - central_peoples_government_authority (agenda_setter/institutional/arbitrage): Administers the NSL through NPCSC and local office, captures political autonomy.
 *   - hong_kong_establishment_elite (beneficiary/powerful/constrained): Benefits from stabilized governance and elimination of opposition threats.
 *   - political_opposition (payer/moderate/trapped): Former legislators and parties facing disqualification and prosecution.
 *   - pro_democracy_activists (payer/powerless/trapped): Civil society and protesters targeted by broad security definitions.
 *   - hong_kong_judiciary (agenda_setter/institutional/constrained): Interprets NSL under NPCSC override authority.
 *   - international_human_rights_observers (observer/institutional/analytical): Monitor compliance with international law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.48).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.62).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law (Sovereignty Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '6dfa56a7-6782-43a8-b381-7e81f856645a').
narrative_ontology:cs_kernel_codification('6dfa56a7-6782-43a8-b381-7e81f856645a', formalized).
narrative_ontology:cs_authority_grounding('6dfa56a7-6782-43a8-b381-7e81f856645a', lineage).
narrative_ontology:cs_interpretation_layer_present('6dfa56a7-6782-43a8-b381-7e81f856645a').
narrative_ontology:cs_reading_relation('6dfa56a7-6782-43a8-b381-7e81f856645a', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dfa56a7-6782-43a8-b381-7e81f856645a', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('6dfa56a7-6782-43a8-b381-7e81f856645a', foundational, national_security_supremacy_over_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_supremacy_over_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6dfa56a7-6782-43a8-b381-7e81f856645a', national_security_supremacy_over_local_autonomy, conventional).
narrative_ontology:cs_axiom('6dfa56a7-6782-43a8-b381-7e81f856645a', foundational, restoration_of_constitutional_order_as_legitimate_mandate).
narrative_ontology:cs_axiom_status(restoration_of_constitutional_order_as_legitimate_mandate, holdable).
narrative_ontology:cs_axiom_grounding('6dfa56a7-6782-43a8-b381-7e81f856645a', restoration_of_constitutional_order_as_legitimate_mandate, conventional).
narrative_ontology:cs_reference_frame('6dfa56a7-6782-43a8-b381-7e81f856645a', sovereign_constitutional_order).
narrative_ontology:cs_drift_state('6dfa56a7-6782-43a8-b381-7e81f856645a', post_nsl_enactment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6dfa56a7-6782-43a8-b381-7e81f856645a', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_establishment_elite).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, national_security_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereignty_integrity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the NSL through the NPCSC and operates the Office for Safeguarding National Security in Hong Kong. It frames the law as closing security loopholes exposed by the 2019 unrest and asserts interpretive authority over the scope of offenses. It gains eliminated opposition challenges and consolidated governance control.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Political and business figures aligned with the sovereign who benefit from the stabilization of governance and the marginalization of opposition threats to their institutional positions and policy preferences.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_establishment_elite, beneficiary,
    powerful, generational, constrained, national).

% Former legislators, district councillors, and party members disqualified or prosecuted under the NSL. Their ability to participate in formal politics collapsed after the law's enactment, and many face exile or imprisonment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    moderate, biographical, trapped, national).

% Civil society organizers, protesters, and advocacy groups prosecuted or intimidated under broad security definitions. They experience the law as criminalizing previously lawful dissent and face arrest, exile, or self-censorship.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, national).

% Hears NSL cases under new procedural rules and evidentiary standards but faces NPCSC override authority that constrains common law autonomy. It administers the legal mechanics while operating under mainland legal system influence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% UN bodies, foreign governments, and NGOs monitoring the NSL's compliance with international human rights law and the Sino-British Joint Declaration. They issue reports and sanctions recommendations but do not directly experience the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_peoples_government_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restoring constitutional order and governance stability in Hong Kong following the 2019 social unrest by closing perceived national security loopholes in the Basic Law framework.
% TRANSFER_FUNCTION: Moves the authority to define and prosecute security threats from the Hong Kong common law tradition to a hybrid mainland-Hong Kong mechanism, transferring political autonomy and legal capacity from opposition actors to central and establishment authorities.
% ABSENT_VOICES: Exiled opposition figures, disbanded independent trade unions, and foreign jurists who would contest the compatibility of the NSL with common law procedural safeguards are structurally absent from the domestic legislative and judicial process.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the current disqualification and prosecution of opposition figures would halt, the legislative and electoral landscape would reopen to previously excluded parties, and the central government's direct enforcement apparatus in Hong Kong would lose its primary legal anchor; the political system would revert toward its pre-2020 contested equilibrium.
% FOUNDING_PROBLEM: The 2019 anti-extradition protests and associated unrest were interpreted by the sovereign authority as exposing a national security vacuum in Hong Kong's legal system, where separatism, subversion, terrorism, and collusion with foreign forces lacked adequate criminalization.
% FOUNDING_PROBLEM_CORROBORATION: The central government and Hong Kong establishment attest the problem is live and ongoing, citing continued foreign interference. Opposition figures, exiled activists, and international human rights bodies attest the founding problem was manufactured or exaggerated to justify political closure; corroboration from outside the benefiting parties supports the manufactured-crisis reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint targets a specific political subset rather than the general population, concentrating extraction on opposition capacity while leaving commercial and non-political civil society largely untouched. Suppression (0.62) is significant because the law's persistence depends on active enforcement, NPCSC interpretive authority, and the exclusion of common law procedural alternatives. Theater ratio (0.40) reflects the increasing performative dimension of 'restoring order' as the initial security threat recedes and enforcement shifts to political management. Accessibility collapse (0.65) is high for opposition actors but moderate for the general public. Resistance (0.70) is substantial from the targeted opposition and international actors.
 *
 * PERSPECTIVAL GAP:
 *   From the CPG authority seat, the constraint is necessary coordination that closes a security vacuum and restores constitutional order; effective extraction is low or negative because the sovereign is merely reclaiming authority that rightfully belongs to it. From the opposition and activist seats, the same structure is experienced as targeted extraction that eliminates lawful political participation; their high directionality amplifies effective extraction. The judiciary sits in a structurally ambivalent position: it administers the constraint but under overriding authority that constrains its common law autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   CPG authority and establishment elite are structural beneficiaries (d near 0.0): the constraint subsidizes their control and eliminates challengers. Political opposition and pro-democracy activists are structural targets (d near 1.0): they bear the costs of disqualification, prosecution, and exile. The Hong Kong judiciary is not declared as a beneficiary or victim; its directionality falls toward the middle due to its dual position as administrator and subject to override. International observers have analytical exit and are outside the directionality chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the NSL as pure extraction (snare) by acknowledging the genuine coordination function of restoring governance stability after prolonged unrest, while also preventing mislabeling it as pure coordination (rope) by recording the identifiable victim set of opposition actors. If the 2019 unrest were fully resolved and the law remained primarily as political management theater, it would drift toward piton; currently the founding problem is contested, keeping it in the hybrid zone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_boundary_ambiguity,
    'Is the classification of ''national security threat'' under the NSL bounded by genuine violent separatism, or does it extend to all organized political opposition by design?',
    'Comparative analysis of prosecutions and disqualifications against objective violence thresholds; doctrinal review of NPCSC interpretations.',
    'If the boundary is inherently unbounded, extraction is structural rather than incidental and the coordination narrative becomes cover for generalized political elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_boundary_ambiguity, conceptual, 'Whether NSL threat definitions are narrowly security-based or broadly political.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the NSL legal text kernel. How would sibling readings (democratic enclosure, jurisdictional capture) restructure the beneficiary/victim sets and the coordination narrative?',
    'Cross-reading comparison of the same legal provisions across the three instantiated constraints.',
    'Sibling readings would shift CPG authority from beneficiary to extractor, the judiciary from administrator to captured institution, and would recast the coordination function as cover story rather than genuine governance restoration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between sibling readings of the NSL kernel.').

omega_variable(
    founding_problem_empirical_basis,
    'Was the 2019 unrest genuinely a separatist national security crisis requiring central intervention, or a localized governance legitimacy crisis?',
    'Independent historical and sociological analysis of protest demands, organizational structures, and foreign involvement levels.',
    'If the unrest was a governance legitimacy crisis rather than a security vacuum, the coordination function is cover for extraction and the founding problem narrative is a manufactured justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_basis, empirical, 'Empirical basis of the founding security problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_sov_rest_tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl_sov_rest_tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(nsl_sov_rest_tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(nsl_sov_rest_tr_t36, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 36, 0.34).
narrative_ontology:measurement(nsl_sov_rest_tr_t48, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 48, 0.37).
narrative_ontology:measurement(nsl_sov_rest_tr_t60, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl_sov_rest_be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nsl_sov_rest_be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(nsl_sov_rest_be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(nsl_sov_rest_be_t36, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 36, 0.42).
narrative_ontology:measurement(nsl_sov_rest_be_t48, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement(nsl_sov_rest_be_t60, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(nsl_sov_rest_su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nsl_sov_rest_su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(nsl_sov_rest_su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(nsl_sov_rest_su_t36, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(nsl_sov_rest_su_t48, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(nsl_sov_rest_su_t60, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nsl_legal_text kernel, decomposed per the epsilon-invariance principle. The sibling readings (democratic_enclosure_reading, jurisdictional_capture_reading) share the same legal text but instantiate structurally distinct constraints with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
