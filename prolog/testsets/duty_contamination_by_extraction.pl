% ============================================================================
% CONSTRAINT STORY: duty_contamination_by_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_duty_contamination_by_extraction, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: duty_contamination_by_extraction
 *   human_readable: Duty Contamination by Adjacent Extraction
 *   domain: social_systems/institutional_dynamics/power_asymmetry
 *
 * SUMMARY:
 *   Duty contamination by extraction describes a network effect where
 *   high-purity reciprocal obligation structures — family duty, professional
 *   codes of conduct, guild membership norms — experience effective purity
 *   degradation when coupled with adjacent extractive mechanisms through
 *   shared enforcement infrastructure (reputation systems, institutional
 *   gatekeeping, social sanctioning). The duty structure itself remains
 *   unchanged: the reciprocal obligations, the behavioral norms, and the
 *   coordination function persist. What changes is the effective experience
 *   of duty-bearers, who find that performing their duties now feeds an
 *   extractive system they did not consent to and cannot exit. The
 *   contamination is measurable through workaround behaviors (lying to family
 *   about professional obligations, concealing income to avoid extraction,
 *   strategic non-compliance with institutional demands) and justification
 *   drift (duty-bearers shift from reciprocity logic — 'I do this because we
 *   take care of each other' — to economic calculation — 'I do this because
 *   the cost of not doing it is too high'). The constraint is structurally
 *   distinct from the contaminating system itself: a predatory lending
 *   regime, a surveillance apparatus, or a regulatory capture mechanism may
 *   have its own extractiveness value, but the duty contamination constraint
 *   measures the network effect on the duty structure's effective purity.
 *   This is a tangled rope because genuine coordination persists (the duty
 *   structure still solves real problems) alongside asymmetric extraction
 *   (the contaminating system siphons value without bearing reciprocal
 *   obligations).
 *
 * KEY AGENTS:
 *   - Identity-Bound Duty Bearer: Primary victim (powerless/identity_locked) — cannot exit duty structure due to identity fusion; experiences maximum extraction as contamination transforms reciprocity into asymmetry
 *   - Constrained Duty Bearer: Secondary victim (moderate/constrained) — recognizes contamination; can exit at high cost; develops workarounds to preserve duty function while minimizing extraction
 *   - Contaminating System Enforcer: Primary beneficiary (institutional/arbitrage) — extracts value from duty structures without bearing reciprocal obligations; experiences coupling as coordination
 *   - Institutional Gatekeeper: Beneficiary and potential reformer (institutional/mobile) — mediates between duty and extraction; increasingly recognizes contamination as problem requiring structural intervention
 *   - Reform Coalition: Organized agents (organized/mobile) — professional associations, family law reform advocates, labor unions building firewalls between duty and extraction
 *   - Duty Structure Integrity: Abstract victim (powerless/trapped) — the reciprocity norm itself as a collective good; cannot exit or organize; bears full cost of contamination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(duty_contamination_by_extraction, 0.48).
domain_priors:suppression_score(duty_contamination_by_extraction, 0.52).
domain_priors:theater_ratio(duty_contamination_by_extraction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(duty_contamination_by_extraction, extractiveness, 0.48).
narrative_ontology:constraint_metric(duty_contamination_by_extraction, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(duty_contamination_by_extraction, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(duty_contamination_by_extraction, tangled_rope).
narrative_ontology:human_readable(duty_contamination_by_extraction, "Duty Contamination by Adjacent Extraction").
narrative_ontology:topic_domain(duty_contamination_by_extraction, "social_systems/institutional_dynamics/power_asymmetry").

domain_priors:requires_active_enforcement(duty_contamination_by_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(duty_contamination_by_extraction, enforcers_of_contaminating_system).
narrative_ontology:constraint_beneficiary(duty_contamination_by_extraction, institutional_gatekeepers).
narrative_ontology:constraint_victim(duty_contamination_by_extraction, actors_subject_to_both_constraints).
narrative_ontology:constraint_victim(duty_contamination_by_extraction, duty_structure_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-BOUND DUTY BEARER (SNARE) — Cannot exit the duty structure (family obligation, professional identity) and experiences maximum extraction as the contaminating system transforms reciprocal obligation into asymmetric extraction. Identity fusion with the duty role prevents recognition that the structure has been contaminated — the agent continues performing duty logic while the effective mechanism has shifted to extraction. High suppression through identity lock rather than material barriers.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED DUTY BEARER (TANGLED ROPE) — Recognizes the contamination and experiences both genuine coordination (the duty structure still solves real problems) and extraction (the adjacent system siphons value). Can exit at high cost (abandoning family, changing profession) but constrained by material and social penalties. Develops workaround behaviors (selective disclosure, strategic lying) to preserve duty function while minimizing extraction. Mixed experience — some agency, some benefit, significant cost.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTAMINATING SYSTEM ENFORCER (ROPE) — Benefits from the contamination by extracting value from duty structures without bearing reciprocal obligations. Experiences the constraint as coordination: the duty structure provides a stable substrate for extraction, and the coupling mechanism (reputation systems, institutional gatekeeping) is a legitimate coordination tool. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Organized agents (professional associations, family law reform advocates, labor unions) see both the coordination value of duty structures and the extraction introduced by contamination. Have mobility to exit specific contaminated instances and agency to advocate for structural separation (firewalls between duty and extraction). Experience moderate extraction because they can organize collective action but face institutional resistance.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL GATEKEEPER TRANSITIONAL (SCAFFOLD) — Institutions that mediate between duty structures and extractive systems (HR departments, family courts, professional licensing boards) increasingly recognize contamination as a problem requiring structural intervention. See the current coupling as temporary — new norms (conflict-of-interest policies, duty-of-care standards, fiduciary obligations) are creating separation mechanisms. Low effective extraction because these actors have agency and see a sunset path, though enforcement mechanisms remain active during transition.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, duty contamination is a recurring structural pattern across cultures and epochs: high-purity reciprocal obligation systems (kinship, guild membership, professional codes) couple with extractive mechanisms (tribute systems, rent-seeking, regulatory capture) through reputation and enforcement networks. The contamination is neither natural law nor pure extraction — it is a hybrid where genuine coordination function persists alongside asymmetric extraction. The analytical classification matches the claimed type, confirming structural diagnosis.
constraint_indexing:constraint_classification(duty_contamination_by_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(duty_contamination_by_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(duty_contamination_by_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(duty_contamination_by_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(duty_contamination_by_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(duty_contamination_by_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The contamination introduces asymmetric extraction into what was previously a reciprocal coordination structure. The duty-bearer continues performing obligations under reciprocity logic while the contaminating system captures value without reciprocating. The extraction is not maximal because the duty structure retains genuine coordination function — family obligations still coordinate care, professional codes still coordinate quality standards — but the effective purity has degraded significantly. The value reflects the network contamination coefficient: intrinsic purity of the duty structure (low ε, perhaps 0.10-0.15) remains stable, but effective purity experienced by duty-bearers drops due to coupling with the extractive system. Suppression (0.52): Moderate-high. Duty-bearers face significant barriers to exit: identity fusion with duty roles (family member, professional, guild member), social sanctioning for duty violation, material dependency on the institutional structure, and reputational damage. The suppression is not total — some agents can exit at high cost, and workarounds provide partial relief — but it is substantial. The suppression mechanism differs by agent: identity-locked agents experience cognitive suppression (cannot imagine exit), constrained agents experience material suppression (high exit cost), and organized agents experience institutional suppression (collective action barriers). Theater ratio (0.38): Moderate. Some performative content has emerged as duty-bearers develop workarounds: strategic compliance (performing duty rituals while minimizing actual obligation), justification theater (claiming reciprocity logic while actually calculating costs), and concealment (hiding workarounds from enforcers). The theater is not as high as in a piton because the duty structure retains substantial functional content — the obligations are real and the coordination function persists. The theater ratio has increased over the interval as contamination has deepened and workarounds have proliferated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a characteristic tangled rope perspectival gap. Identity-bound duty bearers see a snare — they are trapped by identity fusion and experience pure extraction as the contamination transforms reciprocity into asymmetry. They cannot recognize the contamination because their identity is constituted through the duty role; from within that frame, the extraction appears as legitimate obligation. Constrained duty bearers see a tangled rope — they recognize both the genuine coordination function (the duty structure still solves real problems) and the extraction (the contaminating system siphons value). They develop workarounds to preserve the former while minimizing the latter. Contaminating system enforcers see a rope — they experience the coupling as coordination, a legitimate mechanism for enforcing obligations and maintaining standards. They are net beneficiaries and do not perceive extraction. Institutional gatekeepers increasingly see a scaffold — they recognize the contamination as a temporary problem requiring structural intervention, and new norms (conflict-of-interest policies, fiduciary obligations) are creating separation mechanisms. Reform coalitions see a tangled rope with a path to de-contamination — they have agency to build firewalls but face institutional resistance. The analytical observer sees a tangled rope at the civilizational scale — duty contamination is a recurring structural pattern where genuine coordination and asymmetric extraction coexist, neither reducible to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect each agent's structural relationship to the contamination mechanism. Identity-bound duty bearers are full victims with identity-locked exit — they cannot leave the duty structure and experience maximum extraction as the contamination transforms their reciprocal obligations into asymmetric burdens. Their d value is high (victim + identity_locked → d ≈ 0.89), producing high effective extraction. Constrained duty bearers are victims with constrained exit — they recognize the contamination and can leave at high cost, giving them some agency but still significant extraction. Their d value is moderate-high (victim + constrained → d ≈ 0.75). Contaminating system enforcers are beneficiaries with arbitrage exit — they extract value without bearing reciprocal obligations and can move between duty structures freely. Their d value is low (beneficiary + arbitrage → d ≈ 0.05), producing low or negative effective extraction. Institutional gatekeepers occupy a complex position: they benefit from mediating the coupling but increasingly recognize it as problematic. Their d value is low-moderate (beneficiary + mobile → d ≈ 0.15-0.25), reflecting net benefit with emerging agency to reform. Reform coalitions are organized agents with mobile exit — they experience moderate extraction but have collective action capacity to build structural separation. Their d value is moderate (victim + organized + mobile → d ≈ 0.55). The analytical observer sees the full structure: genuine coordination function persists alongside asymmetric extraction, producing a tangled rope classification that matches the claimed type.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the tangled rope classification is structurally necessary: the duty structure retains genuine coordination function (family obligations coordinate care, professional codes coordinate quality) while the contamination introduces asymmetric extraction (the contaminating system captures value without reciprocating). This is not a snare misclassified as coordination — the coordination function is real and measurable through the persistence of reciprocal behaviors even as justification logic shifts. Nor is it a rope misclassified as extraction — the extraction is real and measurable through workaround behaviors, justification drift, and effective purity degradation. The tangled rope classification captures the hybrid structure: both mechanisms operate simultaneously, and neither can be eliminated without changing the constraint's identity. The perspectival gap confirms this: identity-bound agents see a snare (maximum extraction, no coordination visible from within identity lock), beneficiaries see a rope (coordination, no extraction visible from beneficiary position), and the analytical observer sees the tangled rope (both mechanisms structurally present). The mandatrophy is resolved by recognizing that the classification depends on the observer's structural position — all perspectives are legitimate readings of the same structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_vs_effective_purity_threshold,
    'At what contamination coefficient does effective purity drop below the threshold where duty-bearers shift from reciprocity logic to economic calculation?',
    'Longitudinal ethnographic study tracking justification drift in duty-bearer populations; measurement of workaround behavior frequency as function of contamination exposure; identification of tipping point where reciprocal framing collapses',
    'If threshold is low (contamination coefficient > 0.3): most duty structures are already contaminated and the ''pure duty'' baseline is aspirational. If threshold is high (contamination coefficient > 0.7): contamination is rare and duty structures are robust to moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_effective_purity_threshold, empirical, 'Contamination threshold for reciprocity-to-extraction shift').

omega_variable(
    workaround_as_resistance_or_adaptation,
    'Do workaround behaviors (lying, concealment, strategic non-compliance) represent resistance to contamination or adaptation that stabilizes the contaminated equilibrium?',
    'Analysis of workaround behavior outcomes: do they reduce extraction over time (resistance) or enable continued extraction by preventing duty structure collapse (stabilization)? Comparison of contaminated systems with vs without widespread workarounds.',
    'If resistance: workarounds are a path to de-contamination and should be supported. If stabilization: workarounds perpetuate extraction by making it tolerable, and structural separation is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(workaround_as_resistance_or_adaptation, conceptual, 'Whether workarounds resist or stabilize contamination').

omega_variable(
    contamination_reversibility,
    'Can duty structures recover intrinsic purity after contamination is removed, or does contamination cause permanent degradation of reciprocity norms?',
    'Historical case studies of duty structures before, during, and after contamination episodes; measurement of reciprocity norm strength in post-contamination populations; identification of hysteresis effects',
    'If reversible: structural separation (firewalls) can restore duty function. If irreversible: contamination causes permanent norm erosion and duty structures must be rebuilt from scratch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contamination_reversibility, empirical, 'Whether contamination causes permanent norm degradation').

omega_variable(
    identity_lock_mechanism,
    'Is identity lock in duty-bearers a pre-existing condition (duty structures select for identity-fused individuals) or a consequence of contamination (extraction requires identity fusion to persist)?',
    'Comparison of identity fusion levels in duty-bearers in contaminated vs uncontaminated duty structures; longitudinal tracking of identity fusion development in individuals entering duty roles',
    'If pre-existing: contamination exploits existing identity lock. If consequence: contamination actively cultivates identity lock as a suppression mechanism, and de-contamination would reduce identity fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity lock precedes or follows contamination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(duty_contamination_by_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duty_contam_tr_t0, duty_contamination_by_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(duty_contam_tr_t3, duty_contamination_by_extraction, theater_ratio, 3, 0.28).
narrative_ontology:measurement(duty_contam_tr_t6, duty_contamination_by_extraction, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(duty_contam_be_t0, duty_contamination_by_extraction, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(duty_contam_be_t3, duty_contamination_by_extraction, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(duty_contam_be_t6, duty_contamination_by_extraction, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(duty_contamination_by_extraction, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of reputation_as_distributed_enforcement. The reputation system provides the coupling mechanism through which extractive constraints contaminate duty structures: reputation sanctions enforce both reciprocal obligations (duty function) and extractive demands (contaminating system), making it impossible for duty-bearers to comply with one without feeding the other. The duty contamination constraint has its own extractiveness value (0.48) reflecting the network contamination effect, distinct from the reputation system's extractiveness (which measures the reputation mechanism itself). The contaminating systems (predatory lending, surveillance, regulatory capture) would be modeled as separate constraints with their own extractiveness values; this constraint measures the contamination effect on duty structure purity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(duty_contamination_by_extraction, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
