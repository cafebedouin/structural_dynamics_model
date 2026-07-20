% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin: First Held Reading
 *   domain: monetary_history/institutional_economics
 *
 * SUMMARY:
 *   The 'first held' reading of the digital_money_origin kernel identifies
 *   the emergence of digital money with the moment individuals began treating
 *   non-physical instruments as practical stores of value. This reading dates
 *   emergence later than the 'became thinkable' reading and earlier than the
 *   'regulatory recognition' reading. It frames the constraint as a
 *   user-driven monetary innovation whose persistence depends on technical
 *   infrastructure and network effects. The constraint coordinates
 *   long-distance value transfer but asymmetrically extracts from
 *   infrastructure-excluded populations while concentrating benefits among
 *   early adopters and platform providers. As a kernel reading, it is
 *   authored as a clean Îµ-invariant constraint: the structural data describe
 *   the constraint instantiated by this reading alone, without averaging
 *   across sibling readings.
 *
 * KEY AGENTS:
 *   - early_adopters_with_access: Primary beneficiary (moderate/constrained) â gains network-effect rents and liquidity advantages.
 *   - infrastructure_excluded_populations: Primary target (powerless/trapped) â bears exclusion costs as economic life digitizes.
 *   - digital_infrastructure_providers: Agenda setter (institutional/arbitrage) â maintains technical barriers and captures transaction rents.
 *   - monetary_historians: Analytical observer â traces the emergence and contested readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.58).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin: First Held Reading").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'a6807374-7bc4-4492-9c82-c9588f41fd89').
narrative_ontology:cs_kernel_codification('a6807374-7bc4-4492-9c82-c9588f41fd89', distributed).
narrative_ontology:cs_authority_grounding('a6807374-7bc4-4492-9c82-c9588f41fd89', distributed).
narrative_ontology:cs_reading_relation('a6807374-7bc4-4492-9c82-c9588f41fd89', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6807374-7bc4-4492-9c82-c9588f41fd89', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('a6807374-7bc4-4492-9c82-c9588f41fd89', foundational, holding_defines_emergence).
narrative_ontology:cs_axiom_status(holding_defines_emergence, holdable).
narrative_ontology:cs_axiom_grounding('a6807374-7bc4-4492-9c82-c9588f41fd89', holding_defines_emergence, empirically_contingent).
narrative_ontology:cs_axiom('a6807374-7bc4-4492-9c82-c9588f41fd89', foundational, emergence_precedes_regulation).
narrative_ontology:cs_axiom_status(emergence_precedes_regulation, holdable).
narrative_ontology:cs_axiom_grounding('a6807374-7bc4-4492-9c82-c9588f41fd89', emergence_precedes_regulation, empirically_contingent).
narrative_ontology:cs_reference_frame('a6807374-7bc4-4492-9c82-c9588f41fd89', practical_holding_as_monetary_reality).
narrative_ontology:cs_drift_state('a6807374-7bc4-4492-9c82-c9588f41fd89', contemporary_digital_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6807374-7bc4-4492-9c82-c9588f41fd89', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_access).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_infrastructure_providers).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, infrastructure_excluded_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who acquired non-physical monetary instruments early and hold them as practical stores of value. They benefit from network-effect appreciation, liquidity premiums, and first-mover advantages as digital money scales. Their exit is constrained because converting back to physical stores or alternative rails incurs losses, tax events, or loss of network access.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_access, beneficiary,
    moderate, biographical, constrained, national).

% Populations lacking reliable internet, smart devices, or digital literacy. As commerce and savings migrate to digital rails, they face escalating barriers to economic participation and wealth preservation. They are structurally excluded from the constraint's benefit stream and bear higher frictions or complete exclusion from digital-era trade.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, infrastructure_excluded_populations, payer,
    powerless, immediate, trapped, regional).

% Entities that build and maintain the technical protocols, payment networks, and data centers enabling digital money. They set standards that create implementation barriers, capture transaction fees and data rents, and control gatekeeping access. The constraint's persistence depends on their continuous operation and standard-setting.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars who trace the origin and diffusion of digital money. They identify the first_held threshold as a historical claim and observe how network effects and infrastructure asymmetries redistribute value across populations, documenting the contested readings of the kernel.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates asynchronous, long-distance value storage and transfer without physical bearer instruments, reducing geographic and temporal frictions in commerce and savings.
% TRANSFER_FUNCTION: Moves network-effect rents and liquidity premiums from late adopters and infrastructure-excluded populations to early adopters and platform providers; moves transaction fees and data value to infrastructure maintainers.
% ABSENT_VOICES: Cash-dependent communities, populations without internet access, and monetary theorists who define money by state sanction or pure conceptual possibility are underrepresented in the first_held framing; their absence makes the coordination story appear more universal than it is.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, commerce, remittances, and savings organized around digital rails would collapse toward physical cash and correspondent banking; wealth stored in non-physical instruments would face immediate convertibility crises; economic geography would rearrange around infrastructure availability.
% FOUNDING_PROBLEM: Physical money imposes high frictions on distant and asynchronous trade: transport cost, theft risk, settlement delay, and storage bulk limit the scale and speed of economic coordination.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and historians outside the early-adopter beneficiary set attest that physical-money frictions persist for the globally poor; simultaneously, central banks and infrastructure providers note that digital money has partially solved the problem while creating new exclusionary barriers. No single party outside the beneficiary set attests the problem is fully solved.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because network effects and implementation barriers create a transfer from excluded populations to early adopters and infrastructure providers. Suppression (0.58) reflects the active marginalization of cash alternatives as network effects lock in participants. Theater_ratio (0.25) is low because the coordination function is materially real, not primarily performative. Accessibility_collapse (0.70) captures how alternative monetary forms become less viable as digital infrastructure becomes the default commercial rail. Resistance (0.35) is moderate: excluded populations and cash-preferring communities resist but lack organizational power to reverse the trend. The measurement series track the maturation of enforcement infrastructure (technical standards, network scale) from early experimentation through institutionalization on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the early-adopter seat, the constraint is a rope: voluntary participation in a superior monetary technology. From the excluded-population seat, it is a snare: an enforced migration to rails they cannot access. The agenda-setter seat sees a coordination mechanism it maintains for fee revenue. The engine computes these divergent seat classifications from the same structural data; the authored claim of tangled_rope does not resolve the dispute but names the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters with access are structural beneficiaries: their directionality sits near the beneficiary end because the constraint subsidizes their holdings through network-effect appreciation and liquidity. Infrastructure-excluded populations sit near the full-target end: they bear the costs of accessibility collapse and are trapped by lack of infrastructure. Digital infrastructure providers sit at the beneficiary end as agenda-setters who collect rents, though their directionality is slightly above pure beneficiary because they also bear infrastructure maintenance costs. The engine derives this from the structural declarations: beneficiaries get low d, victims get high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents two errors: (1) treating digital money as pure coordination (rope), which would ignore the infrastructure-excluded victims and the asymmetric extraction of network-effect rents; and (2) treating it as pure extraction (snare), which would deny the genuine coordination function in cross-border, asynchronous value transfer. The 'first held' reading specifically anchors the coordination function in verifiable user practice rather than state or corporate promise, making the coordination claim falsifiable and thus structurally testable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the ''first held'' reading of digital money origin logically foreclose the ''became thinkable'' and ''regulatory recognition'' readings, or do they remain live alternatives?',
    'Historical archaeology tracing the earliest empirical instance of individual holding of non-physical monetary instruments, compared against documentary evidence of conceptual anticipation and regulatory aggregation.',
    'If first-held evidence is robust, the reading exerts upstream influence on regulatory_recognition without foreclosing it; if no isolable first-held event exists, the reading weakens and the kernel may shift toward became_thinkable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the first_held reading displaces or merely coexists with sibling readings.').

omega_variable(
    network_effect_contingency,
    'Are the implementation barriers and network effects that exclude infrastructure-poor populations intrinsic technical necessities of digital money, or are they contingent protocol choices that could be redesigned?',
    'Comparative protocol analysis and natural experiments: assessing whether low-bandwidth, offline-capable, or non-smartphone digital money designs achieve comparable coordination with lower exclusion.',
    'If barriers are technically necessary, the measured extraction is partly a coordination cost; if contingent, the constraint''s extraction is potentially reducible without loss of function, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_contingency, empirical, 'Whether exclusionary barriers are technically necessary or politically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(digi_tr_t5, digital_money_origin__first_held_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__first_held_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(digi_tr_t15, digital_money_origin__first_held_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__first_held_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(digi_tr_t25, digital_money_origin__first_held_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(digi_tr_t30, digital_money_origin__first_held_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(digi_be_t5, digital_money_origin__first_held_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__first_held_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(digi_be_t15, digital_money_origin__first_held_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__first_held_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(digi_be_t25, digital_money_origin__first_held_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(digi_be_t30, digital_money_origin__first_held_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(digi_su_t5, digital_money_origin__first_held_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__first_held_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(digi_su_t15, digital_money_origin__first_held_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__first_held_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(digi_su_t25, digital_money_origin__first_held_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(digi_su_t30, digital_money_origin__first_held_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_origin kernel. The 'first_held' reading decomposes the origin question by anchoring emergence to the material practice of individual holding, distinct from the conceptual ('became_thinkable') and state-centric ('regulatory_recognition') siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
