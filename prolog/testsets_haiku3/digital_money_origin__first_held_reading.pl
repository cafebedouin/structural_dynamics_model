% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the first-held reading of digital money's origin:
 *   the constraint comes into existence when individuals begin holding
 *   non-physical monetary instruments as practical stores of value, not when
 *   the concept becomes conceivable or when regulators recognize it. This
 *   reading places digital money's origin in the mid-1990s (early e-commerce
 *   payment systems) to early-2000s (PayPal, digital wallets), later than the
 *   technological concept (which existed in cryptographic theory) and earlier
 *   than regulatory incorporation (which began in the 2010s). The structural
 *   consequence: the constraint is defined by early adopters with
 *   infrastructure access, benefits accrue to those positioned first, and
 *   extraction increases as the system becomes mandatory for participation.
 *   Victims include unbanked populations, low-infrastructure jurisdictions,
 *   and those without early-access timing. This reading competes with two
 *   sibling readings: the became-thinkable reading (which places origin
 *   earlier, in the theory/concept phase) and the regulatory-recognition
 *   reading (which places origin later, when central banks incorporate
 *   digital money into monetary aggregates).
 *
 * KEY AGENTS:
 *   - Early adopters with infrastructure: gain liquidity advantages and network-effects positioning
 *   - Technology platform operators: set technical standards, control network topology, extract transaction-flow rents
 *   - Unbanked populations: excluded from the constraint's coordinating structure, face eventual adoption pressure without access
 *   - Low-infrastructure jurisdictions: forced to accept technical standards they did not author, lose monetary policy flexibility
 *   - Late adopters without access: identity-locked into eventual adoption at disadvantaged terms
 *   - Traditional financial institutions: constrained to adapt to standards set by technology operators
 *   - Regulatory authorities: observe and respond to implementation rather than shaping it (this reading positions them late)
 *   - Central banks: excluded from defining first-held standards, lose control of monetary aggregates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.41).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin: First Held Reading").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '3546ffaf-451d-4f88-b8a1-10899f146a14').
narrative_ontology:cs_kernel_codification('3546ffaf-451d-4f88-b8a1-10899f146a14', distributed).
narrative_ontology:cs_authority_grounding('3546ffaf-451d-4f88-b8a1-10899f146a14', distributed).
narrative_ontology:cs_reading_relation('3546ffaf-451d-4f88-b8a1-10899f146a14', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('3546ffaf-451d-4f88-b8a1-10899f146a14', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('3546ffaf-451d-4f88-b8a1-10899f146a14', foundational, practical_holding_defines_emergence).
narrative_ontology:cs_axiom_status(practical_holding_defines_emergence, holdable).
narrative_ontology:cs_axiom_grounding('3546ffaf-451d-4f88-b8a1-10899f146a14', practical_holding_defines_emergence, conventional).
narrative_ontology:cs_axiom('3546ffaf-451d-4f88-b8a1-10899f146a14', secondary, infrastructure_access_gates_participation).
narrative_ontology:cs_axiom_status(infrastructure_access_gates_participation, holdable).
narrative_ontology:cs_axiom_grounding('3546ffaf-451d-4f88-b8a1-10899f146a14', infrastructure_access_gates_participation, empirically_contingent).
narrative_ontology:cs_reference_frame('3546ffaf-451d-4f88-b8a1-10899f146a14', pre_digital_monetary_holding).
narrative_ontology:cs_drift_state('3546ffaf-451d-4f88-b8a1-10899f146a14', post_platform_infrastructure_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3546ffaf-451d-4f88-b8a1-10899f146a14', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_infrastructure).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, technology_platform_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, low_infrastructure_jurisdictions).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, late_adopters_without_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, traditional_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to borderless value transfer, reduced transaction friction, and early positioning in network effects. Typically possess prior banking infrastructure, internet literacy, and devices. Can exit to traditional currency without significant loss if digital money fails, but benefit from adoption liquidity.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_infrastructure, beneficiary,
    moderate, biographical, arbitrage, global).

% Set technical standards, control network topology, collect transaction fees or data streams, and define what counts as 'practical' digital money through implementation choices. Build and enforce the infrastructure others depend on. Primary extractors from the constraint: control payment flows, network effects, and the definition of functionality itself.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technology_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, technology_platform_operators, beneficiary).

% Lack the devices, literacy, or connectivity to participate in digital money systems. Excluded from the constraint's benefiting structure, yet bear the cost of ecosystem consolidation and the eventual pressure to migrate to digital. No meaningful exit option: as digital becomes the practical standard, exclusion becomes mandatory participation pressure without access.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, unbanked_populations, excluded).

% Face infrastructure requirements and technical dependencies imposed by platforms headquartered elsewhere. Cannot implement digital money systems without foreign tech operators, lose monetary policy flexibility, and face regulatory pressure to accept standards they did not author. Must absorb the costs of financial system remodeling.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, low_infrastructure_jurisdictions, payer,
    powerful, generational, constrained, continental).

% Face increasing pressure to adopt as digital systems become the practical default, but lack the prior access or timing advantage early adopters enjoyed. Pay through slower entry, worse network positioning, and acceptance of terms set by those who moved first. Identity-locked: cannot credibly opt out once digital money is the institutional standard without social and economic penalty.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, late_adopters_without_access, payer,
    moderate, biographical, identity_locked, national).

% See their role mediated or displaced by platform operators who set the technical standards digital money runs on. Must either adopt standards they do not control or face irrelevance. Constrained exit: they cannot reject digital money without losing market share, but adapting means accepting terms from technology platforms.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, payer,
    powerful, generational, constrained, global).

% Attempt to detect and regulate digital money systems but respond reactively to technological implementation rather than shaping it. This reading places regulatory recognition much later, so authorities here observe the constraint's operation but do not define it — that definition power rests with platform operators who implement first.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Lose control of monetary aggregates and transmission mechanisms to systems they did not build and cannot unilaterally alter. Excluded from setting the first-held standards (per this reading's framing: regulators come later). Trapped: they must participate in systems they did not design or lose economic oversight.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, central_banks, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, technology_platform_operators).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a technology-mediated store of value that reduces transaction friction for transfers across geography and institution boundaries; enables peer-to-peer value transfer without institutional intermediation.
% TRANSFER_FUNCTION: Moves control of monetary intermediation from institutional (banking) operators to technology platform operators; transfers transaction fees, data flows, and network-effects rents from traditional finance to platform ecosystems; redistributes access advantages to those with early infrastructure positioning.
% ABSENT_VOICES: Unbanked populations, low-infrastructure jurisdictions, and late-adopters are structurally excluded from the constraint's definition (by definition: digital money is what is first HELD, not what becomes available to all). Central banks that would resist the loss of monetary policy instruments are not consulted in the first-held definition. Traditional financial institutions excluded from setting technical standards.
% DISAPPEARANCE_RATIONALE: If the constraint (digital money as practical held instrument) vanished — if the technical implementations failed or were rejected — platform operators would lose the revenue and control streams that digital money enabled, early adopters would lose liquidity and transfer convenience, and low-infrastructure populations would face renewed pressure to build parallel physical payment systems. The financial ecosystem would reorganize around surviving institutional structures.
% FOUNDING_PROBLEM: Physical currency is slow to transfer across distance and borders; banking intermediation adds friction, cost, and institutional gatekeeping; individuals lack direct control over value transfer. Digital representation of currency solves speed and gatekeeping costs for those with device and network access.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and technology platform operators attest the founding problem remains live and digital money solves it. Unbanked populations, low-infrastructure jurisdictions, and late-adopters attest the founding problem was solved ONLY for the connected and early-positioned; for them the problem persists and the constraint imposes its costs without solving the underlying issue. Academic and regulatory analysis from outside the benefiting parties supports the contested framing: the founding problem is real but the constraint's solution is asymmetric in access.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.62 at interval end) is substantial because the constraint concentrates benefits to early adopters and platform operators while imposing costs on unbanked and late-adopter populations. The extraction increases monotonically from 1995 (0.15, largely theoretical) to 2025 (0.62, implementation entrenched) as network effects entrench platform control and digital becomes the practical default for value transfer. Suppression (0.41) is moderate because the constraint operates through infrastructure barriers and network effects rather than direct coercion — those without devices or connectivity are suppressed structurally, not through active enforcement of explicit rules, though platform operators do enforce payment-flow restrictions and interoperability barriers. Theater ratio (0.18, low) reflects that the coordination function is genuine (reducing transfer friction) but a growing share of platform activity is pure rent extraction (transaction fees, data collection, network-lock-in) rather than coordination cost. The measurement series share one time grid covering 30 years: from theoretical concept (1995) through practical implementation (2002 onward) to entrenched system (2025).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (unbanked, low-infrastructure, late-adopters) and the beneficiary/agenda-setter seats (early adopters, platform operators) should compute as fundamentally different types. From the platform operator's analytical position, digital money is rope (coordination that has genuine efficiency gains). From the unbanked position, it is snare (extraction with no coordination benefit and structural exclusion). The engine computes per-seat; the authored metrics capture the constraint as experienced by those excluded and harmed.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators have d near 1.0 (full targets of external pressure, but they are agenda-setters and primary extractors, so directionality is complex — use override if needed to capture that they set the constraint but are not trapped by it). Early adopters have d around 0.3-0.4 (beneficiaries, with arbitrage exit options allowing them to exit to traditional currency if digital money loses value). Unbanked populations have d near 1.0 (full targets, trapped by infrastructure barriers they cannot overcome). Low-infrastructure jurisdictions have d around 0.7-0.8 (victims forced to accept technical standards, constrained exit). Late adopters without access have d around 0.6-0.7 (payers who are identity-locked into adoption pressure). Central banks and traditional finance have d around 0.65 (excluded from first-held definition, forced to adapt after the fact).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (physical currency is slow and gatekept) was genuine in 1995 and remains somewhat live. However, the constraint's evolution shows mandatrophy signals: by 2025, the constraint is increasingly about extracting rents from mandatory participation (high extractiveness, rising theater) rather than solving the founding problem for all. Late-adopters and unbanked populations bear the costs of a mandatory system without gaining its coordination benefits. The coordination function is real but is increasingly separable from the extraction function — digital money could solve transfer friction without the rent extraction and infrastructure gatekeeping platforms impose. This is a tangled rope with mandatrophy momentum: the coordination justification persists while the extraction function grows and becomes harder to unwind.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_vs_theoretical_boundary,
    'What counts as ''practical'' holding of digital money? Does it require mainstream adoption, institutional integration, or merely technical feasibility?',
    'Definitional archaeology: trace which digital instruments were first practically held (PayPal accounts, mobile money in Kenya, e-cash protocols, Bitcoin) and establish when ''practical'' status was achieved for each.',
    'If ''practical'' requires mainstream adoption, the origin date shifts later (2010s). If it requires only technical feasibility, the origin date shifts earlier (1980s cryptography). This directly determines the set of beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_vs_theoretical_boundary, conceptual, 'The boundary between theoretical digital money and practically-held digital money.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Do the three readings of the digital money kernel logically foreclose each other, or do they coexist as different framings held by different institutional actors?',
    'Examine whether regulatory authorities, technology operators, and academic historians treat the three origin definitions as contradictory or as complementary framings of different aspects of the same phenomenon.',
    'If they foreclose each other, only one can be true of the kernel; if they coexist, multiple readings remain live even after empirical resolution. This determines whether the three constraint stories compete or coordinate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether the three digital-money-origin readings foreclose each other or coexist.').

omega_variable(
    infrastructure_access_as_suppression_vs_structural_barrier,
    'Is the exclusion of unbanked and low-infrastructure populations from digital money a result of active suppression (platforms deliberately excluding them) or structural inability (they lack the prerequisites that digital money requires)?',
    'Post-access experiment: provide infrastructure and literacy to currently-excluded populations; if they rapidly adopt and gain expected benefits, the barrier was primarily structural. If adoption remains slow despite access, suppression or other cultural factors are present.',
    'If suppression: the constraint''s type tilts more toward snare (active exclusion machinery). If structural: the constraint is tangled rope with an equity-access problem that might be solved by redistribution rather than constraint removal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_access_as_suppression_vs_structural_barrier, empirical, 'Whether exclusion from digital money is active suppression or structural inability.').

omega_variable(
    kernel_reading_identity,
    'Is this reading (first_held) a distinct reading of the digital_money_origin kernel, or does it collapse into one of the sibling readings under scrutiny?',
    'If the first-held date and the became-thinkable date are discovered to be the same (e.g., individuals first held digital money immediately when it became theoretically possible), the readings merge. If first-held and regulatory-recognition dates cluster, different reading.',
    'Merger into a sibling reading would collapse this constraint story into a duplicate; persistence as distinct requires that the first-held date is empirically distinct from both siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether first_held_reading is a distinct kernel reading or merges with a sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__first_held_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement_basis(digi_tr_t1995, projected).
narrative_ontology:measurement(digi_tr_t2002, digital_money_origin__first_held_reading, theater_ratio, 2002, 0.08).
narrative_ontology:measurement_basis(digi_tr_t2002, observed).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement_basis(digi_tr_t2010, observed).
narrative_ontology:measurement(digi_tr_t2017, digital_money_origin__first_held_reading, theater_ratio, 2017, 0.16).
narrative_ontology:measurement_basis(digi_tr_t2017, observed).
narrative_ontology:measurement(digi_tr_t2021, digital_money_origin__first_held_reading, theater_ratio, 2021, 0.17).
narrative_ontology:measurement_basis(digi_tr_t2021, observed).
narrative_ontology:measurement(digi_tr_t2025, digital_money_origin__first_held_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(digi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__first_held_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement_basis(digi_be_t1995, projected).
narrative_ontology:measurement(digi_be_t2002, digital_money_origin__first_held_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement_basis(digi_be_t2002, observed).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(digi_be_t2010, observed).
narrative_ontology:measurement(digi_be_t2017, digital_money_origin__first_held_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement_basis(digi_be_t2017, observed).
narrative_ontology:measurement(digi_be_t2021, digital_money_origin__first_held_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement_basis(digi_be_t2021, observed).
narrative_ontology:measurement(digi_be_t2025, digital_money_origin__first_held_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(digi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__first_held_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement_basis(digi_su_t1995, projected).
narrative_ontology:measurement(digi_su_t2002, digital_money_origin__first_held_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement_basis(digi_su_t2002, observed).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement_basis(digi_su_t2010, observed).
narrative_ontology:measurement(digi_su_t2017, digital_money_origin__first_held_reading, suppression_requirement, 2017, 0.39).
narrative_ontology:measurement_basis(digi_su_t2017, observed).
narrative_ontology:measurement(digi_su_t2021, digital_money_origin__first_held_reading, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement_basis(digi_su_t2021, observed).
narrative_ontology:measurement(digi_su_t2025, digital_money_origin__first_held_reading, suppression_requirement, 2025, 0.41).
narrative_ontology:measurement_basis(digi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories instantiate three readings of the digital_money_origin kernel. The first-held reading places origin when individuals began practically holding digital instruments (mid-1990s–2000s), with structural consequences for beneficiary/victim asymmetry and infrastructure-mediated extraction. The became-thinkable reading places origin earlier, in theoretical conceivability. The regulatory-recognition reading places origin later, when central banks formally incorporated digital money. These are not three measurements of one constraint; they are three distinct constraints with different ε values, victim sets, and enforcement mechanisms. They share the same kernel (the contested claim about when digital money 'emerged') but produce different constraint classifications. The first-held reading is influenced downstream by regulatory recognition (regulators eventually codify terms platforms set first) and influences the became-thinkable reading by providing implementation evidence for what was theoretically possible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
