% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the 'non-ratifier enforcement reading' of
 *   the UNCLOS sovereignty boundary kernel. The reading asserts that freedom
 *   of navigation (FON) principles exist as customary international law
 *   independent of UNCLOS ratification, and are enforceable through naval
 *   presence — specifically Freedom of Navigation Operations (FONOPs). The
 *   United States, which has not ratified UNCLOS, is the primary author and
 *   enforcer of this reading. Other naval powers benefit from the customary
 *   law framework whether or not they ratified. Coastal states asserting
 *   exclusive EEZ jurisdiction (particularly China's nine-dash line, Brazil's
 *   Blue Amazon, Canada's Arctic claims) are the structural targets. The
 *   constraint decouples from the UNCLOS text: the legal authority claimed is
 *   customary law formed through state practice (naval operations) and opinio
 *   juris (legal justifications), not treaty obligation. This creates an
 *   asymmetric extraction structure: naval powers enforce rules they are not
 *   bound by, while coastal states bear the costs of restricted sovereignty.
 *
 * KEY AGENTS:
 *   - us_navy: Primary agenda setter (powerful/mobile) — conducts FONOPs, sets enforcement tempo, collects strategic mobility
 *   - other_naval_powers: Beneficiaries (powerful/mobile) — gain legal cover for naval mobility without leading enforcement
 *   - flag_states_shipping: Beneficiaries (organized/mobile) — commercial registries gain predictable transit rights
 *   - coastal_states_eez_claimants: Payers (organized/constrained) — lose exclusive regulatory control, face naval challenges
 *   - developing_coastal_states: Payers (moderate/constrained) — disproportionately affected, lack enforcement capacity
 *   - international_legal_scholars: Observers (analytical/analytical) — analyze customary law formation, no enforcement role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.55).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'bf3b523d-a29d-40f7-8b21-e97de2601976').
narrative_ontology:cs_kernel_codification('bf3b523d-a29d-40f7-8b21-e97de2601976', fixed_text).
narrative_ontology:cs_authority_grounding('bf3b523d-a29d-40f7-8b21-e97de2601976', practice).
narrative_ontology:cs_interpretation_layer_present('bf3b523d-a29d-40f7-8b21-e97de2601976').
narrative_ontology:cs_reading_relation('bf3b523d-a29d-40f7-8b21-e97de2601976', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf3b523d-a29d-40f7-8b21-e97de2601976', unclos_sovereignty_boundary__strict_eez_reading, influences).
narrative_ontology:cs_axiom('bf3b523d-a29d-40f7-8b21-e97de2601976', foundational, customary_law_independent_of_treaty).
narrative_ontology:cs_axiom_status(customary_law_independent_of_treaty, holdable).
narrative_ontology:cs_axiom_grounding('bf3b523d-a29d-40f7-8b21-e97de2601976', customary_law_independent_of_treaty, conventional).
narrative_ontology:cs_axiom('bf3b523d-a29d-40f7-8b21-e97de2601976', foundational, naval_presence_creates_law).
narrative_ontology:cs_axiom_status(naval_presence_creates_law, holdable).
narrative_ontology:cs_axiom_grounding('bf3b523d-a29d-40f7-8b21-e97de2601976', naval_presence_creates_law, instrumental).
narrative_ontology:cs_reference_frame('bf3b523d-a29d-40f7-8b21-e97de2601976', customary_fon_baseline).
narrative_ontology:cs_drift_state('bf3b523d-a29d-40f7-8b21-e97de2601976', contemporary_great_power_competition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf3b523d-a29d-40f7-8b21-e97de2601976', '2026-07-28T14:30:00Z').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, us_navy).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, other_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, flag_states_shipping).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_coastal_states).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_international_law_fon).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_presence_as_law_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducts Freedom of Navigation Operations (FONOPs) worldwide asserting customary law rights without UNCLOS ratification. Sets the operational tempo and geographic focus of enforcement. Collects strategic mobility and legal precedent as benefits. Could ratify UNCLOS but chooses not to, preserving unilateral enforcement freedom.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, us_navy, agenda_setter,
    powerful, biographical, mobile, global).

% Major naval powers (UK, France, Japan, Australia, India) benefit from the customary law framework whether or not they ratified UNCLOS. They participate in joint FONOPs and gain legal cover for their own naval mobility. Their benefit is derivative of the US-led enforcement structure.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, other_naval_powers, beneficiary,
    powerful, biographical, mobile, global).

% Commercial shipping registries (Panama, Liberia, Marshall Islands, etc.) gain predictable transit rights through contested waters. Their vessels operate under naval umbrella without bearing enforcement costs. Benefit is diffuse across global trade but concentrated in major flag states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, flag_states_shipping, beneficiary,
    organized, biographical, mobile, global).

% States asserting exclusive resource rights and regulatory jurisdiction in EEZs (China, Brazil, Indonesia, Canada, etc.) face naval challenges to their claims. Must either acquiesce to foreign naval presence or invest in costly anti-access/area-denial capabilities. Exit from the constraint requires either naval parity or legal acquiescence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants, payer,
    organized, generational, constrained, regional).

% Smaller coastal states lack naval capacity to resist FONOPs or enforce EEZ claims against major powers. Bear disproportionate cost: their resource sovereignty is degraded while they lack the enforcement leverage of powerful states. Dependent on multilateral institutions (ITLOS, ISA) that are themselves contested by the non-ratifier reading.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_coastal_states, payer,
    moderate, generational, constrained, regional).

% Academic and judicial observers (ITLOS judges, ICJ, ILC members, law faculty) analyze customary law formation, state practice, and opinio juris. They do not collect rents or bear enforcement costs. Their analyses are cited by all sides but cannot compel compliance.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Global maritime trade and naval mobility require predictable navigation rights across 70% of Earth's surface. Customary international law provides a baseline framework independent of treaty ratification, allowing states to navigate, overfly, and conduct military activities without negotiating bilateral agreements for every transit.
% TRANSFER_FUNCTION: Transfers enforcement authority and legal interpretation power from coastal states (who would control EEZ access under strict UNCLOS reading) to naval powers (who define customary law through state practice). Coastal states lose exclusive regulatory control; naval powers gain unilateral enforcement rights without reciprocal treaty obligations. Flag states gain transit predictability as a secondary transfer.
% ABSENT_VOICES: Small island developing states (Pacific, Caribbean) whose EEZs constitute vast ocean areas but lack enforcement capacity; landlocked states dependent on transit rights through neighboring EEZs; indigenous coastal communities with traditional marine resource claims; environmental NGOs concerned with naval activity impacts on marine ecosystems.
% DISAPPEARANCE_RATIONALE: If the non-ratifier enforcement reading vanished overnight, the US would lose its primary legal basis for FONOPs without ratifying UNCLOS. Coastal states would assert exclusive EEZ control unchallenged. Global shipping would face a patchwork of national regulations requiring bilateral transit agreements. The current customary law baseline would fragment into regional regimes.
% FOUNDING_PROBLEM: Cold War superpower competition required guaranteed naval mobility for nuclear deterrence (SSBN bastions, carrier battle groups) and power projection without treaty constraints that could be vetoed or interpreted adversarially. The 1982 UNCLOS negotiations produced a treaty the US refused to ratify (Part XI deep seabed provisions), creating a gap filled by customary law assertions.
% FOUNDING_PROBLEM_CORROBORATION: Naval historians (Hattendorf, Till) and Cold War strategists outside the beneficiary set document the strategic mobility imperative. International lawyers (Oxman, Rothwell) note the customary law argument was developed post-UNCLOS to justify non-ratification. The US Navy's own FONOP program records (1979-present) show operational continuity independent of treaty status.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the asymmetric structure: naval powers extract enforcement rights and legal interpretation authority without reciprocal treaty obligations. The US gains unilateral FONOP authority; other naval powers free-ride. Suppression (0.55) is moderate — coastal states are not prevented from making claims, but their claims are actively challenged by naval presence, raising the cost of enforcement. Theater ratio (0.40) captures the performative dimension: 'customary law' is invoked as legal cover for what is fundamentally naval power projection, but the coordination function (predictable navigation for global trade) is genuine. Accessibility collapse (0.60) reflects that alternatives (bilateral agreements, UNCLOS dispute settlement, regional regimes) exist but are structurally disadvantaged by the naval enforcement baseline. Resistance (0.60) is significant: coastal states invest in A2/AD capabilities, lawfare, and multilateral institution-building to counter the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the US Navy's seat, this is a Rope: genuine coordination solving the collective action problem of global maritime mobility, maintained through state practice. From coastal EEZ claimants' seats, this is a Snare: the customary law narrative is cover for unilateral naval dominance, suppression of sovereign rights, and extraction of enforcement authority. The engine will compute this seat divergence from the structural data — the claimed type (tangled_rope) acknowledges both dimensions exist simultaneously. The reading_relations in cs_structure capture how this reading structurally pressures the strict_eez_reading without logically foreclosing it.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Navy sits at the beneficiary pole (d ≈ 0.15): it authors the constraint, controls enforcement, and collects strategic benefits. Other naval powers are secondary beneficiaries (d ≈ 0.25): they benefit but do not set the agenda. Flag states are diffuse beneficiaries (d ≈ 0.40): they gain transit predictability but have no enforcement role. Coastal EEZ claimants are targets (d ≈ 0.75): they bear the costs of restricted sovereignty and must invest in counter-capabilities. Developing coastal states are deeper targets (d ≈ 0.85): same structural position with fewer exit options. Legal scholars are analytical observers (d = 0.5): they analyze but do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War strategic mobility) is contested: the Cold War ended but the enforcement structure persists and expanded. The constraint has not resolved its mandatrophy — it has mutated from superpower deterrence to great power competition. The 'customary law' framing prevents obsolescence by detaching from the original strategic rationale. The tangent between coordination (global trade needs predictable rules) and extraction (naval powers write rules they don't follow) is the engine of persistence. No party benefits enough to maintain the status quo unilaterally (US bears FONOP costs) and no party is hurt enough to force revision (coastal states lack naval parity). This is the tangled_rope dynamic: coordination function genuine, extraction asymmetric, active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_formation_mechanism,
    'Does the non-ratifier enforcement reading''s claim to customary law status reflect genuine opinio juris and state practice, or is ''customary law'' a post-hoc legal rationalization for naval power projection?',
    'Comparative analysis of state practice: if non-navial states consistently protest FONOPs and deny customary law status, the claim fails the opinio juris test. If protests are selective or absent, customary law formation is more credible. ICJ/ITLOS jurisprudence on customary law formation (Nicaragua, North Sea Continental Shelf) provides legal benchmarks.',
    'If customary law claim fails, the constraint is pure extraction (snare) — naval power masquerading as law. If it holds, the coordination function has independent legal legitimacy, supporting tangled_rope classification. Affects whether the constraint can be reformed through legal processes or only through power shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_formation_mechanism, conceptual, 'Whether the customary law foundation is genuine or instrumental').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does this reading represent a genuine alternative interpretation of the UNCLOS sovereignty boundary, or is it a structural adaptation that preserves US naval freedom of action post-UNCLOS-rejection?',
    'Historical tracing: if the customary law argument was developed *before* US UNCLOS rejection (1982), it is an independent legal position. If developed *after* rejection as justification, it is adaptive rationalization. Archival research on US State Department and Navy legal advisers'' memoranda 1975-1985.',
    'If adaptive, the reading''s legitimacy is historically contingent and its persistence reflects power, not legal coherence. If independent, it represents a stable interpretive tradition that would persist even if US ratified UNCLOS. Bears on whether the constraint is a piton (inertial) or tangled_rope (actively maintained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, empirical, 'Historical genesis of the non-ratifier enforcement reading').

omega_variable(
    enforcement_coordination_boundary,
    'At what point does naval enforcement of customary FON become extraction rather than coordination? The reading claims both functions are inseparable; critics argue they are structurally distinct.',
    'Functional decomposition: identify which FONOPs protect genuine global commons navigation (straits, archipelagic sea lanes) versus which challenge specific coastal state resource claims (EEZ military surveys, artificial island status). Measure the ratio over time. If the latter dominates and grows, extraction is the primary function.',
    'If inseparable, the tangled_rope classification holds — coordination and extraction are fused. If separable, the constraint decomposes into a rope (genuine FON coordination) and a snare (targeted EEZ challenges), per ε-invariance principle. Would require splitting this story into two constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coordination_boundary, conceptual, 'Whether coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_non_ratifier_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(unclos_non_ratifier_tr_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement(unclos_non_ratifier_tr_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(unclos_non_ratifier_tr_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(unclos_non_ratifier_tr_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(unclos_non_ratifier_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unclos_non_ratifier_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(unclos_non_ratifier_be_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement(unclos_non_ratifier_be_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(unclos_non_ratifier_be_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(unclos_non_ratifier_be_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(unclos_non_ratifier_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(unclos_non_ratifier_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(unclos_non_ratifier_su_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1994, 0.45).
narrative_ontology:measurement(unclos_non_ratifier_su_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(unclos_non_ratifier_su_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(unclos_non_ratifier_su_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(unclos_non_ratifier_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unclos_sovereignty_boundary kernel. The strict_eez_reading treats UNCLOS Article 57 as exclusive and exhaustive; the historical_rights_reading claims pre-UNCLOS sovereign rights; this reading asserts customary law independence from treaty ratification enforced by naval power. The three readings form a constraint family linked by affects_constraints. The non_ratifier_enforcement_reading influences the strict_eez_reading by demonstrating an alternative enforcement baseline that undermines UNCLOS exclusivity claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, organized, 0.75).
constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
