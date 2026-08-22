% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This constraint instantiates the non-ratifier enforcement reading of the
 *   contested UNCLOS sovereignty boundary kernel. The reading asserts that
 *   freedom of navigation is customary international law independent of
 *   UNCLOS ratification, and that naval powers (especially non-signatories
 *   like the US) can enforce this principle through naval presence and
 *   freedom-of-navigation operations against coastal states attempting EEZ
 *   exclusivity. This reading diverges sharply from the strict-EEZ reading
 *   (which treats UNCLOS Article 57 boundaries as binding and complete) and
 *   the historical-rights reading (which grounds sovereign claims in
 *   occupation and pre-treaty usage). The structural delta is that naval
 *   powers enter the beneficiary set as enforcement agents who collect
 *   strategic advantage without treaty obligation, while coastal states
 *   attempting EEZ control enter the victim set as their legal authority is
 *   undercut by the doctrine. The constraint decouples from the legal text
 *   (UNCLOS) and from treaty obligation, positioning customary law as the
 *   superior binding principle.
 *
 * KEY AGENTS:
 *   - Naval powers (non-signatories, especially US): agenda-setter and beneficiary, enforce the doctrine operationally through freedom-of-navigation operations
 *   - Coastal states (EEZ control seekers): payer and victim, attempt to enforce UNCLOS boundaries but face doctrine that delegitimizes them
 *   - Maritime commerce networks: beneficiary, gain predictable freedom of passage
 *   - Developing maritime nations: payer and victim, unable to exercise EEZ control against non-ratifier naval presence
 *   - UNCLOS signatories: observer, caught between treaty commitment and non-ratifier naval power practice
 *   - Legal traditionalists: beneficiary via doctrine authority, interpret customary law as independent source
 *   - Small island states and excluded voices: structurally excluded from the naval-power discourse, unable to enforce alternative reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '2f4530c8-9147-4e14-ad9f-f2f5a276dd48').
narrative_ontology:cs_kernel_codification('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', distributed).
narrative_ontology:cs_authority_grounding('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', extraction).
narrative_ontology:cs_reading_relation('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', foundational, customary_freedom_supersedes_treaty_codification).
narrative_ontology:cs_axiom_status(customary_freedom_supersedes_treaty_codification, holdable).
narrative_ontology:cs_axiom_grounding('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', customary_freedom_supersedes_treaty_codification, deontological).
narrative_ontology:cs_axiom('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', secondary, naval_power_enforcement_constitutes_customary_practice).
narrative_ontology:cs_axiom_status(naval_power_enforcement_constitutes_customary_practice, holdable).
narrative_ontology:cs_axiom_grounding('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', naval_power_enforcement_constitutes_customary_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', pre_unclos_freedom_of_seas_doctrine).
narrative_ontology:cs_drift_state('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', post_unclos_eez_assertion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f4530c8-9147-4e14-ad9f-f2f5a276dd48', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_signatories).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_commerce_networks).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_control_seekers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_maritime_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, legal_traditionalists_customary_law_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major naval powers (notably the US) claim that freedom of navigation in international waters and through straits is customary international law that exists independently of UNCLOS ratification. They enforce this claim through naval presence, freedom-of-navigation operations, and occasional confrontation with coastal states attempting to enforce EEZ boundaries beyond UNCLOS limits. They benefit from the ability to conduct military and intelligence operations globally without treaty constraint, and collect strategic advantage from the doctrine itself.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_signatories, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_signatories, beneficiary).

% Coastal states that have ratified UNCLOS and attempt to enforce EEZ exclusivity against non-ratifier naval powers. They bear the cost of asserting sovereignty that the non-ratifiers refuse to recognize, face diplomatic pressure and occasional operational challenges when their exclusivity claims are tested, and find their legal frameworks undermined by the customary-law doctrine that denies UNCLOS monopoly authority.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_control_seekers, payer,
    powerful, generational, constrained, regional).

% International shipping and maritime trade benefit from enforceable freedom-of-navigation principles that prevent coastal states from imposing arbitrary restrictions on sea lanes. They gain predictable passage through straits and international waters, reduced risk of unilateral exclusion, and the ability to route commerce globally without treaty-by-treaty negotiation with every coastal state.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_commerce_networks, beneficiary,
    organized, biographical, mobile, global).

% Developing states with limited naval capacity attempt to exercise EEZ control and coastal jurisdiction as their primary economic and security lever. The customary-law doctrine that overrides their UNCLOS-based claims leaves them unable to exclude foreign naval presence, unable to fully control fishing and resource extraction in their claimed waters, and bearing the diplomatic and operational cost of challenging the doctrine.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_maritime_nations, payer,
    moderate, generational, constrained, regional).

% States that have ratified UNCLOS interpret its EEZ provisions as binding customary law that supersedes pre-treaty claims. They take testimony and produce legal briefs in maritime disputes, attempt to mediate between customary-law and treaty-based claims, and face the structural tension between UNCLOS authority and non-ratifier naval enforcement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_signatories, observer,
    institutional, generational, analytical, global).

% International legal scholars and doctrinal authorities who argue that customary international law exists independently of treaty codification and that freedom of navigation is such a customary principle. They benefit from the doctrine's influence on state practice and from the scholarly authority it confers on their interpretation of customary law.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, legal_traditionalists_customary_law_adherents, beneficiary,
    analytical, civilizational, analytical, universal).

% Small island states dependent on EEZ-based fisheries and resource control are structurally excluded from the dominant naval-power framing. They would argue that UNCLOS represents hard-won sovereign gains and that customary-law overlay erodes those gains, but their voices lack the naval capacity or institutional leverage to shape the doctrine.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, excluded_voices_small_island_states, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_signatories).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global maritime regime in which major naval powers and international shipping can operate across international waters and through straits without obstruction by coastal state control, providing navigational certainty and preventing fragmentation of sea lanes.
% TRANSFER_FUNCTION: Transfers sovereignty and resource-control authority from coastal states (especially developing nations with limited naval capacity) to naval powers and maritime commerce networks. The constraint moves the effective ability to restrict passage, set conditions on transit, and exclude foreign military presence from the coastal state to the global naval power asserting customary-law freedom.
% ABSENT_VOICES: Small island states, developing maritime nations, and coastal states attempting EEZ enforcement would argue for UNCLOS treaty supremacy and reject the customary-law reading, but their voices are excluded from the dominant naval-power discourse; they lack the institutional standing and naval capacity to enforce an alternative reading.
% DISAPPEARANCE_RATIONALE: If the customary-law enforcement reading disappeared and UNCLOS treaty provisions became the sole basis for maritime authority, coastal states would gain enforceable EEZ exclusivity, sea lanes would become subject to regional coastal-state regulation, and the global maritime system would reorganize around treaty boundaries rather than customary-law freedom principles.
% FOUNDING_PROBLEM: Pre-UNCLOS maritime practice allowed major naval powers to operate globally without constraint and established freedom of the seas as a foundational principle. The founding problem the constraint solves is: how to preserve that global navigational freedom in the face of UNCLOS EEZ provisions that appear to restrict it.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers (notably the US in freedom-of-navigation operation justifications) and international legal traditionalists attest the founding problem is live and ongoing. Coastal states, UNCLOS signatories, and developing maritime nations attest the founding problem is resolved by UNCLOS and that the constraint represents an invented doctrine to preserve post-war naval hegemony; academic analysis from non-naval-power institutions and UN legal organs supports the shifted-function reading.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval (35 time units, likely decades). Early extractiveness is moderate because the constraint operates within plausible customary-law arguments and geographic scope remains contested (straits, high seas). By interval end, extractiveness is high because the doctrine has hardened into routine naval operations and the beneficiary set (naval powers) has consolidated enforcement without facing legal constraint. Theater ratio rises from 0.24 to 0.41: early operations can appeal to genuine navigational freedom rationales; by interval end, freedom-of-navigation operations increasingly serve strategic intelligence and power-projection goals while maintaining the freedom narrative. Suppression rises from 0.58 to 0.72 because coastal states and developing nations face increasing operational pressure (naval challenges to their EEZ claims) and diplomatic isolation when they attempt to enforce UNCLOS boundaries — the constraint's persistence depends on actively suppressing alternative readings. Accessibility collapse (0.58) is moderate-low because coastal states retain legal recourse through UNCLOS and international courts, but those alternatives are progressively delegitimized by the customary-law doctrine. Resistance (0.64) is substantial: coastal states, UNCLOS signatories, and developing nations mount consistent legal and diplomatic resistance, but the naval powers' operational capacity exceeds their ability to enforce countervailing rules.
 *
 * PERSPECTIVAL GAP:
 *   From the naval-power seat, the constraint is genuine coordination: it solves the founding problem (preserving navigational freedom) and benefits all participants through global maritime stability and shipping efficiency. From the coastal-state seat, the same constraint is enforced extraction: the doctrine was constructed post-UNCLOS to reverse coastal-state gains, the benefits accrue to naval powers and distant shipping interests, and the costs are borne by coastal states unable to exercise their treaty-based sovereign rights. From the maritime-commerce seat, it is coordination with slight asymmetry (they benefit from the coordination, coastal states subsidize their benefit). The engine computes these per-seat divergences from the structural data; the claimed-type field (tangled_rope) represents the author's structural judgment that the constraint exhibits both genuine coordination (navigational certainty) and asymmetric extraction (coastal-state cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers sit at the beneficiary end (d ≈ 0.15–0.25): they collect strategic advantage, set the agenda through operational practice, face no treaty constraint, and have multiple exit options (exit is not salient because they already have what they want). Coastal states attempting EEZ control sit at the target end (d ≈ 0.85–0.95): they bear the cost of delegitimized sovereignty claims, face operational pressure and diplomatic isolation, have constrained exit (cannot exit the maritime boundary system), and are structurally shaped by the constraint. Maritime commerce networks sit near neutral (d ≈ 0.35–0.45): they gain clear coordination benefit (predictable passage) and carry minimal direct cost (they already prefer freedom of passage). Developing maritime nations sit at the target end (d ≈ 0.80–0.90): they are most constrained by EEZ loss, most powerless to resist naval operations, and most dependent on their claimed EEZ for economic and security leverage. The divergence in directionality between the agenda-setter seat (naval powers) and the payer seats (coastal states) should produce different type classifications from the engine — the agenda-setter sees coordination (they organized it and maintain it), the payer sees extraction (they pay the cost and receive limited benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem-status field declares 'contested,' which is appropriate: naval powers attest the founding problem (preserving freedom of navigation) is live and ongoing, while coastal states and legal scholars attest the founding problem is solved by UNCLOS and the constraint represents an invented doctrine. The disappearance verdict is 'world_rearranges,' which identifies the constraint as active and organizationally dependent. The tangled_rope classification captures both aspects: genuine coordination (freedom of navigation solves a real collective-action problem around straits and sea lanes) and asymmetric extraction (the coastal-state cost of maintaining that coordination is unequally borne and actively enforced). Without both components, the constraint would be rope (pure coordination) or snare (pure extraction) — the classification prevents mislabeling either way.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_treaty_codification,
    'Is freedom of navigation a binding customary international law principle independent of UNCLOS ratification, or does UNCLOS Article 57 EEZ provision supersede pre-treaty custom through codification?',
    'ICJ or international tribunal ruling on a concrete maritime dispute between a non-ratifier naval power and a coastal state, or state-practice consensus shift in which non-ratifiers accept UNCLOS boundaries or ratifiers enforce them over naval challenges.',
    'If customary law is found superior, the non-ratifier enforcement reading holds and coastal-state EEZ claims are systematically undercut. If UNCLOS codification supersedes prior custom, coastal-state claims gain binding authority and naval-power enforcement becomes treaty violation. The entire constraint''s classification hinges on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_codification, conceptual, 'Whether customary law stands independent of treaty codification, or treaty text constitutes the new binding customary standard.').

omega_variable(
    enforcement_capacity_vs_legitimacy,
    'Is the non-ratifier enforcement reading sustained by genuine consensus about customary law, or by superior naval capacity that allows non-ratifiers to impose the reading against coastal-state resistance?',
    'Survey of state practice and judicial interpretation: if most courts and states affirm UNCLOS supremacy and only naval powers claim customary-law override, the reading is enforcement-capacity-driven (suppression-dependent). If courts and states broadly accept the customary-law reading across power classes, it is legitimacy-based.',
    'If enforcement-capacity-driven, the suppression metric should be higher (the constraint depends on active suppression of alternative readings), and the theater ratio should rise as the legitimacy cover erodes. If legitimacy-based, suppression should be lower and theater should decline. Current measurements show rising suppression and rising theater, suggesting enforcement-capacity-dependent operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_legitimacy, empirical, 'Whether the reading persists because of genuine legal consensus or because of superior enforcement power.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is coastal-state acceptance of non-ratifier freedom-of-navigation assertions a result of structural legal delegitimization (the customary-law doctrine is accepted as binding), or internalized deference (coastal states have learned to expect naval challenges and preemptively accommodate)?',
    'Post-doctrine-reversal trajectory: if coastal states suddenly assert EEZ enforcement after a treaty or tribunal ruling supersedes the customary-law doctrine, suppression was internalized. If they remain quiescent because they now accept the customary-law reading, suppression was structural.',
    'If internalized, the measured suppression is lower than the actual constraint''s power — coastal states carry the suppression with them even after the doctrine-enforcing incentive disappears. If structural, the measured suppression accurately reflects legal delegitimization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is internalized coastal-state deference or structural legal subordination.').

omega_variable(
    kernel_reading_distinctiveness,
    'Is the non-ratifier enforcement reading genuinely distinct from the historical-rights reading, or do they converge on the same practical outcome (naval powers override coastal-state EEZ claims)?',
    'Hypothetical test: in a dispute over an ancient historical claim (e.g., Ottoman occupation) vs. a freedom-of-navigation assertion by a non-ratifier, which reading prevails in state practice? If naval powers invoke historical-rights arguments when customary-law fails, the readings are tactical variants. If they remain distinct in operation, they are genuine alternatives.',
    'If readings converge, the kernel may be more accurately framed as ''pre-UNCLOS naval-power dominance'' rather than distinguishing customary-law from historical-rights grounds. If distinct, each reading represents a coherent legal position with different vulnerability to challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctiveness, conceptual, 'Whether the non-ratifier enforcement reading is structurally distinct from historical-rights claims or a functional equivalent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(uncl_tr_t20, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(uncl_tr_t25, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).
narrative_ontology:measurement(uncl_tr_t35, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(uncl_be_t20, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(uncl_be_t25, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(uncl_be_t30, observed).
narrative_ontology:measurement(uncl_be_t35, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(uncl_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(uncl_su_t20, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(uncl_su_t25, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(uncl_su_t30, observed).
narrative_ontology:measurement(uncl_su_t35, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(uncl_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% The unclos_sovereignty_boundary kernel decomposes into three constraint stories, one per reading. Each reading assigns different ε values to the same geographic/legal phenomenon (maritime authority in EEZ zones) because each reading defines the constraint differently: the non-ratifier reading treats the constraint as naval-power enforcement of customary freedom; the strict-EEZ reading treats it as coastal-state authority per UNCLOS; the historical-rights reading treats it as pre-treaty occupation claims. These are structurally distinct constraints with different beneficiary/victim sets, different power distributions, and different persistence mechanisms. They are linked as a constraint family via network.affects_constraints: the non-ratifier reading directly opposes the strict-EEZ reading (forecloses per cs_structure), coexists with the historical-rights reading (both serve to override UNCLOS), and influences downstream constraints around maritime access and resource control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
