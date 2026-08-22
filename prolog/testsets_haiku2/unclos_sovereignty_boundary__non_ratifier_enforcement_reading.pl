% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_non_ratifier_enforcement, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation Enforcement via Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested UNCLOS
 *   sovereignty-boundary kernel. The kernel is the underspecified commitment
 *   (UNCLOS Article 57 EEZ boundaries + historical-use and
 *   freedom-of-navigation customary doctrines) that permits multiple
 *   readings. This reading asserts that freedom of navigation is customary
 *   international law binding even on non-UNCLOS ratifiers, enforceable via
 *   naval presence without need for treaty ratification or dispute
 *   resolution. Under this reading, naval powers enter the beneficiary set
 *   (they enforce rules without accepting treaty constraints), and coastal
 *   states enter the victim set (they are prevented from enforcing EEZ
 *   exclusivity). The constraint's persistence depends on active naval
 *   enforcement, not on legal consensus or treaty interpretation. The reading
 *   decouples from the written text (UNCLOS Article 57) by asserting
 *   pre-textual customary doctrine as the operative rule.
 *
 * KEY AGENTS:
 *   - Naval powers (institutional beneficiary + agenda-setter): enforce freedom-of-navigation doctrine through military presence, framing enforcement as law rather than power assertion.
 *   - Coastal states attempting EEZ exclusivity (moderate institutional payers): claim authority under UNCLOS Article 57 but face routine challenge from naval powers invoking customary-law doctrine.
 *   - Developing maritime nations (powerless payers, trapped exit): coast-dependent economies unable to mount naval opposition, facing loss of EEZ resource-monopoly rent.
 *   - Non-ratifying powers (powerful beneficiaries): claim freedom-of-navigation rights without accepting UNCLOS obligations, inverting reciprocity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation Enforcement via Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0').
narrative_ontology:cs_kernel_codification('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', fixed_text).
narrative_ontology:cs_authority_grounding('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', extraction).
narrative_ontology:cs_interpretation_layer_present('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0').
narrative_ontology:cs_reading_relation('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', foundational, customary_law_independence_from_unclos).
narrative_ontology:cs_axiom_status(customary_law_independence_from_unclos, holdable).
narrative_ontology:cs_axiom_grounding('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', customary_law_independence_from_unclos, conventional).
narrative_ontology:cs_axiom('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', foundational, naval_enforcement_legitimacy_without_ratification).
narrative_ontology:cs_axiom_status(naval_enforcement_legitimacy_without_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', naval_enforcement_legitimacy_without_ratification, instrumental).
narrative_ontology:cs_reference_frame('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', customary_freedom_of_navigation_doctrine).
narrative_ontology:cs_drift_state('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', contemporary_naval_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8b5772b-6b2d-444e-a92f-2d4f7f75e3b0', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_commerce_interests).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_exclusive_claims).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_maritime_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce freedom-of-navigation doctrine by deploying naval assets to conduct transits through claimed EEZs, challenge coastal-state restrictions, and conduct military exercises in international waters. They frame enforcement as law-preservation (customary doctrine) rather than power assertion. Control the verification and enforcement machinery directly through naval presence. Maintain strategic flexibility by invoking freedom-of-navigation doctrine for their own transit rights while invoking EEZ exclusivity in their own claimed waters when it suits them.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Claim 200-nautical-mile EEZ boundaries under UNCLOS Article 57 with intent to regulate military activities, control resource extraction, and collect licensing revenue. Face routine naval transits by distant powers asserting freedom-of-navigation doctrine independent of their treaty consent. Cannot mount effective naval opposition and lack access to expedited dispute resolution when their enforcement attempts are challenged. Must absorb loss of resource-monopoly rent and strategic exclusivity.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_exclusive_claims, payer,
    moderate, generational, constrained, global).

% Benefit from guaranteed transit rights through EZs without negotiating resource agreements or paying licensing fees to coastal states. Reduced shipping costs when routes are enforced as open corridors. Lobby governments to maintain naval enforcement of freedom-of-navigation doctrine to preserve access. Experience indirect coordination benefit, though the rule's asymmetry favors them relative to coastal-state resource-collection interests.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_commerce_interests, beneficiary,
    organized, biographical, mobile, global).

% Coast-dependent economies attempting to monetize EEZ resources (fisheries, minerals, petroleum) and enforce exclusivity to fund coastal development. Naval presence from distant powers enforces open transit and undermines their ability to collect rents from resource access. Cannot mount naval opposition and lack institutional capacity to initiate UNCLOS dispute resolution. Trapped between accepting open access (loss of potential revenue) or attempting military confrontation (certain military defeat).
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, developing_maritime_nations, payer,
    powerless, biographical, trapped, global).

% Claim freedom-of-navigation rights to conduct transits and strategic operations through others' EZs without accepting UNCLOS Article 57 EEZ boundaries as binding on themselves. Invoke customary-law doctrine rather than treaty interpretation, preserving strategic flexibility. Can enforce rules they do not contractually accept, inverting the reciprocal structure of treaty law. Maintain arbitrage by invoking EEZ exclusivity when convenient in their own waters while enforcing freedom of navigation elsewhere.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_powers, agenda_setter).

% Ratified UNCLOS and accepted its EEZ provisions, but witness the assertion that freedom of navigation principles bind even non-signatories via customary-law doctrine. Their legal framework is undermined by the reading's claim that treaty boundaries are overlaid by pre-existing customary rights. Face the same naval powers conducting transits through their EZs while invoking customary law rather than treaty interpretation. Constrained by treaty obligations (must accept the EEZ regime) while lacking enforcement capacity to defend their zones against non-ratifier assertions.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_signatory_states, observer,
    institutional, generational, constrained, global).

% Scholarship bodies contest whether freedom-of-navigation doctrine is customary-law independent or treaty-modified by UNCLOS. One school (supporting this reading) argues customary law persists independent of treaty. Another school argues UNCLOS codified and modified customary law, making treaty membership operative. The reading excludes the countervailing scholarship by treating the dispute as settled doctrine rather than live contest. This exclusion is structural: if the doctrine is genuinely settled, countervailing scholarship is outdated; if contested, exclusion is doctrinal suppression.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_law_scholarship, excluded,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes predictable transit rules through maritime zones beyond territorial seas: ensures global trade routes and military passages are not subject to varying coastal-state closure doctrines. Reduces negotiation friction for maritime commerce and enables strategic mobility for naval powers. Provides a standing rule for handling dispute instances (transit challenges) without case-by-case negotiation.
% TRANSFER_FUNCTION: Transfers de facto resource-access monopoly from coastal states to naval powers and maritime commerce: coastal states lose the ability to collect licensing revenue on EEZ resource extraction and military-activity overflights; maritime commerce avoids licensing fees and negotiation costs; naval powers gain unimpeded strategic access to ocean zones globally. The transfer is embedded in doctrine-assertion and naval enforcement rather than explicit contract.
% ABSENT_VOICES: Small island nations and developing coastal states whose EEZ resource monopolies are undermined by this reading are structurally excluded from doctrine-setting forums (maritime-law doctrine is set by naval powers and their legal establishments, not by coastal-state coalitions). Scholarship supporting EEZ exclusivity and coastal-state resource sovereignty is marginalized by treating the dispute as settled rather than live. Alternative maritime dispute-resolution institutions (ITLOS, arbitral tribunals) are bypassed in favor of naval-enforcement adjudication, so their jurisprudential voice is excluded from real-time rule-maintenance.
% DISAPPEARANCE_RATIONALE: If freedom-of-navigation enforcement via naval presence vanished, coastal states would immediately assert EEZ exclusivity without external challenge, maritime commerce would negotiate resource-access agreements with coastal states (shifting shipping costs and logistics complexity), global shipping routes would depend on coastal-state consent and licensing, and naval strategy would shift from unimpeded transit to negotiated access. The constraint permits enforcement without negotiation; removing it restores negotiation as the coordination mechanism.
% FOUNDING_PROBLEM: Pre-UNCLOS, naval powers operated under historical-use and open-ocean doctrines with no binding legal framework for ocean boundaries. After UNCLOS codified EEZ limits (1982), interpretive ambiguity emerged: whether EEZ exclusivity overrides pre-existing freedom-of-navigation customary doctrine, or whether both coexist. Naval powers needed a doctrine permitting enforcement of transit rights without accepting coastal-state EEZ sovereignty as binding.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers, maritime-commerce lobbies, and supporting legal scholars attest the founding problem is live: coastal states threaten to close vital shipping corridors and restrict strategic access. They cite customary-law doctrine as the binding standard independent of UNCLOS ratification. Coastal states, developing-nation maritime scholars, and ITLOS panels in some opinions attest the founding problem was resolved by UNCLOS and customary law has been superseded by treaty codification. UN general assemblies split between coastal-state blocs (supporting EEZ exclusivity) and maritime-commerce coalitions (supporting open transit). No consensus external corroboration exists from a neutral authority; the dispute persists among equivalent international institutional actors.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.55 (1982, pre-widespread enforcement) to 0.68 (2024, routine naval transits challenging coastal-state claims), tracking increased naval-capability and doctrine-assertion. Suppression requirement tracks suppression magnitude (how much enforcement is needed to maintain the rule) and rises from 0.52 to 0.71, indicating coastal-state resistance hardened over the interval as their EEZ claims accumulated material stakes. Theater ratio rises from 0.25 to 0.42, indicating enforcement framing shifted from genuine coordination-maintenance (security/transparency norms) toward doctrine-assertion and strategic access maintenance. The 0.42 ratio indicates mixed functional and performative activity: some enforcement defends genuine coordination (predictable transit), much defends strategic access asymmetry. Accessibility collapse at 0.61 overall: developing maritime nations have few alternatives to accepting open transit (trapped by powerlessness), while naval powers retain arbitrage (can invoke different doctrines in different contexts). The measurement series spans UNCLOS codification (1982) through contemporary enforcement escalation (2024), capturing the doctrine's hardening as navy-power practice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (naval powers) and the payers (coastal states) should compute different type classifications from this structural data. From the naval-power seat, the constraint is coordination (predictable transit, reduced negotiation friction) with embedded asymmetric enforcement cost (suppressing coastal-state alternatives). From the coastal-state seat, the constraint is extraction (loss of resource monopoly rent) defended by doctrine-assertion and power, not coordination agreement. The engine computes this divergence from directionality (naval powers: low d, beneficiary zone; coastal states: high d, target zone) and exit options (naval powers: arbitrage exit via doctrine-switching; coastal states: trapped by military inferiority). The authored claim is tangled_rope (both coordination and extraction present, active enforcement required); the divergence shows why the same rule looks like coordination from the beneficiary seat and extraction from the payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers hold institutional power, arbitrage exit (can invoke EEZ exclusivity in their own waters while enforcing freedom of navigation in others'), and beneficiary position (gain resource access at no licensing cost). Their directionality is low (near full beneficiary, d ≈ 0.15–0.25). Coastal states hold moderate institutional power, constrained exit (can assert EEZ boundaries domestically but cannot enforce against naval-power challenge), and victim position (lose resource monopoly rent and sovereignty over their exclusive zones). Their directionality is high (near full target, d ≈ 0.75–0.85). Maritime commerce interests hold organized power, mobile exit (can reroute through alternative suppliers), and secondary-beneficiary position (gain open transit). Non-ratifying powers hold institutional power, arbitrage exit, and dual-position (beneficiary of enforcement doctrine, potential victim if challenged in their own EEZ). The overrides for non-ratifying powers reflect this dual position: they benefit from the doctrine when asserting transit (d ≈ 0.20), but would be targets if their own EEZ were challenged (d would shift to 0.70 in that context). No override is needed; the structural data produces correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-UNCLOS: naval powers operated under ill-defined historical-use and open-ocean doctrines) was partially addressed by UNCLOS (EEZ boundaries codified with 200-nm limits). However, the non-ratifier reading claims the founding problem persists: that coastal-state EEZ exclusivity threatens freedom of navigation for global commerce and strategic mobility. This reading's persistence depends on the claim that UNCLOS did NOT modify customary law, and that naval powers can enforce pre-treaty doctrines independent of treaty ratification. The mandatrophy risk is substantial: if coastal-state EEZ exclusivity is accepted as binding (strict_eez_reading), the founding problem the non-ratifier reading invokes is resolved, and the reading becomes a power-assertion framing rather than law-enforcement framing. The constraint avoids mandatrophy only if (a) customary law persists independent of UNCLOS, or (b) naval powers maintain enforcement capacity that prevents coastal-state exclusivity from becoming operational. The measurement series shows suppression requirement rising (coastal states are resisting the doctrine harder), suggesting mandatrophy pressure: as the cost of maintaining the rule via enforcement rises, the legitimacy of the rule-framing becomes more fragile. Theater ratio rising also signals mandatrophy risk: when enforcement becomes increasingly performative (doctrine-assertion and power, less genuine coordination), the ruling's cover story weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_treaty_hierarchy,
    'Does customary international law on freedom of navigation persist independently of UNCLOS ratification, or did UNCLOS codify and modify customary law, making treaty membership the operative standard?',
    'ICJ or ITLOS definitive ruling on whether non-ratifiers are bound by customary-law doctrine distinct from treaty text, or UNCLOS consensus establishing customary-law modification by treaty codification.',
    'If customary law is independent, the non-ratifier enforcement reading stands; if UNCLOS modified customary law, EEZ exclusivity becomes the binding standard and the reading collapses to a power-assertion framing rather than law-enforcement framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_hierarchy, conceptual, 'Whether freedom-of-navigation doctrine is customary-law independent or treaty-modified.').

omega_variable(
    enforcement_mechanism_legitimacy,
    'Is naval enforcement of freedom-of-navigation doctrine a legitimate adjudication mechanism, or is it power projection masquerading as law enforcement?',
    'Shift to institutionalized dispute resolution (ITLOS, arbitration) for all freedom-of-navigation challenges; if naval powers cease enforcement and cases go to tribunal, the legitimacy of each reading becomes testable via tribunal outcomes.',
    'If tribunals consistently rule for freedom-of-navigation asserters, the reading is vindicated as law; if tribunals consistently rule for coastal-state exclusivity, the reading is revealed as power assertion, reclassifying the constraint from tangled-rope (mixed coordination/extraction) to snare (pure extraction with law-framing cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_legitimacy, empirical, 'Whether naval enforcement mechanism produces jurisprudentially consistent law or power-dependent outcomes.').

omega_variable(
    reading_identity_fusion,
    'Is this reading adopted as a doctrine by naval powers and their allies because it genuinely reflects international law, or because it serves strategic interests in resource access and power projection?',
    'Analyze shift in naval-power legal positions when they occupy coastal-state geographic roles: do they invoke freedom-of-navigation doctrine for their own EEZs when other powers transit, or do they reverse to EEZ-exclusivity arguments? Asymmetric position-switching indicates strategic identity-lock rather than principled law-reading.',
    'If naval powers adopt EEZ exclusivity when they are the coastal state, the reading is revealed as instrumental doctrine rather than principled law-reading, indicating the constraint is identity-locked and the agenda-setter''s power plays a larger role than doctrine. This would support reclassification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_fusion, empirical, 'Whether the reading is principled doctrine or strategic position-dependent.').

omega_variable(
    kernel_alternative_readings_coexistence,
    'Can the three kernel readings (non-ratifier-enforcement, strict-EEZ, historical-rights) coexist in a single international legal framework, or does one foreclose the others?',
    'Examine UNCLOS negotiation records and subsequent treaty interpretation consensus: if states explicitly accepted that multiple readings would coexist, they coexist; if treaty text and practice converged on one reading, foreclosure occurred. Currently contested; resolution requires authoritative treaty interpretation or evolved consensus.',
    'If one reading forecloses others, this reading''s legitimacy depends on whether the foreclosing doctrine is this one or a sibling. If all three coexist, the constraint is revealed as inherently contestable (supports omega on reading legitimacy). If foreclosed, the constraint should be reclassified to match the foreclosing reading''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_alternative_readings_coexistence, conceptual, 'Whether sibling kernel readings foreclose each other or coexist in international legal frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement_basis(uncl_tr_t1982, observed).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t1995, observed).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(uncl_tr_t2005, observed).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2015, observed).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(uncl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement_basis(uncl_be_t1982, observed).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement_basis(uncl_be_t1995, observed).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement_basis(uncl_be_t2005, observed).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(uncl_be_t2015, observed).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(uncl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.52).
narrative_ontology:measurement_basis(uncl_su_t1982, observed).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(uncl_su_t1995, observed).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement_basis(uncl_su_t2005, observed).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(uncl_su_t2015, observed).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(uncl_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1982, tn=2024
narrative_ontology:measurement(uncl_grid_01, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(class), 1982, 0.38).
narrative_ontology:measurement(uncl_grid_02, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(class), 2024, 0.52).
narrative_ontology:measurement(uncl_grid_03, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(individual), 1982, 0.35).
narrative_ontology:measurement(uncl_grid_04, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(individual), 2024, 0.48).
narrative_ontology:measurement(uncl_grid_05, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(organizational), 1982, 0.42).
narrative_ontology:measurement(uncl_grid_06, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(organizational), 2024, 0.55).
narrative_ontology:measurement(uncl_grid_07, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(structural), 1982, 0.48).
narrative_ontology:measurement(uncl_grid_08, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse(structural), 2024, 0.61).
narrative_ontology:measurement(uncl_grid_09, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(class), 1982, 0.58).
narrative_ontology:measurement(uncl_grid_10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(class), 2024, 0.68).
narrative_ontology:measurement(uncl_grid_11, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(individual), 1982, 0.55).
narrative_ontology:measurement(uncl_grid_12, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(individual), 2024, 0.65).
narrative_ontology:measurement(uncl_grid_13, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(organizational), 1982, 0.62).
narrative_ontology:measurement(uncl_grid_14, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(organizational), 2024, 0.71).
narrative_ontology:measurement(uncl_grid_15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(structural), 1982, 0.65).
narrative_ontology:measurement(uncl_grid_16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance(structural), 2024, 0.74).
narrative_ontology:measurement(uncl_grid_17, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(class), 1982, 0.42).
narrative_ontology:measurement(uncl_grid_18, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(class), 2024, 0.58).
narrative_ontology:measurement(uncl_grid_19, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(individual), 1982, 0.38).
narrative_ontology:measurement(uncl_grid_20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(individual), 2024, 0.54).
narrative_ontology:measurement(uncl_grid_21, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(organizational), 1982, 0.48).
narrative_ontology:measurement(uncl_grid_22, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(organizational), 2024, 0.62).
narrative_ontology:measurement(uncl_grid_23, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(structural), 1982, 0.52).
narrative_ontology:measurement(uncl_grid_24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, stakes_inflation(structural), 2024, 0.67).
narrative_ontology:measurement(uncl_grid_25, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(class), 1982, 0.48).
narrative_ontology:measurement(uncl_grid_26, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(class), 2024, 0.65).
narrative_ontology:measurement(uncl_grid_27, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(individual), 1982, 0.45).
narrative_ontology:measurement(uncl_grid_28, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(individual), 2024, 0.62).
narrative_ontology:measurement(uncl_grid_29, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(organizational), 1982, 0.5).
narrative_ontology:measurement(uncl_grid_30, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(organizational), 2024, 0.68).
narrative_ontology:measurement(uncl_grid_31, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(structural), 1982, 0.55).
narrative_ontology:measurement(uncl_grid_32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression(structural), 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.18).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, maritime_resource_extraction_monopoly).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, great_power_naval_strategy_constraint).

% DUAL FORMULATION NOTE:
% This constraint is part of the UNCLOS sovereignty-boundary kernel family. The three readings (non_ratifier_enforcement, strict_eez, historical_rights) share a referent (the boundary between EEZ exclusivity and freedom-of-navigation rights) but instantiate different constraints due to divergent core premises about customary-law independence and enforcement mechanisms. All three should be authored as separate constraint stories linked via network.affects_constraints. The ε-invariance principle applies: if measuring the constraint via 'what enforcement mechanism controls maritime transit' yields different ε values depending on which reading is adopted, these are two different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
