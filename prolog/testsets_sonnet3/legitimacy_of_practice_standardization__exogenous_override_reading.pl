% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the EXOGENOUS OVERRIDE reading of the
 *   practice-standardization kernel: state authority decrees a calendar and
 *   dress change by law, justified as serving collective modernization,
 *   fiscal alignment, and international legibility. The reading treats
 *   legitimacy as flowing from the decree itself — the state's authority to
 *   compel for collective benefit — rather than from voluntary uptake or
 *   domain partition. What this reading foregrounds structurally, per its own
 *   lights, is the gap between the decree's legal force and its behavioral
 *   uptake: abrupt legal imposition backed by fines and inspection produces
 *   high initial suppression and durable underground persistence of the
 *   displaced practice, not genuine displacement. Rural populations run a
 *   stable double life — lunar dates and traditional dress in private and
 *   ritual contexts, Gregorian dates and standardized dress for the state —
 *   for decades, not as a transitional phase but as an equilibrium the
 *   decree's own enforcement machinery cannot dislodge because it addresses a
 *   problem (fiscal/diplomatic legibility) that rural daily life does not
 *   share.
 *
 * KEY AGENTS:
 *   - central_state_ministries: agenda_setter (institutional/arbitrage) — decrees and enforces the standard, captures legibility gains
 *   - rural_agrarian_populations: payer (powerless/trapped) — bears compliance cost for a standard with no local function, maintains parallel lunar practice
 *   - religious_calendar_authorities: payer/excluded (moderate/constrained) — displaced from legal standing, retained in private practice
 *   - urban_administrative_elites: beneficiary (powerful/mobile) — converts prior voluntary adoption into state-conferred advantage
 *   - historians_of_modernization: observer (analytical) — documents the recurring dual-practice pattern across comparable reforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e').
narrative_ontology:cs_kernel_codification('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', formalized).
narrative_ontology:cs_authority_grounding('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', extraction).
narrative_ontology:cs_interpretation_layer_present('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e').
narrative_ontology:cs_reading_relation('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', foundational, state_decree_for_collective_benefit_confers_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_for_collective_benefit_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', state_decree_for_collective_benefit_confers_legitimacy, instrumental).
narrative_ontology:cs_axiom('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', secondary, sovereign_authority_may_override_local_practice_function).
narrative_ontology:cs_axiom_status(sovereign_authority_may_override_local_practice_function, holdable).
narrative_ontology:cs_axiom_grounding('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', sovereign_authority_may_override_local_practice_function, conventional).
narrative_ontology:cs_reference_frame('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', sovereign_modernization_mandate).
narrative_ontology:cs_drift_state('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', post_decree_multigenerational, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3929b3a-3c2f-43a0-b8fb-c2925e7cec8e', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_ministries).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agrarian_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, religious_calendar_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_dress_artisans).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, national_modernization_narrative).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_synchronization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree replacing the lunar calendar and traditional dress codes with standardized Gregorian dating and Western-style administrative attire, backed by statute and penalty. Frames the change as necessary for tax collection schedules, treaty compliance, and international legibility. Bears none of the disruption cost of abandoned local timekeeping or clothing practice; captures the legibility and coordination gains directly.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_ministries, agenda_setter,
    institutional, generational, arbitrage, national).

% Already operate on Western schedules and dress for reasons of trade and diplomacy; the decree formalizes what this group had adopted voluntarily, and now confers legal advantage and administrative preference on those who comply, converting a prior cultural choice into a state-backed credential.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_elites, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from reduced friction in scheduling, contracts, and diplomatic protocol once the state adopts calendar and dress conventions matching the international system. Do not administer or enforce the domestic decree but their expectations are the stated justification the state cites for it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners, beneficiary,
    institutional, civilizational, analytical, global).

% Continue to organize planting, harvest, festivals, and marriage timing by the lunar calendar because it tracks agricultural and ritual cycles the Gregorian calendar does not. Face fines, denial of administrative services, or social penalty for noncompliance in official contexts, and respond by maintaining a double life: Gregorian dates for state paperwork, lunar dates for everything that matters locally. This is not a transitional accommodation but a stable multi-decade equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agrarian_populations, payer,
    powerless, generational, trapped, regional).

% Lose official standing to set festival and ritual dates once the state calendar becomes the only legally recognized one, though communities continue to consult them privately. Their authority is displaced from law without being displaced from practice, producing a gap between de jure and de facto reckoning that the decree does not resolve, only relocates underground.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, religious_calendar_authorities, payer,
    moderate, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, religious_calendar_authorities, excluded).

% Lose the administrative and civic-service market for traditional garments once state employees and schoolchildren are required to wear standardized dress, while continuing to produce for weddings, funerals, and religious festivals where the old dress persists. Their livelihood narrows to the private-ritual market the decree did not reach.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_dress_artisans, payer,
    powerless, biographical, trapped, local).

% Enforce dress and calendar compliance in schools, offices, and markets through inspection and fines. Increasingly report surface compliance in official settings and unofficial persistence in private life, converting enforcement into a ritual of documentation rather than a mechanism that changes underlying practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, compliance_inspectors, agenda_setter,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, compliance_inspectors, observer).

% Study the decree and its aftermath across comparable cases (calendar reforms, dress reforms, script reforms) and document the recurring pattern: legal imposition from above produces durable dual practice rather than genuine displacement, especially where the imposed practice does not track a real functional need in the governed population's daily life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, historians_of_modernization, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes the state's administrative, fiscal, and diplomatic calendar and comportment with the international system the state trades and negotiates within, reducing transaction friction for treaties, tax schedules, and cross-border commerce.
% TRANSFER_FUNCTION: Moves administrative legibility and international standing to the central state and its urban, internationally-oriented elites; moves compliance cost, penalty exposure, and loss of locally-functional timekeeping and dress onto rural and traditionally-embedded populations who have no functional use for the imposed standard.
% ABSENT_VOICES: Rural populations and religious calendar authorities were not consulted in the decree's drafting; their objection — that lunar timing tracks real agricultural and ritual function the Gregorian calendar does not replace — is absent from the state's stated justification, which addresses only fiscal and diplomatic legibility.
% DISAPPEARANCE_RATIONALE: From the state's seat, repeal would immediately reintroduce friction into tax collection and diplomatic scheduling — the world rearranges. From the rural seat, the decree's disappearance would change almost nothing in daily life, since lunar timekeeping never actually stopped; only the fines and inspections would vanish. The verdict genuinely differs by seat rather than being resolvable to one fact.
% FOUNDING_PROBLEM: The state's tax cycles, treaty obligations, and diplomatic calendar were misaligned with the international system it needed to transact with, and administrative dress signaled pre-modern status to foreign observers whose recognition the state's legitimacy partly depended on.
% FOUNDING_PROBLEM_CORROBORATION: Central ministries attest the problem is live and the decree remains necessary for fiscal and diplomatic function. Independent historians of modernization, examining comparable reforms, attest that the administrative-legibility problem was substantially solved early and that continued enforcement now serves symbolic modernization signaling and urban-elite status conversion more than the original fiscal problem — this reading is corroborated from outside the beneficiary set.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly over the interval (0.55 to 0.68) as the state converts an initial fiscal/diplomatic justification into an entrenched administrative-preference system that increasingly advantages compliant urban elites regardless of the original justification's continued relevance. Theater ratio rises sharply (0.2 to 0.61) because enforcement increasingly consists of documentation and inspection rituals confirming surface compliance rather than producing the substantive uptake the decree was meant to achieve — compliance_inspectors report Gregorian dates on paperwork while lunar dates govern actual planting and ritual timing underneath. Suppression starts very high (0.85, reflecting the abrupt legal imposition and initial enforcement intensity) and eases only slightly as the state settles into tolerating the underground dual practice rather than continuing to fight it, which is itself evidence the enforcement was never able to eliminate the alternative — consistent with a tangled_rope, not a mountain: the coordination function (fiscal/diplomatic legibility) is real, but it rides on asymmetric extraction from populations for whom the imposed standard serves no local function.
 *
 * DIRECTIONALITY LOGIC:
 *   Central state ministries and international trade partners sit at the beneficiary end: they receive legibility and coordination gains and bear none of the disruption cost. Urban administrative elites are also beneficiaries because the decree formalizes and legally rewards a change they had already made voluntarily for their own reasons — the decree effectively transfers a competitive advantage to them. Rural populations, religious calendar authorities, and traditional dress artisans sit at the target end: they bear compliance costs, penalty exposure, and loss of status/market for practices that had a real function the imposed standard does not replace, and their exit options are trapped or constrained by geography, livelihood, and lack of administrative access.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors symmetric to each other. First, it prevents treating the decree as pure Rope (voluntary coordination) simply because a genuine coordination problem (fiscal/diplomatic alignment) exists — the tangled_rope classification requires naming who is coordinated (state, urban elites, trade partners) and who pays (rural populations, religious authorities, artisans) through the same enforcement structure. Second, it prevents treating the decree as pure Snare by acknowledging the coordination function is real and not merely cover — the state's fiscal and diplomatic legibility problem was genuine at founding, even if enforcement now outlives the problem's urgency for the populations it burdens. The tangled_rope reading holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decree_versus_ratification_locus,
    'Did the calendar/dress decree CAUSE the practice change, or did it ratify a change already underway among urban elites for independent reasons (trade contact, prior voluntary emulation)?',
    'Compare adoption timelines among urban administrative elites before and after the decree''s issuance; if substantial voluntary uptake predates the legal instrument, the endogenous_displacement_reading''s causal story is better supported for that population, even while the exogenous_override_reading remains the operative constraint for populations who did not adopt voluntarily.',
    'If the decree only ratified pre-existing elite practice, the exogenous_override reading''s claimed coordination function is weaker than authored here for the beneficiary population, though the extraction imposed on rural populations remains structurally identical either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decree_versus_ratification_locus, empirical, 'Whether state decree caused or merely legalized pre-existing elite practice change.').

omega_variable(
    dual_practice_as_equilibrium_or_transition,
    'Is the rural double-life pattern (Gregorian for state, lunar for daily/ritual life) a stable long-term equilibrium, or a multi-decade transitional phase that will eventually resolve into full displacement?',
    'Longitudinal tracking of lunar-calendar usage in rural festival-timing, marriage-timing, and agricultural planning across multiple generations post-decree; a genuinely transitional phase should show monotonic decline in lunar usage, while a stable equilibrium should show a persistent floor.',
    'If a persistent floor is observed across generations, this corroborates the exogenous_override reading''s core claim (surface compliance masking durable underground practice) and undercuts any state narrative that treats the decree as having succeeded in full displacement; it also strengthens the case for the dual_practice_equilibrium sibling reading''s account of the same behavior as a stable jurisdictional partition rather than evasion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_practice_as_equilibrium_or_transition, empirical, 'Whether dual practice is stable equilibrium or transitional decline.').

omega_variable(
    naturalness_of_collective_benefit_framing,
    'Is ''collective benefit'' (modernization, fiscal stability, international alignment) a good-faith description of a genuine coordination problem, or a legitimating vocabulary that launders an urban-elite status transfer as a national interest?',
    'Examine whether the decree''s stated fiscal/diplomatic justifications correspond to measurable fiscal or diplomatic outcomes attributable to the calendar/dress change specifically, versus outcomes attributable to unrelated modernization measures bundled under the same rhetoric.',
    'If collective-benefit framing does not track measurable outcomes, the tangled_rope classification''s coordination-function claim weakens and the constraint drifts toward a snare (extraction with a coordination story as cover); if outcomes are substantiated, the tangled_rope reading (genuine coordination plus asymmetric extraction) is the accurate classification as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_collective_benefit_framing, conceptual, 'Whether stated collective-benefit justification is substantive or a legitimating cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language kernel 'legitimacy of practice change' (calendar/dress standardization). The exogenous_override_reading (this file) authors ε=0.68 for the state-decree arrangement, assessed by its own lights as substantially extractive tangled_rope. The endogenous_displacement_reading authors a structurally distinct claim (legitimacy from voluntary adoption) with its own ε and likely a rope or mountain-adjacent classification for the elite-adoption population. The dual_practice_equilibrium_reading authors a domain-partition claim treating the rural lunar practice not as suppressed underground activity but as a coherent parallel jurisdiction, likely yielding a lower ε and a rope or scaffold classification for that same observed behavior. All three share the same underlying historical episode but are NOT the same constraint — per the ε-invariance principle, each reading's classification is authored independently and linked here rather than averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
