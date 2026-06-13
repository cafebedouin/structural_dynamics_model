% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Maximum Extraction Authorization (Lord Reading)
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   The feudal oath, sworn between lord and vassal, is a contested kernel
 *   that instantiates three distinct constraints depending on which party's
 *   interpretation framework is adopted. THIS story instantiates the LORDS'
 *   READING: the oath authorizes the lord to extract from the vassal up to
 *   the point the vassal cannot render military service or maintain the
 *   estate — there is no fixed extraction ceiling in the oath's terms, only a
 *   practical capacity limit. The extraction is bounded only by what rebels
 *   against, not by what the oath itself says. This reading vindicates
 *   absolute lordship doctrine and personalized sovereign authority. The
 *   feudal oath under the lord's reading is a snare: vassals are trapped in
 *   identity and location, suppression is high (rebellion courts as treason),
 *   and the constraint persists because its enforcement cost to lords is
 *   lower than the benefit of extraction and because exit costs for vassals
 *   are catastrophic.
 *
 * KEY AGENTS:
 *   - feudal_lords (institutional): set and enforce extraction demands; claim interpretation authority over the oath's terms; use expulsion and military force to suppress resistance
 *   - vassals (moderate power, identity-locked): swear the oath expecting bounded, reciprocal obligations; experience escalating demands over time; resistance is suppressed by treason law and collective punishment threats
 *   - peasantry (powerless): cascade extraction from intensified vassal demands; have no appeal to the lord; trapped by serfdom law and subsistence dependency
 *   - king/sovereign (institutional): authorize feudal oaths as decentralized extraction mechanism; benefit from lords' service upward; call back the oath selectively when rebellion risk emerges
 *   - ecclesiastical authority (institutional, observer with partial exclusion): perform oath's sacramental dimension; claim jurisdiction over oath-breaking; structurally barred from enforcing extraction limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.81).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.77).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Maximum Extraction Authorization (Lord Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '90c5b0ff-c066-45c3-91d8-026eadf2c41e').
narrative_ontology:cs_kernel_codification('90c5b0ff-c066-45c3-91d8-026eadf2c41e', fixed_text).
narrative_ontology:cs_authority_grounding('90c5b0ff-c066-45c3-91d8-026eadf2c41e', extraction).
narrative_ontology:cs_interpretation_layer_present('90c5b0ff-c066-45c3-91d8-026eadf2c41e').
narrative_ontology:cs_reading_relation('90c5b0ff-c066-45c3-91d8-026eadf2c41e', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('90c5b0ff-c066-45c3-91d8-026eadf2c41e', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('90c5b0ff-c066-45c3-91d8-026eadf2c41e', foundational, lord_holds_oath_interpretation_authority).
narrative_ontology:cs_axiom_status(lord_holds_oath_interpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('90c5b0ff-c066-45c3-91d8-026eadf2c41e', lord_holds_oath_interpretation_authority, instrumental).
narrative_ontology:cs_axiom('90c5b0ff-c066-45c3-91d8-026eadf2c41e', foundational, extraction_bounded_by_vassal_capacity_not_text).
narrative_ontology:cs_axiom_status(extraction_bounded_by_vassal_capacity_not_text, holdable).
narrative_ontology:cs_axiom_grounding('90c5b0ff-c066-45c3-91d8-026eadf2c41e', extraction_bounded_by_vassal_capacity_not_text, empirically_contingent).
narrative_ontology:cs_reference_frame('90c5b0ff-c066-45c3-91d8-026eadf2c41e', lord_unilateral_interpretation_authority).
narrative_ontology:cs_drift_state('90c5b0ff-c066-45c3-91d8-026eadf2c41e', charter_emergence_moment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90c5b0ff-c066-45c3-91d8-026eadf2c41e', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasantry_under_vassal_jurisdiction).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness at 0.81 reflects that the lord interprets the oath to authorize escalating demands bounded only by practical capacity — this is the highest ε reading of the oath. Suppression at 0.77 is high because vassal rebellion is criminalized (treason) and collective (village) punishment is the consequence; the constraint requires active threat of force to maintain. Theater_ratio at 0.42 reflects that the original coordination function (military organization and justice without bureaucracy) is genuine at the interval's start but declines over the 280-year span as written law and charters emerge (stage 0–120) and then stabilizes (120–280) because the constraint's form does not change even though its legitimacy erodes. The measurement series show base_extractiveness rising from 0.62 to 0.81 (lords escalate demands over time as their territorial consolidation allows), suppression rising from 0.58 to 0.77 (enforcement machinery hardens as resistance grows), and theater_ratio rising from 0.25 to 0.42 then plateauing (the constraint performs the coordination story even as extraction becomes its primary function). The shared time grid allows the engine to detect that extraction accelerates (early period) while suppression follows (later period) — a signature of extraction layered onto weakening coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the LORD'S seat, the oath is a legitimate authority to extract up to vassal capacity; the sworn reciprocity is satisfied by the lord's provision of protection and justice. From the VASSAL'S seat, the oath specifies bounded obligations and the lord's escalation breaches reciprocity. The engine computes these as different classifications because the structural relationships differ: the lord has directionality ~1.0 (full target becomes beneficiary via extraction authority), the vassal has directionality ~0.9 (full target with identity_locked exit), and the sovereign has directionality ~0.3 (benefits from decentralized extraction without bearing enforcement cost). The same constraint structure — the oath — produces different seat-level types because d is not symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Feudal lords: beneficiaries who set the constraint and extract from it without running the military apparatus (they organize locally but extract upward). Vassals: victims who are trapped by identity (hereditary status), spatial immobility (tied to the fief), and legal disability (oath breach is treason). Their exit options are: rebellion (d stays high, costs catastrophic), abandonment (identity and property loss), or renegotiation (only credible when rebellion threat emerges, then temporary). Ecclesiastical authority: structurally excluded from enforcing extraction limits despite claiming jurisdiction over oath-breaking — they are observed in the constraint's operation but not decision-makers. The directionality derivation: lords as beneficiaries get low d (benefit from it, set it) unless their power is undermined; vassals as victims with identity_locked + trapped exit get d near 1.0; the peasantry with zero formal voice and powerless status also get d near 1.0. No directionality overrides are needed; the derivation chain produces accurate values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows classic mandatrophy dynamics: the founding problem (post-Carolingian military organization) is substantially solved by the 12th-13th century (feudal system is stable, states are consolidating, alternative mechanisms emerge), but the constraint persists because its extraction function now outweighs its coordination function. The theater_ratio rise from 0.25 to 0.42 shows the military-justice coordination story is invoked to justify extraction even as its functional necessity wanes. The founding_problem_status is 'contested' because the extraction readings (lord, vassal) disagree about whether the founding problem's resolution justifies ongoing extraction — lords say military necessity is eternal; vassals say necessity was temporary and extraction has become institutionalized rent. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) correctly fires the mandatrophy flag: the constraint persists despite disputed necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_interpretation_authority,
    'Does the sacramental oath itself constrain the lord''s extraction to a fixed scope, or does it authorize the lord to interpret the oath''s terms and adjust extraction demands within the vassal''s practical capacity?',
    'Historical analysis of oath formulas, Charter evidence on reciprocal obligations, and judicial records showing how disputes over oath-scope were adjudicated. Examination of whether lords or vassals (or ecclesiastical arbiters) held interpretation authority in practice.',
    'If the oath is self-limiting (constrains extraction to fixed terms), the constraint reclassifies toward tangled_rope with bounded extraction; if the oath authorizes the lord to interpret, it remains snare with capacity-limited extraction. This is the central contested premise of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oath_interpretation_authority, conceptual, 'Whether feudal oath is interpretation-resistant (vassal_coordination reading) or lord-interpreted (lord_extraction reading).').

omega_variable(
    ecclesiastical_constraint_on_extraction,
    'Do the sacramental and Christian dimensions of the oath (oath-swearing as a sacred act, Christian charity obligations) constrain the lord''s extraction, or are those dimensions ceremonial without effect on permissible extraction levels?',
    'Ecclesiastical records (penitential literature, pastoral guidance, canon law rulings), disputes where the Church attempted to limit extraction on charitable grounds, and instances where lords were excommunicated or denounced for extraction deemed unchristian.',
    'If ecclesiastical constraint is real and enforced, the extraction measured here (0.81) would be overstated; the actual ceiling would be lower, and the ecclesiastical_mediation_reading would apply. If ecclesiastical constraint is performative without effect, suppression at 0.77 is accurate and this reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_constraint_on_extraction, empirical, 'Whether Church authority imposed effective constraint on feudal extraction (ecclesiastical reading) or provided only ceremonial framing.').

omega_variable(
    vassal_capacity_measurement,
    'What constitutes the practical capacity ceiling that bounds extraction in this reading — the upper limit of what a vassal can render without losing ability to maintain household, render military service, or sustain the next generation''s claim to the fief?',
    'Economic reconstruction of vassal household budgets, military service capacity under various extraction regimes, and historical analysis of when vassals actually rebelled or broke oath — what extraction level triggered the threshold?',
    'If capacity ceiling is firm and historically observable, suppression is properly measured at 0.77 (lords rarely pushed past it because rebellion cost exceeded benefit); if capacity ceiling is flexible and often overshot (with rebellions followed by retrenchment), suppression should be higher and extraction claims less bounded than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vassal_capacity_measurement, empirical, 'The actual binding limit on extraction under the lord-reading interpretation of the oath.').

omega_variable(
    charter_vs_custom_reading,
    'Over the interval, does the emergence of Charter evidence and written reciprocal-obligation texts (Magna Carta, vassal charters) create a competing reading that forecloses the lord''s unfettered interpretation authority, or does it merely influence the terms of extraction without resolving the interpretation contest?',
    'Historical textual analysis of Charter adoption and enforcement, disputes where vassals cited Charter text against lords'' expansion attempts, and whether Charter evidence is accepted by courts or ignored as non-binding custom.',
    'If Charter evidence forecloses the lord_extraction_reading, the constraint should be reclassified toward vassal_coordination_reading by the end of the interval; if Charter evidence influences but does not foreclose, theater_ratio rise to 0.42 (Charter-reading performance without changed extraction substance) is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_vs_custom_reading, conceptual, 'Whether written Charter text forecloses or merely influences the lord-reading of the oath.').

omega_variable(
    resistance_mechanisms_development,
    'Does recorded vassal resistance (petitions, passive noncompliance, subtle defection, charter demands) increase over the interval in response to escalating extraction, or remain constant? If increasing, is it coordinated resistance or isolated incidents?',
    'Archive records of vassal petitions, Charter-making moments, documented disputes, and rebellions. Pattern analysis of whether resistance becomes more organized and textually-grounded over time.',
    'Rising organized resistance would indicate suppression is increasing (0.77 is an underestimate) and that the constraint approaches its instability point; constant or fragmented resistance would suggest the measured suppression and capacity-bounded extraction are accurate. This feeds both the accuracy of measured suppression and whether the constraint is trending toward reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_mechanisms_development, empirical, 'Pattern of vassal resistance to extraction over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 280).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(feud_tr_t120, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement(feud_tr_t240, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 240, 0.42).
narrative_ontology:measurement(feud_tr_t280, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 280, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 80, 0.74).
narrative_ontology:measurement(feud_be_t120, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 120, 0.78).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 160, 0.79).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 200, 0.8).
narrative_ontology:measurement(feud_be_t240, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 240, 0.81).
narrative_ontology:measurement(feud_be_t280, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 280, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(feud_su_t120, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 120, 0.73).
narrative_ontology:measurement(feud_su_t160, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 160, 0.75).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 200, 0.76).
narrative_ontology:measurement(feud_su_t240, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 240, 0.77).
narrative_ontology:measurement(feud_su_t280, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 280, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, noble_rebellion_suppression).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, medieval_charter_limitation_authority).

% DUAL FORMULATION NOTE:
% The feudal_oath_reciprocity kernel instantiates three distinct constraints depending on interpretation frame: lord_extraction_reading (this file, ε=0.81, snare), vassal_coordination_reading (ε estimated 0.48, tangled_rope), ecclesiastical_mediation_reading (ε estimated 0.55, rope). Each reading has its own constraint_id and independent classification because their ε values are structurally determined by who holds interpretation authority (lord, vassal/charter, Church). The three stories are linked via network.affects_constraints to indicate the kernel contest — each reading influences the others' conditions but does not logically foreclose them (coexists_with relation holds across the family). The kernel itself — the feudal oath as a commitment structure — is preserved in cs_structure.kernel_codification as 'fixed_text' with authority_grounding as 'extraction' (lord authority derives from extraction benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
